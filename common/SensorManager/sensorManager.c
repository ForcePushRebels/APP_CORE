////////////////////////////////////////////////////////////
//  sensorManager source file
//  Provides sensorManager interface
//
// general disclosure: copy or share the file is forbidden
// Written : 21/05/2025
////////////////////////////////////////////////////////////

#include "sensorManager.h"
#include "idCard.h"
#include "map_engine.h"
#include <stdatomic.h>
#include <stdbool.h>

static sensorManager_t s_tSensorManager;

#define SENSOR_MANAGER_TASK_PERIOD 30
#define SENSOR_OBSTACLE_THRESHOLD 50

/* Pre-computed thresholds for each sensor :
 * index 0 and 2 are "side" sensors ⇒ threshold / 4
 * index 1 is the front sensor         ⇒ full threshold */
static const uint16_t s_ausObstacleThreshold[SENSOR_MANAGER_SENSORS_COUNT]
    = {SENSOR_OBSTACLE_THRESHOLD / 4, SENSOR_OBSTACLE_THRESHOLD, SENSOR_OBSTACLE_THRESHOLD / 4};

// Lookup table to convert raw sensor readings to millimetres
static bool s_bLUTInitialized = false;
static uint16_t s_ausRawToMmLUT[256];
static void initRawToMmLUT(void);

// prototypes
static void *sensorManagerTask(void *p_ptvParam);

///////////////////////////////////////////
/// initRawToMmLUT
///////////////////////////////////////////
static void initRawToMmLUT(void)
{
    if (s_bLUTInitialized)
    {
        return;
    }

    for (uint16_t raw = 0; raw < 256; raw++)
    {
        uint16_t clamped = raw;

        if (clamped > SENSOR_MAX_RAW_VALUE)
        {
            clamped = SENSOR_MAX_RAW_VALUE;
        }
        else if (clamped < SENSOR_MIN_RAW_VALUE)
        {
            clamped = SENSOR_MIN_RAW_VALUE;
        }

        s_ausRawToMmLUT[raw]
            = (clamped * (SENSOR_MAX_MM_VALUE - SENSOR_MIN_MM_VALUE)) / (SENSOR_MAX_RAW_VALUE - SENSOR_MIN_RAW_VALUE)
              + SENSOR_MIN_MM_VALUE;
    }

    s_bLUTInitialized = true;
}

///////////////////////////////////////////
/// sensorManagerInit
///////////////////////////////////////////
int sensorManagerInit(void)
{
    int32_t l_iRet = SENSOR_MANAGER_OK;

    l_iRet = mutexCreate(&s_tSensorManager.t_tMutex);
    if (l_iRet != MUTEX_OK)
    {
        return l_iRet;
    }

    l_iRet = xTimerCreate(&s_tSensorManager.t_tTimer, SENSOR_MANAGER_TASK_PERIOD, XOS_TIMER_MODE_PERIODIC);
#ifdef DEBUG
    strcpy(s_tSensorManager.t_tTimer.t_acTimerName, "sensorManager Timer");
#endif
    if (l_iRet != XOS_TIMER_OK)
    {
        X_LOG_TRACE("Error creating timer");
        return l_iRet;
    }

    l_iRet = osTaskInit(&s_tSensorManager.t_tTaskHandler);
    if (l_iRet != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("Error initializing task");
        return l_iRet;
    }

    s_tSensorManager.t_tTaskHandler.t_ptTask = sensorManagerTask;
    s_tSensorManager.t_tTaskHandler.t_ptTaskArg = NULL;
    s_tSensorManager.t_tTaskHandler.t_ulStackSize = (size_t)OS_TASK_DEFAULT_STACK_SIZE;
    s_tSensorManager.t_tTaskHandler.a_iStopFlag = OS_TASK_SECURE_FLAG;
    strcpy(s_tSensorManager.t_tTaskHandler.t_acTaskName, "sensorManager");

    // Pre-compute conversion LUT (only once)
    initRawToMmLUT();

    return SENSOR_MANAGER_OK;
}

///////////////////////////////////////////
/// checkMovePossible
///////////////////////////////////////////
bool checkMovePossible(void)
{
    // On suppose que les valeurs sont à jour dans s_tSensorManager.t_tISensors
    for (int i = 0; i < SENSOR_MANAGER_SENSORS_COUNT; ++i)
    {
        if (s_tSensorManager.t_tISensors[i] < s_ausObstacleThreshold[i])
        {
            X_LOG_TRACE("Obstacle detected (sensor %d)", i);
            return false;
        }
    }
    // Aucun obstacle détecté
    return true;
}

///////////////////////////////////////////
/// checkForward
///////////////////////////////////////////
bool checkForward(void)
{
    return checkMovePossible();
}

///////////////////////////////////////////
/// startMonitoring
///////////////////////////////////////////
int startMonitoring()
{

    int32_t l_iRet = SENSOR_MANAGER_OK;

    l_iRet = osTaskCreate(&s_tSensorManager.t_tTaskHandler);
    if (l_iRet != OS_TASK_SUCCESS)
    {
        return l_iRet;
    }

    return SENSOR_MANAGER_OK;
}

///////////////////////////////////////////
/// stopMonitoring
///////////////////////////////////////////
int stopMonitoring(void)
{
    /* Stop the periodic timer to unblock the worker thread */
    xTimerStop(&s_tSensorManager.t_tTimer);

    /* Request task graceful shutdown */
    return osTaskStop(&s_tSensorManager.t_tTaskHandler, 10);
}

///////////////////////////////////////////
/// updateVision
///////////////////////////////////////////
uint16_t updateVision(int p_iSensor)
{
    X_ASSERT(p_iSensor >= 0 && p_iSensor < SENSOR_MANAGER_SENSORS_COUNT - 2);
    uint16_t raw = s_tSensorManager.t_tISensors[p_iSensor];
    return rawValuesToMm(raw);
}

///////////////////////////////////////////
/// updateSensorData
///////////////////////////////////////////
static void updateSensorData(void *p_ptvParam)
{
    (void)p_ptvParam; // warning suppression (unused parameter)

    int32_t l_iRet = 0;

    l_iRet = GetSensorValues(s_tSensorManager.t_tISensors);
    if (l_iRet != 0)
    {
        X_LOG_TRACE("Error getting sensor values");
    }

    /* Conversion brute → mm via LUT en une seule boucle  */
    for (int i = 0; i < SENSOR_MANAGER_SENSORS_COUNT; ++i)
    {
        s_tSensorManager.t_tISensors[i] = s_ausRawToMmLUT[s_tSensorManager.t_tISensors[i]];
    }

    s_tSensorManager.t_tFloorSensor = (uint16_t)GetFloorSensorValue();

    if (idCardGetRole() == IDCARD_ROLE_EXPLO)
    {
        map_engine_update_vision(s_tSensorManager.t_tISensors, (uint8_t)SENSOR_MANAGER_SENSORS_COUNT);
        map_engine_update_floor_sensor(s_tSensorManager.t_tFloorSensor);
    }
}

///////////////////////////////////////////
/// sensorManagerTask
///////////////////////////////////////////
static void *sensorManagerTask(void *p_ptvParam)
{
    (void)p_ptvParam; // warning suppression (unused parameter)

    // Start the timer with the correct period (100ms)
    xTimerStart(&s_tSensorManager.t_tTimer);

    X_LOG_TRACE("Starting sensor monitoring with period of %d ms", SENSOR_MANAGER_TASK_PERIOD);

    /* Boucle principale : blocante jusqu'à ce que le timer soit stoppé */
    int l_iPeriods = xTimerProcessCallback(&s_tSensorManager.t_tTimer, updateSensorData, NULL);

    if (l_iPeriods < 0)
    {
        X_LOG_TRACE("Timer processing error: %d", l_iPeriods);
    }

    /* Assure la bonne fermeture du timer */
    xTimerStop(&s_tSensorManager.t_tTimer);

    return NULL;
}

///////////////////////////////////////////
/// rawValuesToMm
///////////////////////////////////////////
uint16_t rawValuesToMm(uint16_t p_usRawValue)
{

    if (p_usRawValue > SENSOR_MAX_RAW_VALUE)
    {
        p_usRawValue = SENSOR_MAX_RAW_VALUE;
    }
    else if (p_usRawValue < SENSOR_MIN_RAW_VALUE)
    {
        return SENSOR_MIN_MM_VALUE;
    }

    return s_ausRawToMmLUT[p_usRawValue];
}
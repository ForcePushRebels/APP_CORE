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

static sensorManager_t s_tSensorManager;

#define SENSOR_MANAGER_TASK_PERIOD 5
#define SENSOR_OBSTACLE_THRESHOLD 50

// prototypes
static void *sensorManagerTask(void *p_pvParam);

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

    return SENSOR_MANAGER_OK;
}

///////////////////////////////////////////
/// checkMovePossible
///////////////////////////////////////////
bool checkMovePossible(void)
{
    // On suppose que les valeurs sont à jour dans s_tSensorManager.t_tISensors
    for (int i = 0; i < SENSOR_MANAGER_SENSORS_COUNT; i++)
    {

        if (i == 1)
        {
            if (s_tSensorManager.t_tISensors[i] < SENSOR_OBSTACLE_THRESHOLD)
            {
                // Un capteur détecte un obstacle : mouvement impossible
                X_LOG_TRACE("Obstacle détecté devant");
                return false;
            }
        }
        else
        {
            if (s_tSensorManager.t_tISensors[i] < (SENSOR_OBSTACLE_THRESHOLD / 4))
            {
                // Un capteur détecte un obstacle : mouvement impossible
                X_LOG_TRACE("Obstacle détecté coté");
                return false;
            }
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

    // TODO Nico: gérer le roleRobot :
    // Si Explo : updateVision à rajouter
    // Si Inter : rien

    // TODO Nico : fonction capteur sol luminosité pour zone d'intérêt

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
    return osTaskStop(&s_tSensorManager.t_tTaskHandler, 10);
}

uint16_t updateVision(int sensor)
{
    X_ASSERT(sensor >= 0 && sensor < SENSOR_MANAGER_SENSORS_COUNT - 2);
    uint16_t raw = s_tSensorManager.t_tISensors[sensor];
    return rawValuesToMm(raw);
}

///////////////////////////////////////////
/// updateSensorData
///////////////////////////////////////////
static void updateSensorData(void *p_pvParam)
{
    (void)p_pvParam; // warning suppression

    int32_t l_iRet = 0;

    l_iRet = GetSensorValues(s_tSensorManager.t_tISensors);
    if (l_iRet != 0)
    {
        X_LOG_TRACE("Error getting sensor values");
    }

    for (int i = 0; i < SENSOR_MANAGER_SENSORS_COUNT; i++)
    {
        s_tSensorManager.t_tISensors[i] = rawValuesToMm(s_tSensorManager.t_tISensors[i]);
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
static void *sensorManagerTask(void *p_pvParam)
{
    (void)p_pvParam; // warning suppression

    // Start the timer with the correct period (100ms)
    xTimerStart(&s_tSensorManager.t_tTimer);

    X_LOG_TRACE("Starting sensor monitoring with period of %d ms", SENSOR_MANAGER_TASK_PERIOD);

    while (s_tSensorManager.t_tTaskHandler.a_iStopFlag == OS_TASK_SECURE_FLAG)
    {
        int l_iPeriods = xTimerProcessElapsedPeriods(&s_tSensorManager.t_tTimer, updateSensorData, NULL);

        if (l_iPeriods < 0)
        {
            // Negative return value indicates an error in the timer function
            X_LOG_TRACE("Timer processing error: %d", l_iPeriods);
        }

        // Sleep to prevent CPU hogging
        xTimerDelay(SENSOR_MANAGER_TASK_PERIOD);
    }

    xTimerStop(&s_tSensorManager.t_tTimer);

    return NULL;
}

///////////////////////////////////////////
/// rawValuesToMm
///////////////////////////////////////////
uint16_t rawValuesToMm(uint16_t rawValue)
{

    if (rawValue > 255)
        rawValue = 255;

    uint16_t converted_value
        = (rawValue * (SENSOR_MAX_MM_VALUE - SENSOR_MIN_MM_VALUE)) / (SENSOR_MAX_RAW_VALUE - SENSOR_MIN_RAW_VALUE)
          + SENSOR_MIN_MM_VALUE;
    // X_LOG_TRACE("Conversion: %d -> %d", rawValue, converted_value);
    return (uint16_t)converted_value;
}
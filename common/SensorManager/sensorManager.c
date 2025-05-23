////////////////////////////////////////////////////////////
//  sensorManager source file
//  Provides sensorManager interface
//
// general disclosure: copy or share the file is forbidden
// Written : 21/05/2025
////////////////////////////////////////////////////////////

#include "sensorManager.h"

static sensorManager_t s_tSensorManager;

#define SENSOR_MANAGER_TASK_PERIOD 100

// prototypes
static void *sensorManagerTask(void *p_pvParam);
static void updateSensorData(void *p_pvParam);

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
    s_tSensorManager.t_tTaskHandler.t_ulStackSize = OS_TASK_DEFAULT_STACK_SIZE;
    s_tSensorManager.t_tTaskHandler.a_iStopFlag = OS_TASK_SECURE_FLAG;

    return SENSOR_MANAGER_OK;
}

///////////////////////////////////////////
/// startMonitoring
///////////////////////////////////////////
int startMonitoring(void)
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
    return osTaskStop(&s_tSensorManager.t_tTaskHandler, 10);
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
        xTimerDelay(SENSOR_MANAGER_TASK_PERIOD / 4);
    }

    xTimerStop(&s_tSensorManager.t_tTimer);

    return NULL;
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
}

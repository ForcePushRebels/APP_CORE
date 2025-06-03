////////////////////////////////////////////////////////////
//  supervisor implementation file
//  Provides supervisor implementation
//
// general disclosure: copy or share the file is forbidden
// Written : 23/05/2025
////////////////////////////////////////////////////////////

#include "supervisor.h"

////////////////////////////////////////////////////////////
/// Private variables
////////////////////////////////////////////////////////////
static tSupervisorCtx s_tSupervisorCtx;
#define SUPERVISOR_TIMER_PERIOD_MS 1000  // 1 second period

typedef struct position_to_send_t
{
    int16_t x_mm;
    int16_t y_mm;
    float angle_rad;
}position_to_send_t;
/*
//mettre local
Position g_tPosition;
Status  g_tStatus;
Duration g_tDuration;
Speed s_tSpeed;
int s_tBatteryLevel;
FragmentMap s_tFragmentMap;

//xOsTimerCtx s_tTimer;

////////////////////////////////////////////////////////////
/// Private function implementations
////////////////////////////////////////////////////////////
static int32_t sendFragmentMap(Position pNewPosition)
{
    return networkServerSendMessage(1, ID_MAP_FRAGMENT, &g_tFragmentMap, sizeof(tFragmentMap));
}

static int32_t sendPosition(Position_t pNewPosition)
{
    position_to_send_t pPositionToSend = {
        .x_mm = (int)pNewPosition->x_mm;
        .y_mm = (int)pNewPosition->y_mm;
        .angle_rad = (float)pNewPosition->angle;
    }

    return networkServerSendMessage(1, ID_INF_POS, &pPositionToSend, sizeof(pPositionToSend));
}

static int32_t sendStatus(void)
{
    return networkServerSendMessage(1, ID_INF_STATUS, &s_tStatus, sizeof(tStatus));
}

static int32_t sendDuration(void)
{
    return networkServerSendMessage(1, ID_INF_TIME, &s_tDuration, sizeof(tDuration));
}

static int32_t sendBatteryLevel(void)
{
    return networkServerSendMessage(1, ID_INF_BATTERY, &s_iBatteryLevel, sizeof(int32_t));
}

////////////////////////////////////////////////////////////
/// Public function implementations
////////////////////////////////////////////////////////////
int32_t supervisor_init(void)
{
    int32_t l_iResult;
    
    // Initialize mutex
    l_iResult = mutexCreate(&s_tSupervisorCtx.t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        return l_iResult;
    }
    
    // Initialize timer
    l_iResult = xTimerCreate(&s_tSupervisorCtx.t_tTask.t_tTimer, 
                           SUPERVISOR_TIMER_PERIOD_MS,
                           XOS_TIMER_MODE_PERIODIC);
    if (l_iResult != XOS_TIMER_OK)
    {
        mutexDestroy(&s_tSupervisorCtx.t_tMutex);
        return l_iResult;
    }
    
    // Initialize local data
    memset(&s_tPosition, 0, sizeof(tPosition));
    memset(&s_tStatus, 0, sizeof(tStatus));
    memset(&s_tDuration, 0, sizeof(tDuration));
    memset(&s_tSpeed, 0, sizeof(tSpeed));
    s_iBatteryLevel = 0;
    memset(&s_tFragmentMap, 0, sizeof(tFragmentMap));
    
    return 0;
}

int32_t supervisor_shutdown(void)
{
    // Stop timer if active
    if (s_tSupervisorCtx.t_tTask.t_tTimer.t_ucActive)
    {
        xTimerStop(&s_tSupervisorCtx.t_tTask.t_tTimer);
    }
    
    // Destroy mutex
    mutexDestroy(&s_tSupervisorCtx.t_tMutex);
    
    return 0;
}

int32_t supervisor_start(void)
{
    int32_t l_iResult;
    
    // Start timer with checkInfo callback
    l_iResult = xTimerStart(&s_tSupervisorCtx.t_tTask.t_tTimer);
    if (l_iResult != XOS_TIMER_OK)
    {
        return l_iResult;
    }
    
    // Process timer events
    l_iResult = xTimerProcessElapsedPeriods(&s_tSupervisorCtx.t_tTask.t_tTimer, checkInfo, &s_tSupervisorCtx);
    if (l_iResult < 0)
    {
        xTimerStop(&s_tSupervisorCtx.t_tTask.t_tTimer);
        return l_iResult;
    }
    
    return 0;
}

int32_t supervisor_stop(void)
{
    return xTimerStop(&s_tSupervisorCtx.t_tTask.t_tTimer);
}

static void checkInfo(void* arg)
{
    X_ASSERT(arg != NULL);
    
    // Send all information

    networkServerSendMessage(1,ID_INF_BATTERY,*s_tBatteryLevel,)

    sendPosition();
    sendStatus();
    sendDuration();
    sendBatteryLevel();
    sendFragmentMap();
}


////////////////////////////////////////////////////////////
/// checkEndCondition
////////////////////////////////////////////////////////////
/*
static bool checkEndCondition()
{
    X_ASSERT(false); //not implemented 
    return false;
}
*/


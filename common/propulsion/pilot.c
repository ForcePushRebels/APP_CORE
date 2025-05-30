////////////////////////////////////////////////////////////
//  pilot.c
//  implementation of the pilot module
//
// general discloser: copy or share the file is forbidden
// Written : 23/05/2025
// Modified: 30/05/2025 - Fixed security issues
////////////////////////////////////////////////////////////

#include "pilot.h"
#include "xLog.h"


/////////////////////////////////
/// @brief Pilot global variables
/////////////////////////////////
static Pilot g_pilot;
static PilotState g_state = PILOT_STATE_WAIT_MOVE;


/////////////////////////////////
/// @brief Internal prototypes
/////////////////////////////////
static void *pilot_task(void* p_pttArg);
static int move_queue_init(moveQueue_t* p_pttMoveQueue);
static bool move_queue_push(moveQueue_t* p_pttMoveQueue, Move *m);
static bool move_queue_pop(moveQueue_t* p_pttMoveQueue, Move *m);
static int move_queue_size(moveQueue_t* p_pttMoveQueue);
static int event_queue_init(evtQueue_t* p_pttEvtQueue);
static bool event_queue_push(evtQueue_t* p_pttEvtQueue, PilotEvent evt);
static bool event_queue_pop(evtQueue_t* p_pttEvtQueue, PilotEvent *evt);
static void pilot_post_event(PilotEvent evt);

/////////////////////////////////
/// @brief Pilot actions
/////////////////////////////////
void pilot_action_computeAdvance(void* arg);
void pilot_action_computeContinuousAdvance(void* arg);
void pilot_action_computeTurn(void* arg);
void pilot_action_computeGoTo(void* arg);
void pilot_action_startMoves(void* arg);
void pilot_action_handleMove(void* arg);
void pilot_action_computeStop(void* arg);
void pilot_action_endMove(void* arg);
void pilot_action_nextMove(void* arg);
void pilot_action_check_next_move(void* arg);
void pilot_action_updatePosition(void* arg);
void pilot_action_emergencyStop(void* arg);

/////////////////////////////////
/// @brief Pilot transitions
/////////////////////////////////
pilot_transition_t pilot_transitions[PILOT_STATE_COUNT][PILOT_EVT_COUNT] = {
    [PILOT_STATE_WAIT_MOVE] = {
        [PILOT_EVT_ADVANCE] = {.next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeAdvance},
        [PILOT_EVT_CONTINUOUS_ADVANCE] = {.next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeContinuousAdvance},
        [PILOT_EVT_TURN] = {.next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeTurn},
        [PILOT_EVT_GOTO] = {.next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeGoTo},
    },
    [PILOT_STATE_COMPUTE_MOVE] = {
        [PILOT_EVT_START_MOVES] = {.next_state = PILOT_STATE_MOVING, .action = pilot_action_startMoves},
    },
    [PILOT_STATE_MOVING] = {
        [PILOT_EVT_END_MOVE] = {.next_state = PILOT_STATE_END_MOVE, .action = pilot_action_endMove},
        [PILOT_EVT_STOP] = {.next_state = PILOT_STATE_WAIT_MOVE, .action = pilot_action_emergencyStop},
    },
    [PILOT_STATE_END_MOVE] = {
        [PILOT_EVT_CHECK_NEXT_MOVE] = {.next_state = PILOT_STATE_CHECK_NEXT_MOVE, .action = pilot_action_check_next_move},
    },
    [PILOT_STATE_CHECK_NEXT_MOVE] = {
        [PILOT_EVT_NEXT_MOVE] = {.next_state = PILOT_STATE_MOVE_IN_PROGRESS, .action = pilot_action_nextMove},
        [PILOT_EVT_END_ALL_MOVES] = {.next_state = PILOT_STATE_WAIT_MOVE, .action = pilot_action_endMove},
    },
    [PILOT_STATE_MOVE_IN_PROGRESS] = {
        [PILOT_EVT_STOP] = {.next_state = PILOT_STATE_MOVE_IN_PROGRESS, .action = pilot_action_computeStop},
        [PILOT_EVT_END_MOVE] = {.next_state = PILOT_STATE_END_MOVE, .action = pilot_action_endMove},
    },
};


/////////////////////////////////
/// move_queue_init
/////////////////////////////////
static int move_queue_init(moveQueue_t* p_pttMoveQueue)
{
    X_ASSERT(p_pttMoveQueue != NULL);

    int l_iret = 0;

    XOS_MEMORY_SANITIZE(p_pttMoveQueue, sizeof(*p_pttMoveQueue));

    l_iret = mutexCreate(&p_pttMoveQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_init: mutexCreate failed");
        return l_iret;
    }
    return PILOT_OK;
}

/////////////////////////////////
/// move_queue_push
/////////////////////////////////
static bool move_queue_push(moveQueue_t* p_pttMoveQueue, Move *p_pttMove)
{
    X_ASSERT(p_pttMoveQueue != NULL);
    X_ASSERT(p_pttMove != NULL);

    bool l_bReturn = false;
    int l_iret = 0;
    l_iret = mutexLock(&p_pttMoveQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_push: mutexLock failed");
        return l_bReturn;
    }

    if (p_pttMoveQueue->count < 16)
    {
        p_pttMoveQueue->moves[p_pttMoveQueue->tail] = *p_pttMove;
        p_pttMoveQueue->tail = (p_pttMoveQueue->tail + 1) % 16;
        p_pttMoveQueue->count++;
        l_bReturn = true;
    }
    else
    {
        X_LOG_TRACE("move_queue_push: queue FULL!");
    }

    l_iret = mutexUnlock(&p_pttMoveQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_push: mutexUnlock failed");
        return l_bReturn;
    }
    return l_bReturn;
}

/////////////////////////////////
/// move_queue_pop
/////////////////////////////////
static bool move_queue_pop(moveQueue_t* p_pttMoveQueue, Move *p_pttMove)
{
    X_ASSERT(p_pttMoveQueue != NULL);
    X_ASSERT(p_pttMove != NULL);

    bool l_bReturn = false;
    int l_iret = 0;
    l_iret = mutexLock(&p_pttMoveQueue->mutex);

    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_pop: mutexLock failed");
        return l_bReturn;
    }

    if (p_pttMoveQueue->count > 0)
    {
        *p_pttMove = p_pttMoveQueue->moves[p_pttMoveQueue->head];
        p_pttMoveQueue->head = (p_pttMoveQueue->head + 1) % 16;
        p_pttMoveQueue->count--;
        l_bReturn = true;
    }

    l_iret = mutexUnlock(&p_pttMoveQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_pop: mutexUnlock failed");
        return l_bReturn;
    }
    return l_bReturn;
}

/////////////////////////////////
/// move_queue_size
/////////////////////////////////
static int move_queue_size(moveQueue_t* p_pttMoveQueue)
{
    X_ASSERT(p_pttMoveQueue != NULL);

    int l_iret = 0;
    int l_iReturn = 0;

    l_iret = mutexLock(&p_pttMoveQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_size: mutexLock failed");
        return l_iReturn;
    }

    l_iReturn = p_pttMoveQueue->count;

    l_iret = mutexUnlock(&p_pttMoveQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("move_queue_size: mutexUnlock failed");
    }
    return l_iReturn;
}

/////////////////////////////////
/// event_queue_init
/////////////////////////////////
static int event_queue_init(evtQueue_t* p_pttEvtQueue)
{
    X_ASSERT(p_pttEvtQueue != NULL);

    int l_iret = 0;

    XOS_MEMORY_SANITIZE(p_pttEvtQueue, sizeof(*p_pttEvtQueue));

    l_iret = mutexCreate(&p_pttEvtQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("event_queue_init: mutexCreate failed");
        return l_iret;
    }
    return PILOT_OK;
}

/////////////////////////////////
/// event_queue_push
/////////////////////////////////
static bool event_queue_push(evtQueue_t* p_pttEvtQueue, PilotEvent evt)
{
    X_ASSERT(p_pttEvtQueue != NULL);

    bool l_bReturn = false;
    int l_iret = 0;

    l_iret = mutexLock(&p_pttEvtQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("event_queue_push: mutexLock failed");
        return l_bReturn;
    }

    if (p_pttEvtQueue->count < 16)
    {
        p_pttEvtQueue->events[p_pttEvtQueue->tail] = evt;
        p_pttEvtQueue->tail = (p_pttEvtQueue->tail + 1) % 16;
        p_pttEvtQueue->count++;
        l_bReturn = true;
    }
    else
    {
        X_LOG_TRACE("event_queue_push: queue FULL!");
    }

    l_iret = mutexUnlock(&p_pttEvtQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("event_queue_push: mutexUnlock failed");
    }
    return l_bReturn;
}

/////////////////////////////////
/// event_queue_pop
/////////////////////////////////
static bool event_queue_pop(evtQueue_t* p_pttEvtQueue, PilotEvent *p_pttEvt)
{
    X_ASSERT(p_pttEvtQueue != NULL);
    X_ASSERT(p_pttEvt != NULL);

    bool l_bReturn = false;
    int l_iret = 0;

    l_iret = mutexLock(&p_pttEvtQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("event_queue_pop: mutexLock failed");
        return l_bReturn;
    }

    if (p_pttEvtQueue->count > 0)
    {
        *p_pttEvt = p_pttEvtQueue->events[p_pttEvtQueue->head];
        p_pttEvtQueue->head = (p_pttEvtQueue->head + 1) % 16;
        p_pttEvtQueue->count--;
        l_bReturn = true;
    }

    l_iret = mutexUnlock(&p_pttEvtQueue->mutex);
    if (l_iret != MUTEX_OK)
    {
        X_LOG_TRACE("event_queue_pop: mutexUnlock failed");
    }
    return l_bReturn;
}

/////////////////////////////////
/// pilot_post_event
/////////////////////////////////
static void pilot_post_event(PilotEvent evt)
{
    event_queue_push(&g_pilot.evtQueue, evt);
}

/////////////////////////////////
/// pilot_task
/////////////////////////////////
static void *pilot_task(void* p_pttArg)
{
    (void)p_pttArg;

    while (g_pilot.pilotTask.a_iStopFlag == OS_TASK_SECURE_FLAG)
    {
        PilotEvent evt;
        if (event_queue_pop(&g_pilot.evtQueue, &evt))
        {
            pilot_transition_t *t = &pilot_transitions[g_state][evt];
            if (t->action)
            {
                t->action(NULL);
            }
            g_state = t->next_state;
        }
        else
        {
            if ((g_state == PILOT_STATE_MOVING || g_state == PILOT_STATE_MOVE_IN_PROGRESS) &&
                position_control_is_motion_finished())
            {
                pilot_post_event(PILOT_EVT_END_MOVE);
            }
            xTimerDelay(10);
        }
    }
    return NULL;
}

/////////////////////////////////
/// pilot_action_computeAdvance
/////////////////////////////////
void pilot_action_computeAdvance(void* arg)
{
    (void)arg;  // Unused parameter
    pilot_post_event(PILOT_EVT_START_MOVES);
}

/////////////////////////////////
/// pilot_action_computeContinuousAdvance
/////////////////////////////////
void pilot_action_computeContinuousAdvance(void* arg)
{
    (void)arg;  // Unused parameter
    pilot_post_event(PILOT_EVT_START_MOVES);
}

/////////////////////////////////
/// pilot_action_computeTurn
/////////////////////////////////
void pilot_action_computeTurn(void* arg)
{
    (void)arg;  // Unused parameter
    pilot_post_event(PILOT_EVT_START_MOVES);
}

/////////////////////////////////
/// pilot_action_computeGoTo
/////////////////////////////////
void pilot_action_computeGoTo(void* arg)
{
    (void)arg;  // Unused parameter
    pilot_post_event(PILOT_EVT_START_MOVES);
}

/////////////////////////////////
/// pilot_action_startMoves
/////////////////////////////////
void pilot_action_startMoves(void* arg)
{
    (void)arg;  // Unused parameter
    int sz = move_queue_size(&g_pilot.moveQueue);
    if (sz > 0)
    {
        Move move;
        move_queue_pop(&g_pilot.moveQueue, &move);
        if (move.angle_rad != 0.0)
        {
            position_control_turn(move.angle_rad, (float)move.max_speed);
        }
        else
        {
            position_control_advance(move.distance_mm, move.max_speed);
        }
    }
}

/////////////////////////////////
/// pilot_action_emergencyStop
/////////////////////////////////
void pilot_action_emergencyStop(void* arg)
{
    (void)arg;  // Unused parameter
    position_control_stop();
}

/////////////////////////////////
/// pilot_action_endMove
/////////////////////////////////
void pilot_action_endMove(void* arg)
{
    (void)arg;  // Unused parameter
    if (position_control_is_motion_finished())
    {
        pilot_post_event(PILOT_EVT_CHECK_NEXT_MOVE);
    }
}

/////////////////////////////////
/// pilot_action_nextMove
/////////////////////////////////
void pilot_action_nextMove(void* arg)
{
    (void)arg;  // Unused parameter
    pilot_action_startMoves(NULL);
}

/////////////////////////////////
/// pilot_action_check_next_move
/////////////////////////////////
void pilot_action_check_next_move(void* arg)
{
    (void)arg;  // Unused parameter
    if (move_queue_size(&g_pilot.moveQueue) > 0)
    {
        pilot_post_event(PILOT_EVT_NEXT_MOVE);
    }
    else
    {
        pilot_post_event(PILOT_EVT_END_ALL_MOVES);
    }
}

/////////////////////////////////
/// pilot_action_updatePosition
/////////////////////////////////
void pilot_action_updatePosition(void* arg)
{
    (void)arg;  // Unused parameter
    Position_t pos;
    if (position_control_get_position(&pos) == 0)
    {
        g_pilot.position.positionX = pos.x_mm;
        g_pilot.position.positionY = pos.y_mm;
        g_pilot.position.angle = pos.angle_rad;
    }
}

/////////////////////////////////
/// pilot_action_computeStop
/////////////////////////////////
void pilot_action_computeStop(void* arg)
{
    (void)arg;  // Unused parameter
    position_control_stop();
}

/////////////////////////////////
/// pilot_init
/////////////////////////////////
int32_t pilot_init()
{
    XOS_MEMORY_SANITIZE(&g_pilot, sizeof(g_pilot));
    XOS_MEMORY_SANITIZE(&g_pilot.moveQueue, sizeof(g_pilot.moveQueue));
    XOS_MEMORY_SANITIZE(&g_pilot.evtQueue, sizeof(g_pilot.evtQueue));

    int l_iret = 0;

    l_iret = move_queue_init(&g_pilot.moveQueue);
    if (l_iret != PILOT_OK)
    {
        X_LOG_TRACE("pilot_init: move_queue_init failed");
        return l_iret;
    }
    l_iret = event_queue_init(&g_pilot.evtQueue);
    if (l_iret != PILOT_OK)
    {
        X_LOG_TRACE("pilot_init: event_queue_init failed");
        return l_iret;
    }

    l_iret = osTaskInit(&g_pilot.pilotTask);
    if (l_iret != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("pilot_init: osTaskInit failed");
        return l_iret;
    }

    // Configure task function
    g_pilot.pilotTask.t_ptTask = pilot_task;
    g_pilot.pilotTask.t_ptTaskArg = NULL;

    l_iret = osTaskCreate(&g_pilot.pilotTask);
    if (l_iret != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("pilot_init: osTaskCreate failed");
        return l_iret;
    }

    return PILOT_OK;
}

int32_t pilot_shutdown(void)
{
    int32_t l_iret = PILOT_OK;
    atomic_store_explicit(&g_pilot.pilotTask.a_iStopFlag, OS_TASK_STOP_REQUEST , memory_order_relaxed);
    
    l_iret = osTaskStop(&g_pilot.pilotTask, 2);
    if (l_iret != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("pilot_shutdown: osTaskStop failed");
    }
    
    mutexDestroy(&g_pilot.moveQueue.mutex);
    mutexDestroy(&g_pilot.evtQueue.mutex);
    XOS_MEMORY_SANITIZE(&g_pilot, sizeof(g_pilot));
    
    return l_iret;
}

/////////////////////////////////
/// pilot_advance
/////////////////////////////////
void pilot_advance(double distance_mm, float max_speed)
{
    Move move = {
        .distance_mm = distance_mm,
        .angle_rad = 0.0,
        .max_speed = max_speed,
        .direction = DIR_FORWARD,
        .relative = true};
    move_queue_push(&g_pilot.moveQueue, &move);
    pilot_post_event(PILOT_EVT_ADVANCE);
}

/////////////////////////////////
/// pilot_continuousAdvance
/////////////////////////////////
void pilot_continuousAdvance(int max_speed)
{
    Move move = {
        .distance_mm = 1000000,
        .angle_rad = 0.0,
        .max_speed = max_speed,
        .direction = DIR_FORWARD,
        .relative = true};
    move_queue_push(&g_pilot.moveQueue, &move);
    pilot_post_event(PILOT_EVT_CONTINUOUS_ADVANCE);
}

/////////////////////////////////
/// pilot_turn
/////////////////////////////////
void pilot_turn(double angle_rad, int max_speed, bool relative)
{
    Move move = {
        .distance_mm = 0.0,
        .angle_rad = angle_rad,
        .max_speed = max_speed,
        .direction = (angle_rad > 0) ? DIR_LEFT : DIR_RIGHT,
        .relative = relative};
    move_queue_push(&g_pilot.moveQueue, &move);
    pilot_post_event(PILOT_EVT_TURN);
}

/////////////////////////////////
/// pilot_goTo
/////////////////////////////////
void pilot_goTo(double positionX, double positionY, int max_speed)
{
    (void)positionX;  
    (void)positionY;
    (void)max_speed;

    X_LOG_TRACE("Function seems to not be implemented");
    X_ASSERT(0);

    Move move = {
        .distance_mm = 0.0,
        .angle_rad = 0.0,
        .max_speed = max_speed,
        .direction = DIR_FORWARD,
        .relative = false
    };
    move_queue_push(&g_pilot.moveQueue, &move);
    pilot_post_event(PILOT_EVT_GOTO);
}

/////////////////////////////////
/// pilot_stop
/////////////////////////////////
void pilot_stop()
{
    pilot_post_event(PILOT_EVT_STOP);
}

/////////////////////////////////
/// pilot_getPosition
/////////////////////////////////
int32_t pilot_getPosition(Position* p_pttPosition) 
{ 
    X_ASSERT(p_pttPosition != NULL);
    p_pttPosition->positionX = g_pilot.position.positionX;
    p_pttPosition->positionY = g_pilot.position.positionY;
    p_pttPosition->angle = g_pilot.position.angle;
    return PILOT_OK;
}

/////////////////////////////////
/// pilot_getSpeed
/////////////////////////////////
int pilot_getSpeed(void) 
{ 
    return g_pilot.targetSpeed; 
}

/////////////////////////////////
/// pilot_getDistanceMeter
/////////////////////////////////
int pilot_getDistanceMeter(void) 
{ 
    return (int)g_pilot.distanceMeter; 
}

/////////////////////////////////
/// pilot_resetDistanceMeter
/////////////////////////////////
void pilot_resetDistanceMeter(void) 
{ 
    g_pilot.distanceMeter = 0.0; 
}

/////////////////////////////////
/// pilot_setAcceleration
/////////////////////////////////
void pilot_setAcceleration(double linearAcceleration, double angularAcceleration)
{
    (void)linearAcceleration;
    (void)angularAcceleration;
    X_LOG_TRACE("Function seems to not be implemented");
    X_ASSERT(0);
}
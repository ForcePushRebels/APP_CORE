#include "pilot.h"
#include "xTask.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xLog.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "positionControl.h"

// --- Types internes ---
typedef struct
{
    Move moves[16];
    int head, tail, count;
    xOsMutexCtx mutex;
} MoveQueue;

typedef struct
{
    PilotEvent events[16];
    int head, tail, count;
    xOsMutexCtx mutex;
} EventQueue;

// --- Variables globales ---
static Pilot g_pilot;
static PilotState g_state = PILOT_STATE_WAIT_MOVE;
static MoveQueue g_moveQueue;
static EventQueue g_eventQueue;
static xOsTaskCtx g_pilotTask;
static bool g_running = false;

// --- Prototypes internes ---
static void *pilot_task();
static void move_queue_init(MoveQueue *q);
static bool move_queue_push(MoveQueue *q, Move *m);
static bool move_queue_pop(MoveQueue *q, Move *m);
static int move_queue_size(MoveQueue *q);
static void event_queue_init(EventQueue *q);
static bool event_queue_push(EventQueue *q, PilotEvent evt);
static bool event_queue_pop(EventQueue *q, PilotEvent *evt);
static void pilot_post_event(PilotEvent evt);

// --- Table de transitions ---
void pilot_action_computeAdvance();
void pilot_action_computeContinuousAdvance();
void pilot_action_computeTurn();
void pilot_action_computeGoTo();
void pilot_action_startMoves();
void pilot_action_handleMove();
void pilot_action_computeStop();
void pilot_action_endMove();
void pilot_action_nextMove();
void pilot_action_check_next_move();
void pilot_action_updatePosition();
void pilot_action_emergencyStop();

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

// --- Implémentation des queues ---
static void move_queue_init(MoveQueue *q)
{
    memset(q, 0, sizeof(*q));
    mutexCreate(&q->mutex);
}
static bool move_queue_push(MoveQueue *q, Move *m)
{
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count < 16)
    {
        q->moves[q->tail] = *m;
        q->tail = (q->tail + 1) % 16;
        q->count++;
        ok = true;
        X_LOG_TRACE("move_queue_push: count=%d, move=[distance=%.2f, angle=%.2f, speed=%d]", q->count, m->distance_mm, m->angle_rad, m->max_speed);
    }
    else
    {
        X_LOG_TRACE("move_queue_push: queue FULL!");
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static bool move_queue_pop(MoveQueue *q, Move *m)
{
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count > 0)
    {
        *m = q->moves[q->head];
        q->head = (q->head + 1) % 16;
        q->count--;
        ok = true;
        X_LOG_TRACE("move_queue_pop: count=%d, move=[distance=%.2f, angle=%.2f, speed=%d]", q->count, m->distance_mm, m->angle_rad, m->max_speed);
    }
    else
    {
        X_LOG_TRACE("move_queue_pop: queue EMPTY!");
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static int move_queue_size(MoveQueue *q)
{
    int sz;
    mutexLock(&q->mutex);
    sz = q->count;
    mutexUnlock(&q->mutex);
    return sz;
}
static void event_queue_init(EventQueue *q)
{
    memset(q, 0, sizeof(*q));
    mutexCreate(&q->mutex);
}
static bool event_queue_push(EventQueue *q, PilotEvent evt)
{
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count < 16)
    {
        q->events[q->tail] = evt;
        q->tail = (q->tail + 1) % 16;
        q->count++;
        X_LOG_TRACE("event_queue_push: count=%d, evt=%d", q->count, evt);
    }
    else
    {
        X_LOG_TRACE("event_queue_push: queue FULL!");
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static bool event_queue_pop(EventQueue *q, PilotEvent *evt)
{
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count > 0)
    {
        *evt = q->events[q->head];
        q->head = (q->head + 1) % 16;
        q->count--;
        ok = true;
        X_LOG_TRACE("event_queue_pop: count=%d, evt=%d", q->count, *evt);
    }
    else
    {
        X_LOG_TRACE("event_queue_pop: queue EMPTY!");
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static void pilot_post_event(PilotEvent evt)
{
    X_LOG_TRACE("pilot_post_event: evt=%d", evt);
    event_queue_push(&g_eventQueue, evt);
}

// --- Thread principal Pilot ---
static void *pilot_task()
{
    g_running = true;
    while (g_running)
    {
        X_LOG_TRACE("pilot_task running, state=%d", g_state);
        PilotEvent evt;
        if (event_queue_pop(&g_eventQueue, &evt))
        {
            X_LOG_TRACE("pilot_task: event popped: %d in state %d", evt, g_state);
            pilot_transition_t *t = &pilot_transitions[g_state][evt];
            if (t->action)
            {
                X_LOG_TRACE("pilot_task: calling action for evt=%d, state=%d", evt, g_state);
                t->action(NULL);
            }
            g_state = t->next_state;
            X_LOG_TRACE("pilot_task: after action, new state=%d", g_state);
        }
        else
        {
            if ((g_state == PILOT_STATE_MOVING || g_state == PILOT_STATE_MOVE_IN_PROGRESS) &&
                position_control_is_motion_finished())
            {
                X_LOG_TRACE("pilot_task: Detected motion finished, posting END_MOVE");
                pilot_post_event(PILOT_EVT_END_MOVE);
            }
            xTimerDelay(10);
        }
    }
    return NULL;
}

// --- Actions de la machine à états (exemples à compléter) ---
void pilot_action_computeAdvance()
{
    X_LOG_TRACE("pilot_action_computeAdvance");
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_computeContinuousAdvance()
{
    X_LOG_TRACE("pilot_action_computeContinuousAdvance");
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_computeTurn()
{
    X_LOG_TRACE("pilot_action_computeTurn");
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_computeGoTo()
{
    X_LOG_TRACE("pilot_action_computeGoTo");
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_startMoves()
{
    int sz = move_queue_size(&g_moveQueue);
    X_LOG_TRACE("pilot_action_startMoves: move_queue_size=%d", sz);
    if (sz > 0)
    {
        Move move;
        move_queue_pop(&g_moveQueue, &move);
        if (move.angle_rad != 0.0)
        {
            X_LOG_TRACE("pilot_action_startMoves: angle_rad=%.6f, distance_mm=%.2f", move.angle_rad, move.distance_mm);
            position_control_turn(move.angle_rad, (float)move.max_speed);
        }
        else
        {
            X_LOG_TRACE("pilot_action_startMoves: advance distance_mm=%.2f, speed=%d", move.distance_mm, move.max_speed);
            position_control_advance(move.distance_mm, move.max_speed);
        }
    }
    else
    {
        X_LOG_TRACE("pilot_action_startMoves: queue empty, nothing to do");
    }
}
void pilot_action_emergencyStop()
{
    X_LOG_TRACE("pilot_action_emergencyStop");
    position_control_stop();
}
void pilot_action_endMove()
{
    X_LOG_TRACE("pilot_action_endMove");
    if (position_control_is_motion_finished())
    {
        X_LOG_TRACE("pilot_action_endMove: motion finished, posting CHECK_NEXT_MOVE");
        pilot_post_event(PILOT_EVT_CHECK_NEXT_MOVE);
    }
    else
    {
        X_LOG_TRACE("pilot_action_endMove: motion NOT finished");
    }
}
void pilot_action_nextMove()
{
    X_LOG_TRACE("pilot_action_nextMove");
    pilot_action_startMoves(NULL);
}
void pilot_action_check_next_move()
{
    X_LOG_TRACE("pilot_action_check_next_move");
    if (move_queue_size(&g_moveQueue) > 0)
    {
        X_LOG_TRACE("pilot_action_check_next_move: moves left, posting NEXT_MOVE");
        pilot_post_event(PILOT_EVT_NEXT_MOVE);
    }
    else
    {
        X_LOG_TRACE("pilot_action_check_next_move: no moves left, posting END_ALL_MOVES");
        pilot_post_event(PILOT_EVT_END_ALL_MOVES);
    }
}
void pilot_action_updatePosition()
{
    Position_t pos;
    if (position_control_get_position(&pos) == 0)
    {
        g_pilot.position.x = pos.x_mm;
        g_pilot.position.y = pos.y_mm;
        g_pilot.position.theta = pos.angle_rad;
        X_LOG_TRACE("pilot_action_updatePosition: x=%.1f, y=%.1f, theta=%.2f", pos.x_mm, pos.y_mm, pos.angle_rad);
    }
}
void pilot_action_computeStop()
{
    X_LOG_TRACE("pilot_action_computeStop");
    position_control_stop();
}

// --- Interface publique ---
int32_t pilot_init()
{
    memset(&g_pilot, 0, sizeof(g_pilot));
    move_queue_init(&g_moveQueue);
    event_queue_init(&g_eventQueue);

    osTaskInit(&g_pilotTask);
    g_pilotTask.t_ptTask = pilot_task;
    osTaskCreate(&g_pilotTask);

    g_running = true;
    return 0;
}

void pilot_shutdown(void)
{
    g_running = false;
    osTaskStop(&g_pilotTask, 2);
    osTaskWait(&g_pilotTask, NULL);
    mutexDestroy(&g_moveQueue.mutex);
    mutexDestroy(&g_eventQueue.mutex);
}

// Commandes de mouvement (exemples)
void pilot_advance(double distance_mm, float max_speed)
{
    X_LOG_TRACE("pilot_advance called: %f mm, %d", distance_mm, max_speed);

    Move move = {
        .distance_mm = distance_mm,
        .angle_rad = 0.0,
        .max_speed = max_speed,
        .direction = DIR_FORWARD,
        .relative = true};
    move_queue_push(&g_moveQueue, &move);
    pilot_post_event(PILOT_EVT_ADVANCE);
}
void pilot_continuousAdvance(int max_speed)
{
    Move move = {
        .distance_mm = 1000000, // Simule une très grande distance
        .angle_rad = 0.0,
        .max_speed = max_speed,
        .direction = DIR_FORWARD,
        .relative = true};
    move_queue_push(&g_moveQueue, &move);
    pilot_post_event(PILOT_EVT_CONTINUOUS_ADVANCE);
}
void pilot_turn(double angle_rad, int max_speed, bool relative)
{

    X_LOG_TRACE("pilot_turn called: %f rad, %d, %d", angle_rad, max_speed, relative);

    Move move = {
        .distance_mm = 0.0,
        .angle_rad = angle_rad,
        .max_speed = max_speed,
        .direction = (angle_rad > 0) ? DIR_LEFT : DIR_RIGHT,
        .relative = relative};
    move_queue_push(&g_moveQueue, &move);
    pilot_post_event(PILOT_EVT_TURN);
}
void pilot_goTo(double x, double y, int max_speed)
{
    Move move = {
        .distance_mm = 0.0, // À calculer dans l'action si besoin
        .angle_rad = 0.0,   // À calculer dans l'action si besoin
        .max_speed = max_speed,
        .direction = DIR_FORWARD,
        .relative = false
        // Tu peux ajouter x/y dans Move si besoin pour le goTo
    };
    // Si Move ne contient pas x/y, adapte la logique dans pilot_action_computeGoTo
    move_queue_push(&g_moveQueue, &move);
    pilot_post_event(PILOT_EVT_GOTO);
}
void pilot_stop()
{
    X_LOG_TRACE("pilot_stop called ");

    pilot_post_event(PILOT_EVT_STOP);
}

// Accesseurs (à compléter)
Position pilot_getPosition(void) { return g_pilot.position; }
int pilot_getSpeed(void) { return g_pilot.targetSpeed; }
int pilot_getDistanceMeter(void) { return (int)g_pilot.distanceMeter; }
void pilot_resetDistanceMeter(void) { g_pilot.distanceMeter = 0.0; }
void pilot_setAcceleration(double linearAcceleration, double angularAcceleration)
{
    (void)linearAcceleration;
    (void)angularAcceleration;
}
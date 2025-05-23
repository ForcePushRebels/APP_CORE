#include "pilot.h"
#include "xTask.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xLog.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// --- Types internes ---
typedef struct {
    Move moves[16];
    int head, tail, count;
    xOsMutexCtx mutex;
} MoveQueue;

typedef struct {
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
static xOsTaskCtx g_positionWatcherTask;
static bool g_running = false;

// --- Prototypes internes ---
static void* pilot_task(void* arg);
static void* positionWatcher_task(void* arg);
static void move_queue_init(MoveQueue* q);
static bool move_queue_push(MoveQueue* q, Move* m);
static bool move_queue_pop(MoveQueue* q, Move* m);
static int move_queue_size(MoveQueue* q);
static void event_queue_init(EventQueue* q);
static bool event_queue_push(EventQueue* q, PilotEvent evt);
static bool event_queue_pop(EventQueue* q, PilotEvent* evt);
static void pilot_post_event(PilotEvent evt);

// --- Table de transitions ---
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

pilot_transition_t pilot_transitions[PILOT_STATE_COUNT][PILOT_EVT_COUNT] = {
    [PILOT_STATE_WAIT_MOVE] = {
        [PILOT_EVT_ADVANCE]            = { .next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeAdvance },
        [PILOT_EVT_CONTINUOUS_ADVANCE] = { .next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeContinuousAdvance },
        [PILOT_EVT_TURN]               = { .next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeTurn },
        [PILOT_EVT_GOTO]               = { .next_state = PILOT_STATE_COMPUTE_MOVE, .action = pilot_action_computeGoTo },
    },
    [PILOT_STATE_COMPUTE_MOVE] = {
        [PILOT_EVT_START_MOVES]        = { .next_state = PILOT_STATE_MOVING, .action = pilot_action_startMoves },
    },
    [PILOT_STATE_MOVING] = {
        [PILOT_EVT_END_MOVE]           = { .next_state = PILOT_STATE_END_MOVE, .action = pilot_action_endMove },
        [PILOT_EVT_STOP]     = { .next_state = PILOT_STATE_WAIT_MOVE, .action = pilot_action_emergencyStop },
    },
    [PILOT_STATE_END_MOVE] = {
        [PILOT_EVT_CHECK_NEXT_MOVE]    = { .next_state = PILOT_STATE_CHECK_NEXT_MOVE, .action = pilot_action_check_next_move },
    },
    [PILOT_STATE_CHECK_NEXT_MOVE] = {
        [PILOT_EVT_NEXT_MOVE]          = { .next_state = PILOT_STATE_MOVE_IN_PROGRESS, .action = pilot_action_nextMove },
        [PILOT_EVT_END_ALL_MOVES]      = { .next_state = PILOT_STATE_WAIT_MOVE, .action = pilot_action_endMove },
    },
    [PILOT_STATE_MOVE_IN_PROGRESS] = {
        [PILOT_EVT_STOP]               = { .next_state = PILOT_STATE_MOVE_IN_PROGRESS, .action = pilot_action_computeStop },
        [PILOT_EVT_END_MOVE]           = { .next_state = PILOT_STATE_END_MOVE, .action = pilot_action_endMove },
    },
};

// --- Implémentation des queues ---
static void move_queue_init(MoveQueue* q) {
    memset(q, 0, sizeof(*q));
    mutexCreate(&q->mutex);
}
static bool move_queue_push(MoveQueue* q, Move* m) {
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count < 16) {
        q->moves[q->tail] = *m;
        q->tail = (q->tail + 1) % 16;
        q->count++;
        ok = true;
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static bool move_queue_pop(MoveQueue* q, Move* m) {
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count > 0) {
        *m = q->moves[q->head];
        q->head = (q->head + 1) % 16;
        q->count--;
        ok = true;
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static int move_queue_size(MoveQueue* q) {
    int sz;
    mutexLock(&q->mutex);
    sz = q->count;
    mutexUnlock(&q->mutex);
    return sz;
}
static void event_queue_init(EventQueue* q) {
    memset(q, 0, sizeof(*q));
    mutexCreate(&q->mutex);
}
static bool event_queue_push(EventQueue* q, PilotEvent evt) {
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count < 16) {
        q->events[q->tail] = evt;
        q->tail = (q->tail + 1) % 16;
        q->count++;
        ok = true;
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static bool event_queue_pop(EventQueue* q, PilotEvent* evt) {
    bool ok = false;
    mutexLock(&q->mutex);
    if (q->count > 0) {
        *evt = q->events[q->head];
        q->head = (q->head + 1) % 16;
        q->count--;
        ok = true;
    }
    mutexUnlock(&q->mutex);
    return ok;
}
static void pilot_post_event(PilotEvent evt) {
    event_queue_push(&g_eventQueue, evt);
}

// --- Thread principal Pilot ---
static void* pilot_task(void* arg) {
    (void)arg;
    g_running = true;
    while (g_running) {
        PilotEvent evt;
        if (event_queue_pop(&g_eventQueue, &evt)) {
            pilot_transition_t* t = &pilot_transitions[g_state][evt];
            if (t->action) t->action(NULL);
            g_state = t->next_state;
        } else {
            xTimerDelay(10);
        }
    }
    return NULL;
}

// --- Thread positionWatcher ---
static void* positionWatcher_task(void* arg) {
    (void)arg;
    while (g_running) {
        // Lecture encodeurs, mise à jour position
        pilot_action_updatePosition(NULL);
        xTimerDelay(10); // 100 Hz
    }
    return NULL;
}

// --- Actions de la machine à états (exemples à compléter) ---
void pilot_action_computeAdvance(void* arg) {
    // Prépare un Move d'avance, l'ajoute à la moveQueue
    // ...
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_computeContinuousAdvance(void* arg) {
    // ...
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_computeTurn(void* arg) {
    // ...
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_computeGoTo(void* arg) {
    // ...
    pilot_post_event(PILOT_EVT_START_MOVES);
}
void pilot_action_startMoves(void* arg) {
    // Démarre le premier move de la queue
    // ...
}
void pilot_action_handleMove(void* arg) {
    // Gère le move courant
    // ...
}
void pilot_action_computeStop(void* arg) {
    // ...
}
void pilot_action_endMove(void* arg) {
    // ...
    pilot_post_event(PILOT_EVT_CHECK_NEXT_MOVE);
}
void pilot_action_nextMove(void* arg) {
    // ...
}
void pilot_action_check_next_move(void* arg) {
    if (move_queue_size(&g_moveQueue) > 0) {
        pilot_post_event(PILOT_EVT_NEXT_MOVE);
    } else {
        pilot_post_event(PILOT_EVT_END_ALL_MOVES);
    }
}
void pilot_action_updatePosition(void* arg) {
    // Lire les encodeurs, mettre à jour g_pilot.position
}
void pilot_action_emergencyStop(void* arg) {
    // Arrêt d'urgence
}

// --- Interface publique ---
int32_t pilot_init(int max_speed, double wheel_radius_m, double wheel_base_m) {
    memset(&g_pilot, 0, sizeof(g_pilot));
    g_pilot.targetSpeed = max_speed;
    move_queue_init(&g_moveQueue);
    event_queue_init(&g_eventQueue);

    osTaskInit(&g_pilotTask);
    g_pilotTask.t_ptTask = pilot_task;
    osTaskCreate(&g_pilotTask);

    osTaskInit(&g_positionWatcherTask);
    g_positionWatcherTask.t_ptTask = positionWatcher_task;
    osTaskCreate(&g_positionWatcherTask);

    g_running = true;
    return 0;
}

void pilot_shutdown(void) {
    g_running = false;
    osTaskStop(&g_pilotTask, 2);
    osTaskWait(&g_pilotTask, NULL);
    osTaskStop(&g_positionWatcherTask, 2);
    osTaskWait(&g_positionWatcherTask, NULL);
    mutexDestroy(&g_moveQueue.mutex);
    mutexDestroy(&g_eventQueue.mutex);
}

// Commandes de mouvement (exemples)
void pilot_advance(double distance_mm, int max_speed) {
    pilot_post_event(PILOT_EVT_ADVANCE);
}
void pilot_continuousAdvance(int max_speed) {
    pilot_post_event(PILOT_EVT_CONTINUOUS_ADVANCE);
}
void pilot_turn(double angle_rad, int max_speed, bool relative) {
    pilot_post_event(PILOT_EVT_TURN);
}
void pilot_goTo(double x, double y, int max_speed) {
    pilot_post_event(PILOT_EVT_GOTO);
}
void pilot_stop(double decelerationFactor) {
    pilot_post_event(PILOT_EVT_STOP);
}

// Accesseurs (à compléter)
Position pilot_getPosition(void) { return g_pilot.position; }
int pilot_getSpeed(void) { return g_pilot.targetSpeed; }
int pilot_getDistanceMeter(void) { return (int)g_pilot.distanceMeter; }
void pilot_resetDistanceMeter(void) { g_pilot.distanceMeter = 0.0; }
void pilot_setAcceleration(double linearAcceleration, double angularAcceleration) { (void)linearAcceleration; (void)angularAcceleration; }
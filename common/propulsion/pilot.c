////////////////////////////////////////////////////////////
// Module: pilot
// Description: Pilotage des moteurs (machine à états + POSIX mqueues)
// Date    : 06/06/2025
////////////////////////////////////////////////////////////

#include "pilot.h"
#include "positionControl.h"
#include "robotConfiguration.h" // Pour WHEEL_DIAMETER_CM
#include "xLog.h"
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h> // pour sleep, etc.

#ifndef intervention_manager__startMove
#define intervention_manager__startMove() /* TODO: call real startMove */
#endif

#ifndef intervention_manager__endMove
#define intervention_manager__endMove() /* TODO: call real endMove */
#endif

//------------------------------------------------------------------------------
// Variables globales
//------------------------------------------------------------------------------
static Pilot g_pilot;
static PilotState g_state = PILOT_STATE_WAIT_MOVE;

//------------------------------------------------------------------------------
// Prototypes internes
//------------------------------------------------------------------------------
static int move_queue_init(mqd_t *mq, const char *name);
static void move_queue_destroy(mqd_t *mq, const char *name);
static bool move_queue_push(mqd_t mq, const Move *mv);
static bool move_queue_pop(mqd_t mq, Move *mv);

static int event_queue_init(mqd_t *mq, const char *name);
static void event_queue_destroy(mqd_t *mq, const char *name);
static bool event_queue_push(mqd_t mq, const pilot_event_t *evt);
static bool event_queue_pop(mqd_t mq, pilot_event_t *evt);

static void pilot_post_event(const pilot_event_t *evt);

// Fonction de tâche exportée
void *pilot_move_task(void *arg);

//------------------------------------------------------------------------------
// Déclaration des callbacks (actions) de la machine à états
//------------------------------------------------------------------------------
void pilot_action_computeAdvance(void *arg);
void pilot_action_computeContinuousAdvance(void *arg);
void pilot_action_computeTurn(void *arg);
void pilot_action_computeGoTo(void *arg);
void pilot_action_startMoves(void *arg);
void pilot_action_endMove(void *arg);
void pilot_action_check_next_move(void *arg);
void pilot_action_nextMove(void *arg);
void pilot_action_emergencyStop(void *arg);

//------------------------------------------------------------------------------
// Définition de la table de transitions
//------------------------------------------------------------------------------
pilot_transition_t pilot_transitions[PILOT_STATE_COUNT][PILOT_EVT_COUNT] = {
    [PILOT_STATE_WAIT_MOVE] = {
        [PILOT_EVT_ADVANCE]             = { .next_state = PILOT_STATE_COMPUTE_MOVE,        .action = pilot_action_computeAdvance },
        [PILOT_EVT_CONTINUOUS_ADVANCE]  = { .next_state = PILOT_STATE_COMPUTE_MOVE,        .action = pilot_action_computeContinuousAdvance },
        [PILOT_EVT_TURN]                = { .next_state = PILOT_STATE_COMPUTE_MOVE,        .action = pilot_action_computeTurn },
        [PILOT_EVT_GOTO]                = { .next_state = PILOT_STATE_COMPUTE_MOVE,        .action = pilot_action_computeGoTo },
    },
    [PILOT_STATE_COMPUTE_MOVE] = {
        [PILOT_EVT_START_MOVES]         = { .next_state = PILOT_STATE_MOVING,             .action = pilot_action_startMoves },
    },
    [PILOT_STATE_MOVING] = {
        [PILOT_EVT_END_MOVE]            = { .next_state = PILOT_STATE_END_MOVE,            .action = pilot_action_endMove },
        [PILOT_EVT_STOP]                = { .next_state = PILOT_STATE_WAIT_MOVE,           .action = pilot_action_emergencyStop },
    },
    [PILOT_STATE_END_MOVE] = {
        [PILOT_EVT_CHECK_NEXT_MOVE]     = { .next_state = PILOT_STATE_CHECK_NEXT_MOVE,     .action = pilot_action_check_next_move },
    },
    [PILOT_STATE_CHECK_NEXT_MOVE] = {
        [PILOT_EVT_NEXT_MOVE]           = { .next_state = PILOT_STATE_MOVING,              .action = pilot_action_nextMove },
        [PILOT_EVT_END_ALL_MOVES]       = { .next_state = PILOT_STATE_WAIT_MOVE,           .action = pilot_action_endMove },
    },
    // Si ajout d’un état POSITION_WATCHER, compléter ici
};

//------------------------------------------------------------------------------
// move_queue_init : création de la POSIX queue pour Move (non-bloquante)
//------------------------------------------------------------------------------
static int move_queue_init(mqd_t *mq, const char *name)
{
    if (mq == NULL || name == NULL)
    {
        return PILOT_ERROR_INVALID_ARGUMENT;
    }

    struct mq_attr attr;
    memset(&attr, 0, sizeof(attr));
    attr.mq_flags = O_NONBLOCK; // réception non-bloquante
    attr.mq_maxmsg = 10;        // ≤ /proc/sys/fs/mqueue/msg_max
    attr.mq_msgsize = sizeof(Move);
    attr.mq_curmsgs = 0;

    *mq = mq_open(name, O_CREAT | O_RDWR | O_NONBLOCK, 0666, &attr);
    if (*mq == (mqd_t)-1)
    {
        X_LOG_TRACE("move_queue_init: mq_open '%s' failed: %s", name, strerror(errno));
        return PILOT_ERROR_INIT_FAILED;
    }
    return PILOT_OK;
}

//------------------------------------------------------------------------------
// move_queue_destroy : fermeture + suppression de la queue de moves
//------------------------------------------------------------------------------
static void move_queue_destroy(mqd_t *mq, const char *name)
{
    if (mq == NULL || *mq == (mqd_t)-1)
        return;
    mq_close(*mq);
    mq_unlink(name);
    *mq = (mqd_t)-1;
}

//------------------------------------------------------------------------------
// move_queue_push : envoi non-bloquant d’un Move
//------------------------------------------------------------------------------
static bool move_queue_push(mqd_t mq, const Move *mv)
{
    if (mq == (mqd_t)-1 || mv == NULL)
        return false;
    int res = mq_send(mq, (const char *)mv, sizeof(Move), 0);
    if (res == -1 && errno == EAGAIN)
    {
        // Queue pleine
        return false;
    }
    return (res == 0);
}

//------------------------------------------------------------------------------
// move_queue_pop : réception non-bloquante d’un Move
//------------------------------------------------------------------------------
static bool move_queue_pop(mqd_t mq, Move *mv)
{
    if (mq == (mqd_t)-1 || mv == NULL)
        return false;
    ssize_t n = mq_receive(mq, (char *)mv, sizeof(Move), NULL);
    if (n == -1 && errno == EAGAIN)
    {
        // Queue vide
        return false;
    }
    return (n == sizeof(Move));
}

//------------------------------------------------------------------------------
// event_queue_init : création de la POSIX queue pour pilot_event_t (bloquante)
//------------------------------------------------------------------------------
static int event_queue_init(mqd_t *mq, const char *name)
{
    if (mq == NULL || name == NULL)
    {
        return PILOT_ERROR_INVALID_ARGUMENT;
    }

    struct mq_attr attr;
    memset(&attr, 0, sizeof(attr));
    attr.mq_flags = 0;   // réception BLOQUANTE
    attr.mq_maxmsg = 10; // ≤ /proc/sys/fs/mqueue/msg_max
    attr.mq_msgsize = sizeof(pilot_event_t);
    attr.mq_curmsgs = 0;

    *mq = mq_open(name, O_CREAT | O_RDWR, 0666, &attr);
    if (*mq == (mqd_t)-1)
    {
        X_LOG_TRACE("event_queue_init: mq_open '%s' failed: %s", name, strerror(errno));
        return PILOT_ERROR_INIT_FAILED;
    }
    return PILOT_OK;
}

//------------------------------------------------------------------------------
// event_queue_destroy : fermeture + suppression de la queue d’événements
//------------------------------------------------------------------------------
static void event_queue_destroy(mqd_t *mq, const char *name)
{
    if (mq == NULL || *mq == (mqd_t)-1)
        return;
    mq_close(*mq);
    mq_unlink(name);
    *mq = (mqd_t)-1;
}

//------------------------------------------------------------------------------
// event_queue_push : envoi non-bloquant d’un pilot_event_t
//------------------------------------------------------------------------------
static bool event_queue_push(mqd_t mq, const pilot_event_t *evt)
{
    if (mq == (mqd_t)-1 || evt == NULL)
        return false;
    int res = mq_send(mq, (const char *)evt, sizeof(pilot_event_t), 0);
    if (res == -1 && errno == EAGAIN)
    {
        // Queue pleine
        return false;
    }
    return (res == 0);
}

//------------------------------------------------------------------------------
// event_queue_pop : réception BLOQUANTE d’un pilot_event_t
//------------------------------------------------------------------------------
static bool event_queue_pop(mqd_t mq, pilot_event_t *evt)
{
    if (mq == (mqd_t)-1 || evt == NULL)
        return false;
    ssize_t n = mq_receive(mq, (char *)evt, sizeof(pilot_event_t), NULL);
    if (n == -1)
    {
        // Erreur en mode bloquant
        return false;
    }
    return (n == sizeof(pilot_event_t));
}

//------------------------------------------------------------------------------
// pilot_post_event : wrapper pour poster un événement
//------------------------------------------------------------------------------
static void pilot_post_event(const pilot_event_t *evt)
{
    event_queue_push(g_pilot.evtQueue, evt);
}

//------------------------------------------------------------------------------
// pilot_move_task : Tâche principale du pilot (machine à états)
//  - bloque sur la réception d’un event
//------------------------------------------------------------------------------
void *pilot_move_task(void *arg)
{
    (void)arg;
    pilot_event_t evt;

    while (g_pilot.pilotTask.a_iStopFlag == OS_TASK_SECURE_FLAG)
    {
        if (event_queue_pop(g_pilot.evtQueue, &evt))
        {
            X_LOG_TRACE("pilot_move_task: reçu evt type=%d en state=%d", evt.type, g_state);

            pilot_transition_t *t = &pilot_transitions[g_state][evt.type];
            if (t->action)
            {
                t->action(&evt);
            }
            g_state = t->next_state;
            X_LOG_TRACE("pilot_move_task: nouvel état=%d", g_state);
        }
        // Pour terminer un move, positionControl doit appeler pilot_notify_end_move()
    }
    return NULL;
}

//------------------------------------------------------------------------------
// Actions (callbacks) de la machine à états
//------------------------------------------------------------------------------

// 1) ADVANCE → on poste START_MOVES (la conversion a déjà eu lieu dans pilot_advance)
void pilot_action_computeAdvance(void *arg)
{
    (void)arg;
    pilot_event_t next = {0};
    next.type = PILOT_EVT_START_MOVES;
    pilot_post_event(&next);
}

// 2) CONTINUOUS_ADVANCE → on poste START_MOVES
void pilot_action_computeContinuousAdvance(void *arg)
{
    (void)arg;
    pilot_event_t next = {0};
    next.type = PILOT_EVT_START_MOVES;
    pilot_post_event(&next);
}

// 3) TURN → on poste START_MOVES
void pilot_action_computeTurn(void *arg)
{
    (void)arg;
    pilot_event_t next = {0};
    next.type = PILOT_EVT_START_MOVES;
    pilot_post_event(&next);
}

// 4) GOTO → on poste START_MOVES (Move déjà créé dans pilot_goTo)
void pilot_action_computeGoTo(void *arg)
{
    (void)arg;
    pilot_event_t next = {0};
    next.type = PILOT_EVT_START_MOVES;
    pilot_post_event(&next);
}

// 5) START_MOVES → on récupère un Move (en radians) et on appelle positionControl
void pilot_action_startMoves(void *arg)
{
    (void)arg;
    Move mv;
    if (move_queue_pop(g_pilot.moveQueue, &mv))
    {
        intervention_manager__startMove();

        if (mv.direction == DIR_LEFT || mv.direction == DIR_RIGHT)
        {
            // → Rotation du robot
            //    (angle_rad en radian, max_speed_mm_s en rad/s)
            position_control_turn(mv.angle_rad, mv.max_speed_mm_s);
        }
        else
        {
            // → Avance (finie ou continue si distance_mm == UINT32_MAX)
            //    (distance_mm en mm, max_speed_mm_s en mm/s)
            position_control_advance(mv.distance_mm, mv.max_speed_mm_s);
        }
    }
}

// 6) END_MOVE → on notifie puis POST CHECK_NEXT_MOVE
void pilot_action_endMove(void *arg)
{
    (void)arg;
    intervention_manager__endMove();
    X_LOG_TRACE("pilot_action_endMove");
    if (g_pilot.callback)
    {
        g_pilot.callback(NULL);
    }
}

// 7) CHECK_NEXT_MOVE → si queue non vide, POST NEXT_MOVE ; sinon POST END_ALL_MOVES
void pilot_action_check_next_move(void *arg)
{
    (void)arg;
    Move dummy;
    if (move_queue_pop(g_pilot.moveQueue, &dummy))
    {
        pilot_event_t next = {0};
        next.type = PILOT_EVT_NEXT_MOVE;
        pilot_post_event(&next);
    }
    else
    {
        pilot_event_t next = {0};
        next.type = PILOT_EVT_END_ALL_MOVES;
        pilot_post_event(&next);
    }
}

// 8) NEXT_MOVE → on relance startMoves
void pilot_action_nextMove(void *arg)
{
    (void)arg;
    pilot_action_startMoves(NULL);
}

// 9) STOP/Urgence → on stoppe immédiatement
void pilot_action_emergencyStop(void *arg)
{
    (void)arg;
    position_control_stop();
}

//------------------------------------------------------------------------------
// pilot_init : initialisation globale
//------------------------------------------------------------------------------
int32_t pilot_init(void)
{
    XOS_MEMORY_SANITIZE(&g_pilot, sizeof(g_pilot));
    g_state = PILOT_STATE_WAIT_MOVE;

    int err = move_queue_init(&g_pilot.moveQueue, PILOT_MQ_MOVE_NAME);
    if (err != PILOT_OK)
    {
        return err;
    }
    err = event_queue_init(&g_pilot.evtQueue, PILOT_MQ_EVT_NAME);
    if (err != PILOT_OK)
    {
        move_queue_destroy(&g_pilot.moveQueue, PILOT_MQ_MOVE_NAME);
        return err;
    }

    int l_iret = osTaskInit(&g_pilot.pilotTask);
    if (l_iret != OS_TASK_SUCCESS)
    {
        event_queue_destroy(&g_pilot.evtQueue, PILOT_MQ_EVT_NAME);
        move_queue_destroy(&g_pilot.moveQueue, PILOT_MQ_MOVE_NAME);
        return PILOT_ERROR_INIT_FAILED;
    }
    g_pilot.pilotTask.t_ptTask = pilot_move_task;
    g_pilot.pilotTask.t_ptTaskArg = NULL;
    atomic_init(&g_pilot.pilotTask.a_iStopFlag, OS_TASK_SECURE_FLAG);

    l_iret = osTaskCreate(&g_pilot.pilotTask);
    if (l_iret != OS_TASK_SUCCESS)
    {
        osTaskStop(&g_pilot.pilotTask, 0);
        event_queue_destroy(&g_pilot.evtQueue, PILOT_MQ_EVT_NAME);
        move_queue_destroy(&g_pilot.moveQueue, PILOT_MQ_MOVE_NAME);
        return PILOT_ERROR_INIT_FAILED;
    }

    return PILOT_OK;
}

//------------------------------------------------------------------------------
// pilot_shutdown : arrête la tâche et détruit les queues
//------------------------------------------------------------------------------
int32_t pilot_shutdown(void)
{
    atomic_store_explicit(&g_pilot.pilotTask.a_iStopFlag, OS_TASK_STOP_REQUEST, memory_order_relaxed);
    osTaskStop(&g_pilot.pilotTask, 2);

    event_queue_destroy(&g_pilot.evtQueue, PILOT_MQ_EVT_NAME);
    move_queue_destroy(&g_pilot.moveQueue, PILOT_MQ_MOVE_NAME);

    XOS_MEMORY_SANITIZE(&g_pilot, sizeof(g_pilot));
    return PILOT_OK;
}

//------------------------------------------------------------------------------
// pilot_advance : conversion mm → rad roue, queue + EVENT
//------------------------------------------------------------------------------
int32_t pilot_advance(int32_t distance_mm, uint32_t max_speed_mm_s)
{
    if (distance_mm <= 0 || max_speed_mm_s == 0U)
    {
        return PILOT_ERROR_INVALID_ARGUMENT;
    }

    // NOTE : on ne convertit plus du tout en radians ici.
    //      distance_mm est déjà en mm, max_speed_mm_s déjà en mm/s.

    Move mv = {.distance_mm = (uint32_t)distance_mm, // avance finie sur X mm
               .angle_rad = 0.0,                     // ignoré pour l'avance
               .max_speed_mm_s = (double)max_speed_mm_s,
               .direction = DIR_FORWARD, // flag "avance"
               .relative = true};

    if (!move_queue_push(g_pilot.moveQueue, &mv))
    {
        return PILOT_ERROR_QUEUE_FULL;
    }

    // On poste l'événement ADVANCE (machine à états) :
    pilot_event_t evt = {0};
    evt.type = PILOT_EVT_ADVANCE;
    evt.advance.distance_mm = (uint32_t)distance_mm;
    evt.advance.speed_mm_s = max_speed_mm_s;
    pilot_post_event(&evt);

    return PILOT_OK;
}

//------------------------------------------------------------------------------
// pilot_continuousAdvance : conversion mm/s → rad/s, queue + EVENT
//------------------------------------------------------------------------------
void pilot_continuousAdvance(uint32_t max_speed_mm_s)
{
    if (max_speed_mm_s == 0U)
        return;

    // On ne convertit plus : max_speed_mm_s reste en mm/s.
    Move mv = {.distance_mm = UINT32_MAX, // sentinelle "avance continue"
               .angle_rad = 0.0,          // ignoré
               .max_speed_mm_s = (double)max_speed_mm_s,
               .direction = DIR_FORWARD,
               .relative = true};

    move_queue_push(g_pilot.moveQueue, &mv);

    // On poste l’événement CONTINUOUS_ADVANCE :
    pilot_event_t evt = {0};
    evt.type = PILOT_EVT_CONTINUOUS_ADVANCE;
    evt.continuous.speed_mm_s = max_speed_mm_s;
    pilot_post_event(&evt);
}

//------------------------------------------------------------------------------
// pilot_turn : mise en queue en radians, EVENT
//------------------------------------------------------------------------------
int32_t pilot_turn(double angle_rad, double max_speed_rad_s, bool relative)
{
    if (fabs(angle_rad) < 1e-9 || max_speed_rad_s <= 0.0)
    {
        return PILOT_ERROR_INVALID_ARGUMENT;
    }

    // On choisit DIR_LEFT ou DIR_RIGHT selon le signe de angle_rad
    Direction dir = (angle_rad > 0.0) ? DIR_LEFT : DIR_RIGHT;

    // distance_mm reste à 0 pour signaler une rotation
    Move mv = {.distance_mm = 0U,                 // 0 => rotation
               .angle_rad = angle_rad,            // angle du robot en radian
               .max_speed_mm_s = max_speed_rad_s, // ici max_speed_mm_s contient la vitesse angulaire (rad/s)
               .direction = dir,
               .relative = relative};

    if (!move_queue_push(g_pilot.moveQueue, &mv))
    {
        return PILOT_ERROR_QUEUE_FULL;
    }

    // On poste l’événement TURN
    pilot_event_t evt = {0};
    evt.type = PILOT_EVT_TURN;
    evt.turn.angle_rad = angle_rad;
    evt.turn.speed_rad_s = max_speed_rad_s;
    evt.turn.relative = relative;
    pilot_post_event(&evt);

    return PILOT_OK;
}

//------------------------------------------------------------------------------
// pilot_goTo : calcule rotation + translation en mm, convertit → rad, queue + EVENT
//------------------------------------------------------------------------------
void pilot_goTo(int32_t x_mm, int32_t y_mm, uint32_t max_speed_mm_s)
{
    g_pilot.gotoTargetX = x_mm;
    g_pilot.gotoTargetY = y_mm;
    g_pilot.gotoMaxSpeed = max_speed_mm_s;

    double startX = (double)g_pilot.position.positionX;
    double startY = (double)g_pilot.position.positionY;
    double startAngle = (double)g_pilot.position.angle;
    double targetX = (double)x_mm;
    double targetY = (double)y_mm;
    double max_speed = (double)max_speed_mm_s;

    double dx = targetX - startX;
    double dy = targetY - startY;
    double distance = sqrt(dx * dx + dy * dy);
    double angle_to_target = atan2(dy, dx);
    double delta_angle = angle_to_target - startAngle;
    while (delta_angle > M_PI)
        delta_angle -= 2 * M_PI;
    while (delta_angle < -M_PI)
        delta_angle += 2 * M_PI;

    // 1) Rotation éventuelle
    if (fabs(delta_angle) > 1e-6)
    {
        Direction dir = (delta_angle > 0.0) ? DIR_LEFT : DIR_RIGHT;
        Move mv_turn = {.distance_mm = 0U,           // 0 = rotation
                        .angle_rad = delta_angle,    // angle de robot
                        .max_speed_mm_s = max_speed, // vitesse angulaire en rad/s
                        .direction = dir,
                        .relative = true};
        move_queue_push(g_pilot.moveQueue, &mv_turn);
    }

    // 2) Translation en mm (pas de conversion → on laisse tout en mm)
    if (distance > 0.5)
    {
        Move mv_adv = {.distance_mm = (uint32_t)distance, // distance linéaire en mm
                       .angle_rad = 0.0,                  // ignoré pour forward
                       .max_speed_mm_s = (double)max_speed_mm_s,
                       .direction = DIR_FORWARD,
                       .relative = true};
        move_queue_push(g_pilot.moveQueue, &mv_adv);
    }

    // Post de l’événement GOTO
    pilot_event_t evt = {0};
    evt.type = PILOT_EVT_GOTO;
    evt.go_to.x_mm = x_mm;
    evt.go_to.y_mm = y_mm;
    evt.go_to.speed_mm_s = max_speed_mm_s;
    pilot_post_event(&evt);

    // On démarre la machine à états
    pilot_event_t next = {0};
    next.type = PILOT_EVT_START_MOVES;
    pilot_post_event(&next);
}

//------------------------------------------------------------------------------
// pilot_stop : simple STOP
//------------------------------------------------------------------------------
void pilot_stop(void)
{
    pilot_event_t evt = {0};
    evt.type = PILOT_EVT_STOP;
    pilot_post_event(&evt);
}

//------------------------------------------------------------------------------
// pilot_notify_end_move : appelé par positionControl quand un move est terminé
//  poste PILOT_EVT_END_MOVE
//------------------------------------------------------------------------------
void pilot_notify_end_move(void)
{
    X_LOG_TRACE("pilot_notify_end_move");
    pilot_event_t evt = {0};
    evt.type = PILOT_EVT_END_MOVE;
    pilot_post_event(&evt);
    if (g_pilot.callback)
    {
        g_pilot.callback(NULL);
    }
}

//------------------------------------------------------------------------------
// pilot_getPosition : retourne la position courante
//------------------------------------------------------------------------------
int32_t pilot_getPosition(Position *pos_out)
{
    if (pos_out == NULL)
    {
        return PILOT_ERROR_INVALID_ARGUMENT;
    }
    *pos_out = g_pilot.position;
    return PILOT_OK;
}

//------------------------------------------------------------------------------
// pilot_getSpeed : renvoie targetSpeed
//------------------------------------------------------------------------------
int pilot_getSpeed(void)
{
    return g_pilot.targetSpeed;
}

//------------------------------------------------------------------------------
// pilot_getDistanceMeter
//------------------------------------------------------------------------------
int pilot_getDistanceMeter(void)
{
    return (int)g_pilot.distanceMeter;
}

//------------------------------------------------------------------------------
// pilot_resetDistanceMeter
//------------------------------------------------------------------------------
void pilot_resetDistanceMeter(void)
{
    g_pilot.distanceMeter = 0.0f;
}

//------------------------------------------------------------------------------
// pilot_getErrorString
//------------------------------------------------------------------------------
const char *pilot_getErrorString(int32_t error)
{
    switch (error)
    {
        case PILOT_OK:
            return "PILOT_OK";
        case PILOT_ERROR_INVALID_ARGUMENT:
            return "PILOT_ERROR_INVALID_ARGUMENT";
        case PILOT_ERROR_QUEUE_FULL:
            return "PILOT_ERROR_QUEUE_FULL";
        case PILOT_ERROR_QUEUE_EMPTY:
            return "PILOT_ERROR_QUEUE_EMPTY";
        case PILOT_ERROR_INIT_FAILED:
            return "PILOT_ERROR_INIT_FAILED";
        case PILOT_ERROR_NOT_INITIALIZED:
            return "PILOT_ERROR_NOT_INITIALIZED";
        default:
            return "UNKNOWN_PILOT_ERROR";
    }
}

void pilot_register_callback(pilot_callback_t callback)
{
    g_pilot.callback = callback;
}

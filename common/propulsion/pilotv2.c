////////////////////////////////////////////////////////////
// Module: pilot
// Description: Pilotage des moteurs (machine à états + POSIX mqueues)
// Date    : 17/11/2025
////////////////////////////////////////////////////////////

#include "pilotv2.h"
#include "robotConfiguration.h" // Pour WHEEL_DIAMETER_CM
#include "xLog.h"
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h> // pour sleep, etc.

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
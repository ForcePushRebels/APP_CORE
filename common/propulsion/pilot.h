////////////////////////////////////////////////////////////
// Module: pilot
// Description: Module de pilotage des moteurs
//
// Written : 23/05/2025
////////////////////////////////////////////////////////////
#ifndef PILOT_H
#define PILOT_H

#include <stdbool.h>

#include "motorControl.h"
#include "hardwareAbstraction.h"

// --- Types de base ---
typedef struct {
    int x;
    int y;
    float theta;
} Position;

typedef enum {
    DIR_FORWARD,
    DIR_LEFT,
    DIR_RIGHT
} Direction;

// --- Machine à états ---
typedef enum {
    PILOT_STATE_WAIT_MOVE = 0,
    PILOT_STATE_COMPUTE_MOVE,
    PILOT_STATE_MOVING,
    PILOT_STATE_END_MOVE,
    PILOT_STATE_CHECK_NEXT_MOVE,
    PILOT_STATE_MOVE_IN_PROGRESS,
    PILOT_STATE_POSITION_WATCHER,
    PILOT_STATE_COUNT
} PilotState;

typedef enum {
    PILOT_EVT_ADVANCE = 0,
    PILOT_EVT_CONTINUOUS_ADVANCE,
    PILOT_EVT_TURN,
    PILOT_EVT_GOTO,
    PILOT_EVT_START_MOVES,
    PILOT_EVT_END_MOVE,
    PILOT_EVT_EMERGENCY_STOP,
    PILOT_EVT_CHECK_NEXT_MOVE,
    PILOT_EVT_NEXT_MOVE,
    PILOT_EVT_END_ALL_MOVES,
    PILOT_EVT_STOP,
    PILOT_EVT_POSITION_UPDATE,
    PILOT_EVT_COUNT
} PilotEvent;

// --- Move ---
typedef struct {
    double distance_mm;
    double angle_rad;
    float max_speed;
    Direction direction;
    bool relative;
    // Ajoute d'autres champs si besoin
} Move;

// --- Table de transition ---
typedef void (*pilot_action_fct_t)(void* arg);

typedef struct {
    PilotState next_state;
    pilot_action_fct_t action;
} pilot_transition_t;

// --- Structure principale ---
typedef struct Timer Timer;

typedef struct {
    Position position;
    int encodersValues[2];
    int targetSpeed;
    Direction currentDirection;
    void* moveTodo; // mq<Move>
    float distanceMeter;
    Timer* positionWatcherTimer;
    Timer* movementTimer;
} Pilot;

// --- Interface publique ---
// Initialisation/Destruction
int32_t pilot_init();
void pilot_shutdown(void);

// Commandes de mouvement
void pilot_advance(double distance_mm, float max_speed);
void pilot_continuousAdvance(int max_speed);
void pilot_turn(double angle_rad, int max_speed, bool relative);
void pilot_goTo(double x, double y, int max_speed);
void pilot_stop();

// Accesseurs
Position pilot_getPosition(void);
int pilot_getSpeed(void);
int pilot_getDistanceMeter(void);
void pilot_resetDistanceMeter(void);
void pilot_setAcceleration(double linearAcceleration, double angularAcceleration);


// --- Table de transitions (à définir dans pilot.c) ---
extern pilot_transition_t pilot_transitions[PILOT_STATE_COUNT][PILOT_EVT_COUNT];

#endif // PILOT_H
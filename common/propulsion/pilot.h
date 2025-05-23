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
    DIR_BACKWARD,
    DIR_LEFT,
    DIR_RIGHT
    // ... autres directions si besoin
} Direction;

// À adapter selon ton projet
typedef struct Timer Timer;
typedef struct Move Move;

// --- Structure principale ---
typedef struct {
    Position position;
    int encodersValues[2];
    int targetSpeed;
    Direction currentDirection;
    // Remplace par une vraie file de messages ou une abstraction adaptée
    // Ici, on suppose un pointeur ou un handle vers une queue de Move
    void* moveTodo; // mq<Move>
    float distanceMeter;
    Timer* positionWatcherTimer;
    Timer* movementTimer;
} Pilot;

// --- Interface publique ---
// Initialisation/Destruction
int32_t pilot_init(int max_speed, double wheel_radius_m, double wheel_base_m);
void pilot_shutdown(void);


// Commandes de mouvement
void pilot_advance(double distance_mm, int max_speed);
void pilot_continuousAdvance(int max_speed);
void pilot_turn(double angle_rad, int max_speed, bool relative);
void pilot_goTo(double x, double y, int max_speed);
void pilot_stop(double decelerationFactor);

// Accesseurs
Position pilot_getPosition(void);
int pilot_getSpeed(void);
int pilot_getDistanceMeter(void);
void pilot_resetDistanceMeter(void);
void pilot_setAcceleration(double linearAcceleration, double angularAcceleration);

// --- Fonctions internes (privées) ---
int pilot_computeAdvance(double distance_mm, int max_speed);
int pilot_computeTurn(double angle_rad, int speed, bool relative);
int pilot_computeStop(double acceleration);
int pilot_computeGoTo(double x, double y, int speed);
void pilot_addMove(double distance_mm, Direction direction, int max_speed);
void pilot_startMoves(void);
void pilot_nextMove(void);
void pilot_endMove(int reason);
void pilot_setMotorSpeed(double speed_rad_s, Direction direction);
void pilot_handleMove(void);
void pilot_updatePosition(void);


#endif // PILOT_H

////////////////////////////////////////////////////////////
// Module: pilot
// Description: Module de pilotage des moteurs
//
// general discloser: copy or share the file is forbidden
// Written : 23/05/2025
// Updated : 30/05/2025 - Fixed security issues
////////////////////////////////////////////////////////////
#ifndef PILOT_H
#define PILOT_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xOsMemory.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xTimer.h"
#include "positionControl.h"


/////////////////////////////////
/// @brief Error codes
/////////////////////////////////
#define PILOT_OK                        0x43412000  




/////////////////////////////////
/// @brief Action function
/// @param arg : Argument
/////////////////////////////////
typedef void (*pilot_action_fct_t)(void* arg);

/////////////////////////////////
/// @brief Position du robot
/// @param x : Position en x
/// @param y : Position en y
/// @param theta : Angle en radians
/////////////////////////////////
typedef struct 
{
    int positionX;
    int positionY;
    float angle;
} Position;

/////////////////////////////////
/// @brief Direction du robot
/// @param DIR_FORWARD : Avant
/// @param DIR_LEFT : Gauche
/// @param DIR_RIGHT : Droite
/////////////////////////////////
typedef enum 
{
    DIR_FORWARD,
    DIR_LEFT,
    DIR_RIGHT
} Direction;

/////////////////////////////////
/// @brief State of the robot
/////////////////////////////////
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

/////////////////////////////////
/// @brief Events of the robot
/////////////////////////////////
typedef enum {
    PILOT_EVT_ADVANCE = 0,          // Advance
    PILOT_EVT_CONTINUOUS_ADVANCE,   // Continuous advance
    PILOT_EVT_TURN,                 // Turn
    PILOT_EVT_GOTO,                 // Go to a position
    PILOT_EVT_START_MOVES,          // Start moves
    PILOT_EVT_END_MOVE,             // End move
    PILOT_EVT_EMERGENCY_STOP,       // Emergency stop
    PILOT_EVT_CHECK_NEXT_MOVE,      // Check next move
    PILOT_EVT_NEXT_MOVE,            // Next move
    PILOT_EVT_END_ALL_MOVES,        // End all moves
    PILOT_EVT_STOP,                 // Stop
    PILOT_EVT_POSITION_UPDATE,      // Position update
    PILOT_EVT_COUNT                 // Number of events
} PilotEvent;

/////////////////////////////////
/// @brief Movement
/// @param distance_mm : Distance in mm
/// @param angle_rad : Angle in radians
/// @param max_speed : Maximum speed
/// @param direction : Direction
/// @param relative : Relative movement 
/////////////////////////////////
typedef struct 
{
    double distance_mm;
    double angle_rad;
    float max_speed;
    Direction direction;
    bool relative;
    // Ajoute d'autres champs si besoin
} Move;

/////////////////////////////////
/// @brief Transition
/// @param next_state : Next state
/// @param action : Action function
/////////////////////////////////
typedef struct 
{
    PilotState next_state;
    pilot_action_fct_t action;
} pilot_transition_t;

/////////////////////////////////
/// @brief Move queue
/////////////////////////////////
typedef struct MoveQueue
{
    Move moves[16];
    int head;
    int tail;
    int count;
    xOsMutexCtx mutex;
} moveQueue_t;

/////////////////////////////////
/// @brief Event queue
/////////////////////////////////
typedef struct EvtQueue
{
    PilotEvent events[16];
    int head;
    int tail;
    int count;
    xOsMutexCtx mutex;
} evtQueue_t;

/////////////////////////////////
/// @brief Pilot structure
/// @param position : Position
/// @param encodersValues : Encoders values
/// @param targetSpeed : Target speed
/// @param currentDirection : Current direction
/// @param moveTodo : Move to do
/// @param distanceMeter : Distance meter
/// @param positionWatcherTimer : Position watcher timer
/// @param movementTimer : Movement timer
/////////////////////////////////
typedef struct 
{
    Position position;
    int encodersValues[2];
    int targetSpeed;
    Direction currentDirection;
    void* moveTodo; // mq<Move>
    float distanceMeter;
    xOsTimerCtx positionWatcherTimer;
    xOsTimerCtx movementTimer;
    xOsTaskCtx pilotTask;
    xOsMutexCtx mutex;
    moveQueue_t moveQueue;
    evtQueue_t evtQueue;
} Pilot;

/////////////////////////////////
/// @brief Initialization
/// @return PILOT_OK if success, PILOT_ERROR_INVALID_ARGUMENT if error
/////////////////////////////////
int32_t pilot_init(void);

/////////////////////////////////
/// @brief Shutdown
/// @return PILOT_OK if success, PILOT_ERROR_INVALID_ARGUMENT if error
/////////////////////////////////
int32_t pilot_shutdown(void);

/////////////////////////////////
/// @brief Advance
/// @param distance_mm : Distance in mm
/// @param max_speed : Maximum speed
/////////////////////////////////
void pilot_advance(double distance_mm, float max_speed);

/////////////////////////////////
/// @brief Continuous advance
/// @param max_speed : Maximum speed
/////////////////////////////////
void pilot_continuousAdvance(int max_speed);

/////////////////////////////////
/// @brief Turn
/// @param angle_rad : Angle in radians
/// @param max_speed : Maximum speed
/// @param relative : Relative movement
/////////////////////////////////
void pilot_turn(double angle_rad, int max_speed, bool relative);

/////////////////////////////////
/// @brief Go to a position
/// @param positionX : X position
/// @param positionY : Y position
/// @param max_speed : Maximum speed
/////////////////////////////////
void pilot_goTo(double positionX, double positionY, int max_speed);

/////////////////////////////////
/// @brief Stop the pilot
/////////////////////////////////
void pilot_stop();

/////////////////////////////////
/// @brief Get the position
/// @param p_pttPosition : Pointer to the position
/// @return PILOT_OK if success, PILOT_ERROR_INVALID_ARGUMENT if error
/////////////////////////////////
int32_t pilot_getPosition(Position* p_pttPosition);

/////////////////////////////////
/// @brief Get the speed
/// @return The current speed
/////////////////////////////////
int pilot_getSpeed(void);

/////////////////////////////////
/// @brief Get the distance meter
/// @return The distance meter
/////////////////////////////////
int pilot_getDistanceMeter(void);

/////////////////////////////////
/// @brief Reset the distance meter
/////////////////////////////////
void pilot_resetDistanceMeter(void);

/////////////////////////////////
/// @brief Set the acceleration
/// @param linearAcceleration : Linear acceleration
/// @param angularAcceleration : Angular acceleration
/////////////////////////////////
void pilot_setAcceleration(double linearAcceleration, double angularAcceleration);

#endif // PILOT_H
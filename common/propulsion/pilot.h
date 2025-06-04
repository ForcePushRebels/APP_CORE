////////////////////////////////////////////////////////////
// Module: pilot
// Description: Pilotage des moteurs (machine à états + POSIX mqueues)
// Date    : 06/06/2025
////////////////////////////////////////////////////////////

#ifndef PILOT_H
#define PILOT_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mqueue.h>      // Utilisation de POSIX message queues
#include <fcntl.h>       // O_* macros pour mq_open
#include <sys/stat.h>    // mode constants pour mq_open

#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xOsMemory.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xTimer.h"
#include "positionControl.h"

/////////////////////////////////
/// @brief Codes d’erreur
/////////////////////////////////
#define PILOT_OK                            0x43412000
#define PILOT_ERROR_INVALID_ARGUMENT        0x43412001
#define PILOT_ERROR_QUEUE_FULL              0x43412002
#define PILOT_ERROR_QUEUE_EMPTY             0x43412003
#define PILOT_ERROR_INIT_FAILED             0x43412004
#define PILOT_ERROR_NOT_INITIALIZED         0x43412005

/////////////////////////////////
/// @brief Noms (paths) des POSIX queues
/////////////////////////////////
#define PILOT_MQ_EVT_NAME      "/pilot_event_queue"
#define PILOT_MQ_MOVE_NAME     "/pilot_move_queue"

/////////////////////////////////
/// @brief Types d’événements du pilot
/////////////////////////////////
typedef enum {
    PILOT_EVT_ADVANCE = 0,
    PILOT_EVT_CONTINUOUS_ADVANCE,
    PILOT_EVT_TURN,
    PILOT_EVT_GOTO,
    PILOT_EVT_START_MOVES,
    PILOT_EVT_END_MOVE,
    PILOT_EVT_CHECK_NEXT_MOVE,
    PILOT_EVT_NEXT_MOVE,
    PILOT_EVT_END_ALL_MOVES,
    PILOT_EVT_STOP,
    PILOT_EVT_POSITION_UPDATE,
    PILOT_EVT_EMERGENCY_STOP,
    PILOT_EVT_COUNT
} pilot_event_type_t;

/////////////////////////////////
/// @brief Structure d’un événement pilot
/// Les champs int/double sont choisis selon le type d’événement
/////////////////////////////////
typedef struct {
    pilot_event_type_t type;
    union {
        struct {
            uint32_t distance_mm;
            uint32_t speed_mm_s;
        } advance;
        struct {
            uint32_t speed_mm_s;
        } continuous;
        struct {
            double angle_rad;
            double speed_rad_s;
            bool   relative;
        } turn;
        struct {
            int32_t  x_mm;
            int32_t  y_mm;
            uint32_t speed_mm_s;
        } go_to;
    };
} pilot_event_t;

/////////////////////////////////
/// @brief Direction du robot
///   - DIR_FORWARD : avance en avant
///   - DIR_LEFT    : tourne à gauche
///   - DIR_RIGHT   : tourne à droite
/////////////////////////////////
typedef enum {
    DIR_FORWARD = 0,
    DIR_LEFT,
    DIR_RIGHT
} Direction;

/////////////////////////////////
/// @brief « Move »: utilisé en interne dans la queue de moves
/////////////////////////////////
typedef struct {
    uint32_t        distance_mm;    //  >0 = avance finie sur X mm,  
                                    //  UINT32_MAX = avance continue,  
                                    //  0 = rotation
    double          angle_rad;      //   angle en radian du robot (uniquement pour rotation)
    double          max_speed_mm_s; //   vitesse linéaire en mm/s (pour avance) 
                                    //   ou vitesse angulaire en rad/s (pour turn)
    Direction direction;     //   DIR_FORWARD / DIR_LEFT / DIR_RIGHT
    bool            relative;
} Move;

/////////////////////////////////
/// @brief Position du robot
///   - positionX : position en X (mm)
///   - positionY : position en Y (mm)
///   - angle     : orientation en radians
/////////////////////////////////
typedef struct {
    int32_t positionX;
    int32_t positionY;
    double  angle;
} Position;

/////////////////////////////////
/// @brief États du pilot
/////////////////////////////////
typedef enum {
    PILOT_STATE_WAIT_MOVE = 0,
    PILOT_STATE_COMPUTE_MOVE,
    PILOT_STATE_MOVING,
    PILOT_STATE_END_MOVE,
    PILOT_STATE_CHECK_NEXT_MOVE,
    PILOT_STATE_POSITION_WATCHER,
    PILOT_STATE_COUNT
} PilotState;

/////////////////////////////////
/// @brief Prototype d’une action (callback)
/////////////////////////////////
typedef void (*pilot_action_fct_t)(void* arg);

/////////////////////////////////
/// @brief Ligne de transition (next state + action)
/////////////////////////////////
typedef struct {
    PilotState         next_state;
    pilot_action_fct_t action;
} pilot_transition_t;

/////////////////////////////////
/// @brief Structure principale du pilote
/////////////////////////////////
typedef struct {
    // --- Champs d’état, inchangés sauf types modifiés ---
    Position    position;            // position actuelle (x, y, theta)
    int         encodersValues[2];
    int         targetSpeed;
    Direction   currentDirection;
    Move       *moveTodo;            // conservé pour compatibilité
    float       distanceMeter;       // peut rester float pour calculs internes

    int32_t     gotoTargetX;         // mm
    int32_t     gotoTargetY;         // mm
    uint32_t    gotoMaxSpeed;        // mm/s

    xOsTimerCtx positionWatcherTimer;
    xOsTimerCtx movementTimer;
    xOsTaskCtx  pilotTask;
    xOsMutexCtx mutex;               // pour d’autres sections si besoin

    // === Queues POSIX ===
    mqd_t       evtQueue;            // queue d’événements (pilot_event_t)
    mqd_t       moveQueue;           // queue de moves (Move)
} Pilot;

/////////////////////////////////
/// @brief Fonction de tâche (prototype EXACTEMENT identique à votre code source)
/////////////////////////////////
void *pilot_move_task(void *arg);

/////////////////////////////////
/// @brief Prototypes des fonctions publiques (modifiées pour utiliser int32/uint32)
///
/////////////////////////////////
int32_t pilot_init(void);
int32_t pilot_shutdown(void);

// Advance : distance en mm, vitesse en mm/s (uint32)
int32_t pilot_advance(int32_t distance_mm, uint32_t max_speed_mm_s);

// Continuous advance : vitesse en mm/s (uint32)
void    pilot_continuousAdvance(uint32_t max_speed_mm_s);

// Turn : angle (rad du robot), vitesse angulaire en rad/s (double)
int32_t pilot_turn(double angle_rad, double max_speed_rad_s, bool relative);

// GoTo : coordonnées x,y en mm (int32), vitesse en mm/s (uint32)
void    pilot_goTo(int32_t x_mm, int32_t y_mm, uint32_t max_speed_mm_s);

// Stop : arrêt direct
void    pilot_stop(void);

// Position/Speeds
int32_t   pilot_getPosition(Position *pos_out);
int       pilot_getSpeed(void);
int       pilot_getDistanceMeter(void);
void      pilot_resetDistanceMeter(void);

// Notification de fin de move, appelé par positionControl
// Doit poster un événement PILOT_EVT_END_MOVE dans la queue d’événements.
void      pilot_notify_end_move(void);

const char* pilot_getErrorString(int32_t error);

/////////////////////////////////
/// @brief Table de transitions (définie en pilot.c)
/////////////////////////////////
extern pilot_transition_t pilot_transitions[PILOT_STATE_COUNT][PILOT_EVT_COUNT];

#endif // PILOT_H

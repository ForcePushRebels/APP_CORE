// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file strategy_manager.h
 * @brief Header file for the Strategy Manager module.
 *
 * @details
 * This file is part of the PATO project developed by ForcePushRebels.
 * The project was coordinated by Mr. Jérôme DELATOURin collaboration with
 * faculty of the Embedded Software and Cybersecurity (LEC) option at ESEO.
 *
 * It constitutes a collective work as per Article L113-2 of the French
 * Intellectual Property Code.
 *
 * Usage is restricted to educational purposes within ESEO. Redistribution,
 * public useor commercial exploitation without prior written consent is prohibited.
 *
 * The content is provided "as is," without any warranty or guarantee.
 *
 * @author
 * ForcePushRebels – PATO Project (collective contributor)
 * Uriel Fodong <uriel.fodong@reseau.eseo.fr> (individual contributor)
 *
 * @version 1.0.0
 *
 * @copyright
 * © 2025 ESEO – All rights reserved.
 *
 * @par License
 * PATO ESEO License (see LICENSE.md)
 */

#ifndef __STRATEGY_MANAGER_H__
#define __STRATEGY_MANAGER_H__

#include <stdbool.h> // For boolean pseudo-type

#include <map_engine.h>
#include <time.h> // For timer management

#include "../../StrategyWrapper/strategy_wrapper.h"

#define MAX_WRAPPER_PER_MANAGER 5
#define MAP_SIZE 10

// Return codes
#define RET_OK 0
#define RET_ERR_GENERIC -1
#define RET_NOT_IMPL_INT -2
#define RET_NOT_IMPL_BOOL false
#define RET_ERR_RANGE -3
#define RET_ERR_NULL -4

// Point structure
typedef struct
{
    int x;
    int y;
} Point;

typedef enum
{
    INIT,             // Initialisation
    PRET,             // Prêt à démarrer
    MISSION_EN_COURS, // Mission en cours
    MODE_MANUEL,      // Contrôle manuel actif
    FIN_DE_MISSION,   // Mission terminée normalement
    ECHEC,            // Échec ou interruption anormale
    STATUS_NB,
} Status;

typedef enum
{
    END_MOVE,       // Fin d'un déplacement élémentaire
    END_ALL_MOVES,  // Fin de la séquence de déplacements
    EMERGENCY_STOP, // Arrêt d'urgence
    MOVE_REASON_NB,
} MoveReason;

////////////////////////////////////////////////////////////
/// @brief Strategy manager structure
/// @note This structure is used to store the strategy manager state
////////////////////////////////////////////////////////////
typedef struct strategy_manager_s
{
    Status status; // Status of the strategy manager

    mat_t matrix[MAP_SIZE][MAP_SIZE]; // Matrix of the strategy manager
    seq_t sequence[MAP_SIZE];         // Sequence of the strategy manager
    struct timespec start_time;       // Start time of the strategy manager
    struct timespec end_time;         // End time of the strategy manager
} strategyManager_t;

typedef strategyManager_t StrategyManager;

////////////////////////////////////////////////////////////
/// @brief Error codes
////////////////////////////////////////////////////////////
#define STRATEGY_MANAGER_BASE 0x2000
#define STRATEGY_MANAGER_OK (STRATEGY_MANAGER_BASE + 0x00)           // Operation successful
#define STRATEGY_MANAGER_ERR_INIT (STRATEGY_MANAGER_BASE + 0x01)     // Generic error
#define STRATEGY_MANAGER_ERR_NOT_IMPL (STRATEGY_MANAGER_BASE + 0x02) // Not implemented error

/* Public methods */

// Constructor and destructor

int strategyManagerInit(void);

void strategy_manager__delete();

// Strategy management

void strategy_manager__askStrat();

int strategy_manager__giveIDStrategieToFollow(int);

void strategy_manager__setMap();

int strategy_manager__getTimeElapsed();

// Movement control

void strategy_manager__startMove();

void strategy_manager__endMove();

// External alert

bool strategy_manager__alertWallNear();

void strategy_manager__alertEndConditionReach();

// Status management

int strategy_manager__getStatus();

void strategy_manager__reportStatus(MoveReason);

// Manual interlock

void strategy_manager__interlockManuMode();

/* Assertions */

void strategy_manager__getInstance(strategyManager_t *instance);

/* Private methods */

// Strategy computation

void strategy_manager__computeStrat(seq_t *);

// Timer management

int strategy_manager__startTimer();

int strategy_manager__stopTimer();

// Status update

void strategy_manager__updateStatus(Status);

#endif /* __STRATEGY_MANAGER_H__ */
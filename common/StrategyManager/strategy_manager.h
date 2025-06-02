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

#include <time.h> // For timer management
#include <map_engine.h>

#include "../../StrategyWrapper/strategy_wrapper.h"

#define STRATEGY_MANAGER_API_VERSION VER(1, 0, 0)

#define MAX_WRAPPER_PER_MANAGER 5
#define MAP_SIZE 10

// -- Strategy Manager Compatibility --
#ifdef STRATEGY_MANAGER_IMPL_VERSION
#if MIN(STRATEGY_MANAGER_IMPL_VERSION) != MIN(STRATEGY_MANAGER_API_VERSION)
#error "strategy_manager.h: Incompatible Strategy Manager major version"
#endif
#endif

// -- Intervention Manager Compatibility --
#ifdef INTERVENTION_MANAGER_IMPL_VERSION
#if MAJ(INTERVENTION_MANAGER_IMPL_VERSION) != MAJ(STRATEGY_MANAGER_API_VERSION)
#error "intervention_manager.h: Incompatible Intervention Manager major version"
#endif
#endif

typedef enum {
	INIT, // Initialisation
	PRET, // Prêt à démarrer
	MISSION_EN_COURS, // Mission en cours
	MODE_MANUEL, // Contrôle manuel actif
	FIN_DE_MISSION, // Mission terminée normalement
	ECHEC, // Échec ou interruption anormale
	STATUS_NB,
} Status;

typedef enum {
	END_MOVE, // Fin d'un déplacement élémentaire
	END_ALL_MOVES, // Fin de la séquence de déplacements
	EMERGENCY_STOP, // Arrêt d'urgence
	MOVE_REASON_NB,
} MoveReason;

typedef struct strategy_manager_s StrategyManager; // To force using the API

/* Public methods */

	// Constructor and destructor

int strategy_manager__init(void);

void strategy_manager__delete();

	// Strategy management

void strategy_manager__askStrat();

void strategy_manager__giveIDStrategieToFollow(int);

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

/* Private methods */

	// Strategy computation

void strategy_manager__computeStrat(seq_t *);

	// Timer management

int strategy_manager__startTimer();

int strategy_manager__stopTimer();

	// Status update

void strategy_manager__updateStatus(Status);

#endif /* __STRATEGY_MANAGER_H__ */
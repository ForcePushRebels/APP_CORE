// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file strategy_manager.h
 * @brief Header file for the Strategy Manager module.
 * 
 * @details
 * This file is part of the PATO project developed by ForcePushRebels.
 * The project was coordinated by Mr. Jérôme DELATOUR, in collaboration with
 * faculty of the Embedded Software and Cybersecurity (LEC) option at ESEO.
 * 
 * It constitutes a collective work as per Article L113-2 of the French
 * Intellectual Property Code.
 *
 * Usage is restricted to educational purposes within ESEO. Redistribution,
 * public use, or commercial exploitation without prior written consent is prohibited.
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

#define STRATEGY_MANAGER_API_VERSION VER(1, 0, 0)

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
	STATUS_NB
} Status;

typedef enum {
	END_MOVE, // Fin d'un déplacement élémentaire
	END_ALL_MOVES, // Fin de la séquence de déplacements
	EMERGENCY_STOP, // Arrêt d'urgence
	MOVE_REASON_NB
} MoveReason;

typedef struct strategy_manager_s StrategyManager; // To force using the API

/* Public methods */

	// Constructor and destructor

StrategyManager * strategy_manager__create(void);

void strategy_manager__delete(StrategyManager *);

	// Strategy management

void strategy_manager__askStrat(StrategyManager *);

void strategy_manager__giveIDStrategieToFollow(StrategyManager *, int);

	// Movement control

void strategy_manager__startMove(StrategyManager *);

void strategy_manager__endMove(StrategyManager *);

	// External alert

bool strategy_manager__alertWallNear(StrategyManager *);

void strategy_manager__alertEndConditionReach(StrategyManager *);

	// Status management

int strategy_manager__getStatus(StrategyManager *);

void strategy_manager__reportStatus(StrategyManager *, MoveReason);

	// Manual interlock

void strategy_manager__interlockManuMode(StrategyManager *);

/* Private methods */

	// Strategy computation

void strategy_manager__computeStrat(StrategyManager *self);

	// Timer management

int strategy_manager__startTimer(StrategyManager *self);

int strategy_manager__stopTimer(StrategyManager *self);

	// Status update

void strategy_manager__updateStatus(StrategyManager *self);

#endif /* __STRATEGY_MANAGER_H__ */
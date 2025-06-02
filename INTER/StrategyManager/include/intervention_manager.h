// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file intervention_manager.h
 * @brief Header file for the Intervention Manager module.
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

#ifndef __INTERVENTION_MANAGER_H__
#define __INTERVENTION_MANAGER_H__

#include <stdbool.h> // For boolean pseudo-type

#include <time.h> // For timer management

#include "../../helpers/util_macros.h"

#include "../include/strategy_manager.h"

#include "strategy_wrapper.h"

#include "map_engine.h"
#include "pilot.h"
#include "sensorManager.h"

#define INTERVENTION_MANAGER_API_VERSION 	VER(1, 0, 0) // current header API version

// -- Intervention Manager Compatibility --
#ifdef INTERVENTION_MANAGER_IMPL_VERSION
#if MIN(INTERVENTION_MANAGER_IMPL_VERSION) != MIN(INTERVENTION_MANAGER_API_VERSION)
#error "intervention_manager.h: Incompatible Intervention Manager major version"
#endif
#endif

typedef struct intervention_manager_s InterventionManager; // To force using the API

/* Public methods */

	// Constructor and destructor

int intervention_manager__init(void);

	// Strategy management

void intervention_manager__askStrat();

void intervention_manager__giveIDStrategieToFollow(int);


void intervention_manager__addStrategyWrapper(StrategyWrapper *);

	// Movement control

void intervention_manager__startMove();

void intervention_manager__endMove();

	// External alert

bool intervention_manager__alertWallNear();

void intervention_manager__alertEndConditionReach();

	// Status management

int intervention_manager__getStatus();

void intervention_manager__reportStatus(MoveReason);

	// Manual interlock
	
void intervention_manager__interlockManuMode();
 
	// Points selection
void intervention_manager__sendPointsSelection(Point **);

	// Intervention control
void intervention_manager__startInter();

void intervention_manager__stopInter();

	// Time management
int intervention_manager__getTimeInter();

#endif /* __INTERVENTION_MANAGER_H__ */
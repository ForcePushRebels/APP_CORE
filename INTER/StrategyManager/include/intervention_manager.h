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

InterventionManager *intervention_manager__create(void);

void intervention_manager__delete(InterventionManager *);

	// Strategy management

void intervention_manager__askStrat(InterventionManager *);

void intervention_manager__giveIDStrategieToFollow(InterventionManager *, int);


void intervention_manager__addStrategyWrapper(InterventionManager *, StrategyWrapper *);

	// Movement control

void intervention_manager__startMove(InterventionManager *);

void intervention_manager__endMove(InterventionManager *);

	// External alert

bool intervention_manager__alertWallNear(InterventionManager *);

void intervention_manager__alertEndConditionReach(InterventionManager *);

	// Status management

int intervention_manager__getStatus(InterventionManager *);

void intervention_manager__reportStatus(InterventionManager *, MoveReason);

	// Manual interlock
	
void intervention_manager__interlockManuMode(InterventionManager *);
 
	// Points selection
void intervention_manager__sendPointsSelection(InterventionManager *self, Point **);

	// Intervention control
void intervention_manager__startInter(InterventionManager *self);

void intervention_manager__stopInter(InterventionManager *self);

	// Time management
int intervention_manager__getTimeInter(InterventionManager *self);

#endif /* __INTERVENTION_MANAGER_H__ */
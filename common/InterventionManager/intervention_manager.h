////////////////////////////////////////////////////////////
//  intervention_manager.h
//  implements intervention_manager
//
// general discloser: copy or share the file is forbidden
// Written : 05/06/2025
////////////////////////////////////////////////////////////

#ifndef __INTERVENTION_MANAGER_H__
#define __INTERVENTION_MANAGER_H__

#include <stdbool.h>

#include "pilot.h"
#include "sensorManager.h"
#include "strategy_manager.h"
#include "xError.h"



#define INTERVENTION_MANAGER_API_VERSION 	VER(1, 0, 0) // current header API version

// -- Intervention Manager Compatibility --
#ifdef INTERVENTION_MANAGER_IMPL_VERSION
#if MIN(INTERVENTION_MANAGER_IMPL_VERSION) != MIN(INTERVENTION_MANAGER_API_VERSION)
#error "intervention_manager.h: Incompatible Intervention Manager major version"
#endif
#endif

////////////////////////////////////////////////////////////
/// @brief Intervention manager structure
/// @note This structure is used to store the intervention manager state
////////////////////////////////////////////////////////////
typedef struct intervention_manager_s
{
    seq_t pathPoints[100];
    int currentPointIdx;
    int nextPointIdx;
    double angleToNextPoint;
    double distanceToNextPoint;
    Point listZI[100]; // Zone d'intérêt
} InterventionManager;

////////////////////////////////////////////////////////////
/// @brief Initialize the intervention manager
////////////////////////////////////////////////////////////
int intervention_manager__init(void);

////////////////////////////////////////////////////////////
/// @brief Give the id of the strategy to follow
////////////////////////////////////////////////////////////
int giveIdStrategyToFollow(int);

////////////////////////////////////////////////////////////
/// @brief Start the movement
////////////////////////////////////////////////////////////
void intervention_manager__startMove();

////////////////////////////////////////////////////////////
/// @brief End the movement
////////////////////////////////////////////////////////////
void intervention_manager__endMove();

////////////////////////////////////////////////////////////
/// @brief Alert the wall near
////////////////////////////////////////////////////////////
bool intervention_manager__alertWallNear();

////////////////////////////////////////////////////////////
/// @brief Alert the end condition reach
////////////////////////////////////////////////////////////
void intervention_manager__alertEndConditionReach();

// Status management

int intervention_manager__getStatus(); // @Override

void intervention_manager__reportStatus(MoveReason); // @Override

// Manual interlock

void intervention_manager__interlockManuMode(); // @Override

// Points selection

void intervention_manager__sendPointsSelection(Point **);

// Intervention control
int intervention_manager__startInter();

void intervention_manager__stopInter();

// Time management

int intervention_manager__getTimeInter();

#endif /* __INTERVENTION_MANAGER_H__ */
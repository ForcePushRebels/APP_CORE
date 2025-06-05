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

////////////////////////////////////////////////////////////
/// @brief Error codes
////////////////////////////////////////////////////////////
#define INTERVENTION_MANAGER_BASE 0x1AE07800
#define INTERVENTION_MANAGER_OK (INTERVENTION_MANAGER_BASE + 0x00)           // Operation successful
#define INTERVENTION_MANAGER_ERR_INIT (INTERVENTION_MANAGER_BASE + 0x01)     // Generic error
#define INTERVENTION_MANAGER_ERR_NOT_IMPL (INTERVENTION_MANAGER_BASE + 0x02) // Not implemented error

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
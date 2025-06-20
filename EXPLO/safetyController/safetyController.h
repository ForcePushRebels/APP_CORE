////////////////////////////////////////////////////////////
//  safety controller header file
//  defines safety controller types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 28/05/2025
////////////////////////////////////////////////////////////

#ifndef SAFETY_CONTROLLER_H_
#define SAFETY_CONTROLLER_H_

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>

#include "handleNetworkMessage.h"
#include "pilot.h"
#include "sensorManager.h"
#include "xAssert.h"
#include "xLog.h"
#include "xServer.h"

///////////////////////////////////////////
/// @brief Movement type
///////////////////////////////////////////
typedef enum movementType
{
    STOP_MOVEMENT,
    FORWARD_MOVEMENT,
    LEFT_MOVEMENT,
    RIGHT_MOVEMENT,
    MOVE_COUNT,
} movement_type_t;

///////////////////////////////////////////
/// @brief Emergency stop flag
///////////////////////////////////////////
static atomic_bool s_bEmergencyStopFlag = ATOMIC_VAR_INIT(false);

///////////////////////////////////////////
/// @brief Initialize safety controller network callbacks
/// @return void
///////////////////////////////////////////
void safetyControllerInit(void);

///////////////////////////////////////////
/// @brief Emergency stop flag
/// @return void
///////////////////////////////////////////
void stopSafetyController(void);

///////////////////////////////////////////
/// @brief Set autorized movement
/// @return void
///////////////////////////////////////////
void setAutorizedMovement(void);

///////////////////////////////////////////
/// @brief Disable manual movement authorization
///////////////////////////////////////////
void disableManualMovement(void);

#endif // SAFETY_CONTROLLER_H_
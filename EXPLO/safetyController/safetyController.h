////////////////////////////////////////////////////////////
//  safety controller header file
//  defines safety controller types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 28/05/2025
////////////////////////////////////////////////////////////

#ifndef SAFETY_CONTROLLER_H_
#define SAFETY_CONTROLLER_H_

#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>

#include "xLog.h"
#include "handleNetworkMessage.h"
#include "xAssert.h"
#include "sensorManager.h"
#include "pilot.h"

///////////////////////////////////////////
/// @brief Movement type
///////////////////////////////////////////
typedef enum movementType 
{
    STOP_MOVEMENT,
    FORWARD_MOVEMENT,
    LEFT_MOVEMENT,
    RIGHT_MOVEMENT,
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




#endif // SAFETY_CONTROLLER_H_
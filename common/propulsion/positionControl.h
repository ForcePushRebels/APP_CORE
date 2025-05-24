////////////////////////////////////////////////////////////
// Module: positionControl
// Description: Module for position control
//
// Written : 23/05/2025
////////////////////////////////////////////////////////////
#ifndef POSITION_CONTROL_H
#define POSITION_CONTROL_H

#include <stdint.h>
#include <stdbool.h>
#include "mrpiz.h"
#include "error.h"
#include "xOsMutex.h"
#include "xLog.h"
#include "xAssert.h"
#include "xTask.h"
#include "hardwareAbstraction.h"
#include "motorControl.h"

enum  move_type_t
{
    FORWARD,
    LEFT,
    RIGHT
};

// Public functions

////////////////////////////////////////////////////////////
/// @brief Initialize the position control system
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int16_t position_control_init(void);

////////////////////////////////////////////////////////////
/// @brief Shutdown the position control system
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int16_t position_control_shutdown(void);

////////////////////////////////////////////////////////////
/// @brief Check if the current motion is finished
/// @return true if motion is finished, false otherwise
////////////////////////////////////////////////////////////
bool position_control_is_motion_finished(void);

////////////////////////////////////////////////////////////
/// @brief Advance the robot by a given distance
/// @param distance_mm Distance to advance in millimeters
/// @param speed_rad_s_max Maximum speed in radians per second
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int16_t position_control_advance(int16_t distance_mm, float speed_rad_s_max);

////////////////////////////////////////////////////////////
/// @brief Turn the robot by a given angle
/// @param angle_rad Angle to turn in radians
/// @param speed_rad_s_max Maximum speed in radians per second
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int16_t position_control_turn(int16_t angle_rad, float speed_rad_s_max);

////////////////////////////////////////////////////////////
/// @brief Stop the robot
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int16_t position_control_stop(void);

// Test functions
int16_t position_control_test_straight_line(void);

#endif // POSITION_CONTROL_H

////////////////////////////////////////////////////////////
// Module: motorControl
// Description: Module de contrôle des moteurs
//
// Written : 23/05/2025
////////////////////////////////////////////////////////////
#ifndef MOTOR_CONTROL_H
#define MOTOR_CONTROL_H

#include <stdint.h>
#include <stdbool.h>
#include "mrpiz.h"
#include "error.h"
#include "xOsMutex.h"
#include "xLog.h"
#include "xAssert.h"
#include "xTask.h"
#include "hardwareAbstraction.h"

#define WHEEL_RADIUS_M      0.03
#define ENCODER_TICKS_REV   360
#define REGULATION_PERIOD_MS 50
#define KP                  30.0    // Gain proportionnel à adapter
#define KI                  0.1     // Gain intégral
#define MAX_INTEGRAL_ERROR  100.0   // Limite de l'erreur intégrale
#define MAX_SPEED_CM_S      16.0
#define MAX_SPEED_RAD_S     (MAX_SPEED_CM_S / (WHEEL_RADIUS_M * 100.0))  // Conversion de cm/s en rad/s

// Fonctions publiques

////////////////////////////////////////////////////////////
/// @brief Initialize the motor control system
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int motor_control_init(void);

////////////////////////////////////////////////////////////
/// @brief Shutdown the motor control system
/// @return void
////////////////////////////////////////////////////////////
void motor_control_shutdown(void);

////////////////////////////////////////////////////////////
/// @brief Get the current speed of the left motor
/// @return Current speed in radians per second
////////////////////////////////////////////////////////////
double motor_control_get_left_speed(void);

////////////////////////////////////////////////////////////
/// @brief Get the current speed of the right motor
/// @return Current speed in radians per second
////////////////////////////////////////////////////////////
double motor_control_get_right_speed(void);

////////////////////////////////////////////////////////////
/// @brief Set the target speed for the left motor
/// @param speed_rad_s Target speed in radians per second
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int motor_control_set_left_speed(double speed_rad_s);

////////////////////////////////////////////////////////////
/// @brief Set the target speed for the right motor
/// @param speed_rad_s Target speed in radians per second
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int motor_control_set_right_speed(double speed_rad_s);

////////////////////////////////////////////////////////////
/// @brief Stop both motors
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int motor_control_stop(void);

#endif // MOTOR_CONTROL_H

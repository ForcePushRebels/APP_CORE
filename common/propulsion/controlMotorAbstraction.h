////////////////////////////////////////////////////////////
// Module: ControlMotorAbstraction
// Description: Hardware Abstraction Layer for the motor control system
// Provides definitions for basic hardware types, constants, and utilities
//
// Written : 12/05/2025
////////////////////////////////////////////////////////////
#ifndef CONTROL_MOTOR_ABSTRACTION_H
#define CONTROL_MOTOR_ABSTRACTION_H

// Include necessary headers
#include <iostream>

// system includes
#include <stdint.h>
#include <stdbool.h>

// project includes
#include "hardwareAbstraction.h"
#include "error.h"
#include "xOsMutex.h"
#include "xLog.h"
#include "xAssert.h"
#include "xTask.h"

// Define constants
#define MOTOR_SPEED_MAX 100  // Maximum motor speed
#define MOTOR_SPEED_MIN -100 // Minimum motor speed
#define MOTOR_SPEED_STOP 0   // Motor stop speed
#define MOTOR_LEFT 0         // Left motor identifier
#define MOTOR_RIGHT 1        // Right motor identifier


////////////////////////////////////////////////////////////
/// @brief Initialize the motor control system
/// This function sets up the necessary hardware and software
/// components required for motor control.
/// @return 0 if initialization is successful, -1 if error
////////////////////////////////////////////////////////////
int8_t initMotorControl();

////////////////////////////////////////////////////////////
/// @brief Set the speed of the left motor
/// @param speed Desired speed for the left motor
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int8_t setLeftMotorSpeed(int16_t speed);

////////////////////////////////////////////////////////////
/// @brief Set the speed of the right motor
/// @param speed Desired speed for the right motor
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int8_t setRightMotorSpeed(int16_t speed);

////////////////////////////////////////////////////////////
/// @brief Get the encoder value of the left motor
/// @return Encoder value of the left motor
////////////////////////////////////////////////////////////
int16_t getLeftMotorEncoder();

////////////////////////////////////////////////////////////
/// @brief Get the encoder value of the right motor
/// @return Encoder value of the right motor
////////////////////////////////////////////////////////////
int16_t getRightMotorEncoder();

////////////////////////////////////////////////////////////
/// @brief Reset the encoder values of both motors
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int8_t resetMotorEncoders();

////////////////////////////////////////////////////////////
/// @brief Stops the motor operation immediately
////////////////////////////////////////////////////////////
void stopMotor();

#endif // CONTROL_MOTOR_ABSTRACTION_H

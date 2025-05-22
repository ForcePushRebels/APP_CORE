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
#include <mqueue.h>

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

#define HYSTERESIS 5 // Hysteresis value for speed regulation
#define REGULATOR_TASK_DELAY 100 // Delay for regulator task in milliseconds

////////////////////////////////////////////////////////////
/// @param motor_t
/// @brief motor structure
////////////////////////////////////////////////////////////
typedef struct motor
{
    int idMotor;          // Motor identifier (left or right)
    bool move;          // Flag to indicate if the motor is moving
    bool setpointReached; // Flag to indicate if the setpoint is reached
    uint32_t timeLastReadEncoder; // Encoder value for the motor
    regulator_t *regulator; // Pointer to the regulator structure
    int32_t encoderValue;   // Encoder value for the motor
    int16_t speedValueCurent;     // Speed value for the motor
    int16_t speedValueCmd;        // Command speed value for the motor
    int16_t speedValueTarget; // Target speed value for the motor
    int16_t valueP;     // Proportional speed value for the motor
    int16_t valueI;     // Integral speed value for the motor
    int16_t valueD;     // Derivative speed value for the motor
} motor_t;

/// @brief Regulator manager structure
////////////////////////////////////////////////////////////
typedef struct regulatorManager_t
{
    xOsTaskCtx t_tTaskHandler;
    //xOsMutexCtx t_tMutex;
    //xOsTimerCtx t_tTimer;
} regulatorManager_t;

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
/// @return 0 if success, -1 if error mrpiz, -2 if speed out of range
////////////////////////////////////////////////////////////
int8_t setLeftMotorSpeed(int16_t speed);

////////////////////////////////////////////////////////////
/// @brief Set the speed of the right motor
/// @param speed Desired speed for the right motor
/// @return 0 if success, -1 if error mrpiz, -2 if speed out of range
////////////////////////////////////////////////////////////
int8_t setRightMotorSpeed(int16_t speed);

////////////////////////////////////////////////////////////
/// @brief Stops the motor operation immediately
////////////////////////////////////////////////////////////
void stopMotor();

////////////////////////////////////////////////////////////
/// @brief Checks if the right motor setpoint has been reached
/// @return true if the setpoint is reached, false otherwise
////////////////////////////////////////////////////////////
bool checkRightMotorSetpointReached();

////////////////////////////////////////////////////////////
/// @brief Checks if the left motor setpoint has been reached
/// @return true if the setpoint is reached, false otherwise
////////////////////////////////////////////////////////////
bool checkLeftMotorSetpointReached();

#endif // CONTROL_MOTOR_ABSTRACTION_H

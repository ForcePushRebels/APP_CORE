////////////////////////////////////////////////////////////
//  Hardware Abstraction Layer header file for embedded systems
//  Defines the basic hardware types and constants to use in the project
//
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#ifndef HARDWARE_ABSTRACTION_H
#define HARDWARE_ABSTRACTION_H

// system includes
#include <stdint.h>
#include <stdbool.h>

// project includes
#include "mrpiz.h"
#include "error.h"
#include "xOsMutex.h"
#include "xLog.h"
#include "xAssert.h"
#include "xTask.h"

#define HARDWARE_ABSTRACTION_MAX_SENSORS 5
#define HARDWARE_ABSTRACTION_MAX_MOTORS 2

////////////////////////////////////////////////
///  Hardware Abstraction Layer structure
///
///  @param t_Mutex : Mutex for thread safety
///  @param t_iSensors : Array of sensors
///  @param t_iMotors : Array of motors
///  @param t_iLedColor : Color of the LED
///  @param t_iBatteryLevel : Level of the battery
///  @param t_fBatteryVoltage : Voltage of the battery
///  @detail: this structure is used to abstract the hardware layer and only for the hardwareAbstractionInit function. You cannot allocate double time the structure.
////////////////////////////////////////////////
typedef struct
{
    uint16_t t_iSensors[HARDWARE_ABSTRACTION_MAX_SENSORS];
    uint16_t t_iMotors[HARDWARE_ABSTRACTION_MAX_MOTORS];
    mrpiz_led_rgb_color_t t_iLedColor;
    uint8_t t_iBatteryLevel;
    uint8_t t_iMotorEncoder[HARDWARE_ABSTRACTION_MAX_MOTORS];
    float t_fBatteryVoltage;
} hardwareAbstraction_t;

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer initialization
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int hardwareAbstractionInit();

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer close
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int hardwareAbstractionClose();

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get sensor values
/// @param p_ptISensors : pointer to array to store sensor values
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int GetSensorValues(uint16_t *p_ptISensors);

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get motor encoder values
/// @param p_ptIMotors : pointer to array to store motor encoder values
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int GetMotorEncoderValues(uint16_t *p_ptIMotors);

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get battery level
/// @return value of the battery level
////////////////////////////////////////////////////////////
int GetBatteryLevel();

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get battery voltage
/// @return value of the battery voltage
////////////////////////////////////////////////////////////
float GetBatteryVoltage();

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get led color
/// @return value of the led color
////////////////////////////////////////////////////////////
mrpiz_led_rgb_color_t GetLedColor();

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer set led color
/// @param p_iLedColor : color of the led
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int SetLedColor(mrpiz_led_rgb_color_t p_iLedColor);

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer set Motor Speed
/// @param p_iMotor : id of the motor
/// @param p_iSpeed : speed of the motor (-100 to 100)
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int SetMotorSpeed(uint8_t p_iMotor, int p_iSpeed);

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer reset motor encoders
/// @return 0 if success, -1 if error
////////////////////////////////////////////////////////////
int ResetMotorEncoders();

#endif // HARDWARE_ABSTRACTION_H
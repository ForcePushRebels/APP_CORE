////////////////////////////////////////////////////////////
//  Hardware Abstraction Layer implementation file for embedded systems
//  Defines the basic hardware types and constants to use in the project
//
// Written : 25/04/2025
////////////////////////////////////////////////////////////
#include "hardwareAbstraction.h"


////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer structure
////////////////////////////////////////////////////////////
static hardwareAbstraction_t t_HardwareAbstraction;

////////////////////////////////////////////////////////////
/// @brief atomic variable for the hardware abstraction layer
////////////////////////////////////////////////////////////
static bool t_bHardwareAbstractionInitialized = false;


////////////////////////////////////////////////////////////
//  hardwareAbstractionInit
////////////////////////////////////////////////////////////
int hardwareAbstractionInit()
{
    X_LOG_TRACE("Initialise hardware abstraction layer with MrPiz");
    int l_iReturn = 0;
    
    if (t_bHardwareAbstractionInitialized) {
        X_LOG_TRACE("Hardware abstraction layer already initialized");
        return 0;
    }
    
    l_iReturn = mrpiz_init();
    if (l_iReturn != 0) 
    {
        X_LOG_TRACE("Failed to initialize MrPiz, error=%d", l_iReturn);
        return -1;
    }

    //init sensors
    t_HardwareAbstraction.t_iSensors[0] = MRPIZ_PROXY_SENSOR_FRONT_LEFT;
    t_HardwareAbstraction.t_iSensors[1] = MRPIZ_PROXY_SENSOR_FRONT_CENTER_LEFT;
    t_HardwareAbstraction.t_iSensors[2] = MRPIZ_PROXY_SENSOR_FRONT_CENTER;
    t_HardwareAbstraction.t_iSensors[3] = MRPIZ_PROXY_SENSOR_FRONT_CENTER_RIGHT;
    t_HardwareAbstraction.t_iSensors[4] = MRPIZ_PROXY_SENSOR_FRONT_RIGHT;

    //init motors
    t_HardwareAbstraction.t_iMotors[0] = MRPIZ_MOTOR_LEFT;
    t_HardwareAbstraction.t_iMotors[1] = MRPIZ_MOTOR_RIGHT;

    //init led
    t_HardwareAbstraction.t_iLedColor = MRPIZ_LED_RED;
    // Définir la couleur initiale de la LED
    mrpiz_led_rgb_set(t_HardwareAbstraction.t_iLedColor);

    //set initialized
    t_bHardwareAbstractionInitialized = true;

    return 0;
}

////////////////////////////////////////////////////////////
/// hardwareAbstractionClose
////////////////////////////////////////////////////////////
int hardwareAbstractionClose()
{
    X_LOG_TRACE("Close hardware abstraction layer with MrPiz");

    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    // Arrêt des moteurs
    SetMotorSpeed(0, 0);
    SetMotorSpeed(1, 0);
    
    // Extinction de la LED
    mrpiz_led_rgb_set(MRPIZ_LED_OFF);

    mrpiz_close();

    t_bHardwareAbstractionInitialized = false;
    
    return 0;
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get sensor values
////////////////////////////////////////////////////////////
int GetSensorValues(uint16_t* p_ptISensors)
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    for (int i = 0; i < HARDWARE_ABSTRACTION_MAX_SENSORS; i++)
    {
        // La fonction s'appelle mrpiz_proxy_sensor_get et non mrpiz_proxy_sensor_read
        int value = mrpiz_proxy_sensor_get(t_HardwareAbstraction.t_iSensors[i]);
        if (value != -1) 
        {
            p_ptISensors[i] = (uint16_t)value;
        } 

        else
        {
            X_LOG_TRACE("Failed to read sensor %d", i);
            return -1;
        }
    }
    return 0;
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get motor encoder values
////////////////////////////////////////////////////////////
int GetMotorEncoderValues(uint16_t* p_ptIMotors)
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    for (int i = 0; i < HARDWARE_ABSTRACTION_MAX_MOTORS; i++)
    {
        // La fonction s'appelle mrpiz_motor_encoder_get et non mrpiz_motor_encoder_read
        int l_iValue = mrpiz_motor_encoder_get(t_HardwareAbstraction.t_iMotors[i]);
        if (l_iValue != -1) 
        {
            p_ptIMotors[i] = (uint16_t)l_iValue;
        } 

        else 
        {
            X_LOG_TRACE("Failed to read motor encoder");
            return -1;
        }
    }
    return 0;
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get battery level
////////////////////////////////////////////////////////////
int GetBatteryLevel()
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    return mrpiz_battery_level();
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get battery voltage
////////////////////////////////////////////////////////////
float GetBatteryVoltage()
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    // Cette fonction retourne un float, pas un int
    return mrpiz_battery_voltage();
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer get led color
////////////////////////////////////////////////////////////
mrpiz_led_rgb_color_t GetLedColor()
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    // cannot use mrpiz_led_rgb_get() because it is not defined in the API
    return t_HardwareAbstraction.t_iLedColor;
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer set led color
////////////////////////////////////////////////////////////
int SetLedColor(mrpiz_led_rgb_color_t p_iLedColor)
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    // La fonction s'appelle mrpiz_led_rgb_set et non mrpiz_led_color
    int result = mrpiz_led_rgb_set(p_iLedColor);
    if (result == 0) 
    {
        t_HardwareAbstraction.t_iLedColor = p_iLedColor;
    }
    return result;
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer set motor speed
////////////////////////////////////////////////////////////
int SetMotorSpeed(uint8_t p_iMotor, int p_iSpeed)
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    if (p_iMotor >= HARDWARE_ABSTRACTION_MAX_MOTORS) 
    {
        X_LOG_TRACE("Invalid motor ID: %d", p_iMotor);
        return -1;
    }
    
    if (p_iSpeed > 100) p_iSpeed = 100;
    if (p_iSpeed < -100) p_iSpeed = -100;
    
    return mrpiz_motor_set(t_HardwareAbstraction.t_iMotors[p_iMotor], p_iSpeed);
}

////////////////////////////////////////////////////////////
/// @brief Hardware Abstraction Layer reset motor encoders
////////////////////////////////////////////////////////////
int ResetMotorEncoders()
{
    X_ASSERT(t_bHardwareAbstractionInitialized == true);
    
    return mrpiz_motor_encoder_reset(MRPIZ_MOTOR_BOTH);
}

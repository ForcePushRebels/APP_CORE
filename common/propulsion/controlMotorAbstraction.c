#include "controlMotorAbstraction.h"

// Global variables for motor control (if needed)
static int16_t leftMotorEncoder = 0;
static int16_t rightMotorEncoder = 0;

static motor_t *leftMotor = NULL;
static motor_t *rightMotor = NULL;

int8_t initMotorControl()
{
    if (leftMotor == NULL || rightMotor == NULL) {
        X_LOG_TRACE("Failed to allocate memory for motor structures");
        return -1; // Memory allocation error   
    }

    // Initialize the motors
    leftMotor->idMotor = MRPIZ_MOTOR_LEFT;
    leftMotor->move = false;
    leftMotor->encoderValue = GetMotorEncoderValues(leftMotor->idMotor);
    leftMotor->speedValueCurent = 0;
    leftMotor->speedValueTarget = MOTOR_SPEED_STOP;
    leftMotor->valueP = 0;
    leftMotor->valueI = 0;
    leftMotor->valueD = 0;

    rightMotor->idMotor = MRPIZ_MOTOR_RIGHT;
    rightMotor->move = false;
    rightMotor->encoderValue = GetMotorEncoderValues(rightMotor->idMotor);
    rightMotor->speedValueCurent = 0;
    rightMotor->speedValueTarget = MOTOR_SPEED_STOP;
    rightMotor->valueP = 0;
    rightMotor->valueI = 0;
    rightMotor->valueD = 0;

    //Initialize the regulator

    
}



int8_t setLeftMotorSpeed(int16_t speed) 
{
    //TODO: Set the speed of the left motor
    
    // Check if the speed is within the valid range
    if (speed < MOTOR_SPEED_MIN || speed > MOTOR_SPEED_MAX) {
        X_LOG_TRACE("Invalid speed value for left motor: %d. Must be between %d and %d.", speed, MOTOR_SPEED_MIN, MOTOR_SPEED_MAX);
        return -2;       
    }

    //X_ASSERT(speed >= (-1*MOTOR_SPEED_MAX) && speed <= MOTOR_SPEED_MAX);

    int8_t speedValue = speed * 100 / MOTOR_SPEED_MAX; // Scale speed to 0-100 range

    // Set the speed of the left motor
    return mrpiz_motor_set(MRPIZ_MOTOR_LEFT, (int)speedValue);
}

int8_t setRightMotorSpeed(int16_t speed) 
{ 
    //TODO: Set the speed of the right motor
    // Check if the speed is within the valid range
    if (speed < MOTOR_SPEED_MIN || speed > MOTOR_SPEED_MAX) {
        X_LOG_TRACE("Invalid speed value for right motor: %d. Must be between %d and %d.", speed, MOTOR_SPEED_MIN, MOTOR_SPEED_MAX);
        return -2;       
    }
    //X_ASSERT(speed >= (-1*MOTOR_SPEED_MAX) && speed <= MOTOR_SPEED_MAX);
    int8_t speedValue = speed * 100 / MOTOR_SPEED_MAX; // Scale speed to 0-100 range
    
    // Set the speed of the left motor 
    return mrpiz_motor_set(MRPIZ_MOTOR_RIGHT, (int)speedValue);
}

void stopMotor() 
{
    //TODO: Stop both motors
    mrpiz_motor_set(MRPIZ_MOTOR_BOTH, (int)MOTOR_SPEED_STOP);
}

////////////////////////////////////////////////////////////
/// regulator_thread
////////////////////////////////////////////////////////////
void* regulator_thread(void *arg)
{

   
   if (!regulator) {
       X_LOG_TRACE("regulator thread started with NULL context");
       return (void*)(intptr_t)regulator_ERROR_INVALID_PARAM;
   }
   
   X_LOG_TRACE("regulator thread started");
   
   // Main thread loop
   while (!regulator->terminate) {
       // Ping the regulator to reset the timer
       l_iRet = regulator_ping();
       if (l_iRet != regulator_SUCCESS) {
           X_LOG_TRACE("regulator ping failed in thread: %s (code: 0x%x)", 
                     regulator_get_error_string(l_iRet), l_iRet);
       }
       
       // Wait some time before the next ping
       // Use about 1/3 of the timeout to ensure multiple pings before expiration
       usleep((regulator->timeout / 3) * 1000);
   }
   
   X_LOG_TRACE("regulator thread terminated normally");
   return (void*)(intptr_t)regulator_SUCCESS;
}
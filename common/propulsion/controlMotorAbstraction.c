
#include "controlMotorAbstraction.h"

// Global variables for motor control (if needed)
static int16_t leftMotorEncoder = 0;
static int16_t rightMotorEncoder = 0;


int8_t setLeftMotorSpeed(int16_t speed) {
    //TODO: Set the speed of the left motor
    if (speed < MOTOR_SPEED_MIN || speed > MOTOR_SPEED_MAX) {
        return -1; // Invalid speed
    }
    // Set the speed of the left motor
    mrpiz_motor_set(MRPIZ_MOTOR_LEFT, speed);
}

int8_t setRightMotorSpeed(int16_t speed) { 
    //TODO: Set the speed of the right motor
}

int16_t getLeftMotorEncoder() {
    //TODO: Return the encoder value of the left motor
}

int16_t getRightMotorEncoder() {
    //TODO: Return the encoder value of the right motor
}

int8_t resetMotorEncoders() {
    //TODO: Reset the encoder values of both motors
}

void stopMotor() {
    //TODO: Stop both motors
}
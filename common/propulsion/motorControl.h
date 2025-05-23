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


// Fonctions publiques
int motor_control_init(void);
void motor_control_shutdown(void);
double motor_control_get_left_speed(void);
double motor_control_get_right_speed(void);
int motor_control_set_left_speed(double speed_rad_s);
int motor_control_set_right_speed(double speed_rad_s);
int motor_control_stop(void);

#endif // MOTOR_CONTROL_H

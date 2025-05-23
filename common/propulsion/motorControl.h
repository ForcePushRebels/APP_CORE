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

// Définition des constantes
#define MAX_SPEED_RAD_S 10.0  // Vitesse maximale en rad/s


// Fonctions publiques
int motor_control_init(void);
void motor_control_shutdown(void);
int motor_control_set_speed(uint16_t motor_id, double speed_rad_s);
double motor_control_get_left_speed(void);
double motor_control_get_right_speed(void);

#endif // MOTOR_CONTROL_H

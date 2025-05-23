////////////////////////////////////////////////////////////
// Module: motorControl
// Description: Module de contrôle des moteurs
//
// Written : 23/05/2025
////////////////////////////////////////////////////////////
#ifndef MOTORCONTROL_H
#define MOTORCONTROL_H

#include <stdbool.h>

// Initialisation et arrêt global du module motor_control
int motor_control_init(void);
void motor_control_shutdown(void);

// Commande de vitesse en rad/s pour chaque moteur
int motor_control_set_left_speed(double speed_rad_s);
int motor_control_set_right_speed(double speed_rad_s);

// Lecture de la vitesse réelle en rad/s pour chaque moteur
double motor_control_get_left_speed(void);
double motor_control_get_right_speed(void);

// Arrêt d'urgence des deux moteurs
void motor_control_stop(void);

#endif // MOTORCONTROL_H

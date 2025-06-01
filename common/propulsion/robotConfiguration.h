#ifndef ROBOT_CONFIGURATION_H
#define ROBOT_CONFIGURATION_H

#include <math.h>

#define WHEEL_DIAMETER_CM     (3.2)                                 // Rayon de la roue en cm
#define ENCODER_TICKS_REV   (390)                                   // Nombre de ticks par tour de l'encodeur
#define MAX_SPEED_CM_S      (16.0)                                  // Vitesse maximale en cm/s
#define MAX_SPEED_RAD_S     (MAX_SPEED_CM_S / WHEEL_DIAMETER_CM)    // Conversion de cm/s en rad/s
#define WHEEL_DISTANCE_CM   (8.0)                                  // Distance entre les roues en cm
#define WHEEL_PERIMETER_CM  (WHEEL_DIAMETER_CM * M_PI)              // Périmètre de la roue en cm
#define ANGLE_PER_TICK_DEG  (0.21)                                  // Angle par tick en degrés
#define ANGLE_PER_TICK_RAD  (ANGLE_PER_TICK_DEG * M_PI / 180.0)     // Conversion de degrés en radians

#endif // ROBOT_CONFIGURATION_H
#ifndef ROBOT_CONFIGURATION_H
#define ROBOT_CONFIGURATION_H

#define WHEEL_RADIUS_CM     (3.2)                                   // Rayon de la roue en cm
#define ENCODER_TICKS_REV   (390)                                   // Nombre de ticks par tour de l'encodeur
#define MAX_SPEED_CM_S      (16.0)                                  // Vitesse maximale en cm/s
#define MAX_SPEED_RAD_S     (MAX_SPEED_CM_S / WHEEL_RADIUS_CM)      // Conversion de cm/s en rad/s
#define WHEEL_DISTANCE_CM   (10.0)                                  // Distance entre les roues en cm

#endif // ROBOT_CONFIGURATION_H
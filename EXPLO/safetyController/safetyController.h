#ifndef SAFETY_CONTROLLER_H
#define SAFETY_CONTROLLER_H

#include <stdbool.h>

/**
 * @brief Structure représentant le contrôleur de sécurité.
 */
typedef struct 
{
    bool emergencyStopFlag; /**< Indique si l'arrêt d'urgence est actif. */
    
} SafetyController;

/**
 * @brief Initialise le contrôleur de sécurité.
 * @param ctrl Pointeur vers la structure SafetyController à initialiser.
 */
void SafetyController_init(SafetyController* ctrl);

/**
 * @brief Déclenche l'arrêt d'urgence du robot.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param decelerationFactor Facteur de décélération à appliquer lors de l'arrêt.
 */
void stop(SafetyController* ctrl, double decelerationFactor);

/**
 * @brief Demande au robot d’avancer en mode manuel si la voie est libre.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale d'avance.
 */
void moveForward(SafetyController* ctrl, float max_speed);

/**
 * @brief Effectue une rotation à gauche de 90°.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale de rotation.
 */
void moveLeft(SafetyController* ctrl, float max_speed);

/**
 * @brief Effectue une rotation à droite de 90°.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale de rotation.
 */
void moveRight(SafetyController* ctrl, float max_speed);

#endif // SAFETY_CONTROLLER_H
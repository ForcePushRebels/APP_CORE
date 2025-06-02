#ifndef SAFETY_CONTROLLER_H
#define SAFETY_CONTROLLER_H

/**
 * @file safetyController.h
 * @brief Interface du contrôleur de sécurité pour la gestion des arrêts d'urgence et des mouvements du robot.
 *
 * Ce fichier définit les fonctions et structures nécessaires pour gérer la sécurité du robot,
 * y compris l'arrêt d'urgence, les mouvements en avant, à gauche et à droite.
 */
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
 * @brief Vérifie si le mouvement est possible en avant.
 * @return true si la voie est libre, false sinon.
 */

/**
 * @brief Demande au robot d’avancer en mode manuel si la voie est libre.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale d'avance.
 */
void moveForward(SafetyController* ctrl, float max_speed);


/**
 * 
 * 
 * @brief Effectue une rotation à gauche de 90°.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale de rotation.
 */
void moveLeft(SafetyController* ctrl, float max_speed, bool relative) ; 


/**
 * @brief Effectue une rotation à droite de 90°.
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale de rotation.
 */
void moveRight(SafetyController* ctrl, float max_speed, bool relative) ;


/**
 * @brief Réinitialise l'arrêt d'urgence du SafetyController.
 *
 * Cette fonction désactive le flag d'arrêt d'urgence, permettant ainsi au robot de reprendre ses mouvements.
 * Un message de trace est généré pour indiquer la réinitialisation.
 *
 * @param ctrl Pointeur vers la structure SafetyController à réinitialiser.
 */
void resetEmergencyStop(SafetyController* ctrl);


#endif // SAFETY_CONTROLLER_H
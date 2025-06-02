#include <stdbool.h>
#include <stdio.h>
#include "safetyController.h"
#include "sensorManager.h"
#include "pilot.h"
#include "motorControl.h"
#include "xLog.h"
#include "xAssert.h"
#include "xTimer.h"


#define SENSOR_OBSTACLE_THRESHOLD 150

/**
 * @brief Initialise le contrôleur de sécurité.
 * 
 * @param ctrl Pointeur vers la structure SafetyController à initialiser.
 */
void SafetyController_init(SafetyController* ctrl) 
{
    ctrl->emergencyStopFlag = false;
}


/**
 * @brief Déclenche l'arrêt d'urgence du robot.
 * 
 * Si l'arrêt d'urgence est déjà actif, la fonction ne fait rien.
 * 
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param decelerationFactor Facteur de décélération à appliquer lors de l'arrêt.
 */ 
void stop(SafetyController* ctrl, double decelerationFactor) 
{
    if (ctrl->emergencyStopFlag == true) 
    {
        X_LOG_TRACE("[SafetyController] Arrêt d'urgence déjà actif.\n");
        return;
    }
    ctrl->emergencyStopFlag = true;
    pilot_stop(decelerationFactor);  
    //pilot_action_emergencyStop(); 
    X_LOG_TRACE("[SafetyController] Arrêt d'urgence déclenché !\n");
}


/**
 * @brief Demande au robot d’avancer en mode manuel si la voie est libre.
 * 
 * Vérifie l'absence d'obstacle via checkForwoad grace SensorManager. Si la voie est libre, 
 * appelle Pilot pour avancer à la vitesse max spécifiée.
 * 
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale d'avance.
 */
void moveForward(SafetyController* ctrl, float max_speed) 
{
    // Si l'arrêt d'urgence est actif, on ne permet pas le mouvement
    if (ctrl->emergencyStopFlag == true) 
    {
        X_LOG_TRACE("[SafetyController] Mouvement bloqué : arrêt d'urgence actif.\n");
        return;
    } 
    if (checkForward() == true) 
    {
        pilot_continuousAdvance(max_speed);
        X_LOG_TRACE("[SafetyController] Avance autorisée.\n");
    } 
    else 
    {
        X_LOG_TRACE("[SafetyController] Mouvement bloqué : obstacle détecté !\n");
    }
}


/**
 * @brief Effectue une rotation à gauche de 90°.     
 * 
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale de rotation.
 */
void moveLeft(SafetyController* ctrl, float max_speed, bool relative) 
{
    if (ctrl->emergencyStopFlag == true) 
    {
        X_LOG_TRACE("[SafetyController] Rotation gauche bloquée : arrêt d'urgence actif.\n");
        return;
    }
    pilot_turn(3.14159f / 2, max_speed, relative);
    X_LOG_TRACE("[SafetyController] Rotation gauche 90°.\n");
}


/**
 * @brief Effectue une rotation à droite de 90°.
 * 
 * @param ctrl Pointeur vers la structure SafetyController.
 * @param max_speed Vitesse maximale de rotation.
 */
void moveRight(SafetyController* ctrl, float max_speed, bool relative) 
{
    if (ctrl->emergencyStopFlag == true) 
    {
        X_LOG_TRACE("[SafetyController] Rotation droite bloquée : arrêt d'urgence actif.\n");
        return;
    }
    pilot_turn(-3.14159f / 2, max_speed, relative);
    X_LOG_TRACE("[SafetyController] Rotation droite 90°.\n");
}


/**
 * @brief Réinitialise l'arrêt d'urgence du SafetyController.
 *
 * Cette fonction désactive le flag d'arrêt d'urgence, permettant ainsi au robot de reprendre ses mouvements.
 * Un message de trace est généré pour indiquer la réinitialisation.
 *
 * @param ctrl Pointeur vers la structure SafetyController à réinitialiser.
 */
void resetEmergencyStop(SafetyController* ctrl)
{
    ctrl->emergencyStopFlag = false;
    X_LOG_TRACE("[SafetyController] Arrêt d'urgence réinitialisé.\n");
}



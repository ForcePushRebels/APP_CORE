////////////////////////////////////////////////////////////
// Module: PropulsionSystem
// Description: Interface unifiée pour le système de propulsion
// Architecture: PositionTracker (150Hz) + MotionController (50Hz)
// Date: 06/06/2025
////////////////////////////////////////////////////////////

#ifndef PROPULSION_SYSTEM_H
#define PROPULSION_SYSTEM_H

#include <stdint.h>
#include <stdbool.h>
#include "positionTracker.h"
#include "motionController.h"

/////////////////////////////////
/// @brief Configuration système complète
/////////////////////////////////
typedef struct {
    // Configuration du tracker de position
    position_tracker_config_t position_config;
    
    // Configuration du contrôleur de mouvement  
    motion_controller_config_t motion_config;
    
    // Callbacks globaux du système
    position_update_callback_t on_position_updated;
    motion_completed_callback_t on_motion_completed;
    motion_error_callback_t on_motion_error;
} propulsion_system_config_t;

/////////////////////////////////
/// @brief État global du système
/////////////////////////////////
typedef struct {
    bool initialized;
    bool position_tracker_running;
    bool motion_controller_running;
    motion_state_t current_motion_state;
} propulsion_system_status_t;

/////////////////////////////////
/// @brief API Unifiée Système de Propulsion
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Initialiser le système de propulsion complet
/// @param config Configuration complète (NULL = défaut)
/// @return 0 si succès, code d'erreur sinon
////////////////////////////////////////////////////////////
int32_t propulsion_system_init(const propulsion_system_config_t* config);

////////////////////////////////////////////////////////////
/// @brief Arrêter le système de propulsion complet
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_system_shutdown(void);

////////////////////////////////////////////////////////////
/// @brief Obtenir le statut du système
/// @param status Pointeur pour stocker le statut
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_system_get_status(propulsion_system_status_t* status);

/////////////////////////////////
/// @brief Interfaces de Position (délégation vers PositionTracker)
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Obtenir la position actuelle du robot
/// @param position Pointeur pour stocker la position
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_get_position(robot_position_t* position);

////////////////////////////////////////////////////////////
/// @brief Obtenir les vitesses actuelles du robot
/// @param velocities Pointeur pour stocker les vitesses
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_get_velocities(robot_velocities_t* velocities);

////////////////////////////////////////////////////////////
/// @brief Réinitialiser la position du robot
/// @param new_position Nouvelle position (NULL = origine)
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_reset_position(const robot_position_t* new_position);

////////////////////////////////////////////////////////////
/// @brief Obtenir les statistiques de position
/// @param stats Pointeur pour stocker les statistiques
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_get_stats(position_stats_t* stats);

////////////////////////////////////////////////////////////
/// @brief Calibrer la distance
/// @param expected_mm Distance attendue en mm
/// @param measured_mm Distance mesurée en mm
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_calibrate_distance(double expected_mm, double measured_mm);

////////////////////////////////////////////////////////////
/// @brief Calibrer l'angle
/// @param expected_rad Angle attendu en radians
/// @param measured_rad Angle mesuré en radians
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_calibrate_angle(double expected_rad, double measured_rad);

/////////////////////////////////
/// @brief Interfaces de Mouvement (délégation vers MotionController)
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Exécuter une commande de mouvement
/// @param command Commande à exécuter
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_execute_motion(const motion_command_t* command);

////////////////////////////////////////////////////////////
/// @brief Arrêter le mouvement en cours
/// @param emergency true = arrêt d'urgence, false = arrêt progressif
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_stop(bool emergency);

////////////////////////////////////////////////////////////
/// @brief Obtenir l'état du mouvement
/// @return État actuel du mouvement
////////////////////////////////////////////////////////////
motion_state_t propulsion_get_motion_state(void);

////////////////////////////////////////////////////////////
/// @brief Vérifier si le robot est en mouvement
/// @return true si en mouvement, false sinon
////////////////////////////////////////////////////////////
bool propulsion_is_moving(void);

////////////////////////////////////////////////////////////
/// @brief Obtenir le pourcentage de progression
/// @return Pourcentage (0-100), -1 si pas de mouvement
////////////////////////////////////////////////////////////
int32_t propulsion_get_progress(void);

/////////////////////////////////
/// @brief Fonctions de Convenance Simplifiées
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Avancer en ligne droite
/// @param distance_mm Distance en mm (+ = avant, - = arrière)
/// @param speed_mm_s Vitesse en mm/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_move_straight(int32_t distance_mm, uint32_t speed_mm_s);

////////////////////////////////////////////////////////////
/// @brief Tourner d'un angle relatif
/// @param angle_rad Angle en radians (+ = horaire, - = anti-horaire)
/// @param speed_rad_s Vitesse angulaire en rad/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_turn_relative(double angle_rad, double speed_rad_s);

////////////////////////////////////////////////////////////
/// @brief Tourner vers un angle absolu
/// @param angle_rad Angle absolu en radians
/// @param speed_rad_s Vitesse angulaire en rad/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_turn_absolute(double angle_rad, double speed_rad_s);

////////////////////////////////////////////////////////////
/// @brief Aller à une position donnée
/// @param x_mm Position X en mm
/// @param y_mm Position Y en mm
/// @param speed_mm_s Vitesse en mm/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_goto(int32_t x_mm, int32_t y_mm, uint32_t speed_mm_s);

////////////////////////////////////////////////////////////
/// @brief Mouvement continu
/// @param speed_mm_s Vitesse en mm/s (+ = avant, - = arrière)
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t propulsion_move_continuous(int32_t speed_mm_s);

/////////////////////////////////
/// @brief Codes d'erreur
/////////////////////////////////
#define PROPULSION_SYSTEM_OK               0
#define PROPULSION_SYSTEM_ERROR_INIT      -1
#define PROPULSION_SYSTEM_ERROR_PARAM     -2
#define PROPULSION_SYSTEM_ERROR_HARDWARE  -3
#define PROPULSION_SYSTEM_ERROR_POSITION  -4
#define PROPULSION_SYSTEM_ERROR_MOTION    -5

/////////////////////////////////
/// @brief Configuration par défaut
/////////////////////////////////
extern const propulsion_system_config_t PROPULSION_SYSTEM_DEFAULT_CONFIG;

#endif // PROPULSION_SYSTEM_H 
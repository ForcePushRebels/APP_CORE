////////////////////////////////////////////////////////////
// Module: PositionTracker
// Description: Classe de suivi de position en temps réel (odométrie)
// Responsabilité: Thread haute fréquence pour calcul précis de position
// Date: 06/06/2025
////////////////////////////////////////////////////////////

#ifndef POSITION_TRACKER_H
#define POSITION_TRACKER_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "robotConfiguration.h"
#include "xError.h"

/////////////////////////////////
/// @brief Position du robot
/////////////////////////////////
typedef struct {
    int32_t x_mm;           // Position X en mm
    int32_t y_mm;           // Position Y en mm
    double theta_rad;       // Orientation en radians
    uint64_t timestamp_ms;  // Horodatage de la mesure
} robot_position_t;

/////////////////////////////////
/// @brief Vitesses du robot
/////////////////////////////////
typedef struct {
    double linear_mm_s;         // Vitesse linéaire en mm/s
    double angular_rad_s;       // Vitesse angulaire en rad/s
    double left_wheel_rad_s;    // Vitesse roue gauche en rad/s
    double right_wheel_rad_s;   // Vitesse roue droite en rad/s
    uint64_t timestamp_ms;      // Horodatage de la mesure
} robot_velocities_t;

/////////////////////////////////
/// @brief Statistiques de tracking
/////////////////////////////////
typedef struct {
    uint64_t total_updates;        // Nombre total de mises à jour
    double total_distance_mm;      // Distance totale parcourue
    double total_rotation_rad;     // Rotation totale en radians
    uint32_t update_frequency_hz;  // Fréquence de mise à jour réelle
    uint64_t last_update_time_us;  // Durée de la dernière mise à jour
} position_stats_t;

/////////////////////////////////
/// @brief Configuration du tracker
/////////////////////////////////
typedef struct {
    uint32_t update_frequency_hz;    // Fréquence de mise à jour (défaut: 150Hz)
    bool enable_velocity_filter;     // Activer filtrage des vitesses
    double velocity_filter_alpha;    // Coefficient filtre passe-bas (0.0-1.0)
    
    // Facteurs de correction de calibration
    double left_wheel_correction;    // Facteur correctif roue gauche
    double right_wheel_correction;   // Facteur correctif roue droite
    double distance_correction;      // Facteur correctif distance globale
    double angle_correction;         // Facteur correctif angle global
} position_tracker_config_t;

/////////////////////////////////
/// @brief Callback de notification de position
/////////////////////////////////
typedef void (*position_update_callback_t)(const robot_position_t* position, const robot_velocities_t* velocities);

/////////////////////////////////
/// @brief API Publique PositionTracker
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Initialiser le tracker de position
/// @param config Configuration (NULL = défaut)
/// @param callback Callback de notification (optionnel)
/// @return 0 si succès, code d'erreur sinon
////////////////////////////////////////////////////////////
int32_t position_tracker_init(const position_tracker_config_t* config, 
                             position_update_callback_t callback);

////////////////////////////////////////////////////////////
/// @brief Arrêter le tracker de position
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_shutdown(void);

////////////////////////////////////////////////////////////
/// @brief Obtenir la position actuelle (thread-safe)
/// @param position Pointeur pour stocker la position
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_get_position(robot_position_t* position);

////////////////////////////////////////////////////////////
/// @brief Obtenir les vitesses actuelles (thread-safe)
/// @param velocities Pointeur pour stocker les vitesses
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_get_velocities(robot_velocities_t* velocities);

////////////////////////////////////////////////////////////
/// @brief Réinitialiser la position à l'origine ou valeur donnée
/// @param new_position Nouvelle position (NULL = origine)
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_reset_position(const robot_position_t* new_position);

////////////////////////////////////////////////////////////
/// @brief Obtenir les statistiques de tracking
/// @param stats Pointeur pour stocker les statistiques
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_get_stats(position_stats_t* stats);

////////////////////////////////////////////////////////////
/// @brief Réinitialiser les statistiques
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_reset_stats(void);

////////////////////////////////////////////////////////////
/// @brief Calibrer la correction de distance
/// @param expected_distance_mm Distance attendue en mm
/// @param measured_distance_mm Distance mesurée en mm
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_calibrate_distance(double expected_distance_mm, double measured_distance_mm);

////////////////////////////////////////////////////////////
/// @brief Calibrer la correction d'angle
/// @param expected_angle_rad Angle attendu en radians
/// @param measured_angle_rad Angle mesuré en radians
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t position_tracker_calibrate_angle(double expected_angle_rad, double measured_angle_rad);

////////////////////////////////////////////////////////////
/// @brief Vérifier si le tracker est actif
/// @return true si actif, false sinon
////////////////////////////////////////////////////////////
bool position_tracker_is_running(void);

////////////////////////////////////////////////////////////
/// @brief Obtenir la fréquence de mise à jour actuelle
/// @return Fréquence en Hz
////////////////////////////////////////////////////////////
uint32_t position_tracker_get_frequency(void);

/////////////////////////////////
/// @brief Fonctions utilitaires
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Convertir une distance en ticks d'encodeur
/// @param distance_mm Distance en mm
/// @return Nombre de ticks
////////////////////////////////////////////////////////////
double position_tracker_distance_to_ticks(double distance_mm);

////////////////////////////////////////////////////////////
/// @brief Convertir des ticks en distance
/// @param ticks Nombre de ticks
/// @return Distance en mm
////////////////////////////////////////////////////////////
double position_tracker_ticks_to_distance(int32_t ticks);

/////////////////////////////////
/// @brief Codes d'erreur
/////////////////////////////////
#define POSITION_TRACKER_OK                 0
#define POSITION_TRACKER_ERROR_INIT        -1
#define POSITION_TRACKER_ERROR_PARAM       -2
#define POSITION_TRACKER_ERROR_HARDWARE    -3
#define POSITION_TRACKER_ERROR_THREAD      -4
#define POSITION_TRACKER_ERROR_NOT_RUNNING -5

/////////////////////////////////
/// @brief Configuration par défaut
/////////////////////////////////
extern const position_tracker_config_t POSITION_TRACKER_DEFAULT_CONFIG;

#endif // POSITION_TRACKER_H 
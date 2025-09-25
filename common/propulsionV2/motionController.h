////////////////////////////////////////////////////////////
// Module: MotionController
// Description: Classe de contrôle de mouvement optimisée
// Responsabilité: Thread moyenne fréquence pour contrôle moteurs + trajectoires
// Date: 06/06/2025
////////////////////////////////////////////////////////////

#ifndef MOTION_CONTROLLER_H
#define MOTION_CONTROLLER_H

#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "positionTracker.h"  // Utilise les types de position
#include "xError.h"

/////////////////////////////////
/// @brief Types de mouvements
/////////////////////////////////
typedef enum {
    MOTION_TYPE_STRAIGHT = 0,   // Déplacement en ligne droite
    MOTION_TYPE_TURN,           // Rotation sur place
    MOTION_TYPE_GOTO,           // Aller à une position donnée
    MOTION_TYPE_CONTINUOUS,     // Mouvement continu
    MOTION_TYPE_STOP            // Arrêt
} motion_type_t;

/////////////////////////////////
/// @brief Commande de mouvement
/////////////////////////////////
typedef struct {
    motion_type_t type;
    
    union {
        // Déplacement droit
        struct {
            int32_t distance_mm;        // Distance en mm (+ = avant, - = arrière)
            uint32_t max_speed_mm_s;    // Vitesse max en mm/s
        } straight;
        
        // Rotation
        struct {
            double angle_rad;           // Angle en radians (+ = horaire, - = anti-horaire)
            double max_speed_rad_s;     // Vitesse angulaire max en rad/s
            bool relative;              // true = relatif, false = absolu
        } turn;
        
        // Aller à une position
        struct {
            int32_t target_x_mm;        // Position X cible en mm
            int32_t target_y_mm;        // Position Y cible en mm
            uint32_t max_speed_mm_s;    // Vitesse max en mm/s
        } goto_pos;
        
        // Mouvement continu
        struct {
            int32_t speed_mm_s;         // Vitesse continue en mm/s (+ = avant, - = arrière)
        } continuous;
    };
} motion_command_t;

/////////////////////////////////
/// @brief États du contrôleur de mouvement
/////////////////////////////////
typedef enum {
    MOTION_STATE_IDLE = 0,      // Inactif, en attente de commande
    MOTION_STATE_MOVING,        // En mouvement
    MOTION_STATE_TURNING,       // En rotation
    MOTION_STATE_STOPPING,      // En cours d'arrêt
    MOTION_STATE_ERROR          // Erreur
} motion_state_t;

/////////////////////////////////
/// @brief Configuration du contrôleur
/////////////////////////////////
typedef struct {
    uint32_t control_frequency_hz;      // Fréquence de contrôle (défaut: 50Hz)
    uint32_t max_linear_speed_mm_s;     // Vitesse linéaire max en mm/s
    double max_angular_speed_rad_s;     // Vitesse angulaire max en rad/s
    uint32_t max_acceleration_mm_s2;    // Accélération max en mm/s²
    double max_angular_accel_rad_s2;    // Accélération angulaire max en rad/s²
    
    // Tolérances de précision
    double position_tolerance_mm;       // Tolérance de position en mm
    double angle_tolerance_rad;         // Tolérance d'angle en radians
    
    // PID linéaire (optionnel)
    double linear_kp, linear_ki, linear_kd;
    
    // PID angulaire (optionnel)  
    double angular_kp, angular_ki, angular_kd;
} motion_controller_config_t;

/////////////////////////////////
/// @brief Callbacks de notification
/////////////////////////////////
typedef void (*motion_completed_callback_t)(void);
typedef void (*motion_error_callback_t)(int32_t error_code);

/////////////////////////////////
/// @brief API Publique MotionController
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Initialiser le contrôleur de mouvement
/// @param config Configuration (NULL = défaut)
/// @param completed_callback Callback de fin de mouvement (optionnel)
/// @param error_callback Callback d'erreur (optionnel)
/// @return 0 si succès, code d'erreur sinon
////////////////////////////////////////////////////////////
int32_t motion_controller_init(const motion_controller_config_t* config,
                              motion_completed_callback_t completed_callback,
                              motion_error_callback_t error_callback);

////////////////////////////////////////////////////////////
/// @brief Arrêter le contrôleur de mouvement
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_shutdown(void);

////////////////////////////////////////////////////////////
/// @brief Exécuter une commande de mouvement
/// @param command Commande à exécuter
/// @return 0 si succès, code d'erreur sinon
////////////////////////////////////////////////////////////
int32_t motion_controller_execute(const motion_command_t* command);

////////////////////////////////////////////////////////////
/// @brief Arrêter le mouvement en cours
/// @param emergency true = arrêt d'urgence immédiat, false = arrêt progressif
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_stop(bool emergency);

////////////////////////////////////////////////////////////
/// @brief Obtenir l'état actuel du contrôleur
/// @return État actuel
////////////////////////////////////////////////////////////
motion_state_t motion_controller_get_state(void);

////////////////////////////////////////////////////////////
/// @brief Vérifier si le robot est en mouvement
/// @return true si en mouvement, false sinon
////////////////////////////////////////////////////////////
bool motion_controller_is_moving(void);

////////////////////////////////////////////////////////////
/// @brief Obtenir les vitesses cibles actuelles
/// @param linear_speed Pointeur pour vitesse linéaire (mm/s)
/// @param angular_speed Pointeur pour vitesse angulaire (rad/s)
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_get_target_speeds(double* linear_speed, double* angular_speed);

////////////////////////////////////////////////////////////
/// @brief Obtenir le pourcentage de progression du mouvement actuel
/// @return Pourcentage (0-100), -1 si pas de mouvement
////////////////////////////////////////////////////////////
int32_t motion_controller_get_progress_percent(void);

/////////////////////////////////
/// @brief Fonctions de convenance (simplifiées)
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Avancer d'une distance donnée
/// @param distance_mm Distance en mm (+ = avant, - = arrière)
/// @param speed_mm_s Vitesse en mm/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_move_straight(int32_t distance_mm, uint32_t speed_mm_s);

////////////////////////////////////////////////////////////
/// @brief Tourner d'un angle donné (relatif)
/// @param angle_rad Angle en radians (+ = horaire, - = anti-horaire)
/// @param speed_rad_s Vitesse angulaire en rad/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_turn_relative(double angle_rad, double speed_rad_s);

////////////////////////////////////////////////////////////
/// @brief Tourner vers un angle absolu
/// @param angle_rad Angle absolu en radians
/// @param speed_rad_s Vitesse angulaire en rad/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_turn_absolute(double angle_rad, double speed_rad_s);

////////////////////////////////////////////////////////////
/// @brief Aller à une position donnée
/// @param x_mm Position X en mm
/// @param y_mm Position Y en mm
/// @param speed_mm_s Vitesse en mm/s
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_goto(int32_t x_mm, int32_t y_mm, uint32_t speed_mm_s);

////////////////////////////////////////////////////////////
/// @brief Mouvement continu
/// @param speed_mm_s Vitesse continue en mm/s (+ = avant, - = arrière)
/// @return 0 si succès
////////////////////////////////////////////////////////////
int32_t motion_controller_move_continuous(int32_t speed_mm_s);

/////////////////////////////////
/// @brief Codes d'erreur
/////////////////////////////////
#define MOTION_CONTROLLER_OK                0
#define MOTION_CONTROLLER_ERROR_INIT      -1
#define MOTION_CONTROLLER_ERROR_PARAM     -2
#define MOTION_CONTROLLER_ERROR_HARDWARE  -3
#define MOTION_CONTROLLER_ERROR_THREAD    -4
#define MOTION_CONTROLLER_ERROR_BUSY      -5
#define MOTION_CONTROLLER_ERROR_POSITION  -6

/////////////////////////////////
/// @brief Configuration par défaut
/////////////////////////////////
extern const motion_controller_config_t MOTION_CONTROLLER_DEFAULT_CONFIG;

#endif // MOTION_CONTROLLER_H 
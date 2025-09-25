////////////////////////////////////////////////////////////
// Module: PropulsionSystem
// Description: Implémentation de l'interface unifiée de propulsion
// Gestion des 2 classes: PositionTracker (150Hz) + MotionController (50Hz)
// Date: 06/06/2025
////////////////////////////////////////////////////////////

#include "propulsionSystem.h"
#include "positionTracker.h"
#include "motionController.h"
#include "xLog.h"
#include <string.h>

/////////////////////////////////
/// @brief Structure interne du système de propulsion
/////////////////////////////////
typedef struct {
    bool initialized;
    propulsion_system_config_t config;
    
    // État des sous-systèmes
    bool position_tracker_running;
    bool motion_controller_running;
    
    // Callbacks utilisateur
    position_update_callback_t user_position_callback;
    motion_completed_callback_t user_motion_completed_callback;
    motion_error_callback_t user_motion_error_callback;
    
} propulsion_system_t;

/////////////////////////////////
/// @brief Instance globale du système
/////////////////////////////////
static propulsion_system_t g_system = {0};

/////////////////////////////////
/// @brief Configuration par défaut
/////////////////////////////////
const propulsion_system_config_t PROPULSION_SYSTEM_DEFAULT_CONFIG = {
    .position_config = {
        .update_frequency_hz = 150,         // 150Hz pour position
        .enable_velocity_filter = true,
        .velocity_filter_alpha = 0.3,
        .left_wheel_correction = 1.0,
        .right_wheel_correction = 1.0,
        .distance_correction = 1.0,
        .angle_correction = 1.0
    },
    .motion_config = {
        .control_frequency_hz = 50,         // 50Hz pour contrôle
        .max_linear_speed_mm_s = 160,       // 16 cm/s
        .max_angular_speed_rad_s = 2.0,     // ~115 deg/s
        .max_acceleration_mm_s2 = 200,      // 20 cm/s²
        .max_angular_accel_rad_s2 = 3.0,    // ~172 deg/s²
        .position_tolerance_mm = 5.0,       // 5mm de tolérance
        .angle_tolerance_rad = 0.05,        // ~3 degrés
        .linear_kp = 1.0, .linear_ki = 0.0, .linear_kd = 0.0,
        .angular_kp = 1.0, .angular_ki = 0.0, .angular_kd = 0.0
    },
    .on_position_updated = NULL,
    .on_motion_completed = NULL,
    .on_motion_error = NULL
};

/////////////////////////////////
/// @brief Callbacks internes pour liaison avec les sous-systèmes
/////////////////////////////////

////////////////////////////////////////////////////////////
/// @brief Callback interne pour position - transmet à l'utilisateur
////////////////////////////////////////////////////////////
static void internal_position_callback(const robot_position_t* position, const robot_velocities_t* velocities) 
{
    // Transmettre à l'utilisateur si callback défini
    if (g_system.user_position_callback) {
        g_system.user_position_callback(position, velocities);
    }
}

////////////////////////////////////////////////////////////
/// @brief Callback interne pour fin de mouvement - transmet à l'utilisateur
////////////////////////////////////////////////////////////
static void internal_motion_completed_callback(void) 
{
    // Transmettre à l'utilisateur si callback défini
    if (g_system.user_motion_completed_callback) {
        g_system.user_motion_completed_callback();
    }
}

////////////////////////////////////////////////////////////
/// @brief Callback interne pour erreur de mouvement - transmet à l'utilisateur
////////////////////////////////////////////////////////////
static void internal_motion_error_callback(int32_t error_code) 
{
    // Transmettre à l'utilisateur si callback défini
    if (g_system.user_motion_error_callback) {
        g_system.user_motion_error_callback(error_code);
    }
}

/////////////////////////////////
/// @brief API Publique - Interface Système
/////////////////////////////////

int32_t propulsion_system_init(const propulsion_system_config_t* config) 
{
    if (g_system.initialized) {
        return PROPULSION_SYSTEM_OK;
    }
    
    // Configuration
    if (config) {
        g_system.config = *config;
    } else {
        g_system.config = PROPULSION_SYSTEM_DEFAULT_CONFIG;
    }
    
    // Sauvegarder les callbacks utilisateur
    g_system.user_position_callback = g_system.config.on_position_updated;
    g_system.user_motion_completed_callback = g_system.config.on_motion_completed;
    g_system.user_motion_error_callback = g_system.config.on_motion_error;
    
    X_LOG_TRACE("=== PROPULSION SYSTEM V2 INITIALIZATION ===");
    
    // 1. Initialisation du PositionTracker (thread haute fréquence)
    int32_t result = position_tracker_init(&g_system.config.position_config, internal_position_callback);
    if (result != POSITION_TRACKER_OK) {
        X_LOG_TRACE("Position tracker initialization failed: %d", result);
        return PROPULSION_SYSTEM_ERROR_POSITION;
    }
    g_system.position_tracker_running = true;
    X_LOG_TRACE("Position tracker initialized successfully at %dHz", 
               g_system.config.position_config.update_frequency_hz);
    
    // 2. Initialisation du MotionController (thread moyenne fréquence)
    result = motion_controller_init(&g_system.config.motion_config, 
                                   internal_motion_completed_callback,
                                   internal_motion_error_callback);
    if (result != MOTION_CONTROLLER_OK) {
        X_LOG_TRACE("Motion controller initialization failed: %d", result);
        // Cleanup du position tracker en cas d'échec
        position_tracker_shutdown();
        g_system.position_tracker_running = false;
        return PROPULSION_SYSTEM_ERROR_MOTION;
    }
    g_system.motion_controller_running = true;
    X_LOG_TRACE("Motion controller initialized successfully at %dHz", 
               g_system.config.motion_config.control_frequency_hz);
    
    g_system.initialized = true;
    
    X_LOG_TRACE("=== PROPULSION SYSTEM V2 READY ===");
    X_LOG_TRACE("Architecture: 2 threads optimized");
    X_LOG_TRACE("- Position tracking: %dHz (high frequency)", 
               g_system.config.position_config.update_frequency_hz);
    X_LOG_TRACE("- Motion control: %dHz (medium frequency)", 
               g_system.config.motion_config.control_frequency_hz);
    
    return PROPULSION_SYSTEM_OK;
}

int32_t propulsion_system_shutdown(void) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_OK;
    }
    
    X_LOG_TRACE("=== PROPULSION SYSTEM V2 SHUTDOWN ===");
    
    // Arrêt du MotionController en premier
    if (g_system.motion_controller_running) {
        motion_controller_shutdown();
        g_system.motion_controller_running = false;
        X_LOG_TRACE("Motion controller stopped");
    }
    
    // Arrêt du PositionTracker
    if (g_system.position_tracker_running) {
        position_tracker_shutdown();
        g_system.position_tracker_running = false;
        X_LOG_TRACE("Position tracker stopped");
    }
    
    g_system.initialized = false;
    memset(&g_system, 0, sizeof(propulsion_system_t));
    
    X_LOG_TRACE("Propulsion system V2 shutdown complete");
    return PROPULSION_SYSTEM_OK;
}

int32_t propulsion_system_get_status(propulsion_system_status_t* status) 
{
    if (!status) {
        return PROPULSION_SYSTEM_ERROR_PARAM;
    }
    
    status->initialized = g_system.initialized;
    status->position_tracker_running = g_system.position_tracker_running && position_tracker_is_running();
    status->motion_controller_running = g_system.motion_controller_running;
    status->current_motion_state = motion_controller_get_state();
    
    return PROPULSION_SYSTEM_OK;
}

/////////////////////////////////
/// @brief API - Interfaces de Position (délégation vers PositionTracker)
/////////////////////////////////

int32_t propulsion_get_position(robot_position_t* position) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = position_tracker_get_position(position);
    return (result == POSITION_TRACKER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_POSITION;
}

int32_t propulsion_get_velocities(robot_velocities_t* velocities) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = position_tracker_get_velocities(velocities);
    return (result == POSITION_TRACKER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_POSITION;
}

int32_t propulsion_reset_position(const robot_position_t* new_position) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = position_tracker_reset_position(new_position);
    return (result == POSITION_TRACKER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_POSITION;
}

int32_t propulsion_get_stats(position_stats_t* stats) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = position_tracker_get_stats(stats);
    return (result == POSITION_TRACKER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_POSITION;
}

int32_t propulsion_calibrate_distance(double expected_mm, double measured_mm) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = position_tracker_calibrate_distance(expected_mm, measured_mm);
    return (result == POSITION_TRACKER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_POSITION;
}

int32_t propulsion_calibrate_angle(double expected_rad, double measured_rad) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = position_tracker_calibrate_angle(expected_rad, measured_rad);
    return (result == POSITION_TRACKER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_POSITION;
}

/////////////////////////////////
/// @brief API - Interfaces de Mouvement (délégation vers MotionController)
/////////////////////////////////

int32_t propulsion_execute_motion(const motion_command_t* command) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_execute(command);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
}

int32_t propulsion_stop(bool emergency) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_stop(emergency);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
}

motion_state_t propulsion_get_motion_state(void) 
{
    if (!g_system.initialized) {
        return MOTION_STATE_ERROR;
    }
    
    return motion_controller_get_state();
}

bool propulsion_is_moving(void) 
{
    if (!g_system.initialized) {
        return false;
    }
    
    return motion_controller_is_moving();
}

int32_t propulsion_get_progress(void) 
{
    if (!g_system.initialized) {
        return -1;
    }
    
    return motion_controller_get_progress_percent();
}

/////////////////////////////////
/// @brief API - Fonctions de Convenance Simplifiées
/////////////////////////////////

int32_t propulsion_move_straight(int32_t distance_mm, uint32_t speed_mm_s) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_move_straight(distance_mm, speed_mm_s);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
}

int32_t propulsion_turn_relative(double angle_rad, double speed_rad_s) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_turn_relative(angle_rad, speed_rad_s);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
}

int32_t propulsion_turn_absolute(double angle_rad, double speed_rad_s) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_turn_absolute(angle_rad, speed_rad_s);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
}

int32_t propulsion_goto(int32_t x_mm, int32_t y_mm, uint32_t speed_mm_s) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_goto(x_mm, y_mm, speed_mm_s);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
}

int32_t propulsion_move_continuous(int32_t speed_mm_s) 
{
    if (!g_system.initialized) {
        return PROPULSION_SYSTEM_ERROR_INIT;
    }
    
    int32_t result = motion_controller_move_continuous(speed_mm_s);
    return (result == MOTION_CONTROLLER_OK) ? PROPULSION_SYSTEM_OK : PROPULSION_SYSTEM_ERROR_MOTION;
} 
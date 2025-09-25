////////////////////////////////////////////////////////////
// Module: PositionTracker
// Description: Implémentation du suivi de position temps réel
// Thread haute fréquence (150Hz) pour calcul précis d'odométrie
// Date: 06/06/2025
////////////////////////////////////////////////////////////

#include "positionTracker.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xLog.h"
#include <string.h>
#include <math.h>

/////////////////////////////////
/// @brief Structure interne du PositionTracker
/////////////////////////////////
typedef struct {
    // Configuration
    position_tracker_config_t config;
    bool initialized;
    bool running;
    
    // Position et vitesses actuelles
    robot_position_t position;
    robot_velocities_t velocities;
    
    // État des encodeurs
    int32_t encoder_left_last;
    int32_t encoder_right_last;
    uint64_t last_update_ms;
    
    // Filtrage des vitesses
    double filtered_linear_speed;
    double filtered_angular_speed;
    double filtered_left_wheel_speed;
    double filtered_right_wheel_speed;
    
    // Statistiques
    position_stats_t stats;
    uint64_t last_stats_update_ms;
    
    // Thread et synchronisation
    xOsTaskCtx position_task;
    xOsMutexCtx mutex;
    
    // Callback de notification
    position_update_callback_t callback;
    
} position_tracker_t;

/////////////////////////////////
/// @brief Instance globale
/////////////////////////////////
static position_tracker_t g_tracker = {0};

/////////////////////////////////
/// @brief Configuration par défaut
/////////////////////////////////
const position_tracker_config_t POSITION_TRACKER_DEFAULT_CONFIG = {
    .update_frequency_hz = 150,         // 150Hz pour haute précision
    .enable_velocity_filter = true,
    .velocity_filter_alpha = 0.3,       // Filtre passe-bas modéré
    .left_wheel_correction = 1.0,       // Pas de correction par défaut
    .right_wheel_correction = 1.0,
    .distance_correction = 1.0,
    .angle_correction = 1.0
};

/////////////////////////////////
/// @brief Prototypes internes
/////////////////////////////////
static void* position_tracking_task(void* arg);
static void update_position_and_velocities(void);
static void apply_velocity_filter(double linear, double angular, double left_wheel, double right_wheel);
static void update_statistics(double distance_delta, double angle_delta);
static double normalize_angle(double angle);

/////////////////////////////////
/// @brief Fonctions utilitaires
/////////////////////////////////

double position_tracker_distance_to_ticks(double distance_mm) 
{
    return distance_mm / (WHEEL_PERIMETER_CM * 10.0) * ENCODER_TICKS_REV;
}

double position_tracker_ticks_to_distance(int32_t ticks) 
{
    return (double)ticks * (WHEEL_PERIMETER_CM * 10.0) / ENCODER_TICKS_REV;
}

/////////////////////////////////
/// @brief Thread de tracking de position haute fréquence
/////////////////////////////////
static void* position_tracking_task(void* arg) 
{
    (void)arg;
    
    const uint32_t period_ms = 1000 / g_tracker.config.update_frequency_hz;
    uint64_t next_wake_time = xTimerGetCurrentMs() + period_ms;
    
    X_LOG_TRACE("Position tracking task started at %dHz", g_tracker.config.update_frequency_hz);
    
    // Initialisation des encodeurs
    int32_t encoders[2];
    GetMotorEncoderValues(encoders);
    g_tracker.encoder_left_last = encoders[0];
    g_tracker.encoder_right_last = encoders[1];
    g_tracker.last_update_ms = xTimerGetCurrentMs();
    
    while (g_tracker.running) {
        uint64_t update_start_time = xTimerGetCurrentMs();
        
        mutexLock(&g_tracker.mutex);
        
        // Mise à jour principale de position et vitesses
        update_position_and_velocities();
        
        // Notification par callback si défini
        if (g_tracker.callback) {
            g_tracker.callback(&g_tracker.position, &g_tracker.velocities);
        }
        
        mutexUnlock(&g_tracker.mutex);
        
        // Mesure du temps d'exécution pour les statistiques
        uint64_t update_end_time = xTimerGetCurrentMs();
        g_tracker.stats.last_update_time_us = (update_end_time - update_start_time) * 1000;
        
        // Attente temps réel pour respecter la fréquence
        next_wake_time += period_ms;
        uint64_t current_time = xTimerGetCurrentMs();
        if (next_wake_time > current_time) {
            xTimerDelay(next_wake_time - current_time);
        } else {
            // Dépassement de timing - warning
            X_LOG_TRACE("Position tracking timing overrun! Duration: %llu us", 
                       g_tracker.stats.last_update_time_us);
            next_wake_time = current_time;
        }
    }
    
    X_LOG_TRACE("Position tracking task stopped");
    return (void*)OS_TASK_EXIT_SUCCESS;
}

/////////////////////////////////
/// @brief Mise à jour principale de position et vitesses
/////////////////////////////////
static void update_position_and_velocities(void) 
{
    int32_t encoders[2];
    GetMotorEncoderValues(encoders);
    
    uint64_t current_ms = xTimerGetCurrentMs();
    double dt = (current_ms - g_tracker.last_update_ms) / 1000.0;
    
    if (dt > 0.0 && g_tracker.last_update_ms > 0) {
        // Calcul des variations avec corrections de calibration
        int32_t delta_left = encoders[0] - g_tracker.encoder_left_last;
        int32_t delta_right = encoders[1] - g_tracker.encoder_right_last;
        
        // Application des facteurs de correction des roues
        double corrected_left = delta_left * g_tracker.config.left_wheel_correction;
        double corrected_right = delta_right * g_tracker.config.right_wheel_correction;
        
        // Distances parcourues par chaque roue (en mm)
        double left_distance_mm = position_tracker_ticks_to_distance((int32_t)corrected_left);
        double right_distance_mm = position_tracker_ticks_to_distance((int32_t)corrected_right);
        
        // Distance et angle du robot
        double center_distance_mm = (left_distance_mm + right_distance_mm) / 2.0;
        double wheel_distance_mm = WHEEL_DISTANCE_CM * 10.0; // conversion en mm
        double delta_angle_rad = (right_distance_mm - left_distance_mm) / wheel_distance_mm;
        
        // Application des facteurs de calibration globaux
        center_distance_mm *= g_tracker.config.distance_correction;
        delta_angle_rad *= g_tracker.config.angle_correction;
        
        // Mise à jour de la position avec méthode de l'angle moyen (plus précise)
        double cos_theta = cos(g_tracker.position.theta_rad + delta_angle_rad / 2.0);
        double sin_theta = sin(g_tracker.position.theta_rad + delta_angle_rad / 2.0);
        
        g_tracker.position.x_mm += (int32_t)(center_distance_mm * cos_theta);
        g_tracker.position.y_mm += (int32_t)(center_distance_mm * sin_theta);
        g_tracker.position.theta_rad += delta_angle_rad;
        g_tracker.position.theta_rad = normalize_angle(g_tracker.position.theta_rad);
        g_tracker.position.timestamp_ms = current_ms;
        
        // Calcul des vitesses instantanées
        double linear_speed = center_distance_mm / dt;           // mm/s
        double angular_speed = delta_angle_rad / dt;             // rad/s
        double wheel_radius_mm = (WHEEL_DIAMETER_CM * 10.0) / 2.0; // mm
        double left_wheel_speed = left_distance_mm / dt / wheel_radius_mm;   // rad/s
        double right_wheel_speed = right_distance_mm / dt / wheel_radius_mm; // rad/s
        
        // Application du filtrage si activé
        if (g_tracker.config.enable_velocity_filter) {
            apply_velocity_filter(linear_speed, angular_speed, left_wheel_speed, right_wheel_speed);
            g_tracker.velocities.linear_mm_s = g_tracker.filtered_linear_speed;
            g_tracker.velocities.angular_rad_s = g_tracker.filtered_angular_speed;
            g_tracker.velocities.left_wheel_rad_s = g_tracker.filtered_left_wheel_speed;
            g_tracker.velocities.right_wheel_rad_s = g_tracker.filtered_right_wheel_speed;
        } else {
            g_tracker.velocities.linear_mm_s = linear_speed;
            g_tracker.velocities.angular_rad_s = angular_speed;
            g_tracker.velocities.left_wheel_rad_s = left_wheel_speed;
            g_tracker.velocities.right_wheel_rad_s = right_wheel_speed;
        }
        g_tracker.velocities.timestamp_ms = current_ms;
        
        // Mise à jour des statistiques
        update_statistics(fabs(center_distance_mm), fabs(delta_angle_rad));
    }
    
    // Sauvegarde des valeurs pour la prochaine itération
    g_tracker.encoder_left_last = encoders[0];
    g_tracker.encoder_right_last = encoders[1];
    g_tracker.last_update_ms = current_ms;
}

/////////////////////////////////
/// @brief Application du filtre passe-bas sur les vitesses
/////////////////////////////////
static void apply_velocity_filter(double linear, double angular, double left_wheel, double right_wheel) 
{
    double alpha = g_tracker.config.velocity_filter_alpha;
    
    g_tracker.filtered_linear_speed = alpha * linear + (1.0 - alpha) * g_tracker.filtered_linear_speed;
    g_tracker.filtered_angular_speed = alpha * angular + (1.0 - alpha) * g_tracker.filtered_angular_speed;
    g_tracker.filtered_left_wheel_speed = alpha * left_wheel + (1.0 - alpha) * g_tracker.filtered_left_wheel_speed;
    g_tracker.filtered_right_wheel_speed = alpha * right_wheel + (1.0 - alpha) * g_tracker.filtered_right_wheel_speed;
}

/////////////////////////////////
/// @brief Mise à jour des statistiques
/////////////////////////////////
static void update_statistics(double distance_delta, double angle_delta) 
{
    g_tracker.stats.total_updates++;
    g_tracker.stats.total_distance_mm += distance_delta;
    g_tracker.stats.total_rotation_rad += angle_delta;
    
    // Calcul de la fréquence réelle toutes les secondes
    uint64_t current_ms = xTimerGetCurrentMs();
    if (current_ms - g_tracker.last_stats_update_ms >= 1000) {
        // Estimation de la fréquence basée sur le nombre de mises à jour
        static uint64_t last_update_count = 0;
        uint64_t updates_this_second = g_tracker.stats.total_updates - last_update_count;
        g_tracker.stats.update_frequency_hz = (uint32_t)updates_this_second;
        last_update_count = g_tracker.stats.total_updates;
        g_tracker.last_stats_update_ms = current_ms;
    }
}

/////////////////////////////////
/// @brief Normalisation d'angle entre -PI et PI
/////////////////////////////////
static double normalize_angle(double angle) 
{
    while (angle > M_PI) angle -= 2.0 * M_PI;
    while (angle < -M_PI) angle += 2.0 * M_PI;
    return angle;
}

/////////////////////////////////
/// @brief API Publique
/////////////////////////////////

int32_t position_tracker_init(const position_tracker_config_t* config, 
                             position_update_callback_t callback) 
{
    if (g_tracker.initialized) {
        return POSITION_TRACKER_ERROR_INIT;
    }
    
    // Configuration
    if (config) {
        g_tracker.config = *config;
    } else {
        g_tracker.config = POSITION_TRACKER_DEFAULT_CONFIG;
    }
    
    // Validation de la configuration
    if (g_tracker.config.update_frequency_hz < 10 || g_tracker.config.update_frequency_hz > 1000) {
        return POSITION_TRACKER_ERROR_PARAM;
    }
    
    // Initialisation des mutexes
    if (mutexCreate(&g_tracker.mutex) != MUTEX_OK) {
        return POSITION_TRACKER_ERROR_HARDWARE;
    }
    
    // État initial
    memset(&g_tracker.position, 0, sizeof(robot_position_t));
    memset(&g_tracker.velocities, 0, sizeof(robot_velocities_t));
    memset(&g_tracker.stats, 0, sizeof(position_stats_t));
    
    // Initialisation du filtrage
    g_tracker.filtered_linear_speed = 0.0;
    g_tracker.filtered_angular_speed = 0.0;
    g_tracker.filtered_left_wheel_speed = 0.0;
    g_tracker.filtered_right_wheel_speed = 0.0;
    
    // Callback
    g_tracker.callback = callback;
    
    // Horodatage initial
    g_tracker.position.timestamp_ms = xTimerGetCurrentMs();
    g_tracker.velocities.timestamp_ms = g_tracker.position.timestamp_ms;
    g_tracker.last_stats_update_ms = g_tracker.position.timestamp_ms;
    
    // Création du thread de tracking
    osTaskInit(&g_tracker.position_task);
    g_tracker.position_task.t_ptTask = position_tracking_task;
    g_tracker.position_task.t_ptTaskArg = NULL;
    
    if (osTaskCreate(&g_tracker.position_task) != OS_TASK_SUCCESS) {
        mutexDestroy(&g_tracker.mutex);
        return POSITION_TRACKER_ERROR_THREAD;
    }
    
    g_tracker.running = true;
    g_tracker.initialized = true;
    
    X_LOG_TRACE("Position tracker initialized at %dHz", g_tracker.config.update_frequency_hz);
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_shutdown(void) 
{
    if (!g_tracker.initialized) {
        return POSITION_TRACKER_OK;
    }
    
    g_tracker.running = false;
    osTaskStop(&g_tracker.position_task, 2);
    osTaskWait(&g_tracker.position_task, NULL);
    mutexDestroy(&g_tracker.mutex);
    
    g_tracker.initialized = false;
    X_LOG_TRACE("Position tracker shutdown");
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_get_position(robot_position_t* position) 
{
    if (!g_tracker.initialized || !position) {
        return POSITION_TRACKER_ERROR_PARAM;
    }
    
    mutexLock(&g_tracker.mutex);
    *position = g_tracker.position;
    mutexUnlock(&g_tracker.mutex);
    
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_get_velocities(robot_velocities_t* velocities) 
{
    if (!g_tracker.initialized || !velocities) {
        return POSITION_TRACKER_ERROR_PARAM;
    }
    
    mutexLock(&g_tracker.mutex);
    *velocities = g_tracker.velocities;
    mutexUnlock(&g_tracker.mutex);
    
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_reset_position(const robot_position_t* new_position) 
{
    if (!g_tracker.initialized) {
        return POSITION_TRACKER_ERROR_NOT_RUNNING;
    }
    
    mutexLock(&g_tracker.mutex);
    
    if (new_position) {
        g_tracker.position = *new_position;
    } else {
        memset(&g_tracker.position, 0, sizeof(robot_position_t));
    }
    g_tracker.position.timestamp_ms = xTimerGetCurrentMs();
    
    mutexUnlock(&g_tracker.mutex);
    
    X_LOG_TRACE("Position tracker position reset");
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_get_stats(position_stats_t* stats) 
{
    if (!g_tracker.initialized || !stats) {
        return POSITION_TRACKER_ERROR_PARAM;
    }
    
    mutexLock(&g_tracker.mutex);
    *stats = g_tracker.stats;
    mutexUnlock(&g_tracker.mutex);
    
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_reset_stats(void) 
{
    if (!g_tracker.initialized) {
        return POSITION_TRACKER_ERROR_NOT_RUNNING;
    }
    
    mutexLock(&g_tracker.mutex);
    memset(&g_tracker.stats, 0, sizeof(position_stats_t));
    g_tracker.last_stats_update_ms = xTimerGetCurrentMs();
    mutexUnlock(&g_tracker.mutex);
    
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_calibrate_distance(double expected_distance_mm, double measured_distance_mm) 
{
    if (!g_tracker.initialized) {
        return POSITION_TRACKER_ERROR_NOT_RUNNING;
    }
    
    if (measured_distance_mm <= 0.0 || expected_distance_mm <= 0.0) {
        return POSITION_TRACKER_ERROR_PARAM;
    }
    
    mutexLock(&g_tracker.mutex);
    g_tracker.config.distance_correction = expected_distance_mm / measured_distance_mm;
    mutexUnlock(&g_tracker.mutex);
    
    X_LOG_TRACE("Position tracker distance calibrated: factor = %.4f", g_tracker.config.distance_correction);
    return POSITION_TRACKER_OK;
}

int32_t position_tracker_calibrate_angle(double expected_angle_rad, double measured_angle_rad) 
{
    if (!g_tracker.initialized) {
        return POSITION_TRACKER_ERROR_NOT_RUNNING;
    }
    
    if (fabs(measured_angle_rad) < 0.01 || fabs(expected_angle_rad) < 0.01) {
        return POSITION_TRACKER_ERROR_PARAM;
    }
    
    mutexLock(&g_tracker.mutex);
    g_tracker.config.angle_correction = expected_angle_rad / measured_angle_rad;
    mutexUnlock(&g_tracker.mutex);
    
    X_LOG_TRACE("Position tracker angle calibrated: factor = %.4f", g_tracker.config.angle_correction);
    return POSITION_TRACKER_OK;
}

bool position_tracker_is_running(void) 
{
    return g_tracker.initialized && g_tracker.running;
}

uint32_t position_tracker_get_frequency(void) 
{
    return g_tracker.stats.update_frequency_hz;
}

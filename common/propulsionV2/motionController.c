////////////////////////////////////////////////////////////
// Module: MotionController  
// Description: Implémentation du contrôle de mouvement
// Thread moyenne fréquence (50Hz) pour contrôle moteurs + trajectoires
// Date: 06/06/2025
////////////////////////////////////////////////////////////

#include "motionController.h"
#include "positionTracker.h"
#include "hardwareAbstraction.h"
#include "motorControl.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xLog.h"
#include <string.h>
#include <math.h>

/////////////////////////////////
/// @brief États internes du contrôleur
/////////////////////////////////
typedef enum {
    MOTION_INTERNAL_IDLE = 0,
    MOTION_INTERNAL_ACCELERATING,
    MOTION_INTERNAL_CRUISING,
    MOTION_INTERNAL_DECELERATING,
    MOTION_INTERNAL_POSITION_CONTROL
} motion_internal_state_t;

/////////////////////////////////
/// @brief Structure interne du MotionController
/////////////////////////////////
typedef struct {
    // Configuration
    motion_controller_config_t config;
    bool initialized;
    bool running;
    
    // État actuel
    motion_state_t state;
    motion_internal_state_t internal_state;
    
    // Commande courante
    motion_command_t current_command;
    bool has_active_command;
    
    // Contrôle de vitesse
    double target_linear_speed;     // mm/s
    double target_angular_speed;    // rad/s
    double current_linear_speed;    // mm/s  
    double current_angular_speed;   // rad/s
    
    // Objectifs de trajectoire
    robot_position_t start_position;
    int32_t target_distance_mm;
    double target_angle_rad;
    
    // PID Controllers (optionnel)
    struct {
        double linear_integral, linear_last_error;
        double angular_integral, angular_last_error;
    } pid_state;
    
    // Thread et synchronisation
    xOsTaskCtx control_task;
    xOsMutexCtx mutex;
    
    // Callbacks
    motion_completed_callback_t completed_callback;
    motion_error_callback_t error_callback;
    
} motion_controller_t;

/////////////////////////////////
/// @brief Instance globale
/////////////////////////////////
static motion_controller_t g_controller = {0};

/////////////////////////////////
/// @brief Configuration par défaut
/////////////////////////////////
const motion_controller_config_t MOTION_CONTROLLER_DEFAULT_CONFIG = {
    .control_frequency_hz = 50,         // 50Hz pour contrôle
    .max_linear_speed_mm_s = 160,       // 16 cm/s
    .max_angular_speed_rad_s = 2.0,     // ~115 deg/s
    .max_acceleration_mm_s2 = 200,      // 20 cm/s²
    .max_angular_accel_rad_s2 = 3.0,    // ~172 deg/s²
    .position_tolerance_mm = 5.0,       // 5mm de tolérance
    .angle_tolerance_rad = 0.05,        // ~3 degrés
    .linear_kp = 1.0, .linear_ki = 0.0, .linear_kd = 0.0,
    .angular_kp = 1.0, .angular_ki = 0.0, .angular_kd = 0.0
};

/////////////////////////////////
/// @brief Prototypes internes
/////////////////////////////////
static void* motion_control_task(void* arg);
static void update_motion_control(void);
static void calculate_wheel_speeds(double linear_speed, double angular_speed, 
                                  double* left_speed, double* right_speed);
static void apply_motor_speeds(double left_speed, double right_speed);
static bool is_move_completed(void);
static void complete_current_move(void);
static int32_t validate_command(const motion_command_t* command);
static void reset_motion_state(void);
static double apply_acceleration_limit(double current, double target, double max_accel, double dt);

/////////////////////////////////
/// @brief Thread de contrôle de mouvement
/////////////////////////////////
static void* motion_control_task(void* arg) 
{
    (void)arg;
    
    const uint32_t period_ms = 1000 / g_controller.config.control_frequency_hz;
    uint64_t next_wake_time = xTimerGetCurrentMs() + period_ms;
    
    X_LOG_TRACE("Motion control task started at %dHz", g_controller.config.control_frequency_hz);
    
    while (g_controller.running) {
        mutexLock(&g_controller.mutex);
        
        // Mise à jour du contrôle de mouvement
        update_motion_control();
        
        mutexUnlock(&g_controller.mutex);
        
        // Attente de la prochaine période (temps réel)
        next_wake_time += period_ms;
        uint64_t current_time = xTimerGetCurrentMs();
        if (next_wake_time > current_time) {
            xTimerDelay(next_wake_time - current_time);
        } else {
            // Dépassement de timing
            X_LOG_TRACE("Motion control timing overrun!");
            next_wake_time = current_time;
        }
    }
    
    // Arrêt sécurisé des moteurs
    apply_motor_speeds(0.0, 0.0);
    X_LOG_TRACE("Motion control task stopped");
    return (void*)OS_TASK_EXIT_SUCCESS;
}

/////////////////////////////////
/// @brief Mise à jour principale du contrôle de mouvement
/////////////////////////////////
static void update_motion_control(void) 
{
    if (!g_controller.has_active_command) {
        g_controller.state = MOTION_STATE_IDLE;
        g_controller.current_linear_speed = 0.0;
        g_controller.current_angular_speed = 0.0;
        apply_motor_speeds(0.0, 0.0);
        return;
    }
    
    // Vérifier si le mouvement est terminé
    if (is_move_completed()) {
        complete_current_move();
        return;
    }
    
    // Calcul de la vitesse cible selon le type de mouvement
    switch (g_controller.current_command.type) {
        case MOTION_TYPE_STRAIGHT:
            g_controller.state = MOTION_STATE_MOVING;
            g_controller.target_linear_speed = g_controller.current_command.straight.max_speed_mm_s;
            g_controller.target_angular_speed = 0.0;
            break;
            
        case MOTION_TYPE_TURN:
            g_controller.state = MOTION_STATE_TURNING;
            g_controller.target_linear_speed = 0.0;
            g_controller.target_angular_speed = g_controller.current_command.turn.max_speed_rad_s;
            // Ajuster le signe selon l'angle
            if (g_controller.target_angle_rad < 0) {
                g_controller.target_angular_speed = -g_controller.target_angular_speed;
            }
            break;
            
        case MOTION_TYPE_GOTO: {
            g_controller.state = MOTION_STATE_MOVING;
            
            // Obtenir la position actuelle
            robot_position_t current_pos;
            if (position_tracker_get_position(&current_pos) == POSITION_TRACKER_OK) {
                // Calculer la direction vers la cible
                double dx = g_controller.current_command.goto_pos.target_x_mm - current_pos.x_mm;
                double dy = g_controller.current_command.goto_pos.target_y_mm - current_pos.y_mm;
                double distance_to_target = sqrt(dx*dx + dy*dy);
                double angle_to_target = atan2(dy, dx);
                
                // Calculer l'erreur d'angle
                double angle_error = angle_to_target - current_pos.theta_rad;
                // Normaliser l'erreur d'angle
                while (angle_error > M_PI) angle_error -= 2.0 * M_PI;
                while (angle_error < -M_PI) angle_error += 2.0 * M_PI;
                
                // Contrôle proportionnel simple
                if (fabs(angle_error) > 0.1) {  // Si erreur d'angle > ~6°
                    // Priorité à la rotation
                    g_controller.target_linear_speed = 0.0;
                    g_controller.target_angular_speed = g_controller.config.angular_kp * angle_error;
                } else {
                    // Avancer vers la cible
                    g_controller.target_linear_speed = fmin(g_controller.current_command.goto_pos.max_speed_mm_s,
                                                           distance_to_target * 2.0); // Ralentir près de la cible
                    g_controller.target_angular_speed = g_controller.config.angular_kp * angle_error * 0.5;
                }
            }
            break;
        }
        
        case MOTION_TYPE_CONTINUOUS:
            g_controller.state = MOTION_STATE_MOVING;
            g_controller.target_linear_speed = g_controller.current_command.continuous.speed_mm_s;
            g_controller.target_angular_speed = 0.0;
            break;
            
        default:
            g_controller.target_linear_speed = 0.0;
            g_controller.target_angular_speed = 0.0;
            break;
    }
    
    // Limitation des vitesses maximales
    if (fabs(g_controller.target_linear_speed) > g_controller.config.max_linear_speed_mm_s) {
        g_controller.target_linear_speed = (g_controller.target_linear_speed > 0) ? 
            g_controller.config.max_linear_speed_mm_s : -g_controller.config.max_linear_speed_mm_s;
    }
    if (fabs(g_controller.target_angular_speed) > g_controller.config.max_angular_speed_rad_s) {
        g_controller.target_angular_speed = (g_controller.target_angular_speed > 0) ? 
            g_controller.config.max_angular_speed_rad_s : -g_controller.config.max_angular_speed_rad_s;
    }
    
    // Application des rampes d'accélération
    double dt = 1.0 / g_controller.config.control_frequency_hz;
    
    g_controller.current_linear_speed = apply_acceleration_limit(
        g_controller.current_linear_speed, 
        g_controller.target_linear_speed,
        g_controller.config.max_acceleration_mm_s2, 
        dt);
    
    g_controller.current_angular_speed = apply_acceleration_limit(
        g_controller.current_angular_speed, 
        g_controller.target_angular_speed,
        g_controller.config.max_angular_accel_rad_s2, 
        dt);
    
    // Calcul et application des vitesses de roues
    double left_speed, right_speed;
    calculate_wheel_speeds(g_controller.current_linear_speed, 
                          g_controller.current_angular_speed, 
                          &left_speed, &right_speed);
    apply_motor_speeds(left_speed, right_speed);
}

/////////////////////////////////
/// @brief Application des limites d'accélération
/////////////////////////////////
static double apply_acceleration_limit(double current, double target, double max_accel, double dt) 
{
    double error = target - current;
    double max_change = max_accel * dt;
    
    if (fabs(error) > max_change) {
        return current + ((error > 0) ? max_change : -max_change);
    } else {
        return target;
    }
}

/////////////////////////////////
/// @brief Calcul des vitesses de roues (cinématique différentielle)
/////////////////////////////////
static void calculate_wheel_speeds(double linear_speed, double angular_speed, 
                                  double* left_speed, double* right_speed) 
{
    double wheel_radius = (WHEEL_DIAMETER_CM * 10.0) / 2.0; // mm
    double wheel_base = WHEEL_DISTANCE_CM * 10.0; // mm
    
    // Cinématique différentielle
    double left_linear = linear_speed - (angular_speed * wheel_base / 2.0);
    double right_linear = linear_speed + (angular_speed * wheel_base / 2.0);
    
    // Conversion en vitesses angulaires (rad/s)
    *left_speed = left_linear / wheel_radius;
    *right_speed = right_linear / wheel_radius;
    
    // Limitation des vitesses
    double max_wheel_speed = g_controller.config.max_linear_speed_mm_s / wheel_radius;
    if (fabs(*left_speed) > max_wheel_speed) {
        *left_speed = (*left_speed > 0) ? max_wheel_speed : -max_wheel_speed;
    }
    if (fabs(*right_speed) > max_wheel_speed) {
        *right_speed = (*right_speed > 0) ? max_wheel_speed : -max_wheel_speed;
    }
}

/////////////////////////////////
/// @brief Application des vitesses aux moteurs
/////////////////////////////////
static void apply_motor_speeds(double left_speed, double right_speed) 
{
    // Conversion en pourcentage PWM
    double max_speed_rad_s = MAX_SPEED_RAD_S;
    
    int left_pwm = (int)((left_speed / max_speed_rad_s) * 100.0);
    int right_pwm = (int)((right_speed / max_speed_rad_s) * 100.0);
    
    // Saturation
    if (left_pwm > 100) left_pwm = 100;
    if (left_pwm < -100) left_pwm = -100;
    if (right_pwm > 100) right_pwm = 100;
    if (right_pwm < -100) right_pwm = -100;
    
    // Application via le système de contrôle moteur
    SetMotorSpeed(MRPIZ_MOTOR_LEFT, left_pwm);
    SetMotorSpeed(MRPIZ_MOTOR_RIGHT, right_pwm);
}

/////////////////////////////////
/// @brief Vérification de fin de mouvement
/////////////////////////////////
static bool is_move_completed(void) 
{
    robot_position_t current_pos;
    if (position_tracker_get_position(&current_pos) != POSITION_TRACKER_OK) {
        return true; // Erreur, on arrête
    }
    
    switch (g_controller.current_command.type) {
        case MOTION_TYPE_STRAIGHT: {
            // Calculer la distance parcourue
            double dx = current_pos.x_mm - g_controller.start_position.x_mm;
            double dy = current_pos.y_mm - g_controller.start_position.y_mm;
            double distance_traveled = sqrt(dx*dx + dy*dy);
            
            return distance_traveled >= (abs(g_controller.target_distance_mm) - g_controller.config.position_tolerance_mm);
        }
        
        case MOTION_TYPE_TURN: {
            // Calculer l'angle parcouru
            double angle_traveled = fabs(current_pos.theta_rad - g_controller.start_position.theta_rad);
            
            // Gérer le wrap-around de l'angle
            if (angle_traveled > M_PI) {
                angle_traveled = 2.0 * M_PI - angle_traveled;
            }
            
            return angle_traveled >= (fabs(g_controller.target_angle_rad) - g_controller.config.angle_tolerance_rad);
        }
        
        case MOTION_TYPE_GOTO: {
            double dx = g_controller.current_command.goto_pos.target_x_mm - current_pos.x_mm;
            double dy = g_controller.current_command.goto_pos.target_y_mm - current_pos.y_mm;
            double distance = sqrt(dx*dx + dy*dy);
            return distance < g_controller.config.position_tolerance_mm;
        }
        
        case MOTION_TYPE_CONTINUOUS:
            return false; // Jamais terminé
            
        default:
            return true;
    }
}

/////////////////////////////////
/// @brief Finalisation d'un mouvement
/////////////////////////////////
static void complete_current_move(void) 
{
    g_controller.has_active_command = false;
    g_controller.state = MOTION_STATE_IDLE;
    g_controller.internal_state = MOTION_INTERNAL_IDLE;
    
    // Arrêt progressif
    g_controller.target_linear_speed = 0.0;
    g_controller.target_angular_speed = 0.0;
    
    // Callback de fin de mouvement
    if (g_controller.completed_callback) {
        g_controller.completed_callback();
    }
    
    X_LOG_TRACE("Motion completed");
}

/////////////////////////////////
/// @brief Validation d'une commande
/////////////////////////////////
static int32_t validate_command(const motion_command_t* command) 
{
    if (!command) return MOTION_CONTROLLER_ERROR_PARAM;
    
    switch (command->type) {
        case MOTION_TYPE_STRAIGHT:
            if (command->straight.max_speed_mm_s > g_controller.config.max_linear_speed_mm_s) {
                return MOTION_CONTROLLER_ERROR_PARAM;
            }
            break;
            
        case MOTION_TYPE_TURN:
            if (command->turn.max_speed_rad_s > g_controller.config.max_angular_speed_rad_s) {
                return MOTION_CONTROLLER_ERROR_PARAM;
            }
            break;
            
        case MOTION_TYPE_GOTO:
            if (command->goto_pos.max_speed_mm_s > g_controller.config.max_linear_speed_mm_s) {
                return MOTION_CONTROLLER_ERROR_PARAM;
            }
            break;
            
        case MOTION_TYPE_CONTINUOUS:
            if (abs(command->continuous.speed_mm_s) > g_controller.config.max_linear_speed_mm_s) {
                return MOTION_CONTROLLER_ERROR_PARAM;
            }
            break;
            
        default:
            return MOTION_CONTROLLER_ERROR_PARAM;
    }
    
    return MOTION_CONTROLLER_OK;
}

/////////////////////////////////
/// @brief Réinitialisation de l'état de mouvement
/////////////////////////////////
static void reset_motion_state(void) 
{
    g_controller.internal_state = MOTION_INTERNAL_IDLE;
    
    // Sauvegarder la position de départ
    if (position_tracker_get_position(&g_controller.start_position) != POSITION_TRACKER_OK) {
        memset(&g_controller.start_position, 0, sizeof(robot_position_t));
    }
    
    // Reset PID
    g_controller.pid_state.linear_integral = 0.0;
    g_controller.pid_state.linear_last_error = 0.0;
    g_controller.pid_state.angular_integral = 0.0;
    g_controller.pid_state.angular_last_error = 0.0;
}

/////////////////////////////////
/// @brief API Publique
/////////////////////////////////

int32_t motion_controller_init(const motion_controller_config_t* config,
                              motion_completed_callback_t completed_callback,
                              motion_error_callback_t error_callback) 
{
    if (g_controller.initialized) {
        return MOTION_CONTROLLER_OK;
    }
    
    // Configuration
    if (config) {
        g_controller.config = *config;
    } else {
        g_controller.config = MOTION_CONTROLLER_DEFAULT_CONFIG;
    }
    
    // Validation de la configuration
    if (g_controller.config.control_frequency_hz < 10 || g_controller.config.control_frequency_hz > 200) {
        return MOTION_CONTROLLER_ERROR_PARAM;
    }
    
    // Initialisation des mutexes
    if (mutexCreate(&g_controller.mutex) != MUTEX_OK) {
        return MOTION_CONTROLLER_ERROR_HARDWARE;
    }
    
    // État initial
    g_controller.state = MOTION_STATE_IDLE;
    g_controller.has_active_command = false;
    g_controller.completed_callback = completed_callback;
    g_controller.error_callback = error_callback;
    
    // Création et démarrage de la tâche de contrôle
    osTaskInit(&g_controller.control_task);
    g_controller.control_task.t_ptTask = motion_control_task;
    g_controller.control_task.t_ptTaskArg = NULL;
    
    if (osTaskCreate(&g_controller.control_task) != OS_TASK_SUCCESS) {
        mutexDestroy(&g_controller.mutex);
        return MOTION_CONTROLLER_ERROR_THREAD;
    }
    
    g_controller.running = true;
    g_controller.initialized = true;
    
    X_LOG_TRACE("Motion controller initialized at %dHz", g_controller.config.control_frequency_hz);
    return MOTION_CONTROLLER_OK;
}

int32_t motion_controller_shutdown(void) 
{
    if (!g_controller.initialized) {
        return MOTION_CONTROLLER_OK;
    }
    
    g_controller.running = false;
    osTaskStop(&g_controller.control_task, 2);
    osTaskWait(&g_controller.control_task, NULL);
    mutexDestroy(&g_controller.mutex);
    
    g_controller.initialized = false;
    X_LOG_TRACE("Motion controller shutdown");
    return MOTION_CONTROLLER_OK;
}

int32_t motion_controller_execute(const motion_command_t* command) 
{
    if (!g_controller.initialized) {
        return MOTION_CONTROLLER_ERROR_INIT;
    }
    
    int32_t result = validate_command(command);
    if (result != MOTION_CONTROLLER_OK) {
        return result;
    }
    
    mutexLock(&g_controller.mutex);
    
    // Arrêt du mouvement précédent si nécessaire
    if (g_controller.has_active_command) {
        reset_motion_state();
    }
    
    g_controller.current_command = *command;
    g_controller.has_active_command = true;
    
    // Calcul des objectifs selon le type de mouvement
    switch (command->type) {
        case MOTION_TYPE_STRAIGHT:
            g_controller.target_distance_mm = command->straight.distance_mm;
            break;
            
        case MOTION_TYPE_TURN:
            if (command->turn.relative) {
                g_controller.target_angle_rad = command->turn.angle_rad;
            } else {
                // Angle absolu - calculer la différence
                robot_position_t current_pos;
                if (position_tracker_get_position(&current_pos) == POSITION_TRACKER_OK) {
                    g_controller.target_angle_rad = command->turn.angle_rad - current_pos.theta_rad;
                    // Normaliser l'angle
                    while (g_controller.target_angle_rad > M_PI) g_controller.target_angle_rad -= 2.0 * M_PI;
                    while (g_controller.target_angle_rad < -M_PI) g_controller.target_angle_rad += 2.0 * M_PI;
                } else {
                    g_controller.target_angle_rad = command->turn.angle_rad;
                }
            }
            break;
            
        default:
            break;
    }
    
    reset_motion_state();
    mutexUnlock(&g_controller.mutex);
    
    return MOTION_CONTROLLER_OK;
}

int32_t motion_controller_stop(bool emergency) 
{
    if (!g_controller.initialized) {
        return MOTION_CONTROLLER_ERROR_INIT;
    }
    
    mutexLock(&g_controller.mutex);
    
    g_controller.has_active_command = false;
    g_controller.state = MOTION_STATE_STOPPING;
    
    if (emergency) {
        // Arrêt immédiat
        g_controller.current_linear_speed = 0.0;
        g_controller.current_angular_speed = 0.0;
        apply_motor_speeds(0.0, 0.0);
    }
    
    reset_motion_state();
    mutexUnlock(&g_controller.mutex);
    
    return MOTION_CONTROLLER_OK;
}

motion_state_t motion_controller_get_state(void) 
{
    return g_controller.state;
}

bool motion_controller_is_moving(void) 
{
    return g_controller.state == MOTION_STATE_MOVING || 
           g_controller.state == MOTION_STATE_TURNING;
}

int32_t motion_controller_get_target_speeds(double* linear_speed, double* angular_speed) 
{
    if (!g_controller.initialized || !linear_speed || !angular_speed) {
        return MOTION_CONTROLLER_ERROR_PARAM;
    }
    
    mutexLock(&g_controller.mutex);
    *linear_speed = g_controller.target_linear_speed;
    *angular_speed = g_controller.target_angular_speed;
    mutexUnlock(&g_controller.mutex);
    
    return MOTION_CONTROLLER_OK;
}

int32_t motion_controller_get_progress_percent(void) 
{
    if (!g_controller.has_active_command) {
        return -1;
    }
    
    robot_position_t current_pos;
    if (position_tracker_get_position(&current_pos) != POSITION_TRACKER_OK) {
        return -1;
    }
    
    switch (g_controller.current_command.type) {
        case MOTION_TYPE_STRAIGHT: {
            double dx = current_pos.x_mm - g_controller.start_position.x_mm;
            double dy = current_pos.y_mm - g_controller.start_position.y_mm;
            double distance_traveled = sqrt(dx*dx + dy*dy);
            double progress = (distance_traveled / abs(g_controller.target_distance_mm)) * 100.0;
            return (int32_t)fmin(progress, 100.0);
        }
        
        case MOTION_TYPE_TURN: {
            double angle_traveled = fabs(current_pos.theta_rad - g_controller.start_position.theta_rad);
            if (angle_traveled > M_PI) angle_traveled = 2.0 * M_PI - angle_traveled;
            double progress = (angle_traveled / fabs(g_controller.target_angle_rad)) * 100.0;
            return (int32_t)fmin(progress, 100.0);
        }
        
        case MOTION_TYPE_GOTO: {
            double start_dx = g_controller.current_command.goto_pos.target_x_mm - g_controller.start_position.x_mm;
            double start_dy = g_controller.current_command.goto_pos.target_y_mm - g_controller.start_position.y_mm;
            double total_distance = sqrt(start_dx*start_dx + start_dy*start_dy);
            
            double current_dx = g_controller.current_command.goto_pos.target_x_mm - current_pos.x_mm;
            double current_dy = g_controller.current_command.goto_pos.target_y_mm - current_pos.y_mm;
            double remaining_distance = sqrt(current_dx*current_dx + current_dy*current_dy);
            
            if (total_distance > 0) {
                double progress = ((total_distance - remaining_distance) / total_distance) * 100.0;
                return (int32_t)fmax(0.0, fmin(progress, 100.0));
            }
            return 100;
        }
        
        default:
            return -1;
    }
}

/////////////////////////////////
/// @brief Fonctions de convenance
/////////////////////////////////

int32_t motion_controller_move_straight(int32_t distance_mm, uint32_t speed_mm_s) 
{
    motion_command_t cmd = {
        .type = MOTION_TYPE_STRAIGHT,
        .straight = {
            .distance_mm = distance_mm,
            .max_speed_mm_s = speed_mm_s
        }
    };
    return motion_controller_execute(&cmd);
}

int32_t motion_controller_turn_relative(double angle_rad, double speed_rad_s) 
{
    motion_command_t cmd = {
        .type = MOTION_TYPE_TURN,
        .turn = {
            .angle_rad = angle_rad,
            .max_speed_rad_s = speed_rad_s,
            .relative = true
        }
    };
    return motion_controller_execute(&cmd);
}

int32_t motion_controller_turn_absolute(double angle_rad, double speed_rad_s) 
{
    motion_command_t cmd = {
        .type = MOTION_TYPE_TURN,
        .turn = {
            .angle_rad = angle_rad,
            .max_speed_rad_s = speed_rad_s,
            .relative = false
        }
    };
    return motion_controller_execute(&cmd);
}

int32_t motion_controller_goto(int32_t x_mm, int32_t y_mm, uint32_t speed_mm_s) 
{
    motion_command_t cmd = {
        .type = MOTION_TYPE_GOTO,
        .goto_pos = {
            .target_x_mm = x_mm,
            .target_y_mm = y_mm,
            .max_speed_mm_s = speed_mm_s
        }
    };
    return motion_controller_execute(&cmd);
}

int32_t motion_controller_move_continuous(int32_t speed_mm_s) 
{
    motion_command_t cmd = {
        .type = MOTION_TYPE_CONTINUOUS,
        .continuous = {
            .speed_mm_s = speed_mm_s
        }
    };
    return motion_controller_execute(&cmd);
} 
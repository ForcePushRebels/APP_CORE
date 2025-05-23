#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include <math.h>
#include <string.h>

#define WHEEL_RADIUS_M      0.03
#define ENCODER_TICKS_REV   360
#define REGULATION_PERIOD_MS 50
#define KP                  30.0    // Gain proportionnel à adapter
#define KI                  0.1     // Gain intégral
#define MAX_INTEGRAL_ERROR  100.0   // Limite de l'erreur intégrale
#define MAX_SPEED_CM_S      16.0
#define MAX_SPEED_RAD_S     (MAX_SPEED_CM_S / (WHEEL_RADIUS_M * 100.0))  // Conversion de cm/s en rad/s

// Constantes pour les IDs des moteurs
#define MOTOR_LEFT_ID       MRPIZ_MOTOR_LEFT
#define MOTOR_RIGHT_ID      MRPIZ_MOTOR_RIGHT

typedef struct {
    double target_speed_rad_s;
    double current_speed_rad_s;
    double error;           // Erreur actuelle
    double last_error;      // Erreur précédente
    double integral_error;  // Erreur intégrale pour correction PI
    int encoder_last;
    uint64_t last_update_ms;
    xOsMutexCtx mutex;
    bool running;
    xOsTaskCtx task;
    const uint16_t motor_id; // ID constant, ne peut pas être modifié après l'init
} motor_regulator_t;

static motor_regulator_t g_left_motor = { .motor_id = MOTOR_LEFT_ID };
static motor_regulator_t g_right_motor = { .motor_id = MOTOR_RIGHT_ID };
static bool g_initialized = false;

// Prototypes des fonctions statiques
static void* motor_regulator_task(void* arg);
static void motor_regulator_init(motor_regulator_t* reg, uint16_t motor_id);
static void motor_regulator_shutdown(motor_regulator_t* reg);
static bool is_valid_motor_id(uint16_t motor_id);
static double encoder_ticks_to_rad_per_sec(int delta_ticks, double dt);
static void update_motor_speed(motor_regulator_t* reg, int encoder_now, uint64_t now_ms);
static int calculate_pwm_command(motor_regulator_t* reg);

// --- Fonctions internes ---

static double encoder_ticks_to_rad_per_sec(int delta_ticks, double dt) {
    if (dt <= 0.0) return 0.0;
    
    // Calcul de la vitesse angulaire en rad/s
    // 2π radians = ENCODER_TICKS_REV ticks
    double rad_per_tick = (2.0 * M_PI) / ENCODER_TICKS_REV;
    double speed_rad_s = (delta_ticks * rad_per_tick) / dt;
    
    //X_LOG_TRACE("Encoder conversion: delta=%d, dt=%.3f, speed=%.3f rad/s", 
    //            delta_ticks, dt, speed_rad_s);
    
    return speed_rad_s;
}

static bool is_valid_motor_id(uint16_t motor_id) {
    return (motor_id == MOTOR_LEFT_ID || motor_id == MOTOR_RIGHT_ID);
}

static void update_motor_speed(motor_regulator_t* reg, int encoder_now, uint64_t now_ms) {
    double dt = (now_ms - reg->last_update_ms) / 1000.0; // Convert to seconds
    if (dt <= 0.0) return;

    // Calcul du delta d'encodeur
    int delta = encoder_now - reg->encoder_last;
    
    // Gestion du débordement de l'encodeur
    if (delta > ENCODER_TICKS_REV/2) {
        delta -= ENCODER_TICKS_REV;
    } else if (delta < -ENCODER_TICKS_REV/2) {
        delta += ENCODER_TICKS_REV;
    }

    // Calcul de la vitesse actuelle
    double new_speed = encoder_ticks_to_rad_per_sec(delta, dt);
    
    // Mise à jour de la vitesse avec un filtre passe-bas simple
    const double alpha = 0.3; // Facteur de filtrage (0-1)
    reg->current_speed_rad_s = (alpha * new_speed) + ((1.0 - alpha) * reg->current_speed_rad_s);

    // Calcul de l'erreur
    reg->last_error = reg->error;
    reg->error = reg->target_speed_rad_s - reg->current_speed_rad_s;

    // Mise à jour de l'erreur intégrale avec anti-windup
    reg->integral_error += reg->error * dt;
    if (reg->integral_error > MAX_INTEGRAL_ERROR) reg->integral_error = MAX_INTEGRAL_ERROR;
    if (reg->integral_error < -MAX_INTEGRAL_ERROR) reg->integral_error = -MAX_INTEGRAL_ERROR;

    // Mise à jour des valeurs
    reg->encoder_last = encoder_now;
    reg->last_update_ms = now_ms;

    //X_LOG_TRACE("Motor %d - Speed update: Current=%.2f rad/s, Target=%.2f rad/s, Error=%.2f, Integral=%.2f, Delta=%d",
    //            reg->motor_id, reg->current_speed_rad_s, reg->target_speed_rad_s, 
    //            reg->error, reg->integral_error, delta);
}

static int calculate_pwm_command(motor_regulator_t* reg) {
    // Calcul de la commande PWM avec correction PI
    double pwm = (reg->error * KP) + (reg->integral_error * KI);
    
    // Saturation PWM
    if (pwm > 100) pwm = 100;
    if (pwm < -100) pwm = -100;

    //X_LOG_TRACE("Motor %d - PWM calculation: Error=%.2f, Integral=%.2f, PWM=%.1f",
    //            reg->motor_id, reg->error, reg->integral_error, pwm);

    return (int)pwm;
}

static void motor_regulator_init(motor_regulator_t* reg, uint16_t motor_id) {
    //X_LOG_TRACE("Initializing motor regulator for motor %d", motor_id);
    
    // Validation de l'ID du moteur
    if (!is_valid_motor_id(motor_id)) {
        //X_LOG_TRACE("Invalid motor ID: %d (must be %d or %d)", 
        //            motor_id, MOTOR_LEFT_ID, MOTOR_RIGHT_ID);
        return;
    }
    
    // Vérification que l'ID correspond à la structure
    if (reg->motor_id != motor_id) {
        //X_LOG_TRACE("Motor ID mismatch: struct=%d, param=%d", reg->motor_id, motor_id);
        return;
    }
    
    // Initialisation complète de la structure
    memset(reg, 0, sizeof(*reg));
    // Ne pas toucher à motor_id car il est constant
    reg->target_speed_rad_s = 0.0;
    reg->current_speed_rad_s = 0.0;
    reg->error = 0.0;
    reg->last_error = 0.0;
    reg->integral_error = 0.0;
    reg->encoder_last = GetMotorEncoderValues(&motor_id);
    reg->last_update_ms = xTimerGetCurrentMs();
    reg->running = false;  // Ne pas démarrer avant que tout soit initialisé

    // Initialisation du mutex
    if (mutexCreate(&reg->mutex) != MUTEX_OK) {
        //X_LOG_TRACE("Failed to create mutex for motor %d", motor_id);
        return;
    }
    //X_LOG_TRACE("Mutex created for motor %d", motor_id);

    // Initialisation de la tâche avec les paramètres par défaut
    osTaskInit(&reg->task);
    reg->task.t_ptTask = motor_regulator_task;
    reg->task.t_ptTaskArg = reg;

    // Création de la tâche
    int result = osTaskCreate(&reg->task);
    if (result != OS_TASK_SUCCESS) {
        //X_LOG_TRACE("Failed to create task for motor %d, error: %d (%s)", 
        //            motor_id, result, osTaskGetErrorString(result));
        mutexDestroy(&reg->mutex);
        return;
    }
    //X_LOG_TRACE("Task created for motor %d", motor_id);

    // Démarrer la tâche seulement après une initialisation réussie
    reg->running = true;
    //X_LOG_TRACE("Motor %d regulator started", motor_id);
}

static void motor_regulator_shutdown(motor_regulator_t* reg) {
    if (!reg) return;
    
    //X_LOG_TRACE("Shutting down motor regulator for motor %d", reg->motor_id);
    reg->running = false;
    osTaskStop(&reg->task, 2); // Attendre max 2 secondes
    osTaskWait(&reg->task, NULL);
    mutexDestroy(&reg->mutex);
    //X_LOG_TRACE("Motor regulator for motor %d shutdown complete", reg->motor_id);
}

static void* motor_regulator_task(void* arg) {
    motor_regulator_t* reg = (motor_regulator_t*)arg;
    if (!reg) {
        //X_LOG_TRACE("Invalid regulator task argument (NULL)");
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    // Vérification de l'ID du moteur
    if (!is_valid_motor_id(reg->motor_id)) {
        //X_LOG_TRACE("Invalid motor ID in task: %d", reg->motor_id);
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    //X_LOG_TRACE("Starting regulator task for motor %d", reg->motor_id);
    
    // Attendre un peu avant de commencer la régulation
    xTimerDelay(100);
    
    while (reg->running) {
        mutexLock(&reg->mutex);

        // Vérification de l'ID du moteur avant chaque lecture
        if (!is_valid_motor_id(reg->motor_id)) {
            //X_LOG_TRACE("Motor ID corrupted in task: %d", reg->motor_id);
            mutexUnlock(&reg->mutex);
            xTimerDelay(REGULATION_PERIOD_MS);
            continue;
        }

        // Lecture de l'encodeur avec une copie temporaire de l'ID
        uint16_t motor_id = reg->motor_id;
        int encoder_now = GetMotorEncoderValues(&motor_id);
        uint64_t now_ms = xTimerGetCurrentMs();

        // Mise à jour de la vitesse et calcul des erreurs
        //update_motor_speed(reg, encoder_now, now_ms);

        // Calcul de la commande PWM
        //int pwm = calculate_pwm_command(reg);

        // Conversion directe de la vitesse cible en PWM
        int pwm = (int)((reg->target_speed_rad_s / MAX_SPEED_RAD_S) * 100.0);
        if (pwm > 100) pwm = 100;
        if (pwm < -100) pwm = -100;

        // Debug logging
        //X_LOG_TRACE("Motor %d - Target: %.2f rad/s, PWM: %d",
        //            reg->motor_id, reg->target_speed_rad_s, pwm);

        // Appliquer la commande PWM
        SetMotorSpeed(reg->motor_id, pwm);

        mutexUnlock(&reg->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }
    
    //X_LOG_TRACE("Stopping regulator task for motor %d", reg->motor_id);
    // Arrêt moteur à la sortie
    SetMotorSpeed(reg->motor_id, 0);
    return (void*)OS_TASK_EXIT_SUCCESS;
}

// --- Fonctions publiques ---

int motor_control_init(void) {
    if (g_initialized) {
        //X_LOG_TRACE("Motor control already initialized");
        return 0;
    }

    // Initialisation explicite des structures globales
    memset(&g_left_motor, 0, sizeof(g_left_motor));
    memset(&g_right_motor, 0, sizeof(g_right_motor));

    // Initialisation des régulateurs avec les IDs constants

    
    motor_regulator_init(&g_right_motor, MOTOR_RIGHT_ID);
    xTimerDelay(100);  // Attendre un peu entre les initialisations
    motor_regulator_init(&g_left_motor, MOTOR_LEFT_ID);
    g_initialized = true;
    //X_LOG_TRACE("Motor control initialized");
    return 0;
}

void motor_control_shutdown(void) {
    if (!g_initialized) {
        //X_LOG_TRACE("Motor control not initialized, nothing to shutdown");
        return;
    }

    //X_LOG_TRACE("Shutting down motor control system");
    
    // Arrêt des deux régulateurs
    motor_regulator_shutdown(&g_left_motor);
    motor_regulator_shutdown(&g_right_motor);
    
    g_initialized = false;
    //X_LOG_TRACE("Motor control system shutdown complete");
}

int motor_control_set_speed(uint16_t motor_id, double speed_rad_s) {
    if (!g_initialized) {
        //X_LOG_TRACE("Motor control not initialized");
        return -1;
    }

    if (!is_valid_motor_id(motor_id)) {
        //X_LOG_TRACE("Invalid motor ID: %d", motor_id);
        return -1;
    }

    // Limitation de la vitesse à la vitesse maximale
    if (speed_rad_s > MAX_SPEED_RAD_S) speed_rad_s = MAX_SPEED_RAD_S;
    if (speed_rad_s < -MAX_SPEED_RAD_S) speed_rad_s = -MAX_SPEED_RAD_S;

    motor_regulator_t* reg = (motor_id == MOTOR_LEFT_ID) ? &g_left_motor : &g_right_motor;
    
    mutexLock(&reg->mutex);
    reg->target_speed_rad_s = speed_rad_s;
    //X_LOG_TRACE("Setting motor %d speed to %.2f rad/s", motor_id, speed_rad_s);
    mutexUnlock(&reg->mutex);
    
    return 0;
}

int motor_control_set_left_speed(double speed_rad_s) {
    return motor_control_set_speed(MOTOR_LEFT_ID, speed_rad_s);
}

int motor_control_set_right_speed(double speed_rad_s) {
    return motor_control_set_speed(MOTOR_RIGHT_ID, speed_rad_s);
}

double motor_control_get_left_speed(void) {
    if (!g_initialized) {
        //X_LOG_TRACE("Motor control not initialized");
        return 0.0;
    }

    double speed = 0.0;
    mutexLock(&g_left_motor.mutex);
    speed = g_left_motor.current_speed_rad_s;
    mutexUnlock(&g_left_motor.mutex);
    
    //X_LOG_TRACE("Left motor speed read: %.2f rad/s", speed);
    return speed;
}

double motor_control_get_right_speed(void) {
    if (!g_initialized) {
        //X_LOG_TRACE("Motor control not initialized");
        return 0.0;
    }

    double speed = 0.0;
    mutexLock(&g_right_motor.mutex);
    speed = g_right_motor.current_speed_rad_s;
    mutexUnlock(&g_right_motor.mutex);
    
    //X_LOG_TRACE("Right motor speed read: %.2f rad/s", speed);
    return speed;
}

void motor_control_stop(void) {
    if (!g_initialized) return;
    motor_control_set_left_speed(0.0);
    motor_control_set_right_speed(0.0);
}

// --- Fin du fichier ---
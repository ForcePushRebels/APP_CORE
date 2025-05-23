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

typedef struct {
    double target_speed_rad_s;
    double current_speed_rad_s;
    int encoder_last;
    uint64_t last_update_ms;
    xOsMutexCtx mutex;
    bool running;
    xOsTaskCtx task;
    uint16_t motor_id; // 0 = gauche, 1 = droite
} motor_regulator_t;

static motor_regulator_t g_left_motor;
static motor_regulator_t g_right_motor;
static bool g_initialized = false;

// --- Fonctions internes ---

static double encoder_ticks_to_rad_per_sec(int delta_ticks, double dt) {
    if (dt <= 0.0) return 0.0;
    
    // Calcul de la distance parcourue par tick
    double distance_per_tick = (2.0 * M_PI * WHEEL_RADIUS_M) / ENCODER_TICKS_REV;
    
    // Calcul de la distance totale parcourue
    double distance = delta_ticks * distance_per_tick;
    
    // Conversion en vitesse angulaire (rad/s)
    double speed = distance / (WHEEL_RADIUS_M * dt);
    
    return speed;
}

static void* motor_regulator_task(void* arg) {
    motor_regulator_t* reg = (motor_regulator_t*)arg;
    while (reg->running) {
        mutexLock(&reg->mutex);

        int encoder_now = GetMotorEncoderValues(&reg->motor_id);
        uint64_t now_ms = xTimerGetCurrentMs();
        double dt = (now_ms - reg->last_update_ms) / 1000.0; // Convert to seconds

        int delta = encoder_now - reg->encoder_last;
        reg->current_speed_rad_s = encoder_ticks_to_rad_per_sec(delta, dt);

        // Régulation simple proportionnelle
        double error = reg->target_speed_rad_s - reg->current_speed_rad_s;
        int pwm = (int)(reg->target_speed_rad_s * 100.0 / (2.0 * M_PI * 10.0)); // Échelle grossière
        pwm += (int)(error * KP);

        // Saturation PWM
        if (pwm > 100) pwm = 100;
        if (pwm < -100) pwm = -100;

        SetMotorSpeed(reg->motor_id, pwm);

        // Mise à jour état
        reg->encoder_last = encoder_now;
        reg->last_update_ms = now_ms;

        mutexUnlock(&reg->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }
    // Arrêt moteur à la sortie
    SetMotorSpeed(reg->motor_id, 0);
    return (void*)OS_TASK_EXIT_SUCCESS;
}

static void motor_regulator_init(motor_regulator_t* reg, uint16_t motor_id) {
    memset(reg, 0, sizeof(*reg)); // Initialisation de la structure avec des zéros
    reg->motor_id = motor_id;
    reg->target_speed_rad_s = 0.0;
    reg->current_speed_rad_s = 0.0;
    reg->encoder_last = GetMotorEncoderValues(&motor_id);
    reg->last_update_ms = xTimerGetCurrentMs();

    // Initialisation du mutex
    if (mutexCreate(&reg->mutex) != MUTEX_OK) {
        return;
    }

    // Initialisation de la tâche
    osTaskInit(&reg->task);
    reg->task.t_ptTask = motor_regulator_task;
    reg->task.t_ptTaskArg = reg;
    reg->task.t_ulStackSize = 4096; // Taille de pile par défaut
    reg->task.t_iPriority = OS_TASK_DEFAULT_PRIORITY;

    reg->running = true;
    if (osTaskCreate(&reg->task) != OS_TASK_SUCCESS) {
        mutexDestroy(&reg->mutex);
        return;
    }
}

static void motor_regulator_shutdown(motor_regulator_t* reg) {
    reg->running = false;
    osTaskStop(&reg->task, 2); // Attendre max 2 secondes
    osTaskWait(&reg->task, NULL);
    mutexDestroy(&reg->mutex);
}

// --- Fonctions publiques ---

int motor_control_init(void) {
    if (g_initialized) return 0;
    motor_regulator_init(&g_left_motor, MRPIZ_MOTOR_LEFT);
    motor_regulator_init(&g_right_motor, MRPIZ_MOTOR_RIGHT);
    g_initialized = true;
    return 0;
}

void motor_control_shutdown(void) {
    if (!g_initialized) return;
    motor_regulator_shutdown(&g_left_motor);
    motor_regulator_shutdown(&g_right_motor);
    g_initialized = false;
}

int motor_control_set_left_speed(double speed_rad_s) {
    if (!g_initialized) return -1;
    mutexLock(&g_left_motor.mutex);
    g_left_motor.target_speed_rad_s = speed_rad_s;
    mutexUnlock(&g_left_motor.mutex);
    return 0;
}

int motor_control_set_right_speed(double speed_rad_s) {
    if (!g_initialized) return -1;
    mutexLock(&g_right_motor.mutex);
    g_right_motor.target_speed_rad_s = speed_rad_s;
    mutexUnlock(&g_right_motor.mutex);
    return 0;
}

double motor_control_get_left_speed(void) {
    if (!g_initialized) return 0.0;
    mutexLock(&g_left_motor.mutex);
    double speed = g_left_motor.current_speed_rad_s;
    mutexUnlock(&g_left_motor.mutex);
    return speed;
}

double motor_control_get_right_speed(void) {
    if (!g_initialized) return 0.0;
    mutexLock(&g_right_motor.mutex);
    double speed = g_right_motor.current_speed_rad_s;
    mutexUnlock(&g_right_motor.mutex);
    return speed;
}

void motor_control_stop(void) {
    if (!g_initialized) return;
    motor_control_set_left_speed(0.0);
    motor_control_set_right_speed(0.0);
}

// --- Fin du fichier ---
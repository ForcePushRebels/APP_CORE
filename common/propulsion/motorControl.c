#include "motorControl.h"
#include "hardwareAbstraction.h"
#include <pthread.h>
#include <math.h>
#include <time.h>
#include <unistd.h>
#include <string.h>

#define WHEEL_RADIUS_M      0.03
#define ENCODER_TICKS_REV   360
#define REGULATION_PERIOD_MS 50
#define KP                  30.0    // Gain proportionnel à adapter

typedef struct {
    double target_speed_rad_s;
    double current_speed_rad_s;
    int encoder_last;
    struct timespec last_update;
    pthread_mutex_t lock;
    bool running;
    pthread_t thread;
    int motor_id; // 0 = gauche, 1 = droite
} motor_regulator_t;

static motor_regulator_t g_left_motor;
static motor_regulator_t g_right_motor;
static bool g_initialized = false;

// --- Fonctions internes ---

static double encoder_ticks_to_rad_per_sec(int delta_ticks, double dt) {
    if (dt <= 0.0) return 0.0;
    double revs = (double)delta_ticks / ENCODER_TICKS_REV;
    double rad = revs * 2.0 * M_PI;
    return rad / dt;
}

static void* motor_regulator_task(void* arg) {
    motor_regulator_t* reg = (motor_regulator_t*)arg;
    while (reg->running) {
        pthread_mutex_lock(&reg->lock);

        int encoder_now = GetMotorEncoderValues(reg->motor_id);
        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC, &now);
        double dt = (now.tv_sec - reg->last_update.tv_sec) +
                    (now.tv_nsec - reg->last_update.tv_nsec) / 1e9;

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
        reg->last_update = now;

        pthread_mutex_unlock(&reg->lock);
        usleep(REGULATION_PERIOD_MS * 1000);
    }
    // Arrêt moteur à la sortie
    SetMotorSpeed(reg->motor_id, 0);
    return NULL;
}

static void motor_regulator_init(motor_regulator_t* reg, int motor_id) {
    memset(reg, 0, sizeof(*reg)); // Initialisation de la structure avec des zéros
    reg->motor_id = motor_id;
    reg->target_speed_rad_s = 0.0;
    reg->current_speed_rad_s = 0.0;
    reg->encoder_last = GetMotorEncoderValues(motor_id);
    clock_gettime(CLOCK_MONOTONIC, &reg->last_update);
    pthread_mutex_init(&reg->lock, NULL);
    reg->running = true;
    pthread_create(&reg->thread, NULL, motor_regulator_task, reg);
}

static void motor_regulator_shutdown(motor_regulator_t* reg) {
    reg->running = false;
    pthread_join(reg->thread, NULL);
    pthread_mutex_destroy(&reg->lock);
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
    pthread_mutex_lock(&g_left_motor.lock);
    g_left_motor.target_speed_rad_s = speed_rad_s;
    pthread_mutex_unlock(&g_left_motor.lock);
    return 0;
}

int motor_control_set_right_speed(double speed_rad_s) {
    if (!g_initialized) return -1;
    pthread_mutex_lock(&g_right_motor.lock);
    g_right_motor.target_speed_rad_s = speed_rad_s;
    pthread_mutex_unlock(&g_right_motor.lock);
    return 0;
}

double motor_control_get_left_speed(void) {
    if (!g_initialized) return 0.0;
    pthread_mutex_lock(&g_left_motor.lock);
    double speed = g_left_motor.current_speed_rad_s;
    pthread_mutex_unlock(&g_left_motor.lock);
    return speed;
}

double motor_control_get_right_speed(void) {
    if (!g_initialized) return 0.0;
    pthread_mutex_lock(&g_right_motor.lock);
    double speed = g_right_motor.current_speed_rad_s;
    pthread_mutex_unlock(&g_right_motor.lock);
    return speed;
}

void motor_control_stop(void) {
    if (!g_initialized) return;
    motor_control_set_left_speed(0.0);
    motor_control_set_right_speed(0.0);
}


// --- Fin du fichier ---
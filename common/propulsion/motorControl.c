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

typedef struct {
    const uint16_t motor_id;      // ID constant du moteur (MRPIZ_MOTOR_LEFT ou MRPIZ_MOTOR_RIGHT)
    float target_speed_rad_s;     // Vitesse cible en rad/s
    float current_speed_rad_s;    // Vitesse actuelle en rad/s
    float error;                  // Erreur actuelle
    float last_error;             // Erreur précédente
    float integral_error;         // Erreur intégrale pour correction PI
    int encoder_last;
    uint64_t last_update_ms;
    xOsMutexCtx mutex;
    bool running;
    xOsTaskCtx task;
} motor_regulator_t;

// Initialisation des structures avec les IDs constants
static motor_regulator_t g_left_motor = {
    .motor_id = MRPIZ_MOTOR_LEFT,
    .target_speed_rad_s = 0.0f,
    .current_speed_rad_s = 0.0f,
    .error = 0.0f,
    .last_error = 0.0f,
    .integral_error = 0.0f,
    .encoder_last = 0,
    .last_update_ms = 0,
    .running = false
};

static motor_regulator_t g_right_motor = {
    .motor_id = MRPIZ_MOTOR_RIGHT,
    .target_speed_rad_s = 0.0f,
    .current_speed_rad_s = 0.0f,
    .error = 0.0f,
    .last_error = 0.0f,
    .integral_error = 0.0f,
    .encoder_last = 0,
    .last_update_ms = 0,
    .running = false
};

static bool g_initialized = false;

// Prototypes des fonctions statiques
static void* motor_regulator_task(void* arg);
static void motor_regulator_init(motor_regulator_t* reg);
static void motor_regulator_shutdown(motor_regulator_t* reg);
static bool is_valid_motor_id(uint16_t motor_id);
static float encoder_ticks_to_rad_per_sec(int delta_ticks, float dt);
static void update_motor_speed(motor_regulator_t* reg, int encoder_now, uint64_t now_ms);
static int calculate_speed_percentage(motor_regulator_t* reg);

// --- Fonctions internes ---

static float encoder_ticks_to_rad_per_sec(int delta_ticks, float dt) {
    if (dt <= 0.0f) return 0.0f;
    
    // Calcul de la vitesse angulaire en rad/s
    // 2π radians = ENCODER_TICKS_REV ticks
    float rad_per_tick = (2.0f * M_PI) / ENCODER_TICKS_REV;
    float speed_rad_s = (delta_ticks * rad_per_tick) / dt;
    
    return speed_rad_s;
}

static bool is_valid_motor_id(uint16_t motor_id) {
    return (motor_id == MRPIZ_MOTOR_LEFT || motor_id == MRPIZ_MOTOR_RIGHT);
}

static void update_motor_speed(motor_regulator_t* reg, int encoder_now, uint64_t now_ms) {
    float dt = (now_ms - reg->last_update_ms) / 1000.0f; // Convert to seconds
    if (dt <= 0.0f) return;

    // Calcul du delta d'encodeur
    int delta = encoder_now - reg->encoder_last;

    // Calcul de la vitesse actuelle
    float new_speed = encoder_ticks_to_rad_per_sec(delta, dt);
    
    // Mise à jour de la vitesse avec un filtre passe-bas simple
    //const float alpha = 0.3f; // Facteur de filtrage (0-1)
    reg->current_speed_rad_s = new_speed; //(alpha * new_speed) + ((1.0f - alpha) * reg->current_speed_rad_s);

    // Mise à jour des valeurs
    reg->encoder_last = encoder_now;
    reg->last_update_ms = now_ms;
}

static int calculate_speed_percentage(motor_regulator_t* reg) {
    // Conversion simple de la vitesse cible en pourcentage
    float pwm = (reg->target_speed_rad_s / MAX_SPEED_RAD_S) * 100.0f;
    
    // Saturation PWM
    if (pwm > 100.0f) pwm = 100.0f;
    if (pwm < -100.0f) pwm = -100.0f;

    return (int)pwm;
}

static void motor_regulator_init(motor_regulator_t* reg) {
    // Validation de l'ID du moteur
    if (!is_valid_motor_id(reg->motor_id)) {
        return;
    }
    
    // Initialisation des valeurs (sauf motor_id qui est constant)
    reg->target_speed_rad_s = 0.0f;
    reg->current_speed_rad_s = 0.0f;
    reg->error = 0.0f;
    reg->last_error = 0.0f;
    reg->integral_error = 0.0f;
    uint16_t tab[2] = {0, 0};
    GetMotorEncoderValues(tab);
    reg->encoder_last = tab[reg->motor_id];
    reg->last_update_ms = xTimerGetCurrentMs();
    reg->running = false;  // Ne pas démarrer avant que tout soit initialisé

    // Initialisation du mutex
    if (mutexCreate(&reg->mutex) != MUTEX_OK) {
        return;
    }

    // Initialisation de la tâche avec les paramètres par défaut
    osTaskInit(&reg->task);
    reg->task.t_ptTask = motor_regulator_task;
    reg->task.t_ptTaskArg = reg;

    // Création de la tâche
    int result = osTaskCreate(&reg->task);
    if (result != OS_TASK_SUCCESS) {
        mutexDestroy(&reg->mutex);
        return;
    }

    // Démarrer la tâche seulement après une initialisation réussie
    reg->running = true;
}

static void motor_regulator_shutdown(motor_regulator_t* reg) {
    if (!reg) return;
    
    reg->running = false;
    osTaskStop(&reg->task, 2); // Attendre max 2 secondes
    osTaskWait(&reg->task, NULL);
    mutexDestroy(&reg->mutex);
}

static void* motor_regulator_task(void* arg) {
    motor_regulator_t* reg = (motor_regulator_t*)arg;
    if (!reg) {
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    // Vérification de l'ID du moteur
    if (!is_valid_motor_id(reg->motor_id)) {
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    // Attendre un peu avant de commencer la régulation
    xTimerDelay(100);
    
    while (reg->running) {
        mutexLock(&reg->mutex);

        // Vérification de l'ID du moteur avant chaque lecture
        if (!is_valid_motor_id(reg->motor_id)) {
            mutexUnlock(&reg->mutex);
            xTimerDelay(REGULATION_PERIOD_MS);
            continue;
        }

        // Lecture de l'encodeur avec une copie temporaire de l'ID
        uint16_t tab[2] = {0, 0};
        GetMotorEncoderValues(tab);
        int encoder_now = tab[reg->motor_id];
        uint64_t now_ms = xTimerGetCurrentMs();

        // Mise à jour de la vitesse et calcul des erreurs
        update_motor_speed(reg, encoder_now, now_ms);

        // Calcul de la commande Speed percentage
        int speed_percentage = calculate_speed_percentage(reg);

        // Appliquer la commande Speed percentage
        SetMotorSpeed(reg->motor_id, speed_percentage);

        mutexUnlock(&reg->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }
    
    // Arrêt moteur à la sortie
    SetMotorSpeed(reg->motor_id, 0);
    return (void*)OS_TASK_EXIT_SUCCESS;
}

// --- Fonctions publiques ---

int motor_control_init(void) {
    if (g_initialized) {
        return 0;
    }

    // Initialisation des régulateurs avec les IDs constants
    motor_regulator_init(&g_right_motor);
    xTimerDelay(100);  // Attendre un peu entre les initialisations
    motor_regulator_init(&g_left_motor);
    g_initialized = true;
    return 0;
}

void motor_control_shutdown(void) {
    if (!g_initialized) {
        return;
    }
    
    // Arrêt des deux régulateurs
    motor_regulator_shutdown(&g_left_motor);
    motor_regulator_shutdown(&g_right_motor);
    
    g_initialized = false;
}

int motor_control_set_speed(uint16_t motor_id, double speed_rad_s) {
    if (!g_initialized) {
        return -1;
    }

    if (!is_valid_motor_id(motor_id)) {
        return -1;
    }

    // Limitation de la vitesse à la vitesse maximale
    if (speed_rad_s > MAX_SPEED_RAD_S) speed_rad_s = MAX_SPEED_RAD_S;
    if (speed_rad_s < -MAX_SPEED_RAD_S) speed_rad_s = -MAX_SPEED_RAD_S;

    motor_regulator_t* reg = (motor_id == MRPIZ_MOTOR_LEFT) ? &g_left_motor : &g_right_motor;
    
    mutexLock(&reg->mutex);
    reg->target_speed_rad_s = (float)speed_rad_s;
    mutexUnlock(&reg->mutex);
    
    return 0;
}

int motor_control_set_left_speed(double speed_rad_s) {
    return motor_control_set_speed(MRPIZ_MOTOR_LEFT, speed_rad_s);
}

int motor_control_set_right_speed(double speed_rad_s) {
    return motor_control_set_speed(MRPIZ_MOTOR_RIGHT, speed_rad_s);
}

double motor_control_get_left_speed(void) {
    if (!g_initialized) {
        return 0.0;
    }

    double speed = 0.0;
    mutexLock(&g_left_motor.mutex);
    speed = g_left_motor.current_speed_rad_s;
    mutexUnlock(&g_left_motor.mutex);
    
    return speed;
}

double motor_control_get_right_speed(void) {
    if (!g_initialized) {
        return 0.0;
    }

    double speed = 0.0;
    mutexLock(&g_right_motor.mutex);
    speed = g_right_motor.current_speed_rad_s;
    mutexUnlock(&g_right_motor.mutex);
    
    return speed;
}

int motor_control_stop(void) {
    if (!g_initialized) return -1;
    motor_control_set_left_speed(0.0);
    motor_control_set_right_speed(0.0);
    return 0;
}

// --- Fin du fichier ---
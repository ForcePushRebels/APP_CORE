#include "positionControl.h"
#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xLog.h"
#include <math.h>
#include <string.h>

// Définition de la marge de correction en ticks
#define CORRECTION_MARGIN_TICKS 2

// Activer/désactiver la correction du débordement des encodeurs
#define CORRECTION 1

// Variable de position globale
static Position_t g_robot_position = {0, 0, 0.0f};
static xOsMutexCtx g_position_mutex;

// Structure de contrôle de position pour chaque roue
typedef struct 
{
    int32_t current_ticks;      // Position actuelle de l'encodeur en ticks
    int32_t target_ticks;       // Position cible de l'encodeur en ticks
    double current_speed;       // Vitesse actuelle en rad/s
    double target_speed;        // Vitesse cible en rad/s
    double max_speed;          // Vitesse maximale pour ce mouvement
    bool motion_finished;       // Drapeau indiquant si le mouvement est terminé
    bool should_stop;          // Drapeau pour demander l'arrêt du mouvement
    xOsMutexCtx mutex;         // Mutex pour la sécurité des threads
    xOsTaskCtx task;           // Contexte de la tâche
    bool running;              // Drapeau d'exécution de la tâche
    bool is_left_wheel;        // Drapeau pour indiquer si c'est la roue gauche
} wheel_position_control_t;

typedef enum {
    WAIT,
    MOVE_INIT,
    ACC,
    MOVE,
    DECC,
    CORR,
    STOP
} StateMove;

// Variables globales
static wheel_position_control_t g_left_wheel;
static wheel_position_control_t g_right_wheel;
static bool g_initialized = false;
static double g_common_target_speed = 0.0;  // Vitesse cible commune
static xOsMutexCtx g_speed_mutex;          // Mutex pour la vitesse commune
static move_type_t g_current_move_type;    // Type de mouvement en cours
StateMove g_state = WAIT;


// Prototypes de fonctions statiques
static void* wheel_position_control_task(void* arg);
static void wheel_position_control_init(wheel_position_control_t* control, bool is_left);
static void wheel_position_control_shutdown(wheel_position_control_t* control);


// Fonction utilitaire pour convertir la distance en ticks d'encodeur
static int32_t distance_to_ticks(double distance_mm) 
{
    int32_t ticks = (int32_t)((distance_mm / 10.0) / (float)(WHEEL_PERIMETER_CM/ENCODER_TICKS_REV));
    return ticks;
}

// Fonction pour calculer la distance nécessaire pour une rampe de vitesse
static int32_t calculate_ramp_tick(double target_speed, double acceleration)
{
    // Formule : distance = (vitesse_finale² - vitesse_initiale²) / (2 * accélération)
    // Comme vitesse_initiale = 0, on simplifie à : distance = vitesse_finale² / (2 * accélération)
    double distance_rad = (target_speed * target_speed) / (2.0 * acceleration);
    
    // Conversion distance en rad vers cm
    // distance_cm = distance_rad * rayon_roue
    double distance_mm = distance_rad * (WHEEL_DIAMETER_CM / 2.0) * 10;
    
    return distance_to_ticks(distance_mm);
}

// Fonction utilitaire pour convertir l'angle en ticks d'encodeur
static int32_t angle_to_ticks(double angle_rad) 
{
    // Calculer la rotation de roue pour l'angle souhaité
    // Pour une propulsion différentielle, la rotation de roue est:
    // rotation_roue = (angle * distance_roues) / rayon_roue
    double wheel_rotation_rad = ((WHEEL_DISTANCE_CM/2.0) * angle_rad)*0.962;
    
    // Convertir les radians en ticks d'encodeur
    // Multiplier par 10 pour corriger l'échelle
    int32_t ticks = (int32_t)((wheel_rotation_rad) / (WHEEL_PERIMETER_CM/ENCODER_TICKS_REV));
    
    return ticks;
}

// --- Fonctions internes ---

// Fonction utilitaire pour mettre à jour la position du robot
static void update_robot_position(int32_t left_ticks, int32_t right_ticks) 
{
    mutexLock(&g_position_mutex);
    static int32_t prev_left_ticks = 0;
    static int32_t prev_right_ticks = 0;
    int32_t delta_left_ticks = left_ticks - prev_left_ticks;
    int32_t delta_right_ticks = right_ticks - prev_right_ticks;
    prev_left_ticks = left_ticks;
    prev_right_ticks = right_ticks;
    
    // Calculer la variation d'angle en radians
    // 1 tick = 0.21 degrés = 0.00366 radians
    double delta_angle = (delta_right_ticks * -ANGLE_PER_TICK_RAD) + (delta_left_ticks * ANGLE_PER_TICK_RAD);
    
    // Calculer la distance parcourue en cm
    // 1 tick = 0.03 cm
    double center_distance = ((delta_left_ticks + delta_right_ticks) * 0.039) / 2.0;
    
    // Mettre à jour l'angle absolu
    g_robot_position.angle_rad += delta_angle;
    
    // Normaliser l'angle à [-π, π]
    while (g_robot_position.angle_rad > M_PI) g_robot_position.angle_rad -= 2.0 * M_PI;
    while (g_robot_position.angle_rad < -M_PI) g_robot_position.angle_rad += 2.0 * M_PI;
    
    // Mettre à jour les coordonnées x et y
    // Utiliser l'angle moyen pendant le mouvement pour plus de précision
    double avg_angle = g_robot_position.angle_rad - (delta_angle / 2.0);
    double dx = center_distance * 10.0 * cos(avg_angle); // Calcul en double
    double dy = center_distance * 10.0 * sin(avg_angle); // Calcul en double
    
    // Accumuler les changements de position directement en double
    g_robot_position.x_mm += dx;
    g_robot_position.y_mm += dy;
    
    X_LOG_TRACE("Position mise à jour - left_ticks: %d, right_ticks: %d",
                left_ticks, right_ticks);

    X_LOG_TRACE("Position mise à jour - X: %.2f mm, Y: %.2f mm, Angle: %.2f rad",
                g_robot_position.x_mm, g_robot_position.y_mm, g_robot_position.angle_rad);
    
    X_LOG_TRACE("Position mise à jour - delta_left_ticks: %d, delta_right_ticks: %d, delta_angle: %.4f rad",
                delta_left_ticks, delta_right_ticks, delta_angle);
    
    mutexUnlock(&g_position_mutex);
}


static void* wheel_position_control_task(void* arg) 
{
    wheel_position_control_t* control = (wheel_position_control_t*)arg;
    if (!control) 
    {
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    static double step_acc = ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0, step_decc = DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;
    double right_wheel_speed = 0.0, left_wheel_speed = 0.0;
    int32_t nb_decc_tick = 0, nb_acc_tick = 0;
    
    // Variables pour stocker les valeurs précédentes des encodeurs
    static int32_t prev_left_ticks = 0;
    static int32_t prev_right_ticks = 0;
    static bool first_run = true;

    while (control->running) 
    {
        mutexLock(&control->mutex);

        // Obtenir les positions actuelles des encodeurs
        int32_t encoder_values[2] = {0, 0};
        GetMotorEncoderValues(encoder_values);
        
        // Calculer les variations des encodeurs
        int32_t delta_left_ticks = encoder_values[0] - prev_left_ticks;
        int32_t delta_right_ticks = encoder_values[1] - prev_right_ticks;
        
        // Mettre à jour les valeurs précédentes
        prev_left_ticks = encoder_values[0];
        prev_right_ticks = encoder_values[1];
        
        // Mettre à jour la position du robot si ce n'est pas le premier passage
        if (!first_run) {
            update_robot_position(delta_left_ticks, delta_right_ticks);
        }
        first_run = false;
        
        X_LOG_TRACE("Position mise à jour - X: %d mm, Y: %d mm, Angle: %.2f rad",
                g_robot_position.x_mm, g_robot_position.y_mm, g_robot_position.angle_rad);

        // Mettre à jour les ticks actuels
        g_left_wheel.current_ticks = encoder_values[0];
        g_right_wheel.current_ticks = encoder_values[1];
        
        // Calculer la distance restante en ticks
        int32_t remaining_ticks = control->target_ticks - control->current_ticks;
        int32_t abs_remaining = abs(remaining_ticks);

        if (control->should_stop) 
        {
            motor_control_stop();
            control->motion_finished = true;
            control->should_stop = false;
            mutexUnlock(&control->mutex);
            continue;
        }

        switch (g_state)
        {
            case WAIT :
                if(!g_left_wheel.motion_finished && !g_right_wheel.motion_finished)
                {
                    g_state = MOVE_INIT;
                }
                break; 
                
            case MOVE_INIT :
                // Variables pour le contrôle de la phase d'accélération
                step_acc = ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;
                step_decc = DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;

                right_wheel_speed = 0.0; 
                left_wheel_speed = 0.0;
                
                // Calculer le nombre de ticks pendant la rampe de décélération
                nb_decc_tick = calculate_ramp_tick(control->max_speed, DECELERATION_COEF);
                nb_acc_tick = calculate_ramp_tick(control->max_speed, ACCELERATION_COEF);

                if ((control->target_ticks - control->current_ticks) < (nb_acc_tick + nb_decc_tick))
                {
                    int32_t nb_tt_tick = control->target_ticks - control->current_ticks;
                    nb_acc_tick = nb_tt_tick / 2.0;
                    nb_decc_tick =  nb_tt_tick / 2.0;
                }
                g_state = ACC;
                break; 

            case ACC :
                mutexLock(&g_speed_mutex);
                g_common_target_speed += step_acc;
                
                if (g_common_target_speed >= control->max_speed) {
                    g_common_target_speed = control->max_speed;
                    g_state = MOVE;
                }
                mutexUnlock(&g_speed_mutex);

                // Vérifier si on a parcouru la moitié des ticks
                if (abs(control->target_ticks - nb_decc_tick) <= abs(control->current_ticks)) {
                    g_state = DECC;
                }
                break; 

            case MOVE :
                if (abs(control->current_ticks) >= abs(control->target_ticks - nb_decc_tick))
                {
                    g_state = DECC;
                }
                break; 

            case DECC :
                mutexLock(&g_speed_mutex);
                g_common_target_speed -= step_decc;
                if (g_common_target_speed < MIN_SPEED_RAD_S) {
                    g_common_target_speed = 0.0;
                    g_state = CORR;
                }
                mutexUnlock(&g_speed_mutex);
                break; 

            case CORR :
                // Calculer la distance restante
                remaining_ticks = control->target_ticks - control->current_ticks;
                abs_remaining = abs(remaining_ticks);

                if (abs_remaining > CORRECTION_MARGIN_TICKS && CORRECTION) {
                    // Appliquer une petite vitesse de correction
                    double correction = (remaining_ticks > 0) ? CORRECTION_SPEED : -CORRECTION_SPEED;
                    switch (g_current_move_type) {
                    case FORWARD:
                        // Appliquer la correction à la roue appropriée
                        if (control->is_left_wheel) {
                            motor_control_set_left_speed(correction);
                        } else {
                            motor_control_set_right_speed(correction);
                        }
                        break;
                        
                    case LEFT:
                        if (control->is_left_wheel && !control->motion_finished) {
                            motor_control_set_left_speed(correction);
                        } 
                        else if (!control->motion_finished)
                        {
                            motor_control_set_right_speed(correction);
                        }
                        break;
                        
                    case RIGHT:
                        if (control->is_left_wheel && !control->motion_finished) {
                            motor_control_set_left_speed(correction);
                        }
                        else if (!control->motion_finished)
                        {
                            motor_control_set_right_speed(correction);
                        }
                        break;
                    }
                    control->motion_finished = false;
                } 
                
                if (!(abs_remaining > CORRECTION_MARGIN_TICKS) && CORRECTION) {
                    if (control->is_left_wheel && !control->motion_finished) {
                        motor_control_set_left_speed(0.0);
                    } else if (!control->motion_finished){
                        motor_control_set_right_speed(0.0);
                    }
                    control->motion_finished = true;
                }

                if(g_left_wheel.motion_finished && g_right_wheel.motion_finished)
                {
                    g_state = WAIT;
                }
                break; 

            case STOP:
                if (control->is_left_wheel) {
                    motor_control_set_left_speed(0.0);
                } else {
                    motor_control_set_right_speed(0.0);
                }
                control->motion_finished = true;

                if(g_left_wheel.motion_finished && g_right_wheel.motion_finished)
                {
                    g_state = WAIT;
                }
                break; 
        }

        // Appliquer la vitesse au moteur avec le signe approprié selon le type de mouvement
        if (g_state != CORR && g_state != STOP ) {  // Ne pas appliquer la vitesse pendant la phase de correction
            double wheel_speed = g_common_target_speed; 
            
            switch (g_current_move_type) {
                case FORWARD:
                    right_wheel_speed = wheel_speed;
                    left_wheel_speed = wheel_speed;
                    break;
                    
                case LEFT:
                    right_wheel_speed = wheel_speed;
                    left_wheel_speed = -wheel_speed;
                    break;
                    
                case RIGHT:
                    right_wheel_speed = -wheel_speed;
                    left_wheel_speed = wheel_speed;
                    break;
            }
            motor_control_set_left_speed(left_wheel_speed);
            motor_control_set_right_speed(right_wheel_speed);
        }
        
        mutexUnlock(&control->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }

    return (void*)OS_TASK_EXIT_SUCCESS;
}

// --- Fonctions publiques ---

bool position_control_is_motion_finished(void)
{
    if (!g_initialized) return true;
    
    bool finished = false;
    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    finished = g_left_wheel.motion_finished && g_right_wheel.motion_finished;
    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return finished;
}

int16_t position_control_advance(int16_t distance_mm, float speed_rad_s_max) 
{
    if (!g_initialized) return -1;
    int32_t distance_patch = (int32_t)((float)distance_mm * 0.909);

    // Convertir la distance en ticks d'encodeur
    int32_t target_ticks = distance_to_ticks(distance_patch);
    
    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    
    // Obtenir les positions actuelles des encodeurs
    int32_t encoder_values[2] = {0, 0};
    GetMotorEncoderValues(encoder_values);
    
    // Définir les cibles pour les deux roues
    g_left_wheel.current_ticks = encoder_values[0];
    g_right_wheel.current_ticks = encoder_values[1];
    g_left_wheel.target_ticks = g_left_wheel.current_ticks + target_ticks;
    g_right_wheel.target_ticks = g_right_wheel.current_ticks + target_ticks;
    g_left_wheel.max_speed = speed_rad_s_max;
    g_right_wheel.max_speed = speed_rad_s_max;
    g_left_wheel.motion_finished = false;
    g_right_wheel.motion_finished = false;
    g_left_wheel.should_stop = false;
    g_right_wheel.should_stop = false;
    
    // Définir le type de mouvement
    g_current_move_type = FORWARD;
    
    X_LOG_TRACE("Avance: distance=%d mm, target_ticks=%d", distance_mm, target_ticks);
    
    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return 0;
}

int16_t position_control_turn(float angle_rad, float speed_rad_s_max) 
{
    if (!g_initialized) return -1;

    // Convertir l'angle en ticks d'encodeur
    int32_t target_ticks = angle_to_ticks(angle_rad);
    
    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    
    // Obtenir les positions actuelles des encodeurs
    int32_t encoder_values[2] = {0, 0};
    GetMotorEncoderValues(encoder_values);
    
    // Définir les cibles pour les deux roues
    g_left_wheel.current_ticks = encoder_values[0];
    g_right_wheel.current_ticks = encoder_values[1];

    g_left_wheel.target_ticks = g_left_wheel.current_ticks + target_ticks;
    g_right_wheel.target_ticks = g_right_wheel.current_ticks - target_ticks;
    // Si angle négatif = tourner à gauche
    // Si angle positif = tourner à droite
    if (angle_rad < 0) {
        // Tourner à gauche : roue gauche en arrière, roue droite en avant
        g_current_move_type = LEFT;
    } else {
        // Tourner à droite : roue gauche en avant, roue droite en arrière
        g_current_move_type = RIGHT;
    }
    
    g_left_wheel.max_speed = speed_rad_s_max;
    g_right_wheel.max_speed = speed_rad_s_max;
    g_left_wheel.motion_finished = false;
    g_right_wheel.motion_finished = false;
    g_left_wheel.should_stop = false;
    g_right_wheel.should_stop = false;
    
    X_LOG_TRACE("Rotation: angle=%.2f rad, target_ticks=%d, type=%s", 
                angle_rad, target_ticks, 
                g_current_move_type == LEFT ? "GAUCHE" : "DROITE");
    
    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return 0;
}

int16_t position_control_stop(void) 
{
    if (!g_initialized) return -1;

    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    
    g_left_wheel.should_stop = true;
    g_right_wheel.should_stop = true;
    
    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    
    return 0;
}

int16_t position_control_get_position(Position_t* position) 
{
    if (!position) return -1;
    
    mutexLock(&g_position_mutex);
    *position = g_robot_position;
    mutexUnlock(&g_position_mutex);
    
    return 0;
}

static void wheel_position_control_init(wheel_position_control_t* control, bool is_left) 
{
    if (!control) return;

    // Initialiser les valeurs
    control->current_ticks = 0;
    control->target_ticks = 0;
    control->current_speed = 0.0;
    control->target_speed = 0.0;
    control->max_speed = 0.0;
    control->motion_finished = true;
    control->should_stop = false;
    control->running = false;
    control->is_left_wheel = is_left;

    // Initialiser le mutex
    if (mutexCreate(&control->mutex) != MUTEX_OK) 
    {
        X_LOG_TRACE("Échec de la création du mutex de contrôle de position de roue");
        return;
    }

    // Initialiser la tâche
    osTaskInit(&control->task);
    control->task.t_ptTask = wheel_position_control_task;
    control->task.t_ptTaskArg = control;

    // Créer la tâche
    int result = osTaskCreate(&control->task);
    if (result != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Échec de la création de la tâche de contrôle de position de roue");
        mutexDestroy(&control->mutex);
        return;
    }

    control->running = true;
    X_LOG_TRACE("Tâche de contrôle de position de roue initialisée avec succès");
}

static void wheel_position_control_shutdown(wheel_position_control_t* control) 
{
    if (!control) return;

    control->running = false;
    osTaskStop(&control->task, 2); // Attendre max 2 secondes
    osTaskWait(&control->task, NULL);
    mutexDestroy(&control->mutex);
}

int16_t position_control_init(void) 
{
    if (g_initialized) 
    {
        X_LOG_TRACE("Contrôle de position déjà initialisé");
        return 0;
    }

    X_LOG_TRACE("Initialisation du contrôle de position");
    
    // Initialiser le mutex de position
    if (mutexCreate(&g_position_mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Échec de la création du mutex de position");
        return -1;
    }

    // Initialiser le mutex de vitesse
    if (mutexCreate(&g_speed_mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Échec de la création du mutex de vitesse");
        mutexDestroy(&g_position_mutex);
        return -1;
    }
    
    // Initialiser d'abord le contrôle moteur
    int16_t motor_init_result = motor_control_init();
    if (motor_init_result != 0)
    {
        X_LOG_TRACE("Échec de l'initialisation du contrôle moteur: %d", motor_init_result);
        mutexDestroy(&g_position_mutex);
        mutexDestroy(&g_speed_mutex);
        return -1;
    }
    X_LOG_TRACE("Contrôle moteur initialisé avec succès");

    // Initialiser les deux contrôleurs de roue
    wheel_position_control_init(&g_left_wheel, true);
    wheel_position_control_init(&g_right_wheel, false);
    
    g_initialized = true;
    X_LOG_TRACE("Contrôle de position initialisé avec succès");
    return 0;
}

int16_t position_control_shutdown(void) 
{
    if (!g_initialized) 
    {
        X_LOG_TRACE("Contrôle de position non initialisé, rien à arrêter");
        return 0;
    }

    X_LOG_TRACE("Arrêt du contrôle de position");
    
    // Arrêter les contrôleurs de roue
    wheel_position_control_shutdown(&g_left_wheel);
    wheel_position_control_shutdown(&g_right_wheel);
    
    // Arrêter le contrôle moteur
    motor_control_shutdown();
    X_LOG_TRACE("Arrêt du contrôle moteur terminé");
    
    // Nettoyer les mutex
    mutexDestroy(&g_position_mutex);
    mutexDestroy(&g_speed_mutex);
    
    g_initialized = false;
    X_LOG_TRACE("Arrêt du contrôle de position terminé");
    return 0;
}

// --- Fin du fichier ---

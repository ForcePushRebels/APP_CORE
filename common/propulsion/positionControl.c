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
#define CORRECTION_MARGIN_TICKS 0

// Activer/désactiver la correction du débordement des encodeurs
#define CORRECTION 1
bool g_bCorrection = true;

// Variable de position globale
static Position_t g_robot_position = {700, 700, 0.0};
static xOsMutexCtx g_position_mutex;

// Structure de contrôle de position pour chaque roue
typedef struct
{
    int32_t current_ticks; // Position actuelle de l'encodeur en ticks
    int32_t target_ticks;  // Position cible de l'encodeur en ticks
    double current_speed;  // Vitesse actuelle en rad/s
    double target_speed;   // Vitesse cible en rad/s
    double max_speed;      // Vitesse maximale pour ce mouvement
    bool motion_finished;  // Drapeau indiquant si le mouvement est terminé
    bool should_stop;      // Drapeau pour demander l'arrêt du mouvement
    xOsMutexCtx mutex;     // Mutex pour la sécurité des threads
    xOsTaskCtx task;       // Contexte de la tâche
    bool running;          // Drapeau d'exécution de la tâche
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
static double g_common_target_speed = 0.0; // Vitesse cible commune
static xOsMutexCtx g_speed_mutex;          // Mutex pour la vitesse commune
static move_type_t g_current_move_type;    // Type de mouvement en cours
StateMove g_state = WAIT;



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
    // Calculer la variation d'angle en radians
    // 1 tick = 0.21 degrés = 0.00366 radians
    double delta_angle = (right_ticks * -ANGLE_PER_TICK_RAD) + (left_ticks * ANGLE_PER_TICK_RAD);
    
    // Calculer la distance parcourue en cm
    // 1 tick = 0.028 cm
    double center_distance = ((abs(left_ticks) + abs(right_ticks)) * 0.028) / 2.0;
    
    // Déterminer le sens du mouvement
    if (left_ticks < 0 || right_ticks < 0) {
        center_distance = -center_distance;
    }
    
    mutexLock(&g_position_mutex);
    
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
    if (g_current_move_type == FORWARD) {
        g_robot_position.x_mm += dx;
        g_robot_position.y_mm += dy;
    }
    
    // X_LOG_TRACE("Position mise à jour - left_ticks: %d, right_ticks: %d",
    //             left_ticks, right_ticks);

    //X_LOG_TRACE("Position mise à jour - X: %.2f mm, Y: %.2f mm, Angle: %.2f°",
    //             g_robot_position.x_mm, g_robot_position.y_mm, g_robot_position.angle_rad * 180.0 / M_PI);
    
    mutexUnlock(&g_position_mutex);
}


static void* wheel_position_control_task(void* arg) 
{
    (void)arg;

    X_LOG_TRACE("Position control task started");
    
    double step_acc = ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0, step_decc = DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;
    double right_wheel_speed = 0.0, left_wheel_speed = 0.0;
    int32_t nb_decc_tick = 0;
    
    // Variables pour stocker les valeurs précédentes des encodeurs  
    int32_t encoder_values[2] = {0, 0};
    static int32_t prev_left_ticks = 0;
    static int32_t prev_right_ticks = 0;
    static bool first_run = true;

    GetMotorEncoderValues(encoder_values);
    prev_left_ticks = encoder_values[0];
    prev_right_ticks = encoder_values[1];
    

    while (1)//(atomic_load(&self->a_iStopFlag) == OS_TASK_SECURE_FLAG)//(control->running) 
    {

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
            mutexLock(&g_position_mutex);
            update_robot_position(delta_left_ticks, delta_right_ticks);
            mutexUnlock(&g_position_mutex);
        }
        first_run = false;
        
        // Mettre à jour les ticks actuels
        mutexLock(&g_left_wheel.mutex);
        mutexLock(&g_right_wheel.mutex);

        g_left_wheel.current_ticks = encoder_values[0];
        g_right_wheel.current_ticks = encoder_values[1];

        // Afficher la position du robot

        
        // Calculer la distance restante en ticks
        //int32_t remaining_ticks = control->target_ticks - control->current_ticks;
        //int32_t abs_remaining = abs(remaining_ticks);

        if (g_left_wheel.should_stop && g_right_wheel.should_stop) 
        {
            /*X_LOG_TRACE("Position control stop requested - Left: %d, Right: %d", 
                g_left_wheel.should_stop, g_right_wheel.should_stop);*/
            g_left_wheel.should_stop = false;
            g_right_wheel.should_stop = false;
            g_state = DECC;
            step_decc *=2;
            g_bCorrection = false;
            mutexUnlock(&g_right_wheel.mutex);
            mutexUnlock(&g_left_wheel.mutex);
            continue;
        }

        switch (g_state)
        {
            case WAIT :
                //X_LOG_TRACE("État: WAIT - Left_finished: %d - Right_finished: %d", 
                //    g_left_wheel.motion_finished,
                //    g_right_wheel.motion_finished);

                if(!g_left_wheel.motion_finished && !g_right_wheel.motion_finished)
                {
                    g_state = MOVE_INIT;
                }
                break; 
                
            case MOVE_INIT :
                // Variables pour le contrôle de la phase d'accélération

                right_wheel_speed = 0.0; 
                left_wheel_speed = 0.0;

                g_bCorrection = true;

                X_ASSERT(g_left_wheel.max_speed == g_right_wheel.max_speed);
                
                // Calculer le nombre de ticks pendant la rampe de décélération
                nb_decc_tick = calculate_ramp_tick(g_left_wheel.max_speed, DECELERATION_COEF);

                step_decc = DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;

                g_state = ACC;
                break; 

            case ACC :
                /*
                X_LOG_TRACE("État: ACC - left target_ticks: %d - current_ticks: %d - nb_decc_tick: %d && right target_ticks: %d - current_ticks: %d - nb_decc_tick: %d",
                    g_left_wheel.target_ticks, g_left_wheel.current_ticks, nb_decc_tick,
                    g_right_wheel.target_ticks, g_right_wheel.current_ticks, nb_decc_tick);*/

                mutexLock(&g_speed_mutex);
                g_common_target_speed += step_acc;
                if (g_common_target_speed >= g_left_wheel.max_speed) {
                    g_common_target_speed = g_left_wheel.max_speed;
                    g_state = MOVE;
                }
                mutexUnlock(&g_speed_mutex);

                // Vérifier si on a parcouru la moitié des ticks
                if (abs(g_left_wheel.target_ticks - g_left_wheel.current_ticks) <= abs(nb_decc_tick))
                {
                    g_state = DECC;
                }
                break; 

            case MOVE :
                /*
                X_LOG_TRACE("État: MOVE");
                X_LOG_TRACE("État: MOVE - target_ticks: %d - current_ticks: %d - nb_decc_tick: %d",
                    g_left_wheel.target_ticks, g_left_wheel.current_ticks, nb_decc_tick);
                X_LOG_TRACE("État: MOVE - target_ticks: %d - current_ticks: %d - nb_decc_tick: %d",
                    g_right_wheel.target_ticks, g_right_wheel.current_ticks, nb_decc_tick);*/

                //X_LOG_TRACE("État: MOVE - Roue: %s", g_left_wheel.is_left_wheel ? "GAUCHE" : "DROITE");

                if (abs(g_left_wheel.target_ticks - g_left_wheel.current_ticks) <= abs(nb_decc_tick))
                {
                    g_state = DECC;
                }
                break; 

            case DECC :
                //X_LOG_TRACE("État: DECC");

                //X_LOG_TRACE("État: DECC - Roue: %s", g_left_wheel.is_left_wheel ? "GAUCHE" : "DROITE");
                mutexLock(&g_speed_mutex);
                g_common_target_speed -= step_decc;
                if (g_common_target_speed < MIN_SPEED_RAD_S) 
                {
                    g_common_target_speed = 0.0;

                    if (g_bCorrection)
                    {
                        g_state = CORR;
                    }
                    else
                    {
                        g_state = STOP;
                    }

                }
                mutexUnlock(&g_speed_mutex);
                break; 

            case CORR :
                //X_LOG_TRACE("État: CORR");

                if (abs(g_left_wheel.current_ticks - g_left_wheel.target_ticks) > CORRECTION_MARGIN_TICKS && !g_left_wheel.motion_finished) 
                {
                    // Appliquer une petite vitesse de correction
                    double correction = (g_left_wheel.current_ticks - g_left_wheel.target_ticks > 0) ? -CORRECTION_SPEED : CORRECTION_SPEED;
                    motor_control_set_left_speed(correction);
                } 
                else if (!(abs(g_left_wheel.current_ticks - g_left_wheel.target_ticks) > CORRECTION_MARGIN_TICKS) && !g_left_wheel.motion_finished) 
                {
                    // Si la correction est terminée, arrêter la roue gauche
                    motor_control_set_left_speed(0.0);
                    g_left_wheel.motion_finished = true;
                }

                if (abs(g_right_wheel.current_ticks - g_right_wheel.target_ticks) > CORRECTION_MARGIN_TICKS && !g_right_wheel.motion_finished) 
                {
                    // Appliquer une petite vitesse de correction
                    double correction = (g_right_wheel.current_ticks - g_right_wheel.target_ticks > 0) ? -CORRECTION_SPEED : CORRECTION_SPEED;
                    motor_control_set_right_speed(correction);
                } 
                else if (!(abs(g_right_wheel.current_ticks - g_right_wheel.target_ticks) > CORRECTION_MARGIN_TICKS) && !g_right_wheel.motion_finished) 
                {
                    // Si la correction est terminée, arrêter la roue droite
                    motor_control_set_right_speed(0.0);
                    g_right_wheel.motion_finished = true;
                }

                if(g_left_wheel.motion_finished && g_right_wheel.motion_finished)
                {
                    g_state = WAIT;
                }
                break; 

            case STOP:
                //X_LOG_TRACE("État: STOP");

                //X_LOG_TRACE("État: STOP - Roue: %s", control->is_left_wheel ? "GAUCHE" : "DROITE");

                if(g_left_wheel.motion_finished && g_right_wheel.motion_finished)
                {
                    g_state = WAIT;
                }
                else if (!g_left_wheel.motion_finished)
                {
                    // Si la roue gauche n'est pas terminée, arrêter la vitesse de la roue gauche
                    motor_control_set_left_speed(0.0);
                    g_left_wheel.motion_finished = true;
                    g_left_wheel.target_ticks = g_left_wheel.current_ticks;
                }
                else if (!g_right_wheel.motion_finished)
                {
                    // Si la roue droite n'est pas terminée, arrêter la vitesse de la roue droite
                    motor_control_set_right_speed(0.0);
                    g_right_wheel.motion_finished = true;
                    g_right_wheel.target_ticks = g_right_wheel.current_ticks;
                }

                break; 
        }

        // Appliquer la vitesse au moteur avec le signe approprié selon le type de mouvement
        if (g_state != CORR && g_state != STOP ) {  // Ne pas appliquer la vitesse pendant la phase de correction
            mutexLock(&g_speed_mutex);
            double wheel_speed = g_common_target_speed;
            mutexUnlock(&g_speed_mutex);
            
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

        //TODO reset encoder values after limit reached
        

        mutexUnlock(&g_left_wheel.mutex);
        mutexUnlock(&g_right_wheel.mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }

    return (void*)OS_TASK_EXIT_SUCCESS;
}

static void wheel_position_control_init(wheel_position_control_t* control) 
{
    if (!control)
        return;

    // Initialiser les valeurs
    control->current_ticks = 0;
    control->target_ticks = 0;
    control->current_speed = 0.0;
    control->target_speed = 0.0;
    control->max_speed = 0.0;
    control->motion_finished = true;
    control->should_stop = false;
    control->running = false;

    // Initialiser le mutex
    if (mutexCreate(&control->mutex) != MUTEX_OK)
    {
        // X_LOG_TRACE("Échec de la création du mutex de contrôle de position de roue");
        return;
    }

    // Initialiser la tâche
    osTaskInit(&control->task);
    control->task.t_ptTask = wheel_position_control_task;

    // Créer la tâche
    int result = osTaskCreate(&control->task);
    if (result != OS_TASK_SUCCESS)
    {
        // X_LOG_TRACE("Échec de la création de la tâche de contrôle de position de roue");
        mutexDestroy(&control->mutex);
        return;
    }

    control->running = true;
    // X_LOG_TRACE("Tâche de contrôle de position de roue initialisée avec succès");
}

static void wheel_position_control_shutdown(wheel_position_control_t *control)
{
    if (!control)
        return;

    control->running = false;
    osTaskStop(&control->task, 2); // Attendre max 2 secondes
    osTaskWait(&control->task, NULL);
    mutexDestroy(&control->mutex);
}


// --- Fonctions publiques ---

bool position_control_is_motion_finished(void)
{
    if (!g_initialized) return false;

    bool finished = false;
    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    finished = g_left_wheel.motion_finished && g_right_wheel.motion_finished && (g_state == WAIT);
    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return finished;
}

int32_t position_control_advance(int32_t distance_mm, float speed_rad_s_max)
{
    if (!g_initialized) return POSITION_NOT_INITIALIZED;
    int32_t distance_patch = (int32_t)((double)distance_mm * 0.909);

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

    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return POSITION_OK;
}

int32_t position_control_turn(float angle_rad, float speed_rad_s_max)
{
    if (!g_initialized)
        return POSITION_NOT_INITIALIZED;

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

    g_left_wheel.target_ticks = g_left_wheel.current_ticks - target_ticks;
    g_right_wheel.target_ticks = g_right_wheel.current_ticks + target_ticks;
    // Si angle négatif = tourner à gauche
    // Si angle positif = tourner à droite
    if (angle_rad > 0)
    {
        // Tourner à gauche : roue gauche en arrière, roue droite en avant
        g_current_move_type = LEFT;
    }
    else
    {
        // Tourner à droite : roue gauche en avant, roue droite en arrière
        g_current_move_type = RIGHT;
    }

    g_left_wheel.max_speed = speed_rad_s_max;
    g_right_wheel.max_speed = speed_rad_s_max;
    g_left_wheel.motion_finished = false;
    g_right_wheel.motion_finished = false;
    g_left_wheel.should_stop = false;
    g_right_wheel.should_stop = false;

    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return POSITION_OK;
}

int32_t position_control_stop(void)
{
    if (!g_initialized) return POSITION_NOT_INITIALIZED;

    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);

    g_left_wheel.should_stop = true;
    g_right_wheel.should_stop = true;

    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    
    return POSITION_OK;
}

int32_t position_control_get_position(Position_t *position)
{
    if (!position) return POSITION_INVALID_PARAM;
    
    mutexLock(&g_position_mutex);
    *position = g_robot_position;
    mutexUnlock(&g_position_mutex);

    return POSITION_OK;
}

int32_t position_control_init(void)
{
    if (g_initialized)
    {
        return POSITION_OK;
    }

    // Initialiser le mutex de position
    if (mutexCreate(&g_position_mutex) != MUTEX_OK)
    {
        return POSITION_MUTEX_ERROR;
    }

    // Initialiser le mutex de vitesse
    if (mutexCreate(&g_speed_mutex) != MUTEX_OK)
    {
        mutexDestroy(&g_position_mutex);
        return POSITION_MUTEX_ERROR;
    }

    // Initialiser d'abord le contrôle moteur
    int16_t motor_init_result = motor_control_init();
    if (motor_init_result != 0)
    {
        mutexDestroy(&g_position_mutex);
        mutexDestroy(&g_speed_mutex);
        return POSITION_MOTOR_ERROR;
    }

    // Initialiser les deux contrôleurs de roue
    wheel_position_control_init(&g_left_wheel);
    wheel_position_control_init(&g_right_wheel);
    
    g_initialized = true;
    return POSITION_OK;
}

int32_t position_control_shutdown(void)
{
    if (!g_initialized)
    {
        // X_LOG_TRACE("Contrôle de position non initialisé, rien à arrêter");
        return 0;
    }

    // X_LOG_TRACE("Arrêt du contrôle de position");

    // Arrêter les contrôleurs de roue
    wheel_position_control_shutdown(&g_left_wheel);
    wheel_position_control_shutdown(&g_right_wheel);

    // Arrêter le contrôle moteur
    motor_control_shutdown();
    // X_LOG_TRACE("Arrêt du contrôle moteur terminé");

    // Nettoyer les mutex
    mutexDestroy(&g_position_mutex);
    mutexDestroy(&g_speed_mutex);

    g_initialized = false;
    // X_LOG_TRACE("Arrêt du contrôle de position terminé");
    return 0;
}

// --- Fin du fichier ---

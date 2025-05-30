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
#define CORRECTION_MARGIN_TICKS 5

// Activer/désactiver la correction du débordement des encodeurs
#define CORRECTION 0

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

// Variables globales
static wheel_position_control_t g_left_wheel;
static wheel_position_control_t g_right_wheel;
static bool g_initialized = false;
static double g_common_target_speed = 0.0;  // Vitesse cible commune
static xOsMutexCtx g_speed_mutex;          // Mutex pour la vitesse commune
static move_type_t g_current_move_type;    // Type de mouvement en cours

// Prototypes de fonctions statiques
static void* wheel_position_control_task(void* arg);
static void wheel_position_control_init(wheel_position_control_t* control);
static void wheel_position_control_shutdown(wheel_position_control_t* control);


// Fonction utilitaire pour convertir la distance en ticks d'encodeur
static int32_t distance_to_ticks(double distance_mm) 
{
    int32_t ticks = (int32_t)((distance_mm / 10.0) / (WHEEL_PERIMETER_CM/ENCODER_TICKS_REV));

    X_LOG_TRACE("Conversion de distance: distance=%d mm, ticks=%d",
                (int)distance_mm, ticks);
                
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
    
    X_LOG_TRACE("Calcul de distance de rampe: vitesse=%.2f rad/s, accélération=%.2f rad/s², distance=%.2f cm",
                target_speed, acceleration, distance_mm);
    
    return distance_to_ticks(distance_mm);
}

// Fonction utilitaire pour convertir l'angle en ticks d'encodeur
static int32_t angle_to_ticks(double angle_rad) 
{
    // Calculer la rotation de roue pour l'angle souhaité
    // Pour une propulsion différentielle, la rotation de roue est:
    // rotation_roue = (angle * distance_roues) / rayon_roue
    double wheel_rotation_rad = ((WHEEL_DISTANCE_CM / 2.0) * angle_rad);
    
    // Convertir les radians en ticks d'encodeur
    // Multiplier par 10 pour corriger l'échelle
    int32_t ticks = (int32_t)((wheel_rotation_rad) / (WHEEL_PERIMETER_CM/ENCODER_TICKS_REV));
    
    X_LOG_TRACE("Conversion d'angle: angle=%.2f rad, rotation_roue=%.2f rad, ticks=%d",
                angle_rad, wheel_rotation_rad, ticks);
    
    return ticks;
}
/*
// Fonction utilitaire pour calculer le delta d'encodeur en tenant compte du débordement
static int32_t calculate_encoder_delta(uint16_t current, uint16_t previous) {
    // Gestion du débordement
    if (current < previous && (previous - current) > 32768) {
        // Débordement vers le haut
        return (int32_t)((65536 - previous) + current);
    } else if (current > previous && (current - previous) > 32768) {
        // Débordement vers le bas
        return -(int32_t)((65536 - current) + previous);
    } else {
        // Pas de débordement
        return (int32_t)current - (int32_t)previous;
    }
}

// --- Fonctions internes ---

// Fonction utilitaire pour mettre à jour la position du robot
static void update_robot_position(int32_t left_ticks, int32_t right_ticks) 
{
    mutexLock(&g_position_mutex);
    
    // Calculer les mouvements des roues en mm
    // Convertir les ticks en mm: (ticks * circonférence) / ticks_par_revolution
    double left_mm = (left_ticks * 2.0 * M_PI * WHEEL_DISTANCE_CM * 10.0) / ENCODER_TICKS_REV;
    double right_mm = (right_ticks * 2.0 * M_PI * WHEEL_DISTANCE_CM * 10.0) / ENCODER_TICKS_REV;
    
    // Calculer le mouvement linéaire et angulaire
    double linear_mm = (left_mm + right_mm) / 2.0;
    double angular_rad = (right_mm - left_mm) / (WHEEL_DISTANCE_CM * 10.0); // Convertir en mm
    
    // Mettre à jour l'angle
    g_robot_position.angle_rad += angular_rad;
    
    // Normaliser l'angle à [-π, π]
    while (g_robot_position.angle_rad > M_PI) g_robot_position.angle_rad -= 2.0 * M_PI;
    while (g_robot_position.angle_rad < -M_PI) g_robot_position.angle_rad += 2.0 * M_PI;
    
    // Mettre à jour les coordonnées x et y (distance depuis l'origine en mm)
    g_robot_position.x_mm += (int32_t)(linear_mm * cos(g_robot_position.angle_rad));
    g_robot_position.y_mm += (int32_t)(linear_mm * sin(g_robot_position.angle_rad));
    
    //X_LOG_TRACE("Position mise à jour - X: %d mm, Y: %d mm, Angle: %.2f rad",
    //            g_robot_position.x_mm, g_robot_position.y_mm, g_robot_position.angle_rad);
    
    mutexUnlock(&g_position_mutex);
}
*/
static void* wheel_position_control_task(void* arg) 
{
    wheel_position_control_t* control = (wheel_position_control_t*)arg;
    if (!control) 
    {
        //X_LOG_TRACE("Tâche de contrôle de position de roue: Pointeur de contrôle invalide");
        return (void*)OS_TASK_EXIT_FAILURE;
    }

    //X_LOG_TRACE("Tâche de contrôle de position de roue démarrée");
    while (control->running) 
    {
        mutexLock(&control->mutex);
        
        if (control->should_stop) 
        {
            motor_control_stop();
            control->motion_finished = true;
            control->should_stop = false;
            mutexUnlock(&control->mutex);
            continue;
        }

        // Obtenir les positions actuelles des encodeurs
        int32_t encoder_values[2] = {0, 0};
        GetMotorEncoderValues(encoder_values);
        
        // Calculer les deltas des encodeurs en tenant compte du débordement
        //int32_t left_delta = calculate_encoder_delta(encoder_values[0], g_left_wheel.current_ticks);
        //int32_t right_delta = calculate_encoder_delta(encoder_values[1], g_right_wheel.current_ticks);
        
        // Mettre à jour la position du robot
        //update_robot_position(left_delta, right_delta);
        
        // Mettre à jour les ticks actuels
        g_left_wheel.current_ticks = encoder_values[0];
        g_right_wheel.current_ticks = encoder_values[1];
        
        // Calculer la distance restante en ticks
        int32_t remaining_ticks = control->target_ticks - control->current_ticks;
        int32_t abs_remaining = abs(remaining_ticks);

        // Calculer le nombre de ticks pendant la rampe de décélération
        int32_t nb_decc_tick = calculate_ramp_tick(control->max_speed, DECELERATION_COEF);

        //X_LOG_TRACE("nb_decc_tick: %d, %d", nb_decc_tick,abs(control->target_ticks) - nb_decc_tick);
        // Variables pour le contrôle de la phase d'accélération
        static bool acc_done = false;
        static bool start_decc = false;
        static bool decc_done = false;
        static double step_acc = ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;
        static double step_decc = DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0;

        double right_wheel_speed = 0.0, left_wheel_speed = 0.0;


        // Phase d'accélération
        if (!acc_done && !control->motion_finished) {
            start_decc = false;
            decc_done = false;
            mutexLock(&g_speed_mutex);
            g_common_target_speed += step_acc;
            
            if (g_common_target_speed >= control->max_speed) {
                g_common_target_speed = control->max_speed;
                acc_done = true;
            }
            mutexUnlock(&g_speed_mutex);

            // Vérifier si on a parcouru la moitié des ticks
            if (abs_remaining <= (abs(control->target_ticks - control->current_ticks) / 2)) {
                //probleme
                X_LOG_TRACE("Moitie - start decceleration")
                acc_done = true;
                start_decc = true;
            }
        }
        // Phase de décélération
        else if ((abs(control->current_ticks) >= (abs(control->target_ticks) - nb_decc_tick) || start_decc) && !decc_done) {
            //X_LOG_TRACE("Phase décélération - current_ticks: %d, target_ticks: %d, nb_decc_tick: %d, start_decc: %d, condition1: %d, condition2: %d",
            //    abs(control->current_ticks),
            //    abs(control->target_ticks),
            //    nb_decc_tick,
            //    start_decc,
            //    abs(control->current_ticks) >= (abs(control->target_ticks) - nb_decc_tick),
            //    start_decc);
            mutexLock(&g_speed_mutex);
            g_common_target_speed -= step_decc;
            if (g_common_target_speed < MIN_SPEED_RAD_S) {
                g_common_target_speed = 0.0;
                decc_done = true;
            }
            mutexUnlock(&g_speed_mutex);
        }
        // Phase de correction (seulement après la fin de la décélération)
        else if (decc_done) {
            // Calculer la distance restante
            int32_t remaining_ticks = control->target_ticks - control->current_ticks;
            int32_t abs_remaining = abs(remaining_ticks);

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
                    // Les deux roues tournent dans le même sens
                    break;
                    
                case LEFT:
                    // Roue gauche en arrière, roue droite en avant
                    // Appliquer la correction à la roue appropriée
                    if (control->is_left_wheel) {
                        motor_control_set_left_speed(correction);
                        X_LOG_TRACE("Correction LEFT - Roue gauche: current=%d, target=%d, remaining=%d, correction=%.2f, speed=%.2f", 
                            control->current_ticks, control->target_ticks, remaining_ticks, correction, -correction);
                    } else {
                        motor_control_set_right_speed(correction);
                        X_LOG_TRACE("Correction LEFT - Roue droite: current=%d, target=%d, remaining=%d, correction=%.2f, speed=%.2f", 
                            control->current_ticks, control->target_ticks, remaining_ticks, correction, correction);
                    }
                    break;
                    
                case RIGHT:
                    // Roue gauche en avant, roue droite en arrière
                    // Appliquer la correction à la roue appropriée
                    if (control->is_left_wheel) {
                        motor_control_set_left_speed(correction);
                        X_LOG_TRACE("Correction RIGHT - Roue gauche: current=%d, target=%d, remaining=%d, correction=%.2f, speed=%.2f", 
                            control->current_ticks, control->target_ticks, remaining_ticks, correction, correction);
                    } else {
                        motor_control_set_right_speed(correction);
                        X_LOG_TRACE("Correction RIGHT - Roue droite: current=%d, target=%d, remaining=%d, correction=%.2f, speed=%.2f", 
                            control->current_ticks, control->target_ticks, remaining_ticks, correction, -correction);
                    }
                    break;
                }
            } else {
                // Arrêt final si on est dans la marge de correction
                if (control->is_left_wheel) {
                    motor_control_set_left_speed(0.0);
                    X_LOG_TRACE("Arrêt final - Roue gauche: current=%d, target=%d, remaining=%d", 
                        control->current_ticks, control->target_ticks, remaining_ticks);
                } else {
                    motor_control_set_right_speed(0.0);
                    X_LOG_TRACE("Arrêt final - Roue droite: current=%d, target=%d, remaining=%d", 
                        control->current_ticks, control->target_ticks, remaining_ticks);
                }
                control->motion_finished = true;
                // Réinitialiser les drapeaux pour le prochain mouvement
                acc_done = false;
                start_decc = false;
                decc_done = false;
            }
        }

        // Appliquer la vitesse au moteur avec le signe approprié selon le type de mouvement
        if (!decc_done) {  // Ne pas appliquer la vitesse pendant la phase de correction
            double wheel_speed = g_common_target_speed; 
            switch (g_current_move_type) {
                case FORWARD:
                    right_wheel_speed = wheel_speed;
                    left_wheel_speed = wheel_speed;
                    // Les deux roues tournent dans le même sens
                    break;
                    
                case LEFT:
                    // Roue gauche en arrière, roue droite en avant
                    right_wheel_speed = wheel_speed;
                    left_wheel_speed = -wheel_speed;

                    break;
                    
                case RIGHT:
                    // Roue gauche en avant, roue droite en arrière
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

    //X_LOG_TRACE("Tâche de contrôle de position de roue arrêtée");
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

    // Convertir la distance en ticks d'encodeur
    int32_t target_ticks = distance_to_ticks(distance_mm);
    
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
    
    // Si angle négatif = tourner à gauche
    // Si angle positif = tourner à droite
    if (angle_rad < 0) {
        // Tourner à gauche : roue gauche en arrière, roue droite en avant
        g_left_wheel.target_ticks = g_left_wheel.current_ticks - target_ticks;
        g_right_wheel.target_ticks = g_right_wheel.current_ticks + target_ticks;
        g_current_move_type = LEFT;
    } else {
        // Tourner à droite : roue gauche en avant, roue droite en arrière
        g_left_wheel.target_ticks = g_left_wheel.current_ticks + target_ticks;
        g_right_wheel.target_ticks = g_right_wheel.current_ticks - target_ticks;
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
    if (!g_initialized || !position) return -1;
    
    mutexLock(&g_position_mutex);
    *position = g_robot_position;
    mutexUnlock(&g_position_mutex);
    
    return 0;
}

static void wheel_position_control_init(wheel_position_control_t* control) 
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
    control->is_left_wheel = false; // Default value, actual setting should be done elsewhere

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
    wheel_position_control_init(&g_left_wheel);
    wheel_position_control_init(&g_right_wheel);
    
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

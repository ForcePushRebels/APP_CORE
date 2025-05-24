#include "positionControl.h"
#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xLog.h"
#include <math.h>
#include <string.h>

// Constants for position control
#define WHEEL_RADIUS_M      0.03    // Wheel radius in meters
#define WHEEL_DISTANCE_M    0.15    // Distance between wheels in meters
#define REGULATION_PERIOD_MS 50     // Regulation period in milliseconds
#define ACCELERATION_COEF   0.1     // Acceleration coefficient (0-1)
#define DECELERATION_COEF   0.2     // Deceleration coefficient (0-1)
#define MIN_SPEED_RAD_S     0.5     // Minimum speed to start movement

// Position control structure
typedef struct 
{
    double x;                // Current X position in meters
    double y;                // Current Y position in meters
    double theta;            // Current orientation in radians
    double target_x;         // Target X position
    double target_y;         // Target Y position
    double target_theta;     // Target orientation
    double current_speed;    // Current speed in rad/s
    double target_speed;     // Target speed in rad/s
    bool motion_finished;    // Flag indicating if motion is finished
    xOsMutexCtx mutex;       // Mutex for thread safety
    xOsTaskCtx task;         // Task context
    bool running;            // Task running flag
} position_control_t;

// Global variables
static position_control_t g_position_control;
static bool g_initialized = false;

// Static function prototypes
static void* position_control_task(void* arg);
static void position_control_init_task(position_control_t* control);
static void position_control_shutdown_task(position_control_t* control);
static void update_position(position_control_t* control);
static double calculate_distance(double x1, double y1, double x2, double y2);
static double normalize_angle(double angle);
static void apply_speed_profile(position_control_t* control, double distance_to_target);

// --- Internal functions ---

static void* position_control_task(void* arg) 
{
    position_control_t* control = (position_control_t*)arg;
    if (!control) 
    {
        X_LOG_TRACE("Position control task: Invalid control pointer");
        return (void*)OS_TASK_EXIT_FAILURE;
    }

    X_LOG_TRACE("Position control task started");
    while (control->running) 
    {
        mutexLock(&control->mutex);
        
        // Calculate distance to target
        double distance = calculate_distance(control->x, control->y, 
                                          control->target_x, control->target_y);
        
        // Apply speed profile based on distance
        apply_speed_profile(control, distance);
        
        // Update position
        update_position(control);
        
        X_LOG_TRACE("Position: (%.3f, %.3f, %.1f°), Target: (%.3f, %.3f, %.1f°), Distance: %.3fm, Speed: %.2f rad/s",
                    control->x, control->y, control->theta * 180.0 / M_PI,
                    control->target_x, control->target_y, control->target_theta * 180.0 / M_PI,
                    distance, control->current_speed);
        
        mutexUnlock(&control->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }

    X_LOG_TRACE("Position control task stopped");
    return (void*)OS_TASK_EXIT_SUCCESS;
}

static void position_control_shutdown_task(position_control_t* control) 
{
    if (!control) return;

    control->running = false;
    osTaskStop(&control->task, 2); // Wait max 2 seconds
    osTaskWait(&control->task, NULL);
    mutexDestroy(&control->mutex);
}

static void update_position(position_control_t* control) 
{
    // Get current motor speeds
    double left_speed = motor_control_get_left_speed();
    double right_speed = motor_control_get_right_speed();

    // Calculate linear and angular velocities
    double linear_velocity = (WHEEL_RADIUS_M * (left_speed + right_speed)) / 2.0;
    double angular_velocity = (WHEEL_RADIUS_M * (right_speed - left_speed)) / WHEEL_DISTANCE_M;

    // Update position
    double dt = REGULATION_PERIOD_MS / 1000.0;
    control->x += linear_velocity * cos(control->theta) * dt;
    control->y += linear_velocity * sin(control->theta) * dt;
    control->theta = normalize_angle(control->theta + angular_velocity * dt);
}

static void apply_speed_profile(position_control_t* control, double distance_to_target) 
{
    // Calculate deceleration distance
    double decel_distance = (control->current_speed * control->current_speed) / 
                           (2.0 * DECELERATION_COEF);
    
    X_LOG_TRACE("Speed profile - Current: %.2f rad/s, Target: %.2f rad/s, Distance: %.3fm, Decel dist: %.3fm",
                control->current_speed, control->target_speed, distance_to_target, decel_distance);
    
    if (distance_to_target <= decel_distance) 
    {
        // Deceleration phase
        control->target_speed = control->current_speed - 
                               (DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0);
        
        if (control->target_speed < MIN_SPEED_RAD_S) 
        {
            control->target_speed = 0.0;
            control->motion_finished = true;
            X_LOG_TRACE("Motion finished, stopping");
        }
        else
        {
            X_LOG_TRACE("Decelerating: distance=%.3fm, speed=%.2f rad/s", 
                       distance_to_target, control->target_speed);
        }
    }
    else if (control->current_speed < control->target_speed) 
    {
        // Acceleration phase
        control->target_speed = control->current_speed + 
                               (ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0);
        X_LOG_TRACE("Accelerating: distance=%.3fm, speed=%.2f rad/s", 
                   distance_to_target, control->target_speed);
    }
    
    // Calculate motor speeds based on target speed and movement type
    double left_speed = control->target_speed;
    double right_speed = control->target_speed;
    
    // Apply speeds to motors
    motor_control_set_left_speed(left_speed);
    motor_control_set_right_speed(right_speed);
}

static double calculate_distance(double x1, double y1, double x2, double y2) 
{
    double dx = x2 - x1;
    double dy = y2 - y1;
    return sqrt(dx * dx + dy * dy);
}

static double normalize_angle(double angle) 
{
    while (angle > M_PI) angle -= 2.0 * M_PI;
    while (angle < -M_PI) angle += 2.0 * M_PI;
    return angle;
}

// --- Public functions ---

bool position_control_is_motion_finished(void)
{
    if (!g_initialized) return true;
    
    bool finished = false;
    mutexLock(&g_position_control.mutex);
    finished = g_position_control.motion_finished;
    mutexUnlock(&g_position_control.mutex);
    return finished;
}

int16_t position_control_advance(int16_t distance_mm, float speed_rad_s_max) 
{
    if (!g_initialized) return -1;

    double distance_m = distance_mm / 1000.0;
    
    mutexLock(&g_position_control.mutex);
    
    g_position_control.target_x = g_position_control.x + distance_m * cos(g_position_control.theta);
    g_position_control.target_y = g_position_control.y + distance_m * sin(g_position_control.theta);
    g_position_control.target_speed = speed_rad_s_max;
    g_position_control.motion_finished = false;
    
    mutexUnlock(&g_position_control.mutex);
    return 0;
}

int16_t position_control_turn(int16_t angle_rad, float speed_rad_s_max) 
{
    if (!g_initialized) return -1;

    mutexLock(&g_position_control.mutex);
    
    g_position_control.target_theta = normalize_angle(g_position_control.theta + angle_rad);
    g_position_control.target_speed = speed_rad_s_max;
    g_position_control.motion_finished = false;
    
    mutexUnlock(&g_position_control.mutex);
    return 0;
}

int16_t position_control_stop(void) 
{
    if (!g_initialized) return -1;

    motor_control_stop();
    return 0;
}

int16_t position_control_shutdown(void) 
{
    if (!g_initialized) 
    {
        X_LOG_TRACE("Position control not initialized, nothing to shutdown");
        return 0;
    }

    X_LOG_TRACE("Shutting down position control");
    // Shutdown task
    position_control_shutdown_task(&g_position_control);
    
    // Shutdown motor control
    motor_control_shutdown();
    X_LOG_TRACE("Motor control shutdown complete");
    
    g_initialized = false;
    X_LOG_TRACE("Position control shutdown complete");
    return 0;
}

static void position_control_init_task(position_control_t* control) 
{
    if (!control) 
    {
        X_LOG_TRACE("Failed to initialize position control task: Invalid control pointer");
        return;
    }

    X_LOG_TRACE("Initializing position control task");
    // Initialize values
    control->x = 0.0;
    control->y = 0.0;
    control->theta = 0.0;
    control->target_x = 0.0;
    control->target_y = 0.0;
    control->target_theta = 0.0;
    control->current_speed = 0.0;
    control->target_speed = 0.0;
    control->motion_finished = true;
    control->running = false;

    // Initialize mutex
    if (mutexCreate(&control->mutex) != MUTEX_OK) 
    {
        X_LOG_TRACE("Failed to create position control mutex");
        return;
    }

    // Initialize task
    osTaskInit(&control->task);
    control->task.t_ptTask = position_control_task;
    control->task.t_ptTaskArg = control;

    // Create task
    int result = osTaskCreate(&control->task);
    if (result != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to create position control task");
        mutexDestroy(&control->mutex);
        return;
    }

    control->running = true;
    X_LOG_TRACE("Position control task initialized successfully");
}

// --- Test functions ---

int16_t position_control_init(void) 
{
    if (g_initialized) 
    {
        X_LOG_TRACE("Position control already initialized");
        return 0;
    }

    X_LOG_TRACE("Initializing position control");
    
    // Initialize motor control first
    int16_t motor_init_result = motor_control_init();
    if (motor_init_result != 0)
    {
        X_LOG_TRACE("Failed to initialize motor control: %d", motor_init_result);
        return -1;
    }
    X_LOG_TRACE("Motor control initialized successfully");

    memset(&g_position_control, 0, sizeof(g_position_control));
    g_position_control.running = false;

    // Initialize task
    position_control_init_task(&g_position_control);
    
    g_initialized = true;
    X_LOG_TRACE("Position control initialized successfully");
    return 0;
}

int16_t position_control_test_straight_line(void) 
{
    if (!g_initialized) 
    {
        X_LOG_TRACE("Cannot run test: position control not initialized");
        return -1;
    }

    static bool test_started = false;
    static uint64_t start_time = 0;
    uint64_t current_time = xTimerGetCurrentMs();
    
    if (!test_started) 
    {
        X_LOG_TRACE("Starting straight line test");
        
        // Désactiver temporairement la tâche de contrôle de position
        mutexLock(&g_position_control.mutex);
        g_position_control.running = false;
        mutexUnlock(&g_position_control.mutex);
        
        // Initialiser le test
        int stop_result = motor_control_stop();
        X_LOG_TRACE("motor_control_stop() returned: %d", stop_result);
        xTimerDelay(1000); // Attendre 1 seconde pour stabilisation
        
        // Avancer en ligne droite à vitesse constante
        int left_result = motor_control_set_left_speed(2.0);  // 2 rad/s
        int right_result = motor_control_set_right_speed(2.0); // 2 rad/s
        X_LOG_TRACE("motor_control_set_left_speed() returned: %d", left_result);
        X_LOG_TRACE("motor_control_set_right_speed() returned: %d", right_result);
        
        test_started = true;
        start_time = current_time;
        X_LOG_TRACE("Motors set to 2.0 rad/s");
    }
    
    // Afficher la progression toutes les 500ms
    if (current_time - start_time > 500) 
    {
        double left_speed = motor_control_get_left_speed();
        double right_speed = motor_control_get_right_speed();
        X_LOG_TRACE("Test in progress - Time elapsed: %.1f seconds", (current_time - start_time) / 1000.0);
        X_LOG_TRACE("Left motor - Target: 2.0 rad/s, Current: %.2f rad/s", left_speed);
        X_LOG_TRACE("Right motor - Target: 2.0 rad/s, Current: %.2f rad/s", right_speed);
        start_time = current_time;
    }

    return 0;
}

// --- End of file ---

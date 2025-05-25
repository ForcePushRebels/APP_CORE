#include "positionControl.h"
#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xLog.h"
#include <math.h>
#include <string.h>


// Position control structure for each wheel
typedef struct 
{
    int32_t current_ticks;      // Current encoder position in ticks
    int32_t target_ticks;       // Target encoder position in ticks
    double current_speed;       // Current speed in rad/s
    double target_speed;        // Target speed in rad/s
    double max_speed;          // Maximum speed for this movement
    bool motion_finished;       // Flag indicating if motion is finished
    bool should_stop;          // Flag to request motion stop
    xOsMutexCtx mutex;         // Mutex for thread safety
    xOsTaskCtx task;           // Task context
    bool running;              // Task running flag
} wheel_position_control_t;

// Global variables
static wheel_position_control_t g_left_wheel;
static wheel_position_control_t g_right_wheel;
static bool g_initialized = false;

// Static function prototypes
static void* wheel_position_control_task(void* arg);
static void wheel_position_control_init(wheel_position_control_t* control);
static void wheel_position_control_shutdown(wheel_position_control_t* control);
static double calculate_ramp_distance(double speed, double acceleration);
//static void update_wheel_position(wheel_position_control_t* control);
//static void apply_speed_profile(wheel_position_control_t* control);

// Helper function to convert distance to encoder ticks
static int32_t distance_to_ticks(double distance_mm) 
{
    // Convert mm to cm, then to wheel rotation in radians
    double distance_cm = distance_mm / 10.0;
    double wheel_rotation_rad = distance_cm / WHEEL_RADIUS_CM;
    
    // Convert radians to encoder ticks
    // One revolution (2π radians) = ENCODER_TICKS_REV ticks
    // Multiply by 10 to correct the scaling
    int32_t ticks = (int32_t)(wheel_rotation_rad * ENCODER_TICKS_REV/ (2.0 * M_PI));
    
    X_LOG_TRACE("Distance conversion: distance=%d mm, wheel_rotation=%.2f rad, ticks=%d",
                (int)distance_mm, wheel_rotation_rad, ticks);
    
    return ticks;
}

// Helper function to convert angle to encoder ticks
static int32_t angle_to_ticks(double angle_rad) 
{
    // Calculate wheel rotation for the desired angle
    // For a differential drive, the wheel rotation is:
    // wheel_rotation = (angle * wheel_distance) / wheel_radius
    double wheel_distance = WHEEL_DISTANCE_CM / 2.0;
    double wheel_rotation_rad = (angle_rad * wheel_distance) / WHEEL_RADIUS_CM;
    
    // Convert radians to encoder ticks
    // Multiply by 10 to correct the scaling
    int32_t ticks = (int32_t)(wheel_rotation_rad * ENCODER_TICKS_REV / (2.0 * M_PI));
    
    X_LOG_TRACE("Angle conversion: angle=%.2f rad, wheel_rotation=%.2f rad, ticks=%d",
                angle_rad, wheel_rotation_rad, ticks);
    
    return ticks;
}

// --- Internal functions ---

static double calculate_ramp_distance(double speed, double acceleration) 
{
    // Calculate distance needed for acceleration/deceleration
    // Using the formula: distance = (v²)/(2*a)
    return (speed * speed) / (2.0 * acceleration);
}

static void* wheel_position_control_task(void* arg) 
{
    wheel_position_control_t* control = (wheel_position_control_t*)arg;
    if (!control) 
    {
        X_LOG_TRACE("Wheel position control task: Invalid control pointer");
        return (void*)OS_TASK_EXIT_FAILURE;
    }

    X_LOG_TRACE("Wheel position control task started");
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

        // Calculate remaining distance in ticks
        int32_t remaining_ticks = control->target_ticks - control->current_ticks;
        int32_t abs_remaining = abs(remaining_ticks);
        
        // Calculate acceleration and deceleration distances
        double accel_distance = calculate_ramp_distance(control->max_speed, ACCELERATION_COEF);
        double decel_distance = calculate_ramp_distance(control->max_speed, DECELERATION_COEF);
        
        // Convert distances to encoder ticks
        int32_t accel_ticks = (int32_t)(accel_distance * ENCODER_TICKS_REV / (2.0 * M_PI));
        int32_t decel_ticks = (int32_t)(decel_distance * ENCODER_TICKS_REV / (2.0 * M_PI));
        
        // Check if we can reach max speed
        bool can_reach_max_speed = (abs_remaining > (accel_ticks + decel_ticks));
        
        if (can_reach_max_speed) 
        {
            // Full acceleration-deceleration profile
            if (abs_remaining <= decel_ticks) 
            {
                // Deceleration phase
                control->target_speed = control->current_speed - 
                                      (DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0);
                if (control->target_speed < MIN_SPEED_RAD_S) 
                {
                    control->target_speed = 0.0;
                    control->motion_finished = true;
                }
            }
            else if (control->current_speed < control->max_speed) 
            {
                // Acceleration phase
                control->target_speed = control->current_speed + 
                                      (ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0);
            }
        }
        else 
        {
            // Triangular profile
            int32_t mid_point = abs_remaining / 2;
            if (abs_remaining <= mid_point) 
            {
                // Deceleration phase
                control->target_speed = control->current_speed - 
                                      (DECELERATION_COEF * REGULATION_PERIOD_MS / 1000.0);
                if (control->target_speed < MIN_SPEED_RAD_S) 
                {
                    control->target_speed = 0.0;
                    control->motion_finished = true;
                }
            }
            else 
            {
                // Acceleration phase
                control->target_speed = control->current_speed + 
                                      (ACCELERATION_COEF * REGULATION_PERIOD_MS / 1000.0);
            }
        }
        
        // Apply speed to motor
        if (control == &g_left_wheel) 
        {
            motor_control_set_left_speed(control->target_speed);
        } 
        else 
        {
            motor_control_set_right_speed(control->target_speed);
        }
        
        // Update current position
        uint16_t encoder_values[2] = {0, 0};
        GetMotorEncoderValues(encoder_values);
        control->current_ticks = (control == &g_left_wheel) ? 
                               encoder_values[0] : encoder_values[1];
        
        // Check if we need position correction
        if (control->motion_finished && abs_remaining > 0) 
        {
            // Apply small correction speed
            double correction = (remaining_ticks > 0) ? CORRECTION_SPEED : -CORRECTION_SPEED;
            if (control == &g_left_wheel) 
            {
                motor_control_set_left_speed(correction);
            } 
            else 
            {
                motor_control_set_right_speed(correction);
            }
        }
        
        X_LOG_TRACE("Wheel position: %d ticks, Target: %d ticks, Speed: %.2f rad/s",
                    control->current_ticks, control->target_ticks, control->current_speed);
        
        mutexUnlock(&control->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }

    X_LOG_TRACE("Wheel position control task stopped");
    return (void*)OS_TASK_EXIT_SUCCESS;
}

// --- Public functions ---

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

    // Convert distance to encoder ticks
    int32_t target_ticks = distance_to_ticks(distance_mm);
    
    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    
    // Get current encoder positions
    uint16_t encoder_values[2] = {0, 0};
    GetMotorEncoderValues(encoder_values);
    
    // Set targets for both wheels
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
    
    X_LOG_TRACE("Advance: distance=%d mm, target_ticks=%d", distance_mm, target_ticks);
    
    mutexUnlock(&g_right_wheel.mutex);
    mutexUnlock(&g_left_wheel.mutex);
    return 0;
}

int16_t position_control_turn(int16_t angle_rad, float speed_rad_s_max) 
{
    if (!g_initialized) return -1;

    // Convert angle to encoder ticks
    int32_t target_ticks = angle_to_ticks(angle_rad);
    
    mutexLock(&g_left_wheel.mutex);
    mutexLock(&g_right_wheel.mutex);
    
    // Get current encoder positions
    uint16_t encoder_values[2] = {0, 0};
    GetMotorEncoderValues(encoder_values);
    
    // Set targets for both wheels
    g_left_wheel.current_ticks = encoder_values[0];
    g_right_wheel.current_ticks = encoder_values[1];
    g_left_wheel.target_ticks = g_left_wheel.current_ticks - target_ticks;  // Left wheel goes backward
    g_right_wheel.target_ticks = g_right_wheel.current_ticks + target_ticks; // Right wheel goes forward
    g_left_wheel.max_speed = speed_rad_s_max;
    g_right_wheel.max_speed = speed_rad_s_max;
    g_left_wheel.motion_finished = false;
    g_right_wheel.motion_finished = false;
    g_left_wheel.should_stop = false;
    g_right_wheel.should_stop = false;
    
    X_LOG_TRACE("Turn: angle=%.2f rad, target_ticks=%d", angle_rad, target_ticks);
    
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

static void wheel_position_control_init(wheel_position_control_t* control) 
{
    if (!control) return;

    // Initialize values
    control->current_ticks = 0;
    control->target_ticks = 0;
    control->current_speed = 0.0;
    control->target_speed = 0.0;
    control->max_speed = 0.0;
    control->motion_finished = true;
    control->should_stop = false;
    control->running = false;

    // Initialize mutex
    if (mutexCreate(&control->mutex) != MUTEX_OK) 
    {
        X_LOG_TRACE("Failed to create wheel position control mutex");
        return;
    }

    // Initialize task
    osTaskInit(&control->task);
    control->task.t_ptTask = wheel_position_control_task;
    control->task.t_ptTaskArg = control;

    // Create task
    int result = osTaskCreate(&control->task);
    if (result != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to create wheel position control task");
        mutexDestroy(&control->mutex);
        return;
    }

    control->running = true;
    X_LOG_TRACE("Wheel position control task initialized successfully");
}

static void wheel_position_control_shutdown(wheel_position_control_t* control) 
{
    if (!control) return;

    control->running = false;
    osTaskStop(&control->task, 2); // Wait max 2 seconds
    osTaskWait(&control->task, NULL);
    mutexDestroy(&control->mutex);
}

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

    // Initialize both wheel controllers
    wheel_position_control_init(&g_left_wheel);
    wheel_position_control_init(&g_right_wheel);
    
    g_initialized = true;
    X_LOG_TRACE("Position control initialized successfully");
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
    
    // Shutdown wheel controllers
    wheel_position_control_shutdown(&g_left_wheel);
    wheel_position_control_shutdown(&g_right_wheel);
    
    // Shutdown motor control
    motor_control_shutdown();
    X_LOG_TRACE("Motor control shutdown complete");
    
    g_initialized = false;
    X_LOG_TRACE("Position control shutdown complete");
    return 0;
}



// --- End of file ---

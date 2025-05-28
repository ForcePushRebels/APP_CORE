#include "motorControl.h"
#include "hardwareAbstraction.h"
#include "xTimer.h"
#include "xOsMutex.h"
#include "xTask.h"
#include <math.h>
#include <string.h>


typedef struct {
    const uint16_t motor_id;      // Constant motor ID (MRPIZ_MOTOR_LEFT or MRPIZ_MOTOR_RIGHT)
    float target_speed_rad_s;     // Target speed in rad/s
    float current_speed_rad_s;    // Current speed in rad/s
    float error;                  // Current error
    float last_error;             // Previous error
    float integral_error;         // Integral error for PI correction
    int encoder_last;
    uint64_t last_update_ms;
    xOsMutexCtx mutex;
    bool running;
    xOsTaskCtx task;
} motor_regulator_t;

// Initialize structures with constant IDs
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

// Static function prototypes
static void* motor_regulator_task(void* arg);
static void motor_regulator_init(motor_regulator_t* reg);
static void motor_regulator_shutdown(motor_regulator_t* reg);
static bool is_valid_motor_id(uint16_t motor_id);
static float encoder_ticks_to_rad_per_sec(int delta_ticks, float dt);
static void update_motor_speed(motor_regulator_t* reg, int encoder_now, uint64_t now_ms);
static int calculate_speed_percentage(motor_regulator_t* reg);

// --- Internal functions ---

static float encoder_ticks_to_rad_per_sec(int delta_ticks, float dt) 
{
    if (dt <= 0.0f) return 0.0f;
    
    // Calculate angular speed in rad/s
    // 2π radians = ENCODER_TICKS_REV ticks
    float rad_per_tick = (2.0f * M_PI) / ENCODER_TICKS_REV;
    float speed_rad_s = (delta_ticks * rad_per_tick) / dt;
    
    return speed_rad_s;
}

static bool is_valid_motor_id(uint16_t motor_id) 
{
    return (motor_id == MRPIZ_MOTOR_LEFT || motor_id == MRPIZ_MOTOR_RIGHT);
}

static void update_motor_speed(motor_regulator_t* reg, int encoder_now, uint64_t now_ms) 
{
    float dt = (now_ms - reg->last_update_ms) / 1000.0f; // Convert to seconds
    if (dt <= 0.0f) return;

    // Calculate encoder delta
    int delta = encoder_now - reg->encoder_last;

    // Calculate current speed
    float new_speed = encoder_ticks_to_rad_per_sec(delta, dt);
    
    // Update speed with simple low-pass filter
    //const float alpha = 0.3f; // Filtering factor (0-1)
    reg->current_speed_rad_s = new_speed; //(alpha * new_speed) + ((1.0f - alpha) * reg->current_speed_rad_s);

    // Update values
    reg->encoder_last = encoder_now;
    reg->last_update_ms = now_ms;
}

static int calculate_speed_percentage(motor_regulator_t* reg) 
{
    // Simple conversion of target speed to percentage
    float pwm = (reg->target_speed_rad_s / MAX_SPEED_RAD_S) * 100.0f;
    
    // PWM saturation
    if (pwm > 100.0f) pwm = 100.0f;
    if (pwm < -100.0f) pwm = -100.0f;

    return (int)pwm;
}

static void motor_regulator_init(motor_regulator_t* reg) 
{
    // Validate motor ID
    if (!is_valid_motor_id(reg->motor_id)) {
        return;
    }
    
    // Initialize values (except motor_id which is constant)
    reg->target_speed_rad_s = 0.0f;
    reg->current_speed_rad_s = 0.0f;
    reg->error = 0.0f;
    reg->last_error = 0.0f;
    reg->integral_error = 0.0f;
    uint16_t tab[2] = {0, 0};
    GetMotorEncoderValues(tab);
    reg->encoder_last = tab[reg->motor_id];
    reg->last_update_ms = xTimerGetCurrentMs();
    reg->running = false;  // Don't start until everything is initialized

    // Initialize mutex
    if (mutexCreate(&reg->mutex) != MUTEX_OK) {
        return;
    }

    // Initialize task with default parameters
    osTaskInit(&reg->task);
    reg->task.t_ptTask = motor_regulator_task;
    reg->task.t_ptTaskArg = reg;

    // Create task
    int result = osTaskCreate(&reg->task);
    if (result != OS_TASK_SUCCESS) {
        mutexDestroy(&reg->mutex);
        return;
    }

    // Start task only after successful initialization
    reg->running = true;
}

static void motor_regulator_shutdown(motor_regulator_t* reg) 
{
    if (!reg) return;
    
    reg->running = false;
    osTaskStop(&reg->task, 2); // Wait max 2 seconds
    osTaskWait(&reg->task, NULL);
    mutexDestroy(&reg->mutex);
}

static void* motor_regulator_task(void* arg) 
{
    motor_regulator_t* reg = (motor_regulator_t*)arg;
    if (!reg) {
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    // Check motor ID
    if (!is_valid_motor_id(reg->motor_id)) {
        return (void*)OS_TASK_EXIT_FAILURE;
    }
    
    // Wait a bit before starting regulation
    xTimerDelay(100);
    
    while (reg->running) {
        mutexLock(&reg->mutex);

        // Check motor ID before each reading
        if (!is_valid_motor_id(reg->motor_id)) {
            mutexUnlock(&reg->mutex);
            xTimerDelay(REGULATION_PERIOD_MS);
            continue;
        }

        // Read encoder with temporary ID copy
        uint16_t tab[2] = {0, 0};
        GetMotorEncoderValues(tab);
        int encoder_now = tab[reg->motor_id];
        uint64_t now_ms = xTimerGetCurrentMs();

        // Update speed and calculate errors
        update_motor_speed(reg, encoder_now, now_ms);

        // Calculate Speed percentage command
        int speed_percentage = calculate_speed_percentage(reg);

        // Apply Speed percentage command
        SetMotorSpeed(reg->motor_id, speed_percentage);

        mutexUnlock(&reg->mutex);
        xTimerDelay(REGULATION_PERIOD_MS);
    }
    
    // Stop motor on exit
    SetMotorSpeed(reg->motor_id, 0);
    return (void*)OS_TASK_EXIT_SUCCESS;
}

// --- Public functions ---

int motor_control_init(void) 
{
    if (g_initialized) {
        return 0;
    }

    // Initialize regulators with constant IDs
    motor_regulator_init(&g_right_motor);
    xTimerDelay(100);  // Wait a bit between initializations
    motor_regulator_init(&g_left_motor);
    g_initialized = true;
    return 0;
}

void motor_control_shutdown(void) 
{
    if (!g_initialized) {
        return;
    }
    
    // Stop both regulators
    motor_regulator_shutdown(&g_left_motor);
    motor_regulator_shutdown(&g_right_motor);
    
    g_initialized = false;
}

int motor_control_set_speed(uint16_t motor_id, double speed_rad_s) 
{
    if (!g_initialized) {
        return -1;
    }

    if (!is_valid_motor_id(motor_id)) {
        return -1;
    }

    // Limit speed to maximum speed
    if (speed_rad_s > MAX_SPEED_RAD_S) speed_rad_s = MAX_SPEED_RAD_S;
    if (speed_rad_s < -MAX_SPEED_RAD_S) speed_rad_s = -MAX_SPEED_RAD_S;

    motor_regulator_t* reg = (motor_id == MRPIZ_MOTOR_LEFT) ? &g_left_motor : &g_right_motor;
    
    mutexLock(&reg->mutex);
    reg->target_speed_rad_s = (float)speed_rad_s;
    mutexUnlock(&reg->mutex);
    
    return 0;
}

int motor_control_set_left_speed(double speed_rad_s) 
{
    return motor_control_set_speed(MRPIZ_MOTOR_LEFT, speed_rad_s);
}

int motor_control_set_right_speed(double speed_rad_s) 
{
    return motor_control_set_speed(MRPIZ_MOTOR_RIGHT, speed_rad_s);
}

double motor_control_get_left_speed(void) 
{
    if (!g_initialized) {
        return 0.0;
    }

    double speed = 0.0;
    mutexLock(&g_left_motor.mutex);
    speed = g_left_motor.current_speed_rad_s;
    mutexUnlock(&g_left_motor.mutex);
    
    return speed;
}

double motor_control_get_right_speed(void) 
{
    if (!g_initialized) {
        return 0.0;
    }

    double speed = 0.0;
    mutexLock(&g_right_motor.mutex);
    speed = g_right_motor.current_speed_rad_s;
    mutexUnlock(&g_right_motor.mutex);
    
    return speed;
}

int motor_control_stop(void) 
{
    if (!g_initialized) return -1;
    motor_control_set_left_speed(0.0);
    motor_control_set_right_speed(0.0);
    return 0;
}

// --- End of file ---
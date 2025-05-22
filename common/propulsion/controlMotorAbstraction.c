#include "controlMotorAbstraction.h"

// prototypes
static void *regulatorManagerTask(void *p_pvParam);

// Global variables for motor control (if needed)
static int16_t leftMotorEncoder = 0;
static int16_t rightMotorEncoder = 0;

static motor_t *leftMotor = NULL;
static motor_t *rightMotor = NULL;

typedef struct motorManager_t
{
    motor_t *leftMotor;
    motor_t *rightMotor;
} motorManager_t;

typedef struct command_t
{
    int16_t idMotor;
    int16_t speed;
} command_t;

//Queue message
struct mq_attr attr;

void *regulatorManagerTask(void *p_pvParam)
{
    motorManager_t *motorManager = (motorManager_t *)p_pvParam;
    
    // Création d'un lien local vers la queue afin de gérer les droits
    mqd_t mq;

    // transmit the order to the bartender
    // ouverture de la file de message
    if ((mq = mq_open(ALAMBIX_BARTENDER_MQ,O_WRONLY, 0644, &attr)) == -1) {
		perror("mq_open()");
		exit(EXIT_FAILURE);
	}

    if (motorManager == NULL) {
        X_LOG_TRACE("Invalid motor manager parameter");
        return NULL;
    }

    // Main task loop
    while (1) {
        // Read mq messages
        command_t command;

        struct timespec timeout = {0, 0}; // No wait
        ssize_t bytesRead = mq_timedreceive(mq, (char *)&command, sizeof(command), NULL, &timeout);

        if (bytesRead < 0) 
        {
            if (errno != EAGAIN) 
            { // EAGAIN means no message available
            perror("mq_timedreceive()");
            }
        } else if (bytesRead == sizeof(command)) 
        {
            // Process the command
            if (command.idMotor == MRPIZ_MOTOR_LEFT) 
            {
            motorManager->leftMotor->speedValueTarget = command.speed;
            } else if (command.idMotor == MRPIZ_MOTOR_RIGHT) 
            {
            motorManager->rightMotor->speedValueTarget = command.speed;
            } else 
            {
            X_LOG_TRACE("Invalid motor ID: %d", command.idMotor);
            }
        } else 
        {
            X_LOG_TRACE("Received invalid command size");
        }

        // Regulate motor speeds based on sensor feedback
        regulateMotorSpeed(motorManager->leftMotor);
        regulateMotorSpeed(motorManager->rightMotor);

        // Sleep or wait for the next regulation cycle
        osDelay(REGULATOR_TASK_DELAY);
    }

    return NULL;
}

regulateMotorSpeed(motor_t *motor)
{
    if (motor == NULL) {
        X_LOG_TRACE("Invalid motor parameter");
        return;
    }

    // Get the current encoder value
    motor->encoderValue = GetMotorEncoderValues(motor->idMotor);

    // Calculate time difference since last update
    struct timespec currentTime;
    clock_gettime(CLOCK_MONOTONIC, &currentTime);

    // Calculate speed based on encoder value and time difference
    long diffTime = (currentTime.tv_sec - motor->lastUpdateTime.tv_sec) * 1000000000 + 
                    (currentTime.tv_nsec - motor->lastUpdateTime.tv_nsec);
                    
    motor->speedValueCurent = (motor->encoderValue - motor->lastEncoderValue) * 1e9 / diffTime;

    // Store current values for next iteration
    motor->lastEncoderValue = motor->encoderValue;
    motor->lastUpdateTime = currentTime;

    // Calculate the error between target and current speed
    int16_t error = motor->speedValueTarget - motor->speedValueCurent;

    if (error > HYSTERESIS || error < -HYSTERESIS) {
        motor->setpointReached = true; // Set the setpointReached flag if the error is significant
    } else {
        motor->setpointReached = false; // Stop the motor if within hysteresis
    }

    // Update the current speed based on PID values
    motor->speedValueCmd += (error);

    // Set the new speed to the motor
    if (!motor->setpointReached) {
        SetMotorSpeed(motor->idMotor, motor->speedValueCmd);
    }
}

int8_t initMotorControl()
{
    if (leftMotor == NULL || rightMotor == NULL) {
        X_LOG_TRACE("Failed to allocate memory for motor structures");
        return -1; // Memory allocation error   
    }

    // Destruction si on n'a pas arrêté proprement avant
    mq_unlink(ALAMBIX_BARTENDER_MQ);

    // Création d'un lien local vers la queue afin de gérer les droits
    mqd_t mq;

    // configuration de la file de message
    attr.mq_maxmsg = MQ_MSG_MAX;
	attr.mq_msgsize = MQ_MSG_SIZE;

    // création/ouverture de la file de message
	if ((mq = mq_open(ALAMBIX_BARTENDER_MQ,O_RDONLY | O_CREAT, 0644, &attr)) == -1) {
		perror("mq_open()");
		exit(EXIT_FAILURE);
	}

    // Fermeture du lien local vers la queue
    mq_close(mq);


    // Initialize the motors
    leftMotor->idMotor = MRPIZ_MOTOR_LEFT;
    leftMotor->move = false;
    leftMotor->encoderValue = GetMotorEncoderValues(leftMotor->idMotor);
    leftMotor->speedValueCurent = 0;
    leftMotor->speedValueCmd = 0;
    leftMotor->speedValueTarget = MOTOR_SPEED_STOP;
    leftMotor->valueP = 0;
    leftMotor->valueI = 0;
    leftMotor->valueD = 0;

    rightMotor->idMotor = MRPIZ_MOTOR_RIGHT;
    rightMotor->move = false;
    rightMotor->encoderValue = GetMotorEncoderValues(rightMotor->idMotor);
    rightMotor->speedValueCurent = 0;
    rightMotor->speedValueCmd = 0;
    rightMotor->speedValueTarget = MOTOR_SPEED_STOP;
    rightMotor->valueP = 0;
    rightMotor->valueI = 0;
    rightMotor->valueD = 0;


    int32_t l_iRet = REGULATOR_MANAGER_OK;

    //Initialize the regulator
    l_iRet = osTaskInit(&s_tRegulatorManager.t_tTaskHandler);
    if (l_iRet != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("Error initializing task");
        return l_iRet;
    }

    motorManager_t sMotorManager = {
        .leftMotor = leftMotor,
        .rightMotor = rightMotor
    };

    s_tRegulatorManager.t_tTaskHandler.t_ptTask = regulatorManagerTask;
    s_tRegulatorManager.t_tTaskHandler.t_ptTaskArg = sMotorManager;
    s_tRegulatorManager.t_tTaskHandler.t_ulStackSize = OS_TASK_DEFAULT_STACK_SIZE;
    s_tRegulatorManager.t_tTaskHandler.a_iStopFlag = OS_TASK_SECURE_FLAG;
    
}

int8_t setLeftMotorSpeed(int16_t speed) 
{
    //TODO: Set the speed of the left motor
    
    // Check if the speed is within the valid range
    if (speed < MOTOR_SPEED_MIN || speed > MOTOR_SPEED_MAX) {
        X_LOG_TRACE("Invalid speed value for left motor: %d. Must be between %d and %d.", speed, MOTOR_SPEED_MIN, MOTOR_SPEED_MAX);
        return -2;       
    }

    //X_ASSERT(speed >= (-1*MOTOR_SPEED_MAX) && speed <= MOTOR_SPEED_MAX);

    int8_t speedValue = speed * 100 / MOTOR_SPEED_MAX; // Scale speed to 0-100 range


    // Set the speed of the left motor
    return mrpiz_motor_set(MRPIZ_MOTOR_LEFT, (int)speedValue);
}

int8_t setRightMotorSpeed(int16_t speed) 
{ 
    //TODO: Set the speed of the right motor
    // Check if the speed is within the valid range
    if (speed < MOTOR_SPEED_MIN || speed > MOTOR_SPEED_MAX) {
        X_LOG_TRACE("Invalid speed value for right motor: %d. Must be between %d and %d.", speed, MOTOR_SPEED_MIN, MOTOR_SPEED_MAX);
        return -2;       
    }


    //X_ASSERT(speed >= (-1*MOTOR_SPEED_MAX) && speed <= MOTOR_SPEED_MAX);
    int8_t speedValue = speed * 100 / MOTOR_SPEED_MAX; // Scale speed to 0-100 range
    
    // Set the speed of the left motor 
    return mrpiz_motor_set(MRPIZ_MOTOR_RIGHT, (int)speedValue);
}

void stopMotor() 
{
    //TODO: Stop both motors
    mrpiz_motor_set(MRPIZ_MOTOR_BOTH, (int)MOTOR_SPEED_STOP);
}

bool checkRightMotorSetpointReached()
{
    X_ASSERT(rightMotor != NULL);
    //add mutex

    return rightMotor->setpointReached;
}
bool checkLeftMotorSetpointReached()
{
    X_ASSERT(leftMotor != NULL);

    return leftMotor->setpointReached;
}
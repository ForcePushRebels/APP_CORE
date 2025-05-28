////////////////////////////////////////////////////////////
// os Task src file 
// defines the os function for task manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 28/05/2025 - NASA JPL compliance improvements
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xTask.h"
#include "xLog.h"
#include <errno.h>
#include <signal.h>
#include <time.h>

// Forward declarations for helper functions 
static int validateTimeout(int p_iTimeout);
static int validateStackSize(size_t p_ulStackSize);
#ifdef OS_USE_RT_SCHEDULING
static int validatePriority(int p_iPriority);
#endif // OS_USE_RT_SCHEDULING

////////////////////////////////////////////////////////////
/// osTaskInit 
////////////////////////////////////////////////////////////
int osTaskInit(xOsTaskCtx *p_pttOSTask)
{
    X_ASSERT(p_pttOSTask != NULL);
    
    XOS_SECURE_BUFFER_INIT(p_pttOSTask, sizeof(xOsTaskCtx));

    p_pttOSTask->t_ulStackSize = OS_TASK_DEFAULT_STACK_SIZE;
    
#ifdef OS_USE_RT_SCHEDULING
    p_pttOSTask->t_policy = OS_DEFAULT_SCHED_POLICY;
    p_pttOSTask->t_iPriority = OS_TASK_DEFAULT_PRIORITY;
#endif
    
    p_pttOSTask->t_iState = OS_TASK_STATUS_READY;
    p_pttOSTask->t_iExitCode = OS_TASK_EXIT_SUCCESS;
    
    atomic_init(&p_pttOSTask->a_iStopFlag, OS_TASK_SECURE_FLAG);

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskCreate 
////////////////////////////////////////////////////////////
int osTaskCreate(xOsTaskCtx *p_pttOSTask)
{
    X_ASSERT(p_pttOSTask != NULL);

    void *(*l_ptTaskFunc)(void *) = p_pttOSTask->t_ptTask;
    if (l_ptTaskFunc == NULL)
    {
        return OS_TASK_ERROR_INVALID_PARAM;
    }

    int l_iStackValidation = validateStackSize(p_pttOSTask->t_ulStackSize);
    if (l_iStackValidation != OS_TASK_SUCCESS)
    {
        return l_iStackValidation;
    }

#ifdef OS_USE_RT_SCHEDULING

    int l_iPriorityValidation = validatePriority(p_pttOSTask->t_iPriority);
    if (l_iPriorityValidation != OS_TASK_SUCCESS)
    {
        return l_iPriorityValidation;
    }
#endif

    // Initialize pthread attributes
    pthread_attr_t l_tAttr;
    if (pthread_attr_init(&l_tAttr) != 0)
    {
        return OS_TASK_ERROR_INIT_FAILED;
    }

    // Configure the thread attributes with error checking
    if (pthread_attr_setdetachstate(&l_tAttr, PTHREAD_CREATE_JOINABLE) != 0)
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_INIT_FAILED;
    }

    size_t l_ulStackSize = p_pttOSTask->t_ulStackSize;
    
    // Ensure minimum stack size compliance
    if ((long int)l_ulStackSize < PTHREAD_STACK_MIN)
    {
        X_LOG_TRACE("Stack size adjusted to minimum: %ld", (long)PTHREAD_STACK_MIN);
        l_ulStackSize = PTHREAD_STACK_MIN;
        p_pttOSTask->t_ulStackSize = l_ulStackSize;
    }

    if (pthread_attr_setstacksize(&l_tAttr, l_ulStackSize) != 0)
    {
        X_LOG_TRACE("pthread_attr_setstacksize failed with error: %s", strerror(errno));
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_STACK_SIZE;
    }

#ifdef OS_USE_RT_SCHEDULING
    // Configure priority and scheduling policy with validation
    int l_iPolicy;
    

    t_SchedPolicy l_eTaskPolicy = p_pttOSTask->t_policy;
    

    switch (l_eTaskPolicy)
    {
    case OS_SCHED_FIFO:
        l_iPolicy = SCHED_FIFO;
        break;
    case OS_SCHED_RR:
        l_iPolicy = SCHED_RR;
        break;
    case OS_SCHED_BATCH:
        l_iPolicy = SCHED_BATCH;
        break;
    case OS_SCHED_IDLE:
        l_iPolicy = SCHED_IDLE;
        break;
    default:
        l_iPolicy = SCHED_OTHER;
        break;
    }

    if (pthread_attr_setschedpolicy(&l_tAttr, l_iPolicy) != 0)
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_POLICY;
    }

    p_pttOSTask->t_sched_param.sched_priority = p_pttOSTask->t_iPriority;
    if (pthread_attr_setschedparam(&l_tAttr, &p_pttOSTask->t_sched_param) != 0)
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_PRIORITY;
    }

    // Ensure that the scheduling attributes are respected
    if (pthread_attr_setinheritsched(&l_tAttr, PTHREAD_EXPLICIT_SCHED) != 0)
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_INIT_FAILED;
    }
#endif

    // Create the thread with error checking
    int l_iReturn = pthread_create(&p_pttOSTask->t_tHandle, 
                                   &l_tAttr,
                                   l_ptTaskFunc, 
                                   p_pttOSTask->t_ptTaskArg);
    
    // Clean up attributes immediately after use
    pthread_attr_destroy(&l_tAttr);

    if (l_iReturn != 0)
    {
        return OS_TASK_ERROR_CREATE_FAILED;
    }

    // Update task state and ID
    p_pttOSTask->t_iState = OS_TASK_STATUS_RUNNING;
    p_pttOSTask->t_iId = (int)(uintptr_t)p_pttOSTask->t_tHandle;

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskEnd 
////////////////////////////////////////////////////////////
int osTaskEnd(xOsTaskCtx *p_pttOSTask)
{
    X_ASSERT(p_pttOSTask != NULL);

    int l_iCurrentState = p_pttOSTask->t_iState;
    if (l_iCurrentState == OS_TASK_STATUS_TERMINATED)
    {
        return OS_TASK_SUCCESS; 
    }

    // Check if the thread is still valid with bounds checking
    pthread_t l_tHandle = p_pttOSTask->t_tHandle;
    if (pthread_kill(l_tHandle, 0) != 0)
    {
        // Thread already terminated
        p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        return OS_TASK_SUCCESS;
    }

    // Request the thread cancellation
    int l_iReturn = pthread_cancel(l_tHandle);
    if (l_iReturn != 0 && l_iReturn != ESRCH)
    { 
        // ESRCH = No such process
        return OS_TASK_ERROR_TERMINATE_FAILED;
    }

    // Join with timeout to avoid indefinite blocking
    l_iReturn = pthread_join(l_tHandle, NULL);


    if (l_iReturn != 0 && l_iReturn != ESRCH)
    {
        // If the join fails but the thread still exists
        p_pttOSTask->t_iState = OS_TASK_STATUS_BLOCKED;
        return OS_TASK_ERROR_JOIN_FAILED;
    }

    p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskGetStatus 
////////////////////////////////////////////////////////////
int osTaskGetStatus(xOsTaskCtx *p_pttOSTask)
{
    X_ASSERT(p_pttOSTask != NULL);

    int l_iCurrentState = p_pttOSTask->t_iState;
    
    // If the task is not yet terminated, check its current state
    if (l_iCurrentState != OS_TASK_STATUS_TERMINATED)
    {
        pthread_t l_tHandle = p_pttOSTask->t_tHandle;
        
        // Check if the thread is still active
        int l_iReturn = pthread_kill(l_tHandle, 0);

        // ESRCH means "No such process" - the thread no longer exists
        if (l_iReturn == ESRCH)
        {
            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        }
        // Another error occurred during the check
        else if (l_iReturn != 0)
        {
            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        }

        // Additional validation for handle validity
        if (p_pttOSTask->t_iState != OS_TASK_STATUS_TERMINATED)
        {
            // We can also check if the handle attribute is valid
            if (l_tHandle == 0 || l_tHandle == (pthread_t)-1)
            {
                p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
            }
        }
    }

    return p_pttOSTask->t_iState;
}

////////////////////////////////////////////////////////////
/// osTaskWait 
////////////////////////////////////////////////////////////
int osTaskWait(xOsTaskCtx *p_pttOSTask, void **p_pvExitValue)
{
    X_ASSERT(p_pttOSTask != NULL);

    int l_iCurrentState = p_pttOSTask->t_iState;
    
    // If the task is already terminated, no need to wait
    if (l_iCurrentState == OS_TASK_STATUS_TERMINATED)
    {
        return OS_TASK_SUCCESS;
    }

    // Rule 6: Minimal scope variables
    void *l_pvThreadResult = NULL;
    

    pthread_t l_tHandle = p_pttOSTask->t_tHandle;
    
    int l_iReturn = pthread_join(l_tHandle, &l_pvThreadResult);


    if (l_iReturn != 0)
    {
        return OS_TASK_ERROR_JOIN_FAILED;
    }

    p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;

    // Set exit code based on result
    if (l_pvThreadResult == PTHREAD_CANCELED)
    {
        p_pttOSTask->t_iExitCode = OS_TASK_EXIT_FAILURE;
    }
    else
    {
        p_pttOSTask->t_iExitCode = (int)(intptr_t)l_pvThreadResult;
    }


    if (p_pvExitValue != NULL)
    {
        *p_pvExitValue = l_pvThreadResult;
    }

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskStop 
////////////////////////////////////////////////////////////
int osTaskStop(xOsTaskCtx *p_pttOSTask, int p_iTimeout)
{
    X_ASSERT(p_pttOSTask != NULL);
    
    int l_iTimeoutValidation = validateTimeout(p_iTimeout);
    if (l_iTimeoutValidation != OS_TASK_SUCCESS)
    {
        return l_iTimeoutValidation;
    }

    int l_iCurrentState = p_pttOSTask->t_iState;
    
    // If the task is already terminated, no need to stop it
    if (l_iCurrentState == OS_TASK_STATUS_TERMINATED)
    {
        return OS_TASK_SUCCESS;
    }

    pthread_t l_tHandle = p_pttOSTask->t_tHandle;
    
    // Check if the thread is still valid
    int l_iReturn = pthread_kill(l_tHandle, 0);
    if (l_iReturn == ESRCH) // ESRCH = No such process
    {
        // Thread already terminated
        p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        return OS_TASK_SUCCESS;
    }
    else if (l_iReturn != 0)
    {
        return OS_TASK_ERROR_NOT_RUNNING;
    }

    // Set the stop flag atomically
    atomic_store(&p_pttOSTask->a_iStopFlag, OS_TASK_STOP_REQUEST);

    // If timeout is 0, don't wait
    if (p_iTimeout <= 0)
    {
        return OS_TASK_SUCCESS;
    }

    // Rule 2: Fixed bounds for loops - calculate maximum iterations
    const int l_iSleepIntervalMs = OS_TASK_SLEEP_INTERVAL_MS;
    int l_iMaxIterations = (p_iTimeout * 1000) / l_iSleepIntervalMs;
    
    // Rule 3: Ensure we don't exceed maximum iterations bound
    if (l_iMaxIterations > OS_TASK_MAX_STOP_ITERATIONS)
    {
        l_iMaxIterations = OS_TASK_MAX_STOP_ITERATIONS;
    }
    
    // Wait for the task to terminate with fixed loop bounds
    for (int i = 0; i < l_iMaxIterations; i++)
    {
        // Check if the task has terminated
        l_iReturn = pthread_kill(l_tHandle, 0);
        if (l_iReturn == ESRCH)
        {
            // Thread naturally terminated
            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
            return OS_TASK_SUCCESS;
        }

        // Wait before next check
        struct timespec l_tSleepTime;
        l_tSleepTime.tv_sec = 0;
        l_tSleepTime.tv_nsec = l_iSleepIntervalMs * 1000000; // Convert ms to ns
        nanosleep(&l_tSleepTime, NULL);
    }
    
    // If we reach here, timeout was reached - force termination
    int l_iEndResult = osTaskEnd(p_pttOSTask);
    if (l_iEndResult != OS_TASK_SUCCESS)
    {
        return OS_TASK_ERROR_TIMEOUT;
    }
    return l_iEndResult;
}

////////////////////////////////////////////////////////////
/// osTaskGetErrorString 
////////////////////////////////////////////////////////////
const char *osTaskGetErrorString(int p_iErrorCode)
{

    switch (p_iErrorCode)
    {
    case OS_TASK_SUCCESS:
        return "Success";
    case OS_TASK_ERROR_INVALID_PARAM:
        return "Invalid parameter";
    case OS_TASK_ERROR_INIT_FAILED:
        return "Initialization failed";
    case OS_TASK_ERROR_CREATE_FAILED:
        return "Task creation failed";
    case OS_TASK_ERROR_ALREADY_RUNNING:
        return "Task already running";
    case OS_TASK_ERROR_NOT_RUNNING:
        return "Task not running";
    case OS_TASK_ERROR_TERMINATE_FAILED:
        return "Task termination failed";
    case OS_TASK_ERROR_JOIN_FAILED:
        return "Task join failed";
    case OS_TASK_ERROR_TIMEOUT:
        return "Timeout expired";
    case OS_TASK_ERROR_PRIORITY:
        return "Invalid task priority";
    case OS_TASK_ERROR_STACK_SIZE:
        return "Invalid stack size";
    case OS_TASK_ERROR_POLICY:
        return "Invalid scheduling policy";
    default:
        return "Unknown error code";
    }
}

////////////////////////////////////////////////////////////
/// validateTimeout
////////////////////////////////////////////////////////////
static int validateTimeout(int p_iTimeout)
{

    if (p_iTimeout < 0 || p_iTimeout > OS_TASK_MAX_TIMEOUT_SEC)
    {
        return OS_TASK_ERROR_TIMEOUT;
    }
    
    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// validateStackSize
////////////////////////////////////////////////////////////
static int validateStackSize(size_t p_ulStackSize)
{
    if ((long int)p_ulStackSize < (long int)OS_TASK_MIN_STACK_SIZE || 
        (long int)p_ulStackSize > (long int)OS_TASK_MAX_STACK_SIZE)
    {
        return OS_TASK_ERROR_STACK_SIZE;
    }
    
    return OS_TASK_SUCCESS;
}

#ifdef OS_USE_RT_SCHEDULING
////////////////////////////////////////////////////////////
/// validatePriority
////////////////////////////////////////////////////////////
static int validatePriority(int p_iPriority)
{

    if (p_iPriority < OS_TASK_LOWEST_PRIORITY || 
        p_iPriority > OS_TASK_HIGHEST_PRIORITY)
    {
        return OS_TASK_ERROR_PRIORITY;
    }
    
    return OS_TASK_SUCCESS;
}
#endif // OS_USE_RT_SCHEDULING

////////////////////////////////////////////////////////////
// EXAMPLE OF SAFE TASK STOPPING MECHANISM USAGE
//
// For the safe stop mechanism to work, tasks must
// periodically check the a_iStopFlag.
// Here's an example of a well-designed task:
//
// void* myTaskFunction(void* arg) {
//     xOsTaskCtx* self = (xOsTaskCtx*)arg;
//
//     // Resource initialization
//     FILE* file = fopen("data.txt", "r");
//     void* resource = malloc(1024);
//
//     // Main task loop
//     while (atomic_load(&self->a_iStopFlag) == OS_TASK_SECURE_FLAG) {
//         // Normal processing
//
//         // Periodically check stop flag
//     }
//
//     // Clean up resources before terminating
//     if (file) fclose(file);
//     if (resource) free(resource);
//
//     return (void*)(intptr_t)OS_TASK_EXIT_SUCCESS;
// }
////////////////////////////////////////////////////////////

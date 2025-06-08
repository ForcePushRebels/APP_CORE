////////////////////////////////////////////////////////////
// os task src file
// defines the os linux function for task manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xTask.h"
#include "xLog.h"
#include <errno.h>
#include <signal.h>
#include <time.h>

////////////////////////////////////////////////////////////
/// osTaskInit
////////////////////////////////////////////////////////////
int osTaskInit(xOsTaskCtx *p_pttOSTask)
{
    if (p_pttOSTask == NULL)
    {
        return OS_TASK_ERROR_NULL_POINTER;
    }

    memset(p_pttOSTask, 0, sizeof(xOsTaskCtx));
    p_pttOSTask->t_ulStackSize = OS_TASK_DEFAULT_STACK_SIZE;
#ifdef OS_USE_RT_SCHEDULING
    p_pttOSTask->policy = OS_DEFAULT_SCHED_POLICY;
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
    if (p_pttOSTask == NULL)
    {
        return OS_TASK_ERROR_NULL_POINTER;
    }

    if (p_pttOSTask->t_ptTask == NULL)
    {
        return OS_TASK_ERROR_INVALID_PARAM;
    }

    if (p_pttOSTask->t_ulStackSize == 0)
    {
        return OS_TASK_ERROR_STACK_SIZE;
    }

    // Check t_iPriority values
#ifdef OS_USE_RT_SCHEDULING
    if (p_pttOSTask->t_iPriority < OS_TASK_LOWEST_PRIORITY || p_pttOSTask->t_iPriority > OS_TASK_HIGHEST_PRIORITY)
    {
        return OS_TASK_ERROR_PRIORITY;
    }
#endif

    pthread_attr_t l_tAttr;
    if (pthread_attr_init(&l_tAttr) != 0)
    {
        return OS_TASK_ERROR_INIT_FAILED;
    }

    // Configure the thread attributes
    if (pthread_attr_setdetachstate(&l_tAttr, PTHREAD_CREATE_JOINABLE) != 0)
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_INIT_FAILED;
    }

    // check if the stack size is greater than the default stack size
    if ((long int)p_pttOSTask->t_ulStackSize < PTHREAD_STACK_MIN)
    {
        X_LOG_TRACE("pthread_attr_setstacksize: Stack size is less than the default stack size");
        p_pttOSTask->t_ulStackSize = PTHREAD_STACK_MIN;
    }

    if (pthread_attr_setstacksize(&l_tAttr, p_pttOSTask->t_ulStackSize) != 0)
    {
        X_LOG_TRACE("pthread_attr_setstacksize failed with error: %s", strerror(errno));
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_STACK_SIZE;
    }

    // Configure the t_iPriority and scheduling policy
#ifdef OS_USE_RT_SCHEDULING
    // For systems supporting real-time
    int policy;
    switch (p_pttOSTask->policy)
    {
        case OS_SCHED_FIFO:
            policy = SCHED_FIFO;
            break;
        case OS_SCHED_RR:
            policy = SCHED_RR;
            break;
        case OS_SCHED_BATCH:
            policy = SCHED_BATCH;
            break;
        case OS_SCHED_IDLE:
            policy = SCHED_IDLE;
            break;
        default:
            policy = SCHED_OTHER;
            break;
    }

    if (pthread_attr_setschedpolicy(&l_tAttr, policy) != 0)
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR_POLICY;
    }

    p_pttOSTask->sched_param.sched_priority = p_pttOSTask->t_iPriority;
    if (pthread_attr_setschedparam(&l_tAttr, &p_pttOSTask->sched_param) != 0)
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

    // Create the thread
    int l_iReturn = pthread_create(&p_pttOSTask->t_tHandle, &l_tAttr, p_pttOSTask->t_ptTask, p_pttOSTask->t_ptTaskArg);
    pthread_attr_destroy(&l_tAttr);

    if (l_iReturn != 0)
    {
        return OS_TASK_ERROR_CREATE_FAILED;
    }

    p_pttOSTask->t_iState = OS_TASK_STATUS_RUNNING;
    p_pttOSTask->t_iId = (int)(uintptr_t)p_pttOSTask->t_tHandle;

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskEnd
////////////////////////////////////////////////////////////
int osTaskEnd(xOsTaskCtx *p_pttOSTask)
{
    if (p_pttOSTask == NULL)
    {
        return OS_TASK_ERROR_NULL_POINTER;
    }

    if (p_pttOSTask->t_iState == OS_TASK_STATUS_TERMINATED)
    {
        return OS_TASK_SUCCESS; // Already terminated, no error
    }

    // Check if the thread is still valid
    if (pthread_kill(p_pttOSTask->t_tHandle, 0) != 0)
    {
        // Thread already terminated
        p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        return OS_TASK_SUCCESS;
    }

    // Request the thread cancellation
    int l_iReturn = pthread_cancel(p_pttOSTask->t_tHandle);
    if (l_iReturn != 0 && l_iReturn != ESRCH)
    { // ESRCH = No such process
        return OS_TASK_ERROR_TERMINATE_FAILED;
    }

    // Assign a timeout for the join to avoid blocking
    struct timespec timeout;
    clock_gettime(CLOCK_REALTIME, &timeout);
    timeout.tv_sec += 2; // 2 seconds timeout

    l_iReturn = pthread_join(p_pttOSTask->t_tHandle, NULL);

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
    if (p_pttOSTask == NULL)
    {
        return OS_TASK_ERROR_NULL_POINTER;
    }

    // If the t_ptTask is not yet terminated, check its current t_iState
    if (p_pttOSTask->t_iState != OS_TASK_STATUS_TERMINATED)
    {
        // Check if the thread is still active
        int l_iReturn = pthread_kill(p_pttOSTask->t_tHandle, 0);

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

        if (p_pttOSTask->t_iState != OS_TASK_STATUS_TERMINATED)
        {

            // We can also check if the handle attribute is valid
            if (p_pttOSTask->t_tHandle == 0 || p_pttOSTask->t_tHandle == (pthread_t)-1)
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
    if (p_pttOSTask == NULL)
    {
        return OS_TASK_ERROR_NULL_POINTER;
    }

    // If the t_ptTask is already terminated, no need to wait
    if (p_pttOSTask->t_iState == OS_TASK_STATUS_TERMINATED)
    {
        return OS_TASK_SUCCESS;
    }

    void *threadResult = NULL;
    int l_iReturn = pthread_join(p_pttOSTask->t_tHandle, &threadResult);

    if (l_iReturn != 0)
    {
        return OS_TASK_ERROR_JOIN_FAILED;
    }

    p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;

    if (threadResult == PTHREAD_CANCELED)
    {
        p_pttOSTask->t_iExitCode = OS_TASK_EXIT_FAILURE;
    }
    else
    {
        p_pttOSTask->t_iExitCode = (int)(intptr_t)threadResult;
    }

    if (p_pvExitValue != NULL)
    {
        *p_pvExitValue = threadResult;
    }

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskStop
////////////////////////////////////////////////////////////
int osTaskStop(xOsTaskCtx *p_pttOSTask, int p_iTimeout)
{
    if (p_pttOSTask == NULL)
    {
        return OS_TASK_ERROR_NULL_POINTER;
    }

    // check if the thread is already marked as terminated, if so, do nothing.
    if (p_pttOSTask->t_iState == OS_TASK_STATUS_TERMINATED)
    {
        return OS_TASK_SUCCESS;
    }

    // check if the thread exists really at the operating system level.
    int l_iReturn = pthread_kill(p_pttOSTask->t_tHandle, 0);
    if (l_iReturn == ESRCH) // ESRCH = No such process
    {
        pthread_join(p_pttOSTask->t_tHandle, NULL); 
        p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        return OS_TASK_SUCCESS;
    }

    else if (l_iReturn != 0)
    {
        return OS_TASK_ERROR_NOT_RUNNING;
    }

    atomic_store(&p_pttOSTask->a_iStopFlag, OS_TASK_STOP_REQUEST);

    if (p_iTimeout <= 0)
    {
        return OS_TASK_SUCCESS;
    }

    time_t start_time = time(NULL);

    while ((time(NULL) - start_time) < p_iTimeout)
    {
        // check if the thread has terminated.
        l_iReturn = pthread_kill(p_pttOSTask->t_tHandle, 0);
        if (l_iReturn == ESRCH)
        {
            pthread_join(p_pttOSTask->t_tHandle, NULL);

            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
            X_LOG_TRACE("Task ID %d terminated gracefully.", p_pttOSTask->t_iId);
            return OS_TASK_SUCCESS; 
        }

        // wait a short time before checking again to avoid saturating the CPU.
        struct timespec ts = {.tv_sec = 0, .tv_nsec = 100 * 1000 * 1000}; // 100 ms
        nanosleep(&ts, NULL);
    }

    X_LOG_TRACE("Task ID %d did not stop gracefully. Forcing termination.", p_pttOSTask->t_iId);
    osTaskEnd(p_pttOSTask); 

    return OS_TASK_ERROR_TIMEOUT; 
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
        case OS_TASK_ERROR_NULL_POINTER:
            return "Null pointer provided";
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
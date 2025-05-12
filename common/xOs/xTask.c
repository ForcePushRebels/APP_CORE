////////////////////////////////////////////////////////////
// os t_ptTask src file
// defines the os function for t_ptTask manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xTask.h"
#include <errno.h>
#include <signal.h>  
#include <time.h>

////////////////////////////////////////////////////////////
/// osTaskInit
////////////////////////////////////////////////////////////
int osTaskInit(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL) 
    {
        return OS_TASK_ERROR;
    }

    memset(p_pttOSTask, 0, sizeof(t_TaskCtx));
    p_pttOSTask->t_ulStackSize = OS_TASK_DEFAULT_STACK_SIZE;
#ifdef OS_USE_RT_SCHEDULING
    p_pttOSTask->policy = OS_DEFAULT_SCHED_POLICY;
#endif
    p_pttOSTask->t_iState = OS_TASK_STATUS_READY;
    p_pttOSTask->t_iExitCode = OS_TASK_EXIT_SUCCESS;
    atomic_init(&p_pttOSTask->a_iStopFlag, 0);

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskCreate
////////////////////////////////////////////////////////////
int osTaskCreate(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL || p_pttOSTask->t_ptTask == NULL ||
        p_pttOSTask->t_ulStackSize == 0) 
    {
        return OS_TASK_ERROR;
    }

    // Check t_iPriority values
#ifdef OS_USE_RT_SCHEDULING
    if (p_pttOSTask->t_iPriority < OS_TASK_LOWEST_PRIORITY || 
        p_pttOSTask->t_iPriority > OS_TASK_HIGHEST_PRIORITY) 
    {
        return OS_TASK_ERROR;
    }
#endif

    pthread_attr_t l_tAttr;
    if (pthread_attr_init(&l_tAttr) != 0) 
    {
        return OS_TASK_ERROR;
    }

    // Configure the thread attributes
    if (pthread_attr_setdetachstate(&l_tAttr, PTHREAD_CREATE_JOINABLE) != 0) 
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR;
    }

    if (pthread_attr_setstacksize(&l_tAttr, p_pttOSTask->t_ulStackSize) != 0) 
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR;
    }

    // Configure the t_iPriority and scheduling policy
#ifdef OS_USE_RT_SCHEDULING
    // For systems supporting real-time
    int policy;
    switch (p_pttOSTask->policy) {
        case OS_SCHED_FIFO:  policy = SCHED_FIFO; break;
        case OS_SCHED_RR:    policy = SCHED_RR; break;
        case OS_SCHED_BATCH: policy = SCHED_BATCH; break;
        case OS_SCHED_IDLE:  policy = SCHED_IDLE; break;
        default:             policy = SCHED_OTHER; break;
    }
    
    if (pthread_attr_setschedpolicy(&l_tAttr, policy) != 0) 
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR;
    }
    
    p_pttOSTask->sched_param.sched_priority = p_pttOSTask->t_iPriority;
    if (pthread_attr_setschedparam(&l_tAttr, &p_pttOSTask->sched_param) != 0) 
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR;
    }
    
    // Ensure that the scheduling attributes are respected
    if (pthread_attr_setinheritsched(&l_tAttr, PTHREAD_EXPLICIT_SCHED) != 0) 
    {
        pthread_attr_destroy(&l_tAttr);
        return OS_TASK_ERROR;
    }
#endif

    // Create the thread
    int ret = pthread_create(&p_pttOSTask->t_tHandle, &l_tAttr,
        p_pttOSTask->t_ptTask, p_pttOSTask->t_ptTaskArg);
    pthread_attr_destroy(&l_tAttr);

    if (ret != 0) 
    {
        return OS_TASK_ERROR;
    }

    p_pttOSTask->t_iState = OS_TASK_STATUS_RUNNING;
    p_pttOSTask->t_iId = (int)(uintptr_t)p_pttOSTask->t_tHandle;

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskEnd
////////////////////////////////////////////////////////////
int osTaskEnd(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL) 
    {
        return OS_TASK_ERROR;
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
    int ret = pthread_cancel(p_pttOSTask->t_tHandle);
    if (ret != 0 && ret != ESRCH) { // ESRCH = No such process
        return OS_TASK_ERROR;
    }

    // Assign a timeout for the join to avoid blocking
    struct timespec timeout;
    clock_gettime(CLOCK_REALTIME, &timeout);
    timeout.tv_sec += 2; // 2 seconds timeout

    ret = pthread_join(p_pttOSTask->t_tHandle, NULL);
    
    if (ret != 0 && ret != ESRCH) 
    {
        // If the join fails but the thread still exists
        p_pttOSTask->t_iState = OS_TASK_STATUS_BLOCKED;
        return OS_TASK_ERROR;
    }

    p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskGetStatus
////////////////////////////////////////////////////////////
int osTaskGetStatus(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL) {
        return OS_TASK_ERROR;
    }
    
    // If the t_ptTask is not yet terminated, check its current t_iState
    if (p_pttOSTask->t_iState != OS_TASK_STATUS_TERMINATED) 
    {
        // Check if the thread is still active
        int kill_result = pthread_kill(p_pttOSTask->t_tHandle, 0);
        
        // ESRCH means "No such process" - the thread no longer exists
        if (kill_result == ESRCH) 
        {
            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        }
        // Another error occurred during the check
        else if (kill_result != 0) 
        {
            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        }
        
        if (p_pttOSTask->t_iState != OS_TASK_STATUS_TERMINATED) {
            
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
int osTaskWait(t_TaskCtx* p_pttOSTask, void** p_pvExitValue)
{
    if (p_pttOSTask == NULL) 
    {
        return OS_TASK_ERROR;
    }

    // If the t_ptTask is already terminated, no need to wait
    if (p_pttOSTask->t_iState == OS_TASK_STATUS_TERMINATED) 
    {
        return OS_TASK_SUCCESS;
    }

    void* threadResult = NULL;
    int ret = pthread_join(p_pttOSTask->t_tHandle, &threadResult);
    
    if (ret != 0) 
    {
        return OS_TASK_ERROR;
    }
    
    p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
    
    if (threadResult == PTHREAD_CANCELED) {
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
int osTaskStop(t_TaskCtx* p_pttOSTask, int timeout_seconds)
{
    if (p_pttOSTask == NULL) 
    {
        return OS_TASK_ERROR;
    }

    // If the task is already terminated, no need to stop it
    if (p_pttOSTask->t_iState == OS_TASK_STATUS_TERMINATED) 
    {
        return OS_TASK_SUCCESS;
    }

    // Check if the thread is still valid
    int kill_result = pthread_kill(p_pttOSTask->t_tHandle, 0);
    if (kill_result == ESRCH) // ESRCH = No such process
    {
        // Thread already terminated
        p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
        return OS_TASK_SUCCESS;
    }
    else if (kill_result != 0)
    {
        return OS_TASK_ERROR;
    }

    // Set the stop flag
    p_pttOSTask->a_iStopFlag = OS_TASK_STOP_REQUEST;

    // If timeout is 0, don't wait
    if (timeout_seconds <= 0)
    {
        return OS_TASK_SUCCESS;
    }

    // Wait for the task to terminate with timeout
    time_t start_time = time(NULL);
    time_t current_time;
    
    while (1)
    {
        // Check if the task has terminated
        kill_result = pthread_kill(p_pttOSTask->t_tHandle, 0);
        if (kill_result == ESRCH)
        {
            // Thread naturally terminated
            p_pttOSTask->t_iState = OS_TASK_STATUS_TERMINATED;
            return OS_TASK_SUCCESS;
        }
        
        // Check the timeout
        current_time = time(NULL);
        if ((current_time - start_time) >= timeout_seconds)
        {
            // If timeout is reached, force task termination
            return osTaskEnd(p_pttOSTask);
        }
        
        // Wait a short moment before testing again
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 100 * 1000000; // 100 ms
        nanosleep(&ts, NULL);
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
//     t_TaskCtx* self = (t_TaskCtx*)arg;
//     
//     // Resource initialization
//     FILE* file = fopen("data.txt", "r");
//     void* resource = malloc(1024);
//     
//     // Main task loop
//     while (atomic_load(&self->a_iStopFlag) != OS_TASK_STOP_REQUEST) {
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

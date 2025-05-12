////////////////////////////////////////////////////////////
// os Task header file
// defines the os function for task manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
////////////////////////////////////////////////////////////

#pragma once
#ifndef OS_TASK_H_
#define OS_TASK_H_

#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include "xAssert.h"  


// Return codes
#define OS_TASK_SUCCESS         0xE2F41A50
#define OS_TASK_ERROR           0xE2F41A51

// Task states
#define OS_TASK_STATUS_READY       0UL
#define OS_TASK_STATUS_RUNNING     1UL
#define OS_TASK_STATUS_BLOCKED     2UL
#define OS_TASK_STATUS_SUSPENDED   3UL
#define OS_TASK_STATUS_TERMINATED  4UL

// Task exit codes
#define OS_TASK_EXIT_SUCCESS 0xE2F41A60
#define OS_TASK_EXIT_FAILURE 0xE2F41A61

// Task stop codes
#define OS_TASK_STOP_REQUEST  0xE2F41A00    // Stop request
#define OS_TASK_SECURE_FLAG   0xE2F41A01    // Security flag for stopping
#define OS_TASK_STOP_TIMEOUT  5UL           // Timeout in seconds for graceful shutdown

typedef enum {
    OS_SCHED_NORMAL = 0,  // SCHED_OTHER - Standard scheduling
    OS_SCHED_FIFO,        // SCHED_FIFO - Real-time FIFO
    OS_SCHED_RR,          // SCHED_RR - Real-time Round Robin
    OS_SCHED_BATCH,       // SCHED_BATCH - Batch processing
    OS_SCHED_IDLE         // SCHED_IDLE - Very low priority
} t_SchedPolicy;

#ifdef OS_USE_RT_SCHEDULING
#define OS_DEFAULT_SCHED_POLICY OS_SCHED_FIFO
// For Linux with real-time policy
#define OS_TASK_LOWEST_PRIORITY  1  // Minimum for SCHED_FIFO/RR
#define OS_TASK_HIGHEST_PRIORITY 99 // Maximum for SCHED_FIFO/RR
#define OS_TASK_DEFAULT_PRIORITY 50 // Median value
#else
#define OS_DEFAULT_SCHED_POLICY OS_SCHED_NORMAL
// For normal scheduling (nice values)
#define OS_TASK_LOWEST_PRIORITY  19  // Lowest priority (nice +19)
#define OS_TASK_HIGHEST_PRIORITY -20 // Highest priority (nice -20)
#define OS_TASK_DEFAULT_PRIORITY 0   // Normal priority (nice 0)
#endif

// Default stack size: 8 MB
#define OS_TASK_DEFAULT_STACK_SIZE (8 * 1024 * 1024)

//////////////////////////////////
// Task management structure
//////////////////////////////////
typedef struct {
    void* (*t_ptTask)(void*);           // Pointer to the task function
    void* t_ptTaskArg;                  // Task arguments
    int t_iPriority;                    // Task priority (for RT, defined via SCHED_RR; for normal, this value is fixed)
    size_t t_ulStackSize;               // Stack size
    int t_iId;                          // Task ID (internal attribute, do not modify)
    int t_iState;                       // Task status
    int t_iExitCode;                    // Task exit code
    pthread_t t_tHandle;                // pthread thread handle
    atomic_int a_iStopFlag;             // Stop request flag for graceful shutdown
#ifdef OS_USE_RT_SCHEDULING
    struct sched_param t_sched_param;   // Scheduling parameters
    t_SchedPolicy t_policy;             // Scheduling policy
#endif
} t_TaskCtx;

//////////////////////////////////
/// @brief Initialise a task context with default values
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, OS_TASK_ERROR otherwise
//////////////////////////////////
int osTaskInit(t_TaskCtx* p_pttOSTask);

//////////////////////////////////
/// @brief Create a task
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, OS_TASK_ERROR otherwise
/// 
/// @note create a task with the given parameters
/// @note the task is created with the default stack size
/// @pre configuration of the context structure
//////////////////////////////////
int osTaskCreate(t_TaskCtx* p_pttOSTask);

//////////////////////////////////
/// @brief End a task (unsafe method)
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, OS_TASK_ERROR otherwise
/// @deprecated Use osTaskStop() instead
//////////////////////////////////
int osTaskEnd(t_TaskCtx* p_pttOSTask);

//////////////////////////////////
/// @brief Safely stop a task
/// @param p_pttOSTask : pointer to the task structure context
/// @param timeout_seconds : timeout in seconds to wait for graceful termination (0 = no timeout)
/// @return OS_TASK_SUCCESS if success, OS_TASK_ERROR otherwise
/// 
/// @note The task should periodically check the a_iStopFlag in its execution loop
//////////////////////////////////
int osTaskStop(t_TaskCtx* p_pttOSTask, int timeout_seconds);

//////////////////////////////////
/// @brief Get the task t_iState
/// @param p_pttOSTask : pointer to the task structure context
/// @return Task t_iState or OS_TASK_ERROR if error
//////////////////////////////////
int osTaskGetStatus(t_TaskCtx* p_pttOSTask);

//////////////////////////////////
/// @brief Wait for a task to complete
/// @param p_pttOSTask : pointer to the task structure context
/// @param p_pvExitValue : pointer to store the exit value (can be NULL)
/// @return OS_TASK_SUCCESS if success, OS_TASK_ERROR otherwise
//////////////////////////////////
int osTaskWait(t_TaskCtx* p_pttOSTask, void** p_pvExitValue);

//////////////////////////////////
/// @brief Get task exit code
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, OS_TASK_ERROR otherwise
//////////////////////////////////
int osTaskGetExitCode(t_TaskCtx* p_pttOSTask);      //this function will be deleted don't use it 

#endif // OS_TASK_H_

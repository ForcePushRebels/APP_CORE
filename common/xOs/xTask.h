////////////////////////////////////////////////////////////
// os Task header file
// defines the os function for task manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
////////////////////////////////////////////////////////////

#ifndef OS_TASK_H_
#define OS_TASK_H_

#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <ulimit.h>
#include "xAssert.h"

// Return codes
#define OS_TASK_SUCCESS 0x2F41A50

#define OS_TASK_ERROR_NULL_POINTER 0x2F41A52
#define OS_TASK_ERROR_INVALID_PARAM 0x2F41A53
#define OS_TASK_ERROR_INIT_FAILED 0x2F41A54
#define OS_TASK_ERROR_CREATE_FAILED 0x2F41A55
#define OS_TASK_ERROR_ALREADY_RUNNING 0x2F41A56
#define OS_TASK_ERROR_NOT_RUNNING 0x2F41A57
#define OS_TASK_ERROR_TERMINATE_FAILED 0x2F41A58
#define OS_TASK_ERROR_JOIN_FAILED 0x2F41A59
#define OS_TASK_ERROR_TIMEOUT 0x2F41A5A
#define OS_TASK_ERROR_PRIORITY 0x2F41A5B
#define OS_TASK_ERROR_STACK_SIZE 0x2F41A5C
#define OS_TASK_ERROR_POLICY 0x2F41A5D

// Task states
#define OS_TASK_STATUS_READY 0UL
#define OS_TASK_STATUS_RUNNING 1UL
#define OS_TASK_STATUS_BLOCKED 2UL
#define OS_TASK_STATUS_SUSPENDED 3UL
#define OS_TASK_STATUS_TERMINATED 4UL

// Task exit codes
#define OS_TASK_EXIT_SUCCESS 0x2F41A60
#define OS_TASK_EXIT_FAILURE 0x2F41A61

// Task stop codes
#define OS_TASK_STOP_REQUEST 0x2F41A00 // Stop request
#define OS_TASK_SECURE_FLAG 0x2F41A01  // Security flag for stopping
#define OS_TASK_STOP_TIMEOUT 5UL       // Timeout in seconds for graceful shutdown

typedef enum
{
    OS_SCHED_NORMAL = 0, // SCHED_OTHER - Standard scheduling
    OS_SCHED_FIFO,       // SCHED_FIFO - Real-time FIFO
    OS_SCHED_RR,         // SCHED_RR - Real-time Round Robin
    OS_SCHED_BATCH,      // SCHED_BATCH - Batch processing
    OS_SCHED_IDLE        // SCHED_IDLE - Very low priority
} t_SchedPolicy;

#ifdef OS_USE_RT_SCHEDULING
#define OS_DEFAULT_SCHED_POLICY OS_SCHED_FIFO
// For Linux with real-time policy
#define OS_TASK_LOWEST_PRIORITY 1   // Minimum for SCHED_FIFO/RR
#define OS_TASK_HIGHEST_PRIORITY 99 // Maximum for SCHED_FIFO/RR
#define OS_TASK_DEFAULT_PRIORITY 50 // Median value
#else
#define OS_DEFAULT_SCHED_POLICY OS_SCHED_NORMAL
// For normal scheduling (nice values)
#define OS_TASK_LOWEST_PRIORITY 19   // Lowest priority (nice +19)
#define OS_TASK_HIGHEST_PRIORITY -20 // Highest priority (nice -20)
#define OS_TASK_DEFAULT_PRIORITY 0   // Normal priority (nice 0)
#endif

// Default stack size: 8 MB
#define OS_TASK_DEFAULT_STACK_SIZE PTHREAD_STACK_MIN

//////////////////////////////////
// Task management structure
//////////////////////////////////
typedef struct xos_task_ctx_t
{
    void *(*t_ptTask)(void *); // Pointer to the task function
    void *t_ptTaskArg;         // Task arguments
    int t_iPriority;           // Task priority (for RT, defined via SCHED_RR; for normal, this value is fixed)
    size_t t_ulStackSize;      // Stack size
    int t_iId;                 // Task ID (internal attribute, do not modify)
    int t_iState;              // Task status
    int t_iExitCode;           // Task exit code
    pthread_t t_tHandle;       // pthread thread handle
    atomic_int a_iStopFlag;    // Stop request flag for graceful shutdown
#ifdef OS_USE_RT_SCHEDULING
    struct sched_param t_sched_param; // Scheduling parameters
    t_SchedPolicy t_policy;           // Scheduling policy
#endif
} xOsTaskCtx;

//////////////////////////////////
/// @brief Initialise a task context with default values
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
//////////////////////////////////
int osTaskInit(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Create a task
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
///
/// @note create a task with the given parameters
/// @note the task is created with the default stack size
/// @pre configuration of the context structure
//////////////////////////////////
int osTaskCreate(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief End a task (unsafe method)
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @deprecated Use osTaskStop() instead
//////////////////////////////////
int osTaskEnd(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Safely stop a task
/// @param p_pttOSTask : pointer to the task structure context
/// @param p_iTimeout : timeout in seconds to wait for graceful termination (0 = no timeout)
/// @return OS_TASK_SUCCESS if success, error code otherwise
///
/// @note The task should periodically check the a_iStopFlag in its execution loop
//////////////////////////////////
int osTaskStop(xOsTaskCtx *p_pttOSTask, int p_iTimeout);

//////////////////////////////////
/// @brief Get the task t_iState
/// @param p_pttOSTask : pointer to the task structure context
/// @return Task t_iState or error code
//////////////////////////////////
int osTaskGetStatus(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Wait for a task to complete
/// @param p_pttOSTask : pointer to the task structure context
/// @param p_pvExitValue : pointer to store the exit value (can be NULL)
/// @return OS_TASK_SUCCESS if success, error code otherwise
//////////////////////////////////
int osTaskWait(xOsTaskCtx *p_pttOSTask, void **p_pvExitValue);

//////////////////////////////////
/// @brief Get task exit code
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
//////////////////////////////////
int osTaskGetExitCode(xOsTaskCtx *p_pttOSTask); // this function will be deleted don't use it

//////////////////////////////////
/// @brief Convertir un code d'erreur en message texte
/// @param p_iErrorCode : code d'erreur à convertir
/// @return Chaîne de caractères décrivant l'erreur
//////////////////////////////////
const char *osTaskGetErrorString(int p_iErrorCode);

#endif // OS_TASK_H_

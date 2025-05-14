////////////////////////////////////////////////////////////
//  Watchdog src file
//  Provides watchdog support using POSIX timer
//
// Written : 02/05/2025
////////////////////////////////////////////////////////////


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/signal.h>

#include "watchdog.h"
#include "xLog.h"
#include "xOsMutex.h"

// Global variables
static watchdog_t g_watchdog;
static int g_is_initialized = 0;
static void (*g_expiry_handler)(void) = NULL;

// Private variables for POSIX timers
static timer_t g_timer_id;
static struct sigevent g_sev;
static struct itimerspec g_its;

////////////////////////////////////////////////////////////
/// watchdog_thread
////////////////////////////////////////////////////////////
void* watchdog_thread(void *arg)
{
   watchdog_t *watchdog = (watchdog_t *)arg;
   int l_iRet = 0;
   
   if (!watchdog) {
       X_LOG_TRACE("Watchdog thread started with NULL context");
       return (void*)(intptr_t)WATCHDOG_ERROR_INVALID_PARAM;
   }
   
   X_LOG_TRACE("Watchdog thread started");
   
   // Main thread loop
   while (!watchdog->terminate) {
       // Ping the watchdog to reset the timer
       l_iRet = watchdog_ping();
       if (l_iRet != WATCHDOG_SUCCESS) {
           X_LOG_TRACE("Watchdog ping failed in thread: %s (code: 0x%x)", 
                     watchdog_get_error_string(l_iRet), l_iRet);
       }
       
       // Wait some time before the next ping
       // Use about 1/3 of the timeout to ensure multiple pings before expiration
       usleep((watchdog->timeout / 3) * 1000);
   }
   
   X_LOG_TRACE("Watchdog thread terminated normally");
   return (void*)(intptr_t)WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// timer_handler
////////////////////////////////////////////////////////////
static void timer_handler(union sigval sv)
{
    watchdog_t *watchdog = (watchdog_t *)sv.sival_ptr;
    if (!watchdog) {
        X_LOG_TRACE("Timer handler called with NULL context");
        return;
    }

    // Protect access with mutex
    if ((unsigned int)mutexLock(&watchdog->mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in timer handler");
        return;
    }

    // Check if the watchdog is still initialized
    if (!g_is_initialized)
    {
        mutexUnlock(&watchdog->mutex);
        return;
    }

    // Mark the watchdog as expired
    watchdog->should_reset = true;

    // Call the expiry handler if defined
    if (g_expiry_handler != NULL)
    {
        g_expiry_handler();
    }
    else
    {
        X_LOG_TRACE("WATCHDOG TIMEOUT - System will restart");
        // In production environment, use an appropriate method to restart
        // the system rather than abruptly exiting
        mutexUnlock(&watchdog->mutex);
        exit(EXIT_FAILURE);
    }

    mutexUnlock(&watchdog->mutex);
}

////////////////////////////////////////////////////////////
/// watchdog_init
////////////////////////////////////////////////////////////
int watchdog_init(int timeout_ms)
{
    unsigned long l_ulReturn;
    // Check if already initialized
    if (g_is_initialized)
    {
        X_LOG_TRACE("Watchdog already initialized");
        return WATCHDOG_ERROR_ALREADY_INIT;
    }

    // Timeout configuration
    uint32_t actual_timeout = timeout_ms > 0 ? timeout_ms : WATCHDOG_DEFAULT_TIMEOUT;

    // Watchdog structure initialization
    memset(&g_watchdog, 0, sizeof(watchdog_t));
    g_watchdog.timeout = actual_timeout;
    g_watchdog.should_reset = false;
    g_watchdog.is_running = 0;
    g_watchdog.terminate = 0;

    // Mutex initialization
    if ((unsigned int)mutexCreate(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to create watchdog mutex");
        return WATCHDOG_ERROR_MUTEX_FAILED;
    }

    // Event configuration for the timer
    memset(&g_sev, 0, sizeof(struct sigevent));
    g_sev.sigev_notify = SIGEV_THREAD;           // Uses a thread to handle the event
    g_sev.sigev_notify_function = timer_handler; // Callback function
    g_sev.sigev_value.sival_ptr = &g_watchdog;   // Pass the watchdog structure to the callback
    g_sev.sigev_notify_attributes = NULL;        // Use default attributes

    // Timer creation
    if (timer_create(CLOCK_MONOTONIC, &g_sev, &g_timer_id) != 0)
    {
        X_LOG_TRACE("Failed to create timer: %s", strerror(errno));
        mutexDestroy(&g_watchdog.mutex);
        return WATCHDOG_ERROR_TIMER_CREATE;
    }
    
    // Store the timer ID in the watchdog structure
    g_watchdog.timer_id = (void*)g_timer_id;

    g_is_initialized = 1;
    X_LOG_TRACE("POSIX timer watchdog initialized (timeout=%dms)", actual_timeout);

    // Create the watchdog thread
    l_ulReturn = osTaskInit(&g_watchdog.task_ctx);
    if (l_ulReturn != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to initialize watchdog task: %s (code: 0x%x)", 
                   osTaskGetErrorString(l_ulReturn), l_ulReturn);
        timer_delete(g_timer_id);
        mutexDestroy(&g_watchdog.mutex);
        g_is_initialized = 0;  
        return WATCHDOG_ERROR_THREAD_FAILED;
    }
    
    g_watchdog.task_ctx.t_iPriority = 1;        
    g_watchdog.task_ctx.t_ulStackSize = PTHREAD_STACK_MIN; 
    g_watchdog.task_ctx.t_ptTask = watchdog_thread;
    g_watchdog.task_ctx.t_ptTaskArg = &g_watchdog;
    
    l_ulReturn = osTaskCreate(&g_watchdog.task_ctx);
    if (l_ulReturn != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to create watchdog thread: %s (code: 0x%x)", 
                   osTaskGetErrorString(l_ulReturn), l_ulReturn);
        timer_delete(g_timer_id);
        mutexDestroy(&g_watchdog.mutex);
        g_is_initialized = 0;
        return WATCHDOG_ERROR_THREAD_CREATE;
    }
    
    g_watchdog.is_running = 1;
    
    // Activate the watchdog immediately
    if (watchdog_ping() != WATCHDOG_SUCCESS) 
    {
        X_LOG_TRACE("Failed to start watchdog timer");
        g_watchdog.terminate = 1;
        osTaskEnd(&g_watchdog.task_ctx);
        timer_delete(g_timer_id);
        mutexDestroy(&g_watchdog.mutex);
        g_is_initialized = 0;
        return WATCHDOG_ERROR_TIMER_SET;
    }

    return WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// watchdog_stop
////////////////////////////////////////////////////////////
void watchdog_stop(void)
{
    // Check if initialized
    if (!g_is_initialized)
    {
        return;
    }

    // Protect access with mutex
    if ((unsigned int)mutexLock(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in watchdog_stop");
        return;
    }

    // Signal the thread to terminate
    g_watchdog.terminate = 1;
    
    // Mark as uninitialized to prevent new entries
    g_is_initialized = 0;

    // Disarm the timer
    struct itimerspec its;
    memset(&its, 0, sizeof(struct itimerspec));

    if (timer_settime(g_timer_id, 0, &its, NULL) != 0)
    {
        X_LOG_TRACE("Failed to stop timer: %s", strerror(errno));
    }
    
    // Release the mutex to allow the thread to terminate
    mutexUnlock(&g_watchdog.mutex);
    
    if (g_watchdog.is_running) {
        // Wait for the thread to terminate properly (with a timeout)
        int wait_count = 0;
        while (g_watchdog.is_running && wait_count < 10) {
            usleep(100000); // 100ms
            wait_count++;
        }
        
        // If the thread is still running, terminate it
        if (g_watchdog.is_running) {
            X_LOG_TRACE("Forcing watchdog thread termination");
            osTaskEnd(&g_watchdog.task_ctx);
        }
    }

    // Delete the timer
    if (timer_delete(g_timer_id) != 0)
    {
        X_LOG_TRACE("Failed to delete timer: %s", strerror(errno));
    }

    // Destroy the mutex
    mutexDestroy(&g_watchdog.mutex);

    X_LOG_TRACE("Watchdog stopped");
}

////////////////////////////////////////////////////////////
/// watchdog_ping
////////////////////////////////////////////////////////////
int watchdog_ping(void)
{
    // Check if initialized
    if (!g_is_initialized)
    {
        X_LOG_TRACE("Watchdog not initialized");
        return WATCHDOG_ERROR_NOT_INIT;
    }

    // Protect access with mutex
    if ((unsigned int)mutexLock(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in watchdog_ping");
        return WATCHDOG_ERROR_MUTEX_FAILED;
    }

    // Configure timer for single-shot (no repetition)
    memset(&g_its, 0, sizeof(struct itimerspec));
    g_its.it_value.tv_sec = g_watchdog.timeout / 1000;
    g_its.it_value.tv_nsec = (g_watchdog.timeout % 1000) * 1000000;
    g_its.it_interval.tv_sec = 0;
    g_its.it_interval.tv_nsec = 0;

    // Arm/rearm the timer
    if (timer_settime(g_timer_id, 0, &g_its, NULL) != 0)
    {
        X_LOG_TRACE("Failed to set timer: %s", strerror(errno));
        mutexUnlock(&g_watchdog.mutex);
        return WATCHDOG_ERROR_TIMER_SET;
    }

    mutexUnlock(&g_watchdog.mutex);
    return WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// watchdog_has_expired
////////////////////////////////////////////////////////////
bool watchdog_has_expired(void)
{
    // Check if initialized
    if (!g_is_initialized)
    {
        X_LOG_TRACE("Watchdog not initialized when checking expiry");
        return false;
    }

    bool has_expired = false;

    // Protect access with mutex if initialized
    if ((unsigned int)mutexLock(&g_watchdog.mutex) == MUTEX_OK)
    {
        has_expired = g_watchdog.should_reset;
        mutexUnlock(&g_watchdog.mutex);
    }
    else 
    {
        X_LOG_TRACE("Failed to lock mutex in watchdog_has_expired: %s",
                  watchdog_get_error_string(WATCHDOG_ERROR_MUTEX_FAILED));
    }

    return has_expired;
}

////////////////////////////////////////////////////////////
/// watchdog_set_expiry_handler
////////////////////////////////////////////////////////////
void watchdog_set_expiry_handler(void (*callback)(void))
{
    // Protect access with mutex if initialized
    if (g_is_initialized && (unsigned int)mutexLock(&g_watchdog.mutex) == MUTEX_OK)
    {
        g_expiry_handler = callback;
        mutexUnlock(&g_watchdog.mutex);
    }
    else
    {
        g_expiry_handler = callback;
    }
}

////////////////////////////////////////////////////////////
/// watchdog_get_error_string
////////////////////////////////////////////////////////////
const char* watchdog_get_error_string(int error_code)
{
    switch (error_code)
    {
        case WATCHDOG_SUCCESS:              return "Success";
        case WATCHDOG_ERROR_INIT_FAILED:    return "Watchdog initialization failed";
        case WATCHDOG_ERROR_ALREADY_INIT:   return "Watchdog already initialized";
        case WATCHDOG_ERROR_NOT_INIT:       return "Watchdog not initialized";
        case WATCHDOG_ERROR_MUTEX_FAILED:   return "Watchdog mutex operation failed";
        case WATCHDOG_ERROR_TIMER_CREATE:   return "Failed to create watchdog timer";
        case WATCHDOG_ERROR_TIMER_SET:      return "Failed to set/start watchdog timer";
        case WATCHDOG_ERROR_THREAD_CREATE:  return "Failed to create watchdog thread";
        case WATCHDOG_ERROR_THREAD_FAILED:  return "Watchdog thread operation failed";
        case WATCHDOG_ERROR_INVALID_PARAM:  return "Invalid parameter for watchdog";
        default:                            return "Unknown watchdog error";
    }
}

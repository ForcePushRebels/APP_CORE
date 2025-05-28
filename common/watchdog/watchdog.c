////////////////////////////////////////////////////////////
//  Watchdog src file - NASA JPL 10 Rules Compliant 
//  Provides ultra-fast  watchdog support using POSIX timer
//
// general disclosure: copy or share the file is forbidden
// Written : 02/05/2025
// Modified: 28/05/2025 - Security improvements , optimization for maximum performance²
// Intellectual property of Christophe Benedetti
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
#include "xAssert.h"

//  Global variables with atomic initialization
static watchdog_t g_watchdog;
static atomic_int g_is_initialized = ATOMIC_VAR_INIT(0);
static atomic_uintptr_t g_expiry_handler = ATOMIC_VAR_INIT(0); //  callback storage

// Private variables for POSIX timers
static timer_t g_timer_id;
static struct sigevent s_tSignalEvent;

// Forward declarations for helper functions
static int validateTimeout(int p_iTimeout);
static int validateWatchdogState(void);
static void cleanupWatchdogResources(void);
static inline uint64_t getCurrentTimeMs(void);

////////////////////////////////////////////////////////////
/// watchdog_thread
////////////////////////////////////////////////////////////
void* watchdog_thread(void *arg)
{
    X_ASSERT(arg != NULL);
    
    watchdog_t *l_pWatchdog = (watchdog_t *)arg;
    if (!l_pWatchdog) {
        X_LOG_TRACE(" Watchdog thread started with NULL context");
        return (void*)(intptr_t)WATCHDOG_ERROR_INVALID_PARAM;
    }
    
    X_LOG_TRACE(" Watchdog thread started - MAXIMUM PERFORMANCE MODE");

    // Cache values locally for ultra-fast performance
    unsigned long l_ulCachedTimeout = atomic_load_explicit(&l_pWatchdog->a_ulTimeout, memory_order_acquire);
    unsigned int l_uSleepTime = (l_ulCachedTimeout / 4) * 1000; // Faster ping rate for better responsiveness
    
    // Local cache variables to minimize atomic operations
    int l_iStopFlag = atomic_load_explicit(&l_pWatchdog->task_ctx.a_iStopFlag, memory_order_acquire);
    int l_iTerminate = atomic_load_explicit(&l_pWatchdog->a_iTerminate, memory_order_acquire);
    int l_iCycleCounter = 0;
    
    // Main loop with optimized atomic operations
    while (l_iStopFlag == OS_TASK_SECURE_FLAG && !l_iTerminate)
    {
        // ULTRA-FAST ping - no mutex, pure atomic operation
        int l_iRet = watchdog_ping();
        if (l_iRet != WATCHDOG_SUCCESS) {
            X_LOG_TRACE(" Watchdog ping failed in thread: %s (code: 0x%x)", 
                      watchdog_get_error_string(l_iRet), l_iRet);
        }
        
        // Sleep with cached value for maximum performance
        if (l_uSleepTime > 0) {
            usleep(l_uSleepTime);
        }
        
        // Optimize cache refresh - only every N cycles
        if (++l_iCycleCounter >= WATCHDOG_CACHE_REFRESH_CYCLES) {
            l_iCycleCounter = 0;
            
            // Refresh cached values with relaxed ordering for speed
            unsigned long l_ulNewTimeout = atomic_load_explicit(&l_pWatchdog->a_ulTimeout, memory_order_relaxed);
            if (l_ulNewTimeout != l_ulCachedTimeout) {
                l_ulCachedTimeout = l_ulNewTimeout;
                l_uSleepTime = (l_ulCachedTimeout / 4) * 1000;
            }
            
            // Refresh control flags with acquire ordering
            l_iStopFlag = atomic_load_explicit(&l_pWatchdog->task_ctx.a_iStopFlag, memory_order_acquire);
            l_iTerminate = atomic_load_explicit(&l_pWatchdog->a_iTerminate, memory_order_acquire);
        }
    }
    
    // Signal thread termination
    atomic_store_explicit(&l_pWatchdog->a_iIsRunning, 0, memory_order_release);
    
    X_LOG_TRACE(" Watchdog thread terminated - ULTRA-FAST shutdown");
    return (void*)(intptr_t)WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// timer_handler
////////////////////////////////////////////////////////////
static void timer_handler(union sigval sv)
{
    watchdog_t *l_pWatchdog = (watchdog_t *)sv.sival_ptr;
    if (!l_pWatchdog) {
        X_LOG_TRACE(" Timer handler called with NULL context");
        return;
    }

    // Check if still initialized with fast acquire ordering
    if (!atomic_load_explicit(&g_is_initialized, memory_order_acquire)) {
        return;
    }

    // Atomically mark as expired and reset flag with release ordering
    atomic_store_explicit(&l_pWatchdog->a_iExpired, 1, memory_order_release);
    atomic_store_explicit(&l_pWatchdog->a_bShouldReset, true, memory_order_release);

    // Get callback handler with atomic load
    uintptr_t l_handlerAddr = atomic_load_explicit(&g_expiry_handler, memory_order_acquire);
    void (*l_pHandler)(void) = (void(*)(void))l_handlerAddr;
    
    if (l_pHandler != NULL) {
        l_pHandler();
    } else {
        X_LOG_TRACE(" WATCHDOG TIMEOUT - REAL TIME ERROR - ULTRA-FAST RESPONSE");
        exit(EXIT_FAILURE);
    }
}

////////////////////////////////////////////////////////////
/// watchdog_init
////////////////////////////////////////////////////////////
int watchdog_init(int timeout_ms)
{
    // Check if already initialized with fast acquire ordering
    if (atomic_load_explicit(&g_is_initialized, memory_order_acquire)) {
        X_LOG_TRACE(" Watchdog already initialized");
        return WATCHDOG_ERROR_ALREADY_INIT;
    }

    int l_iValidation = validateTimeout(timeout_ms);
    if (l_iValidation != WATCHDOG_SUCCESS) {
        return l_iValidation;
    }

    uint32_t l_ulActualTimeout = (timeout_ms > 0) ? (uint32_t)timeout_ms : WATCHDOG_DEFAULT_TIMEOUT;

    // Securely initialize watchdog structure
    XOS_MEMORY_SANITIZE(&g_watchdog, sizeof(watchdog_t));
    
    // Initialize atomic variables with relaxed ordering for speed
    atomic_init(&g_watchdog.a_ulTimeout, l_ulActualTimeout);
    atomic_init(&g_watchdog.a_bShouldReset, false);
    atomic_init(&g_watchdog.a_iIsRunning, 0);
    atomic_init(&g_watchdog.a_iTerminate, 0);
    atomic_init(&g_watchdog.a_ulTimerArmed, 0);
    atomic_init(&g_watchdog.a_iExpired, 0);

    // Securely initialize signal event structure
    XOS_MEMORY_SANITIZE(&s_tSignalEvent, sizeof(struct sigevent));
    s_tSignalEvent.sigev_notify = SIGEV_THREAD;
    s_tSignalEvent.sigev_notify_function = timer_handler;
    s_tSignalEvent.sigev_value.sival_ptr = &g_watchdog;
    s_tSignalEvent.sigev_notify_attributes = NULL;

    // Timer creation with error checking
    if (timer_create(CLOCK_MONOTONIC, &s_tSignalEvent, &g_timer_id) != 0) {
        X_LOG_TRACE(" Failed to create timer: %s", strerror(errno));
        return WATCHDOG_ERROR_TIMER_CREATE;
    }
    
    // Store the timer ID in the watchdog structure
    g_watchdog.timer_id = (void*)g_timer_id;

    // Create and configure the watchdog thread
    unsigned long l_ulReturn = osTaskInit(&g_watchdog.task_ctx);
    if (l_ulReturn != OS_TASK_SUCCESS) {
        X_LOG_TRACE(" Failed to initialize watchdog task: %s (code: 0x%lx)", 
                   osTaskGetErrorString(l_ulReturn), l_ulReturn);
        cleanupWatchdogResources();
        return WATCHDOG_ERROR_THREAD_FAILED;
    }
    
    // Configure task parameters
    g_watchdog.task_ctx.t_iPriority = 1;        
    g_watchdog.task_ctx.t_ulStackSize = PTHREAD_STACK_MIN; 
    g_watchdog.task_ctx.t_ptTask = watchdog_thread;
    g_watchdog.task_ctx.t_ptTaskArg = &g_watchdog;
    
    l_ulReturn = osTaskCreate(&g_watchdog.task_ctx);
    if (l_ulReturn != OS_TASK_SUCCESS) {
        X_LOG_TRACE(" Failed to create watchdog thread: %s (code: 0x%lx)", 
                   osTaskGetErrorString(l_ulReturn), l_ulReturn);
        cleanupWatchdogResources();
        return WATCHDOG_ERROR_THREAD_CREATE;
    }
    
    // Group atomic stores for better cache coherency
    atomic_store_explicit(&g_watchdog.a_iIsRunning, 1, memory_order_relaxed);
    atomic_store_explicit(&g_is_initialized, 1, memory_order_release);
    
    // Activate the watchdog immediately
    if (watchdog_ping() != WATCHDOG_SUCCESS) {
        X_LOG_TRACE(" Failed to start watchdog timer");
        atomic_store_explicit(&g_watchdog.a_iTerminate, 1, memory_order_release);
        osTaskEnd(&g_watchdog.task_ctx);
        cleanupWatchdogResources();
        atomic_store_explicit(&g_is_initialized, 0, memory_order_release);
        return WATCHDOG_ERROR_TIMER_SET;
    }

    X_LOG_TRACE(" POSIX timer watchdog initialized (timeout=%ums) - MAXIMUM PERFORMANCE", l_ulActualTimeout);
    return WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// watchdog_stop
////////////////////////////////////////////////////////////
void watchdog_stop(void)
{
    // Check if initialized with fast acquire ordering
    if (!atomic_load_explicit(&g_is_initialized, memory_order_acquire)) {
        return;
    }

    // Atomically mark as uninitialized to prevent new entries
    atomic_store_explicit(&g_is_initialized, 0, memory_order_release);

    // Signal the thread to terminate atomically
    atomic_store_explicit(&g_watchdog.a_iTerminate, 1, memory_order_release);
    
    // Disarm the timer - direct timer operation
    struct itimerspec l_tIts;
    XOS_MEMORY_SANITIZE(&l_tIts, sizeof(struct itimerspec));

    if (timer_settime(g_timer_id, 0, &l_tIts, NULL) != 0) {
        X_LOG_TRACE(" Failed to stop timer: %s", strerror(errno));
    }
    
    // Wait for thread termination with cached state
    int l_iIsRunning = atomic_load_explicit(&g_watchdog.a_iIsRunning, memory_order_acquire);
    if (l_iIsRunning) {
        for (int i = 0; i < WATCHDOG_MAX_STOP_ITERATIONS; i++) {
            l_iIsRunning = atomic_load_explicit(&g_watchdog.a_iIsRunning, memory_order_relaxed);
            if (!l_iIsRunning) {
                break; // Thread terminated naturally
            }
            usleep(WATCHDOG_SLEEP_INTERVAL_MS * 1000); // 100ms
        }
        
        // If the thread is still running, force termination
        if (l_iIsRunning) {
            X_LOG_TRACE(" Forcing watchdog thread termination");
            osTaskEnd(&g_watchdog.task_ctx);
        }
    }

    // Clean up all resources
    cleanupWatchdogResources();
    
    X_LOG_TRACE(" Watchdog stopped - ULTRA-FAST shutdown");
}

////////////////////////////////////////////////////////////
/// watchdog_ping
////////////////////////////////////////////////////////////
int watchdog_ping(void)
{
    // Ultra-fast initialization check with acquire ordering
    if (!atomic_load_explicit(&g_is_initialized, memory_order_acquire)) {
        return WATCHDOG_ERROR_NOT_INIT;
    }

    // Validate watchdog state
    int l_iValidation = validateWatchdogState();
    if (l_iValidation != WATCHDOG_SUCCESS) {
        return l_iValidation;
    }

    // Get current timestamp for tracking
    uint64_t l_ulCurrentTime = getCurrentTimeMs();
    atomic_store_explicit(&g_watchdog.a_ulTimerArmed, l_ulCurrentTime, memory_order_relaxed);

    struct itimerspec l_tIts;
    XOS_MEMORY_SANITIZE(&l_tIts, sizeof(struct itimerspec));
    
    // Single atomic load with relaxed ordering for speed
    unsigned long l_ulTimeout = atomic_load_explicit(&g_watchdog.a_ulTimeout, memory_order_relaxed);
    l_tIts.it_value.tv_sec = l_ulTimeout / 1000;
    l_tIts.it_value.tv_nsec = (l_ulTimeout % 1000) * 1000000;
    l_tIts.it_interval.tv_sec = 0;
    l_tIts.it_interval.tv_nsec = 0;

    //Arm/rearm the timer - direct timer operation
    if (timer_settime(g_timer_id, 0, &l_tIts, NULL) != 0) {
        X_LOG_TRACE(" Failed to set timer: %s", strerror(errno));
        return WATCHDOG_ERROR_TIMER_SET;
    }

    atomic_store_explicit(&g_watchdog.a_iExpired, 0, memory_order_relaxed);

    return WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// watchdog_has_expired 
////////////////////////////////////////////////////////////
bool watchdog_has_expired(void)
{
    if (!atomic_load_explicit(&g_is_initialized, memory_order_acquire)) {
        return false;
    }

    return atomic_load_explicit(&g_watchdog.a_iExpired, memory_order_acquire) != 0;
}

////////////////////////////////////////////////////////////
/// watchdog_set_expiry_handler 
////////////////////////////////////////////////////////////
void watchdog_set_expiry_handler(void (*callback)(void))
{
    // : Ultra-fast atomic store of callback pointer
    uintptr_t l_handlerAddr = (uintptr_t)callback;
    atomic_store_explicit(&g_expiry_handler, l_handlerAddr, memory_order_release);
}

////////////////////////////////////////////////////////////
/// watchdog_get_error_string - NO CHANGE NEEDED
////////////////////////////////////////////////////////////
const char* watchdog_get_error_string(int error_code)
{
    // Rule 1: Simple control structures - single switch
    switch (error_code)
    {
        case WATCHDOG_SUCCESS:              return "Success";
        case WATCHDOG_ERROR_INIT_FAILED:    return "Watchdog initialization failed";
        case WATCHDOG_ERROR_ALREADY_INIT:   return "Watchdog already initialized";
        case WATCHDOG_ERROR_NOT_INIT:       return "Watchdog not initialized";
        case WATCHDOG_ERROR_TIMER_CREATE:   return "Failed to create watchdog timer";
        case WATCHDOG_ERROR_TIMER_SET:      return "Failed to set/start watchdog timer";
        case WATCHDOG_ERROR_THREAD_CREATE:  return "Failed to create watchdog thread";
        case WATCHDOG_ERROR_THREAD_FAILED:  return "Watchdog thread operation failed";
        case WATCHDOG_ERROR_INVALID_PARAM:  return "Invalid parameter for watchdog";
        default:                            return "Unknown watchdog error";
    }
}

////////////////////////////////////////////////////////////
/// getCurrentTimeMs -  Helper
////////////////////////////////////////////////////////////
static inline uint64_t getCurrentTimeMs(void)
{
    struct timespec l_ts;
    if (clock_gettime(CLOCK_MONOTONIC, &l_ts) != 0) {
        return 0;
    }
    return (uint64_t)(l_ts.tv_sec * 1000) + (uint64_t)(l_ts.tv_nsec / 1000000);
}

////////////////////////////////////////////////////////////
/// validateTimeout - NO CHANGE NEEDED
////////////////////////////////////////////////////////////
static int validateTimeout(int p_iTimeout)
{
    if (p_iTimeout < 0) {
        return WATCHDOG_ERROR_INVALID_PARAM;
    }
    
    if (p_iTimeout > 0 && 
        (p_iTimeout < WATCHDOG_MIN_TIMEOUT_MS || p_iTimeout > WATCHDOG_MAX_TIMEOUT_MS)) {
        return WATCHDOG_ERROR_INVALID_PARAM;
    }
    
    return WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// validateWatchdogState - 
////////////////////////////////////////////////////////////
static int validateWatchdogState(void)
{
    // : Check if watchdog structure is in valid state with acquire ordering
    if (!atomic_load_explicit(&g_is_initialized, memory_order_acquire)) {
        return WATCHDOG_ERROR_NOT_INIT;
    }
    
    return WATCHDOG_SUCCESS;
}

////////////////////////////////////////////////////////////
/// cleanupWatchdogResources - 
////////////////////////////////////////////////////////////
static void cleanupWatchdogResources(void)
{
    // Delete the timer if created
    if (timer_delete(g_timer_id) != 0) {
        X_LOG_TRACE(" Failed to delete timer: %s", strerror(errno));
    }

    // : No more mutex to destroy!
    
    // Securely clear the watchdog structure
    XOS_MEMORY_SANITIZE(&g_watchdog, sizeof(watchdog_t));
}

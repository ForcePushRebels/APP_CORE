////////////////////////////////////////////////////////////
//  Watchdog header file
//  Provides watchdog support using POSIX timer
//
// general disclosure: copy or share the file is forbidden
// Written : 02/05/2025
////////////////////////////////////////////////////////////

#ifndef WATCHDOG_H
#define WATCHDOG_H

#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include <signal.h>
#include <sys/types.h>
#include "xTask.h"
#include "xOsMutex.h"

// Définitions pour le timer
#define WATCHDOG_DEFAULT_TIMEOUT 100 // 100ms
#define WATCHDOG_DEVICE_NAME "watchdog"

////////////////////////////////////////////////////////////
/// @param watchdog_t
/// @brief Watchdog structure
////////////////////////////////////////////////////////////
typedef struct watchdog
{
    t_TaskCtx task_ctx;              // Contexte de tâche pour la gestion du watchdog
    void *timer_id;                  // Timer POSIX (void* pour éviter les dépendances de types)
    uint32_t timeout;                // Délai d'expiration en ms
    xOsMutexCtx mutex;               // Mutex pour protéger l'accès concurrent
    bool should_reset;               // Indique si un reset du système est nécessaire
    int is_running;                  // Indique si le thread watchdog est en cours d'exécution
    volatile sig_atomic_t terminate; // Signal pour arrêter proprement le thread
} watchdog_t;

//////////////////////////////////
/// @brief Watchdog thread function
/// @param arg : Thread argument pointer
/// @return Pointer for pthread compatibility
//////////////////////////////////
void *watchdog_thread(void *arg);

//////////////////////////////////
/// @brief Initialize the software watchdog with POSIX timer
/// @param timeout_ms : Expiration timeout in milliseconds (0 to use default)
/// @return 0 on success, -1 on error
//////////////////////////////////
int watchdog_init(int timeout_ms);

//////////////////////////////////
/// @brief Stop the watchdog and release resources
/// @return none
//////////////////////////////////
void watchdog_stop(void);

//////////////////////////////////
/// @brief Manually send a heartbeat signal to the watchdog
/// @details This signal resets the timer, preventing its expiration
/// @return 0 on success, -1 on error
//////////////////////////////////
int watchdog_ping(void);

//////////////////////////////////
/// @brief Check if the watchdog has expired
/// @return true if watchdog has expired, false otherwise
//////////////////////////////////
bool watchdog_has_expired(void);

//////////////////////////////////
/// @brief Set callback handler for watchdog expiration
/// @param callback : Function to call when watchdog expires
/// @return none
//////////////////////////////////
void watchdog_set_expiry_handler(void (*callback)(void));

#endif /* WATCHDOG_H */

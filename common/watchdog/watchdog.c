#define _POSIX_C_SOURCE 199309L
#define _BSD_SOURCE       // Pour usleep
#define _DEFAULT_SOURCE   // Alternative pour les systèmes plus récents

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
// Inclusions spécifiques pour les timers POSIX
#include <sys/signal.h>
// Inclusions locales
#include "watchdog.h"
#include "xLog.h"
#include "xOsMutex.h"

// Variables globales
static watchdog_t g_watchdog;
static int g_is_initialized = 0;
static void (*g_expiry_handler)(void) = NULL;

// Variables privées pour les timers POSIX
static timer_t g_timer_id;
static struct sigevent g_sev;
static struct itimerspec g_its;

// Implémentation de la fonction thread du watchdog
void* watchdog_thread(void *arg)
{
   watchdog_t *watchdog = (watchdog_t *)arg;
   int l_iRet = 0;
   
   if (!watchdog) {
       X_LOG_TRACE("Watchdog thread started with NULL context");
       return NULL;
   }
   
   X_LOG_TRACE("Watchdog thread started");
   
   // Boucle principale du thread
   while (!watchdog->terminate) {
       // Ping le watchdog pour réinitialiser le timer
       l_iRet = watchdog_ping();
       if (l_iRet != 0) {
           X_LOG_TRACE("Watchdog ping failed in thread");
       }
       
       // Attendre un certain temps avant le prochain ping
       // Utilisez environ 1/3 du timeout pour garantir plusieurs pings avant expiration
       usleep((watchdog->timeout / 3) * 1000);
   }
   
   X_LOG_TRACE("Watchdog thread terminated normally");
   return NULL;
}

// Fonction de callback pour le timer POSIX
static void timer_handler(union sigval sv)
{
    watchdog_t *watchdog = (watchdog_t *)sv.sival_ptr;
    if (!watchdog) {
        X_LOG_TRACE("Timer handler called with NULL context");
        return;
    }

    // Protéger l'accès avec le mutex
    if (mutexLock(&watchdog->mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in timer handler");
        return;
    }

    // Vérifier si le watchdog est toujours initialisé
    if (!g_is_initialized)
    {
        mutexUnlock(&watchdog->mutex);
        return;
    }

    // Marquer le watchdog comme ayant expiré
    watchdog->should_reset = true;

    // Appeler le handler d'expiration si défini
    if (g_expiry_handler != NULL)
    {
        g_expiry_handler();
    }
    else
    {
        X_LOG_TRACE("WATCHDOG TIMEOUT - System will restart");
        // En environnement de production, utilisez une méthode appropriée pour redémarrer
        // le système plutôt que de quitter brutalement
        mutexUnlock(&watchdog->mutex);
        exit(EXIT_FAILURE);
    }

    mutexUnlock(&watchdog->mutex);
}

// Initialisation du watchdog
int watchdog_init(int timeout_ms)
{
    // Vérifier si déjà initialisé
    if (g_is_initialized)
    {
        X_LOG_TRACE("Watchdog already initialized");
        return 0;
    }

    // Configuration du timeout
    uint32_t actual_timeout = timeout_ms > 0 ? timeout_ms : WATCHDOG_DEFAULT_TIMEOUT;

    // Initialisation de la structure watchdog
    memset(&g_watchdog, 0, sizeof(watchdog_t));
    g_watchdog.timeout = actual_timeout;
    g_watchdog.should_reset = false;
    g_watchdog.is_running = 0;
    g_watchdog.terminate = 0;

    // Initialisation du mutex
    if (mutexCreate(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to create watchdog mutex");
        return -1;
    }

    // Configuration de l'événement pour le timer
    memset(&g_sev, 0, sizeof(struct sigevent));
    g_sev.sigev_notify = SIGEV_THREAD;           // Utilise un thread pour traiter l'événement
    g_sev.sigev_notify_function = timer_handler; // Fonction de callback
    g_sev.sigev_value.sival_ptr = &g_watchdog;   // Passe la structure watchdog au callback
    g_sev.sigev_notify_attributes = NULL;        // Utilise les attributs par défaut

    // Création du timer
    if (timer_create(CLOCK_MONOTONIC, &g_sev, &g_timer_id) != 0)
    {
        X_LOG_TRACE("Failed to create timer: %s", strerror(errno));
        mutexDestroy(&g_watchdog.mutex);
        return -1;
    }
    
    // Stocker l'ID du timer dans la structure watchdog
    g_watchdog.timer_id = (void*)g_timer_id;

    g_is_initialized = 1;
    X_LOG_TRACE("POSIX timer watchdog initialized (timeout=%dms)", actual_timeout);

    // Créer le thread watchdog
    g_watchdog.task_ctx.priority = 1; // Haute priorité pour le watchdog
    g_watchdog.task_ctx.stack_size = 4096; // 4KB devrait être suffisant
    g_watchdog.task_ctx.task = watchdog_thread;
    g_watchdog.task_ctx.arg = &g_watchdog;
    
    int ret = osTaskCreate(&g_watchdog.task_ctx);
    if (ret != OS_TASK_SUCCESS) {
        X_LOG_TRACE("Failed to create watchdog thread: %d", ret);
        timer_delete(g_timer_id);
        mutexDestroy(&g_watchdog.mutex);
        g_is_initialized = 0;
        return -1;
    }
    
    g_watchdog.is_running = 1;
    
    // Activer le watchdog immédiatement
    if (watchdog_ping() != 0) {
        X_LOG_TRACE("Failed to start watchdog timer");
        g_watchdog.terminate = 1;
        osTaskEnd(&g_watchdog.task_ctx);
        timer_delete(g_timer_id);
        mutexDestroy(&g_watchdog.mutex);
        g_is_initialized = 0;
        return -1;
    }

    return 0;
}

// Arrêt du watchdog
void watchdog_stop(void)
{
    // Vérifier si initialisé
    if (!g_is_initialized)
    {
        return;
    }

    // Protéger l'accès avec le mutex
    if (mutexLock(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in watchdog_stop");
        return;
    }

    // Signaler au thread de terminer
    g_watchdog.terminate = 1;
    
    // Marquer comme non-initialisé pour éviter de nouvelles entrées
    g_is_initialized = 0;

    // Désarmer le timer
    struct itimerspec its;
    memset(&its, 0, sizeof(struct itimerspec));

    if (timer_settime(g_timer_id, 0, &its, NULL) != 0)
    {
        X_LOG_TRACE("Failed to stop timer: %s", strerror(errno));
    }
    
    // Libérer le mutex pour permettre au thread de se terminer
    mutexUnlock(&g_watchdog.mutex);
    
    if (g_watchdog.is_running) {
        // Attendre que le thread se termine proprement (avec un timeout)
        int wait_count = 0;
        while (g_watchdog.is_running && wait_count < 10) {
            usleep(100000); // 100ms
            wait_count++;
        }
        
        // Si le thread est toujours en cours, le terminer
        if (g_watchdog.is_running) {
            X_LOG_TRACE("Forcing watchdog thread termination");
            osTaskEnd(&g_watchdog.task_ctx);
        }
    }

    // Supprimer le timer
    if (timer_delete(g_timer_id) != 0)
    {
        X_LOG_TRACE("Failed to delete timer: %s", strerror(errno));
    }

    // Détruire le mutex
    mutexDestroy(&g_watchdog.mutex);

    X_LOG_TRACE("Watchdog stopped");
}

// Envoi manuel d'un heartbeat au watchdog (reset du timer)
int watchdog_ping(void)
{
    // Vérifier si initialisé
    if (!g_is_initialized)
    {
        return -1;
    }

    // Protéger l'accès avec le mutex
    if (mutexLock(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in watchdog_ping");
        return -1;
    }

    // Convertir le timeout en secondes/nanosecondes
    time_t seconds = g_watchdog.timeout / 1000;
    long nanoseconds = (g_watchdog.timeout % 1000) * 1000000L;

    // Configurer le timer pour single-shot (pas de répétition)
    memset(&g_its, 0, sizeof(struct itimerspec));
    g_its.it_value.tv_sec = seconds;
    g_its.it_value.tv_nsec = nanoseconds;
    g_its.it_interval.tv_sec = 0;
    g_its.it_interval.tv_nsec = 0;

    // Armer/réarmer le timer
    if (timer_settime(g_timer_id, 0, &g_its, NULL) != 0)
    {
        X_LOG_TRACE("Failed to arm timer: %s", strerror(errno));
        mutexUnlock(&g_watchdog.mutex);
        return -1;
    }

    mutexUnlock(&g_watchdog.mutex);
    return 0;
}

// Vérifie si le watchdog a expiré
bool watchdog_has_expired(void)
{
    // Vérifier si initialisé
    if (!g_is_initialized)
    {
        return false;
    }

    bool result = false;

    // Protéger l'accès avec le mutex
    if (mutexLock(&g_watchdog.mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to lock mutex in watchdog_has_expired");
        return false;
    }

    result = g_watchdog.should_reset;

    mutexUnlock(&g_watchdog.mutex);

    return result;
}

// Définit le handler de callback pour les expirations du watchdog
void watchdog_set_expiry_handler(void (*callback)(void))
{
    // Protéger l'accès avec le mutex si initialisé
    if (g_is_initialized)
    {
        if (mutexLock(&g_watchdog.mutex) != MUTEX_OK)
        {
            X_LOG_TRACE("Failed to lock mutex in watchdog_set_expiry_handler");
            return;
        }
    }

    g_expiry_handler = callback;

    if (g_is_initialized)
    {
        mutexUnlock(&g_watchdog.mutex);
    }
}

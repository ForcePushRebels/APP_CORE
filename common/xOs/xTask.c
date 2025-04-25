////////////////////////////////////////////////////////////
// os Task src file
// defines the os function for task manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
////////////////////////////////////////////////////////////

#include "xTask.h"
#include <errno.h>

// Fonction wrapper pour exécuter des tâches avec prise en charge de la suspension
static void* osTaskWrapper(void* arg) {
    t_TaskCtx* taskCtx = (t_TaskCtx*)arg;
    void* result = NULL;
    
    if (taskCtx == NULL || taskCtx->task == NULL) {
        return NULL;
    }
    
    // Boucle principale pour vérifier périodiquement la suspension
    while (1) {
        // Vérifier l'état de suspension
        int should_run = 0;
        pthread_mutex_lock(&taskCtx->suspend_mutex);
        if (!taskCtx->suspend_flag) {
            should_run = 1;
        } else {
            // Attendre le signal de reprise
            pthread_cond_wait(&taskCtx->suspend_cond, &taskCtx->suspend_mutex);
            if (!taskCtx->suspend_flag) {
                should_run = 1;
            }
        }
        pthread_mutex_unlock(&taskCtx->suspend_mutex);
        
        // Si la tâche peut s'exécuter
        if (should_run) {
            // Exécuter la fonction utilisateur et sortir
            result = taskCtx->task(taskCtx->arg);
            break;
        }
    }
    
    return result;
}

////////////////////////////////////////////////////////////
/// osTaskCreate
////////////////////////////////////////////////////////////
int osTaskCreate(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL || p_pttOSTask->task == NULL ||
        p_pttOSTask->stack_size == 0) {
        return OS_TASK_ERROR;
    }

    // Initialiser les éléments de suspension
    p_pttOSTask->suspend_flag = 0;
    if (pthread_mutex_init(&p_pttOSTask->suspend_mutex, NULL) != 0) {
        return OS_TASK_ERROR;
    }
    if (pthread_cond_init(&p_pttOSTask->suspend_cond, NULL) != 0) {
        pthread_mutex_destroy(&p_pttOSTask->suspend_mutex);
        return OS_TASK_ERROR;
    }

    pthread_attr_t attr;

    if (pthread_attr_init(&attr) != 0) {
        pthread_mutex_destroy(&p_pttOSTask->suspend_mutex);
        pthread_cond_destroy(&p_pttOSTask->suspend_cond);
        return OS_TASK_ERROR;
    }

    // Configuration des attributs du thread
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    pthread_attr_setstacksize(&attr, p_pttOSTask->stack_size);

	// Configuration de la priorité
#ifdef OS_USE_RT_SCHEDULING
    struct sched_param schedParam;
    // Configuration de la politique d'ordonnancement
    pthread_attr_setschedpolicy(&attr, SCHED_FIFO);
    schedParam.sched_priority = p_pttOSTask->priority;
    pthread_attr_setschedparam(&attr, &schedParam);
#endif

    // Création du thread en utilisant notre wrapper
    int ret = pthread_create(&p_pttOSTask->handle, &attr, 
                           osTaskWrapper, p_pttOSTask);
    pthread_attr_destroy(&attr);

    if (ret != 0) {
        pthread_mutex_destroy(&p_pttOSTask->suspend_mutex);
        pthread_cond_destroy(&p_pttOSTask->suspend_cond);
        return OS_TASK_ERROR;
    }

    p_pttOSTask->status = OS_TASK_STATUS_READY;
    p_pttOSTask->id = (int)(uintptr_t)p_pttOSTask->handle;

    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskEnd
////////////////////////////////////////////////////////////
int osTaskEnd(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL) {
        return OS_TASK_ERROR;
    }

    if (p_pttOSTask->status == OS_TASK_STATUS_TERMINATED) {
        return OS_TASK_ERROR;
    }

    int ret = pthread_cancel(p_pttOSTask->handle);
    if (ret != 0) {
        return OS_TASK_ERROR;
    }

    // Jointure pour libérer les ressources
    if (pthread_join(p_pttOSTask->handle, NULL) != 0) {
        return OS_TASK_ERROR;
    }

    // Nettoyage des ressources de synchronisation
    pthread_mutex_destroy(&p_pttOSTask->suspend_mutex);
    pthread_cond_destroy(&p_pttOSTask->suspend_cond);

    p_pttOSTask->status = OS_TASK_STATUS_TERMINATED;
    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskSuspend
////////////////////////////////////////////////////////////
int osTaskSuspend(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL) {
        return OS_TASK_ERROR;
    }

    // Vérifier que la tâche est dans un état valide pour la suspension
    if (p_pttOSTask->status != OS_TASK_STATUS_READY && 
        p_pttOSTask->status != OS_TASK_STATUS_RUNNING) {
        return OS_TASK_ERROR;
    }

    // Activer le drapeau de suspension
    pthread_mutex_lock(&p_pttOSTask->suspend_mutex);
    p_pttOSTask->suspend_flag = 1;
    pthread_mutex_unlock(&p_pttOSTask->suspend_mutex);

    p_pttOSTask->status = OS_TASK_STATUS_SUSPENDED;
    return OS_TASK_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osTaskResume
////////////////////////////////////////////////////////////
int osTaskResume(t_TaskCtx* p_pttOSTask)
{
    if (p_pttOSTask == NULL) {
        return OS_TASK_ERROR;
    }

    // Vérifier que la tâche est suspendue
    if (p_pttOSTask->status != OS_TASK_STATUS_SUSPENDED) {
        return OS_TASK_ERROR;
    }

    // Désactiver le drapeau de suspension et signaler la reprise
    pthread_mutex_lock(&p_pttOSTask->suspend_mutex);
    p_pttOSTask->suspend_flag = 0;
    pthread_cond_signal(&p_pttOSTask->suspend_cond);
    pthread_mutex_unlock(&p_pttOSTask->suspend_mutex);

    p_pttOSTask->status = OS_TASK_STATUS_READY;
    return OS_TASK_SUCCESS;
}

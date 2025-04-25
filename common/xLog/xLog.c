////////////////////////////////////////////////////////////
//  log source file
//  implements log functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
////////////////////////////////////////////////////////////

#include "xLog.h"
#include "xAssert.h"
#include "xOsHorodateur.h"
#include "xOsMutex.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#ifdef __cplusplus
#include <atomic>   //only for test lib and C++ 20
#else
#include <stdatomic.h> 
#endif

//
static t_logCtx s_tLogConfig = { 0 };
static t_MutexCtx s_tLogMutex;
#ifdef __cplusplus
static std::atomic_bool s_bInitialized = ATOMIC_VAR_INIT(false);
static std::atomic_bool s_bMutexInitialized = ATOMIC_VAR_INIT(false);
#else
static atomic_bool s_bInitialized = ATOMIC_VAR_INIT(false);
static atomic_bool s_bMutexInitialized = ATOMIC_VAR_INIT(false);
#endif

////////////////////////////////////////////////////////////
/// xLogInit
////////////////////////////////////////////////////////////
int xLogInit(t_logCtx* p_ptConfig)
{
    X_ASSERT(p_ptConfig != NULL);

    // Vérification rapide si déjà initialisé
    if (atomic_load(&s_bInitialized)) 
    {
        return XOS_LOG_OK;
    }

    // Initialisation du mutex si nécessaire
    bool expected = false;
    if (atomic_compare_exchange_strong(&s_bMutexInitialized, &expected, true)) 
    {
        int l_iRet = mutexCreate(&s_tLogMutex);
        if (l_iRet != MUTEX_OK) 
        {
            atomic_store(&s_bMutexInitialized, false);
            return XOS_LOG_MUTEX_ERROR;
        }
    }

    // Verrouiller le mutex pour l'initialisation
    int l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != MUTEX_OK) 
    {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Double vérification après verrouillage
    if (atomic_load(&s_bInitialized)) 
    {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_OK;
    }

    // Copier la configuration
    memcpy(&s_tLogConfig, p_ptConfig, sizeof(t_logCtx));

    // Créer ou vérifier le fichier de log
    if (s_tLogConfig.t_bLogToFile) 
    {
        FILE* l_ptFile = fopen(s_tLogConfig.t_cLogPath, "w");
        if (l_ptFile == NULL) 
        {
            mutexUnlock(&s_tLogMutex);
            return XOS_LOG_ERROR;
        }
        fclose(l_ptFile);
    }

    // Marquer comme initialisé
    atomic_store(&s_bInitialized, true);
    mutexUnlock(&s_tLogMutex);

    return XOS_LOG_OK;
}

////////////////////////////////////////////////////////////
/// xLogWrite
////////////////////////////////////////////////////////////
int xLogWrite(const char* p_ptkcFile, uint32_t p_ulLine, const char* p_ptkcFormat, ...)
{
    X_ASSERT(p_ptkcFile != NULL);
    X_ASSERT(p_ptkcFormat != NULL);

    // Vérification rapide de l'initialisation
    if (!atomic_load(&s_bInitialized)) {
        return XOS_LOG_NOT_INIT;
    }

    // Vérifier si le mutex est initialisé
    if (!atomic_load(&s_bMutexInitialized)) {
        return XOS_LOG_NOT_INIT;
    }

    // Verrouiller le mutex pour l'écriture
    int l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != MUTEX_OK) {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Double vérification après verrouillage
    if (!atomic_load(&s_bInitialized)) {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_NOT_INIT;
    }

    // Formater le message
    va_list args;
    va_start(args, p_ptkcFormat);
    char l_cBuffer[XOS_LOG_MSG_SIZE];
    vsnprintf(l_cBuffer, sizeof(l_cBuffer), p_ptkcFormat, args);
    va_end(args);

    // Écrire le log sur la console si activé
    if (s_tLogConfig.t_bLogToConsole) 
    {
        printf("%s | %s:%d | %s\n", xHorodateurGetString(), p_ptkcFile, p_ulLine, l_cBuffer);
    }

    // Écrire le log dans le fichier si activé
    if (s_tLogConfig.t_bLogToFile) 
    {
        FILE* l_ptFile = fopen(s_tLogConfig.t_cLogPath, "a");
        if (l_ptFile == NULL) 
        {
            mutexUnlock(&s_tLogMutex);
            return XOS_LOG_ERROR;
        }
        fprintf(l_ptFile, "%s | %s:%d | %s\n", xHorodateurGetString(), p_ptkcFile, p_ulLine, l_cBuffer);
        fclose(l_ptFile);
    }

    mutexUnlock(&s_tLogMutex);
    return XOS_LOG_OK;
}

////////////////////////////////////////////////////////////
/// xLogClose
////////////////////////////////////////////////////////////
int xLogClose(void)
{
    // Vérifier si le système est initialisé
    if (!atomic_load(&s_bInitialized)) {
        return XOS_LOG_NOT_INIT;
    }

    // Vérifier si le mutex est initialisé
    if (!atomic_load(&s_bMutexInitialized)) {
        return XOS_LOG_NOT_INIT;
    }

    // Verrouiller le mutex pour la fermeture
    int l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != MUTEX_OK) {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Marquer comme non initialisé
    atomic_store(&s_bInitialized, false);

    // Déverrouiller le mutex avant de le détruire
    mutexUnlock(&s_tLogMutex);

    // Détruire le mutex
    l_iRet = mutexDestroy(&s_tLogMutex);
    if (l_iRet != MUTEX_OK) {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Marquer le mutex comme non initialisé
    atomic_store(&s_bMutexInitialized, false);

    return XOS_LOG_OK;
}

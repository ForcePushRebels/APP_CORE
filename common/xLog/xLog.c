////////////////////////////////////////////////////////////
//  log source file
//  implements log functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xLog.h"
#include "xAssert.h"
#include "xOsHorodateur.h"
#include "xOsMutex.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdatomic.h>

// Logger state
typedef enum
{
    XOS_LOG_STATE_UNINITIALIZED = 0x0,
    XOS_LOG_STATE_INITIALIZED = 0x1
} t_logState;

// Logger context
static t_logCtx s_tLogConfig = {0};
static xOsMutexCtx s_tLogMutex;
static FILE *s_ptLogFile = NULL;

static atomic_int s_eLogState = ATOMIC_VAR_INIT(XOS_LOG_STATE_UNINITIALIZED);

////////////////////////////////////////////////////////////
/// xLogInit
////////////////////////////////////////////////////////////
int xLogInit(t_logCtx *p_ptConfig)
{
    if (p_ptConfig == NULL)
    {
        return XOS_LOG_INVALID;
    }

    // Early return if already initialized
    if (atomic_load(&s_eLogState) == XOS_LOG_STATE_INITIALIZED)
    {
        return XOS_LOG_OK;
    }

    // Create mutex
    int l_iRet = mutexCreate(&s_tLogMutex);
    if (l_iRet != (int)MUTEX_OK)
    {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Lock mutex for initialization
    l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != (int)MUTEX_OK)
    {
        mutexDestroy(&s_tLogMutex);
        return XOS_LOG_MUTEX_ERROR;
    }

    // Double-check after acquiring lock
    if (atomic_load(&s_eLogState) == XOS_LOG_STATE_INITIALIZED)
    {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_OK;
    }

    // Copy configuration (with size limits)
    memcpy(&s_tLogConfig, p_ptConfig, sizeof(t_logCtx));

    // Open log file if needed
    if (s_tLogConfig.t_bLogToFile)
    {
        if (s_tLogConfig.t_cLogPath[0] == '\0')
        {
            mutexUnlock(&s_tLogMutex);
            mutexDestroy(&s_tLogMutex);
            return XOS_LOG_INVALID;
        }

        s_ptLogFile = fopen(s_tLogConfig.t_cLogPath, "w");
        if (s_ptLogFile == NULL)
        {
            mutexUnlock(&s_tLogMutex);
            mutexDestroy(&s_tLogMutex);
            return XOS_LOG_ERROR;
        }
    }

    // Mark as initialized
    atomic_store(&s_eLogState, XOS_LOG_STATE_INITIALIZED);
    mutexUnlock(&s_tLogMutex);

    return XOS_LOG_OK;
}

////////////////////////////////////////////////////////////
/// xLogWrite
////////////////////////////////////////////////////////////
int xLogWrite(const char *p_ptkcFile, uint32_t p_ulLine, const char *p_ptkcFormat, ...)
{
    // Vérification des pointeurs
    if (p_ptkcFormat == NULL)
    {
        return XOS_LOG_INVALID;
    }

    // Early return if not initialized
    if (atomic_load(&s_eLogState) != XOS_LOG_STATE_INITIALIZED)
    {
        return XOS_LOG_NOT_INIT;
    }

    // Initialiser les buffers avec des valeurs sûres
    char l_cTimestamp[32] = "UnknownTime";
    char l_cUserMsg[XOS_LOG_MSG_SIZE] = {0};
    char l_cFullMsg[XOS_LOG_MSG_SIZE + 128] = {0};
    const char *l_pcFileName = "UnknownFile";
    
    // Récupération sécurisée de l'horodatage
    const char *l_pcTimestamp = xHorodateurGetString();
    if (l_pcTimestamp != NULL)
    {
        strncpy(l_cTimestamp, l_pcTimestamp, sizeof(l_cTimestamp) - 1);
        l_cTimestamp[sizeof(l_cTimestamp) - 1] = '\0';
    }

    // Extraction sécurisée du nom de fichier
    if (p_ptkcFile != NULL)
    {
        l_pcFileName = p_ptkcFile;
    const char *l_pcLastSlash = strrchr(p_ptkcFile, '/');
    if (l_pcLastSlash != NULL)
    {
        l_pcFileName = l_pcLastSlash + 1;
    }
    else
    {
        l_pcLastSlash = strrchr(p_ptkcFile, '\\');
        if (l_pcLastSlash != NULL)
        {
            l_pcFileName = l_pcLastSlash + 1;
            }
        }
    }

    // Format user message with va_args (outside mutex)
    va_list args;
    va_start(args, p_ptkcFormat);
    vsnprintf(l_cUserMsg, sizeof(l_cUserMsg) - 1, p_ptkcFormat, args);
    l_cUserMsg[sizeof(l_cUserMsg) - 1] = '\0'; // Garantie de terminaison
    va_end(args);

    // Format complete log message
    snprintf(l_cFullMsg, sizeof(l_cFullMsg) - 1, "%s | %s:%u | %s\n",
             l_cTimestamp, l_pcFileName, p_ulLine, l_cUserMsg);
    l_cFullMsg[sizeof(l_cFullMsg) - 1] = '\0'; // Garantie de terminaison

    // Now lock mutex only for the actual I/O operations
    int l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != (int)MUTEX_OK)
    {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Verify we're still initialized after lock
    if (atomic_load(&s_eLogState) != XOS_LOG_STATE_INITIALIZED)
    {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_NOT_INIT;
    }

    // Write to console if enabled
    if (s_tLogConfig.t_bLogToConsole)
    {
        fputs(l_cFullMsg, stdout);
        fflush(stdout);
    }

    // Write to file if enabled
    if (s_tLogConfig.t_bLogToFile && s_ptLogFile != NULL)
    {
        fputs(l_cFullMsg, s_ptLogFile);
        fflush(s_ptLogFile);
    }

    mutexUnlock(&s_tLogMutex);
    return XOS_LOG_OK;
}

////////////////////////////////////////////////////////////
/// xLogClose
////////////////////////////////////////////////////////////
int xLogClose(void)
{
    // Early return if not initialized
    if (atomic_load(&s_eLogState) != XOS_LOG_STATE_INITIALIZED)
    {
        return XOS_LOG_NOT_INIT;
    }

    // Lock mutex for shutdown
    int l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != (int)MUTEX_OK)
    {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Double-check after acquiring lock
    if (atomic_load(&s_eLogState) != XOS_LOG_STATE_INITIALIZED)
    {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_NOT_INIT;
    }

    // Close log file if open
    if (s_ptLogFile != NULL)
    {
        fclose(s_ptLogFile);
        s_ptLogFile = NULL;
    }

    // Mark as uninitialized first (prevents new logging operations)
    atomic_store(&s_eLogState, XOS_LOG_STATE_UNINITIALIZED);

    // Release mutex and destroy it
    mutexUnlock(&s_tLogMutex);
    mutexDestroy(&s_tLogMutex);

    return XOS_LOG_OK;
}

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
#include "xOsMemory.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdatomic.h>
#include <unistd.h>
#include <libgen.h>
#include <limits.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>

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

// Atomic log state with explicit initialization for better performance
static atomic_int s_eLogState = ATOMIC_VAR_INIT(XOS_LOG_STATE_UNINITIALIZED);

// Forward declarations of secure helper functions
static bool isValidLogFileName(const char *p_pcFileName);
static int secureOpenLogFile(const char *p_pcPath, FILE **p_ppFile);
static void sanitizeLogContent(char *p_pcContent, size_t p_iMaxSize);
static int xLogGetExecutablePath(char *p_pcExecutablePath, size_t p_iSize);

////////////////////////////////////////////////////////////
/// xLogInit
////////////////////////////////////////////////////////////
int xLogInit(t_logCtx *p_ptConfig)
{
    // Single dereference rule - validate once and store
    if (p_ptConfig == NULL)
    {
        return XOS_LOG_INVALID;
    }
    
    // Store dereferenced values once for security
    const bool l_bLogToFile = p_ptConfig->t_bLogToFile;
    const bool l_bLogToConsole = p_ptConfig->t_bLogToConsole;
    
    // Fixed bounds buffers
    char l_cConfigPath[XOS_LOG_PATH_SIZE] = {0};
    snprintf(l_cConfigPath, sizeof(l_cConfigPath), "%s", p_ptConfig->t_cLogPath);

    // Early return if already initialized - use acquire ordering for proper synchronization
    if (atomic_load_explicit(&s_eLogState, memory_order_acquire) == XOS_LOG_STATE_INITIALIZED)
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

    // Double-check after acquiring lock - use relaxed ordering within mutex protection
    if (atomic_load_explicit(&s_eLogState, memory_order_relaxed) == XOS_LOG_STATE_INITIALIZED)
    {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_OK;
    }

    // Clear and copy configuration securely with fixed bounds
    XOS_MEMORY_SANITIZE(&s_tLogConfig, sizeof(s_tLogConfig));
    s_tLogConfig.t_bLogToFile = l_bLogToFile;
    s_tLogConfig.t_bLogToConsole = l_bLogToConsole;
    strncpy(s_tLogConfig.t_cLogPath, l_cConfigPath, sizeof(s_tLogConfig.t_cLogPath) - 1);
    s_tLogConfig.t_cLogPath[sizeof(s_tLogConfig.t_cLogPath) - 1] = '\0';

    // Always construct full path when logging to file
    if (s_tLogConfig.t_bLogToFile)
    {
        // Fixed bounds buffers
        char l_cExecutablePath[XOS_LOG_TEMP_BUFFER_SIZE] = {0};
        char l_cFullLogPath[XOS_LOG_PATH_SIZE] = {0};
        char l_cOriginalFileName[XOS_LOG_MAX_FILENAME_SIZE] = {0};
        
        // Copy original filename with fixed bounds
        snprintf(l_cOriginalFileName, sizeof(l_cOriginalFileName), "%s", l_cConfigPath);
        
        // Validate filename security
        if (!isValidLogFileName(l_cOriginalFileName))
        {
            XOS_MEMORY_SANITIZE(&s_tLogConfig, sizeof(s_tLogConfig));
            XOS_MEMORY_SANITIZE(l_cOriginalFileName, sizeof(l_cOriginalFileName));
            mutexUnlock(&s_tLogMutex);
            mutexDestroy(&s_tLogMutex);
            return XOS_LOG_SECURITY_ERROR;
        }
        
        // Get executable directory
        int l_iPathRet = xLogGetExecutablePath(l_cExecutablePath, sizeof(l_cExecutablePath));
        if (l_iPathRet < 0)
        {
            // Fallback to current directory
            strncpy(l_cExecutablePath, ".", sizeof(l_cExecutablePath) - 1);
            l_cExecutablePath[sizeof(l_cExecutablePath) - 1] = '\0';
        }
        
        // Construct full path securely with fixed bounds
        int l_iSnprintfRet = snprintf(l_cFullLogPath, sizeof(l_cFullLogPath), "%s/%s", 
                                      l_cExecutablePath, l_cOriginalFileName);
        
        // Check for formatting errors
        if (l_iSnprintfRet < 0)
        {
            XOS_MEMORY_SANITIZE(&s_tLogConfig, sizeof(s_tLogConfig));
            XOS_MEMORY_SANITIZE(l_cOriginalFileName, sizeof(l_cOriginalFileName));
            XOS_MEMORY_SANITIZE(l_cExecutablePath, sizeof(l_cExecutablePath));
            XOS_MEMORY_SANITIZE(l_cFullLogPath, sizeof(l_cFullLogPath));
            mutexUnlock(&s_tLogMutex);
            mutexDestroy(&s_tLogMutex);
            return XOS_LOG_ERROR;
        }
        
        // Copy the constructed path back to config
        strncpy(s_tLogConfig.t_cLogPath, l_cFullLogPath, sizeof(s_tLogConfig.t_cLogPath) - 1);
        s_tLogConfig.t_cLogPath[sizeof(s_tLogConfig.t_cLogPath) - 1] = '\0';
        
        // Clear sensitive temporary data
        XOS_MEMORY_SANITIZE(l_cOriginalFileName, sizeof(l_cOriginalFileName));
        XOS_MEMORY_SANITIZE(l_cExecutablePath, sizeof(l_cExecutablePath));
        XOS_MEMORY_SANITIZE(l_cFullLogPath, sizeof(l_cFullLogPath));
    }

    // Open log file securely if needed
    if (s_tLogConfig.t_bLogToFile)
    {
        if (s_tLogConfig.t_cLogPath[0] == '\0')
        {
            XOS_MEMORY_SANITIZE(&s_tLogConfig, sizeof(s_tLogConfig));
            mutexUnlock(&s_tLogMutex);
            mutexDestroy(&s_tLogMutex);
            return XOS_LOG_INVALID;
        }

        l_iRet = secureOpenLogFile(s_tLogConfig.t_cLogPath, &s_ptLogFile);
        if (l_iRet != XOS_LOG_OK)
        {
            XOS_MEMORY_SANITIZE(&s_tLogConfig, sizeof(s_tLogConfig));
            mutexUnlock(&s_tLogMutex);
            mutexDestroy(&s_tLogMutex);
            return l_iRet;
        }
    }

    // Mark as initialized
    atomic_store_explicit(&s_eLogState, XOS_LOG_STATE_INITIALIZED, memory_order_release);
    mutexUnlock(&s_tLogMutex);

    return XOS_LOG_OK;
}

////////////////////////////////////////////////////////////
/// xLogWrite
////////////////////////////////////////////////////////////
int xLogWrite(const char *p_ptkcFile, uint32_t p_ulLine, const char *p_ptkcFormat, ...)
{
    // Single dereference rule - validate once and store
    if (p_ptkcFormat == NULL)
    {
        return XOS_LOG_INVALID;
    }

    // Store dereferenced values once for security
    const uint32_t l_ulLineNumber = p_ulLine;

    // Fixed bounds buffer for format string copy
    char l_cFormatCopy[XOS_LOG_MSG_SIZE] = {0};
    strncpy(l_cFormatCopy, p_ptkcFormat, sizeof(l_cFormatCopy) - 1);
    l_cFormatCopy[sizeof(l_cFormatCopy) - 1] = '\0';

    // Cache log state check with acquire ordering - early return optimization
    int l_iLogState = atomic_load_explicit(&s_eLogState, memory_order_acquire);
    if (l_iLogState != XOS_LOG_STATE_INITIALIZED)
    {
        return XOS_LOG_NOT_INIT;
    }

    char l_cTimestamp[XOS_LOG_TIMESTAMP_SIZE] = {0};
    char l_cUserMsg[XOS_LOG_MSG_SIZE] = {0};
    char l_cFullMsg[XOS_LOG_FULL_MSG_SIZE] = {0};
    char l_cSafeFileName[XOS_LOG_BASENAME_SIZE] = {0};

    const char *l_pcTimestampPtr = xHorodateurGetString();
    if (l_pcTimestampPtr != NULL)
    {
        strncpy(l_cTimestamp, l_pcTimestampPtr, sizeof(l_cTimestamp) - 1);
        l_cTimestamp[sizeof(l_cTimestamp) - 1] = '\0';
    }
    else
    {
        strncpy(l_cTimestamp, "UnknownTime", sizeof(l_cTimestamp) - 1);
        l_cTimestamp[sizeof(l_cTimestamp) - 1] = '\0';
    }

    if (p_ptkcFile != NULL)
    {
        char l_cFileCopy[XOS_LOG_PATH_SIZE] = {0};
        strncpy(l_cFileCopy, p_ptkcFile, sizeof(l_cFileCopy) - 1);
        l_cFileCopy[sizeof(l_cFileCopy) - 1] = '\0';
        
        const char *l_pcBaseName = l_cFileCopy;
        const char *l_pcForwardSlashPtr = strrchr(l_cFileCopy, '/');
        if (l_pcForwardSlashPtr != NULL)
        {
            l_pcBaseName = l_pcForwardSlashPtr + 1;
        }
        else
        {
            const char *l_pcBackSlashPtr = strrchr(l_cFileCopy, '\\');
            if (l_pcBackSlashPtr != NULL)
            {
                l_pcBaseName = l_pcBackSlashPtr + 1;
            }
        }
        
        // Copy basename securely with fixed bounds
        strncpy(l_cSafeFileName, l_pcBaseName, sizeof(l_cSafeFileName) - 1);
        l_cSafeFileName[sizeof(l_cSafeFileName) - 1] = '\0';
        
        // Sanitize filename to prevent log injection
        sanitizeLogContent(l_cSafeFileName, sizeof(l_cSafeFileName));
        
        // Clear temporary file copy
        XOS_MEMORY_SANITIZE(l_cFileCopy, sizeof(l_cFileCopy));
    }
    else
    {
        strncpy(l_cSafeFileName, "UnknownFile", sizeof(l_cSafeFileName) - 1);
        l_cSafeFileName[sizeof(l_cSafeFileName) - 1] = '\0';
    }

    // Format user message securely with fixed bounds
    va_list args;
    va_start(args, p_ptkcFormat);
    int l_iVsnprintfRet = vsnprintf(l_cUserMsg, sizeof(l_cUserMsg) - 1, l_cFormatCopy, args);
    l_cUserMsg[sizeof(l_cUserMsg) - 1] = '\0';
    va_end(args);
    
    // Check for formatting errors
    if (l_iVsnprintfRet < 0)
    {
        strncpy(l_cUserMsg, "[FORMAT_ERROR]", sizeof(l_cUserMsg) - 1);
        l_cUserMsg[sizeof(l_cUserMsg) - 1] = '\0';
    }
    
    // Sanitize user message to prevent log injection
    sanitizeLogContent(l_cUserMsg, sizeof(l_cUserMsg));

    // Format complete log message with fixed bounds
    int l_iSnprintfRet = snprintf(l_cFullMsg, sizeof(l_cFullMsg) - 1, "%s | %s:%u | %s\n",
                                 l_cTimestamp, l_cSafeFileName, l_ulLineNumber, l_cUserMsg);
    l_cFullMsg[sizeof(l_cFullMsg) - 1] = '\0';
    
    // Check for formatting errors
    if (l_iSnprintfRet < 0)
    {
        // Clear sensitive data and return error
        XOS_MEMORY_SANITIZE(l_cUserMsg, sizeof(l_cUserMsg));
        XOS_MEMORY_SANITIZE(l_cFullMsg, sizeof(l_cFullMsg));
        XOS_MEMORY_SANITIZE(l_cFormatCopy, sizeof(l_cFormatCopy));
        return XOS_LOG_ERROR;
    }

    // Lock mutex for I/O operations
    int l_iRet = mutexLock(&s_tLogMutex);
    if (l_iRet != (int)MUTEX_OK)
    {
        XOS_MEMORY_SANITIZE(l_cUserMsg, sizeof(l_cUserMsg));
        XOS_MEMORY_SANITIZE(l_cFullMsg, sizeof(l_cFullMsg));
        XOS_MEMORY_SANITIZE(l_cFormatCopy, sizeof(l_cFormatCopy));
        return XOS_LOG_MUTEX_ERROR;
    }

    // Verify we're still initialized after lock - use cached state with relaxed check
    // Since we're within mutex protection and state was cached above, use relaxed ordering
    int l_iCurrentState = atomic_load_explicit(&s_eLogState, memory_order_relaxed);
    if (l_iCurrentState != XOS_LOG_STATE_INITIALIZED)
    {
        XOS_MEMORY_SANITIZE(l_cUserMsg, sizeof(l_cUserMsg));
        XOS_MEMORY_SANITIZE(l_cFullMsg, sizeof(l_cFullMsg));
        XOS_MEMORY_SANITIZE(l_cFormatCopy, sizeof(l_cFormatCopy));
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_NOT_INIT;
    }

    // Write to console if enabled (single dereference of config)
    const bool l_bLogToConsole = s_tLogConfig.t_bLogToConsole;
    if (l_bLogToConsole)
    {
        if (fputs(l_cFullMsg, stdout) == EOF || fflush(stdout) == EOF)
        {
            // Console write failed, but continue with file logging
        }
    }

    // Write to file if enabled (single dereference of config)
    const bool l_bLogToFile = s_tLogConfig.t_bLogToFile;
    if (l_bLogToFile && s_ptLogFile != NULL)
    {
        if (fputs(l_cFullMsg, s_ptLogFile) == EOF || fflush(s_ptLogFile) == EOF)
        {
            // File write failed
            l_iRet = XOS_LOG_ERROR;
        }
    }

    // Clear sensitive data before unlocking
    XOS_MEMORY_SANITIZE(l_cUserMsg, sizeof(l_cUserMsg));
    XOS_MEMORY_SANITIZE(l_cFullMsg, sizeof(l_cFullMsg));
    XOS_MEMORY_SANITIZE(l_cFormatCopy, sizeof(l_cFormatCopy));
    
    mutexUnlock(&s_tLogMutex);
    return (l_iRet == (int)MUTEX_OK) ? XOS_LOG_OK : l_iRet;
}

////////////////////////////////////////////////////////////
/// xLogClose
////////////////////////////////////////////////////////////
int xLogClose(void)
{
    // Early return if not initialized
    if (atomic_load_explicit(&s_eLogState, memory_order_acquire) != XOS_LOG_STATE_INITIALIZED)
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
    if (atomic_load_explicit(&s_eLogState, memory_order_acquire) != XOS_LOG_STATE_INITIALIZED)
    {
        mutexUnlock(&s_tLogMutex);
        return XOS_LOG_NOT_INIT;
    }

    // Close log file if open
    if (s_ptLogFile != NULL)
    {
        // Flush any remaining data before closing
        if (fflush(s_ptLogFile) == EOF)
        {
            // Flush failed, but continue with cleanup
        }
        
        if (fclose(s_ptLogFile) == EOF)
        {
            // Close failed, but continue with cleanup
        }
        
        s_ptLogFile = NULL;
    }

    // Mark as uninitialized first (prevents new logging operations)
    atomic_store_explicit(&s_eLogState, XOS_LOG_STATE_UNINITIALIZED, memory_order_release);

    // Securely clear configuration data
    XOS_MEMORY_SANITIZE(&s_tLogConfig, sizeof(s_tLogConfig));

    // Release mutex and destroy it
    mutexUnlock(&s_tLogMutex);
    mutexDestroy(&s_tLogMutex);

    return XOS_LOG_OK;
}

////////////////////////////////////////////////////////////
/// xLogGetExecutablePath
////////////////////////////////////////////////////////////
static int xLogGetExecutablePath(char *p_pcExecutablePath, size_t p_iSize)
{
    // Input validation with fixed bounds
    if (p_pcExecutablePath == NULL || p_iSize == 0 || p_iSize > XOS_LOG_TEMP_BUFFER_SIZE)
    {
        return -1;
    }

    // Initialize output buffer
    XOS_MEMORY_SANITIZE(p_pcExecutablePath, p_iSize);

    // Fixed bounds buffers
    char l_cExePath[XOS_LOG_TEMP_BUFFER_SIZE] = {0};
    char l_cExePathCopy[XOS_LOG_TEMP_BUFFER_SIZE] = {0};
    
    ssize_t l_lPathLen = readlink("/proc/self/exe", l_cExePath, sizeof(l_cExePath) - 1);
    if (l_lPathLen == -1 || l_lPathLen == 0)
    {
        XOS_MEMORY_SANITIZE(l_cExePath, sizeof(l_cExePath));
        return -1;
    }
    
    // Ensure null termination
    l_cExePath[l_lPathLen] = '\0';

    // Create a copy for dirname with secure copy
    strncpy(l_cExePathCopy, l_cExePath, sizeof(l_cExePathCopy) - 1);
    l_cExePathCopy[sizeof(l_cExePathCopy) - 1] = '\0';

    // Get directory path of executable - single dereference
    char *l_pcDirPathPtr = dirname(l_cExePathCopy);
    if (l_pcDirPathPtr == NULL)
    {
        XOS_MEMORY_SANITIZE(l_cExePath, sizeof(l_cExePath));
        XOS_MEMORY_SANITIZE(l_cExePathCopy, sizeof(l_cExePathCopy));
        return -1;
    }

    // Check output buffer size - store length once
    size_t l_iDirLen = strlen(l_pcDirPathPtr);
    if (l_iDirLen >= p_iSize)
    {
        XOS_MEMORY_SANITIZE(l_cExePath, sizeof(l_cExePath));
        XOS_MEMORY_SANITIZE(l_cExePathCopy, sizeof(l_cExePathCopy));
        return -1; // Buffer too small
    }

    // Copy directory path to output buffer securely
    strncpy(p_pcExecutablePath, l_pcDirPathPtr, p_iSize - 1);
    p_pcExecutablePath[p_iSize - 1] = '\0';

    // Clear sensitive temporary data
    XOS_MEMORY_SANITIZE(l_cExePath, sizeof(l_cExePath));
    XOS_MEMORY_SANITIZE(l_cExePathCopy, sizeof(l_cExePathCopy));

    return (int)l_iDirLen;
}

////////////////////////////////////////////////////////////
/// Secure helper functions
////////////////////////////////////////////////////////////

// Validate log filename - prevent path traversal attacks
static bool isValidLogFileName(const char *p_pcFileName)
{
    // Single dereference rule - validate and store once
    if (p_pcFileName == NULL)
    {
        return false;
    }
    
    // Store filename pointer once for security
    const char *l_pcFilenamePtr = p_pcFileName;
    size_t l_iLen = strlen(l_pcFilenamePtr);
    if (l_iLen == 0 || l_iLen >= XOS_LOG_MAX_FILENAME_SIZE)
    {
        return false;
    }
    
    // Check for path traversal attempts - single dereference per call
    if (strstr(l_pcFilenamePtr, "..") != NULL ||
        strstr(l_pcFilenamePtr, "/") != NULL ||
        strstr(l_pcFilenamePtr, "\\") != NULL)
    {
        return false;
    }
    
    // Check for valid characters only - single dereference with local copy
    for (size_t i = 0; i < l_iLen; i++)
    {
        char c = l_pcFilenamePtr[i];
        if (!isalnum(c) && c != '.' && c != '_' && c != '-')
        {
            return false;
        }
    }
    
    // Ensure it doesn't start with a dot - single dereference
    char l_cFirstChar = l_pcFilenamePtr[0];
    if (l_cFirstChar == '.')
    {
        return false;
    }
    
    return true;
}

// Securely open log file with proper permissions
static int secureOpenLogFile(const char *p_pcPath, FILE **p_ppFile)
{
    // Single dereference rule - validate pointers once
    if (p_pcPath == NULL || p_ppFile == NULL)
    {
        return XOS_LOG_INVALID;
    }
    
    // Store path pointer once for security
    const char *l_pcPathPtr = p_pcPath;
    
    // Open with restricted permissions (owner read/write only)
    int l_iFd = open(l_pcPathPtr, O_WRONLY | O_CREAT | O_TRUNC | O_NOFOLLOW, S_IRUSR | S_IWUSR);
    if (l_iFd == -1)
    {
        return XOS_LOG_ERROR;
    }
    
    // Convert file descriptor to FILE* - single dereference
    FILE *l_pFile = fdopen(l_iFd, "w");
    if (l_pFile == NULL)
    {
        close(l_iFd);
        return XOS_LOG_ERROR;
    }
    
    // Store result with single dereference
    *p_ppFile = l_pFile;
    
    return XOS_LOG_OK;
}

// Sanitize log content to prevent log injection
static void sanitizeLogContent(char *p_pcContent, size_t p_iMaxSize)
{
    // Single dereference rule - validate pointer once
    if (p_pcContent == NULL || p_iMaxSize == 0)
    {
        return;
    }
    
    // Store content pointer once for security
    char *l_pcContentPtr = p_pcContent;
    
    for (size_t i = 0; i < p_iMaxSize && l_pcContentPtr[i] != '\0'; i++)
    {
        char c = l_pcContentPtr[i];
        
        // Replace control characters (except tab and space) with underscore
        if ((c < 0x20 && c != '\t') || c == 0x7F)
        {
            l_pcContentPtr[i] = '_';
        }
        // Replace potential log injection characters
        else if (c == '\r' || c == '\n')
        {
            l_pcContentPtr[i] = '_';
        }
    }
}
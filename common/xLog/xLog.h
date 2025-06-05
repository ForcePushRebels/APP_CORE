////////////////////////////////////////////////////////////
//  log header file
//  defines log types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_LOG_H_
#define XOS_LOG_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "xError.h"

// Log buffer sizes - FIXED BOUNDS for security
#define XOS_LOG_PATH_SIZE 512
#define XOS_LOG_MSG_SIZE 1024
#define XOS_LOG_MAX_FILENAME_SIZE 128
#define XOS_LOG_TIMESTAMP_SIZE 32
#define XOS_LOG_BASENAME_SIZE 64
#define XOS_LOG_FULL_MSG_SIZE (XOS_LOG_MSG_SIZE + 128) // Fixed calculation
#define XOS_LOG_TEMP_BUFFER_SIZE 256                   // For temporary operations

// Log configuration structure
typedef struct
{
    bool t_bLogToFile;                  // Enable file logging
    bool t_bLogToConsole;               // Enable console logging
    char t_cLogPath[XOS_LOG_PATH_SIZE]; // Log filename (full path will be auto-constructed with executable directory)
} t_logCtx;

// Log severity levels
typedef enum {
    XOS_LOG_LEVEL_TRACE = 0,
    XOS_LOG_LEVEL_DEBUG = 1,
    XOS_LOG_LEVEL_INFO = 2,
    XOS_LOG_LEVEL_WARN = 3,
    XOS_LOG_LEVEL_ERROR = 4,
    XOS_LOG_LEVEL_FATAL = 5
} t_logLevel;

//////////////////////////////////
/// @brief Initialize logging system
/// @param p_ptConfig : log configuration
/// @return success or error code
//////////////////////////////////
int xLogInit(t_logCtx *p_ptConfig);

//////////////////////////////////
/// @brief Write log message
/// @param p_ptkcFile : source file
/// @param p_ulLine : line number
/// @param p_ptkcFormat : message format
/// @return success or error code
//////////////////////////////////
int xLogWrite(const char *p_ptkcFile, uint32_t p_ulLine, const char *p_ptkcFormat, ...);

//////////////////////////////////
/// @brief Close logging system
/// @return success or error code
//////////////////////////////////
int xLogClose(void);

// Enhanced log macros with severity levels and proper color handling
#define X_LOG_TRACE(msg, ...) xLogWrite(__FILE__, __LINE__, "\033[90m[TRACE] " msg "\033[0m", ##__VA_ARGS__)
#define X_LOG_DEBUG(msg, ...) xLogWrite(__FILE__, __LINE__, "\033[36m[DEBUG] " msg "\033[0m", ##__VA_ARGS__)
#define X_LOG_INFO(msg, ...) xLogWrite(__FILE__, __LINE__, "\033[32m[INFO] " msg "\033[0m", ##__VA_ARGS__)
#define X_LOG_WARN(msg, ...) xLogWrite(__FILE__, __LINE__, "\033[33m[WARN] " msg "\033[0m", ##__VA_ARGS__)
#define X_LOG_ERROR(msg, ...) xLogWrite(__FILE__, __LINE__, "\033[31m[ERROR] " msg "\033[0m", ##__VA_ARGS__)
#define X_LOG_FATAL(msg, ...) xLogWrite(__FILE__, __LINE__, "\033[35m[FATAL] " msg "\033[0m", ##__VA_ARGS__)

// Legacy macro (keep for compatibility)
#define X_LOG_ASSERT(msg, ...) X_LOG_ERROR("ASSERT | " msg, ##__VA_ARGS__)

#endif // XOS_LOG_H_

////////////////////////////////////////////////////////////
//  horodateur header file
//  defines horodateur functions for timestamp
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 28/05/2025 - Security improvements
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_HORODATEUR_H_
#define XOS_HORODATEUR_H_

#include <stdint.h>
#include "xError.h"

#define XOS_HORODATEUR_BUFFER_SIZE   72
#define XOS_HORODATEUR_MAX_YEAR      9999
#define XOS_HORODATEUR_MIN_YEAR      1900
#define XOS_HORODATEUR_MAX_FORMAT_ITERATIONS 10

//////////////////////////////////
/// @brief Get current formatted timestamp with microsecond precision
/// @return Pointer to thread-local timestamp string (YYYY-MM-DD HH:MM:SS.microseconds)
//////////////////////////////////
const char* xHorodateurGetString(void);

//////////////////////////////////
/// @brief Get current timestamp in seconds (Unix timestamp)
/// @return Current Unix timestamp in seconds
//////////////////////////////////
uint32_t xHorodateurGet(void);

//////////////////////////////////
/// @brief Get current high-resolution timestamp in microseconds since Unix epoch
/// @return Current timestamp in microseconds since 1970-01-01 00:00:00 UTC
//////////////////////////////////
uint64_t xHorodateurGetMicroseconds(void);

#endif // XOS_HORODATEUR_H_

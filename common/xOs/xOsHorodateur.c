////////////////////////////////////////////////////////////
//  horodateur source file
//  implements timestamp functions
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 2025 - Enhanced precision to microseconds
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xOsHorodateur.h"
#include "xAssert.h"
#include <time.h>
#include <string.h>
#include <stdio.h>

#define XOS_HORODATEUR_BUFFER_SIZE 72   

// Thread-local buffer: each thread has its own buffer
static __thread char s_cTimeBuffer[XOS_HORODATEUR_BUFFER_SIZE];

////////////////////////////////////////////////////////////
/// xHorodateurGetString - Enhanced with microsecond precision
////////////////////////////////////////////////////////////
const char* xHorodateurGetString(void)
{
    struct timespec ts;
    struct tm tm_result;

    // Get current high-resolution time
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) 
    {
        return NULL;
    }

    // Convert to local time in thread-safe manner
    if (localtime_r(&ts.tv_sec, &tm_result) == NULL) 
    {
        return NULL;
    }

    // Format complete timestamp in one call: YYYY-MM-DD HH:MM:SS.microseconds
    // Convert nanoseconds to microseconds (divide by 1000)
    int written = snprintf(s_cTimeBuffer, XOS_HORODATEUR_BUFFER_SIZE,
                          "%04d-%02d-%02d %02d:%02d:%02d.%06ld",
                          tm_result.tm_year + 1900,
                          tm_result.tm_mon + 1,
                          tm_result.tm_mday,
                          tm_result.tm_hour,
                          tm_result.tm_min,
                          tm_result.tm_sec,
                          ts.tv_nsec / 1000);  // nanoseconds to microseconds

    if (written < 0 || written >= XOS_HORODATEUR_BUFFER_SIZE) 
    {
        return NULL;
    }

    return s_cTimeBuffer;
}

////////////////////////////////////////////////////////////
/// xHorodateurGet
////////////////////////////////////////////////////////////
uint32_t xHorodateurGet(void)
{
    struct timespec ts;
    // Get current time
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) 
    {
        X_ASSERT(0); // Critical error
        return 0;
    }
    // Return seconds part (Unix timestamp)
    return (uint32_t)ts.tv_sec;
}

////////////////////////////////////////////////////////////
/// xHorodateurGetMicroseconds - High-resolution timestamp
////////////////////////////////////////////////////////////
uint64_t xHorodateurGetMicroseconds(void)
{
    struct timespec ts;
    // Get current high-resolution time
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) 
    {
        X_ASSERT(0); // Critical error
        return 0;
    }
    
    // Convert to microseconds since Unix epoch
    // seconds * 1,000,000 + nanoseconds / 1,000
    return ((uint64_t)ts.tv_sec * 1000000ULL) + ((uint64_t)ts.tv_nsec / 1000ULL);
}
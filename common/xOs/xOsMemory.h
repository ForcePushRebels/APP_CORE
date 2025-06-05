////////////////////////////////////////////////////////////
//  xOsSecureMemory header file
//  Secure memory sanitization and protection macros
//
// general discloser: copy or share the file is forbidden
// Written : 28/05/2025
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#ifndef XOS_SECURE_MEMORY_H_
#define XOS_SECURE_MEMORY_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "xError.h"

// Fixed bounds for security operations
#define XOS_SECURE_MAX_CLEAR_SIZE (SIZE_MAX / 2)
#define XOS_SECURE_MAX_ITERATIONS 1024
#define XOS_SECURE_TIMING_LOOPS 16


////////////////////////////////////////////////////////////
// Core Memory Sanitization Macros
////////////////////////////////////////////////////////////

// Secure memory clearing - prevents compiler optimization
#define XOS_MEMORY_SANITIZE(ptr, size)                                                                                      \
    do                                                                                                                      \
    {                                                                                                                       \
        if ((ptr) != NULL && (size) > 0 && (size) <= XOS_SECURE_MAX_CLEAR_SIZE)                                             \
        {                                                                                                                   \
            volatile unsigned char *volatile_ptr = (volatile unsigned char *)(ptr);                                         \
            for (size_t i = 0; i < (size); i++)                                                                             \
            {                                                                                                               \
                volatile_ptr[i] = 0;                                                                                        \
            }                                                                                                               \
        }                                                                                                                   \
    } while (0)

////////////////////////////////////////////////////////////
// Secure Buffer Management
////////////////////////////////////////////////////////////

// Secure buffer initialization with bounds checking
#define XOS_SECURE_BUFFER_INIT(buffer, size)                                                                                \
    do                                                                                                                      \
    {                                                                                                                       \
        if ((buffer) != NULL && (size) > 0 && (size) <= XOS_SECURE_MAX_CLEAR_SIZE)                                          \
        {                                                                                                                   \
            XOS_MEMORY_SANITIZE(buffer, size);                                                                              \
        }                                                                                                                   \
    } while (0)

// Secure buffer copy with size validation
#define XOS_SECURE_BUFFER_COPY(dest, dest_size, src, src_size)                                                              \
    do                                                                                                                      \
    {                                                                                                                       \
        if ((dest) != NULL && (src) != NULL && (dest_size) > 0 && (src_size) > 0                                            \
            && (dest_size) <= XOS_SECURE_MAX_CLEAR_SIZE && (src_size) <= XOS_SECURE_MAX_CLEAR_SIZE                          \
            && (src_size) <= (dest_size))                                                                                   \
        {                                                                                                                   \
            volatile unsigned char *vdest = (volatile unsigned char *)(dest);                                               \
            volatile const unsigned char *vsrc = (volatile const unsigned char *)(src);                                     \
            for (size_t i = 0; i < (src_size); i++)                                                                         \
            {                                                                                                               \
                vdest[i] = vsrc[i];                                                                                         \
            }                                                                                                               \
            if ((src_size) < (dest_size))                                                                                   \
            {                                                                                                               \
                XOS_MEMORY_SANITIZE(&vdest[src_size], (dest_size) - (src_size));                                            \
            }                                                                                                               \
        }                                                                                                                   \
    } while (0)

#endif // XOS_SECURE_MEMORY_H_

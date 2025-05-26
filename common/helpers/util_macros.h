// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file util_macros.h
 * @brief Header file for the util_macros module.
 *
 * @author
 * ForcePushRebels – PATO Project (collective contributor)  
 * Uriel Fodong <uriel.fodong@reseau.eseo.fr> (individual contributor)
 *
 * @copyright
 * © 2025 ESEO – All rights reserved.
 *
 * @par License
 * PATO ESEO License (see LICENSE.md)
 */

#ifndef __UTIL_MACROS_H__
#define __UTIL_MACROS_H__

// For unused variables (to silence compiler warnings)
#define UNUSED(val)      ((void)(val))
#define UNUSED_INT       (0)
#define UNUSED_BOOL      (false)
#define UNUSED_PTR       ((void *)0)
#define UNUSED_STR       ("unused")
#define FAKE_PTR		 ((void*)0x1)

#define SAFE_FREE(ptr)                \
	do {                              \
		free(ptr);                    \
		(ptr) = NULL;                 \
	} while (0)

// Return early if argument is NULL
#define RETURN_IF_NULL(ptr)           \
    do {                              \
        if ((ptr) == NULL)            \
            return RET_ERR_NULL;      \
    } while (0)

// Return early if argument is invalid (0 or false)
#define RETURN_IF_INVALID(x)          \
    do {                              \
        if (!(x))                     \
            return RET_ERR_INVALID_ARG; \
    } while (0)

// Return early with custom code
#define RETURN_IF_FAIL(expr, code)    \
    do {                              \
        if (!(expr))                  \
            return (code);            \
    } while (0)

// Check return value of a function and propagate if error
#define RETURN_IF_ERROR(expr)         \
    do {                              \
        int __ret = (expr);           \
        if (__ret != RET_OK)          \
            return __ret;             \
    } while (0)

#endif /* __UTIL_MACROS_H__ */
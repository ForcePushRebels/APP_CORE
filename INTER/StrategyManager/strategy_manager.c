// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file strategy_manager.c
 * @brief Source file for the Strategy Manager module.
 *
 * @author
 * ForcePushRebels – PATO Project (collective contributor)  
 * Uriel Fodong <uriel.fodong@reseau.eseo.fr> (individual contributor)
 *
 * @version 0.0.1
 *
 * @copyright
 * © 2025 ESEO – All rights reserved.
 *
 * @par License
 * PATO ESEO License (see LICENSE.md)
 */

#include "strategy_manager.h"

// Implementation declares compatibility range (which APIs it supports)
#define STRATEGY_MANAGER_API_COMPAT_MIN V(0, 0, 1)
#define STRATEGY_MANAGER_API_COMPAT_MAX V(0, 0, 1)
#define STRATEGY_MANAGER_IMPL_VERSION   V(0, 0, 1)

// Check that header API version is within supported range
#if STRATEGY_MANAGER_API_VERSION < STRATEGY_MANAGER_API_COMPAT_MIN || \
    STRATEGY_MANAGER_API_VERSION > STRATEGY_MANAGER_API_COMPAT_MAX
#error "Header API version outside implementation compatibility range"
#endif
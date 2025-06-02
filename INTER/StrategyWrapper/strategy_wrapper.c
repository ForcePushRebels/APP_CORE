// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file strategy_wrapper.c
 * @brief Source file for the Intervention Manager module.
 *
 * @author
 * ForcePushRebels – PATO Project (collective contributor)  
 * Uriel Fodong <uriel.fodong@reseau.eseo.fr> (individual contributor)
 *
 * @version 1.0.0
 *
 * @copyright
 * © 2025 ESEO – All rights reserved.
 *
 * @par License
 * PATO ESEO License (see LICENSE.md)
 */

#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "../../helpers/util_macros.h"
#include "strategy_wrapper.h"

#include "xLog.h"
#include "../../symbols/ret_codes.h"
#include <map_engine.h>

#define LOG_TAG "StrategyWrapper"

static StrategyWrapper strategyWrapper;

/* Constructor */
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_wrapper__init(char * name)
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	X_LOG_TRACE("entering strategy_wrapper__init()");

	/* ===== Variables locales ===== */

	/* ===== Logique principale ===== */
	
	strategyWrapper.name = name;

	X_LOG_TRACE("exiting strategy_wrapper__init()");

	/* ===== Postconditions ===== */
    // assert(interventionManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return 1;
}

/* Binders */

void strategy_wrapper__bindMap(mat_t (*map_ptr)[10])
{
	strategyWrapper.map = map_ptr;
}

void strategy_wrapper__bindPrepare(prep_func_cb *prep_func_ptr)
{
	strategyWrapper.prepare = prep_func_ptr;
}

void strategy_wrapper__bindExecute(exec_func_cb *exec_func_ptr)
{
	strategyWrapper.execute = exec_func_ptr;
}

/* Callbacks */

void strategy_wrapper__prepare(mat_t (*map)[10])
{
	strategyWrapper.prepare(map);
}

void strategy_wrapper__execute(seq_t *way, Point *initial, Point *final)
{
	strategyWrapper.execute(way, initial, final);
}
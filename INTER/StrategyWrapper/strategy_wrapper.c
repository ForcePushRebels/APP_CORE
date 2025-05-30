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

/* Constructor */
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
StrategyWrapper *strategy_wrapper__create(char * name)
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	X_LOG_TRACE("entering strategy_wrapper__create()");

	/* ===== Variables locales ===== */
	StrategyWrapper *strategyWrapper;

	/* ===== Logique principale ===== */
	strategyWrapper = malloc(sizeof(StrategyWrapper));
	
	strategyWrapper->name = name;

	X_LOG_TRACE("exiting strategy_wrapper__create()");

	/* ===== Postconditions ===== */
    // assert(interventionManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return strategyWrapper;
}

/* Binders */

void strategy_wrapper__bindMap(StrategyWrapper *self, mat_t *map_ptr)
{
	self->map = map_ptr;
}

void strategy_wrapper__bindPrepare(StrategyWrapper *self, prep_func_cb *prep_func_ptr)
{
	self->prepare = prep_func_ptr;
}

void strategy_wrapper__bindExecute(StrategyWrapper *self, exec_func_cb *exec_func_ptr)
{
	self->execute = exec_func_ptr;
}

/* Callbacks */

void strategy_wrapper__prepare(StrategyWrapper *self, mat_t *map)
{
	self->prepare(map);
}

void strategy_wrapper__execute(StrategyWrapper *self, seq_t *way, Point *initial, Point *final)
{
	self->execute(way, initial, final);
}

/* Destructor */
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_wrapper__delete(StrategyWrapper *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_wrapper__delete()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	free(self); // ⬅️ Libère la mémoire allouée pour l'InterventionManager

	X_LOG_TRACE("exiting strategy_wrapper__delete()");

	/* ===== Postconditions ===== */
    // assert(interventionManager == NULL); // ⬅️ À décommenter. Quand le SAFE_FREE() est utilisé

	return; // ⬅️ À conserver. Retour explicite (void)
}
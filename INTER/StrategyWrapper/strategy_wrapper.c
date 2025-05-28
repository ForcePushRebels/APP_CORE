// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file intervention_manager.c
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

#include "../../logger/log.h"
#include "../../symbols/ret_codes.h"

#define LOG_TAG "StrategyWrapper"

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
StrategyWrapper *strategy_wrapper__create(char * name)
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering intervention_manager__create()");

	/* ===== Variables locales ===== */
	StrategyWrapper *strategyWrapper;

	/* ===== Logique principale ===== */
	strategyWrapper = malloc(sizeof(StrategyWrapper));
	
	strategyWrapper->name = name;

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting intervention_manager__create()");

	/* ===== Postconditions ===== */
    // assert(interventionManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return strategyWrapper;
}

void strategy_wrapper__bindPrepare(StrategyWrapper *self, PrepFuncSign ptr)
{
	self->prepare = ptr;
}

void strategy_wrapper__bindExecute(StrategyWrapper *self, ExecFuncSign ptr)
{
	self->execute = ptr;
}

void strategy_wrapper__prepare(StrategyWrapper *self)
{
	self->prepare();
}

void strategy_wrapper__execute(StrategyWrapper *self)
{
	self->execute();
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_wrapper__delete(StrategyWrapper *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering intervention_manager__delete()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	free(self); // ⬅️ Libère la mémoire allouée pour l'InterventionManager

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting intervention_manager__delete()");

	/* ===== Postconditions ===== */
    // assert(interventionManager == NULL); // ⬅️ À décommenter. Quand le SAFE_FREE() est utilisé

	return; // ⬅️ À conserver. Retour explicite (void)
}
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

static StrategyWrapper strategyWrapper[STRATEGY_NB];

int currentStrategyID = 0; // ID de la stratégie actuellement suivie
int listStratLen = 0; // Nombre de stratégies dans le gestionnaire

/* Constructor */
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_wrapper__init(char * name, prep_func_cb *prep_func, exec_func_cb *exec_func)
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition
	
	X_LOG_TRACE("entering strategy_wrapper__init()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	if (listStratLen >= STRATEGY_NB)
	{
		X_LOG_TRACE("Maximum number of strategies reached (%d). Cannot initialize more.", STRATEGY_NB);
		return 1; // Erreur si on dépasse le nombre de stratégies
	}
	strategyWrapper[listStratLen].name = name;
	strategyWrapper[listStratLen].map = NULL; // Initialisation du pointeur de la matrice
	strategyWrapper[listStratLen].prepare = prep_func; // Initialisation du pointeur de la fonction de préparation
	strategyWrapper[listStratLen].execute = exec_func; // Initialisation du pointeur de la fonction d'exécution

	++listStratLen; // Incrémentation du nombre de stratégies dans le gestionnaire
	X_LOG_TRACE("StrategyWrapper '%s' initialized with ID %d", name, currentStrategyID);

	X_LOG_TRACE("exiting strategy_wrapper__init()");

	/* ===== Postconditions ===== */
    // assert(interventionManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return ret;
}

/* Binders */

void strategy_wrapper__giveIDStrategieToFollow(int id)
{
	/* ===== Préconditions ===== */
	assert(id >= 0 && id < STRATEGY_NB); // Vérifie que l'ID est valide

	X_LOG_TRACE("entering strategy_wrapper__giveIDStrategieToFollow()");

	/* ===== Logique principale ===== */
	if(id < 0 || id >= STRATEGY_NB)
	{
		X_LOG_ERROR("Invalid strategy ID: %d. Must be between 0 and %d.", id, STRATEGY_NB - 1);
		return; // Erreur si l'ID est invalide
	}
	
	X_LOG_TRACE("Strategy ID %d is valid. Setting current strategy.", id);
	
	currentStrategyID = id; // Mise à jour de l'ID de la stratégie actuelle

	X_LOG_TRACE("exiting strategy_wrapper__giveIDStrategieToFollow()");
}

void strategy_wrapper__bindMap(mat_t (*map_ptr)[10])
{
	strategyWrapper[currentStrategyID].map = map_ptr;
}

/* Callbacks */

void strategy_wrapper__prepare(mat_t (*map)[10])
{
	strategyWrapper[currentStrategyID].prepare(map);
}

void strategy_wrapper__execute(seq_t *way, Point *initial, Point *final)
{
	strategyWrapper[currentStrategyID].execute(way, initial, final);
}
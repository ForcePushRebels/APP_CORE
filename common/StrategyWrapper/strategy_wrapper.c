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

#include "util_macros.h"
#include "strategy_wrapper.h"

#include "xAssert.h"
#include "xLog.h"
#include "ret_codes.h"
#include <map_engine.h>

#define LOG_TAG "StrategyWrapper"

static StrategyWrapper strategyWrapper[STRATEGY_NB];
int currentStrategyID = 0; // ID de la stratégie actuellement suivie
int listStratLen = 0; // Nombre de stratégies dans le gestionnaire

int strategy_wrapper__addStrategy(char * name, prep_func_cb *prep_func, exec_func_cb *exec_func)
{
	/* ===== Préconditions ===== */
	X_ASSERT(name != NULL); // Vérifie que le nom de la stratégie n'est pas NULL
	X_ASSERT(prep_func != NULL); // Vérifie que la fonction de préparation n'est pas NULL
	X_ASSERT(exec_func != NULL); // Vérifie que la fonction d'exécution n'est pas NULL

	X_LOG_TRACE("entering strategy_wrapper__init()");

	/* ===== Variables locales ===== */
	int ret = RET_ERR_GENERIC; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	if (listStratLen >= STRATEGY_NB)
	{
		X_LOG_TRACE("Maximum number of strategies reached (%d). Cannot initialize more.", STRATEGY_NB);
		ret = RET_ERR_RANGE;
		goto func_exit; // ⬅️ Sortie anticipée en cas d'erreur
	}
	
	if (name == NULL || prep_func == NULL || exec_func == NULL)
	{
		X_LOG_TRACE("Invalid parameters: name, prep_func, or exec_func is NULL.");
		ret = RET_ERR_NULL;
		goto func_exit; // ⬅️ Sortie anticipée en cas d'erreur
	}

	strategyWrapper[listStratLen].name = name; // Initialisation du nom de la stratégie
	strategyWrapper[listStratLen].map = NULL; // Initialisation du pointeur de la matrice
	strategyWrapper[listStratLen].prepare = prep_func; // Initialisation du pointeur de la fonction de préparation
	strategyWrapper[listStratLen].execute = exec_func; // Initialisation du pointeur de la fonction d'exécution

	++listStratLen; // Incrémentation du nombre de stratégies dans le gestionnaire
	X_LOG_TRACE("StrategyWrapper '%s' initialized with ID %d", name, currentStrategyID);

	ret = RET_OK; // Mise à jour du code de retour pour indiquer le succès

func_exit:

	X_LOG_TRACE("exiting strategy_wrapper__init()");

	/* ===== Postconditions ===== */
	X_ASSERT(listStratLen <= STRATEGY_NB); // Vérifie que le nombre de stratégies ne dépasse pas la limite
	X_ASSERT(currentStrategyID >= 0 && currentStrategyID < STRATEGY_NB); // Vérifie que l'ID de la stratégie actuelle est valide
	X_ASSERT(strategyWrapper[currentStrategyID].name != NULL); // Vérifie que le nom de la stratégie actuelle n'est pas NULL
	X_ASSERT(strategyWrapper[currentStrategyID].prepare != NULL); // Vérifie que la fonction de préparation de la stratégie actuelle n'est pas NULL
	X_ASSERT(strategyWrapper[currentStrategyID].execute != NULL); // Vérifie que la fonction d'exécution de la stratégie actuelle n'est pas NULL

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
		X_LOG_TRACE("Invalid strategy ID: %d. Must be between 0 and %d.", id, STRATEGY_NB - 1);
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
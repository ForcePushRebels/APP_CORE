// SPDX-License-Identifier: LicenseRef-PATO-ESEO

/**
 * @file strategy_manager.c
 * @brief Source file for the Strategy Manager module.
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

#include "util_macros.h"
#include "util_macros.h"
#include "../debug/debug_utils.h"

#define STRATEGY_MANAGER_IMPL_VERSION VER(1, 0, 0)
#include "strategy_manager.h"
#include "strategy_wrapper.h"

#include "ret_codes.h"
#include "supervisor.h"

#include <stdio.h>
#include "xLog.h"

#define LOG_TAG "StrategyManager"

#define config_launcher__giveStrat(listStratName)  // TODO

#define supervisor__giveEndCondition(listEndCondition) // TODO

static StrategyManager strategyManager;

struct strategy_manager_s
{
	Status status;
	mat_t matrix[MAP_SIZE][MAP_SIZE];
	seq_t sequence[MAP_SIZE];
	struct timespec start_time, end_time;
};

int strategy_manager__init()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL);
	X_ASSERT(strategyManager.matrix != NULL);
	X_ASSERT(strategyManager.sequence != NULL);

	X_LOG_DEBUG("entering strategy_manager__init()");

	/* ===== Variables locales ===== */
	int ret = STRATEGY_MANAGER_ERR_INIT;;

	/* ===== Logique principale ===== */
	memset(&strategyManager, 0, sizeof(strategyManager));

	if(strategyManager.matrix == NULL || strategyManager.sequence == NULL)
	{
		X_LOG_FATAL("Failed to allocate memory for strategyManager components");
		goto func_exit; // ⬅️ Sortie anticipée en cas d'erreur
	}
	
	ret = astar_wrapper__init();
	if (ret == 0)
	{
		X_LOG_INFO("AStar strategy initialized successfully");
		ret = STRATEGY_MANAGER_OK; // ⬅️ Mise à jour du code de retour en cas de succès
		goto func_exit; // ⬅️ Sortie anticipée en cas d'erreur
	}

func_exit:

	X_LOG_TRACE("exiting strategy_manager__init()");

	/* ===== Postconditions ===== */
	X_ASSERT(strategyManager.status == INIT); // Vérifie que le statut initial est correct
	X_ASSERT(strategyManager.matrix != NULL); // Vérifie que la matrice est initialisée
	X_ASSERT(strategyManager.sequence != NULL); // Vérifie que la séquence est initialisée
	X_ASSERT(strategyManager.start_time.tv_sec == 0 && strategyManager.end_time.tv_sec == 0); // Vérifie que les temps sont initialisés à zéro

	return ret;
}

void strategy_manager__setMap()
{
	map_engine_get_map(strategyManager.matrix);

	strategy_wrapper__bindMap(strategyManager.matrix);
}

void strategy_manager__askStrat()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	X_LOG_DEBUG("entering strategy_manager__askStrat()");

	/* ===== Logique principale ===== */
	char * listStratName;
	config_launcher__giveStrat(listStratName)  // 📌

	X_LOG_DEBUG("exiting strategy_manager__askStrat()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

int strategy_manager__giveIDStrategieToFollow(int idStrat)
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(idStrat); // ⬅️ À retirer dès que 'idStrat' est utilisé dans la logique

	X_LOG_DEBUG("entering strategy_manager__giveIDStrategieToFollow()");

	/* ===== Variables locales ===== */
	int ret = RET_ERR_GENERIC; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	ret = strategy_wrapper__giveIDStrategieToFollow(idStrat); // 📌
	supervisor__giveEndCondition(listEndCondition); // 📌

	X_LOG_DEBUG("exiting strategy_manager__giveIDStrategieToFollow()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	if(ret == RET_OK)
	{
		ret = STRATEGY_MANAGER_OK; // ⬅️ Mise à jour du code de retour en cas de succès
	}

	return ret; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__startMove()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_DEBUG("entering strategy_manager__startMove()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	/*
		TODO : Démarrer un déplacement basé sur la stratégie courante.
		       Cette fonction peut :
		       - Lire strategyManager.currentStrategyID ou une structure de mouvement
		       - Initialiser un mouvement (ex : appel moteur, consigne de distance/vitesse)
		       - Changer l'état interne : strategyManager.isMoving = true;

		       Exemple :
		           motion_controller__start(strategyManager.motion);
		           strategyManager.isMoving = true;
	*/

	X_LOG_DEBUG("exiting strategy_manager__startMove()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__endMove()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_DEBUG("entering strategy_manager__endMove()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	/*
		TODO : Gérer la fin d'un déplacement stratégique.
		       Cette fonction pourrait :
		       - Arrêter proprement les moteurs ou contrôleurs de mouvement
		       - Mettre à jour l'état interne : strategyManager.isMoving = false;
		       - Notifier le système ou enchaîner vers la prochaine action

		       Exemple :
		           motion_controller__stop(strategyManager.motion);
		           strategyManager.isMoving = false;
	*/

	X_LOG_DEBUG("exiting strategy_manager__endMove()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

bool strategy_manager__alertWallNear()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering strategy_manager__alertWallNear()");

	/* ===== Variables locales ===== */
	bool ret = RET_NOT_IMPL_BOOL; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	/*
		TODO : Implémenter la logique pour détecter si un mur est proche.
		Probablement via les données de strategyManager.Timer ou capteurs associés.
	*/

	X_LOG_TRACE("exiting strategy_manager__alertWallNear()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return ret; // ⬅️ À remplacer par la vraie valeur de retour une fois implémenté
}

void strategy_manager__alertEndConditionReach()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering strategy_manager__alertEndConditionReach()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	/*
		TODO : Implémenter le traitement lorsqu'une condition de fin est atteinte.
		       Par exemple :
		       - Marquer un drapeau interne indiquant que l'objectif est atteint
		       - Passer à une autre stratégie ou arrêter le système
		       - Notifier un autre module (logique d’état ou communication)
		       Exemple :
		           strategyManager.hasReachedEndCondition = true;
		           strategy_manager__switchToIdle(self);
	*/

	X_LOG_TRACE("exiting strategy_manager__alertEndConditionReach()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

int strategy_manager__getStatus()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering strategy_manager__getStatus()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	/*
		TODO : Retourner le statut courant de la stratégie.
		       Cela pourrait impliquer :
		       - Lire un champ d'état interne, ex : strategyManager.status
		       - Évaluer l'état courant à partir de plusieurs flags
		       - Combiner ou normaliser plusieurs états internes

		       Exemple :
		           return strategyManager.status;
		           // ou : return strategy_manager__computeStatus(self);
	*/

	X_LOG_TRACE("exiting strategy_manager__getStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return ret; // ⬅️ Constante temporaire, à remplacer par un vrai code de statut
}

void strategy_manager__reportStatus(MoveReason pilotStatus)
{
	/* ===== Préconditions ===== */
	X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
	assert(0 <= pilotStatus &&           // ⬅️ Vérifie que pilotStatus est dans la plage valide
	       pilotStatus < MOVE_REASON_NB);
	UNUSED(pilotStatus); // ⬅️ À retirer. Dès que 'pilotStatus' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__reportStatus()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	/*
		TODO : Traiter et reporter le statut reçu.
		       Par exemple :
		       - Mettre à jour un champ interne dans self avec pilotStatus
		       - Envoyer un log ou un événement à un autre module
		       - Déclencher une action liée au changement de statut pilote

		       Exemple pseudo-code :
		           strategyManager.lastPilotStatus = pilotStatus;
		           log_info("Pilot status reporté : %d", pilotStatus);
	*/

	X_LOG_TRACE("exiting strategy_manager__reportStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__interlockManuMode()
{
	/* ===== Préconditions ===== */
    X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering strategy_manager__interlockManuMode()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
    /*
        TODO : Implémenter ici la logique d'interverrouillage (interlock) en mode manuel.
        Exemple possible :
            - Vérifier si mode manuel activé
            - Bloquer certaines commandes automatiques
            - Mettre à jour l'état interne en conséquence
    */

	X_LOG_TRACE("exiting strategy_manager__interlockManuMode()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__computeStrat(seq_t *sequence)
{
	/* ===== Préconditions ===== */
    X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_INFO("Computing strategy path...");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	strategy_wrapper__prepare(strategyManager.matrix);

	Point initial = {0, 0}, final = {9, 9};

	strategy_wrapper__execute(sequence, &initial, &final);

	X_LOG_INFO("Strategy path computation completed");
	debug_print_point("Initial position", &initial);
	debug_print_point("Target position", &final);
	debug_print_sequence("Computed path", sequence, MAP_SIZE * MAP_SIZE);
	debug_print_path_grid(sequence, MAP_SIZE * MAP_SIZE, strategyManager.matrix);

	X_LOG_DEBUG("exiting strategy_manager__computeStrat()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

int strategy_manager__startTimer()
{
	/* ===== Préconditions ===== */
    X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
    // X_ASSERT(strategyManager.Timer != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_DEBUG("Starting strategy timer");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
    ret = clock_gettime(CLOCK_MONOTONIC, &strategyManager.start_time);

	X_LOG_TRACE("exiting strategy_manager__startTimer()");

	/* ===== Postconditions ===== */
	// assert(ret == RET_OK); // ⬅️ À décommenter. Pour les plus téméraires

    return ret;
}

int strategy_manager__stopTimer()
{
	/* ===== Préconditions ===== */
    X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
    // X_ASSERT(strategyManager.Timer != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_DEBUG("Stopping strategy timer");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	clock_gettime(CLOCK_MONOTONIC, &strategyManager.end_time);

	X_LOG_TRACE("exiting strategy_manager__stopTimer()");

	/* ===== Postconditions ===== */
	// assert(ret == RET_OK); // ⬅️ À décommenter. Pour les plus téméraires

    return ret;
}

int strategy_manager__getTimeElapsed() {
	return strategyManager.end_time.tv_sec - strategyManager.start_time.tv_sec;
}

void strategy_manager__updateStatus(Status status)
{
	/* ===== Préconditions ===== */
	// Vérifie les invariants avant logique

    X_ASSERT(&strategyManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(status); // ⬅️ À retirer. Dès que 'status' est utilisé en dehors des assert()

	X_LOG_INFO("Updating strategy status to: %d", status);

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	strategyManager.status = status;
   
	X_LOG_TRACE("exiting strategy_manager__updateStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__getInstance(StrategyManager *instance)
{
	/* ===== Préconditions ===== */
	X_ASSERT(instance != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering strategy_manager__getInstance()");

	/* ===== Logique principale ===== */
	*instance = strategyManager;

	X_LOG_TRACE("exiting strategy_manager__getInstance()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}
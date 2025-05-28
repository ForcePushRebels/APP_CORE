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

#include "../../helpers/util_macros.h"

#define STRATEGY_MANAGER_IMPL_VERSION VER(1, 0, 0)
#include "../include/strategy_manager.h"

#include "../../logger/log.h"
#include "../../symbols/ret_codes.h"
#include <geometry.h>

#define LOG_TAG "StrategyManager"

#define config_launcher__giveStrat(listStratName)  // TODO

#define supervisor__giveEndCondition(listEndCondition) // TODO

struct strategy_manager_s
{
	// TODO Ajouter à la conception
	StrategyWrapper **listStrat;
	// TODO Ajouter à la conception
	int currentStrategyID; // ID de la stratégie actuellement suivie
	int listStratLen;

	Status status;

	Point *map;

	// TODO Ajouter à la conception
	// FIXME time_t Timer;
	struct timespec start_time, end_time;
};

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
StrategyManager *strategy_manager__create()
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__create()");

	/* ===== Variables locales ===== */
	StrategyManager *strategyManager; // ⬅️ À remplacer. malloc/calloc pour la persistence

	/* ===== Logique principale ===== */
	strategyManager = malloc(sizeof(StrategyManager)); // ⬅️ Alloue dynamiquement un StrategyManager

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__create()");

	/* ===== Postconditions ===== */
    // assert(strategyManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return strategyManager;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__delete(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__delete()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	free(self); // ⬅️ Libère la mémoire allouée pour le StrategyManager

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__delete()");

	/* ===== Postconditions ===== */
    // assert(strategyManager == NULL); // ⬅️ À décommenter. Quand le SAFE_FREE() est utilisé

	return; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__addStrategy(StrategyManager *self, StrategyWrapper *strategyWrapper)
{	
	self->listStrat[self->listStratLen++] = strategyWrapper;	
}

void strategy_manager__setMap(StrategyManager *self, Point * map)
{	
	self->map = map;	
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__askStrat(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__askStrat()");

	/* ===== Logique principale ===== */
	char * listStratName;
	config_launcher__giveStrat(listStratName)  // 📌

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__askStrat()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__giveIDStrategieToFollow(StrategyManager *self, int idStrat)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer dès que 'self' est utilisé en dehors des assert()
	UNUSED(idStrat); // ⬅️ À retirer dès que 'idStrat' est utilisé dans la logique

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__giveIDStrategieToFollow()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	self->currentStrategyID = idStrat;
	supervisor__giveEndCondition(listEndCondition); // 📌

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__giveIDStrategieToFollow()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__startMove(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__startMove()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	/*
		TODO : Démarrer un déplacement basé sur la stratégie courante.
		       Cette fonction peut :
		       - Lire self->currentStrategyID ou une structure de mouvement
		       - Initialiser un mouvement (ex : appel moteur, consigne de distance/vitesse)
		       - Changer l'état interne : self->isMoving = true;

		       Exemple :
		           motion_controller__start(self->motion);
		           self->isMoving = true;
	*/

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__startMove()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__endMove(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__endMove()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	/*
		TODO : Gérer la fin d'un déplacement stratégique.
		       Cette fonction pourrait :
		       - Arrêter proprement les moteurs ou contrôleurs de mouvement
		       - Mettre à jour l'état interne : self->isMoving = false;
		       - Notifier le système ou enchaîner vers la prochaine action

		       Exemple :
		           motion_controller__stop(self->motion);
		           self->isMoving = false;
	*/

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__endMove()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
bool strategy_manager__alertWallNear(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__alertWallNear()");

	/* ===== Variables locales ===== */
	bool ret = RET_NOT_IMPL_BOOL; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	/*
		TODO : Implémenter la logique pour détecter si un mur est proche.
		Probablement via les données de self->Timer ou capteurs associés.
	*/

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__alertWallNear()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return ret; // ⬅️ À remplacer par la vraie valeur de retour une fois implémenté
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__alertEndConditionReach(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__alertEndConditionReach()");

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
		           self->hasReachedEndCondition = true;
		           strategy_manager__switchToIdle(self);
	*/

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__alertEndConditionReach()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_manager__getStatus(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__getStatus()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	/*
		TODO : Retourner le statut courant de la stratégie.
		       Cela pourrait impliquer :
		       - Lire un champ d'état interne, ex : self->status
		       - Évaluer l'état courant à partir de plusieurs flags
		       - Combiner ou normaliser plusieurs états internes

		       Exemple :
		           return self->status;
		           // ou : return strategy_manager__computeStatus(self);
	*/

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__getStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return ret; // ⬅️ Constante temporaire, à remplacer par un vrai code de statut
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__reportStatus(StrategyManager *self, MoveReason pilotStatus)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
	assert(0 <= pilotStatus &&           // ⬅️ Vérifie que pilotStatus est dans la plage valide
	       pilotStatus < MOVE_REASON_NB);

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()
	UNUSED(pilotStatus); // ⬅️ À retirer. Dès que 'pilotStatus' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__reportStatus()");

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
		           self->lastPilotStatus = pilotStatus;
		           log_info("Pilot status reporté : %d", pilotStatus);
	*/

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__reportStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__interlockManuMode(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__interlockManuMode()");

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

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__interlockManuMode()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__computeStrat(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__computeStrat()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	strategy_wrapper__prepare(self->listStrat[self->currentStrategyID]);

	strategy_wrapper__execute(self->listStrat[self->currentStrategyID]);

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__computeStrat()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_manager__startTimer(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
    // assert(self->Timer != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__startTimer()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
    ret = clock_gettime(CLOCK_MONOTONIC, &self->start_time);

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__startTimer()");

	/* ===== Postconditions ===== */
	// assert(ret == RET_OK); // ⬅️ À décommenter. Pour les plus téméraires

    return ret;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_manager__stopTimer(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
    // assert(self->Timer != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__stopTimer()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	clock_gettime(CLOCK_MONOTONIC, &self->end_time);

	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__stopTimer()");

	/* ===== Postconditions ===== */
	// assert(ret == RET_OK); // ⬅️ À décommenter. Pour les plus téméraires

    return ret;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__updateStatus(StrategyManager *self, Status status)
{
	/* ===== Préconditions ===== */
	// Vérifie les invariants avant logique

    assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	UNUSED(status); // ⬅️ À retirer. Dès que 'status' est utilisé en dehors des assert()

	LOG_DEBUG_MSG(LOG_TAG, ASCII_R2ARROW "entering strategy_manager__updateStatus()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	self->status = status;
   
	LOG_DEBUG_MSG(LOG_TAG, ASCII_L2ARROW "exiting strategy_manager__updateStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}
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
#include "../../StrategyWrapper/strategy_wrapper.h"

#include "../../symbols/ret_codes.h"
#include "../../common/supervisor/supervisor.h"

#include "../../config.h"
#include "xLog.h"

#define LOG_TAG "StrategyManager"

#define config_launcher__giveStrat(listStratName)  // TODO

#define supervisor__giveEndCondition(listEndCondition) // TODO

static void print_colored_grid(StrategyManager *self, seq_t *path, size_t path_len);

struct strategy_manager_s
{
	// TODO Ajouter à la conception
	StrategyWrapper **listStrat;
	// TODO Ajouter à la conception
	int currentStrategyID; // ID de la stratégie actuellement suivie
	int listStratLen;

	Status status;

	mat_t *matrix;
	seq_t *sequence;

	// TODO Ajouter à la conception
	// FIXME time_t Timer;
	struct timespec start_time, end_time;
};

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
StrategyManager *strategy_manager__create()
{
	/* ===== Préconditions ===== */
	X_ASSERT(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	X_LOG_TRACE("entering strategy_manager__create()");

	/* ===== Variables locales ===== */
	StrategyManager *strategyManager; // ⬅️ À remplacer. malloc/calloc pour la persistence

	/* ===== Logique principale ===== */
	strategyManager = malloc(sizeof(StrategyManager)); // ⬅️ Alloue dynamiquement un StrategyManager

	strategyManager->listStrat = malloc(sizeof(StrategyWrapper) * MAX_WRAPPER_PER_MANAGER);
	strategyManager->listStratLen = 0;

	strategyManager->matrix = malloc(sizeof(mat_t) * MAP_SIZE);
	strategyManager->sequence = malloc(sizeof(seq_t) * MAP_SIZE * MAP_SIZE);

	X_LOG_TRACE("exiting strategy_manager__create()");

	/* ===== Postconditions ===== */
    // X_ASSERT(strategyManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return strategyManager;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__delete(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__delete()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	free(self); // ⬅️ Libère la mémoire allouée pour le StrategyManager

	X_LOG_TRACE("exiting strategy_manager__delete()");

	/* ===== Postconditions ===== */
    // X_ASSERT(strategyManager == NULL); // ⬅️ À décommenter. Quand le SAFE_FREE() est utilisé

	return; // ⬅️ À conserver. Retour explicite (void)
}

void strategy_manager__addStrategyWrapper(StrategyManager *self, StrategyWrapper *strategyWrapper)
{	
	self->listStrat[self->listStratLen++] = strategyWrapper;	
}

void strategy_manager__setMap(StrategyManager *self)
{
	map_engine_get_map(self->matrix);

	strategy_wrapper__bindMap(self->listStrat[self->currentStrategyID], self->matrix);
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__askStrat(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	X_LOG_TRACE("entering strategy_manager__askStrat()");

	/* ===== Logique principale ===== */
	char * listStratName;
	config_launcher__giveStrat(listStratName)  // 📌

	X_LOG_TRACE("exiting strategy_manager__askStrat()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__giveIDStrategieToFollow(StrategyManager *self, int idStrat)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer dès que 'self' est utilisé en dehors des assert()
	UNUSED(idStrat); // ⬅️ À retirer dès que 'idStrat' est utilisé dans la logique

	X_LOG_TRACE("entering strategy_manager__giveIDStrategieToFollow()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	self->currentStrategyID = idStrat;
	supervisor__giveEndCondition(listEndCondition); // 📌

	X_LOG_TRACE("exiting strategy_manager__giveIDStrategieToFollow()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__startMove(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__startMove()");

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

	X_LOG_TRACE("exiting strategy_manager__startMove()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__endMove(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__endMove()");

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

	X_LOG_TRACE("exiting strategy_manager__endMove()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
bool strategy_manager__alertWallNear(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__alertWallNear()");

	/* ===== Variables locales ===== */
	bool ret = RET_NOT_IMPL_BOOL; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	/*
		TODO : Implémenter la logique pour détecter si un mur est proche.
		Probablement via les données de self->Timer ou capteurs associés.
	*/

	X_LOG_TRACE("exiting strategy_manager__alertWallNear()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return ret; // ⬅️ À remplacer par la vraie valeur de retour une fois implémenté
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__alertEndConditionReach(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

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
		           self->hasReachedEndCondition = true;
		           strategy_manager__switchToIdle(self);
	*/

	X_LOG_TRACE("exiting strategy_manager__alertEndConditionReach()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_manager__getStatus(StrategyManager *self)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__getStatus()");

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

	X_LOG_TRACE("exiting strategy_manager__getStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return ret; // ⬅️ Constante temporaire, à remplacer par un vrai code de statut
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__reportStatus(StrategyManager *self, MoveReason pilotStatus)
{
	/* ===== Préconditions ===== */
	X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
	assert(0 <= pilotStatus &&           // ⬅️ Vérifie que pilotStatus est dans la plage valide
	       pilotStatus < MOVE_REASON_NB);

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()
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
		           self->lastPilotStatus = pilotStatus;
		           log_info("Pilot status reporté : %d", pilotStatus);
	*/

	X_LOG_TRACE("exiting strategy_manager__reportStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__interlockManuMode(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

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

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__computeStrat(StrategyManager *self, seq_t *sequence)
{
	/* ===== Préconditions ===== */
    X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__computeStrat()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	strategy_wrapper__prepare(self->listStrat[self->currentStrategyID], self->matrix);

	Point initial = {0, 0}, final = {9, 9};

	strategy_wrapper__execute(self->listStrat[self->currentStrategyID], sequence, &initial, &final);

	print_colored_grid(self, sequence, MAP_SIZE * MAP_SIZE);

	X_LOG_TRACE("exiting strategy_manager__computeStrat()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_manager__startTimer(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
    // X_ASSERT(self->Timer != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__startTimer()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
    ret = clock_gettime(CLOCK_MONOTONIC, &self->start_time);

	X_LOG_TRACE("exiting strategy_manager__startTimer()");

	/* ===== Postconditions ===== */
	// assert(ret == RET_OK); // ⬅️ À décommenter. Pour les plus téméraires

    return ret;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int strategy_manager__stopTimer(StrategyManager *self)
{
	/* ===== Préconditions ===== */
    X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
    // X_ASSERT(self->Timer != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__stopTimer()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	clock_gettime(CLOCK_MONOTONIC, &self->end_time);

	X_LOG_TRACE("exiting strategy_manager__stopTimer()");

	/* ===== Postconditions ===== */
	// assert(ret == RET_OK); // ⬅️ À décommenter. Pour les plus téméraires

    return ret;
}

int strategy_manager__getTimeElapsed(StrategyManager *self) {
	return self->end_time.tv_sec - self->start_time.tv_sec;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void strategy_manager__updateStatus(StrategyManager *self, Status status)
{
	/* ===== Préconditions ===== */
	// Vérifie les invariants avant logique

    X_ASSERT(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	UNUSED(status); // ⬅️ À retirer. Dès que 'status' est utilisé en dehors des assert()

	X_LOG_TRACE("entering strategy_manager__updateStatus()");

	/* ===== Variables locales ===== */
    // Déclare les variables temporaires

	/* ===== Logique principale ===== */
	self->status = status;
   
	X_LOG_TRACE("exiting strategy_manager__updateStatus()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

#define ROWS 10
#define COLS 10
#define PATH_MAX_LEN MAP_SIZE * MAP_SIZE
static void print_colored_grid(StrategyManager *self, seq_t *path, size_t path_len) {
	int grid[ROWS][COLS] = {0};  // 0: empty, 1: path, 2: obstacle

	// Mark obstacles and path in single loop
	for (int i = 0; i < ROWS; ++i) {
		for (int j = 0; j < COLS; ++j) {
		if (self->matrix[i][j].type == MAP_CELL_WALL) {
			grid[i][j] = 2; // Mark as obstacle
		}
		}
	}

	// Mark path points
	for (size_t i = 0; i < path_len; ++i) {
		int x = path[i][0];
		int y = path[i][1];
		
		if (x >= 0 && x < ROWS && y >= 0 && y < COLS) {
		grid[x][y] = 1; // Mark as path
		}
	}

	// Print column headers
	printf("   ");
	for (int j = 0; j < COLS; ++j) {
		printf("%2d ", j);
	}
	printf("\n");

	// Print separator line
	printf("   ");
	for (int j = 0; j < COLS; ++j) {
		printf("---");
	}
	printf("\n");

	// Print grid with coloring
	for (int i = 0; i < ROWS; ++i) {
		printf("%2d|", i);  // Row label
		for (int j = 0; j < COLS; ++j) {
			switch (grid[i][j]) {
				case 1: // Path
					printf("\033[1;30;47m # \033[0m");  // black on white
					break;
				case 2: // Obstacle
					printf("\033[1;31m @ \033[0m");     // red
					break;
				default:
					printf(" . ");                      // empty
			}
		}
		printf("\n");
	}
	printf("\n");
}

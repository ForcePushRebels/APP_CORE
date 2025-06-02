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
#define INTERVENTION_MANAGER_IMPL_VERSION VER(1, 0, 0)
#include "../include/intervention_manager.h"

#include "../../logger/log.h"
#include "../../symbols/ret_codes.h"

#define LOG_TAG "InterventionManager"

// Void macros
#define pilot__turn pilot_turn
#define pilot__continuousAdvance pilot_continuousAdvance
#define pilot__stop pilot_stop

#define sensor_manager__startMonitoring startMonitoring
#define sensor_manager__stopMonitoring stopMonitoring

struct intervention_manager_s
{
	StrategyManager *strategyManager;
	
	int interventionPriority;

	seq_t *pathPoints;
	int currentPointIdx;
	int nextPointIdx;
	int angleToNextPoint;
	int distanceToNextPoint;

	Point *listZI;
};

static void intervention_manager__computeStrat(InterventionManager *self);
static int intervention_manager__startTimer(InterventionManager *self);
static int intervention_manager__stopTimer(InterventionManager *self);
static void intervention_manager__updateStatus(InterventionManager *self, Status status);
static int intervention_manager__computeAngleToPoint(InterventionManager *self);
static int intervention_manager__computeDistanceToPoint(InterventionManager *self);
static int intervention_manager__generatePathOfPoints(InterventionManager *self);
static void intervention_manager__retrieveNextPoint(InterventionManager *self);
static int intervention_manager__updateTrace(InterventionManager *self);

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
InterventionManager *intervention_manager__create()
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	X_LOG_TRACE("entering intervention_manager__create()");


	/* ===== Variables locales ===== */
	InterventionManager *interventionManager;

	/* ===== Logique principale ===== */
	interventionManager = malloc(sizeof(InterventionManager));

	interventionManager->strategyManager = strategy_manager__create();

	interventionManager->pathPoints = malloc(sizeof(seq_t) * 100);

	X_LOG_TRACE("exiting intervention_manager__create()");

	/* ===== Postconditions ===== */
    // assert(interventionManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return interventionManager;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__delete(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering intervention_manager__delete()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	strategy_manager__delete(self->strategyManager); // ⬅️ Libère la mémoire allouée pour le StrategyManager

	free(self); // ⬅️ Libère la mémoire allouée pour l'InterventionManager

	X_LOG_TRACE("exiting intervention_manager__delete()");

	/* ===== Postconditions ===== */
    // assert(interventionManager == NULL); // ⬅️ À décommenter. Quand le SAFE_FREE() est utilisé

	return; // ⬅️ À conserver. Retour explicite (void)
}

// TODO Ajouter à la conception
void intervention_manager__followTrajectory(InterventionManager *self) {

	intervention_manager__retrieveNextPoint(self);

	intervention_manager__computeAngleToPoint(self);

	int max_speed = 1;
	int relative = 1;

	if(self->angleToNextPoint == +M_PI_2)
	{
		pilot__turn(-M_PI_2, max_speed, relative);
	}

	if(self->angleToNextPoint == -M_PI_2)
	{
		pilot__turn(+M_PI_2, max_speed, relative);
	}

	intervention_manager__computeDistanceToPoint(self);
	pilot__continuousAdvance(max_speed); // 📌

	intervention_manager__updateTrace(self);

	// geo_positionner__sendTrace(); // 📌
}

void intervention_manager__addStrategyWrapper(InterventionManager *self, StrategyWrapper *strategyWrapper)
{
	strategy_manager__addStrategyWrapper(self->strategyManager, strategyWrapper);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__askStrat(InterventionManager *self)
{
	strategy_manager__askStrat(self->strategyManager);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__giveIDStrategieToFollow(InterventionManager *self, int idStrat)
{
	strategy_manager__giveIDStrategieToFollow(self->strategyManager, idStrat);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__startMove(InterventionManager *self)
{
	strategy_manager__startMove(self->strategyManager);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__endMove(InterventionManager *self)
{
	strategy_manager__endMove(self->strategyManager);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
bool intervention_manager__alertWallNear(InterventionManager *self)
{
	return strategy_manager__alertWallNear(self->strategyManager);
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__alertEndConditionReach(InterventionManager *self)
{
	intervention_manager__stopInter(self);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int intervention_manager__getStatus(InterventionManager *self)
{
	return strategy_manager__getStatus(self->strategyManager);
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__reportStatus(InterventionManager *self, MoveReason pilotStatus)
{
	strategy_manager__reportStatus(self->strategyManager, pilotStatus);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__interlockManuMode(InterventionManager *self)
{
	strategy_manager__interlockManuMode(self->strategyManager);

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__sendPointsSelection(InterventionManager *self, Point **listPoints)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
	assert(listPoints != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()
	UNUSED(listPoints); // ⬅️ À retirer. Dès que 'listPoints' est utilisé en dehors des assert()

	X_LOG_TRACE("entering intervention_manager__sendPointsSelection()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	// self->listZI = listPoints;

	X_LOG_TRACE("exiting intervention_manager__sendPointsSelection()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__startInter(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering intervention_manager__startInter()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires
	
	/* ===== Logique principale ===== */
	intervention_manager__updateStatus(self, MISSION_EN_COURS);

	strategy_manager__setMap(self->strategyManager);

	intervention_manager__computeStrat(self);

	intervention_manager__generatePathOfPoints(self); // TODO remove

	intervention_manager__startTimer(self);

	int roleRobot = 0;
	sensor_manager__startMonitoring(roleRobot); // 📌

	X_LOG_TRACE("exiting intervention_manager__startInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__stopInter(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering intervention_manager__stopInter()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	int decelerationFactor = 1;
	pilot__stop(decelerationFactor); // 📌

	intervention_manager__updateStatus(self, FIN_DE_MISSION);

	sensor_manager__stopMonitoring(); // 📌

	intervention_manager__stopTimer(self);

	X_LOG_TRACE("exiting intervention_manager__stopInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int intervention_manager__getTimeInter(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering intervention_manager__getTimeInter()");

	/* ===== Variables locales ===== */
	int timeInter = 0; // Valeur par défaut, à remplacer par le calcul réel

	/* ===== Logique principale ===== */
	timeInter = strategy_manager__getTimeElapsed(self->strategyManager);

	X_LOG_TRACE("exiting intervention_manager__getTimeInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return timeInter; // ⬅️ Retourne le temps d'intervention calculé
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static void intervention_manager__computeStrat(InterventionManager *self)
{
	strategy_manager__computeStrat(self->strategyManager, self->pathPoints);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__startTimer(InterventionManager *self)
{
	return strategy_manager__startTimer(self->strategyManager);
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__stopTimer(InterventionManager *self)
{
	return strategy_manager__stopTimer(self->strategyManager);
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static void intervention_manager__updateStatus(InterventionManager *self, Status status)
{
	strategy_manager__updateStatus(self->strategyManager, status);

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__computeAngleToPoint(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering computeAngleToPoint()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	self->angleToNextPoint = atan2(self->pathPoints[self->nextPointIdx][1] - self->pathPoints[self->currentPointIdx][1] , \
					self->pathPoints[self->nextPointIdx][0] - self->pathPoints[self->currentPointIdx][0]);
	// Note : Assurez-vous que les coordonnées sont correctement orientées
	// 		selon votre système de coordonnées.

	X_LOG_TRACE("exiting computeAngleToPoint()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique
	
	return ret; // ⬅️ Retourne l'angle calculé ou un code d'erreur
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__computeDistanceToPoint(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering computeDistanceToPoint()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	self->distanceToNextPoint = sqrt(pow(self->pathPoints[self->nextPointIdx][0] - self->pathPoints[self->currentPointIdx][0], 2) +
		                       pow(self->pathPoints[self->nextPointIdx][1] - self->pathPoints[self->currentPointIdx][1], 2));

	X_LOG_TRACE("exiting computeDistanceToPoint()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return 0; // ⬅️ Retourne la distance calculée{

}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__generatePathOfPoints(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
	
	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()
	
	X_LOG_TRACE("entering generatePathOfPoints()");
	
	/* ===== Variables locales ===== */
	Point *pathOfPoints;

	/* ===== Logique principale ===== */
	/*
		TODO : Générer un tableau de positions ordonnées à partir des points listZI.
		       Cela pourrait impliquer :
		       - Parcourir listZI et créer des Position pour chaque point
		       - Stocker ces positions dans pathPoints
		       - Retourner le tableau de positions
	*/
	
	X_LOG_TRACE("exiting generatePathOfPoints()");
	
	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return 1;
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static void intervention_manager__retrieveNextPoint(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(self); // ⬅️ À retirer. Dès que 'self' est utilisé en dehors des assert()

	X_LOG_TRACE("entering retrieveNexPoint()");
	
	/* ===== Variables locales ===== */
	int pathPointsLen;	
	/* ===== Logique principale ===== */
	pathPointsLen = sizeof(self->pathPoints) / sizeof(Point);
	if(self->currentPointIdx >= pathPointsLen)
	{
		return;
	}

	self->currentPointIdx = self->nextPointIdx; // FIXME

	X_LOG_TRACE("exiting retrieveNexPoint()");
	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__updateTrace(InterventionManager *self)
{
	/* ===== Préconditions ===== */
	// Vérifie les invariants avant logique

    assert(self != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini

    UNUSED(self); // ⬅️ À retirer dès que 'self' est utilisé au-delà du assert()

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
    /*
        TODO : Mettre à jour le statut interne de la stratégie.
               Cela peut inclure :
               - Analyse des états internes
               - Mise à jour de flags ou codes de statut dans self
               - Déclenchement d'événements ou notifications si besoin
    */

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

    return ret;
}
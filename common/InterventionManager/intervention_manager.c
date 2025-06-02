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

#include "../helpers/util_macros.h"
#include "../symbols/ret_codes.h"

#define INTERVENTION_MANAGER_IMPL_VERSION VER(1, 0, 0)
#include "intervention_manager.h"

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

	seq_t pathPoints[100];
	int currentPointIdx;
	int nextPointIdx;
	int angleToNextPoint;
	int distanceToNextPoint;

	Point listZI[100]; // Zone d'intérêt
};

static void intervention_manager__computeStrat();
static int intervention_manager__startTimer();
static int intervention_manager__stopTimer();
static void intervention_manager__updateStatus(Status status);
static int intervention_manager__computeAngleToPoint();
static int intervention_manager__computeDistanceToPoint();
static int intervention_manager__generatePathOfPoints();
static void intervention_manager__retrieveNextPoint();
static int intervention_manager__updateTrace();

static InterventionManager interventionManager;

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int intervention_manager__init()
{
	/* ===== Préconditions ===== */
	assert(true); // ⬅️ À conserver. Indique explicitement qu'il n'y a pas de précondition

	X_LOG_TRACE("entering intervention_manager__init()");


	/* ===== Variables locales ===== */

	/* ===== Logique principale ===== */
	strategy_manager__init();

	X_LOG_TRACE("exiting intervention_manager__create()");

	/* ===== Postconditions ===== */
    // assert(interventionManager != NULL); // ⬅️ À décommenter. Pour les plus téméraires

	return 1;
}

// TODO Ajouter à la conception
void intervention_manager__followTrajectory() {

	intervention_manager__retrieveNextPoint();

	intervention_manager__computeAngleToPoint();

	int max_speed = 1;
	int relative = 1;

	if(interventionManager.angleToNextPoint == +M_PI_2)
	{
		pilot__turn(-M_PI_2, max_speed, relative);
	}

	if(interventionManager.angleToNextPoint == -M_PI_2)
	{
		pilot__turn(+M_PI_2, max_speed, relative);
	}

	intervention_manager__computeDistanceToPoint();
	pilot__continuousAdvance(max_speed); // 📌

	intervention_manager__updateTrace();

	// geo_positionner__sendTrace(); // 📌
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__askStrat()
{
	strategy_manager__askStrat();

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__giveIDStrategieToFollow(int idStrat)
{
	strategy_manager__giveIDStrategieToFollow(idStrat);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__startMove()
{
	strategy_manager__startMove();

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__endMove()
{
	strategy_manager__endMove();

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
bool intervention_manager__alertWallNear()
{
	return strategy_manager__alertWallNear();
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__alertEndConditionReach()
{
	intervention_manager__stopInter();

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int intervention_manager__getStatus()
{
	return strategy_manager__getStatus();
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__reportStatus(MoveReason pilotStatus)
{
	strategy_manager__reportStatus(pilotStatus);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__interlockManuMode()
{
	strategy_manager__interlockManuMode();

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__sendPointsSelection(Point **listPoints)
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
	assert(listPoints != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	UNUSED(listPoints); // ⬅️ À retirer. Dès que 'listPoints' est utilisé en dehors des assert()

	X_LOG_TRACE("entering intervention_manager__sendPointsSelection()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	// self.listZI = listPoints;

	X_LOG_TRACE("exiting intervention_manager__sendPointsSelection()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__startInter()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering intervention_manager__startInter()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires
	
	/* ===== Logique principale ===== */
	intervention_manager__updateStatus(MISSION_EN_COURS);

	strategy_manager__setMap();

	intervention_manager__computeStrat();

	intervention_manager__generatePathOfPoints(); // TODO remove

	intervention_manager__startTimer();

	int roleRobot = 0;
	sensor_manager__startMonitoring(roleRobot); // 📌

	X_LOG_TRACE("exiting intervention_manager__startInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
void intervention_manager__stopInter()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering intervention_manager__stopInter()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	int decelerationFactor = 1;
	pilot__stop(decelerationFactor); // 📌

	intervention_manager__updateStatus(FIN_DE_MISSION);

	sensor_manager__stopMonitoring(); // 📌

	intervention_manager__stopTimer();

	X_LOG_TRACE("exiting intervention_manager__stopInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int intervention_manager__getTimeInter()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering intervention_manager__getTimeInter()");

	/* ===== Variables locales ===== */
	int timeInter = 0; // Valeur par défaut, à remplacer par le calcul réel

	/* ===== Logique principale ===== */
	timeInter = strategy_manager__getTimeElapsed();

	X_LOG_TRACE("exiting intervention_manager__getTimeInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return timeInter; // ⬅️ Retourne le temps d'intervention calculé
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static void intervention_manager__computeStrat()
{
	strategy_manager__computeStrat(interventionManager.pathPoints);

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__startTimer()
{
	return strategy_manager__startTimer();
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__stopTimer()
{
	return strategy_manager__stopTimer();
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static void intervention_manager__updateStatus(Status status)
{
	strategy_manager__updateStatus(status);

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__computeAngleToPoint()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering computeAngleToPoint()");

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)

	/* ===== Logique principale ===== */
	interventionManager.angleToNextPoint = atan2(interventionManager.pathPoints[interventionManager.nextPointIdx].y - interventionManager.pathPoints[interventionManager.currentPointIdx].y , \
					interventionManager.pathPoints[interventionManager.nextPointIdx].x - interventionManager.pathPoints[interventionManager.currentPointIdx].x);
	// Note : Assurez-vous que les coordonnées sont correctement orientées
	// 		selon votre système de coordonnées.

	X_LOG_TRACE("exiting computeAngleToPoint()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique
	
	return ret; // ⬅️ Retourne l'angle calculé ou un code d'erreur
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__computeDistanceToPoint()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering computeDistanceToPoint()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	interventionManager.distanceToNextPoint = sqrt(pow(interventionManager.pathPoints[interventionManager.nextPointIdx].x - interventionManager.pathPoints[interventionManager.currentPointIdx].x, 2) +
		                       pow(interventionManager.pathPoints[interventionManager.nextPointIdx].y - interventionManager.pathPoints[interventionManager.currentPointIdx].y, 2));

	X_LOG_TRACE("exiting computeDistanceToPoint()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return 0; // ⬅️ Retourne la distance calculée{

}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__generatePathOfPoints()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)
		
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
static void intervention_manager__retrieveNextPoint()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering retrieveNexPoint()");
	
	/* ===== Variables locales ===== */
	int pathPointsLen;	
	/* ===== Logique principale ===== */
	pathPointsLen = sizeof(interventionManager.pathPoints) / sizeof(Point);
	if(interventionManager.currentPointIdx >= pathPointsLen)
	{
		return;
	}

	interventionManager.currentPointIdx = interventionManager.nextPointIdx; // FIXME

	X_LOG_TRACE("exiting retrieveNexPoint()");
	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
static int intervention_manager__updateTrace()
{
	/* ===== Préconditions ===== */
	// Vérifie les invariants avant logique

    assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini

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
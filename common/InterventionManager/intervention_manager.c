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
#include "../debug/debug_utils.h"

#define INTERVENTION_MANAGER_IMPL_VERSION VER(1, 0, 0)
#include "intervention_manager.h"

// Void macros
#define pilot__turn pilot_turn
#define pilot__continuousAdvance pilot_continuousAdvance
#define pilot__stop pilot_stop

#define sensor_manager__startMonitoring startMonitoring
#define sensor_manager__stopMonitoring stopMonitoring

struct intervention_manager_s
{	
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
static int intervention_manager__retrieveNextPoint();
static int intervention_manager__updateTrace();

static InterventionManager interventionManager;

/////////////////////////////////
/// intervention_manager__init
/////////////////////////////////
int intervention_manager__init()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&interventionManager != NULL);

	X_LOG_DEBUG("entering intervention_manager__init()");

	/* ===== Variables locales ===== */
	int ret = INTERVENTION_MANAGER_ERR_INIT;

	/* ===== Logique principale ===== */
	memset(&interventionManager, 0, sizeof(interventionManager));

	ret = strategy_manager__init();
	if (ret != STRATEGY_MANAGER_OK)
	{
		X_LOG_FATAL("Failed to initialize Strategy Manager");
		goto func_exit;
	}
	
	X_LOG_INFO("Intervention manager initialized successfully");
    ret = INTERVENTION_MANAGER_OK;

func_exit:
    X_LOG_DEBUG("exiting intervention_manager__init()");

	/* ===== Postconditions ===== */
	X_ASSERT(interventionManager.interventionPriority == 0); // ⬅️ Initial value after memset
	X_ASSERT(interventionManager.currentPointIdx == 0); // ⬅️ Initial value after memset
	X_ASSERT(interventionManager.nextPointIdx == 0); // ⬅️ Initial value after memset
	X_ASSERT(interventionManager.angleToNextPoint == 0); // ⬅️ Initial value after memset 
	X_ASSERT(interventionManager.distanceToNextPoint == 0); // ⬅️ Initial value after memset

	for(int i = 0; i < 100; i++)
	{
		X_ASSERT(interventionManager.pathPoints[i].x == 0 && interventionManager.pathPoints[i].y == 0); // ⬅️ Initial values after memset
		X_ASSERT(interventionManager.listZI[i].x == 0 && interventionManager.listZI[i].y == 0); // ⬅️ Initial values after memset
	}

	return ret;
}

/////////////////////////////////
/// intervention_manager__followTrajectory
/////////////////////////////////
void intervention_manager__followTrajectory()
{
    X_LOG_DEBUG("Starting trajectory following");
    
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	/* ===== Variables locales ===== */
	int ret = RET_NOT_IMPL_INT; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)
	int max_speed = 1;
	int relative = 1;

	/* ===== Logique principale ===== */
	ret = intervention_manager__retrieveNextPoint();
	if(ret != INTERVENTION_MANAGER_OK)
	{
		X_LOG_ERROR("Failed to retrieve next point: %d", ret);
		goto func_exit;
	}

	Point current = {
		interventionManager.pathPoints[interventionManager.currentPointIdx].x,
		interventionManager.pathPoints[interventionManager.currentPointIdx].y
	};
	Point next = {
		interventionManager.pathPoints[interventionManager.nextPointIdx].x,
		interventionManager.pathPoints[interventionManager.nextPointIdx].y
	};

	debug_print_point("Current position", &current);
	debug_print_point("Next target", &next);

	ret = intervention_manager__computeAngleToPoint();
	if(ret != RET_OK)
	{
		X_LOG_ERROR("Failed to compute angle to next point: %d", ret);
		goto func_exit;
	}

	ret = intervention_manager__computeDistanceToPoint();
	if(ret != RET_OK)
	{
		X_LOG_ERROR("Failed to compute distance to next point: %d", ret);
		goto func_exit;
	}

	X_LOG_INFO("Moving to point: angle=%.2f, distance=%.2f", 
        interventionManager.angleToNextPoint,
        interventionManager.distanceToNextPoint);

	if(interventionManager.angleToNextPoint == +M_PI_2)
	{
		pilot__turn(-M_PI_2, max_speed, relative);
	}

	if(interventionManager.angleToNextPoint == -M_PI_2)
	{
		pilot__turn(+M_PI_2, max_speed, relative);
	}

	ret = pilot_advance(interventionManager.distanceToNextPoint, max_speed);
	if(ret != RET_OK)
	{
		X_LOG_ERROR("Failed to advance to next point: %d", ret);
		goto func_exit;
	}

	intervention_manager__updateTrace();
	
func_exit:

	X_LOG_DEBUG("exiting intervention_manager__followTrajectory()");
}

/////////////////////////////////
/// intervention_manager__askStrat
/////////////////////////////////
void intervention_manager__askStrat()
{
	strategy_manager__askStrat();

	return; // ⬅️ À conserver. Retour explicite (void)
}

// @Override
__attribute__((unused)) // ⬅️ À retirer. Lorsque la fonction est utilisée
int intervention_manager__giveIDStrategieToFollow(int idStrat)
{
	if(strategy_manager__giveIDStrategieToFollow(idStrat) == RET_OK)
	{
		return INTERVENTION_MANAGER_OK; // ⬅️ À conserver. Retour explicite (void)
	}

}

/////////////////////////////////
/// intervention_manager__startMove
/////////////////////////////////
void intervention_manager__startMove()
{
	// @Override
	strategy_manager__startMove();

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__endMove
/////////////////////////////////
void intervention_manager__endMove()
{
	int ret = intervention_manager__retrieveNextPoint();
	if(ret != RET_OK)
	{
		return; // ⬅️ À conserver. Sortie anticipée en cas d'erreur
	}

	intervention_manager__followTrajectory();

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__alertWallNear
/////////////////////////////////
bool intervention_manager__alertWallNear()
{
	return strategy_manager__alertWallNear();
}

/////////////////////////////////
/// intervention_manager__alertEndConditionReach
/////////////////////////////////
void intervention_manager__alertEndConditionReach()
{
	intervention_manager__stopInter();

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__getStatus
/////////////////////////////////
int intervention_manager__getStatus()
{
	return strategy_manager__getStatus();
}

/////////////////////////////////
/// intervention_manager__reportStatus
/////////////////////////////////
void intervention_manager__reportStatus(MoveReason pilotStatus)
{
	strategy_manager__reportStatus(pilotStatus);

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__interlockManuMode
/////////////////////////////////
void intervention_manager__interlockManuMode()
{
	strategy_manager__interlockManuMode();

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__sendPointsSelection
/////////////////////////////////
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
int intervention_manager__startInter()
{
    X_LOG_INFO("Starting intervention");
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering intervention_manager__startInter()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires
	
	/* ===== Logique principale ===== */
	intervention_manager__updateStatus(MISSION_EN_COURS);

	strategy_manager__setMap();

	intervention_manager__computeStrat();

	X_LOG_DEBUG("Generating path points");
	intervention_manager__generatePathOfPoints(); // TODO remove

	intervention_manager__startTimer();

	int roleRobot = 0;
	X_LOG_DEBUG("Starting monitoring with role: %d", roleRobot);
	sensor_manager__startMonitoring(roleRobot); // 📌

	intervention_manager__followTrajectory();

	X_LOG_TRACE("exiting intervention_manager__startInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return INTERVENTION_MANAGER_OK; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__stopInter
/////////////////////////////////
void intervention_manager__stopInter()
{
    X_LOG_INFO("Stopping intervention");
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering intervention_manager__stopInter()");

	/* ===== Variables locales ===== */
	// Déclare les variables temporaires

	/* ===== Logique principale ===== */
	int decelerationFactor = 1;
	pilot__stop(decelerationFactor); // 📌

	intervention_manager__updateStatus(FIN_DE_MISSION);

	X_LOG_DEBUG("Stopping monitoring and timers");
	sensor_manager__stopMonitoring(); // 📌

	intervention_manager__stopTimer();

	X_LOG_TRACE("exiting intervention_manager__stopInter()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__getTimeInter
/////////////////////////////////
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

/////////////////////////////////
/// intervention_manager__computeStrat
/////////////////////////////////
static void intervention_manager__computeStrat()
{
	/* ===== Préconditions ===== */
	X_ASSERT(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	// @Override
	strategy_manager__computeStrat(interventionManager.pathPoints);
	debug_print_sequence("Generated path", interventionManager.pathPoints, 100);

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__startTimer
/////////////////////////////////
static int intervention_manager__startTimer()
{
	return strategy_manager__startTimer();
}

/////////////////////////////////
/// intervention_manager__stopTimer
/////////////////////////////////
static int intervention_manager__stopTimer()
{
	return strategy_manager__stopTimer();
}

/////////////////////////////////
/// intervention_manager__updateStatus
/////////////////////////////////
static void intervention_manager__updateStatus(Status status)
{
	strategy_manager__updateStatus(status);

	return; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__computeAngleToPoint
/////////////////////////////////
static int intervention_manager__computeAngleToPoint()
{
    X_LOG_DEBUG("Computing angle to next point");

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

	X_LOG_INFO("Angle computed: %.2f", interventionManager.angleToNextPoint);

	X_LOG_TRACE("exiting computeAngleToPoint()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique
	
	return ret; // ⬅️ Retourne l'angle calculé ou un code d'erreur
}

/////////////////////////////////
/// intervention_manager__computeDistanceToPoint
/////////////////////////////////
static int intervention_manager__computeDistanceToPoint()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering computeDistanceToPoint()");

	/* ===== Variables locales ===== */
	int ret = RET_ERR_GENERIC;
	
	/* ===== Logique principale ===== */
	interventionManager.distanceToNextPoint = sqrt(pow(interventionManager.pathPoints[interventionManager.nextPointIdx].x - interventionManager.pathPoints[interventionManager.currentPointIdx].x, 2) +
		                       pow(interventionManager.pathPoints[interventionManager.nextPointIdx].y - interventionManager.pathPoints[interventionManager.currentPointIdx].y, 2));

	X_LOG_TRACE("exiting computeDistanceToPoint()");

	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

	return 0; // ⬅️ Retourne la distance calculée{

}

/////////////////////////////////
/// intervention_manager__generatePathOfPoints
/////////////////////////////////
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

/////////////////////////////////
/// intervention_manager__retrieveNextPoint
/////////////////////////////////
static int intervention_manager__retrieveNextPoint()
{
	/* ===== Préconditions ===== */
	assert(&interventionManager != NULL); // ⬅️ À conserver. Désactivé si NDEBUG est défini (build release)

	X_LOG_TRACE("entering retrieveNexPoint()");
	
	/* ===== Variables locales ===== */
	int ret = RET_ERR_GENERIC; // ⬅️ "Rater-vite". Initialisé par un code d'erreur (prog défensive)
	int pathPointsLen;
	/* ===== Logique principale ===== */
	pathPointsLen = sizeof(interventionManager.pathPoints) / sizeof(Point);
	
	if(interventionManager.currentPointIdx >= pathPointsLen)
	{
		ret = RET_ERR_RANGE;
		goto func_exit;
	}

	interventionManager.currentPointIdx = interventionManager.nextPointIdx; // FIXME

	ret = INTERVENTION_MANAGER_OK;

	X_LOG_TRACE("exiting retrieveNexPoint()");
	/* ===== Postconditions ===== */
	// Vérifie les invariants après logique

func_exit:
	
	return ret; // ⬅️ À conserver. Retour explicite (void)
}

/////////////////////////////////
/// intervention_manager__updateTrace
/////////////////////////////////
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
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include "hardwareAbstraction.h"
#include "xNetwork.h"
#include "xLog.h"
#include "xAssert.h"
#include "watchdog.h"
#include "networkServer.h"
#include "handleNetworkMessage.h"
#include "idCard.h"
#include "sensorManager.h"
#include "motorControl.h"
#include "xTimer.h"
#include "map_engine.h"
#include "safetyController.h"
#include "sensorManager.h"

#define SENSOR_OBSTACLE_THRESHOLD 150

int main(void)
{
    int l_iReturn = 0;

    // Initialisation des logs
    t_logCtx t_LogConfig;
    t_LogConfig.t_bLogToFile = true;
    t_LogConfig.t_bLogToConsole = true;
    snprintf((char*)t_LogConfig.t_cLogPath, sizeof(t_LogConfig.t_cLogPath), "explo.log");
    l_iReturn = xLogInit(&t_LogConfig);
    X_ASSERT(l_iReturn == XOS_LOG_OK);

    // Initialisation du hardware abstraction layer
    l_iReturn = hardwareAbstractionInit();
    X_ASSERT(l_iReturn == 0);

    // Initialisation du sensor manager
    l_iReturn = sensorManagerInit();
    X_ASSERT(l_iReturn == SENSOR_MANAGER_OK);

    // Initialisation du contrôle moteur
    l_iReturn = motor_control_init();
    X_ASSERT(l_iReturn == 0);

    // Initialisation du SafetyController
    SafetyController safetyCtrl;
    SafetyController_init(&safetyCtrl);

    X_LOG_TRACE("=== Début du test SafetyController ===");

  while (1)
{
    uint16_t sensor_values[SENSOR_MANAGER_SENSORS_COUNT];
    if (GetSensorValues(sensor_values) == 0) {
        for (int i = 0; i < SENSOR_MANAGER_SENSORS_COUNT; i++) {
            X_LOG_TRACE("Capteur %d = %d", i, sensor_values[i]);
        }
    }

    if (checkForward()) {
        X_LOG_TRACE("Voie libre : avance !");
        moveForward(&safetyCtrl, 4.0f);
        usleep(500 * 1000); // 500 ms

        X_LOG_TRACE("Tourne à droite !");
        moveRight(&safetyCtrl, 2.0f, true);
        usleep(500 * 1000); // 500 ms

        X_LOG_TRACE("Tourne à gauche !");
        moveLeft(&safetyCtrl, 2.0f, true);
        usleep(500 * 1000); // 500 ms

    } else {
        X_LOG_TRACE("Obstacle détecté : arrêt d'urgence !");
        stop(&safetyCtrl, 1.0);
        usleep(500 * 1000); // 500 ms
    }
}

    // Arrêt propre (jamais atteint ici)
    motor_control_shutdown();
    //sensorManagerClose();
    stopMonitoring();
    
    xLogClose();

    return 0;
}
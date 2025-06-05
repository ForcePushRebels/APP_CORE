////////////////////////////////////////////////////////////
//  main source file
//  implements main function
//
// general discloser: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

// system includes
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

// project includes
#include "handleNetworkMessage.h"
#include "hardwareAbstraction.h"
#include "idCard.h"
#include "map_engine.h"
#include "motorControl.h"
#include "networkServer.h"
#include "pilot.h"
#include "positionControl.h"
#include "safetyController.h"
#include "sensorManager.h"
#include "supervisor.h"
#include "watchdog.h"
#include "xAssert.h"
#include "xLog.h"
#include "xNetwork.h"
#include "xTimer.h"

#include "ihm.h"
#include "map_engine.h"
#include "explorationManager.h"

// chemin des logs avec l'executable en chemin de l'executable
static const uint8_t s_aCLogPath[] = "explo.log";
int start = 0;
// Définition des durées pour le code morse (en millisecondes)
#define UNIT_TIME 300
#define SHORT_SIGNAL (UNIT_TIME)
#define LONG_SIGNAL (UNIT_TIME * 3)
#define PAUSE_SIGNAL (UNIT_TIME)
#define PAUSE_LETTER (UNIT_TIME * 3)

// Fonction pour faire clignoter la LED avec une couleur pendant une durée
void blinkLed(mrpiz_led_rgb_color_t t_eColor, int t_iDurationMs)
{
    SetLedColor(t_eColor);
    usleep(t_iDurationMs * 1000);
    SetLedColor(MRPIZ_LED_OFF);
    usleep(PAUSE_SIGNAL * 1000);
}

// Fonction pour envoyer un SOS en morse avec différentes couleurs
void sendMorseSOS()
{
    // S: ... (3 courts en rouge)
    for (int i = 0; i < 3; i++)
    {
        blinkLed(MRPIZ_LED_RED, SHORT_SIGNAL);
    }

    // Pause entre lettres
    usleep((PAUSE_LETTER - PAUSE_SIGNAL) * 100);

    // O: --- (3 longs en vert)
    for (int i = 0; i < 3; i++)
    {
        blinkLed(MRPIZ_LED_GREEN, LONG_SIGNAL);
    }

    // Pause entre lettres
    usleep((PAUSE_LETTER - PAUSE_SIGNAL) * 100);

    // S: ... (3 courts en bleu)
    for (int i = 0; i < 3; i++)
    {
        blinkLed(MRPIZ_LED_BLUE, SHORT_SIGNAL);
    }

    // Pause finale
    usleep(PAUSE_LETTER * 100);
}

static void l_fWatchdogExpiryHandler(void)
{
    // X_LOG_TRACE("Watchdog expired");
    // X_ASSERT(false);
}

int main()
{
    int l_iReturn = 0;

    // Configuration des logs - le chemin complet sera construit automatiquement
    t_logCtx t_LogConfig;
    t_LogConfig.t_bLogToFile = false;
    t_LogConfig.t_bLogToConsole = true;
    strncpy(t_LogConfig.t_cLogPath, (const char *)s_aCLogPath, sizeof(t_LogConfig.t_cLogPath) - 1);
    t_LogConfig.t_cLogPath[sizeof(t_LogConfig.t_cLogPath) - 1] = '\0';

    // initialisation des logs
    l_iReturn = xLogInit(&t_LogConfig);
    X_ASSERT(l_iReturn == XOS_LOG_OK);

    // init hardware abstraction
    l_iReturn = hardwareAbstractionInit();
    X_ASSERT(l_iReturn == 0);
    X_LOG_TRACE("Hardware abstraction initialized successfully");

    // init watchdog
    l_iReturn = watchdog_init(300);
    X_ASSERT(l_iReturn == WATCHDOG_SUCCESS);
    watchdog_set_expiry_handler(l_fWatchdogExpiryHandler);

    // Initialisation du système de handlers de messages
    initMessageHandlerSystem();

    // init server
    l_iReturn = networkServerInit();
    X_ASSERT(l_iReturn == SERVER_OK);

    ServerConfig l_tServerConfig = networkServerCreateDefaultConfig();
    l_tServerConfig.t_usPort = 8080;
    l_tServerConfig.t_pcBindAddress = "0.0.0.0";
    l_tServerConfig.t_iMaxClients = 10;
    l_tServerConfig.t_iBacklog = 5;
    l_tServerConfig.t_bUseTimeout = false;
    l_tServerConfig.t_iReceiveTimeout = 0;

    l_iReturn = networkServerConfigure(&l_tServerConfig);
    X_ASSERT(l_iReturn == SERVER_OK);

    // Définir le gestionnaire de messages
    networkServerSetMessageHandler(handleNetworkMessage);

    // Configurer et initialiser la découverte UDP
    idCardNetworkInit();

    // init sensor manager
    l_iReturn = sensorManagerInit();
    X_ASSERT(l_iReturn == SENSOR_MANAGER_OK);

    // start monitoring
    l_iReturn = startMonitoring();
    X_ASSERT(l_iReturn == SENSOR_MANAGER_OK);

    // Initialisation du contrôle des moteurs
    l_iReturn = motor_control_init();
    X_ASSERT(l_iReturn == 0);
    X_LOG_TRACE("Motor control initialized");

    // Initialisation du contrôle de position
    l_iReturn = position_control_init();
    X_ASSERT(l_iReturn == POSITION_OK);
    X_LOG_TRACE("Position control initialized");

    // Initialisation du pilotage
    l_iReturn = pilot_init(); // Exemple: (max_speed, rayon_roue_m, entraxe_m)
    X_ASSERT(l_iReturn == PILOT_OK);
    X_LOG_TRACE("Pilot initialized");

    // start server
    l_iReturn = networkServerStart();
    X_ASSERT(l_iReturn == SERVER_OK);

    // init map engine
    l_iReturn = map_engine_init();
    X_ASSERT(l_iReturn == MAP_ENGINE_OK);

    // init supervisor
    l_iReturn = supervisor_init();
    X_ASSERT(l_iReturn == SUPERVISOR_OK);
    X_LOG_TRACE("Supervisor initialized");

    safetyControllerInit();

    // init exploration manager
    explorationManager_start();
    X_LOG_TRACE("Exploration manager initialized");

    while (1)
    {
        sleep(1);
    }

    // Ce code ne sera jamais atteint, mais pour être complet:
    hardwareAbstractionClose();
    motor_control_shutdown();
    position_control_shutdown();
    cleanupMessageHandlerSystem();
    idCardNetworkCleanup();
    networkServerStop();
    networkServerCleanup();

    return 0;
}

////////////////////////////////////////////////////////////
//  main source file
//  implements main function
//
// general discloser: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

// system includes
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

// project includes
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
#include "pilot.h"
#include "positionControl.h"

#include "map_engine.h"

// chemin des logs avec l'executable en chemin de l'executable
static const uint8_t s_aCLogPath[] = "explo.log";

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

// Fonction de test des moteurs
void testMotors(void)
{
    static uint8_t test_phase = 0;
    static uint64_t last_phase_time = 0;
    uint64_t current_time = xTimerGetCurrentMs();

    // Changer de phase toutes les 3 secondes
    if (current_time - last_phase_time > 3000)
    {
        test_phase = (test_phase + 1) % 8; // 8 phases au total
        last_phase_time = current_time;

        // Arrêter les moteurs avant de changer de phase
        motor_control_stop();
        xTimerDelay(1000); // Attendre 1000ms pour stabilisation
    }

    // Différentes phases de test
    switch (test_phase)
    {
    case 0:                                // Moteur gauche en avant
        motor_control_set_left_speed(2.0); // 2 rad/s
        motor_control_set_right_speed(0.0);
        X_LOG_TRACE("Test phase 0: Left motor forward");
        break;

    case 1:                                 // Moteur gauche en arrière
        motor_control_set_left_speed(-2.0); // -2 rad/s
        motor_control_set_right_speed(0.0);
        X_LOG_TRACE("Test phase 1: Left motor backward");
        break;

    case 2:                                 // Moteur droit en avant
        motor_control_set_right_speed(2.0); // 2 rad/s
        motor_control_set_left_speed(0.0);
        X_LOG_TRACE("Test phase 2: Right motor forward");
        break;

    case 3:                                  // Moteur droit en arrière
        motor_control_set_right_speed(-2.0); // -2 rad/s
        motor_control_set_left_speed(0.0);
        X_LOG_TRACE("Test phase 3: Right motor backward");
        break;

    case 4: // Les deux moteurs en avant
        motor_control_set_left_speed(2.0);
        motor_control_set_right_speed(2.0);
        X_LOG_TRACE("Test phase 4: Both motors forward");
        break;

    case 5: // Les deux moteurs en arrière
        motor_control_set_left_speed(-2.0);
        motor_control_set_right_speed(-2.0);
        X_LOG_TRACE("Test phase 5: Both motors backward");
        break;

    case 6: // Rotation gauche (moteur gauche en arrière, droit en avant)
        motor_control_set_left_speed(-2.0);
        motor_control_set_right_speed(2.0);
        X_LOG_TRACE("Test phase 6: Left rotation");
        break;

    case 7: // Rotation droite (moteur gauche en avant, droit en arrière)
        motor_control_set_left_speed(2.0);
        motor_control_set_right_speed(-2.0);
        X_LOG_TRACE("Test phase 7: Right rotation");
        break;
    }

    // Afficher les vitesses actuelles

    X_LOG_TRACE("Current speeds - Left: %.2f rad/s, Right: %.2f rad/s",
                motor_control_get_left_speed(),
                motor_control_get_right_speed());
}

// Fonction de test du hardware abstraction layer
void testHardwareAbstraction(void)
{
    static uint8_t test_phase = 0;
    static uint64_t last_phase_time = 0;
    uint64_t current_time = xTimerGetCurrentMs();

    // Changer de phase toutes les 2 secondes
    if (current_time - last_phase_time > 2000)
    {
        test_phase = (test_phase + 1) % 6; // 6 phases de test
        last_phase_time = current_time;

        // Arrêter les moteurs avant de changer de phase
        SetMotorSpeed(0, 0);
        SetMotorSpeed(1, 0);
        xTimerDelay(500); // Attendre 500ms pour stabilisation
    }

    // Différentes phases de test
    switch (test_phase)
    {
    case 0: // Test des moteurs
        X_LOG_TRACE("=== Testing Motors ===");
        SetMotorSpeed(0, 50); // Moteur gauche à 50%
        SetMotorSpeed(1, 50); // Moteur droit à 50%

        // Lire les encodeurs
        uint16_t motor_ids[2] = {0, 1};
        if (GetMotorEncoderValues(motor_ids) == 0)
        {
            X_LOG_TRACE("Encoder values - Left: %d, Right: %d", motor_ids[0], motor_ids[1]);
        }
        break;

    case 1: // Test des capteurs
        X_LOG_TRACE("=== Testing Sensors ===");
        uint16_t sensor_values[HARDWARE_ABSTRACTION_MAX_SENSORS];
        if (GetSensorValues(sensor_values) == 0)
        {
            for (int i = 0; i < HARDWARE_ABSTRACTION_MAX_SENSORS; i++)
            {
                X_LOG_TRACE("Sensor %d value: %d", i, sensor_values[i]);
            }
        }
        break;

    case 2: // Test de la batterie
        X_LOG_TRACE("=== Testing Battery ===");
        int battery_level = GetBatteryLevel();
        float battery_voltage = GetBatteryVoltage();
        X_LOG_TRACE("Battery level: %d%%, Voltage: %.2fV", battery_level, battery_voltage);
        break;

    case 3: // Test des LEDs
        X_LOG_TRACE("=== Testing LEDs ===");
        SetLedColor(MRPIZ_LED_RED);
        X_LOG_TRACE("LED set to RED");
        break;

    case 4: // Test des LEDs (suite)
        X_LOG_TRACE("=== Testing LEDs (continued) ===");
        SetLedColor(MRPIZ_LED_GREEN);
        X_LOG_TRACE("LED set to GREEN");
        break;

    case 5: // Test des LEDs (fin)
        X_LOG_TRACE("=== Testing LEDs (final) ===");
        SetLedColor(MRPIZ_LED_BLUE);
        X_LOG_TRACE("LED set to BLUE");
        break;
    }
}

// Fonction de test du contrôle de position
void testPositionControl(void)
{
    static uint8_t test_phase = 0;
    static uint64_t last_phase_time = 0;
    uint64_t current_time = xTimerGetCurrentMs();

    // Changer de phase toutes les 5 secondes
    if (current_time - last_phase_time > 5000)
    {
        test_phase = (test_phase + 1) % 6; // 6 phases de test
        last_phase_time = current_time;

        // Arrêter le mouvement avant de changer de phase
        position_control_stop();
        xTimerDelay(1000); // Attendre 1000ms pour stabilisation
    }

    // Différentes phases de test
    switch (test_phase)
    {
    case 0: // Avancer de 500mm
        X_LOG_TRACE("Test phase 0: Advance 500mm");
        position_control_advance(500, 2.0); // 500mm à 2 rad/s
        break;

    case 1: // Rotation à gauche de π/2 radians (90 degrés)
        X_LOG_TRACE("Test phase 1: Rotate left π/2 rad (90°)");
        position_control_turn(M_PI / 2, 2.0); // π/2 rad à 2 rad/s
        break;

    case 2: // Avancer de 300mm
        X_LOG_TRACE("Test phase 2: Advance 300mm");
        position_control_advance(300, 2.0); // 300mm à 2 rad/s
        break;

    case 3: // Rotation à droite de π radians (180 degrés)
        X_LOG_TRACE("Test phase 3: Rotate right π rad (180°)");
        position_control_turn(-M_PI, 2.0); // -π rad à 2 rad/s
        break;

    case 4: // Avancer de 200mm
        X_LOG_TRACE("Test phase 4: Advance 200mm");
        position_control_advance(200, 2.0); // 200mm à 2 rad/s
        break;

    case 5: // Rotation à gauche de π/2 radians (90 degrés)
        X_LOG_TRACE("Test phase 5: Rotate left π/2 rad (90°)");
        position_control_turn(M_PI / 2, 2.0); // π/2 rad à 2 rad/s
        break;
    }
}
void testPilot(void)
{
    static bool command_sent = false;

    // // Afficher la position courante
    // Position pos = pilot_getPosition();
    // X_LOG_TRACE("Pilot position: x=%.1f mm, y=%.1f mm, theta=%.2f rad", pos.x, pos.y, pos.theta);

    // N'envoie la commande qu'une seule fois
    if (!command_sent)
    {
        X_LOG_TRACE("Pilot test: Advance 1000mm");
        pilot_advance(1000, 2.0); // 500mm à 2 rad/s
        command_sent = true;
    }
}

int main()
{
    int l_iReturn = 0;
    char l_cExecutablePath[256] = {0};
    char l_cLogPath[256 + sizeof(s_aCLogPath)] = {0};

    l_iReturn = xLogGetExecutablePath(l_cExecutablePath, sizeof(l_cExecutablePath));
    if (l_iReturn < 0)
    {
        strcpy(l_cExecutablePath, ".");
    }
    snprintf(l_cLogPath, sizeof(l_cLogPath), "%s/%s", l_cExecutablePath, s_aCLogPath);

    t_logCtx t_LogConfig;
    t_LogConfig.t_bLogToFile = false;
    t_LogConfig.t_bLogToConsole = true;
    strncpy(t_LogConfig.t_cLogPath, l_cLogPath, sizeof(t_LogConfig.t_cLogPath) - 1);
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
    l_tServerConfig.t_pcBindAddress = "127.0.0.1";
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
    X_ASSERT(l_iReturn == 0);
    X_LOG_TRACE("Position control initialized");

    // Initialisation du pilotage
    l_iReturn = pilot_init(10, 0.03, 0.15); // Exemple: (max_speed, rayon_roue_m, entraxe_m)
    X_ASSERT(l_iReturn == 0);
    X_LOG_TRACE("Pilot initialized");

    // start server
    l_iReturn = networkServerStart();
    X_ASSERT(l_iReturn == SERVER_OK);

    // init map engine
    l_iReturn = map_engine_init();
    X_ASSERT(l_iReturn == MAP_ENGINE_OK);

    // main loop
    while (1)
    {
        // Test des moteurs
        // testMotors();

        // Envoyer le signal SOS en morse
        // sendMorseSOS();
        testPilot(); // <-- Active cette ligne pour tester le pilotage
        // xTimerDelay(100); // Ajoute un petit délai pour éviter de saturer le CPU
        // testPositionControl();
        // Pour envoyer des mises à jour périodiques, on devra attendre d'avoir un client connecté
        // et utiliser serverSendMessage à ce moment-là.
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

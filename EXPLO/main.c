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
/*
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
*/
// Fonction de test du contrôle de position
void testPositionControl(void)
{
    static uint8_t test_phase = 0;
    static uint64_t last_phase_time = 0;
    static uint64_t last_position_update = 0;
    uint64_t current_time = xTimerGetCurrentMs();
    Position_t current_position;
    
    // Afficher la position toutes les 500ms
    if (current_time - last_position_update > 500) {
        if (position_control_get_position(&current_position) == 0) {
            //TODO
        }
        last_position_update = current_time;
    }
    
    // Changer de phase toutes les 5 secondes
    if (current_time - last_phase_time > 5000)
    {
        test_phase = (test_phase + 1) % 6; // 6 phases de test
        last_phase_time = current_time;

        // Arrêter le mouvement avant de changer de phase
        //position_control_stop();
        xTimerDelay(1000); // Attendre 1000ms pour stabilisation
        
        // Afficher la position finale de la phase précédente
        if (position_control_get_position(&current_position) == 0) {
            X_LOG_TRACE("Phase %d completed - Final position: X: %d mm, Y: %d mm, Angle: %.2f rad (%.1f°)",
                        test_phase,
                        current_position.x_mm,
                        current_position.y_mm,
                        current_position.angle_rad,
                        current_position.angle_rad * 180.0 / M_PI);
        }
    }
    if(start == 0)
    {
        position_control_advance(1000, 2.0);
        //position_control_turn(-M_PI, 1.0);
        sleep(10);
        //X_LOG_TRACE("STOP !");

        position_control_stop();
        
        while(!position_control_is_motion_finished());
        //X_LOG_TRACE("STOP DONE");
        position_control_turn(M_PI, 1.0);
        
        start = 1;
    }
    
}
void testPilot(void)
{
    static int phase = 0;
    static uint64_t phase_start_time = 0;

    uint64_t now = xTimerGetCurrentMs();

    switch (phase)
    {
    case 0:
        X_LOG_TRACE("Pilot test: Advance 1000mm");
        pilot_advance(1000, 2.0);
        phase_start_time = now;
        phase = 1;
        break;

    case 1:
        // Stop après 3 secondes
        if (now - phase_start_time > 3000)
        {
            X_LOG_TRACE("Pilot test: STOP");
            pilot_stop();
            phase_start_time = now;
            phase = 2;
        }
        break;

    case 2:
        // Attendre 2 secondes à l'arrêt, puis rotation
        if (now - phase_start_time > 2000)
        {
            X_LOG_TRACE("Pilot test: Turn 90 deg left");
            pilot_turn(M_PI/2, 1.0, true); // Tourner de 90° à gauche (relatif)
            phase_start_time = now;
            phase = 3;
        }
        break;

    case 3:
        // Tu peux ajouter d'autres phases ici si besoin
        break;
    }
}

int main()
{
    int l_iReturn = 0;

    // Configuration des logs - le chemin complet sera construit automatiquement
    t_logCtx t_LogConfig;
    t_LogConfig.t_bLogToFile = false;
    t_LogConfig.t_bLogToConsole = true;
    strncpy(t_LogConfig.t_cLogPath, (const char*)s_aCLogPath, sizeof(t_LogConfig.t_cLogPath) - 1);
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
    l_iReturn = pilot_init(); // Exemple: (max_speed, rayon_roue_m, entraxe_m)
    X_ASSERT(l_iReturn == PILOT_OK);
    X_LOG_TRACE("Pilot initialized");

    // start server
    l_iReturn = networkServerStart();

    X_LOG_TRACE("Lireturn %x", l_iReturn);
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
        //testPilot(); // <-- Active cette ligne pour tester le pilotage
        // xTimerDelay(100); // Ajoute un petit délai pour éviter de saturer le CPU
        testPositionControl();
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

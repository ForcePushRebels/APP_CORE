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

// project includes
#include "hardwareAbstraction.h"
#include "xNetwork.h"
#include "xLog.h"
#include "xAssert.h"
#include "watchdog.h"

static const uint8_t s_aCLogPath[] = "explo.log";

// Définition des durées pour le code morse (en millisecondes)
#define UNIT_TIME 300
#define SHORT_SIGNAL (UNIT_TIME)
#define LONG_SIGNAL (UNIT_TIME * 3)
#define PAUSE_SIGNAL (UNIT_TIME)
#define PAUSE_LETTER (UNIT_TIME * 3)

// Fonction pour faire clignoter la LED avec une couleur pendant une durée
void blinkLed(mrpiz_led_rgb_color_t color, int duration_ms) {
    SetLedColor(color);
    usleep(duration_ms * 1000);
    SetLedColor(MRPIZ_LED_OFF);
    usleep(PAUSE_SIGNAL * 1000);
}

// Fonction pour envoyer un SOS en morse avec différentes couleurs
void sendMorseSOS() {
    
    // S: ... (3 courts en rouge)
    for (int i = 0; i < 3; i++) {
        blinkLed(MRPIZ_LED_RED, SHORT_SIGNAL);
    }
    
    // Pause entre lettres
    usleep((PAUSE_LETTER - PAUSE_SIGNAL) * 100);
    
    // O: --- (3 longs en vert)
    for (int i = 0; i < 3; i++) {
        blinkLed(MRPIZ_LED_GREEN, LONG_SIGNAL);
    }
    
    // Pause entre lettres
    usleep((PAUSE_LETTER - PAUSE_SIGNAL) * 100);
    
    // S: ... (3 courts en bleu)
    for (int i = 0; i < 3; i++) {
        blinkLed(MRPIZ_LED_BLUE, SHORT_SIGNAL);
    }
    
    // Pause finale
    usleep(PAUSE_LETTER * 100);
}

static void l_fWatchdogExpiryHandler(void)
{
    X_LOG_TRACE("Watchdog expired");
    X_ASSERT(false);
}

int main()
{
    int l_iReturn = 0;

    t_logCtx t_LogConfig;
    t_LogConfig.t_bLogToFile = true;
    t_LogConfig.t_bLogToConsole = true;
    memcpy(t_LogConfig.t_cLogPath, s_aCLogPath, sizeof(s_aCLogPath));

    // initiatlisation des logs
    l_iReturn = xLogInit(&t_LogConfig);
    X_ASSERT(l_iReturn == XOS_LOG_OK);

    // init hardware abstraction
    l_iReturn = hardwareAbstractionInit();
    X_ASSERT(l_iReturn == 0);

    // init watchdog
    l_iReturn = watchdog_init(300);
    X_ASSERT(l_iReturn == 0);
    watchdog_set_expiry_handler(l_fWatchdogExpiryHandler);

    while (1)
    {
        // Envoyer le signal SOS en morse
        sendMorseSOS();
        
    }
}

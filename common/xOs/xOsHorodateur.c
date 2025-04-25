#include "xOsHorodateur.h"
#include "xAssert.h"
#include <time.h>
#include <string.h>
#include <stdio.h>


#define XOS_HORODATEUR_BUFFER_SIZE 64   // Size of the buffer for the formatted timestamp

// Buffer thread-local : each thread has its own buffer
static __thread char s_cTimeBuffer[XOS_HORODATEUR_BUFFER_SIZE];

////////////////////////////////////////////////////////////
/// xHorodateurGetString
////////////////////////////////////////////////////////////
const char* xHorodateurGetString(void)
{
    struct timespec ts;
    struct tm tm_result;

    // R�initialiser le buffer thread-local²
    memset(s_cTimeBuffer, 0, XOS_HORODATEUR_BUFFER_SIZE);

    // Obtenir l'heure actuelle avec haute r�solution
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
        return NULL;
    }

    // Conversion en heure locale de fa�on thread-safe
    if (localtime_r(&ts.tv_sec, &tm_result) == NULL) {
        return NULL;
    }

    // Formater la partie "date/heure" (YYYY-MM-DD HH:MM:SS)
    if (strftime(s_cTimeBuffer, XOS_HORODATEUR_BUFFER_SIZE, "%Y-%m-%d %H:%M:%S", &tm_result) == 0) {
        return NULL;
    }

    // Calculer la taille actuelle dans le buffer
    size_t len = strlen(s_cTimeBuffer);

    // Concat�ner directement les millisecondes dans le buffer
    // ts.tv_nsec est en nanosecondes, on le convertit en millisecondes
    int written = snprintf(s_cTimeBuffer + len, XOS_HORODATEUR_BUFFER_SIZE - len, ".%03ld", ts.tv_nsec / 1000000);
    if (written < 0 || (size_t)written >= XOS_HORODATEUR_BUFFER_SIZE - len) {
        return NULL;
    }

    return s_cTimeBuffer;
}

////////////////////////////////////////////////////////////
/// xHorodateurGet
////////////////////////////////////////////////////////////
uint32_t xHorodateurGet(void)
{
    struct timespec ts;
    // Obtenir le temps actuel
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
        X_ASSERT(0); // Erreur critique
        return 0;
    }
    // Retourner la partie secondes (timestamp Unix)
    return (uint32_t)ts.tv_sec;
}
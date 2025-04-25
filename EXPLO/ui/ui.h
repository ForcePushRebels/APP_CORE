#ifndef IHM_H
#define IHM_H

#include <stdbool.h>

// Définition des constantes
#define MAX_COMMAND_LENGTH 100
#define VERSION "1.2.4"

// Initialise l'IHM
void ihm_init(void);

// Affiche l'en-tête de l'application
void ihm_display_header(void);

// Affiche l'état actuel
void ihm_display_status(const char* status, int battery, const char* mode);

// Traite une commande utilisateur
// Retourne true si le programme doit continuer, false s'il doit s'arrêter
bool ihm_process_command(const char* command);

// Affiche le prompt et récupère la commande utilisateur
void ihm_get_command(char* command, int max_length);

// Fonction principale de l'IHM, à appeler depuis le main
// Retourne 0 en cas de succès, -1 en cas d'erreur
int ihm_run(void);

#endif // IHM_H

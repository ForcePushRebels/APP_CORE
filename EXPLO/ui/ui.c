#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "ui.h"
#include "hardwareAbstraction.h"

// Variables globales
static bool g_initialized = false;

// Fonction pour initialiser l'IHM
void ihm_init(void) {
    if (!g_initialized) {
        g_initialized = true;
        printf("\033[2J\033[H"); // Efface l'écran et positionne le curseur en haut à gauche
    }
}

// Fonction pour afficher l'en-tête de l'application
void ihm_display_header(void) {
    printf("╔════════════════════════════════════════════════════════════════╗\n");
    printf("║                                                                ║\n");
    printf("║                  EXPLO v%s (Incrément 1)                    ║\n", VERSION);
    printf("║                                                                ║\n");
    printf("╚════════════════════════════════════════════════════════════════╝\n\n");
}

// Fonction pour afficher l'état actuel
void ihm_display_status(const char* status, int battery, const char* mode) {
    printf("┌─────────────┐ ┌───────────────┐ ┌───────────────┐\n");
    printf("│ État: %-5s │ │ Batterie: %2d%% │ │ Mode: %-7s │\n", status, battery, mode);
    printf("└─────────────┘ └───────────────┘ └───────────────┘\n\n");
}

// Fonction pour afficher le menu d'aide
static void display_help(void) {
    printf("┌─ Menu d'aide ──────────────────────────────────────────────────┐\n");
    printf("│ Commande  │ Description                                        │\n");
    printf("│ aide      │ Affiche ce menu d'aide                             │\n");
    printf("│ metriques │ Affiche les métriques actuelles                    │\n");
    printf("│ info      │ Affiche les informations détaillées                │\n");
    printf("│ z         │ Fait avancer le robot                              │\n");
    printf("│ q         │ Fait tourner le robot à gauche                     │\n");
    printf("│ d         │ Fait tourner le robot à droite                     │\n");
    printf("│ s         │ Arrête le robot                                    │\n");
    printf("│ quitter   │ Quitte le programme                                │\n");
    printf("└────────────────────────────────────────────────────────────────┘\n\n");
}

// Fonction pour afficher les informations détaillées
static void display_info(void) {
    // Récupérer les informations réelles du robot
    float batteryVoltage = GetBatteryVoltage();
    int batteryLevel = GetBatteryLevel();
    
    printf("╔═ Informations détaillées ═══════════════════════════════════════╗\n");
    printf("║ État détaillé du robot:                                         ║\n");
    printf("║ • Statut     : Operationnel                                     ║\n");
    printf("║ • Batterie   : %d%% (%.2fV)                                     ║\n", 
           batteryLevel, batteryVoltage);
    printf("║ • Position   : (X: 10, Y: 15)                                   ║\n");
    printf("║ • Orientation: 45°                                              ║\n");
    printf("║ • Version    : %s                                            ║\n", VERSION);
    printf("║ • Date       : 25/04/2025 11:42                                 ║\n");
    printf("╚═════════════════════════════════════════════════════════════════╝\n\n");
}

// Fonction pour afficher les métriques
static void display_metrics(void) {
    printf("┌─ Métriques actuelles ───────────────────────────────────────────────────────────┐\n");
    printf("│                         PERFORMANCES D'EXPLORATION                              │\n");
    printf("├──────────────────────┬──────────────────────┬───────────────────────────────────┤\n");
    printf("│ Temps total          │ Distance parcourue   │ Pourcentage de zone cartographiée │\n");
    printf("│ d'exploration        │                      │                                   │\n");
    printf("│      120 secondes    │      45,3 mètres     │              68%%                  │\n");
    printf("├──────────────────────┴──────────────────────┼───────────────────────────────────┤\n");
    printf("│ Vitesse moyenne d'exploration: 22,5 m²/minute                                   │\n");
    printf("├─────────────────────────────────────────────────────────────────────────────────┤\n");
    printf("│                        PERFORMANCES OPÉRATIONNELLES                             │\n");
    printf("├─────────────────────────────────────────────────────────────────────────────────┤\n");
    printf("│ Consommation de batterie: 5,2%% par heure                                        │\n");
    printf("└─────────────────────────────────────────────────────────────────────────────────┘\n\n");
}

// Fonction pour contrôler le robot avec les touches Z, Q, D, S
static void control_robot(char key) {
    switch(tolower(key)) {
        case 'z':
            printf("Commande: Avancer\n");
            // Faire avancer les deux moteurs
            SetMotorSpeed(0, 50);  // Moteur gauche
            SetMotorSpeed(1, 50);  // Moteur droit
            break;
        case 'q':
            printf("Commande: Tourner à gauche\n");
            // Tourner à gauche (moteur droit avance, moteur gauche recule)
            SetMotorSpeed(0, -30);  // Moteur gauche
            SetMotorSpeed(1, 30);   // Moteur droit
            break;
        case 'd':
            printf("Commande: Tourner à droite\n");
            // Tourner à droite (moteur gauche avance, moteur droit recule)
            SetMotorSpeed(0, 30);   // Moteur gauche
            SetMotorSpeed(1, -30);  // Moteur droit
            break;
        case 's':
            printf("Commande: Arrêter\n");
            // Arrêter les deux moteurs
            SetMotorSpeed(0, 0);  // Moteur gauche
            SetMotorSpeed(1, 0);  // Moteur droit
            break;
        default:
            printf("Commande de contrôle non reconnue\n");
            break;
    }
    printf("\n");
}

// Fonction pour traiter une commande utilisateur
bool ihm_process_command(const char* command) {
    if (strlen(command) == 0) {
        return true;
    }
    
    printf("\n");
    
    // Traitement des commandes
    if (strcmp(command, "aide") == 0) {
        display_help();
    } else if (strcmp(command, "info") == 0) {
        display_info();
    } else if (strcmp(command, "metriques") == 0) {
        display_metrics();
    } else if (strcmp(command, "quitter") == 0) {
        printf("Arrêt du système EXPLO ...\n");
        return false;
    } else if (strlen(command) == 1 && strchr("zqds", tolower(command[0]))) {
        // Commandes de contrôle du robot
        control_robot(command[0]);
    } else {
        printf("Commande non reconnue. Tapez 'aide' pour voir les commandes disponibles.\n\n");
    }
    
    return true;
}

// Fonction pour afficher le prompt et récupérer la commande utilisateur
void ihm_get_command(char* command, int max_length) {
    printf("EXPLO > ");
    fgets(command, max_length, stdin);
    
    // Supprimer le caractère de nouvelle ligne
    command[strcspn(command, "\n")] = 0;
}

// Fonction principale de l'IHM, à appeler depuis le main
int ihm_run(void) {
    char command[MAX_COMMAND_LENGTH];
    bool running = true;
    
    // Initialisation
    ihm_init();
    
    // Affichage initial
    ihm_display_header();
    ihm_display_status("PRÊT", GetBatteryLevel(), "AUTO");
    
    // Boucle principale
    while (running) {
        ihm_get_command(command, MAX_COMMAND_LENGTH);
        running = ihm_process_command(command);
    }
    
    return 0;
}

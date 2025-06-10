/**
 * @file ihm.c
 * @brief IHM
 * @author dbenech
 * @date 02/06/2025
 * @see ihm.c
 * @copyright Cecill-C (Cf. LICENCE.txt)
 */

/* ******************************************************* Includes ****************************************************** */
#include "ihm.h"
#include "xLog.h"
#include "xOsMemory.h"
#include "xOsMutex.h"
#include "xTask.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* **************************************************** Private macros *************************************************** */
#define MAX_COMMANDS 10
#define BANNER_WIDTH 64
#define STATUS_BOX_WIDTH 15

/* ************************************************ Private type definition ********************************************** */

/* ********************************************* Private functions declarations ****************************************** */
static void ihm_process_command(const char *input);
static void ihm_display_prompt(void);
static int ihm_read_input(char *input);
static void ihm_print_line(const char *char_type, int width);
static int ihm_find_command(const char *cmd_name);

/* ************************************************** Private variables ************************************************** */
static xOsTaskCtx ihmTask;
static ihm_command_t commands[MAX_COMMANDS];
static int command_count = 0;
static ihm_robot_status_t current_status;
static ihm_metrics_t current_metrics;

/* ********************************************** Private functions definitions ****************************************** */

static void *ihm_task(void *arg)
{
    xOsTaskCtx *task = (xOsTaskCtx *)arg;
    char input[IHM_MAX_CMD_LENGTH];

    // Display initial banner and status
    ihm_display_banner();
    ihm_display_status_bar(&current_status);
    printf("\n");

    while (task->a_iStopFlag == OS_TASK_SECURE_FLAG)
    {
        // Clear input buffer
        memset(input, 0, sizeof(input));

        ihm_display_prompt();
        int ret = ihm_read_input(input);

        if (ret == 0 && strlen(input) > 0)
        {
            ihm_process_command(input);
        }
        else if (ret == -1)
        {
            // Handle input error or EOF
            printf("Erreur de lecture input\n");
            break;
        }

        printf("\n");
        fflush(stdout); // Force flush output buffer
    }

    printf("Arrêt de la tâche IHM\n");
    return NULL;
}

static void ihm_process_command(const char *input)
{
    char cmd_name[IHM_MAX_CMD_NAME_LENGTH];
    const char *args = NULL;

    // Safety check
    if (input == NULL || strlen(input) == 0)
    {
        return;
    }

    // Initialize cmd_name buffer
    memset(cmd_name, 0, sizeof(cmd_name));

    // Extract command name and arguments
    const char *space_pos = strchr(input, ' ');
    if (space_pos != NULL)
    {
        size_t cmd_len = (size_t)(space_pos - input);
        if (cmd_len >= IHM_MAX_CMD_NAME_LENGTH)
        {
            cmd_len = IHM_MAX_CMD_NAME_LENGTH - 1;
        }
        strncpy(cmd_name, input, cmd_len);
        cmd_name[cmd_len] = '\0';

        // Skip whitespace in arguments
        args = space_pos + 1;
        while (*args == ' ')
            args++; // Skip leading spaces
        if (*args == '\0')
            args = NULL; // No arguments if only spaces
    }
    else
    {
        strncpy(cmd_name, input, IHM_MAX_CMD_NAME_LENGTH - 1);
        cmd_name[IHM_MAX_CMD_NAME_LENGTH - 1] = '\0';
    }

    // Find and execute command
    int cmd_index = ihm_find_command(cmd_name);
    if (cmd_index >= 0)
    {
        // Safety check before calling callback
        if (commands[cmd_index].callback != NULL)
        {
            commands[cmd_index].callback(args);
        }
        else
        {
            printf("Erreur: callback de commande NULL\n");
        }
    }
    else
    {
        // Display unknown command message in a box
        printf("\n");
        printf(" Commande inconnue: '%s'                        \n", cmd_name);
        printf("                                                  \n");
        printf(" Tapez 'aide' pour voir la liste des commandes   \n");
        printf(" disponibles.                                     \n");

        // Draw frame around the error message
        ihm_draw_frame(4, 53, FRAME_TYPE_SINGLE, "COMMANDE INCONNUE");
    }
}

static void ihm_display_prompt(void)
{
    printf("%s", IHM_PROMPT);
    fflush(stdout);
}

static int ihm_read_input(char *input)
{
    // Clear any pending input in stdin buffer
    fflush(stdin);

    if (fgets(input, IHM_MAX_CMD_LENGTH, stdin) != NULL)
    {
        // Remove newline character
        size_t len = strlen(input);
        if (len > 0 && input[len - 1] == '\n')
        {
            input[len - 1] = '\0';
        }

        // Handle case where input is too long
        if (len == (size_t)(IHM_MAX_CMD_LENGTH - 1) && input[len - 1] != '\n')
        {
            // Clear remaining characters in stdin buffer
            int c;
            while ((c = getchar()) != '\n' && c != EOF)
                ;
        }

        return 0; // Success
    }

    // Check for EOF or error
    if (feof(stdin))
    {
        printf("EOF détecté\n");
        return -2; // EOF
    }

    if (ferror(stdin))
    {
        printf("Erreur de lecture stdin\n");
        clearerr(stdin);
        return -1; // Error
    }

    return -1; // Generic error
}

static void ihm_print_line(const char *char_type, int width)
{
    for (int i = 0; i < width; i++)
    {
        printf("%s", char_type);
    }
}

static int ihm_find_command(const char *cmd_name)
{
    for (size_t i = 0; i < (size_t)command_count; i++)
    {
        if (strcmp(commands[i].name, cmd_name) == 0)
        {
            return (int)i;
        }
    }
    return -1;
}

/* ********************************************** Public functions definitions ******************************************* */

int ihm_init()
{
    // Initialize default robot status
    strcpy(current_status.status, "PRÊT");
    current_status.battery_level = 85;
    strcpy(current_status.mode, "AUTO");
    current_status.position_x = 10.0f;
    current_status.position_y = 15.0f;
    current_status.orientation = 45.0f;
    strcpy(current_status.version, "1.2.4");

    // Initialize default metrics
    current_metrics.exploration_time_sec = 120;
    current_metrics.distance_traveled_m = 45.3f;
    current_metrics.mapped_percentage = 68;
    current_metrics.exploration_speed_m2_min = 22.5f;
    current_metrics.battery_consumption_per_hour = 5.2f;

    // Register commands
    strcpy(commands[command_count].name, "aide");
    commands[command_count].description = "Affiche ce menu d'aide";
    commands[command_count].callback = ihm_cmd_aide;
    command_count++;

    strcpy(commands[command_count].name, "info");
    commands[command_count].description = "Affiche les informations détaillées";
    commands[command_count].callback = ihm_cmd_info;
    command_count++;

    strcpy(commands[command_count].name, "metriques");
    commands[command_count].description = "Affiche les métriques actuelles";
    commands[command_count].callback = ihm_cmd_metriques;
    command_count++;

    strcpy(commands[command_count].name, "quitter");
    commands[command_count].description = "Quitte le programme";
    commands[command_count].callback = ihm_cmd_quitter;
    command_count++;

    int ret = osTaskInit(&ihmTask);
    if (ret != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("ihm_init: osTaskInit failed");
        return ret;
    }
    ihmTask.t_ptTask = ihm_task;
    ihmTask.t_ptTaskArg = (void *)&ihmTask;
    atomic_init(&ihmTask.a_iStopFlag, OS_TASK_SECURE_FLAG);

    ret = osTaskCreate(&ihmTask);
    if (ret != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("ihm_init: osTaskCreate failed");
        return ret;
    }
    return IHM_OK;
}

void ihm_display_banner(void)
{
    printf("%s", BOX_TOP_LEFT);
    ihm_print_line(BOX_HORIZONTAL, BANNER_WIDTH);
    printf("%s\n", BOX_TOP_RIGHT);

    printf("%s%*s%s\n", BOX_VERTICAL, BANNER_WIDTH, "", BOX_VERTICAL);
    printf("%s%*sEXPLO v%s (Incrément 1) %*s%s\n", BOX_VERTICAL, 18, "", current_status.version, 19, "", BOX_VERTICAL);
    printf("%s%*s%s\n", BOX_VERTICAL, BANNER_WIDTH, "", BOX_VERTICAL);

    printf("%s", BOX_BOTTOM_LEFT);
    ihm_print_line(BOX_HORIZONTAL, BANNER_WIDTH);
    printf("%s\n\n", BOX_BOTTOM_RIGHT);
}

void ihm_display_status_bar(const ihm_robot_status_t *status)
{
    // Display the status information without borders first
    printf(" État: %-6s   Batterie: %d%%   Mode: %-6s \n", status->status, status->battery_level, status->mode);

    // Draw frame around the status bar
    ihm_draw_frame(1, 48, FRAME_TYPE_SINGLE, "Statut du robot");
}

void ihm_display_box(const char *title, const char *content, int width)
{
    // Top border
    printf("%s", BOX_TOP_LEFT);
    if (title != NULL)
    {
        printf("%s %s ", BOX_HORIZONTAL, title);
        ihm_print_line(BOX_HORIZONTAL, width - (int)strlen(title) - 4);
    }
    else
    {
        ihm_print_line(BOX_HORIZONTAL, width);
    }
    printf("%s\n", BOX_TOP_RIGHT);

    // Content
    if (content != NULL)
    {
        printf("%s %s", BOX_VERTICAL, content);
        int content_len = (int)strlen(content);
        for (int i = content_len; i < width - 1; i++)
        {
            printf(" ");
        }
        printf("%s\n", BOX_VERTICAL);
    }

    // Bottom border
    printf("%s", BOX_BOTTOM_LEFT);
    ihm_print_line(BOX_HORIZONTAL, width);
    printf("%s\n", BOX_BOTTOM_RIGHT);
}

void ihm_set_robot_status(const ihm_robot_status_t *status)
{
    if (status != NULL)
    {
        current_status = *status;
    }
}

void ihm_set_metrics(const ihm_metrics_t *metrics)
{
    if (metrics != NULL)
    {
        current_metrics = *metrics;
    }
}

void ihm_draw_frame(int lines_count, int width, int frame_type, const char *title)
{
    const char *top_left, *top_right, *bottom_left, *bottom_right;
    const char *horizontal, *vertical;

    // Select frame characters based on type
    if (frame_type == FRAME_TYPE_DOUBLE)
    {
        top_left = BOX_TOP_LEFT;
        top_right = BOX_TOP_RIGHT;
        bottom_left = BOX_BOTTOM_LEFT;
        bottom_right = BOX_BOTTOM_RIGHT;
        horizontal = BOX_HORIZONTAL;
        vertical = BOX_VERTICAL;
    }
    else
    {
        top_left = BOX_LIGHT_TOP_LEFT;
        top_right = BOX_LIGHT_TOP_RIGHT;
        bottom_left = BOX_LIGHT_BOTTOM_LEFT;
        bottom_right = BOX_LIGHT_BOTTOM_RIGHT;
        horizontal = BOX_LIGHT_HORIZONTAL;
        vertical = BOX_LIGHT_VERTICAL;
    }

    // Save current cursor position
    printf(ANSI_CURSOR_SAVE);

    // Move cursor up to draw top border
    printf("\033[%dA", lines_count + 1);

    // Draw top border
    printf("%s", top_left);
    if (title != NULL)
    {
        printf("%s %s ", horizontal, title);
        int remaining = width - (int)strlen(title) - 5;
        for (int i = 0; i < remaining; i++)
        {
            printf("%s", horizontal);
        }
    }
    else
    {
        for (int i = 0; i < width - 2; i++)
        {
            printf("%s", horizontal);
        }
    }
    printf("%s\n", top_right);

    // Draw side borders for each content line
    for (int i = 0; i < lines_count; i++)
    {
        printf("%s", vertical);
        printf("\033[%dC", width - 2); // Move cursor to right position
        printf("%s\n", vertical);
    }

    // Draw bottom border
    printf("%s", bottom_left);
    for (int i = 0; i < width - 2; i++)
    {
        printf("%s", horizontal);
    }
    printf("%s\n", bottom_right);

    // Restore cursor position
    printf(ANSI_CURSOR_RESTORE);
}

/* ***************************************** Public callback functions definitions *************************************** */

int ihm_cmd_aide(const char *args)
{
    (void)args;
    printf("\n");
    printf(" %-9s │ %-48s \n", "Commande", "Description");

    for (size_t i = 0; i < (size_t)command_count; i++)
    {
        printf(" %-9s │ %-48s \n", commands[i].name, commands[i].description);
    }

    // Draw frame around the help menu
    ihm_draw_frame(command_count + 1, 62, FRAME_TYPE_SINGLE, "Menu d'aide");

    return IHM_OK;
}

int ihm_cmd_info(const char *args)
{
    (void)args;
    time_t now;
    struct tm *timeinfo;
    char date_str[32];

    time(&now);
    timeinfo = localtime(&now);
    strftime(date_str, sizeof(date_str), "%d/%m/%Y %H:%M", timeinfo);

    printf("\n");
    printf(" État détaillé du robot:                                         \n");
    printf(" • Statut     : %-30s \n", current_status.status);
    printf(" • Batterie   : %d%%                           \n", current_status.battery_level);
    printf(" • Position   : (X: %.0f, Y: %.0f)               \n", current_status.position_x, current_status.position_y);
    printf(" • Orientation: %.0f°                          \n", current_status.orientation);
    printf(" • Version    : %s                        \n", current_status.version);
    printf(" • Date       : %s                     \n", date_str);

    // Draw frame around the info
    ihm_draw_frame(7, 67, FRAME_TYPE_DOUBLE, "Informations détaillées");

    return IHM_OK;
}

int ihm_cmd_metriques(const char *args)
{
    (void)args;
    printf("\n%s", BOX_LIGHT_TOP_LEFT);
    printf("%s Métriques actuelles ", BOX_LIGHT_HORIZONTAL);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 60);
    printf("%s\n", BOX_LIGHT_TOP_RIGHT);

    printf("%s%*sPERFORMANCES D'EXPLORATION%*s %s\n", BOX_LIGHT_VERTICAL, 27, "", 26, "", BOX_LIGHT_VERTICAL);

    printf("%s", BOX_LIGHT_T_RIGHT);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 22);
    printf("%s", BOX_LIGHT_T_DOWN);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 22);
    printf("%s", BOX_LIGHT_T_DOWN);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 35);
    printf("%s\n", BOX_LIGHT_T_LEFT);

    printf("%s Temps total          %s Distance parcourue   %s Pourcentage de zone cartographiée %s\n",
           BOX_LIGHT_VERTICAL,
           BOX_LIGHT_VERTICAL,
           BOX_LIGHT_VERTICAL,
           BOX_LIGHT_VERTICAL);
    printf("%s d'exploration        %s                      %s                                   %s\n",
           BOX_LIGHT_VERTICAL,
           BOX_LIGHT_VERTICAL,
           BOX_LIGHT_VERTICAL,
           BOX_LIGHT_VERTICAL);
    printf("%s      %d secondes    %s      %.1f mètres     %s              %d%%                  %s\n",
           BOX_LIGHT_VERTICAL,
           current_metrics.exploration_time_sec,
           BOX_LIGHT_VERTICAL,
           current_metrics.distance_traveled_m,
           BOX_LIGHT_VERTICAL,
           current_metrics.mapped_percentage,
           BOX_LIGHT_VERTICAL);

    printf("%s", BOX_LIGHT_T_RIGHT);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 22);
    printf("%s", BOX_LIGHT_T_UP);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 22);
    printf("%s", BOX_LIGHT_T_DOWN);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 35);
    printf("%s\n", BOX_LIGHT_T_LEFT);

    printf("%s Vitesse moyenne d'exploration: %.1f m²/minute%*s %s\n",
           BOX_LIGHT_VERTICAL,
           current_metrics.exploration_speed_m2_min,
           19,
           "",
           BOX_LIGHT_VERTICAL);

    printf("%s", BOX_LIGHT_T_RIGHT);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 81);
    printf("%s\n", BOX_LIGHT_T_LEFT);

    printf("%s%*sPERFORMANCES OPÉRATIONNELLES%*s %s\n", BOX_LIGHT_VERTICAL, 26, "", 26, "", BOX_LIGHT_VERTICAL);

    printf("%s", BOX_LIGHT_T_RIGHT);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 81);
    printf("%s\n", BOX_LIGHT_T_LEFT);

    printf("%s Consommation de batterie: %.1f%% par heure%*s %s\n",
           BOX_LIGHT_VERTICAL,
           current_metrics.battery_consumption_per_hour,
           32,
           "",
           BOX_LIGHT_VERTICAL);

    printf("%s", BOX_LIGHT_BOTTOM_LEFT);
    ihm_print_line(BOX_LIGHT_HORIZONTAL, 81);
    printf("%s\n", BOX_LIGHT_BOTTOM_RIGHT);

    return IHM_OK;
}

int ihm_cmd_quitter(const char *args)
{
    (void)args;
    printf("Arrêt du système EXPLO ...\n");
    fflush(stdout);
    exit(0);
    return IHM_OK;
}
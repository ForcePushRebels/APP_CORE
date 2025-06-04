/**
 * @file ihm.h
 * @brief IHM
 * @author dbenech
 * @date 02/06/2025
 * @see ihm.c
 * @copyright Cecill-C (Cf. LICENCE.txt)
 */

#ifndef _IHM_H_
#define _IHM_H_

/* ******************************************************* Includes ****************************************************** */
#include <stdint.h>

/* ***************************************************** Public macros *************************************************** */

// clang-format off
#define IHM_BASE                     0x56E09A00
#define IHM_OK                       (IHM_BASE + 0)
#define IHM_ERROR_INIT               (IHM_BASE + 1)
#define IHM_ERROR_NO_MAP_AVAILABLE   (IHM_BASE + 2)
#define IHM_ERROR_UPDATE_VISION      (IHM_BASE + 3)
#define IHM_ERROR_UNKNOWN            (IHM_BASE + 4)

#define IHM_MAX_CMD_LENGTH           64
#define IHM_MAX_CMD_NAME_LENGTH      16
#define IHM_PROMPT                   "EXPLO > "

// Unicode box drawing characters
#define BOX_HORIZONTAL               "═"
#define BOX_VERTICAL                 "║"
#define BOX_TOP_LEFT                 "╔"
#define BOX_TOP_RIGHT                "╗"
#define BOX_BOTTOM_LEFT              "╚"
#define BOX_BOTTOM_RIGHT             "╝"
#define BOX_LIGHT_HORIZONTAL         "─"
#define BOX_LIGHT_VERTICAL           "│"
#define BOX_LIGHT_TOP_LEFT           "┌"
#define BOX_LIGHT_TOP_RIGHT          "┐"
#define BOX_LIGHT_BOTTOM_LEFT        "└"
#define BOX_LIGHT_BOTTOM_RIGHT       "┘"
#define BOX_LIGHT_CROSS              "┼"
#define BOX_LIGHT_T_DOWN             "┬"
#define BOX_LIGHT_T_UP               "┴"
#define BOX_LIGHT_T_RIGHT            "├"
#define BOX_LIGHT_T_LEFT             "┤"

// ANSI escape codes for cursor movement
#define ANSI_CURSOR_UP(n)            "\033[" #n "A"
#define ANSI_CURSOR_DOWN(n)          "\033[" #n "B"
#define ANSI_CURSOR_RIGHT(n)         "\033[" #n "C"
#define ANSI_CURSOR_LEFT(n)          "\033[" #n "D"
#define ANSI_CURSOR_SAVE             "\033[s"
#define ANSI_CURSOR_RESTORE          "\033[u"
#define ANSI_CURSOR_MOVE(line, col)  "\033[" #line ";" #col "H"

// Frame types
#define FRAME_TYPE_SINGLE            0
#define FRAME_TYPE_DOUBLE            1

// clang-format on

/* ************************************************** Public types definition ******************************************** */

/**
 * @brief Command callback function type
 * @param args Command arguments (after command name)
 * @return Command execution result
 */
typedef int (*ihm_cmd_callback_t)(const char *args);

/**
 * @brief Command structure
 */
typedef struct
{
    char name[IHM_MAX_CMD_NAME_LENGTH]; // Command name
    const char *description;            // Command description
    ihm_cmd_callback_t callback;        // Command callback function
} ihm_command_t;

/**
 * @brief Robot status structure for display
 */
typedef struct
{
    char status[32];       // Robot status (e.g., "PRÊT", "EN COURS", "ERREUR")
    uint8_t battery_level; // Battery level in percentage
    char mode[16];         // Operating mode (e.g., "AUTO", "MANUEL")
    float position_x;      // X position
    float position_y;      // Y position
    float orientation;     // Orientation in degrees
    char version[16];      // Software version
} ihm_robot_status_t;

/**
 * @brief Metrics structure for display
 */
typedef struct
{
    uint32_t exploration_time_sec;      // Total exploration time in seconds
    float distance_traveled_m;          // Distance traveled in meters
    uint8_t mapped_percentage;          // Percentage of mapped area
    float exploration_speed_m2_min;     // Exploration speed in m²/minute
    float battery_consumption_per_hour; // Battery consumption per hour in %
} ihm_metrics_t;

/* *********************************************** Public functions declarations ***************************************** */

/**
 * @brief Initialize the IHM system
 * @return IHM_OK on success, error code otherwise
 */
int ihm_init();

/**
 * @brief Display the main banner
 */
void ihm_display_banner(void);

/**
 * @brief Display the status bar
 * @param status Robot status information
 */
void ihm_display_status_bar(const ihm_robot_status_t *status);

/**
 * @brief Display a bordered box with content
 * @param title Box title (can be NULL)
 * @param content Box content
 * @param width Box width
 */
void ihm_display_box(const char *title, const char *content, int width);

/**
 * @brief Set robot status for display
 * @param status New robot status
 */
void ihm_set_robot_status(const ihm_robot_status_t *status);

/**
 * @brief Set metrics for display
 * @param metrics New metrics
 */
void ihm_set_metrics(const ihm_metrics_t *metrics);

/**
 * @brief Draw a frame around previously printed text using ANSI cursor movements
 * @param lines_count Number of text lines inside the frame
 * @param width Frame width (including borders)
 * @param frame_type Frame type (FRAME_TYPE_SINGLE or FRAME_TYPE_DOUBLE)
 * @param title Optional title for the frame (can be NULL)
 */
void ihm_draw_frame(int lines_count, int width, int frame_type, const char *title);

/* ******************************************* Public callback functions declarations ************************************ */

/**
 * @brief Help command callback
 * @param args Command arguments
 * @return Command execution result
 */
int ihm_cmd_aide(const char *args);

/**
 * @brief Info command callback
 * @param args Command arguments
 * @return Command execution result
 */
int ihm_cmd_info(const char *args);

/**
 * @brief Metrics command callback
 * @param args Command arguments
 * @return Command execution result
 */
int ihm_cmd_metriques(const char *args);

/**
 * @brief Quit command callback
 * @param args Command arguments
 * @return Command execution result
 */
int ihm_cmd_quitter(const char *args);

#endif /* _IHM_H_ */
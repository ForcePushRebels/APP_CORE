////////////////////////////////////////////////////////////
//  strategy manager header file
//  defines strategy manager types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 08/06/2025
////////////////////////////////////////////////////////////

#ifndef __STRATEGY_MANAGER_H__
#define __STRATEGY_MANAGER_H__

#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include "map_engine.h"
#include "xAssert.h"
#include "xLog.h"

/* ************************************************** Public macros *************************************************** */

// Return codes
#define STRATEGY_MANAGER_BASE               0x2000
#define STRATEGY_MANAGER_OK                 (STRATEGY_MANAGER_BASE + 0x00)
#define STRATEGY_MANAGER_ERR_INIT           (STRATEGY_MANAGER_BASE + 0x01)
#define STRATEGY_MANAGER_ERR_NOT_IMPL       (STRATEGY_MANAGER_BASE + 0x02)
#define STRATEGY_MANAGER_ERR_INVALID_PARAM  (STRATEGY_MANAGER_BASE + 0x03)
#define STRATEGY_MANAGER_ERR_NO_PATH        (STRATEGY_MANAGER_BASE + 0x04)
#define STRATEGY_MANAGER_ERR_OUT_OF_BOUNDS  (STRATEGY_MANAGER_BASE + 0x05)

/* ************************************************** Public types *************************************************** */

/**
 * @brief Point structure for pathfinding
 */
typedef struct point_s
{
    int16_t x_position;
    int16_t y_position;
} point_t;

/**
 * @brief Path sequence structure
 */
typedef struct sequence_s
{
    point_t *points;
    size_t length;
    size_t capacity;
} sequence_t;

/**
 * @brief Strategy manager status
 */
typedef enum strategy_status_e
{
    STRATEGY_STATUS_INIT,             // Initialisation
    STRATEGY_STATUS_READY,            // Prêt à démarrer
    STRATEGY_STATUS_MISSION_RUNNING,  // Mission en cours
    STRATEGY_STATUS_MANUAL_MODE,      // Contrôle manuel actif
    STRATEGY_STATUS_MISSION_COMPLETE, // Mission terminée normalement
    STRATEGY_STATUS_FAILURE,          // Échec ou interruption anormale
    STRATEGY_STATUS_NB,
} strategy_status_t;

/**
 * @brief Move completion reasons
 */
typedef enum move_reason_e
{
    MOVE_REASON_END_MOVE,       // Fin d'un déplacement élémentaire
    MOVE_REASON_END_ALL_MOVES,  // Fin de la séquence de déplacements
    MOVE_REASON_EMERGENCY_STOP, // Arrêt d'urgence
    MOVE_REASON_NB,
} move_reason_t;

/**
 * @brief Strategy manager structure
 */
typedef struct strategy_manager_s
{
    strategy_status_t status;           // Current status
    map_cell_t *current_map;           // Pointer to current map data
    size_t map_width;                  // Map width in cells
    size_t map_height;                 // Map height in cells
    size_t resolution_mm_per_cell;     // Map resolution
    sequence_t current_path;           // Current computed path
    struct timespec start_time;        // Start time for timing operations
    struct timespec end_time;          // End time for timing operations
    bool is_moving;                    // Movement state flag
    bool manual_mode_active;           // Manual mode flag
} strategy_manager_t;

/* *********************************************** Public functions declarations ***************************************** */

/**
 * @brief Initialize the strategy manager
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_init(void);

/**
 * @brief Shutdown the strategy manager
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_shutdown(void);

/**
 * @brief Update the internal map from the map engine
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_update_map(void);

/**
 * @brief Compute a path from start to goal
 * @param start_point Starting position
 * @param goal_point Goal position
 * @param path Output path sequence
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_compute_path(const point_t *start_point, const point_t *goal_point, sequence_t *path);

/**
 * @brief Start movement execution
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_start_movement(void);

/**
 * @brief Stop movement execution
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_stop_movement(void);

/**
 * @brief Check if wall is near based on current sensors
 * @return true if wall is detected nearby, false otherwise
 */
bool strategy_manager_is_wall_near(void);

/**
 * @brief Handle end condition reached notification
 */
void strategy_manager_end_condition_reached(void);

/**
 * @brief Get current strategy manager status
 * @return Current status code
 */
strategy_status_t strategy_manager_get_status(void);

/**
 * @brief Report pilot/movement status
 * @param pilot_status Status to report
 */
void strategy_manager_report_status(move_reason_t pilot_status);

/**
 * @brief Enable/disable manual mode interlock
 * @param enable true to enable manual mode, false to disable
 */
void strategy_manager_set_manual_mode(bool enable);

/**
 * @brief Start internal timer
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_start_timer(void);

/**
 * @brief Stop internal timer
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_stop_timer(void);

/**
 * @brief Get elapsed time in seconds
 * @return Elapsed time since timer start
 */
int32_t strategy_manager_get_elapsed_time(void);

/**
 * @brief Update strategy manager status
 * @param new_status New status to set
 */
void strategy_manager_update_status(strategy_status_t new_status);

/**
 * @brief Get strategy manager instance (for debugging/monitoring)
 * @param instance Pointer to fill with current instance data
 * @return STRATEGY_MANAGER_OK if successful, error code otherwise
 */
int strategy_manager_get_instance(strategy_manager_t *instance);

/**
 * @brief Free a sequence structure
 * @param seq Sequence to free
 */
void strategy_manager_free_sequence(sequence_t *seq);

#endif /* __STRATEGY_MANAGER_H__ */
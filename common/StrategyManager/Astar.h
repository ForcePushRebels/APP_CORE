////////////////////////////////////////////////////////////
//  Astar header file
//  defines Astar types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 08/06/2025
////////////////////////////////////////////////////////////

#ifndef __ASTAR_PATHFINDER_H__
#define __ASTAR_PATHFINDER_H__

#include <stdbool.h>
#include <stdint.h>
#include "map_engine.h"
#include "strategy_manager.h"
#include "xAssert.h"
#include "xLog.h"

/* ************************************************** Public macros *************************************************** */

#define ASTAR_BASE                    0x3000
#define ASTAR_OK                      (ASTAR_BASE + 0x00)
#define ASTAR_ERR_INVALID_PARAM       (ASTAR_BASE + 0x01)
#define ASTAR_ERR_NO_PATH             (ASTAR_BASE + 0x02)
#define ASTAR_ERR_OUT_OF_MEMORY       (ASTAR_BASE + 0x03)
#define ASTAR_ERR_OUT_OF_BOUNDS       (ASTAR_BASE + 0x04)
#define ASTAR_ERR_MAP_NOT_READY       (ASTAR_BASE + 0x05)

// Default maximum number of nodes to explore (safety limit)
#define ASTAR_MAX_NODES_DEFAULT       10000
#define ASTAR_MAX_PATH_LENGTH         1000

/* ************************************************** Public types *************************************************** */

/**
 * @brief Node cost structure for A* algorithm
 */
typedef struct astar_node_s
{
    int16_t x;                    // Grid x coordinate
    int16_t y;                    // Grid y coordinate
    float g_cost;                 // Cost from start to this node
    float h_cost;                 // Heuristic cost from this node to goal
    float f_cost;                 // Total cost (g_cost + h_cost)
    struct astar_node_s *parent;  // Parent node for path reconstruction
    bool in_open_set;             // True if node is in open set
    bool in_closed_set;           // True if node is in closed set
} astar_node_t;

/**
 * @brief A* pathfinder context
 */
typedef struct astar_context_s
{
    map_cell_t *map_data;         // Pointer to map data
    size_t map_width;             // Map width in cells
    size_t map_height;            // Map height in cells
    astar_node_t *node_pool;      // Pre-allocated node pool
    size_t node_pool_size;        // Size of node pool
    size_t nodes_used;            // Number of nodes currently used
    size_t max_nodes;             // Maximum nodes to explore (safety limit)
    bool initialized;             // Initialization flag
} astar_context_t;

/* *********************************************** Public functions declarations ***************************************** */

/**
 * @brief Initialize the A* pathfinder context
 * @param context Pathfinder context to initialize
 * @param map_data Pointer to map data
 * @param map_width Map width in cells
 * @param map_height Map height in cells
 * @param max_nodes Maximum nodes to explore (0 for default)
 * @return ASTAR_OK if successful, error code otherwise
 */
int astar_init(astar_context_t *context, map_cell_t *map_data, 
               size_t map_width, size_t map_height, size_t max_nodes);

/**
 * @brief Shutdown and cleanup the A* pathfinder context
 * @param context Pathfinder context to cleanup
 */
void astar_shutdown(astar_context_t *context);

/**
 * @brief Find path from start to goal using A* algorithm
 * @param context Pathfinder context
 * @param start Starting point
 * @param goal Goal point
 * @param path Output path sequence
 * @return ASTAR_OK if path found, error code otherwise
 */
int astar_find_path(astar_context_t *context, const point_t *start, 
                    const point_t *goal, sequence_t *path);

/**
 * @brief Check if a cell is traversable
 * @param context Pathfinder context
 * @param x X coordinate
 * @param y Y coordinate
 * @return true if cell is traversable, false otherwise
 */
bool astar_is_traversable(const astar_context_t *context, int16_t x, int16_t y);

/**
 * @brief Calculate Manhattan distance heuristic
 * @param from Starting point
 * @param to Ending point
 * @return Manhattan distance
 */
float astar_manhattan_distance(const point_t *from, const point_t *to);

/**
 * @brief Calculate Euclidean distance heuristic
 * @param from Starting point
 * @param to Ending point
 * @return Euclidean distance
 */
float astar_euclidean_distance(const point_t *from, const point_t *to);

/**
 * @brief Reset pathfinder context for new search
 * @param context Pathfinder context to reset
 */
void astar_reset(astar_context_t *context);

/**
 * @brief Get pathfinder statistics (for debugging/monitoring)
 * @param context Pathfinder context
 * @param nodes_explored Output: number of nodes explored
 * @param nodes_in_path Output: number of nodes in final path
 */
void astar_get_stats(const astar_context_t *context, size_t *nodes_explored, size_t *nodes_in_path);

#endif /* __ASTAR_PATHFINDER_H__ */ 
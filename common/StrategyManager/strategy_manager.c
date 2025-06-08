////////////////////////////////////////////////////////////
//  strategy manager header file
//  defines strategy manager types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 08/06/2025
////////////////////////////////////////////////////////////
#include "strategy_manager.h"
#include "Astar.h"
#include <stdlib.h>
#include <string.h>

/* ************************************************** Private macros *************************************************** */

#define STRATEGY_MANAGER_DEFAULT_MAP_SIZE   4096  // Default map buffer size

/* ************************************************** Private types *************************************************** */

/**
 * @brief Internal strategy manager context
 */
typedef struct strategy_manager_ctx_s
{
    strategy_manager_t public_data;     // Public interface data
    astar_context_t pathfinder;         // A* pathfinder context
    map_cell_t *map_buffer;            // Internal map buffer
    bool map_buffer_allocated;         // Flag for buffer allocation
} strategy_manager_ctx_t;

/* ********************************************** Private variables ************************************************** */

static strategy_manager_ctx_t g_strategy_manager_ctx = {0};

/* ********************************************* Private functions declarations **************************************** */

static void print_path_visualization(const sequence_t *path);
static int allocate_map_buffer(strategy_manager_ctx_t *ctx, size_t map_size);
static void free_map_buffer(strategy_manager_ctx_t *ctx);

/* ********************************************** Private functions definitions ***************************************** */

/**
 * @brief Print visual representation of computed path
 */
static void print_path_visualization(const sequence_t *path)
{
    if (path == NULL || path->points == NULL || path->length == 0)
    {
        X_LOG_DEBUG("No path to visualize");
        return;
    }

    strategy_manager_ctx_t *ctx = &g_strategy_manager_ctx;
    
    if (ctx->public_data.map_width == 0 || ctx->public_data.map_height == 0)
    {
        X_LOG_DEBUG("Map size not available for visualization");
        return;
    }

    // Create visualization grid
    size_t grid_size = ctx->public_data.map_width * ctx->public_data.map_height;
    char *grid = (char *)calloc(grid_size, sizeof(char));
    if (grid == NULL)
    {
        X_LOG_ERROR("Failed to allocate visualization grid");
        return;
    }

    // Mark obstacles and empty spaces
    for (size_t y = 0; y < ctx->public_data.map_height; y++)
    {
        for (size_t x = 0; x < ctx->public_data.map_width; x++)
        {
            size_t index = y * ctx->public_data.map_width + x;
            map_cell_t cell = ctx->public_data.current_map[index];
            
            switch (cell.type)
            {
                case MAP_CELL_WALL:
                    grid[index] = '@';
                    break;
                case MAP_CELL_INTEREST_AREA:
                    grid[index] = 'I';
                    break;
                default:
                    grid[index] = '.';
                    break;
            }
        }
    }

    // Mark path points
    for (size_t i = 0; i < path->length; i++)
    {
        int16_t x = path->points[i].x_position;
        int16_t y = path->points[i].y_position;
        
        if (x >= 0 && x < (int16_t)ctx->public_data.map_width && 
            y >= 0 && y < (int16_t)ctx->public_data.map_height)
        {
            size_t index = (size_t)y * ctx->public_data.map_width + (size_t)x;
            grid[index] = (i == 0) ? 'S' : (i == path->length - 1) ? 'G' : '#';
        }
    }

    // Print visualization (limit to reasonable size)
    size_t max_display_size = 20;
    size_t display_width = (ctx->public_data.map_width > max_display_size) ? 
                          max_display_size : ctx->public_data.map_width;
    size_t display_height = (ctx->public_data.map_height > max_display_size) ? 
                           max_display_size : ctx->public_data.map_height;

    X_LOG_INFO("Path visualization (%zu points):", path->length);
    X_LOG_INFO("Legend: S=Start, G=Goal, #=Path, @=Wall, I=Interest, .=Empty");
    
    for (size_t y = 0; y < display_height; y++)
    {
        char line[max_display_size + 1];
        memset(line, 0, sizeof(line));
        for (size_t x = 0; x < display_width; x++)
        {
            size_t index = y * ctx->public_data.map_width + x;
            line[x] = grid[index];
        }
        X_LOG_INFO("%s", line);
    }

    free(grid);
}

/**
 * @brief Allocate internal map buffer
 */
static int allocate_map_buffer(strategy_manager_ctx_t *ctx, size_t map_size)
{
    X_ASSERT(ctx != NULL);

    if (ctx->map_buffer_allocated)
    {
        free_map_buffer(ctx);
    }

    ctx->map_buffer = (map_cell_t *)malloc(map_size);
    if (ctx->map_buffer == NULL)
    {
        X_LOG_ERROR("Failed to allocate map buffer (%zu bytes)", map_size);
        return STRATEGY_MANAGER_ERR_INIT;
    }

    ctx->map_buffer_allocated = true;
    X_LOG_DEBUG("Allocated map buffer: %zu bytes", map_size);
    
    return STRATEGY_MANAGER_OK;
}

/**
 * @brief Free internal map buffer
 */
static void free_map_buffer(strategy_manager_ctx_t *ctx)
{
    if (ctx != NULL && ctx->map_buffer_allocated && ctx->map_buffer != NULL)
    {
        free(ctx->map_buffer);
        ctx->map_buffer = NULL;
        ctx->map_buffer_allocated = false;
        X_LOG_DEBUG("Map buffer freed");
    }
}

/* ********************************************** Public functions definitions ******************************************* */

int strategy_manager_init(void)
{
    X_LOG_DEBUG("Initializing strategy manager");

    // Clear context
    memset(&g_strategy_manager_ctx, 0, sizeof(strategy_manager_ctx_t));

    // Initialize public data
    g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_INIT;
    g_strategy_manager_ctx.public_data.is_moving = false;
    g_strategy_manager_ctx.public_data.manual_mode_active = false;

    // Get map information from map engine
    size_t map_size = map_engine_get_map_size(
        &g_strategy_manager_ctx.public_data.map_width,
        &g_strategy_manager_ctx.public_data.map_height,
        &g_strategy_manager_ctx.public_data.resolution_mm_per_cell
    );

    if (map_size == 0)
    {
        X_LOG_ERROR("Failed to get map size from map engine");
        return STRATEGY_MANAGER_ERR_INIT;
    }

    X_LOG_INFO("Map dimensions: %zu x %zu cells (%zu mm/cell, %zu bytes total)",
               g_strategy_manager_ctx.public_data.map_width,
               g_strategy_manager_ctx.public_data.map_height,
               g_strategy_manager_ctx.public_data.resolution_mm_per_cell,
               map_size);

    // Allocate map buffer
    int result = allocate_map_buffer(&g_strategy_manager_ctx, map_size);
    if (result != STRATEGY_MANAGER_OK)
    {
        return result;
    }

    g_strategy_manager_ctx.public_data.current_map = g_strategy_manager_ctx.map_buffer;

    // Update map from map engine
    result = strategy_manager_update_map();
    if (result != STRATEGY_MANAGER_OK)
    {
        X_LOG_WARN("Initial map update failed, continuing with empty map");
    }

    // Initialize A* pathfinder
    result = astar_init(&g_strategy_manager_ctx.pathfinder,
                       g_strategy_manager_ctx.public_data.current_map,
                       g_strategy_manager_ctx.public_data.map_width,
                       g_strategy_manager_ctx.public_data.map_height,
                       0);  // Use default max nodes

    if (result != ASTAR_OK)
    {
        X_LOG_ERROR("Failed to initialize A* pathfinder: 0x%x", result);
        free_map_buffer(&g_strategy_manager_ctx);
        return STRATEGY_MANAGER_ERR_INIT;
    }

    g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_READY;
    X_LOG_INFO("Strategy manager initialized successfully");

    return STRATEGY_MANAGER_OK;
}

int strategy_manager_shutdown(void)
{
    X_LOG_DEBUG("Shutting down strategy manager");

    // Shutdown A* pathfinder
    astar_shutdown(&g_strategy_manager_ctx.pathfinder);

    // Free current path if allocated
    strategy_manager_free_sequence(&g_strategy_manager_ctx.public_data.current_path);

    // Free map buffer
    free_map_buffer(&g_strategy_manager_ctx);

    // Clear context
    memset(&g_strategy_manager_ctx, 0, sizeof(strategy_manager_ctx_t));

    X_LOG_INFO("Strategy manager shutdown complete");
    return STRATEGY_MANAGER_OK;
}

int strategy_manager_update_map(void)
{
    X_ASSERT(g_strategy_manager_ctx.public_data.current_map != NULL);

    X_LOG_TRACE("Updating map from map engine");

    int result = map_engine_get_map(g_strategy_manager_ctx.public_data.current_map);
    if (result != MAP_ENGINE_OK)
    {
        X_LOG_ERROR("Failed to get map from map engine: 0x%x", result);
        return STRATEGY_MANAGER_ERR_INIT;
    }

    X_LOG_DEBUG("Map updated successfully");
    return STRATEGY_MANAGER_OK;
}

int strategy_manager_compute_path(const point_t *start_point, const point_t *goal_point, sequence_t *path)
{
    X_ASSERT(start_point != NULL);
    X_ASSERT(goal_point != NULL);
    X_ASSERT(path != NULL);

    if (g_strategy_manager_ctx.public_data.status == STRATEGY_STATUS_INIT)
    {
        X_LOG_ERROR("Strategy manager not ready");
        return STRATEGY_MANAGER_ERR_NOT_IMPL;
    }

    X_LOG_DEBUG("Computing path from (%d,%d) to (%d,%d)",
                start_point->x_position, start_point->y_position,
                goal_point->x_position, goal_point->y_position);

    // Update map before pathfinding
    int result = strategy_manager_update_map();
    if (result != STRATEGY_MANAGER_OK)
    {
        X_LOG_WARN("Map update failed, using current map");
    }

    // Clear any existing path data
    memset(path, 0, sizeof(sequence_t));

    // Find path using A*
    result = astar_find_path(&g_strategy_manager_ctx.pathfinder, start_point, goal_point, path);
    
    if (result == ASTAR_OK)
    {
        X_LOG_INFO("Path computed successfully: %zu points", path->length);
        print_path_visualization(path);
        
        // Store current path in manager
        strategy_manager_free_sequence(&g_strategy_manager_ctx.public_data.current_path);
        g_strategy_manager_ctx.public_data.current_path = *path;
        
        return STRATEGY_MANAGER_OK;
    }
    else
    {
        X_LOG_WARN("Path computation failed: 0x%x", result);
        
        switch (result)
        {
            case ASTAR_ERR_NO_PATH:
                return STRATEGY_MANAGER_ERR_NO_PATH;
            case ASTAR_ERR_OUT_OF_BOUNDS:
                return STRATEGY_MANAGER_ERR_OUT_OF_BOUNDS;
            case ASTAR_ERR_INVALID_PARAM:
                return STRATEGY_MANAGER_ERR_INVALID_PARAM;
            default:
                return STRATEGY_MANAGER_ERR_NOT_IMPL;
        }
    }
}

int strategy_manager_start_movement(void)
{
    X_LOG_TRACE("Starting movement execution");

    if (g_strategy_manager_ctx.public_data.status != STRATEGY_STATUS_READY)
    {
        X_LOG_ERROR("Strategy manager not in ready state");
        return STRATEGY_MANAGER_ERR_NOT_IMPL;
    }

    if (g_strategy_manager_ctx.public_data.current_path.length == 0)
    {
        X_LOG_ERROR("No path available for movement");
        return STRATEGY_MANAGER_ERR_NO_PATH;
    }

    g_strategy_manager_ctx.public_data.is_moving = true;
    g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_MISSION_RUNNING;

    // Start internal timer
    strategy_manager_start_timer();

    X_LOG_INFO("Movement started with %zu waypoints", 
               g_strategy_manager_ctx.public_data.current_path.length);

    return STRATEGY_MANAGER_OK;
}

int strategy_manager_stop_movement(void)
{
    X_LOG_TRACE("Stopping movement execution");

    g_strategy_manager_ctx.public_data.is_moving = false;

    if (g_strategy_manager_ctx.public_data.status == STRATEGY_STATUS_MISSION_RUNNING)
    {
        g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_READY;
    }

    // Stop internal timer
    strategy_manager_stop_timer();

    X_LOG_INFO("Movement stopped");
    return STRATEGY_MANAGER_OK;
}

bool strategy_manager_is_wall_near(void)
{
    // TODO: Implement wall detection based on current sensors
    // This would typically interface with the SensorManager module
    
    X_LOG_TRACE("Wall detection not implemented");
    return false;
}

void strategy_manager_end_condition_reached(void)
{
    X_LOG_INFO("End condition reached");

    g_strategy_manager_ctx.public_data.is_moving = false;
    g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_MISSION_COMPLETE;

    strategy_manager_stop_timer();
}

strategy_status_t strategy_manager_get_status(void)
{
    return g_strategy_manager_ctx.public_data.status;
}

void strategy_manager_report_status(move_reason_t pilot_status)
{
    X_ASSERT(pilot_status < MOVE_REASON_NB);

    X_LOG_DEBUG("Pilot status reported: %d", pilot_status);

    switch (pilot_status)
    {
        case MOVE_REASON_END_MOVE:
            X_LOG_DEBUG("Single move completed");
            break;
            
        case MOVE_REASON_END_ALL_MOVES:
            X_LOG_INFO("All moves completed");
            strategy_manager_end_condition_reached();
            break;
            
        case MOVE_REASON_EMERGENCY_STOP:
            X_LOG_WARN("Emergency stop requested");
            g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_FAILURE;
            strategy_manager_stop_movement();
            break;
            
        default:
            X_LOG_WARN("Unknown pilot status: %d", pilot_status);
            break;
    }
}

void strategy_manager_set_manual_mode(bool enable)
{
    X_LOG_DEBUG("Manual mode %s", enable ? "enabled" : "disabled");

    g_strategy_manager_ctx.public_data.manual_mode_active = enable;

    if (enable)
    {
        if (g_strategy_manager_ctx.public_data.is_moving)
        {
            strategy_manager_stop_movement();
        }
        g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_MANUAL_MODE;
    }
    else
    {
        if (g_strategy_manager_ctx.public_data.status == STRATEGY_STATUS_MANUAL_MODE)
        {
            g_strategy_manager_ctx.public_data.status = STRATEGY_STATUS_READY;
        }
    }
}

int strategy_manager_start_timer(void)
{
    X_LOG_TRACE("Starting internal timer");

    int result = clock_gettime(CLOCK_MONOTONIC, &g_strategy_manager_ctx.public_data.start_time);
    if (result != 0)
    {
        X_LOG_ERROR("Failed to start timer");
        return STRATEGY_MANAGER_ERR_NOT_IMPL;
    }

    return STRATEGY_MANAGER_OK;
}

int strategy_manager_stop_timer(void)
{
    X_LOG_TRACE("Stopping internal timer");

    int result = clock_gettime(CLOCK_MONOTONIC, &g_strategy_manager_ctx.public_data.end_time);
    if (result != 0)
    {
        X_LOG_ERROR("Failed to stop timer");
        return STRATEGY_MANAGER_ERR_NOT_IMPL;
    }

    return STRATEGY_MANAGER_OK;
}

int32_t strategy_manager_get_elapsed_time(void)
{
    return (int32_t)(g_strategy_manager_ctx.public_data.end_time.tv_sec - 
                     g_strategy_manager_ctx.public_data.start_time.tv_sec);
}

void strategy_manager_update_status(strategy_status_t new_status)
{
    X_ASSERT(new_status < STRATEGY_STATUS_NB);

    strategy_status_t old_status = g_strategy_manager_ctx.public_data.status;
    g_strategy_manager_ctx.public_data.status = new_status;

    X_LOG_DEBUG("Status updated: %d -> %d", old_status, new_status);
}

int strategy_manager_get_instance(strategy_manager_t *instance)
{
    X_ASSERT(instance != NULL);

    *instance = g_strategy_manager_ctx.public_data;
    return STRATEGY_MANAGER_OK;
}

void strategy_manager_free_sequence(sequence_t *seq)
{
    if (seq != NULL && seq->points != NULL)
    {
        free(seq->points);
        seq->points = NULL;
        seq->length = 0;
        seq->capacity = 0;
    }
}

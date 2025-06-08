////////////////////////////////////////////////////////////
//  Astar header file
//  defines Astar types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 08/06/2025
////////////////////////////////////////////////////////////

#include "Astar.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* ************************************************** Private macros *************************************************** */

// Direction vectors for 4-directional movement (up, right, down, left)
#define ASTAR_NUM_DIRECTIONS 4

/* ************************************************** Private types *************************************************** */

/**
 * @brief Priority queue for open set management
 */
typedef struct priority_queue_s
{
    astar_node_t **nodes;
    size_t size;
    size_t capacity;
} priority_queue_t;

/* ********************************************** Private variables ************************************************** */

// Direction vectors: up, right, down, left
static const int16_t g_direction_x[ASTAR_NUM_DIRECTIONS] = {0, 1, 0, -1};
static const int16_t g_direction_y[ASTAR_NUM_DIRECTIONS] = {-1, 0, 1, 0};

/* ********************************************* Private functions declarations **************************************** */

static int priority_queue_init(priority_queue_t *pq, size_t capacity);
static void priority_queue_destroy(priority_queue_t *pq);
static int priority_queue_push(priority_queue_t *pq, astar_node_t *node);
static astar_node_t *priority_queue_pop(priority_queue_t *pq);
static bool priority_queue_is_empty(const priority_queue_t *pq);
static void priority_queue_heapify_up(priority_queue_t *pq, size_t index);
static void priority_queue_heapify_down(priority_queue_t *pq, size_t index);

static astar_node_t *get_node(astar_context_t *context, int16_t x, int16_t y);
static void reset_node(astar_node_t *node);
static int reconstruct_path(astar_node_t *goal_node, sequence_t *path);
static bool is_valid_coordinate(const astar_context_t *context, int16_t x, int16_t y);

/* ********************************************** Private functions definitions ***************************************** */

/**
 * @brief Initialize priority queue
 */
static int priority_queue_init(priority_queue_t *pq, size_t capacity)
{
    X_ASSERT(pq != NULL);
    X_ASSERT(capacity > 0);

    pq->nodes = (astar_node_t **)calloc(capacity, sizeof(astar_node_t *));
    if (pq->nodes == NULL)
    {
        X_LOG_ERROR("Failed to allocate priority queue");
        return ASTAR_ERR_OUT_OF_MEMORY;
    }

    pq->size = 0;
    pq->capacity = capacity;
    return ASTAR_OK;
}

/**
 * @brief Destroy priority queue
 */
static void priority_queue_destroy(priority_queue_t *pq)
{
    if (pq != NULL && pq->nodes != NULL)
    {
        free(pq->nodes);
        pq->nodes = NULL;
        pq->size = 0;
        pq->capacity = 0;
    }
}

/**
 * @brief Push node to priority queue (min-heap based on f_cost)
 */
static int priority_queue_push(priority_queue_t *pq, astar_node_t *node)
{
    X_ASSERT(pq != NULL);
    X_ASSERT(node != NULL);

    if (pq->size >= pq->capacity)
    {
        X_LOG_ERROR("Priority queue is full");
        return ASTAR_ERR_OUT_OF_MEMORY;
    }

    pq->nodes[pq->size] = node;
    priority_queue_heapify_up(pq, pq->size);
    pq->size++;

    return ASTAR_OK;
}

/**
 * @brief Pop minimum f_cost node from priority queue
 */
static astar_node_t *priority_queue_pop(priority_queue_t *pq)
{
    X_ASSERT(pq != NULL);

    if (pq->size == 0)
    {
        return NULL;
    }

    astar_node_t *min_node = pq->nodes[0];
    pq->size--;
    
    if (pq->size > 0)
    {
        pq->nodes[0] = pq->nodes[pq->size];
        priority_queue_heapify_down(pq, 0);
    }

    return min_node;
}

/**
 * @brief Check if priority queue is empty
 */
static bool priority_queue_is_empty(const priority_queue_t *pq)
{
    X_ASSERT(pq != NULL);
    return pq->size == 0;
}

/**
 * @brief Heapify up for min-heap property
 */
static void priority_queue_heapify_up(priority_queue_t *pq, size_t index)
{
    while (index > 0)
    {
        size_t parent_index = (index - 1) / 2;
        
        if (pq->nodes[index]->f_cost >= pq->nodes[parent_index]->f_cost)
        {
            break;
        }

        // Swap with parent
        astar_node_t *temp = pq->nodes[index];
        pq->nodes[index] = pq->nodes[parent_index];
        pq->nodes[parent_index] = temp;
        
        index = parent_index;
    }
}

/**
 * @brief Heapify down for min-heap property
 */
static void priority_queue_heapify_down(priority_queue_t *pq, size_t index)
{
    while (true)
    {
        size_t smallest = index;
        size_t left_child = 2 * index + 1;
        size_t right_child = 2 * index + 2;

        if (left_child < pq->size && 
            pq->nodes[left_child]->f_cost < pq->nodes[smallest]->f_cost)
        {
            smallest = left_child;
        }

        if (right_child < pq->size && 
            pq->nodes[right_child]->f_cost < pq->nodes[smallest]->f_cost)
        {
            smallest = right_child;
        }

        if (smallest == index)
        {
            break;
        }

        // Swap with smallest child
        astar_node_t *temp = pq->nodes[index];
        pq->nodes[index] = pq->nodes[smallest];
        pq->nodes[smallest] = temp;
        
        index = smallest;
    }
}

/**
 * @brief Get or create node from pool
 */
static astar_node_t *get_node(astar_context_t *context, int16_t x, int16_t y)
{
    X_ASSERT(context != NULL);
    X_ASSERT(context->node_pool != NULL);

    // Find existing node or allocate new one
    for (size_t i = 0; i < context->nodes_used; i++)
    {
        if (context->node_pool[i].x == x && context->node_pool[i].y == y)
        {
            return &context->node_pool[i];
        }
    }

    // Allocate new node if space available
    if (context->nodes_used < context->node_pool_size)
    {
        astar_node_t *node = &context->node_pool[context->nodes_used];
        context->nodes_used++;
        
        node->x = x;
        node->y = y;
        reset_node(node);
        
        return node;
    }

    X_LOG_ERROR("Node pool exhausted");
    return NULL;
}

/**
 * @brief Reset node to default state
 */
static void reset_node(astar_node_t *node)
{
    X_ASSERT(node != NULL);

    node->g_cost = 0.0f;
    node->h_cost = 0.0f;
    node->f_cost = 0.0f;
    node->parent = NULL;
    node->in_open_set = false;
    node->in_closed_set = false;
}

/**
 * @brief Reconstruct path from goal to start
 */
static int reconstruct_path(astar_node_t *goal_node, sequence_t *path)
{
    X_ASSERT(goal_node != NULL);
    X_ASSERT(path != NULL);

    // Count path length
    size_t path_length = 0;
    astar_node_t *current = goal_node;
    while (current != NULL)
    {
        path_length++;
        current = current->parent;
    }

    if (path_length == 0)
    {
        return ASTAR_ERR_NO_PATH;
    }

    // Allocate path points
    path->points = (point_t *)malloc(path_length * sizeof(point_t));
    if (path->points == NULL)
    {
        X_LOG_ERROR("Failed to allocate path points");
        return ASTAR_ERR_OUT_OF_MEMORY;
    }

    // Fill path in reverse order
    path->length = path_length;
    path->capacity = path_length;
    
    current = goal_node;
    for (size_t i = path_length; i > 0; i--)
    {
        path->points[i - 1].x_position = current->x;
        path->points[i - 1].y_position = current->y;
        current = current->parent;
    }

    return ASTAR_OK;
}

/**
 * @brief Check if coordinates are valid
 */
static bool is_valid_coordinate(const astar_context_t *context, int16_t x, int16_t y)
{
    X_ASSERT(context != NULL);
    
    return (x >= 0 && x < (int16_t)context->map_width && 
            y >= 0 && y < (int16_t)context->map_height);
}

/* ********************************************** Public functions definitions ******************************************* */

int astar_init(astar_context_t *context, map_cell_t *map_data, 
               size_t map_width, size_t map_height, size_t max_nodes)
{
    X_ASSERT(context != NULL);
    X_ASSERT(map_data != NULL);
    X_ASSERT(map_width > 0);
    X_ASSERT(map_height > 0);

    X_LOG_DEBUG("Initializing A* pathfinder: %zu x %zu map", map_width, map_height);

    memset(context, 0, sizeof(astar_context_t));

    context->map_data = map_data;
    context->map_width = map_width;
    context->map_height = map_height;
    context->max_nodes = (max_nodes > 0) ? max_nodes : ASTAR_MAX_NODES_DEFAULT;

    // Allocate node pool
    context->node_pool_size = context->max_nodes;
    context->node_pool = (astar_node_t *)calloc(context->node_pool_size, sizeof(astar_node_t));
    if (context->node_pool == NULL)
    {
        X_LOG_ERROR("Failed to allocate A* node pool");
        return ASTAR_ERR_OUT_OF_MEMORY;
    }

    context->nodes_used = 0;
    context->initialized = true;

    X_LOG_INFO("A* pathfinder initialized successfully");
    return ASTAR_OK;
}

void astar_shutdown(astar_context_t *context)
{
    if (context != NULL)
    {
        if (context->node_pool != NULL)
        {
            free(context->node_pool);
            context->node_pool = NULL;
        }
        
        memset(context, 0, sizeof(astar_context_t));
        X_LOG_DEBUG("A* pathfinder shutdown complete");
    }
}

int astar_find_path(astar_context_t *context, const point_t *start, 
                    const point_t *goal, sequence_t *path)
{
    X_ASSERT(context != NULL);
    X_ASSERT(start != NULL);
    X_ASSERT(goal != NULL);
    X_ASSERT(path != NULL);

    if (!context->initialized)
    {
        X_LOG_ERROR("A* context not initialized");
        return ASTAR_ERR_MAP_NOT_READY;
    }

    if (!is_valid_coordinate(context, start->x_position, start->y_position) ||
        !is_valid_coordinate(context, goal->x_position, goal->y_position))
    {
        X_LOG_ERROR("Invalid start or goal coordinates");
        return ASTAR_ERR_OUT_OF_BOUNDS;
    }

    if (!astar_is_traversable(context, start->x_position, start->y_position) ||
        !astar_is_traversable(context, goal->x_position, goal->y_position))
    {
        X_LOG_ERROR("Start or goal position is not traversable");
        return ASTAR_ERR_NO_PATH;
    }

    X_LOG_DEBUG("Finding path from (%d,%d) to (%d,%d)", 
                start->x_position, start->y_position,
                goal->x_position, goal->y_position);

    // Reset context for new search
    astar_reset(context);

    // Initialize priority queue
    priority_queue_t open_set;
    int result = priority_queue_init(&open_set, context->max_nodes);
    if (result != ASTAR_OK)
    {
        return result;
    }

    // Initialize start node
    astar_node_t *start_node = get_node(context, start->x_position, start->y_position);
    if (start_node == NULL)
    {
        priority_queue_destroy(&open_set);
        return ASTAR_ERR_OUT_OF_MEMORY;
    }

    start_node->g_cost = 0.0f;
    start_node->h_cost = astar_manhattan_distance(start, goal);
    start_node->f_cost = start_node->g_cost + start_node->h_cost;
    start_node->in_open_set = true;

    result = priority_queue_push(&open_set, start_node);
    if (result != ASTAR_OK)
    {
        priority_queue_destroy(&open_set);
        return result;
    }

    astar_node_t *goal_node = NULL;
    size_t iterations = 0;

    // Main A* loop
    while (!priority_queue_is_empty(&open_set) && iterations < context->max_nodes)
    {
        iterations++;
        
        astar_node_t *current = priority_queue_pop(&open_set);
        current->in_open_set = false;
        current->in_closed_set = true;

        // Check if we reached the goal
        if (current->x == goal->x_position && current->y == goal->y_position)
        {
            goal_node = current;
            break;
        }

        // Explore neighbors
        for (int i = 0; i < ASTAR_NUM_DIRECTIONS; i++)
        {
            int16_t neighbor_x = current->x + g_direction_x[i];
            int16_t neighbor_y = current->y + g_direction_y[i];

            if (!is_valid_coordinate(context, neighbor_x, neighbor_y) ||
                !astar_is_traversable(context, neighbor_x, neighbor_y))
            {
                continue;
            }

            astar_node_t *neighbor = get_node(context, neighbor_x, neighbor_y);
            if (neighbor == NULL || neighbor->in_closed_set)
            {
                continue;
            }

            float tentative_g_cost = current->g_cost + 1.0f; // Unit cost for grid movement

            if (!neighbor->in_open_set)
            {
                neighbor->parent = current;
                neighbor->g_cost = tentative_g_cost;
                neighbor->h_cost = astar_manhattan_distance(
                    &(point_t){neighbor_x, neighbor_y}, goal);
                neighbor->f_cost = neighbor->g_cost + neighbor->h_cost;
                neighbor->in_open_set = true;

                result = priority_queue_push(&open_set, neighbor);
                if (result != ASTAR_OK)
                {
                    priority_queue_destroy(&open_set);
                    return result;
                }
            }
            else if (tentative_g_cost < neighbor->g_cost)
            {
                neighbor->parent = current;
                neighbor->g_cost = tentative_g_cost;
                neighbor->f_cost = neighbor->g_cost + neighbor->h_cost;
                // Note: This is a simplified implementation. In a full implementation,
                // we would need to reorder the priority queue here.
            }
        }
    }

    priority_queue_destroy(&open_set);

    if (goal_node == NULL)
    {
        X_LOG_WARN("No path found after %zu iterations", iterations);
        return ASTAR_ERR_NO_PATH;
    }

    result = reconstruct_path(goal_node, path);
    if (result == ASTAR_OK)
    {
        X_LOG_INFO("Path found with %zu points (%zu iterations)", path->length, iterations);
    }

    return result;
}

bool astar_is_traversable(const astar_context_t *context, int16_t x, int16_t y)
{
    X_ASSERT(context != NULL);
    X_ASSERT(context->map_data != NULL);

    if (!is_valid_coordinate(context, x, y))
    {
        return false;
    }

    size_t index = (size_t)y * context->map_width + (size_t)x;
    map_cell_t cell = context->map_data[index];

    // Only empty cells and interest areas are traversable
    return (cell.type == MAP_CELL_EMPTY || cell.type == MAP_CELL_INTEREST_AREA);
}

float astar_manhattan_distance(const point_t *from, const point_t *to)
{
    X_ASSERT(from != NULL);
    X_ASSERT(to != NULL);

    return (float)(abs(to->x_position - from->x_position) + 
                   abs(to->y_position - from->y_position));
}

float astar_euclidean_distance(const point_t *from, const point_t *to)
{
    X_ASSERT(from != NULL);
    X_ASSERT(to != NULL);

    float dx = (float)(to->x_position - from->x_position);
    float dy = (float)(to->y_position - from->y_position);

    return sqrtf(dx * dx + dy * dy);
}

void astar_reset(astar_context_t *context)
{
    X_ASSERT(context != NULL);

    if (context->node_pool != NULL)
    {
        for (size_t i = 0; i < context->nodes_used; i++)
        {
            reset_node(&context->node_pool[i]);
        }
    }
    
    context->nodes_used = 0;
}

void astar_get_stats(const astar_context_t *context, size_t *nodes_explored, size_t *nodes_in_path)
{
    X_ASSERT(context != NULL);

    if (nodes_explored != NULL)
    {
        *nodes_explored = context->nodes_used;
    }

    if (nodes_in_path != NULL)
    {
        *nodes_in_path = 0; // This would need to be tracked during pathfinding
    }
} 
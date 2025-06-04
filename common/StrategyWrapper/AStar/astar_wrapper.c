#include "astar_wrapper.h"
#include "AStar.h"
#include "debug_utils.h"  // Use relative path version

// Define your node structure
typedef struct {
    int x;
    int y;
} GridNode;

// Example grid representation
#define GRID_SIZE 10
int grid[GRID_SIZE][GRID_SIZE];  // 0 for empty, 1 for obstacle

static float GridHeuristic(void* fromNode, void* toNode, void* context);
static const ASPathNodeSource PathNodeSource;

static AStarWrapper astar_wrapper;

int astar_wrapper__init()
{
    X_LOG_DEBUG("Initializing AStar wrapper");
    int ret = strategy_wrapper__addStrategy("AStar", astar_wrapper__prepare, astar_wrapper__execute);
    if (ret != 0) {
        X_LOG_ERROR("Failed to register AStar strategy");
    } else {
        X_LOG_INFO("AStar wrapper initialized successfully");
    }
    return ret;
}

//@Override
int astar_wrapper__prepare(mat_t (*mat)[10]) {
    X_LOG_DEBUG("Preparing AStar grid");

    // Clear grid first
    memset(grid, 0, sizeof(grid));

    int obstacleCount = 0;

    // Copy map to grid
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if(mat[i][j].type == MAP_CELL_WALL) {
                grid[i][j] = 1;  // Mark as obstacle
                obstacleCount++;
                X_LOG_DEBUG("Obstacle at (%d,%d)", i, j);
            }
        }   
    }

    X_LOG_INFO("AStar grid prepared with %d obstacles", obstacleCount);
    return 0;
}

//@Override
int astar_wrapper__execute(seq_t *seq, Point *initial, Point *final) {
    X_LOG_DEBUG("Executing AStar pathfinding from (%d,%d) to (%d,%d)", 
                initial->x, initial->y, final->x, final->y);

    debug_print_point("Start position", initial);
    debug_print_point("Goal position", final);

    GridNode start = {initial->x, initial->y};
    GridNode goal = {final->x, final->y};

    // Find the path
    ASPath path = ASPathCreate(&PathNodeSource, NULL, &start, &goal);

    // Check if path exists
    size_t pathCount = ASPathGetCount(path);
    if (pathCount > 0) {
        for (size_t i = 0; i < pathCount; i++) {
            GridNode* node = (GridNode*)ASPathGetNode(path, i);
            seq[i].x = node->x;
            seq[i].y = node->y;
        }
        X_LOG_INFO("Path found with %zu points", pathCount);
        debug_print_sequence("AStar path", seq, pathCount);
    } else {
        X_LOG_WARN("No path found between points");
        pathCount = 0;
    }

    // Clean up
    ASPathDestroy(path);
    return pathCount;  // Return the number of points in path
}

// Node neighbor function
static void GridNodeNeighbors(ASNeighborList neighbors, void* node, void* context) {
    GridNode* currentNode = (GridNode*)node;
    int dx[] = {0, 1, 0, -1};  // Right, Down, Left, Up
    int dy[] = {1, 0, -1, 0};
    
    for(int i = 0; i < 4; i++) {
        int newX = currentNode->x + dx[i];
        int newY = currentNode->y + dy[i];
        
        // Check bounds and obstacles
        if (newX >= 0 && newX < GRID_SIZE && 
            newY >= 0 && newY < GRID_SIZE && 
            grid[newX][newY] != 1) {
            
            GridNode neighbor = {newX, newY};
            ASNeighborListAdd(neighbors, &neighbor, 1.0f);  // Cost 1.0 for each step
        }
    }
}

// Manhattan distance heuristic
static float GridHeuristic(void* fromNode, void* toNode, void* context) {
    GridNode* from = (GridNode*)fromNode;
    GridNode* to = (GridNode*)toNode;
    return (float)(abs(to->x - from->x) + abs(to->y - from->y));
}

// Set up the path finding
static const ASPathNodeSource PathNodeSource = {
    sizeof(GridNode),
    &GridNodeNeighbors,
    &GridHeuristic,
    NULL,
    NULL
};
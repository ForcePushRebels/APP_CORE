#include "astar_wrapper.h"
#include "AStar/AStar.h"

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

AStarWrapper *astar_wrapper__create()
{	
	AStarWrapper *self = malloc(sizeof(AStarWrapper));

	self = strategy_wrapper__create("AStar");
	
	strategy_wrapper__bindPrepare(self, astar_wrapper__prepare);
	
	strategy_wrapper__bindExecute(self, astar_wrapper__execute);
	
	return self;
}

//@Override
void astar_wrapper__delete(AStarWrapper *self) {

	strategy_wrapper__delete(self);
	
}

//@Override
int astar_wrapper__prepare(mat_t *mat) {
	// Initialize the grid with a static maze (0 = empty, 1 = obstacle)
	for (int i = 0; i < GRID_SIZE; i++) {
		for (int j = 0; j < GRID_SIZE; j++) {
			if(mat[i][j].type == MAP_CELL_WALL) {
				grid[i][j] = 1;
			}
		}	
	}

	return 0;
}

//@Override
int astar_wrapper__execute(seq_t *seq, Point *initial, Point *final) {

	GridNode start = {initial->x, initial->y};
	GridNode goal = {final->x, final->y};

	// Find the path
	ASPath path = ASPathCreate(&PathNodeSource, NULL, &start, &goal);

	// Check if path exists
	size_t pathCount = ASPathGetCount(path);
	if (pathCount > 0) {
		for (size_t i = 0; i < pathCount; i++) {
			GridNode* node = (GridNode*)ASPathGetNode(path, i);
			seq[i][0] = node->x;
			seq[i][1] = node->y;
		}
	}

	// Clean up
	ASPathDestroy(path);

	return 0;
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
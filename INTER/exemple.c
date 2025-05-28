#include <stdio.h>

#include "../../StrategyManager/include/intervention_manager.h"

#include "strategy_wrapper.h"
#include "AStar/astar_wrapper.h"

#define GRID_SIZE 10

int maze[GRID_SIZE][GRID_SIZE] = {
		{0,1,1,1,1,1,1,1,1,0},
		{0,0,0,0,0,1,0,0,1,0},
		{1,0,1,1,0,1,0,1,1,0},
		{1,0,1,0,0,0,0,1,0,0},
		{1,0,1,0,1,1,1,1,0,1},
		{1,0,0,0,1,0,0,0,0,1},
		{1,1,1,0,1,0,1,1,0,1},
		{1,0,0,0,0,0,1,0,0,1},
		{1,0,1,1,1,0,1,1,0,0},
		{1,1,1,1,1,0,0,0,0,0}
	};

int main() { 

	AStarWrapper *aStarWrapper = astar_wrapper__create();

	InterventionManager * interventionManager = intervention_manager__create();

	intervention_manager__addStrategy(interventionManager, aStarWrapper);
		
	return 0;
}
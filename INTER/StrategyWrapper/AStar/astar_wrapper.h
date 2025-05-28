#ifndef __ASTAR_WRAPPER__
#define __ASTAR_WRAPPER__

#include "strategy_wrapper.h"

typedef struct astar_wrapper_s AStarWrapper;

AStarWrapper *astar_wrapper__create(void);

void AStar_Wrapper__delete();

void AStar__prepare(int (* map_ptr)[10]);

void AStar__execute(int (*map_ptr)[2]);

#endif /* __ASTAR_WRAPPER__ */
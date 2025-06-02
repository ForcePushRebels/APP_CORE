#ifndef __ASTAR_WRAPPER__
#define __ASTAR_WRAPPER__

#include "strategy_wrapper.h"

//@Alias
typedef StrategyWrapper AStarWrapper;

//@Override
int astar_wrapper__init(void);

//@Override
int astar_wrapper__prepare(mat_t (*)[10]);

//@Override
int astar_wrapper__execute(seq_t *, Point*, Point*);

#endif /* __ASTAR_WRAPPER__ */
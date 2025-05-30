#ifndef __ASTAR_WRAPPER__
#define __ASTAR_WRAPPER__

#include "strategy_wrapper.h"

//@Alias
typedef StrategyWrapper AStarWrapper;

//@Override
AStarWrapper *astar_wrapper__create(void);

//@Override
void astar_wrapper__delete(AStarWrapper *);

//@Override
int astar_wrapper__prepare(mat_t *);

//@Override
int astar_wrapper__execute(seq_t *, Point*, Point*);

#endif /* __ASTAR_WRAPPER__ */
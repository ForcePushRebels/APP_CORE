#ifndef __ASTAR_WRAPPER__
#define __ASTAR_WRAPPER__

#include "strategy_wrapper.h"
#include "xLog.h"

#define ASTAR_WRAPPER 0x3000
#define ASTAR_WRAPPER_OK (ASTAR_WRAPPER + 0x00) // Success code
#define ASTAR_WRAPPER_ERR_INIT (ASTAR_WRAPPER + 0x01) // Generic error
#define ASTAR_WRAPPER_ERR_NOT_IMPL (ASTAR_WRAPPER + 0x02) // Not implemented error


//@Alias
typedef StrategyWrapper AStarWrapper;

//@Override
int astar_wrapper__init(void);

//@Override
int astar_wrapper__prepare(mat_t (*)[10]);

//@Override
int astar_wrapper__execute(seq_t *, Point*, Point*);

#endif /* __ASTAR_WRAPPER__ */
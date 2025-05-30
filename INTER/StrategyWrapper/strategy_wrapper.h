#ifndef __STRATEGY_WRAPPER__
#define __STRATEGY_WRAPPER__

#include <map_engine.h>

typedef map_cell_t mat_t[10]; // matrix
typedef int seq_t[2]; //sequence

typedef struct Point {
	int x, y;
} Point;

typedef int (prep_func_cb)(mat_t *);
typedef int (exec_func_cb)(seq_t *, Point *, Point *);

typedef struct strategy_s {
	char *name;
	mat_t *map;
    prep_func_cb *prepare;
    exec_func_cb *execute;
} StrategyWrapper;

StrategyWrapper *strategy_wrapper__create(char *);

void strategy_wrapper__prepare(StrategyWrapper *, mat_t *);

void strategy_wrapper__execute(StrategyWrapper *, seq_t *, Point *, Point *);

void strategy_wrapper__bindMap(StrategyWrapper *, mat_t *);

void strategy_wrapper__bindPrepare(StrategyWrapper *, prep_func_cb *);

void strategy_wrapper__bindExecute(StrategyWrapper *, exec_func_cb *);

void strategy_wrapper__delete(StrategyWrapper *);

#endif /* __STRATEGY_WRAPPER__ */
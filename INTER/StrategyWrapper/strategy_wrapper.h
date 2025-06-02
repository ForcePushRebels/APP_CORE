#ifndef __STRATEGY_WRAPPER__
#define __STRATEGY_WRAPPER__

#include <map_engine.h>

typedef struct Point {
	int x, y;
} Point;

typedef map_cell_t mat_t; // matrix
typedef Point seq_t; //sequence

typedef int (prep_func_cb)(mat_t (*)[10]);
typedef int (exec_func_cb)(seq_t *, Point *, Point *);

typedef struct strategy_s {
	char *name;
	mat_t (*map)[10];
    prep_func_cb *prepare;
    exec_func_cb *execute;
} StrategyWrapper;

int strategy_wrapper__create(char *);

void strategy_wrapper__prepare(mat_t (*)[10]);

void strategy_wrapper__execute(seq_t *, Point *, Point *);

void strategy_wrapper__bindMap(mat_t (*)[10]);

void strategy_wrapper__bindPrepare(prep_func_cb *);

void strategy_wrapper__bindExecute(exec_func_cb *);

void strategy_wrapper__delete(StrategyWrapper *);

#endif /* __STRATEGY_WRAPPER__ */
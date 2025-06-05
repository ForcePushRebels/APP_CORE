#ifndef __STRATEGY_WRAPPER__
#define __STRATEGY_WRAPPER__

#include "xAssert.h"
#include "xLog.h"
#include <map_engine.h>

typedef struct point_s
{
    int xPosition;
    int yPosition;
} point_t;

typedef map_cell_t mat_t; // matrix
typedef point_t seq_t;    //sequence

typedef int(prep_func_cb)(mat_t (*)[10]);
typedef int(exec_func_cb)(seq_t *, point_t *, point_t *);

typedef struct strategy_s
{
    char *name;
    mat_t (*map)[10];
    prep_func_cb *prepare;
    exec_func_cb *execute;
} StrategyWrapper;

typedef enum strategy_e
{
    STRATEGY_ASTAR,
    STRATEGY_NB,
} Strategy;

int strategy_wrapper__addStrategy(char *, prep_func_cb *, exec_func_cb *);

void strategy_wrapper__prepare(mat_t (*)[10]);

void strategy_wrapper__execute(seq_t *, point_t *, point_t *);

int strategy_wrapper__giveIDStrategieToFollow(int id);

void strategy_wrapper__bindMap(mat_t (*)[10]);

void strategy_wrapper__bindPrepare(prep_func_cb *);

void strategy_wrapper__bindExecute(exec_func_cb *);

void strategy_wrapper__delete(StrategyWrapper *);

#endif /* __STRATEGY_WRAPPER__ */
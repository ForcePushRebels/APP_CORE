#ifndef __STRATEGY_WRAPPER__
#define __STRATEGY_WRAPPER__

typedef void (*PrepFuncSign)(int (*map_ptr)[10]);
typedef void (*ExecFuncSign)(int (*map_ptr)[2]);

typedef struct strategy_s {
	char * name;
    PrepFuncSign prepare;
    ExecFuncSign execute;
} StrategyWrapper;

StrategyWrapper *strategy_wrapper__create(char *);

void strategy_wrapper__prepare();

void strategy_wrapper__execute();

void strategy_wrapper__bindExecute(StrategyWrapper *self, ExecFuncSign ptr);

void strategy_wrapper__bindPrepare(StrategyWrapper *self, PrepFuncSign ptr);

void strategy_wrapper__delete(StrategyWrapper *self);

#endif /* __STRATEGY_WRAPPER__ */
////////////////////////////////////////////////////////////
//  supervisor header file
//  Provides supervisor interface
//
// general disclosure: copy or share the file is forbidden
// Written : 23/05/2025
////////////////////////////////////////////////////////////

#ifndef SUPERVISOR_H
#define SUPERVISOR_H

#include "xLog.h"
#include "xTask.h"
#include "xOsMutex.h"
#include "xAssert.h"
//TODO : inclure la map 

#include <mqueue.h>
#include <stdatomic.h>

typedef struct position_t
{
    int32_t t_iXPosition;
    int32_t t_iYPosition;
    float   t_fOrientation;
}tPosition;

typedef struct report_t
{
    uint64_t t_ulTime;
    tPosition t_tPosition;
    int32_t t_iBatteryLevel;
    int32_t t_iSpeed;
    int32_t t_iDistance;
    tPosition t_tCurrentPosition;
}tReportCtx;


typedef struct supervisorCtx
{
    xOsTaskCtx t_tTask;
    xOsMutexCtx t_tMutex;
    uint32_t t_iBatteryLevel;
    uint64_t t_ulTime;
    tPosition t_tPosition;
    tReportCtx t_tLastReport;
    tReportCtx t_tCurrentReport;
}tSupervisorCtx;







#endif // SUPERVISOR_H
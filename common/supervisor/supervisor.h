////////////////////////////////////////////////////////////
//  supervisor header file
//  Provides supervisor interface
//
// general disclosure: copy or share the file is forbidden
// Written : 23/05/2025
////////////////////////////////////////////////////////////

#ifndef SUPERVISOR_H
#define SUPERVISOR_H

#include "networkEncode.h"
#include "networkServer.h"
#include "xAssert.h"
#include "xLog.h"
#include "xOsMemory.h"
#include "xOsMutex.h"
#include "xTask.h"
#include "xTimer.h"
#include <stdatomic.h>

#define SUPERVISOR_TIMER_PERIOD_MS 200

#define SUPERVISOR_OK 0x6E00A000
#define SUPERVISOR_ERROR_INVALID_PARAM 0x6E00A002
#define SUPERVISOR_ERROR_MEMORY_ALLOCATION 0x6E00A003

////////////////////////////////////////////////////////////
/// @brief Position structure
////////////////////////////////////////////////////////////
typedef struct position_t
{
    int32_t t_iXPosition;
    int32_t t_iYPosition;
    float t_fOrientation;
} tPosition;

////////////////////////////////////////////////////////////
/// @brief Report structure
////////////////////////////////////////////////////////////
typedef struct report_t
{
    uint64_t t_ulTime;
    tPosition t_tPosition;
    int32_t t_iBatteryLevel;
    int32_t t_iSpeed;
    int32_t t_iDistance;
    tPosition t_tCurrentPosition;
    uint32_t map_hash;

} tReportCtx;

////////////////////////////////////////////////////////////
/// @brief Supervisor context structure
////////////////////////////////////////////////////////////
typedef struct supervisorCtx
{
    xOsTaskCtx t_tTask;
    xOsMutexCtx t_tMutex;
    xOsTimerCtx t_tTimer;
    xOsTimerCtx t_tStartTimer;
    uint32_t t_iBatteryLevel;
    uint64_t t_ulTime;
    tPosition t_tPosition;
    tReportCtx t_tLastReport;
    tReportCtx t_tCurrentReport;
} tSupervisorCtx;

////////////////////////////////////////////////////////////
/// @brief Initialize the supervisor
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
int32_t supervisor_init(void);

////////////////////////////////////////////////////////////
/// @brief Shutdown the supervisor
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
int32_t supervisor_shutdown(void);

////////////////////////////////////////////////////////////
/// @brief Start the supervisor
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
int32_t supervisor_start(void);

////////////////////////////////////////////////////////////
/// @brief Stop the supervisor
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
int32_t supervisor_stop(void);

////////////////////////////////////////////////////////////
/// @brief Send the full map to the client
/// @param client_id The client ID
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
int32_t supervisor_send_full_map(ClientID client_id);

#endif // SUPERVISOR_H
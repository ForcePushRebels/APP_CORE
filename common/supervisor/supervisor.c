////////////////////////////////////////////////////////////
//  supervisor implementation file
//  Provides supervisor implementation
//
// general disclosure: copy or share the file is forbidden
// Written : 23/05/2025
////////////////////////////////////////////////////////////

#include "supervisor.h"
#include "explorationManager.h"
#include "handleNetworkMessage.h"
#include "hardwareAbstraction.h"
#include "map_engine.h"
#include "networkEncode.h"
#include "networkServer.h"
#include "positionControl.h"
#include <stdint.h>
#include <string.h>
#include <unistd.h>

////////////////////////////////////////////////////////////
/// Private variables
////////////////////////////////////////////////////////////
static tSupervisorCtx s_tSupervisorCtx;

static bool supervisor_is_running = false;
////////////////////////////////////////////////////////////
/// Private function declarations
////////////////////////////////////////////////////////////
static void *supervisor_task(void *arg);
static void checkInfo(void *arg);
static int32_t send_map_fragments(void);

uint32_t float_to_uint32(float f)
{
    union {
        float f;
        uint32_t i;
    } u;
    u.f = f;
    return u.i;
}

////////////////////////////////////////////////////////////
/// sendFragmentMap
////////////////////////////////////////////////////////////

static int32_t send_map_fragments(void)
{
    X_LOG_TRACE("Sending map delta");
    uint32_t updated_cells_count = map_engine_get_updated_cells_count();
    if (updated_cells_count == 0)
    {
        return SUPERVISOR_OK;
    }
    map_fragment_t *cells = (map_fragment_t *)malloc(updated_cells_count * sizeof(map_fragment_t));
    if (cells == NULL)
    {
        X_LOG_TRACE("Failed to allocate memory for map delta");
        return SUPERVISOR_ERROR_MEMORY_ALLOCATION;
    }
    map_engine_get_updated_cells(cells, updated_cells_count);
    for (uint32_t i = 0; i < updated_cells_count; i++)
    {
        X_LOG_TRACE("Cell %d: %d, %d, %d, %d",
                    i,
                    cells[i].x_grid,
                    cells[i].y_grid,
                    cells[i].cell.type,
                    cells[i].cell.wall.wall_intensity);
        map_cell_t *cell = &cells[i].cell;
        cells[i].x_grid = HOST_TO_NET_SHORT(cells[i].x_grid);
        cells[i].y_grid = HOST_TO_NET_SHORT(cells[i].y_grid);
        int ret = networkServerSendMessage(1, ID_MAP_FRAGMENT, &cells[i], sizeof(map_fragment_t));
        if (ret != SERVER_OK)
        {
            X_LOG_TRACE("Failed to send map fragment %d: 0x%x, next cell", i, ret);
        }
    }

    X_LOG_TRACE("Map delta %d cells sent", updated_cells_count);
    map_engine_clear_updated_cells(cells, updated_cells_count);
    free(cells);
    return SUPERVISOR_OK;
}

////////////////////////////////////////////////////////////
/// sendFullMapHandle
////////////////////////////////////////////////////////////
static void sendFullMapHandle(clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    (void)p_ptMessage; // unused parameter
    X_LOG_TRACE("sendFullMapHandle");
    supervisor_send_full_map(networkServerGetClientID(p_ptClient));
}

////////////////////////////////////////////////////////////
/// supervisor_send_full_map
////////////////////////////////////////////////////////////
int32_t supervisor_send_full_map(ClientID client_id)
{
    // get map size
    size_t x_size, y_size, map_size, resolution_mm_per_cell;

    map_size = map_engine_get_map_size(&x_size, &y_size, &resolution_mm_per_cell);
    X_LOG_TRACE("Map size: %d, %d", x_size, y_size);

    typedef struct __attribute__((packed))
    {
        uint16_t x_size;
        uint16_t y_size;
        uint16_t resolution_mm_per_cell;
        map_cell_t map;
    } map_buffer_t;

    size_t map_buffer_size = 6 + map_size;

    map_buffer_t *map_buffer = (map_buffer_t *)malloc(map_buffer_size);
    if (map_buffer == NULL)
    {
        X_LOG_TRACE("Failed to allocate memory for map buffer");
        return SUPERVISOR_ERROR_MEMORY_ALLOCATION;
    }

    map_buffer->x_size = HOST_TO_NET_SHORT(x_size);
    map_buffer->y_size = HOST_TO_NET_SHORT(y_size);
    map_buffer->resolution_mm_per_cell = HOST_TO_NET_SHORT(resolution_mm_per_cell);

    // Use temporary buffer to avoid packed member address issue
    map_cell_t *temp_map = (map_cell_t *)malloc(map_size);
    if (temp_map == NULL)
    {
        free(map_buffer);
        X_LOG_TRACE("Failed to allocate memory for temporary map buffer");
        return SUPERVISOR_ERROR_MEMORY_ALLOCATION;
    }

    map_engine_get_map(temp_map);
    memcpy(&map_buffer->map, temp_map, map_size);
    free(temp_map);

    int ret = networkServerSendMessage(client_id, ID_MAP_FULL, map_buffer, map_buffer_size);
    free(map_buffer);
    return ret;
}

////////////////////////////////////////////////////////////
/// sendPosition
////////////////////////////////////////////////////////////
static int32_t sendPosition(tPosition pNewPosition)
{
    PositionPacked_t l_tPosition = {
        HOST_TO_NET_SHORT(pNewPosition.t_iXPosition),
        HOST_TO_NET_SHORT(pNewPosition.t_iYPosition),
        HOST_TO_NET_LONG(float_to_uint32(pNewPosition.t_fOrientation)),
    };

    int ret = networkServerSendMessage(1, ID_INF_POS, &l_tPosition, sizeof(l_tPosition));
    if (ret == SERVER_OK)
    {
        X_LOG_TRACE(
            "Position sent: %d, %d, %f", pNewPosition.t_iXPosition, pNewPosition.t_iYPosition, pNewPosition.t_fOrientation);
    }
    return ret;
}

////////////////////////////////////////////////////////////
/// sendStatus
////////////////////////////////////////////////////////////
static int32_t sendStatus(void)
{
#ifdef EXPLO_BUILD
    exploration_manager_state_t l_eStatus = explorationManager_getState();
    // Convert enum to uint32_t for network transmission
    uint32_t l_ulStatusNetwork = HOST_TO_NET_LONG((uint32_t)l_eStatus);

    return networkServerSendMessage(1, ID_INF_STATUS, &l_ulStatusNetwork, sizeof(uint32_t));
#else
    return 0;
#endif
}

////////////////////////////////////////////////////////////
/// sendDuration
////////////////////////////////////////////////////////////
static int32_t sendDuration(void)
{
#ifdef EXPLO_BUILD
    uint64_t l_ulStartTime = getStartTimeExploration();
    uint64_t l_ulCurrentTime = xTimerGetCurrentMs();
    uint64_t l_ulDuration = l_ulCurrentTime - l_ulStartTime;

    // Cap duration to uint32_t maximum value
    if (l_ulDuration > UINT32_MAX)
    {
        l_ulDuration = UINT32_MAX;
    }

    // Convert to network byte order
    uint32_t l_ulDurationNetwork = HOST_TO_NET_LONG((uint32_t)l_ulDuration);

    return networkServerSendMessage(1, ID_INF_TIME, &l_ulDurationNetwork, sizeof(uint32_t));
#else
    return 0;
#endif
}

////////////////////////////////////////////////////////////
/// sendBatteryLevel
////////////////////////////////////////////////////////////
static int32_t sendBatteryLevel(void)
{
    uint16_t l_iBatteryLevel = (uint16_t)s_tSupervisorCtx.t_iBatteryLevel;
    return networkServerSendMessage(1, ID_INF_BATTERY, &l_iBatteryLevel, sizeof(uint16_t));
}

////////////////////////////////////////////////////////////
/// supervisor_init
////////////////////////////////////////////////////////////
int32_t supervisor_init(void)
{
    int32_t l_iResult;

    XOS_MEMORY_SANITIZE(&s_tSupervisorCtx, sizeof(tSupervisorCtx));

    // Initialize context values
    s_tSupervisorCtx.t_iBatteryLevel = 100; // Start with full battery
    s_tSupervisorCtx.t_ulTime = 0;
    XOS_MEMORY_SANITIZE(&s_tSupervisorCtx.t_tPosition, sizeof(tPosition));
    XOS_MEMORY_SANITIZE(&s_tSupervisorCtx.t_tLastReport, sizeof(tReportCtx));
    XOS_MEMORY_SANITIZE(&s_tSupervisorCtx.t_tCurrentReport, sizeof(tReportCtx));

    // Initialize timer
    s_tSupervisorCtx.t_tTimer.t_ucMode = XOS_TIMER_MODE_PERIODIC;
    s_tSupervisorCtx.t_tTimer.t_ulPeriod = SUPERVISOR_TIMER_PERIOD_MS;

    // Initialize mutex
    l_iResult = mutexCreate(&s_tSupervisorCtx.t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        return l_iResult;
    }

    // Initialize timer
    l_iResult = xTimerCreate(&s_tSupervisorCtx.t_tTimer, SUPERVISOR_TIMER_PERIOD_MS, XOS_TIMER_MODE_PERIODIC);
    if (l_iResult != XOS_TIMER_OK)
    {
        mutexDestroy(&s_tSupervisorCtx.t_tMutex);
        return l_iResult;
    }

    //init task
    l_iResult = osTaskInit(&s_tSupervisorCtx.t_tTask);
    if (l_iResult != OS_TASK_SUCCESS)
    {
        mutexDestroy(&s_tSupervisorCtx.t_tMutex);
        return l_iResult;
    }

    s_tSupervisorCtx.t_tTask.t_ptTask = supervisor_task;
    s_tSupervisorCtx.t_tTask.t_ptTaskArg = &s_tSupervisorCtx;

    //create task
    l_iResult = osTaskCreate(&s_tSupervisorCtx.t_tTask);
    if (l_iResult != OS_TASK_SUCCESS)
    {
        mutexDestroy(&s_tSupervisorCtx.t_tMutex);
        xTimerDestroy(&s_tSupervisorCtx.t_tTimer);
        return l_iResult;
    }

    registerMessageHandler(ID_MAP_FULL, sendFullMapHandle);

    return SUPERVISOR_OK;
}

////////////////////////////////////////////////////////////
/// supervisor_shutdown
////////////////////////////////////////////////////////////
int32_t supervisor_shutdown(void)
{
    // Stop timer if active
    if (s_tSupervisorCtx.t_tTimer.t_ucActive)
    {
        xTimerStop(&s_tSupervisorCtx.t_tTimer);
    }

    // Signal task to stop
    atomic_store(&s_tSupervisorCtx.t_tTask.a_iStopFlag, OS_TASK_STOP_REQUEST);

    // Destroy timer
    xTimerDestroy(&s_tSupervisorCtx.t_tTimer);

    // Destroy mutex
    mutexDestroy(&s_tSupervisorCtx.t_tMutex);

    return SUPERVISOR_OK;
}

////////////////////////////////////////////////////////////
/// supervisor_stop
////////////////////////////////////////////////////////////
int32_t supervisor_stop(void)
{
    int32_t l_iResult;

    l_iResult = xTimerStop(&s_tSupervisorCtx.t_tTimer);
    if (l_iResult != XOS_TIMER_OK)
    {
        return l_iResult;
    }

    return SUPERVISOR_OK;
}

////////////////////////////////////////////////////////////
/// checkInfo
////////////////////////////////////////////////////////////
static void checkInfo(void *arg)
{
    if (!supervisor_is_running)
    {
        return;
    }

    uint64_t last_position_update = 0;

    Position_t l_tCurrentPosition;
    int32_t l_iResult;
    (void)arg; // Unused parameter

    // Lock mutex for thread safety
    l_iResult = mutexLock(&s_tSupervisorCtx.t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        return;
    }

    // Update time
    s_tSupervisorCtx.t_ulTime += SUPERVISOR_TIMER_PERIOD_MS;

    // Get current position - using Position_t type to match function signature
    l_iResult = position_control_get_position(&l_tCurrentPosition);
    if (l_iResult == POSITION_OK)
    {
        // Convert Position_t to tPosition for comparison and storage
        tPosition l_tConvertedPosition;
        l_tConvertedPosition.t_iXPosition = (int32_t)l_tCurrentPosition.x_mm;
        l_tConvertedPosition.t_iYPosition = (int32_t)l_tCurrentPosition.y_mm;
        l_tConvertedPosition.t_fOrientation = l_tCurrentPosition.angle_rad;

        // Update position if changed
        if (memcmp(&s_tSupervisorCtx.t_tPosition, &l_tConvertedPosition, sizeof(tPosition)) != 0
            || last_position_update + 1000 < xTimerGetCurrentMs())
        {
            s_tSupervisorCtx.t_tPosition = l_tConvertedPosition;
            // get timestamp ms
            last_position_update = xTimerGetCurrentMs();
            sendPosition(l_tConvertedPosition);
        }
    }

    int32_t l_iBatteryLevel = GetBatteryLevel();
    if (l_iBatteryLevel != (int32_t)s_tSupervisorCtx.t_iBatteryLevel)
    {
        s_tSupervisorCtx.t_iBatteryLevel = (uint32_t)l_iBatteryLevel;
        sendBatteryLevel();
    }

    uint32_t map_hash = map_engine_get_hash();
    if (map_hash != s_tSupervisorCtx.t_tCurrentReport.map_hash)
    {
        s_tSupervisorCtx.t_tCurrentReport.map_hash = map_hash;
        send_map_fragments();
    }

    // Update current report
    s_tSupervisorCtx.t_tCurrentReport.t_ulTime = s_tSupervisorCtx.t_ulTime;
    s_tSupervisorCtx.t_tCurrentReport.t_tPosition = s_tSupervisorCtx.t_tPosition;
    s_tSupervisorCtx.t_tCurrentReport.t_iBatteryLevel = (int32_t)s_tSupervisorCtx.t_iBatteryLevel;
    //s_tSupervisorCtx.t_tCurrentReport.t_iSpeed = getCurrentSpeed();
    //s_tSupervisorCtx.t_tCurrentReport.t_iDistance = getTotalDistance();
    s_tSupervisorCtx.t_tCurrentReport.t_tCurrentPosition = s_tSupervisorCtx.t_tPosition;

    // Store current report as last report
    s_tSupervisorCtx.t_tLastReport = s_tSupervisorCtx.t_tCurrentReport;

    // Send periodic reports
    sendDuration();
    sendStatus();

    // Unlock mutex
    mutexUnlock(&s_tSupervisorCtx.t_tMutex);
}

////////////////////////////////////////////////////////////
/// supervisor_task
////////////////////////////////////////////////////////////
static void *supervisor_task(void *arg)
{
    X_ASSERT(arg != NULL);

    while (!supervisor_is_running)
    {
        sleep(1);
    }
    sleep(3);
    X_LOG_TRACE("Supervisor task started");

    tSupervisorCtx *l_ptCtx = (tSupervisorCtx *)arg;

    int32_t l_iResult = xTimerStart(&l_ptCtx->t_tTimer);
    X_ASSERT(l_iResult == XOS_TIMER_OK);

    while (!l_ptCtx->t_tTimer.t_ucActive || !atomic_load(&l_ptCtx->t_tTimer.t_bPeriodicLockFlag))
    {
        // Vérifier si on doit arrêter la tâche
        if (atomic_load(&l_ptCtx->t_tTask.a_iStopFlag) == OS_TASK_STOP_REQUEST)
        {
            X_LOG_TRACE("Task stop requested while waiting for timer");
            return NULL;
        }
    }

    // Maintenant on peut appeler xTimerProcessPeriodicCallback
    l_iResult = xTimerProcessPeriodicCallback(&l_ptCtx->t_tTimer, checkInfo, l_ptCtx);

    return NULL;
}

////////////////////////////////////////////////////////////
/// supervisor_start
////////////////////////////////////////////////////////////
int32_t supervisor_start(void)
{
    supervisor_is_running = true;
    return SUPERVISOR_OK;
}

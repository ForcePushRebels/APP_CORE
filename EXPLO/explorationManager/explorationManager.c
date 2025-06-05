////////////////////////////////////////////////////////////
//  explorationManager implementation file
//  defines the explorationManager types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 04/06/2025
////////////////////////////////////////////////////////////

#include "explorationManager.h"
#include "safetyController.h"
#include "xAssert.h"
#include "xLog.h"
#include "xOsMemory.h"
#include "xTimer.h"
#include <stdatomic.h>
#include <string.h>

static atomic_int_fast32_t s_eExplorationManagerState = INIT;
static atomic_uint_fast64_t s_ulMissionStartTime = 0;

////////////////////////////////////////////////////////////
/// @brief Mission control message
////////////////////////////////////////////////////////////
typedef enum missionControlMessage
{
    MISSION_CONTROL_START,
    MISSION_CONTROL_STOP,
    MISSION_CONTROL_MANU_MODE,
} mission_control_message_t;

////////////////////////////////////////////////////////////
/// @brief Handle the mission control message
/// @param p_ptClient The client context
/// @param p_ptMessage The message
////////////////////////////////////////////////////////////
static void handleMissionControl(clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    X_ASSERT(p_ptClient != NULL);
    X_ASSERT(p_ptMessage != NULL);

    network_message_t l_tMessage;
    XOS_MEMORY_SANITIZE(&l_tMessage, sizeof(network_message_t));

    memcpy(&l_tMessage, p_ptMessage, sizeof(network_message_t));

    switch (l_tMessage.t_ptucPayload[0])
    {
        case MISSION_CONTROL_START:
            atomic_store_explicit(&s_eExplorationManagerState, MISSION_EN_COURS, memory_order_relaxed);

            // if not already started, start the mission
            if (atomic_load_explicit(&s_ulMissionStartTime, memory_order_relaxed) == 0)
            {
                atomic_store_explicit(&s_ulMissionStartTime, xTimerGetCurrentMs(), memory_order_relaxed);
            }
            break;
        case MISSION_CONTROL_STOP:
            atomic_store_explicit(&s_eExplorationManagerState, FIN_DE_MISSION, memory_order_relaxed);
            stopSafetyController();
            break;
        case MISSION_CONTROL_MANU_MODE:
            atomic_store_explicit(&s_eExplorationManagerState, MODE_MANUEL, memory_order_relaxed);
            // if not already started, start the mission
            if (atomic_load_explicit(&s_ulMissionStartTime, memory_order_relaxed) == 0)
            {
                atomic_store_explicit(&s_ulMissionStartTime, xTimerGetCurrentMs(), memory_order_relaxed);
            }
            setAutorizedMovement();
            break;
        default:
            X_LOG_TRACE("Message type %x currently not supported", l_tMessage.t_iHeader);
            return;
    }
}
////////////////////////////////////////////////////////////
/// @brief Start the exploration manager
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
void explorationManager_start(void)
{
    registerMessageHandler(ID_MISSION_CONTROL, handleMissionControl);
}

////////////////////////////////////////////////////////////
/// @brief Stop the exploration manager
/// @return 0 if success, error code otherwise
////////////////////////////////////////////////////////////
void explorationManager_stop(void)
{
    unregisterMessageHandler(ID_MISSION_CONTROL);
}

////////////////////////////////////////////////////////////
/// @brief Get the exploration manager state
/// @return The exploration manager state
////////////////////////////////////////////////////////////
exploration_manager_state_t explorationManager_getState(void)
{
    return atomic_load_explicit(&s_eExplorationManagerState, memory_order_relaxed);
}

////////////////////////////////////////////////////////////
/// @brief Set the exploration manager state
/// @param p_eExplorationManagerState The exploration manager state
////////////////////////////////////////////////////////////
void explorationManager_setState(exploration_manager_state_t p_eExplorationManagerState)
{
    atomic_store_explicit(&s_eExplorationManagerState, p_eExplorationManagerState, memory_order_relaxed);
}

////////////////////////////////////////////////////////////
/// @brief Get the mission start time
/// @return The mission start time
////////////////////////////////////////////////////////////
uint64_t getStartTimeExploration(void)
{
    return atomic_load_explicit(&s_ulMissionStartTime, memory_order_relaxed);
}

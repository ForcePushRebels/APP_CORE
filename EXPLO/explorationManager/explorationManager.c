////////////////////////////////////////////////////////////
//  explorationManager implementation file
//  defines the explorationManager types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 04/06/2025
////////////////////////////////////////////////////////////

#include "explorationManager.h"
#include "pilot.h"
#include "safetyController.h"
#include "xAssert.h"
#include "xLog.h"
#include "xOsMemory.h"
#include "xTimer.h"
#include <stdatomic.h>
#include <string.h>
static bool real_mission_started = false;

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
/// @brief Callback for the pilot
////////////////////////////////////////////////////////////
void pilot_callback_exploration(void *arg)
{
    (void)arg;
    if (!real_mission_started)
    {
        return;
    }

    typedef enum
    {
        MOVING_FORWARD,
        MOVING_TURN_1,
        MOVING_FORWARD_2,
        MOVING_TURN_2,
        MOVING_FORWARD_3,
        MOVING_TURN_3,
        MOVING_TURN_4,
        MOVING_FORWARD_4,
        MOVING_TURN_5,
        MOVING_FORWARD_5,
        MOVING_TURN_6,
        MOVING_END,
    } state_t;

    static state_t s_eState = MOVING_FORWARD;

    float speed = 40;
    switch (s_eState)
    {
        case MOVING_FORWARD:
            s_eState = MOVING_TURN_1;
            X_LOG_TRACE("Turning right");
            position_control_turn(-M_PI / 2, speed);
            break;
        case MOVING_TURN_1:
            s_eState = MOVING_FORWARD_2;
            X_LOG_TRACE("Moving forward");
            position_control_advance(100, speed);
            break;
        case MOVING_FORWARD_2:
            s_eState = MOVING_TURN_2;
            X_LOG_TRACE("Turning left");
            position_control_turn(M_PI / 2, speed);
            break;
        case MOVING_TURN_2:
            s_eState = MOVING_FORWARD_3;
            X_LOG_TRACE("Moving forward end");
            position_control_advance(580, speed);
            break;
        case MOVING_FORWARD_3:
            s_eState = MOVING_TURN_3;
            X_LOG_TRACE("Turning right");
            position_control_turn(-M_PI, speed);
            break;
        case MOVING_TURN_3:
            s_eState = MOVING_TURN_4;
            X_LOG_TRACE("Turning 4");
            position_control_turn(3 * M_PI / 2, speed);
            break;
        case MOVING_TURN_4:
            s_eState = MOVING_FORWARD_4;
            X_LOG_TRACE("Moving forward end");
            position_control_advance(400, speed);
            break;
        case MOVING_FORWARD_4:
            s_eState = MOVING_TURN_5;
            X_LOG_TRACE("Turning 5");
            position_control_turn(-M_PI / 2, speed);
            break;
        case MOVING_FORWARD_5:
            s_eState = MOVING_TURN_6;
            X_LOG_TRACE("Moving forward end");
            position_control_advance(250, speed);
            break;
        case MOVING_TURN_6:
            s_eState = MOVING_END;
            X_LOG_TRACE("Turning 6");
            position_control_turn(2 * M_PI, speed);
            break;
        case MOVING_END:
            s_eState = MOVING_END;
            X_LOG_TRACE("Moving end");
            position_control_stop();
            break;

        default:
            break;
    }
}

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
            X_LOG_TRACE("Starting exploration mission");
            atomic_store_explicit(&s_eExplorationManagerState, MISSION_EN_COURS, memory_order_relaxed);

            // if not already started, start the mission
            if (atomic_load_explicit(&s_ulMissionStartTime, memory_order_relaxed) == 0)
            {
                atomic_store_explicit(&s_ulMissionStartTime, xTimerGetCurrentMs(), memory_order_relaxed);
            }

            pilot_register_callback(pilot_callback_exploration);
            pilot_advance(100, 100);
            break;
        case MISSION_CONTROL_STOP:
            atomic_store_explicit(&s_eExplorationManagerState, FIN_DE_MISSION, memory_order_relaxed);
            stopSafetyController();
            break;
        case MISSION_CONTROL_MANU_MODE:

        {
            // if not already started, start the mission
            if (atomic_load_explicit(&s_ulMissionStartTime, memory_order_relaxed) == 0)
            {
                atomic_store_explicit(&s_ulMissionStartTime, xTimerGetCurrentMs(), memory_order_relaxed);
            }
            if (!real_mission_started)
            {
                pilot_register_callback(pilot_callback_exploration);
                // pilot_advance(150, 1000);
                position_control_advance(100, 20);
                real_mission_started = true;
                atomic_store_explicit(&s_eExplorationManagerState, MISSION_EN_COURS, memory_order_relaxed);
            }
            else
            {
                position_control_stop();
                real_mission_started = false;
                atomic_store_explicit(&s_eExplorationManagerState, MODE_MANUEL, memory_order_relaxed);
            }

            setAutorizedMovement();
        }

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
    if (p_eExplorationManagerState == MODE_MANUEL)
    {
        real_mission_started = false;
    }
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

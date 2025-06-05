////////////////////////////////////////////////////////////
//  safety controller header file
//  defines safety controller types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 28/05/2025
////////////////////////////////////////////////////////////

#include "safetyController.h"
#include "explorationManager.h"

static atomic_bool s_bAutorizedMovement = ATOMIC_VAR_INIT(false);

///////////////////////////////////////////
/// setMovementHandle
///////////////////////////////////////////
static void setMovementHandle(clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    (void)p_ptClient; // unused parameter

    if (atomic_load_explicit(&s_bAutorizedMovement, memory_order_relaxed) == false)
    {
        X_LOG_TRACE("Manual movement not authorized without autorization");
        return;
    }

    //cast the payload from 1bytes to movement_type_t
    if (p_ptMessage->t_ptucPayload == NULL)
    {
        X_LOG_TRACE("Payload is NULL");
        atomic_store_explicit(&s_bEmergencyStopFlag, true, memory_order_relaxed);
        return;
    }
    // cast the payload to movement_type_t
    movement_type_t l_eMovement = (movement_type_t)p_ptMessage->t_ptucPayload[0];

    // check if the movement is valid
    if (l_eMovement >= MOVE_COUNT)
    {
        X_LOG_TRACE("Invalid movement: %d", l_eMovement);
        atomic_store_explicit(&s_bEmergencyStopFlag, true, memory_order_relaxed);
        return;
    }

    explorationManager_setState(MODE_MANUEL);
    // call the movement function
    switch (l_eMovement)
    {
        case FORWARD_MOVEMENT:
            bool l_bMovePossible = checkForward();

            // check the sensors values
            if (l_bMovePossible == false)
            {
                X_LOG_TRACE("Move not possible");
                atomic_store_explicit(&s_bEmergencyStopFlag, true, memory_order_relaxed);
                return;
            }

            position_control_advance(10000, 2);
            //pilot_continuousAdvance(100);
            break;
        case LEFT_MOVEMENT:
            //pilot_turn(M_PI * 2, 100, true);
            position_control_turn(M_PI * 10, 2);
            break;
        case RIGHT_MOVEMENT:
            //pilot_turn(-M_PI * 2, 100, true);
            position_control_turn(-M_PI * 10, 2);
            break;
        case STOP_MOVEMENT:
            //pilot_stop();
            position_control_stop();
            break;
        default:
            break;
    }
}

///////////////////////////////////////////
/// safetyControllerInit
///////////////////////////////////////////
void safetyControllerInit(void)
{
    // regsiter network callbacks for ID set movement
    registerMessageHandler(ID_SET_MOVEMENT, setMovementHandle);
}

///////////////////////////////////////////
/// stopSafetyController
///////////////////////////////////////////
void stopSafetyController(void)
{
    atomic_store_explicit(&s_bEmergencyStopFlag, true, memory_order_relaxed);
}

///////////////////////////////////////////
/// setAutorizedMovement
///////////////////////////////////////////
void setAutorizedMovement(void)
{
    atomic_store_explicit(&s_bAutorizedMovement, true, memory_order_relaxed);
}
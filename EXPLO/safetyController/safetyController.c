////////////////////////////////////////////////////////////
//  safety controller header file
//  defines safety controller types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 28/05/2025
////////////////////////////////////////////////////////////

#include "safetyController.h"

///////////////////////////////////////////
/// setMovementHandle
///////////////////////////////////////////
static void setMovementHandle(clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    (void)p_ptClient; // unused parameter
    int l_iReturn = 0;
    int l_iPayloadSize = p_ptMessage->t_iPayloadSize;

    //cast the paayload from 1bytes to movement_type_t
    if (p_ptMessage->t_ptucPayload == NULL)
    {
        X_LOG_TRACE("Payload is NULL");
        atomic_store(&s_bEmergencyStopFlag, true);
        return;
    }

    movement_type_t l_eMovement = (movement_type_t)p_ptMessage->t_ptucPayload[0];
    X_LOG_TRACE("Received set movement message: %d", l_eMovement);
    switch (l_eMovement)
    {
        case STOP_MOVEMENT:
            X_LOG_TRACE("Stop movement");
            break;
        case FORWARD_MOVEMENT:
            X_LOG_TRACE("Forward movement");
            break;
        case LEFT_MOVEMENT:
            X_LOG_TRACE("Left movement");
            break;
        case RIGHT_MOVEMENT:
            X_LOG_TRACE("Right movement");
            break;
        default:
            break;
    }

    // check if the movement is valid
    if (l_eMovement >= MOVE_COUNT)
    {
        X_LOG_TRACE("Invalid movement: %d", l_eMovement);
        atomic_store(&s_bEmergencyStopFlag, true);
        return;
    }

    bool l_bMovePossible = checkForward();

    // check the sensors values
    if (l_bMovePossible == false)
    {
        X_LOG_TRACE("Move not possible");
        atomic_store(&s_bEmergencyStopFlag, true);
        return;
    }

    // call the movement function
    switch (l_eMovement)
    {
    case FORWARD_MOVEMENT:
        position_control_advance(10000, 2.0);
        //pilot_continuousAdvance(100);
        break;
    case LEFT_MOVEMENT:
        //pilot_turn(M_PI * 2, 100, true);
        position_control_turn(M_PI * 10, 0.5);
        break;
    case RIGHT_MOVEMENT:
        //pilot_turn(-M_PI * 2, 100, true);
        position_control_turn(-M_PI * 10, 0.5);
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
    atomic_store(&s_bEmergencyStopFlag, true);
}
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
    if (l_iPayloadSize != sizeof(movement_type_t))
    {
        X_LOG_TRACE("Invalid payload size");
        atomic_store(&s_bEmergencyStopFlag, true);
        return;
    }
    movement_type_t l_eMovement = (movement_type_t)p_ptMessage->t_ptucPayload[0];

    // check if the movement is valid
    if (l_eMovement < STOP_MOVEMENT || l_eMovement > LEFT_MOVEMENT)
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
        l_iReturn = position_control_advance(100, 100); // TODO Taper audren car ca marche pas
        break;
    case LEFT_MOVEMENT:
        l_iReturn = position_control_turn(90, 100); // TODO Taper audren car ca marche pas
        break;
    case RIGHT_MOVEMENT:
        l_iReturn = position_control_turn(-90, 100); // TODO Taper audren car ca marche pas
        break;
    case STOP_MOVEMENT:
        l_iReturn = position_control_stop(); // TODO Taper audren car ca marche pas
        break;
    default:
        l_iReturn = -1;
        break;
    }

    if (l_iReturn != 0)
    {
        X_LOG_TRACE("Movement failed with error code: %d", l_iReturn);
        atomic_store(&s_bEmergencyStopFlag, true);
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
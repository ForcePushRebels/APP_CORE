////////////////////////////////////////////////////////////
//  Network message handler implementation
//  Implements message handling system for network communications
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#include "handleNetworkMessage.h"
#include "networkEncode.h"
#include "networkServer.h"
#include "xLog.h"
#include "xAssert.h"
#include <stdlib.h>

///////////////////////////////////////////
/// Each index corresponds to a message type (0-47)
///////////////////////////////////////////
static message_handler_t s_atHandlers[MAX_MESSAGE_HANDLERS];
static bool s_abInitialized = false;                         // Initialization flag

///////////////////////////////////////////
/// handleUnknownMessage
///////////////////////////////////////////
static void handleUnknownMessage(clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    (void)p_ptClient;
    X_LOG_TRACE("Unhandled message type: 0x%02X", p_ptMessage->t_iHeader[0]);
}

///////////////////////////////////////////
/// initMessageHandlerSystem
///////////////////////////////////////////
void initMessageHandlerSystem(void)
{
    for (int i = 0; i < MAX_MESSAGE_HANDLERS; i++)
    {
        s_atHandlers[i] = handleUnknownMessage;
    }
    s_abInitialized = true;

    X_LOG_TRACE("Message handler system initialized with %d slots", MAX_MESSAGE_HANDLERS);
}

///////////////////////////////////////////
/// cleanupMessageHandlerSystem
///////////////////////////////////////////
void cleanupMessageHandlerSystem(void)
{
    if (s_abInitialized)
    {
        for (int i = 0; i < MAX_MESSAGE_HANDLERS; i++)
        {
            s_atHandlers[i] = handleUnknownMessage;
        }
        s_abInitialized = false;
    }

    X_LOG_TRACE("Message handler system cleaned up");
}

///////////////////////////////////////////
/// registerMessageHandler
///////////////////////////////////////////
void registerMessageHandler(uint8_t p_ucMessageType, message_handler_t p_ptHandler)
{
    X_ASSERT(s_abInitialized); // check if the system is initialized

    s_atHandlers[p_ucMessageType] = p_ptHandler;

    X_LOG_TRACE("Registered handler for message type 0x%02X", p_ucMessageType);
}

///////////////////////////////////////////
/// unregisterMessageHandler
///////////////////////////////////////////
void unregisterMessageHandler(uint8_t p_ucMessageType)
{
    s_atHandlers[p_ucMessageType] = handleUnknownMessage;

    X_LOG_TRACE("Unregistered handler for message type 0x%02X", p_ucMessageType);
}

///////////////////////////////////////////
/// findMessageHandler
///////////////////////////////////////////
static inline message_handler_t findMessageHandler(uint8_t p_ucMessageType)
{
    return s_atHandlers[p_ucMessageType];
}

///////////////////////////////////////////
/// handleNetworkMessage
///////////////////////////////////////////
void handleNetworkMessage(clientCtx *p_ptClient,
                          const network_message_t *p_ptMessage)
{
    X_ASSERT(p_ptClient != NULL);
    X_ASSERT(p_ptMessage != NULL);

    char clientAddress[64];
    networkServerGetClientAddress(networkServerGetClientID(p_ptClient), clientAddress, sizeof(clientAddress));

    X_LOG_TRACE("Received message from %s: type=0x%02X, size=%u bytes",
                clientAddress, p_ptMessage->t_iHeader[0], p_ptMessage->t_iPayloadSize);

    uint8_t l_ucMsgType = p_ptMessage->t_iHeader[0];

    // find the handler for this type of message
    message_handler_t l_ptHandler = findMessageHandler(l_ucMsgType);

    // call the handler
    l_ptHandler(p_ptClient, p_ptMessage);
}

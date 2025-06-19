////////////////////////////////////////////////////////////
//  Network message handler implementation
//  Implements message handling system for network communications
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#include "handleNetworkMessage.h"
#include "networkEncode.h"
#include "xServer.h"
#include "xAssert.h"
#include "xLog.h"
#include <stdlib.h>

///////////////////////////////////////////
/// Each index corresponds to a message type (0-47)
///////////////////////////////////////////
static message_handler_t s_atHandlers[MAX_MESSAGE_HANDLERS];
static bool s_abInitialized = false; // Initialization flag

///////////////////////////////////////////
/// handleUnknownMessage
///////////////////////////////////////////
static void handleUnknownMessage(struct clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    (void)p_ptClient;
    X_LOG_TRACE("Unhandled message type: 0x%02X", p_ptMessage->t_iHeader[0]);
}

///////////////////////////////////////////
/// initMessageHandlerSystem
///////////////////////////////////////////
void initMessageHandlerSystem(void)
{
    // Prevent double initialization that would erase registered handlers
    if (s_abInitialized)
    {
        X_LOG_TRACE("Message handler system already initialized (preventing handler reset)");
        return;
    }

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
    // Validate system initialization
    if (!s_abInitialized)
    {
        X_LOG_TRACE("Message handler system not initialized");
        return;
    }

    // Validate message type range
    if (p_ucMessageType >= MAX_MESSAGE_HANDLERS)
    {
        X_LOG_TRACE("Invalid message type 0x%02X (max: 0x%02X)", p_ucMessageType, MAX_MESSAGE_HANDLERS - 1);
        return;
    }

    // Validate handler is not NULL
    if (p_ptHandler == NULL)
    {
        X_LOG_TRACE("Cannot register NULL handler for message type 0x%02X", p_ucMessageType);
        return;
    }

    s_atHandlers[p_ucMessageType] = p_ptHandler;

    X_LOG_TRACE("Registered handler for message type 0x%02X", p_ucMessageType);
}

///////////////////////////////////////////
/// unregisterMessageHandler
///////////////////////////////////////////
void unregisterMessageHandler(uint8_t p_ucMessageType)
{
    // Validate system initialization
    if (!s_abInitialized)
    {
        X_LOG_TRACE("Message handler system not initialized");
        return;
    }

    // Validate message type range
    if (p_ucMessageType >= MAX_MESSAGE_HANDLERS)
    {
        X_LOG_TRACE("Invalid message type 0x%02X (max: 0x%02X)", p_ucMessageType, MAX_MESSAGE_HANDLERS - 1);
        return;
    }

    s_atHandlers[p_ucMessageType] = handleUnknownMessage;

    X_LOG_TRACE("Unregistered handler for message type 0x%02X", p_ucMessageType);
}

///////////////////////////////////////////
/// findMessageHandler
///////////////////////////////////////////
static inline message_handler_t findMessageHandler(uint8_t p_ucMessageType)
{
    // Validate system initialization
    if (!s_abInitialized)
    {
        X_LOG_TRACE("Message handler system not initialized, using default handler");
        return handleUnknownMessage;
    }

    // Validate message type range
    if (p_ucMessageType >= MAX_MESSAGE_HANDLERS)
    {
        X_LOG_TRACE(
            "Invalid message type 0x%02X (max: 0x%02X), using default handler", p_ucMessageType, MAX_MESSAGE_HANDLERS - 1);
        return handleUnknownMessage;
    }

    message_handler_t l_ptHandler = s_atHandlers[p_ucMessageType];

    // Double-check handler is not NULL (safety against corruption)
    if (l_ptHandler == NULL)
    {
        X_LOG_TRACE("Handler for message type 0x%02X is NULL, using default handler", p_ucMessageType);
        return handleUnknownMessage;
    }

    return l_ptHandler;
}

///////////////////////////////////////////
/// handleNetworkMessage
///////////////////////////////////////////
void handleNetworkMessage(struct clientCtx *p_ptClient, const network_message_t *p_ptMessage)
{
    // Validate input parameters
    if (p_ptClient == NULL)
    {
        X_LOG_TRACE("NULL client context provided");
        return;
    }

    if (p_ptMessage == NULL)
    {
        X_LOG_TRACE("NULL message provided");
        return;
    }

    uint8_t l_ucMsgType = p_ptMessage->t_iHeader[0];

    // find the handler for this type of message
    message_handler_t l_ptHandler = findMessageHandler(l_ucMsgType);

    // Additional safety check before calling handler
    if (l_ptHandler != NULL)
    {
        // call the handler
        l_ptHandler(p_ptClient, p_ptMessage);
    }
    else
    {
        X_LOG_TRACE("CRITICAL: Handler is NULL even after validation - system corruption possible");
        // Use fallback
        handleUnknownMessage(p_ptClient, p_ptMessage);
    }
}

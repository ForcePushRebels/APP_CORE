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
/// messageHandlerEntry_t
///////////////////////////////////////////
typedef struct message_handler_entry_t
{
    uint8_t t_ucMessageType;       // Message type
    message_handler_t t_ptHandler; // Message handler function
} messageHandlerEntry_t;

static messageHandlerEntry_t *s_ptHandlers = NULL; // list of handlers
static size_t s_ulHandlersCount = 0;               // number of handlers registered
static size_t s_ulHandlersCapacity = 0;            // current capacity of the handlers array

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
    s_ulHandlersCapacity = 10; // Initial size
    s_ptHandlers = (messageHandlerEntry_t *)malloc(s_ulHandlersCapacity * sizeof(messageHandlerEntry_t));
    s_ulHandlersCount = 0;

    X_LOG_TRACE("Message handler system initialized with capacity %zu", s_ulHandlersCapacity);
}

///////////////////////////////////////////
/// cleanupMessageHandlerSystem
///////////////////////////////////////////
void cleanupMessageHandlerSystem(void)
{
    if (s_ptHandlers != NULL)
    {
        free(s_ptHandlers);
        s_ptHandlers = NULL;
    }
    s_ulHandlersCount = 0;
    s_ulHandlersCapacity = 0;

    X_LOG_TRACE("Message handler system cleaned up");
}

///////////////////////////////////////////
/// registerMessageHandler
///////////////////////////////////////////
void registerMessageHandler(uint8_t p_ucMessageType, message_handler_t p_ptHandler)
{
    X_ASSERT(s_ptHandlers != NULL); // check if the system is initialized

    // check if a handler already exists for this type
    for (size_t i = 0; i < s_ulHandlersCount; i++)
    {
        if (s_ptHandlers[i].t_ucMessageType == p_ucMessageType)
        {
            s_ptHandlers[i].t_ptHandler = p_ptHandler;
            X_LOG_TRACE("Updated handler for message type 0x%02X", p_ucMessageType);
            return;
        }
    }

    // increase the capacity if necessary
    if (s_ulHandlersCount >= s_ulHandlersCapacity)
    {
        s_ulHandlersCapacity *= 2;
        s_ptHandlers = (messageHandlerEntry_t *)realloc(s_ptHandlers,
                                                        s_ulHandlersCapacity * sizeof(messageHandlerEntry_t));
        X_ASSERT(s_ptHandlers != NULL);

        X_LOG_TRACE("Expanded handler system capacity to %zu", s_ulHandlersCapacity);
    }

    // add the new handler
    s_ptHandlers[s_ulHandlersCount].t_ucMessageType = p_ucMessageType;
    s_ptHandlers[s_ulHandlersCount].t_ptHandler = p_ptHandler;
    s_ulHandlersCount++;

    X_LOG_TRACE("Registered handler for message type 0x%02X", p_ucMessageType);
}

///////////////////////////////////////////
/// unregisterMessageHandler
///////////////////////////////////////////
void unregisterMessageHandler(uint8_t p_ucMessageType)
{
    for (size_t i = 0; i < s_ulHandlersCount; i++)
    {
        if (s_ptHandlers[i].t_ucMessageType == p_ucMessageType)
        {
            // Move the last element to the place of the deleted element
            s_ptHandlers[i] = s_ptHandlers[s_ulHandlersCount - 1];
            s_ulHandlersCount--;
            X_LOG_TRACE("Unregistered handler for message type 0x%02X", p_ucMessageType);
            return;
        }
    }

    X_LOG_TRACE("No handler found for message type 0x%02X", p_ucMessageType);
}

///////////////////////////////////////////
/// findMessageHandler
///////////////////////////////////////////
static message_handler_t findMessageHandler(uint8_t p_ucMessageType)
{
    for (size_t i = 0; i < s_ulHandlersCount; i++)
    {
        if (s_ptHandlers[i].t_ucMessageType == p_ucMessageType)
        {
            return s_ptHandlers[i].t_ptHandler;
        }
    }
    return handleUnknownMessage;
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

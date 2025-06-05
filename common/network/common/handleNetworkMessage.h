////////////////////////////////////////////////////////////
//  Network message handler header file
//  Provides message handling interface for network communications
//
// general disclosure: copy or share the file is forbidden
// Written : 25/04/2025
////////////////////////////////////////////////////////////

#ifndef HANDLE_NETWORK_MESSAGE_H_
#define HANDLE_NETWORK_MESSAGE_H_

#include "networkServer.h"
#include "networkEncode.h"

///////////////////////////////////////////
/// @brief Definition of the type for the message handling functions
///////////////////////////////////////////
typedef void (*message_handler_t)(clientCtx* p_ptClient, const network_message_t* p_ptMessage);

///////////////////////////////////////////
/// @brief Register a handler for a specific message type
///
/// @param messageType Type of message to register the handler for
/// @param handler Function to call when the message is received
///////////////////////////////////////////
void registerMessageHandler(uint8_t p_ucMessageType, message_handler_t p_ptHandler);

///////////////////////////////////////////
/// @brief Unregister a handler for a specific message type
///
/// @param messageType Type of message to unregister the handler for
///////////////////////////////////////////
void unregisterMessageHandler(uint8_t p_ucMessageType);

///////////////////////////////////////////
/// @brief Get maximum number of supported message types
/// @return Maximum message type value + 1
///////////////////////////////////////////
#define MAX_MESSAGE_HANDLERS 48  

///////////////////////////////////////////
/// @brief Main function to handle network messages
///
/// @param client Client context
/// @param message Network message to handle
///////////////////////////////////////////
void handleNetworkMessage(clientCtx* p_ptClient, 
                          const network_message_t* p_ptMessage);

///////////////////////////////////////////
/// @brief Initialize message handler system
///////////////////////////////////////////
void initMessageHandlerSystem(void);

///////////////////////////////////////////
/// @brief Clean up message handler system
///////////////////////////////////////////
void cleanupMessageHandlerSystem(void);

#endif // HANDLE_NETWORK_MESSAGE_H_

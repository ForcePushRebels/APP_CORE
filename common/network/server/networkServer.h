////////////////////////////////////////////////////////////
//  Network Server Header
//  Provides a streamlined API for bidirectional Java-C communication
//
// general disclosure: copy or share the file is forbidden
// Written : 18/04/2025
////////////////////////////////////////////////////////////

#ifndef NETWORK_SERVER_H
#define NETWORK_SERVER_H

#include "xNetwork.h"
#include "xTask.h"
#include "xOsMutex.h"
#include "networkEncode.h"
#include <stdbool.h>
#include <stdint.h>

// Forward declarations
typedef struct server_ctx_t serverCtx;
typedef struct client_ctx_t clientCtx;

// Message handler function type
typedef void (*MessageHandler)(serverCtx* p_pttServer, clientCtx* p_pttClient, const network_message_t* p_pttMessage);

// Server configuration 
typedef struct {
    uint16_t t_usPort;             // Server port
    const char* t_pcBindAddress;   // Address to bind to (NULL for any)
    int t_iMaxClients;            // Maximum number of clients
    int t_iBacklog;               // Backlog for listen
    bool t_bUseTimeout;           // Use timeout for receive
    int t_iReceiveTimeout;        // Timeout in ms (0 for no timeout)
} ServerConfig;

// Default configuration values
#define DEFAULT_SERVER_PORT     8080
#define DEFAULT_MAX_CLIENTS     5
#define DEFAULT_BACKLOG         5
#define DEFAULT_RECV_TIMEOUT    0

// Error codes
#define SERVER_OK                   0x200B000
#define SERVER_ERROR                0x200B001
#define SERVER_MAX_CLIENTS_REACHED  0x200B002
#define SERVER_INVALID_STATE        0x200B003
#define SERVER_THREAD_ERROR         0x200B004
#define SERVER_CLIENT_DISCONNECTED  0x200B005
#define SERVER_SOCKET_ERROR         0x200B006
#define SERVER_MEMORY_ERROR         0x200B007
#define SERVER_TIMEOUT              0x200B008
#define SERVER_INVALID_PARAM        0x200B009
#define SERVER_NOT_RUNNING          0x200B00A

//-----------------------------------------------------------------------------
// Server Management Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Create a new server instance with default configuration
/// @return A new server instance or NULL on failure
///////////////////////////////////////////
serverCtx* serverCreate(void);

///////////////////////////////////////////
/// @brief Configure the server with custom settings
/// @param server The server instance
/// @param config Configuration parameters
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverConfigure(serverCtx* p_pttServer, const ServerConfig* p_pttConfig);

///////////////////////////////////////////
/// @brief Set message handler function for processing incoming messages
/// @param server The server instance
/// @param handler The message handler function
///////////////////////////////////////////
void serverSetMessageHandler(serverCtx* p_pttServer, MessageHandler p_pttHandler);

///////////////////////////////////////////
/// @brief Start the server
/// @param server The server instance
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverStart(serverCtx* p_pttServer);

///////////////////////////////////////////
/// @brief Stop the server
/// @param server The server instance
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverStop(serverCtx* p_pttServer);

///////////////////////////////////////////
/// @brief Destroy the server and free all resources
/// @param server The server instance
///////////////////////////////////////////
void serverDestroy(serverCtx* p_pttServer);

//-----------------------------------------------------------------------------
// Client Management Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Get client IP address as string
/// @param client The client instance
/// @param buffer Buffer to store address
/// @param size Buffer size
/// @return true on success
///////////////////////////////////////////
bool serverGetClientAddress(clientCtx* p_pttClient, char* p_pcBuffer, int p_iSize);

///////////////////////////////////////////
/// @brief Get client port
/// @param client The client instance
/// @return Port number or 0 on error
///////////////////////////////////////////
uint16_t serverGetClientPort(clientCtx* p_pttClient);

///////////////////////////////////////////
/// @brief Disconnect a client
/// @param server The server instance
/// @param client The client instance
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverDisconnectClient(serverCtx* p_pttServer, clientCtx* p_pttClient);

///////////////////////////////////////////
/// @brief Set custom data for a client
/// @param client The client instance
/// @param userData User data pointer
///////////////////////////////////////////
void serverSetClientUserData(clientCtx* p_pttClient, void* p_pvUserData);

///////////////////////////////////////////
/// @brief Get custom data for a client
/// @param client The client instance
/// @return User data pointer
///////////////////////////////////////////
void* serverGetClientUserData(clientCtx* p_pttClient);

///////////////////////////////////////////
/// @brief Send data to a client
/// @param client The client to send data to
/// @param data The data to send
/// @param size The size of the data in bytes
/// @return Number of bytes sent or negative error code
///////////////////////////////////////////
int serverSendToClient(clientCtx* p_pttClient, const void* p_pvData, int p_iSize);

//-----------------------------------------------------------------------------
// Message Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Send a message to a client
/// @param server The server instance
/// @param client The client instance
/// @param msgType Message type from network_message_type_t
/// @param payload Message data (can be NULL if size is 0)
/// @param payloadSize Size of payload in bytes
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverSendMessage(serverCtx* p_pttServer, clientCtx* p_pttClient, 
                    uint8_t p_ucMsgType, const void* p_pvPayload, uint32_t p_ulPayloadSize);

///////////////////////////////////////////
/// @brief Get string representation of error code
/// @param error The error code
/// @return Error string
///////////////////////////////////////////
const char* serverGetErrorString(int p_iError); 

///////////////////////////////////////////
/// @brief Create default server configuration
/// @return Default configuration
///////////////////////////////////////////
ServerConfig serverCreateDefaultConfig(void);

#endif // NETWORK_SERVER_H 
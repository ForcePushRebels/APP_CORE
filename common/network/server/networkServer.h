////////////////////////////////////////////////////////////
//  Network Server Header
//  Provides a streamlined API for bidirectional Java-C communication
//
// general disclosure: copy or share the file is forbidden
// Written : 18/04/2025
////////////////////////////////////////////////////////////

#ifndef NETWORK_SERVER_H
#define NETWORK_SERVER_H

#include "networkEncode.h"
#include "xNetwork.h"
#include "xOsMutex.h"
#include "xProtocol.h"
#include "xTask.h"
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>

// Forward declarations
typedef struct server_ctx_t serverCtx;
typedef struct client_ctx_t clientCtx;

// Server configuration
typedef struct
{
    uint16_t t_usPort;           // Server port
    const char *t_pcBindAddress; // Address to bind to (NULL for any)
    int t_iMaxClients;           // Maximum number of clients
    int t_iBacklog;              // Backlog for listen
    bool t_bUseTimeout;          // Use timeout for receive
    int t_iReceiveTimeout;       // Timeout in ms (0 for no timeout)
} ServerConfig;

// Default configuration values
#define DEFAULT_SERVER_PORT 8080
#define DEFAULT_MAX_CLIENTS 5
#define DEFAULT_BACKLOG 5
#define DEFAULT_RECV_TIMEOUT 0

// Client ID type
typedef uint32_t ClientID;
#define INVALID_CLIENT_ID 0

// Error codes
#define SERVER_OK 0x200B000
#define SERVER_ERROR 0x200B001
#define SERVER_MAX_CLIENTS_REACHED 0x200B002
#define SERVER_INVALID_STATE 0x200B003
#define SERVER_THREAD_ERROR 0x200B004
#define SERVER_CLIENT_DISCONNECTED 0x200B005
#define SERVER_SOCKET_ERROR 0x200B006
#define SERVER_MEMORY_ERROR 0x200B007
#define SERVER_TIMEOUT 0x200B008
#define SERVER_INVALID_PARAM 0x200B009
#define SERVER_NOT_RUNNING 0x200B00A
#define SERVER_CLIENT_NOT_FOUND 0x200B00B

///////////////////////////////////////////
/// @brief Initialize the network server system
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerInit(void);

///////////////////////////////////////////
/// @brief Configure the server with custom parameters
/// @param config Configuration parameters
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerConfigure(const ServerConfig *p_pttConfig);

///////////////////////////////////////////
/// @brief Start the server
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerStart(void);

///////////////////////////////////////////
/// @brief Stop the server
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerStop(void);

///////////////////////////////////////////
/// @brief Clean up the network server system and release all resources
///////////////////////////////////////////
void networkServerCleanup(void);

///////////////////////////////////////////
/// @brief Connect server to network (encapsulates bind + listen)
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerConnect(void);

//-----------------------------------------------------------------------------
// Client Management Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Get the client address as a string
/// @param clientId Client ID
/// @param buffer Buffer to store the address
/// @param size Buffer size
/// @return true on success
///////////////////////////////////////////
bool networkServerGetClientAddress(ClientID p_tClientId, char *p_ptcBuffer, int p_iSize);

///////////////////////////////////////////
/// @brief Get the client port
/// @param clientId Client ID
/// @return Port number or 0 on error
///////////////////////////////////////////
uint16_t networkServerGetClientPort(ClientID p_tClientId);

///////////////////////////////////////////
/// @brief Disconnect a client
/// @param clientId Client ID
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerDisconnectClient(ClientID p_tClientId);

///////////////////////////////////////////
/// @brief Set custom data for a client
/// @param clientId Client ID
/// @param userData Pointer to the user data
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerSetClientUserData(ClientID p_tClientId, void *p_pvUserData);

///////////////////////////////////////////
/// @brief Get custom data for a client
/// @param clientId Client ID
/// @return Pointer to the user data or NULL on error
///////////////////////////////////////////
void *networkServerGetClientUserData(ClientID p_tClientId);

///////////////////////////////////////////
/// @brief Send data to a client
/// @param clientId Client ID
/// @param data Data to send
/// @param size Data size in bytes
/// @return Number of bytes sent or negative error code
///////////////////////////////////////////
int networkServerSendToClient(ClientID p_tClientId, const void *p_pvData, int p_iSize);

//-----------------------------------------------------------------------------
// Message Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Send a message to a client
/// @param clientId Client ID
/// @param msgType Type of message from network_message_type_t
/// @param payload Data of the message (can be NULL if size is 0)
/// @param payloadSize Size of the payload in bytes (must be <= UINT16_MAX)
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerSendMessage(ClientID p_tClientId, uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize);

///////////////////////////////////////////
/// @brief Get the error string representation
/// @param error Error code
/// @return Error string
///////////////////////////////////////////
const char *networkServerGetErrorString(int p_iError);

///////////////////////////////////////////
/// @brief Create a default server configuration
/// @return Default configuration
///////////////////////////////////////////
ServerConfig networkServerCreateDefaultConfig(void);

///////////////////////////////////////////
/// @brief Get the client ID from the client context
/// @param client The client context
/// @return Client ID or INVALID_CLIENT_ID on error
///////////////////////////////////////////
ClientID networkServerGetClientID(clientCtx *p_ptClient);

///////////////////////////////////////////
/// @brief Get the client context from the client ID
/// @param clientId Client ID
/// @return Client context or NULL on error
///////////////////////////////////////////
clientCtx *networkServerGetClientCtx(ClientID p_tClientId);

///////////////////////////////////////////
/// @brief Send a message to all clients
/// @param msgType Type of message from network_message_type_t
/// @param payload Data of the message (can be NULL if size is 0)
/// @param payloadSize Size of the payload in bytes (must be <= UINT16_MAX)
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerSendMessageToAllClients(uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize);

//-----------------------------------------------------------------------------
// Android Client Management Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Set the Android client ID for supervisor communication
/// @param clientId Client ID of the Android client
/// @return SERVER_OK or error code
///////////////////////////////////////////
int networkServerSetAndroidClient(ClientID p_tClientId);

///////////////////////////////////////////
/// @brief Get the Android client ID (validated)
/// @return Valid Android client ID or INVALID_CLIENT_ID if not connected
///////////////////////////////////////////
ClientID networkServerGetAndroidClient(void);

#endif // NETWORK_SERVER_H
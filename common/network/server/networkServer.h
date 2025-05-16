////////////////////////////////////////////////////////////
//  server header file
//  declare server functions
//
// general discloser: copy or share the file is forbidden
// Written : 18/04/2025
////////////////////////////////////////////////////////////

#ifndef SERVER_H
#define SERVER_H

#include "xNetwork.h"
#include "xTask.h"
#include "xOsMutex.h"

///////////////////////////////////////////
/// Server error codes
///////////////////////////////////////////
#define SERVER_OK                   0x200B000
#define SERVER_ERROR                0x200B001
#define SERVER_MAX_CLIENTS_REACHED  0x200B002
#define SERVER_INVALID_STATE        0x200B003
#define SERVER_THREAD_ERROR         0x200B004
#define SERVER_CLIENT_DISCONNECTED  0x200B005
#define SERVER_SOCKET_ERROR         0x200B006
#define SERVER_MEMORY_ERROR         0x200B007
#define SERVER_TIMEOUT              0x200B008
#define SERVER_INVALID_PARAMETER    0x200B009
#define SERVER_NOT_RUNNING          0x200B00A

///////////////////////////////////////////
/// Server configuration defaults
///////////////////////////////////////////
#define SERVER_DEFAULT_PORT         8080
#define SERVER_MAX_CLIENTS          5
#define SERVER_BUFFER_SIZE          4096
#define SERVER_SOCKET_TIMEOUT       0 

///////////////////////////////////////////
/// Server states
///////////////////////////////////////////
typedef enum 
{
    SERVER_STATE_UNINITIALIZED,
    SERVER_STATE_INITIALIZED,
    SERVER_STATE_RUNNING,
    SERVER_STATE_STOPPING,
    SERVER_STATE_ERROR
} ServerState;

// Forward declarations
typedef struct server_ctx_t serverCtx;
typedef struct client_ctx_t clientCtx;

///////////////////////////////////////////
/// @brief Client connection event callback
///
/// @param server Server context
/// @param client Client context
///////////////////////////////////////////
typedef void (*ServerClientConnectCallback)(serverCtx* server, clientCtx* client);

///////////////////////////////////////////
/// @brief Client disconnection event callback
///
/// @param server Server context
/// @param client Client context
///////////////////////////////////////////
typedef void (*ServerClientDisconnectCallback)(serverCtx* server, clientCtx* client);

///////////////////////////////////////////
/// @brief Data received event callback
///
/// @param server Server context
/// @param client Client context
/// @param data Received data
/// @param size Data size
///////////////////////////////////////////
typedef void (*ServerDataReceivedCallback)(serverCtx* server, clientCtx* client, void* data, int size);

///////////////////////////////////////////
/// @brief Server configuration structure
///////////////////////////////////////////
typedef struct {
    uint16_t port;             // Listening port
    const char* bindAddress;   // Address to bind to (NULL for any)
    int maxClients;            // Maximum number of concurrent clients
    int backlog;               // Maximum number of pending connections
    bool useTimeout;           // Whether to use socket timeout
    int receiveTimeout;        // Receive timeout in milliseconds (0 = no timeout)
    bool reuseAddr;            // Whether to reuse address
} ServerConfig;

///////////////////////////////////////////
/// @brief Create a new server instance
///
/// @return Pointer to server context or NULL on error
///////////////////////////////////////////
serverCtx* serverCreate(void);

///////////////////////////////////////////
/// @brief Configure the server
///
/// @param server Server context
/// @param config Server configuration
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverConfigure(serverCtx* server, const ServerConfig* config);

///////////////////////////////////////////
/// @brief Set callback for client connection events
///
/// @param server Server context
/// @param callback Callback function
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverSetOnClientConnect(serverCtx* server, ServerClientConnectCallback callback);

///////////////////////////////////////////
/// @brief Set callback for client disconnection events
///
/// @param server Server context
/// @param callback Callback function
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverSetOnClientDisconnect(serverCtx* server, ServerClientDisconnectCallback callback);

///////////////////////////////////////////
/// @brief Set callback for data received events
///
/// @param server Server context
/// @param callback Callback function
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverSetOnDataReceived(serverCtx* server, ServerDataReceivedCallback callback);

///////////////////////////////////////////
/// @brief Start the server
///
/// @param server Server context
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverStart(serverCtx* server);

///////////////////////////////////////////
/// @brief Stop the server
///
/// @param server Server context
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverStop(serverCtx* server);

///////////////////////////////////////////
/// @brief Destroy the server and free all associated resources
///
/// @param server Server context
///////////////////////////////////////////
void serverDestroy(serverCtx* server);

///////////////////////////////////////////
/// @brief Send data to a client
///
/// @param client Client context
/// @param data Data to send
/// @param size Data size
/// @return Number of bytes sent or error code
///////////////////////////////////////////
int serverSendToClient(clientCtx* client, const void* data, int size);

///////////////////////////////////////////
/// @brief Disconnect a client
///
/// @param client Client context
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverDisconnectClient(clientCtx* client);

///////////////////////////////////////////
/// @brief Set user data associated with a client
///
/// @param client Client context
/// @param userData User data pointer
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverSetClientUserData(clientCtx* client, void* userData);

///////////////////////////////////////////
/// @brief Get user data associated with a client
///
/// @param client Client context
/// @return User data pointer or NULL if not set
///////////////////////////////////////////
void* serverGetClientUserData(clientCtx* client);

///////////////////////////////////////////
/// @brief Get client address
///
/// @param client Client context
/// @param address Output buffer for address string
/// @param size Buffer size
/// @return SERVER_OK or error code
///////////////////////////////////////////
int serverGetClientAddress(clientCtx* client, char* address, int size);

///////////////////////////////////////////
/// @brief Get client port
///
/// @param client Client context
/// @return Client port or 0 on error
///////////////////////////////////////////
uint16_t serverGetClientPort(clientCtx* client);

///////////////////////////////////////////
/// @brief Get error string for server error code
///
/// @param errorCode Server error code
/// @return Error string
///////////////////////////////////////////
const char* serverGetErrorString(int errorCode);

///////////////////////////////////////////
/// @brief Create default server configuration
///
/// @return Default server configuration
///////////////////////////////////////////
ServerConfig serverCreateDefaultConfig(void);

#endif // SERVER_H

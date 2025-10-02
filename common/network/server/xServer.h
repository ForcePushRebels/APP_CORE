////////////////////////////////////////////////////////////
//  xServer - Simple & Safe TCP Server
//  Zero dynamic allocation, runtime-safe design
//  Thread-per-client architecture with fixed-size pools
//
// general disclosure: copy or share the file is forbidden
// Written : 19/01/2025 - Clean rewrite
////////////////////////////////////////////////////////////

#ifndef X_SERVER_H
#define X_SERVER_H

#include "networkEncode.h"
#include "tlsEngine.h"
#include "xNetwork.h"
#include "xOsMutex.h"
#include "xProtocol.h"
#include "xTask.h"
#include <stdbool.h>
#include <stdint.h>

//-----------------------------------------------------------------------------
// Configuration Constants
//-----------------------------------------------------------------------------

#define SERVER_MAX_CLIENTS 8          // Maximum concurrent clients
#define SERVER_RECV_BUFFER_SIZE 65536 // Client receive buffer size
#define SERVER_SEND_BUFFER_SIZE 65536 // Client send buffer size
#define SERVER_CLIENT_NAME_SIZE 64    // Client name/address buffer
#define SERVER_DEFAULT_PORT 8080      // Default server port
#define SERVER_EPOLL_TIMEOUT 100      // Epoll timeout in milliseconds

//-----------------------------------------------------------------------------
// Error Codes
//-----------------------------------------------------------------------------

#define SERVER_OK 0x400A000
#define SERVER_ERROR 0x400A001
#define SERVER_INVALID_PARAM 0x400A002
#define SERVER_NOT_INITIALIZED 0x400A003
#define SERVER_ALREADY_RUNNING 0x400A004
#define SERVER_NOT_RUNNING 0x400A005
#define SERVER_MAX_CLIENTS_REACHED 0x400A006
#define SERVER_CLIENT_NOT_FOUND 0x400A007
#define SERVER_SOCKET_ERROR 0x400A008
#define SERVER_TLS_ERROR 0x400A009
#define SERVER_THREAD_ERROR 0x400A00A

//-----------------------------------------------------------------------------
// Types
//-----------------------------------------------------------------------------

typedef uint8_t ClientID;
#define INVALID_CLIENT_ID 0xFF

// Server configuration
typedef struct
{
    uint16_t t_usPort;      // Server port
    bool t_bUseTls;         // Enable TLS
    int t_iSocketTimeout;   // Socket timeout (seconds)
    char t_acCertFile[256]; // TLS certificate file path
    char t_acKeyFile[256];  // TLS private key file path
    char t_acCaDir[256];    // TLS CA directory
} ServerConfig;

// Client context (fixed-size, no dynamic allocation)
typedef struct
{
    ClientID t_tId;                               // Client ID (0xFF = unused slot)
    NetworkSocket *t_ptSocket;                    // Client socket
    NetworkAddress t_tAddress;                    // Client address
    char t_acClientName[SERVER_CLIENT_NAME_SIZE]; // Client address string

    // Threading
    xOsTaskCtx t_tClientTask;   // Client thread
    volatile bool t_bConnected; // Connection state
    volatile bool t_bShutdown;  // Shutdown request

    // TLS support
    WOLFSSL *t_ptTlsSession; // TLS session (NULL if plain TCP)

    // Buffers (no dynamic allocation)
    uint8_t t_aucRecvBuffer[SERVER_RECV_BUFFER_SIZE];
    uint8_t t_aucSendBuffer[SERVER_SEND_BUFFER_SIZE];

    // Statistics
    uint64_t t_ullBytesReceived;
    uint64_t t_ulBytesSent;
    uint32_t t_ulMessagesReceived;
    uint32_t t_ulMessagesSent;
} clientCtx;

//-----------------------------------------------------------------------------
// Public API
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// @brief Initialize the server
/// @return SERVER_OK or error code
///////////////////////////////////////////
int xServerInit(void);

///////////////////////////////////////////
/// @brief Configure the server
/// @param p_ptConfig Server configuration
/// @return SERVER_OK or error code
///////////////////////////////////////////
int xServerConfigure(const ServerConfig *p_ptConfig);

///////////////////////////////////////////
/// @brief Start the server
/// @return SERVER_OK or error code
///////////////////////////////////////////
int xServerStart(void);

///////////////////////////////////////////
/// @brief Stop the server
/// @return SERVER_OK or error code
///////////////////////////////////////////
int xServerStop(void);

///////////////////////////////////////////
/// @brief Cleanup server resources
///////////////////////////////////////////
void xServerCleanup(void);

///////////////////////////////////////////
/// @brief Send message to specific client
/// @param p_tClientId Client ID
/// @param p_ucMsgType Message type
/// @param p_pvPayload Payload data
/// @param p_ulPayloadSize Payload size
/// @return SERVER_OK or error code
///////////////////////////////////////////
int xServerSendMessage(ClientID p_tClientId, uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize);

///////////////////////////////////////////
/// @brief Broadcast message to all clients
/// @param p_ucMsgType Message type
/// @param p_pvPayload Payload data
/// @param p_ulPayloadSize Payload size
/// @return SERVER_OK or error code
///////////////////////////////////////////
int xServerBroadcastMessage(uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize);

///////////////////////////////////////////
/// @brief Get client information
/// @param p_tClientId Client ID
/// @param p_ptcBuffer Buffer for client info string
/// @param p_iBufferSize Buffer size
/// @return true if client found, false otherwise
///////////////////////////////////////////
bool xServerGetClientInfo(ClientID p_tClientId, char *p_ptcBuffer, int p_iBufferSize);

///////////////////////////////////////////
/// @brief Get connected clients count
/// @return Number of connected clients
///////////////////////////////////////////
int xServerGetClientCount(void);

///////////////////////////////////////////
/// @brief Create default configuration
/// @return Default server configuration
///////////////////////////////////////////
ServerConfig xServerCreateDefaultConfig(void);

///////////////////////////////////////////
/// @brief Get error string
/// @param p_iError Error code
/// @return Error description
///////////////////////////////////////////
const char *xServerGetErrorString(int p_iError);

///////////////////////////////////////////
/// @brief Get client context (for message handlers)
/// @param p_tClientId Client ID
/// @return Client context or NULL if not found
/// @note This is safe because we use fixed-size arrays
///////////////////////////////////////////
clientCtx *xServerGetclientCtx(ClientID p_tClientId);

///////////////////////////////////////////
/// @brief Get client ID from context (for message handlers)
/// @param p_ptContext Client context
/// @return Client ID or INVALID_CLIENT_ID
///////////////////////////////////////////
ClientID xServerGetClientID(const clientCtx *p_ptContext);

#endif // X_SERVER_H
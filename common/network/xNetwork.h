////////////////////////////////////////////////////////////
//  Network core header file for embedded systems
//  Defines the basic network types and constants
//  IPv4-focused, simplified API with thread-safety
//
// general disclosure: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 22/04/2025 - Simplified API without TLS
////////////////////////////////////////////////////////////

#ifndef NETWORK_CORE_H_
#define NETWORK_CORE_H_

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <netdb.h>
#include <sys/select.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "xAssert.h"
#include "xOsMutex.h"
#include "xLog.h"

// Configuration constants
#define NETWORK_MAX_SOCKETS 16        // Maximum number of simultaneous sockets
#define NETWORK_BUFFER_SIZE 512       // Default buffer size for operations
#define NETWORK_MAX_PENDING 5         // Default pending connections queue
#define NETWORK_DEFAULT_TIMEOUT 30000 // Default timeout in milliseconds (30 seconds)

// Network error codes
#define NETWORK_OK              0
#define NETWORK_ERROR           -1
#define NETWORK_TIMEOUT         -2
#define NETWORK_INVALID_PARAM   -3

// Byte order conversion macros
#define HOST_TO_NET_LONG(p_uiValue) htonl(p_uiValue)
#define HOST_TO_NET_SHORT(p_usValue) htons(p_usValue)
#define NET_TO_HOST_LONG(p_uiValue) ntohl(p_uiValue)
#define NET_TO_HOST_SHORT(p_usValue) ntohs(p_usValue)

// Socket type definition
typedef struct
{
    int t_iSocketFd;     // Socket file descriptor
    int t_iType;         // Socket type (TCP/UDP)
    bool t_bConnected;   // Connection state
    t_MutexCtx t_Mutex;  // Mutex for thread safety
} NetworkSocket;

// Network address structure (IPv4 only)
typedef struct
{
    char t_cAddress[INET_ADDRSTRLEN];
    unsigned short t_usPort;
} NetworkAddress;

// Available socket types
#define NETWORK_SOCK_TCP SOCK_STREAM
#define NETWORK_SOCK_UDP SOCK_DGRAM

//////////////////////////////////
/// Core API functions
//////////////////////////////////

//////////////////////////////////
/// @brief Create a socket
/// @param p_iType Socket type (NETWORK_SOCK_TCP or NETWORK_SOCK_UDP)
/// @return NetworkSocket* Socket handle or NULL on error
//////////////////////////////////
NetworkSocket *networkCreateSocket(int p_iType);

//////////////////////////////////
/// @brief Create a network address
/// @param p_pcAddress IP address string (NULL or empty for INADDR_ANY)
/// @param p_usPort Port number
/// @return NetworkAddress Address structure
//////////////////////////////////
NetworkAddress networkMakeAddress(const char *p_pcAddress, unsigned short p_usPort);

//////////////////////////////////
/// @brief Bind socket to address
/// @param p_pSocket Socket handle
/// @param p_pAddress Address structure
/// @return int Error code
//////////////////////////////////
int networkBind(NetworkSocket *p_pSocket, const NetworkAddress *p_pAddress);

//////////////////////////////////
/// @brief Listen for incoming connections
/// @param p_pSocket Socket handle
/// @param p_iBacklog Maximum pending connections
/// @return int Error code
//////////////////////////////////
int networkListen(NetworkSocket *p_pSocket, int p_iBacklog);

//////////////////////////////////
/// @brief Accept incoming connection
/// @param p_pSocket Listening socket
/// @param p_pClientAddress Address to store client info (can be NULL)
/// @return NetworkSocket* New socket handle or NULL on error
//////////////////////////////////
NetworkSocket *networkAccept(NetworkSocket *p_pSocket, NetworkAddress *p_pClientAddress);

//////////////////////////////////
/// @brief Connect to remote server
/// @param p_pSocket Socket handle
/// @param p_pAddress Remote address
/// @return int Error code
//////////////////////////////////
int networkConnect(NetworkSocket *p_pSocket, const NetworkAddress *p_pAddress);

//////////////////////////////////
/// @brief Send data
/// @param p_pSocket Socket handle
/// @param p_pBuffer Data buffer
/// @param p_ulSize Data size
/// @return int Bytes sent or error code
//////////////////////////////////
int networkSend(NetworkSocket *p_pSocket, const void *p_pBuffer, unsigned long p_ulSize);

//////////////////////////////////
/// @brief Receive data
/// @param p_pSocket Socket handle
/// @param p_pBuffer Data buffer
/// @param p_ulSize Buffer size
/// @return int Bytes received or error code
//////////////////////////////////
int networkReceive(NetworkSocket *p_pSocket, void *p_pBuffer, unsigned long p_ulSize);

//////////////////////////////////
/// @brief Close socket
/// @param p_pSocket Socket handle
/// @return int Error code
//////////////////////////////////
int networkCloseSocket(NetworkSocket *p_pSocket);

//////////////////////////////////
/// @brief Set socket timeout for send or receive operations
/// @param p_pSocket Socket handle
/// @param p_iTimeoutMs Timeout in milliseconds (0 to disable timeout)
/// @param p_bSendTimeout True for send timeout, false for receive timeout
/// @return int Error code
//////////////////////////////////
int networkSetTimeout(NetworkSocket *p_pSocket, int p_iTimeoutMs, bool p_bSendTimeout);

//////////////////////////////////
/// @brief Wait for events on socket
/// @param p_pSocket Socket handle
/// @param p_iTimeoutMs Timeout in milliseconds (-1 for infinite)
/// @return int 1 if ready, 0 if timeout, negative for error
//////////////////////////////////
int networkWaitForActivity(NetworkSocket *p_pSocket, int p_iTimeoutMs);

//////////////////////////////////
/// @brief Get last error description
/// @param p_iError Error code
/// @return const char* Error description
//////////////////////////////////
const char *networkGetErrorString(int p_iError);

//////////////////////////////////
/// @brief Check if socket is connected
/// @param p_pSocket Socket handle
/// @return bool True if connected
//////////////////////////////////
bool networkIsConnected(NetworkSocket *p_pSocket);

#endif // NETWORK_CORE_H_
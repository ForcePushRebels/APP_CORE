////////////////////////////////////////////////////////////
//  Network core implementation file for embedded systems
//  Implements the network interface defined in xNetwork.h
//  IPv4-focused, simplified API without TLS
//
// general disclosure: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 22/04/2025 - Simplified API without TLS
////////////////////////////////////////////////////////////

#include "xNetwork.h"

//////////////////////////////////
/// Core API Implementation
//////////////////////////////////

//////////////////////////////////
/// networkCreateSocket
//////////////////////////////////
NetworkSocket *networkCreateSocket(int p_iType)
{
    // Allocate a new socket structure
    NetworkSocket *l_pSocket = (NetworkSocket *)malloc(sizeof(NetworkSocket));
    if (!l_pSocket)
    {
        X_LOG_TRACE("networkCreateSocket: Memory allocation failed");
        return NULL;
    }

    // Create the actual socket
    l_pSocket->t_iSocketFd = socket(AF_INET, p_iType, 0);
    if (l_pSocket->t_iSocketFd < 0)
    {
        X_LOG_TRACE("networkCreateSocket: Socket creation failed with errno %d", errno);
        free(l_pSocket);
        return NULL;
    }

    // Enable address reuse
    int l_iOption = 1;
    setsockopt(l_pSocket->t_iSocketFd, SOL_SOCKET, SO_REUSEADDR, &l_iOption, sizeof(l_iOption));

    // Enable broadcast for UDP sockets
    if (p_iType == NETWORK_SOCK_UDP)
    {
        int l_iBroadcastOpt = 1;
        if (setsockopt(l_pSocket->t_iSocketFd, SOL_SOCKET, SO_BROADCAST,
                       &l_iBroadcastOpt, sizeof(l_iBroadcastOpt)) < 0)
        {
            X_LOG_TRACE("networkCreateSocket: Failed to set broadcast option with errno %d", errno);
            // Continue anyway, this is not fatal
        }
        else
        {
            X_LOG_TRACE("networkCreateSocket: Broadcast option enabled for UDP socket");
        }
    }

    // Set socket properties
    l_pSocket->t_iType = p_iType;
    l_pSocket->t_bConnected = false;

    // Initialize the mutex for thread safety
    mutexCreate(&l_pSocket->t_Mutex);

    X_LOG_TRACE("networkCreateSocket: Socket created successfully");
    return l_pSocket;
}

//////////////////////////////////
/// networkMakeAddress
//////////////////////////////////
NetworkAddress networkMakeAddress(const char *p_ptcAddress, unsigned short p_usPort)
{
    NetworkAddress l_tAddress;
    memset(&l_tAddress, 0, sizeof(NetworkAddress));

    // Check pointer parameters with assertions
    X_ASSERT(p_ptcAddress != NULL);

    // Replace non-pointer assertions with condition checks
    if (p_usPort <= 0 || strlen(p_ptcAddress) == 0)
    {
        // Return empty address on invalid parameters
        return l_tAddress;
    }

    l_tAddress.t_usPort = p_usPort; // Set port

    // Validate IPv4 address format
    struct in_addr l_tAddr;
    if (inet_pton(AF_INET, p_ptcAddress, &l_tAddr) == 1)
    {
        // Direct memory copy instead of strncpy for fixed-size fields
        memcpy(l_tAddress.t_cAddress, p_ptcAddress,
               (strlen(p_ptcAddress) < INET_ADDRSTRLEN) ? strlen(p_ptcAddress) : INET_ADDRSTRLEN - 1);
    }

    return l_tAddress;
}

//////////////////////////////////
/// networkBind
//////////////////////////////////
int networkBind(NetworkSocket *p_ptSocket, const NetworkAddress *p_pAddress)
{
    // Check pointers
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
    {
        X_LOG_TRACE("networkBind: Invalid socket");
        return NETWORK_INVALID_PARAM;
    }

    if (!p_pAddress)
    {
        X_LOG_TRACE("networkBind: Invalid address");
        return NETWORK_INVALID_PARAM;
    }

    struct sockaddr_in l_tAddr;
    memset(&l_tAddr, 0, sizeof(l_tAddr));
    l_tAddr.sin_family = AF_INET;
    l_tAddr.sin_port = HOST_TO_NET_SHORT(p_pAddress->t_usPort);

    if (strlen(p_pAddress->t_cAddress) > 0)
    {
        if (inet_pton(AF_INET, p_pAddress->t_cAddress, &l_tAddr.sin_addr) <= 0)
            return NETWORK_INVALID_PARAM;
    }
    else
    {
        l_tAddr.sin_addr.s_addr = INADDR_ANY;
    }

    if (bind(p_ptSocket->t_iSocketFd, (struct sockaddr *)&l_tAddr, sizeof(l_tAddr)) < 0)
    {
        X_LOG_TRACE("networkBind: Bind failed with error %d", errno);
        return NETWORK_ERROR;
    }

    return NETWORK_OK;
}

//////////////////////////////////
/// networkListen
//////////////////////////////////
int networkListen(NetworkSocket *p_ptSocket, int p_iBacklog)
{
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
    {
        X_LOG_TRACE("networkBind: Invalid socket");
        return NETWORK_INVALID_PARAM;
    }

    int l_iBacklog = (p_iBacklog > 0) ? p_iBacklog : NETWORK_MAX_PENDING;

    if (listen(p_ptSocket->t_iSocketFd, l_iBacklog) < 0)
    {
        X_LOG_TRACE("networkListen: Listen failed with error %d", errno);
        return NETWORK_ERROR;
    }

    return NETWORK_OK;
}

//////////////////////////////////
/// networkAccept
//////////////////////////////////
NetworkSocket *networkAccept(NetworkSocket *p_ptSocket, NetworkAddress *p_pClientAddress)
{
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
    {
        X_LOG_TRACE("networkAccept: Invalid socket");
        return NULL;
    }

    X_LOG_TRACE("networkAccept: Accepting new connection on socket %d", p_ptSocket->t_iSocketFd);

    struct sockaddr_in l_tClientAddr;
    socklen_t l_iAddrLen = sizeof(l_tClientAddr);

    int l_iClientFd = accept(p_ptSocket->t_iSocketFd, (struct sockaddr *)&l_tClientAddr, &l_iAddrLen);
    if (l_iClientFd < 0)
    {
        X_LOG_TRACE("networkAccept: accept() failed with error %d", errno);
        return NULL;
    }

    // Store client address if requested
    if (p_pClientAddress)
    {
        inet_ntop(AF_INET, &l_tClientAddr.sin_addr, p_pClientAddress->t_cAddress, INET_ADDRSTRLEN);
        p_pClientAddress->t_usPort = NET_TO_HOST_SHORT(l_tClientAddr.sin_port);
    }

    X_LOG_TRACE("networkAccept: Connection accepted from %s:%d",
                p_pClientAddress ? p_pClientAddress->t_cAddress : "unknown",
                p_pClientAddress ? p_pClientAddress->t_usPort : 0);

    // Allocate socket for the new connection
    NetworkSocket *l_pClientSocket = (NetworkSocket *)malloc(sizeof(NetworkSocket));
    if (!l_pClientSocket)
    {
        X_LOG_TRACE("networkAccept: Failed to allocate memory for client socket");
        close(l_iClientFd);
        return NULL;
    }

    // Configure client socket
    l_pClientSocket->t_iSocketFd = l_iClientFd;
    l_pClientSocket->t_iType = NETWORK_SOCK_TCP;
    l_pClientSocket->t_bConnected = true;

    // Initialize the mutex for thread safety
    mutexCreate(&l_pClientSocket->t_Mutex);

    X_LOG_TRACE("networkAccept: Connection successful");
    return l_pClientSocket;
}

//////////////////////////////////
/// networkConnect
//////////////////////////////////
int networkConnect(NetworkSocket *p_ptSocket, const NetworkAddress *p_pAddress)
{
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
        return NETWORK_INVALID_PARAM;

    if (!p_pAddress)
        return NETWORK_INVALID_PARAM;

    struct sockaddr_in l_tAddr;
    memset(&l_tAddr, 0, sizeof(l_tAddr));
    l_tAddr.sin_family = AF_INET;
    l_tAddr.sin_port = HOST_TO_NET_SHORT(p_pAddress->t_usPort);

    if (inet_pton(AF_INET, p_pAddress->t_cAddress, &l_tAddr.sin_addr) <= 0)
        return NETWORK_INVALID_PARAM;

    X_LOG_TRACE("networkConnect: Connecting to %s:%d", p_pAddress->t_cAddress, p_pAddress->t_usPort);
    if (connect(p_ptSocket->t_iSocketFd, (struct sockaddr *)&l_tAddr, sizeof(l_tAddr)) < 0)
    {
        X_LOG_TRACE("networkConnect: TCP connection failed with error %d", errno);
        p_ptSocket->t_bConnected = false;
        return NETWORK_ERROR;
    }

    p_ptSocket->t_bConnected = true;
    X_LOG_TRACE("networkConnect: Connection successful");
    return NETWORK_OK;
}

//////////////////////////////////
/// networkSend
//////////////////////////////////
int networkSend(NetworkSocket *p_ptSocket, const void *p_pBuffer, unsigned long p_ulSize)
{
    int l_iReturn;

    // Validate essential parameters
    if (!p_ptSocket || !p_pBuffer)
    {
        X_LOG_TRACE("networkSend: Invalid parameters");
        return NETWORK_INVALID_PARAM;
    }

    // Early return for zero size
    if (p_ulSize == 0)
    {
        X_LOG_TRACE("networkSend: Zero size");
        return 0;
    }

    // Lock mutex before accessing socket
    mutexLock(&p_ptSocket->t_Mutex);

    l_iReturn = send(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, 0);
    if (l_iReturn < 0)
    {
        X_LOG_TRACE("networkSend: Send failed with error code %d", errno);
        l_iReturn = NETWORK_ERROR;
    }
    else
    {
        X_LOG_TRACE("networkSend: Send successful, sent %d bytes", l_iReturn);
    }

    // Unlock mutex after socket operation
    mutexUnlock(&p_ptSocket->t_Mutex);

    return l_iReturn;
}

//////////////////////////////////
/// networkReceive
//////////////////////////////////
int networkReceive(NetworkSocket *p_ptSocket, void *p_pBuffer, unsigned long p_ulSize)
{
    int l_iReturn;

    // Validate essential parameters
    if (!p_ptSocket || !p_pBuffer)
    {
        X_LOG_TRACE("networkReceive: Invalid parameters");
        return NETWORK_INVALID_PARAM;
    }

    // Early return for zero size
    if (p_ulSize == 0)
    {
        X_LOG_TRACE("networkReceive: Zero size");
        return 0;
    }

    // Lock mutex before accessing socket
    mutexLock(&p_ptSocket->t_Mutex);

    l_iReturn = recv(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, 0);
    if (l_iReturn < 0)
    {
        X_LOG_TRACE("networkReceive: Receive failed with error code %d", errno);
        l_iReturn = NETWORK_ERROR;
    }
    else if (l_iReturn == 0)
    {
        X_LOG_TRACE("networkReceive: Receive returned 0 bytes (connection closed)");
    }
    else
    {
        X_LOG_TRACE("networkReceive: Receive successful, received %d bytes", l_iReturn);
    }

    // Unlock mutex after socket operation
    mutexUnlock(&p_ptSocket->t_Mutex);

    return l_iReturn;
}

//////////////////////////////////
/// networkCloseSocket
//////////////////////////////////
int networkCloseSocket(NetworkSocket *p_ptSocket)
{
    if (!p_ptSocket)
    {
        X_LOG_TRACE("networkCloseSocket: Invalid socket");
        return NETWORK_INVALID_PARAM;
    }

    if (p_ptSocket->t_iSocketFd >= 0)
    {
        X_LOG_TRACE("networkCloseSocket: Closing socket %d", p_ptSocket->t_iSocketFd);
        close(p_ptSocket->t_iSocketFd);
        p_ptSocket->t_iSocketFd = -1;
    }

    p_ptSocket->t_bConnected = false;

    // Destroy mutex before freeing socket
    mutexDestroy(&p_ptSocket->t_Mutex);

    // Free the socket structure
    free(p_ptSocket);

    return NETWORK_OK;
}

//////////////////////////////////
/// networkSetTimeout
//////////////////////////////////
int networkSetTimeout(NetworkSocket *p_ptSocket, int p_iTimeoutMs, bool p_bSendTimeout)
{
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
        return NETWORK_INVALID_PARAM;

    struct timeval l_tTimeout;
    l_tTimeout.tv_sec = p_iTimeoutMs / 1000;
    l_tTimeout.tv_usec = (p_iTimeoutMs % 1000) * 1000;

    int l_iOptionName = p_bSendTimeout ? SO_SNDTIMEO : SO_RCVTIMEO;

    if (setsockopt(p_ptSocket->t_iSocketFd, SOL_SOCKET, l_iOptionName, &l_tTimeout, sizeof(l_tTimeout)) < 0)
        return NETWORK_ERROR;

    return NETWORK_OK;
}

//////////////////////////////////
/// networkWaitForActivity
//////////////////////////////////
int networkWaitForActivity(NetworkSocket *p_ptSocket, int p_iTimeoutMs)
{
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
    {
        X_LOG_TRACE("Invalid socket");
        return NETWORK_INVALID_PARAM;
    }

    fd_set l_tReadSet;
    FD_ZERO(&l_tReadSet);
    FD_SET(p_ptSocket->t_iSocketFd, &l_tReadSet);

    struct timeval l_tTimeout, *l_pTimeout = NULL;
    if (p_iTimeoutMs >= 0)
    {
        l_tTimeout.tv_sec = p_iTimeoutMs / 1000;
        l_tTimeout.tv_usec = (p_iTimeoutMs % 1000) * 1000;
        l_pTimeout = &l_tTimeout;
    }

    int l_ulReturn = select(p_ptSocket->t_iSocketFd + 1, &l_tReadSet, NULL, NULL, l_pTimeout);

    if (l_ulReturn < 0)
        return NETWORK_ERROR;

    if (l_ulReturn == 0)
    {
        X_LOG_TRACE("Timeout, no events");
        return NETWORK_TIMEOUT;
    }

    X_LOG_TRACE("Socket is ready for reading");
    return NETWORK_OK;
}

//////////////////////////////////
/// networkGetErrorString
//////////////////////////////////
const char *networkGetErrorString(int p_iError)
{
    switch (p_iError)
    {
    case NETWORK_OK:
        return "Success";
    case NETWORK_ERROR:
        return "General network error";
    case NETWORK_TIMEOUT:
        return "Operation timed out";
    case NETWORK_INVALID_PARAM:
        return "Invalid parameter";
    case NETWORK_ZERO_SIZE:
        return "Zero size";
    default:
        return "Unknown error";
    }
}

//////////////////////////////////
/// networkIsConnected
//////////////////////////////////
bool networkIsConnected(NetworkSocket *p_ptSocket)
{
    if (!p_ptSocket)
    {
        X_LOG_TRACE("networkIsConnected: Invalid socket");
        return false;
    }

    return p_ptSocket->t_bConnected;
}

//////////////////////////////////
/// UDP-specific API Implementation
//////////////////////////////////

//////////////////////////////////
/// networkSendTo
//////////////////////////////////
int networkSendTo(NetworkSocket *p_ptSocket, const void *p_pBuffer, unsigned long p_ulSize, const NetworkAddress *p_pAddress)
{
    int l_iReturn;

    // Validate essential parameters
    if (!p_ptSocket || !p_pBuffer || !p_pAddress)
    {
        X_LOG_TRACE("networkSendTo: Invalid parameters");
        return NETWORK_INVALID_PARAM;
    }

    // Check if socket is UDP type
    if (p_ptSocket->t_iType != NETWORK_SOCK_UDP)
    {
        X_LOG_TRACE("networkSendTo: Not a UDP socket");
        return NETWORK_INVALID_PARAM;
    }

    // Early return for zero size
    if (p_ulSize == 0)
    {
        X_LOG_TRACE("networkSendTo: Zero size");
        return 0;
    }

    // Set up destination address
    struct sockaddr_in l_tAddr;
    memset(&l_tAddr, 0, sizeof(l_tAddr));
    l_tAddr.sin_family = AF_INET;
    l_tAddr.sin_port = HOST_TO_NET_SHORT(p_pAddress->t_usPort);

    if (inet_pton(AF_INET, p_pAddress->t_cAddress, &l_tAddr.sin_addr) <= 0)
    {
        X_LOG_TRACE("networkSendTo: Invalid address");
        return NETWORK_INVALID_PARAM;
    }

    // Lock mutex before accessing socket
    mutexLock(&p_ptSocket->t_Mutex);

    X_LOG_TRACE("networkSendTo: Sending datagram to %s:%d, %lu bytes",
                p_pAddress->t_cAddress, p_pAddress->t_usPort, p_ulSize);

    l_iReturn = sendto(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, 0,
                       (struct sockaddr *)&l_tAddr, sizeof(l_tAddr));

    if (l_iReturn < 0)
    {
        X_LOG_TRACE("networkSendTo: Send failed with error code %d", errno);
        l_iReturn = NETWORK_ERROR;
    }
    else
    {
        X_LOG_TRACE("networkSendTo: Send successful, sent %d bytes", l_iReturn);
    }

    // Unlock mutex after socket operation
    mutexUnlock(&p_ptSocket->t_Mutex);

    return l_iReturn;
}

//////////////////////////////////
/// networkReceiveFrom
//////////////////////////////////
int networkReceiveFrom(NetworkSocket *p_ptSocket, void *p_pBuffer, unsigned long p_ulSize, NetworkAddress *p_pAddress)
{
    int l_iReturn;

    if (!p_ptSocket || !p_pBuffer)
        return NETWORK_INVALID_PARAM;

    // Check if socket is UDP type
    if (p_ptSocket->t_iType != NETWORK_SOCK_UDP)
    {
        X_LOG_TRACE("networkReceiveFrom: Not a UDP socket");
        return NETWORK_INVALID_PARAM;
    }

    // Early return for zero size
    if (p_ulSize == 0)
        return 0;

    struct sockaddr_in l_tSenderAddr;
    socklen_t l_iAddrLen = sizeof(l_tSenderAddr);

    // Lock mutex before accessing socket
    mutexLock(&p_ptSocket->t_Mutex);

    X_LOG_TRACE("networkReceiveFrom: Receiving datagram on socket %d, buffer size %lu",
                p_ptSocket->t_iSocketFd, p_ulSize);

    l_iReturn = recvfrom(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, 0,
                         (struct sockaddr *)&l_tSenderAddr, &l_iAddrLen);

    // get last errno error
    int l_iErrnoReturn = errno;
    X_LOG_TRACE("networkReceiveFrom: Last errno error: %d", l_iErrnoReturn);

    if (l_iReturn < 0 && errno != EAGAIN && errno != EWOULDBLOCK)
    {
        l_iReturn = NETWORK_ERROR;
    }
    else if (l_iReturn == 0)
    {
        X_LOG_TRACE("networkReceiveFrom: Receive returned 0 bytes");
    }
    else
    {
        X_LOG_TRACE("networkReceiveFrom: Receive successful, received %d bytes", l_iReturn);

        // Store sender address if requested
        if (p_pAddress)
        {
            inet_ntop(AF_INET, &l_tSenderAddr.sin_addr, p_pAddress->t_cAddress, INET_ADDRSTRLEN);
            p_pAddress->t_usPort = NET_TO_HOST_SHORT(l_tSenderAddr.sin_port);
            X_LOG_TRACE("networkReceiveFrom: Datagram from %s:%d",
                        p_pAddress->t_cAddress, p_pAddress->t_usPort);
        }
    }

    // Unlock mutex after socket operation
    mutexUnlock(&p_ptSocket->t_Mutex);

    return l_iReturn;
}

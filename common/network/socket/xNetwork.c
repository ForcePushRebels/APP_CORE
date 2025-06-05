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

    // TCP-specific optimizations
    if (p_iType == NETWORK_SOCK_TCP)
    {
        // Disable Nagle's algorithm for low latency
        int l_iNodelay = 1;
        if (setsockopt(l_pSocket->t_iSocketFd, IPPROTO_TCP, TCP_NODELAY, &l_iNodelay, sizeof(l_iNodelay)) < 0)
        {
            X_LOG_TRACE("networkCreateSocket: Failed to set TCP_NODELAY with errno %d", errno);
            // Continue anyway, this is not fatal
        }
        else
        {
            X_LOG_TRACE("networkCreateSocket: TCP_NODELAY enabled for TCP socket");
        }

        // Increase send buffer size for better throughput
        int l_iSendBufSize = NETWORK_OPT_TCP_SEND_BUFFER_SIZE;
        if (setsockopt(l_pSocket->t_iSocketFd, SOL_SOCKET, SO_SNDBUF, &l_iSendBufSize, sizeof(l_iSendBufSize)) < 0)
        {
            X_LOG_TRACE("networkCreateSocket: Failed to set send buffer size with errno %d", errno);
            // Continue anyway, this is not fatal
        }
        else
        {
            X_LOG_TRACE("networkCreateSocket: Send buffer size set to %d bytes", l_iSendBufSize);
        }

        // Increase receive buffer size for better throughput
        int l_iRecvBufSize = NETWORK_OPT_TCP_RECV_BUFFER_SIZE;
        if (setsockopt(l_pSocket->t_iSocketFd, SOL_SOCKET, SO_RCVBUF, &l_iRecvBufSize, sizeof(l_iRecvBufSize)) < 0)
        {
            X_LOG_TRACE("networkCreateSocket: Failed to set receive buffer size with errno %d", errno);
            // Continue anyway, this is not fatal
        }
        else
        {
            X_LOG_TRACE("networkCreateSocket: Receive buffer size set to %d bytes", l_iRecvBufSize);
        }
    }

    // Enable broadcast for UDP sockets
    if (p_iType == NETWORK_SOCK_UDP)
    {
        int l_iBroadcastOpt = 1;
        if (setsockopt(l_pSocket->t_iSocketFd, SOL_SOCKET, SO_BROADCAST, &l_iBroadcastOpt, sizeof(l_iBroadcastOpt)) < 0)
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
        memcpy(l_tAddress.t_cAddress,
               p_ptcAddress,
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

    // Apply TCP optimizations to accepted client socket
    // Disable Nagle's algorithm for low latency
    int l_iNodelay = 1;
    if (setsockopt(l_iClientFd, IPPROTO_TCP, TCP_NODELAY, &l_iNodelay, sizeof(l_iNodelay)) < 0)
    {
        X_LOG_TRACE("networkAccept: Failed to set TCP_NODELAY on client socket");
        // Continue anyway, this is not fatal
    }

    // Increase send buffer size for better throughput
    int l_iSendBufSize = NETWORK_OPT_TCP_SEND_BUFFER_SIZE;
    if (setsockopt(l_iClientFd, SOL_SOCKET, SO_SNDBUF, &l_iSendBufSize, sizeof(l_iSendBufSize)) < 0)
    {
        X_LOG_TRACE("networkAccept: Failed to set send buffer size on client socket");
        // Continue anyway, this is not fatal
    }

    // Increase receive buffer size for better throughput
    int l_iRecvBufSize = NETWORK_OPT_TCP_RECV_BUFFER_SIZE;
    if (setsockopt(l_iClientFd, SOL_SOCKET, SO_RCVBUF, &l_iRecvBufSize, sizeof(l_iRecvBufSize)) < 0)
    {
        X_LOG_TRACE("networkAccept: Failed to set receive buffer size on client socket");
    }

    X_LOG_TRACE("networkAccept: Connection successful (mutex-free)");
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
    // Fast parameter validation - single check with early return
    if (!p_ptSocket || !p_pBuffer || p_ptSocket->t_iSocketFd < 0 || p_ulSize == 0)
    {
        return (p_ulSize == 0) ? 0 : NETWORK_INVALID_PARAM;
    }

    int l_iReturn = send(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, MSG_NOSIGNAL);
    
    // Simplified error handling
    return (l_iReturn < 0) ? NETWORK_ERROR : l_iReturn;
}

//////////////////////////////////
/// networkReceive 
//////////////////////////////////
int networkReceive(NetworkSocket *p_ptSocket, void *p_pBuffer, unsigned long p_ulSize)
{
    // Fast parameter validation - single check with early return
    if (!p_ptSocket || !p_pBuffer || p_ptSocket->t_iSocketFd < 0 || p_ulSize == 0)
    {
        return (p_ulSize == 0) ? 0 : NETWORK_INVALID_PARAM;
    }

    int l_iReturn = recv(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, 0);
    
    // Simplified error handling
    return (l_iReturn < 0) ? NETWORK_ERROR : l_iReturn;
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
    // Fast parameter validation
    if (!p_ptSocket || p_ptSocket->t_iSocketFd < 0)
    {
        return NETWORK_INVALID_PARAM;
    }

#ifdef NETWORK_HAS_EPOLL
    // Use epoll for better performance on Linux
    int l_iEpollFd = epoll_create1(EPOLL_CLOEXEC);
    if (l_iEpollFd < 0)
    {
        return NETWORK_ERROR;
    }

    struct epoll_event l_tEvent;
    l_tEvent.events = EPOLLIN | EPOLLET; // Edge-triggered for better performance
    l_tEvent.data.fd = p_ptSocket->t_iSocketFd;

    if (epoll_ctl(l_iEpollFd, EPOLL_CTL_ADD, p_ptSocket->t_iSocketFd, &l_tEvent) < 0)
    {
        close(l_iEpollFd);
        return NETWORK_ERROR;
    }

    struct epoll_event l_tResult;
    int l_iNumEvents = epoll_wait(l_iEpollFd, &l_tResult, 1, p_iTimeoutMs);
    
    close(l_iEpollFd);
    
    return (l_iNumEvents < 0) ? NETWORK_ERROR : 
           (l_iNumEvents == 0) ? NETWORK_TIMEOUT : NETWORK_OK;

#else
    // Fallback to select on non-Linux systems
    fd_set l_tReadSet;
    FD_ZERO(&l_tReadSet);
    FD_SET(p_ptSocket->t_iSocketFd, &l_tReadSet);

    struct timeval *l_pTimeout = NULL;
    struct timeval l_tTimeout;
    if (p_iTimeoutMs >= 0)
    {
        l_tTimeout.tv_sec = p_iTimeoutMs / 1000;
        l_tTimeout.tv_usec = (p_iTimeoutMs % 1000) * 1000;
        l_pTimeout = &l_tTimeout;
    }

    int l_iResult = select(p_ptSocket->t_iSocketFd + 1, &l_tReadSet, NULL, NULL, l_pTimeout);
    
    return (l_iResult < 0) ? NETWORK_ERROR : 
           (l_iResult == 0) ? NETWORK_TIMEOUT : NETWORK_OK;
#endif
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
    // Fast parameter validation with early returns
    if (!p_ptSocket || !p_pBuffer || !p_pAddress || 
        p_ptSocket->t_iSocketFd < 0 || p_ptSocket->t_iType != NETWORK_SOCK_UDP)
    {
        return NETWORK_INVALID_PARAM;
    }

    if (p_ulSize == 0)
    {
        return 0;
    }

    // Pre-build destination address structure (stack allocation, faster than malloc)
    struct sockaddr_in l_tAddr = {
        .sin_family = AF_INET,
        .sin_port = HOST_TO_NET_SHORT(p_pAddress->t_usPort)
    };

    // Validate and convert address in one step
    if (inet_pton(AF_INET, p_pAddress->t_cAddress, &l_tAddr.sin_addr) <= 0)
    {
        return NETWORK_INVALID_PARAM;
    }

    // Direct sendto without mutex - UDP operations are atomic at kernel level
    int l_iReturn = sendto(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, MSG_NOSIGNAL, 
                          (struct sockaddr *)&l_tAddr, sizeof(l_tAddr));

    return (l_iReturn < 0) ? NETWORK_ERROR : l_iReturn;
}

//////////////////////////////////
/// networkReceiveFrom 
//////////////////////////////////
int networkReceiveFrom(NetworkSocket *p_ptSocket, void *p_pBuffer, unsigned long p_ulSize, NetworkAddress *p_pAddress)
{
    // Fast parameter validation with early returns
    if (!p_ptSocket || !p_pBuffer || p_ptSocket->t_iSocketFd < 0 || 
        p_ptSocket->t_iType != NETWORK_SOCK_UDP)
    {
        return NETWORK_INVALID_PARAM;
    }

    if (p_ulSize == 0)
    {
        return 0;
    }

    struct sockaddr_in l_tSenderAddr;
    socklen_t l_iAddrLen = sizeof(l_tSenderAddr);

    // Direct recvfrom without mutex - UDP operations are atomic at kernel level
    int l_iReturn = recvfrom(p_ptSocket->t_iSocketFd, p_pBuffer, p_ulSize, 0, 
                            (struct sockaddr *)&l_tSenderAddr, &l_iAddrLen);
    int l_iErrno = errno; // Capture errno immediately after syscall

    // Handle errors and success outside critical section
    if (l_iReturn < 0)
    {
        // Only treat real errors as NETWORK_ERROR, not EAGAIN/EWOULDBLOCK
        return (l_iErrno != EAGAIN && l_iErrno != EWOULDBLOCK) ? NETWORK_ERROR : l_iReturn;
    }

    // Store sender address if requested and we received data
    if (l_iReturn > 0 && p_pAddress)
    {
        inet_ntop(AF_INET, &l_tSenderAddr.sin_addr, p_pAddress->t_cAddress, INET_ADDRSTRLEN);
        p_pAddress->t_usPort = NET_TO_HOST_SHORT(l_tSenderAddr.sin_port);
    }

    return l_iReturn;
}

#ifdef NETWORK_HAS_EPOLL
//////////////////////////////////
/// networkWaitForMultipleActivity 
//////////////////////////////////
int networkWaitForMultipleActivity(NetworkSocket **p_pptSockets, int p_iNumSockets, 
                                   int *p_piReadySocket, int p_iTimeoutMs)
{
    // Parameter validation
    if (!p_pptSockets || p_iNumSockets <= 0 || p_iNumSockets > NETWORK_MAX_SOCKETS || !p_piReadySocket)
    {
        return NETWORK_INVALID_PARAM;
    }

    *p_piReadySocket = -1; // Initialize to "no socket ready"

    // Create epoll instance
    int l_iEpollFd = epoll_create1(EPOLL_CLOEXEC);
    if (l_iEpollFd < 0)
    {
        return NETWORK_ERROR;
    }

    // Add all sockets to epoll
    for (int i = 0; i < p_iNumSockets; i++)
    {
        if (!p_pptSockets[i] || p_pptSockets[i]->t_iSocketFd < 0)
        {
            continue; // Skip invalid sockets
        }

        struct epoll_event l_tEvent;
        l_tEvent.events = EPOLLIN | EPOLLET; // Edge-triggered for performance
        l_tEvent.data.u32 = i; // Store socket index for quick lookup

        if (epoll_ctl(l_iEpollFd, EPOLL_CTL_ADD, p_pptSockets[i]->t_iSocketFd, &l_tEvent) < 0)
        {
            // Continue with other sockets if one fails
            continue;
        }
    }

    // Wait for events
    struct epoll_event l_atResults[NETWORK_MAX_SOCKETS];
    int l_iNumEvents = epoll_wait(l_iEpollFd, l_atResults, p_iNumSockets, p_iTimeoutMs);
    
    close(l_iEpollFd);

    if (l_iNumEvents < 0)
    {
        return NETWORK_ERROR;
    }
    
    if (l_iNumEvents == 0)
    {
        return NETWORK_TIMEOUT;
    }

    // Return the first ready socket (in practice, this is usually sufficient)
    *p_piReadySocket = l_atResults[0].data.u32;
    return NETWORK_OK;
}
#endif

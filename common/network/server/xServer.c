////////////////////////////////////////////////////////////
//  xServer - Simple & Safe TCP Server Implementation
//  Zero dynamic allocation, runtime-safe design
//  Thread-per-client architecture with fixed-size pools
//
// general disclosure: copy or share the file is forbidden
// Written : 19/01/2025 - Clean rewrite
////////////////////////////////////////////////////////////

#include "xServer.h"
#include "handleNetworkMessage.h"
#include "xLog.h"
#include "xAssert.h"
#include <sys/epoll.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

//-----------------------------------------------------------------------------
// Server State Structure (single instance, no allocation)
//-----------------------------------------------------------------------------

typedef struct {
    // Configuration
    ServerConfig t_sConfig;
    bool t_bInitialized;
    volatile bool t_bRunning;
    
    // Network
    NetworkSocket *t_ptListenSocket;
    int t_iEpollFd;
    
    // TLS engine
    xTlsEngine_t *t_ptTlsEngine;
    
    // Threading
    xOsTaskCtx t_tServerTask;
    
    // Client pool (fixed-size, no allocation)
    clientCtx t_atClients[SERVER_MAX_CLIENTS];
    xOsMutexCtx t_tClientsMutex;
    int t_iClientCount;
    
    // Broadcast buffer (shared, no allocation per send)
    uint8_t t_aucBroadcastBuffer[PROTOCOL_MAX_FRAME_SIZE];
    
} ServerInstance;

//-----------------------------------------------------------------------------
// Global Server Instance (single, statically allocated)
//-----------------------------------------------------------------------------

static ServerInstance s_tServer = {0};

//-----------------------------------------------------------------------------
// Forward Declarations
//-----------------------------------------------------------------------------

static void *serverMainThread(void *p_pvArg);
static void *clientThread(void *p_pvArg);
static int handleNewConnection(NetworkSocket *p_ptClientSocket, const NetworkAddress *p_ptClientAddr);
static clientCtx *findFreeClientSlot(void);
static clientCtx *findClientById(ClientID p_tClientId);
static void clientCleanup(clientCtx *p_ptClient);
static int clientSendData(clientCtx *p_ptClient, const void *p_pvData, int p_iSize);
static int clientReceiveMessage(clientCtx *p_ptClient);
static void cleanupDisconnectedClients(void);

//-----------------------------------------------------------------------------
// Server Core Implementation
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// xServerInit
///////////////////////////////////////////
int xServerInit(void)
{
    if (s_tServer.t_bInitialized)
    {
        X_LOG_WARN("Server already initialized");
        return SERVER_OK;
    }

    // Clear server structure
    memset(&s_tServer, 0, sizeof(ServerInstance));
    
    // Initialize client pool
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        s_tServer.t_atClients[i].t_tId = INVALID_CLIENT_ID; // Mark as unused
    }
    
    // Initialize clients mutex
    if (mutexCreate(&s_tServer.t_tClientsMutex) != MUTEX_OK)
    {
        X_LOG_ERROR("Failed to create clients mutex");
        return SERVER_ERROR;
    }
    
    // Initialize message handler system
    initMessageHandlerSystem();
    
    // Create epoll instance
    s_tServer.t_iEpollFd = epoll_create1(EPOLL_CLOEXEC);
    if (s_tServer.t_iEpollFd < 0)
    {
        X_LOG_ERROR("Failed to create epoll instance: %s", strerror(errno));
        mutexDestroy(&s_tServer.t_tClientsMutex);
        return SERVER_ERROR;
    }
    
    // Set default configuration
    s_tServer.t_sConfig = xServerCreateDefaultConfig();
    s_tServer.t_bInitialized = true;
    
    // Ignore SIGPIPE (handle EPIPE instead)
    signal(SIGPIPE, SIG_IGN);
    
    X_LOG_INFO("Server initialized successfully");
    return SERVER_OK;
}

///////////////////////////////////////////
/// xServerConfigure
///////////////////////////////////////////
int xServerConfigure(const ServerConfig *p_ptConfig)
{
    if (!s_tServer.t_bInitialized)
    {
        X_LOG_ERROR("Server not initialized");
        return SERVER_NOT_INITIALIZED;
    }
    
    if (p_ptConfig == NULL)
    {
        X_LOG_ERROR("Invalid configuration parameter");
        return SERVER_INVALID_PARAM;
    }
    
    if (s_tServer.t_bRunning)
    {
        X_LOG_ERROR("Cannot configure running server");
        return SERVER_ALREADY_RUNNING;
    }
    
    // Copy configuration
    memcpy(&s_tServer.t_sConfig, p_ptConfig, sizeof(ServerConfig));
    
    X_LOG_INFO("Server configured for port %d (TLS: %s)", 
              p_ptConfig->t_usPort, p_ptConfig->t_bUseTls ? "enabled" : "disabled");
    return SERVER_OK;
}

///////////////////////////////////////////
/// xServerStart
///////////////////////////////////////////
int xServerStart(void)
{
    if (!s_tServer.t_bInitialized)
    {
        X_LOG_ERROR("Server not initialized");
        return SERVER_NOT_INITIALIZED;
    }
    
    if (s_tServer.t_bRunning)
    {
        X_LOG_WARN("Server already running");
        return SERVER_OK;
    }
    
    // Initialize TLS engine if enabled
    if (s_tServer.t_sConfig.t_bUseTls)
    {
        int l_iTlsResult = tlsEngineCreate(&s_tServer.t_ptTlsEngine,
                                          TLS_MODE_SERVER,
                                          s_tServer.t_sConfig.t_acCertFile,
                                          s_tServer.t_sConfig.t_acKeyFile,
                                          s_tServer.t_sConfig.t_acCaDir,
                                          true);
        if (l_iTlsResult != CERT_OK)
        {
            X_LOG_ERROR("Failed to initialize TLS engine: 0x%x", l_iTlsResult);
            return SERVER_TLS_ERROR;
        }
        X_LOG_INFO("TLS engine initialized successfully");
    }
    
    // Create and bind listening socket
    s_tServer.t_ptListenSocket = networkCreateSocket(NETWORK_SOCK_TCP);
    if (s_tServer.t_ptListenSocket == NULL)
    {
        X_LOG_ERROR("Failed to create listening socket");
        return SERVER_SOCKET_ERROR;
    }
    
    NetworkAddress l_tServerAddr = networkMakeAddress("0.0.0.0", s_tServer.t_sConfig.t_usPort);
    int l_iResult = networkBind(s_tServer.t_ptListenSocket, &l_tServerAddr);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_ERROR("Failed to bind socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(s_tServer.t_ptListenSocket);
        s_tServer.t_ptListenSocket = NULL;
        return SERVER_SOCKET_ERROR;
    }
    
    // Start listening
    l_iResult = networkListen(s_tServer.t_ptListenSocket, 5);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_ERROR("Failed to listen: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(s_tServer.t_ptListenSocket);
        s_tServer.t_ptListenSocket = NULL;
        return SERVER_SOCKET_ERROR;
    }
    
    // Add listening socket to epoll
    struct epoll_event l_tEvent;
    l_tEvent.events = EPOLLIN;
    l_tEvent.data.ptr = &s_tServer; // Mark as server socket
    
    if (epoll_ctl(s_tServer.t_iEpollFd, EPOLL_CTL_ADD, 
                  s_tServer.t_ptListenSocket->t_iSocketFd, &l_tEvent) < 0)
    {
        X_LOG_ERROR("Failed to add server socket to epoll: %s", strerror(errno));
        networkCloseSocket(s_tServer.t_ptListenSocket);
        s_tServer.t_ptListenSocket = NULL;
        return SERVER_ERROR;
    }
    
    // Start server running
    s_tServer.t_bRunning = true;
    
    // Create server thread
    if (osTaskInit(&s_tServer.t_tServerTask) != OS_TASK_SUCCESS)
    {
        X_LOG_ERROR("Failed to initialize server task");
        s_tServer.t_bRunning = false;
        return SERVER_THREAD_ERROR;
    }
    
    s_tServer.t_tServerTask.t_ptTask = serverMainThread;
    s_tServer.t_tServerTask.t_ptTaskArg = &s_tServer;
    strncpy(s_tServer.t_tServerTask.t_acTaskName, "xServer", sizeof(s_tServer.t_tServerTask.t_acTaskName) - 1);
    
    if (osTaskCreate(&s_tServer.t_tServerTask) != OS_TASK_SUCCESS)
    {
        X_LOG_ERROR("Failed to create server thread");
        s_tServer.t_bRunning = false;
        return SERVER_THREAD_ERROR;
    }
    
    X_LOG_INFO("Server started on port %d", s_tServer.t_sConfig.t_usPort);
    return SERVER_OK;
}

///////////////////////////////////////////
/// xServerStop
///////////////////////////////////////////
int xServerStop(void)
{
    if (!s_tServer.t_bInitialized)
    {
        X_LOG_ERROR("Server not initialized");
        return SERVER_NOT_INITIALIZED;
    }
    
    if (!s_tServer.t_bRunning)
    {
        X_LOG_WARN("Server not running");
        return SERVER_NOT_RUNNING;
    }
    
    X_LOG_INFO("Stopping server...");
    
    // Signal server to stop
    s_tServer.t_bRunning = false;
    
    // Wait for server thread to finish
    osTaskWait(&s_tServer.t_tServerTask, NULL);
    
    // Stop all client threads
    mutexLock(&s_tServer.t_tClientsMutex);
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        clientCtx *l_ptClient = &s_tServer.t_atClients[i];
        if (l_ptClient->t_tId != INVALID_CLIENT_ID)
        {
            l_ptClient->t_bShutdown = true;
            mutexUnlock(&s_tServer.t_tClientsMutex); // Unlock before waiting
            osTaskWait(&l_ptClient->t_tClientTask, NULL);
            mutexLock(&s_tServer.t_tClientsMutex); // Lock again for next iteration
            
            // Check if client is still present (might have been cleaned up automatically)
            if (l_ptClient->t_tId != INVALID_CLIENT_ID)
            {
                clientCleanup(l_ptClient);
                s_tServer.t_iClientCount--;
            }
        }
    }
    // Ensure count is reset to 0
    s_tServer.t_iClientCount = 0;
    mutexUnlock(&s_tServer.t_tClientsMutex);
    
    // Close listening socket
    if (s_tServer.t_ptListenSocket != NULL)
    {
        epoll_ctl(s_tServer.t_iEpollFd, EPOLL_CTL_DEL, 
                  s_tServer.t_ptListenSocket->t_iSocketFd, NULL);
        networkCloseSocket(s_tServer.t_ptListenSocket);
        s_tServer.t_ptListenSocket = NULL;
    }
    
    X_LOG_INFO("Server stopped");
    return SERVER_OK;
}

///////////////////////////////////////////
/// xServerCleanup
///////////////////////////////////////////
void xServerCleanup(void)
{
    if (!s_tServer.t_bInitialized)
    {
        return;
    }
    
    // Stop server if running
    if (s_tServer.t_bRunning)
    {
        xServerStop();
    }
    
    // Cleanup TLS engine
    if (s_tServer.t_ptTlsEngine != NULL)
    {
        tlsEngineDestroy(s_tServer.t_ptTlsEngine);
        s_tServer.t_ptTlsEngine = NULL;
    }
    
    // Cleanup message handler system
    cleanupMessageHandlerSystem();
    
    // Destroy mutex
    mutexDestroy(&s_tServer.t_tClientsMutex);
    
    // Close epoll
    if (s_tServer.t_iEpollFd >= 0)
    {
        close(s_tServer.t_iEpollFd);
        s_tServer.t_iEpollFd = -1;
    }
    
    // Reset state
    memset(&s_tServer, 0, sizeof(ServerInstance));
    
    X_LOG_INFO("Server cleanup completed");
}

//-----------------------------------------------------------------------------
// Server Thread Implementation
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// serverMainThread
///////////////////////////////////////////
static void *serverMainThread(void *p_pvArg)
{
    ServerInstance *l_ptServer = (ServerInstance *)p_pvArg;
    struct epoll_event l_atEvents[SERVER_MAX_CLIENTS + 1];
    
    X_LOG_INFO("Server thread started");
    
    while (l_ptServer->t_bRunning)
    {
        int l_iNumEvents = epoll_wait(l_ptServer->t_iEpollFd, l_atEvents, 
                                     SERVER_MAX_CLIENTS + 1, SERVER_EPOLL_TIMEOUT);
        
        if (l_iNumEvents < 0)
        {
            if (errno == EINTR)
            {
                continue; // Interrupted by signal
            }
            X_LOG_ERROR("epoll_wait failed: %s", strerror(errno));
            break;
        }
        
        if (l_iNumEvents == 0)
        {
            // Timeout - perform maintenance tasks
            cleanupDisconnectedClients();
            continue; // Check running flag
        }
        
        // Process events
        for (int i = 0; i < l_iNumEvents; i++)
        {
            struct epoll_event *l_ptEvent = &l_atEvents[i];
            
            if (l_ptEvent->data.ptr == l_ptServer)
            {
                // New connection on server socket
                NetworkAddress l_tClientAddr;
                NetworkSocket *l_ptClientSocket = networkAccept(l_ptServer->t_ptListenSocket, &l_tClientAddr);
                
                if (l_ptClientSocket != NULL)
                {
                    int l_iResult = handleNewConnection(l_ptClientSocket, &l_tClientAddr);
                    if (l_iResult != SERVER_OK)
                    {
                        X_LOG_WARN("Failed to handle new connection: %s", xServerGetErrorString(l_iResult));
                        networkCloseSocket(l_ptClientSocket);
                    }
                }
            }
        }
    }
    
    X_LOG_INFO("Server thread terminating");
    return NULL;
}

//-----------------------------------------------------------------------------
// Client Management
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// findFreeClientSlot
///////////////////////////////////////////
static clientCtx *findFreeClientSlot(void)
{
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        if (s_tServer.t_atClients[i].t_tId == INVALID_CLIENT_ID)
        {
            return &s_tServer.t_atClients[i];
        }
    }
    return NULL;
}

///////////////////////////////////////////
/// findClientById
///////////////////////////////////////////
static clientCtx *findClientById(ClientID p_tClientId)
{
    if (p_tClientId == INVALID_CLIENT_ID)
    {
        return NULL;
    }
    
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        if (s_tServer.t_atClients[i].t_tId == p_tClientId)
        {
            return &s_tServer.t_atClients[i];
        }
    }
    return NULL;
}

///////////////////////////////////////////
/// handleNewConnection
///////////////////////////////////////////
static int handleNewConnection(NetworkSocket *p_ptClientSocket, const NetworkAddress *p_ptClientAddr)
{
    X_LOG_INFO("New client connection from %s:%d", p_ptClientAddr->t_cAddress, p_ptClientAddr->t_usPort);
    
    // Find free client slot
    mutexLock(&s_tServer.t_tClientsMutex);
    
    // First, try to cleanup any disconnected clients to free slots
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        clientCtx *l_ptCheckClient = &s_tServer.t_atClients[i];
        if (l_ptCheckClient->t_tId != INVALID_CLIENT_ID && !l_ptCheckClient->t_bConnected && 
            l_ptCheckClient->t_tClientTask.t_iState == OS_TASK_STATUS_TERMINATED)
        {
            X_LOG_INFO("Freeing slot for terminated client %u before new connection", l_ptCheckClient->t_tId);
            clientCleanup(l_ptCheckClient);
            s_tServer.t_iClientCount--;
        }
    }
    
    if (s_tServer.t_iClientCount >= SERVER_MAX_CLIENTS)
    {
        mutexUnlock(&s_tServer.t_tClientsMutex);
        X_LOG_WARN("Maximum clients reached, rejecting connection");
        return SERVER_MAX_CLIENTS_REACHED;
    }
    
    clientCtx *l_ptClient = findFreeClientSlot();
    if (l_ptClient == NULL)
    {
        mutexUnlock(&s_tServer.t_tClientsMutex);
        X_LOG_ERROR("No free client slot found");
        return SERVER_ERROR;
    }
    
    // Initialize client context
    memset(l_ptClient, 0, sizeof(clientCtx));
    l_ptClient->t_tId = (ClientID)((l_ptClient - s_tServer.t_atClients));  // Use array index as ID
    l_ptClient->t_ptSocket = p_ptClientSocket;
    l_ptClient->t_tAddress = *p_ptClientAddr;
    l_ptClient->t_bConnected = true;
    l_ptClient->t_bShutdown = false;
    
    // Create client name string
    snprintf(l_ptClient->t_acClientName, sizeof(l_ptClient->t_acClientName), 
            "%s:%d", p_ptClientAddr->t_cAddress, p_ptClientAddr->t_usPort);
    
    // Setup TLS if enabled
    if (s_tServer.t_sConfig.t_bUseTls && s_tServer.t_ptTlsEngine != NULL)
    {
        int l_iTlsResult = tlsEngineAttachSocket(s_tServer.t_ptTlsEngine,
                                                p_ptClientSocket->t_iSocketFd,
                                                &l_ptClient->t_ptTlsSession);
        if (l_iTlsResult != CERT_OK)
        {
            X_LOG_WARN("TLS handshake failed for client %s, closing connection", l_ptClient->t_acClientName);
            mutexUnlock(&s_tServer.t_tClientsMutex);
            return SERVER_TLS_ERROR;
        }
        X_LOG_INFO("TLS session established for client %u", l_ptClient->t_tId);
    }
    
    // Set socket timeout
    if (s_tServer.t_sConfig.t_iSocketTimeout > 0)
    {
        networkSetTimeout(p_ptClientSocket, s_tServer.t_sConfig.t_iSocketTimeout, false);
    }
    
    // Create client thread
    if (osTaskInit(&l_ptClient->t_tClientTask) != OS_TASK_SUCCESS)
    {
        X_LOG_ERROR("Failed to initialize client task");
        clientCleanup(l_ptClient);
        mutexUnlock(&s_tServer.t_tClientsMutex);
        return SERVER_THREAD_ERROR;
    }
    
    l_ptClient->t_tClientTask.t_ptTask = clientThread;
    l_ptClient->t_tClientTask.t_ptTaskArg = l_ptClient;
    snprintf(l_ptClient->t_tClientTask.t_acTaskName, sizeof(l_ptClient->t_tClientTask.t_acTaskName), "client_%u", l_ptClient->t_tId);
    
    if (osTaskCreate(&l_ptClient->t_tClientTask) != OS_TASK_SUCCESS)
    {
        X_LOG_ERROR("Failed to create client thread");
        clientCleanup(l_ptClient);
        mutexUnlock(&s_tServer.t_tClientsMutex);
        return SERVER_THREAD_ERROR;
    }
    
    s_tServer.t_iClientCount++;
    mutexUnlock(&s_tServer.t_tClientsMutex);
    
    X_LOG_INFO("Client %u (%s) connected successfully, total clients: %d", 
              l_ptClient->t_tId, l_ptClient->t_acClientName, s_tServer.t_iClientCount);
    
    return SERVER_OK;
}

//-----------------------------------------------------------------------------
// Client Thread Implementation
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// clientThread
///////////////////////////////////////////
static void *clientThread(void *p_pvArg)
{
    clientCtx *l_ptClient = (clientCtx *)p_pvArg;
    
    X_LOG_INFO("Client %u thread started", l_ptClient->t_tId);
    
    // Main client loop
    while (l_ptClient->t_bConnected && !l_ptClient->t_bShutdown)
    {
        int l_iResult = clientReceiveMessage(l_ptClient);
        
        if (l_iResult <= 0)
        {
            if (l_iResult == NETWORK_BROKEN_PIPE || l_iResult == NETWORK_ERROR)
            {
                X_LOG_INFO("Client %u disconnected in thread loop", l_ptClient->t_tId);
                // t_bConnected is already set to false in clientReceiveMessage
                break;
            }
            // Timeout or temporary error, continue
            continue;
        }
        
        // Additional safety check: if t_bConnected was set to false by another thread
        if (!l_ptClient->t_bConnected)
        {
            X_LOG_INFO("Client %u marked as disconnected by another thread", l_ptClient->t_tId);
            break;
        }
    }
    
    X_LOG_INFO("Client %u thread terminating", l_ptClient->t_tId);
    
    // Ensure t_bConnected is false (thread-safe, atomic operation)
    l_ptClient->t_bConnected = false;
    
    // Immediate cleanup when thread terminates - thread-safe with mutex
    mutexLock(&s_tServer.t_tClientsMutex);
    
    // Double-check client is still valid and not already cleaned up
    if (l_ptClient->t_tId != INVALID_CLIENT_ID && l_ptClient->t_ptSocket != NULL)
    {
        ClientID l_tClientId = l_ptClient->t_tId; // Save ID for logging before cleanup
        X_LOG_INFO("Thread-safe cleanup of client %u after disconnection", l_tClientId);
        
        // Perform cleanup (this will set t_tId to INVALID_CLIENT_ID)
        clientCleanup(l_ptClient);
        
        // Safely decrement client count
        if (s_tServer.t_iClientCount > 0)
        {
            s_tServer.t_iClientCount--;
        }
        
        X_LOG_INFO("Client %u cleanup completed, remaining clients: %d", l_tClientId, s_tServer.t_iClientCount);
    }
    else
    {
        X_LOG_INFO("Client %u already cleaned up by another thread", l_ptClient->t_tId);
    }
    
    mutexUnlock(&s_tServer.t_tClientsMutex);
    
    return NULL;
}

///////////////////////////////////////////
/// clientReceiveMessage
///////////////////////////////////////////
static int clientReceiveMessage(clientCtx *p_ptClient)
{
    // Read protocol header first (3 bytes)
    uint8_t l_aucHeader[PROTOCOL_HEADER_SIZE];
    int l_iHeaderReceived;
    
    if (p_ptClient->t_ptTlsSession != NULL)
    {
        // TLS receive
        l_iHeaderReceived = wolfSSL_read(p_ptClient->t_ptTlsSession, l_aucHeader, PROTOCOL_HEADER_SIZE);
        if (l_iHeaderReceived <= 0)
        {
            int l_iError = wolfSSL_get_error(p_ptClient->t_ptTlsSession, l_iHeaderReceived);
            if (l_iError == SOCKET_PEER_CLOSED_E || l_iError == WOLFSSL_ERROR_ZERO_RETURN)
            {
                X_LOG_INFO("Client %u TLS disconnected (close_notify or peer closed)", p_ptClient->t_tId);
                // IMPORTANT: Mark client as disconnected to stop the thread loop
                p_ptClient->t_bConnected = false;
                return NETWORK_ERROR;
            }
        }
    }
    else
    {
        // Plain TCP receive
        l_iHeaderReceived = networkReceive(p_ptClient->t_ptSocket, l_aucHeader, PROTOCOL_HEADER_SIZE);
    }
    
    if (l_iHeaderReceived != PROTOCOL_HEADER_SIZE)
    {
        if (l_iHeaderReceived == NETWORK_BROKEN_PIPE || l_iHeaderReceived == NETWORK_ERROR || l_iHeaderReceived == 0)
        {
            X_LOG_INFO("Client %u disconnected (header read: %d)", p_ptClient->t_tId, l_iHeaderReceived);
            // IMPORTANT: Mark client as disconnected to stop the thread loop
            p_ptClient->t_bConnected = false;
            return NETWORK_ERROR;
        }
        return NETWORK_TIMEOUT;
    }
    
    // Validate header and get payload size
    uint32_t l_ulPayloadSize;
    uint8_t l_ucMsgType;
    int l_iValidateResult = protocolValidateHeader(l_aucHeader, &l_ulPayloadSize, &l_ucMsgType);
    if (l_iValidateResult != PROTOCOL_OK)
    {
        X_LOG_WARN("Invalid protocol header from client %u", p_ptClient->t_tId);
        return NETWORK_ERROR;
    }
    
    // Calculate total frame size
    uint32_t l_ulFrameSize = protocolCalculateFrameSize(l_ulPayloadSize - 1); // -1 because payload includes message type
    
    if (l_ulFrameSize > SERVER_RECV_BUFFER_SIZE)
    {
        X_LOG_WARN("Frame too large from client %u: %u bytes", p_ptClient->t_tId, l_ulFrameSize);
        return NETWORK_ERROR;
    }
    
    // Copy header to buffer
    memcpy(p_ptClient->t_aucRecvBuffer, l_aucHeader, PROTOCOL_HEADER_SIZE);
    
    // Read remaining payload if any
    if (l_ulPayloadSize > 1)
    {
        uint32_t l_ulRemainingBytes = l_ulPayloadSize - 1;
        int l_iPayloadReceived;
        
        if (p_ptClient->t_ptTlsSession != NULL)
        {
            // TLS receive
            l_iPayloadReceived = wolfSSL_read(p_ptClient->t_ptTlsSession, 
                                            p_ptClient->t_aucRecvBuffer + PROTOCOL_HEADER_SIZE, 
                                            (int)l_ulRemainingBytes);
            // Check for TLS disconnection during payload read
            if (l_iPayloadReceived <= 0)
            {
                int l_iError = wolfSSL_get_error(p_ptClient->t_ptTlsSession, l_iPayloadReceived);
                if (l_iError == SOCKET_PEER_CLOSED_E || l_iError == WOLFSSL_ERROR_ZERO_RETURN)
                {
                    X_LOG_INFO("Client %u TLS disconnected during payload read", p_ptClient->t_tId);
                    p_ptClient->t_bConnected = false;
                    return NETWORK_ERROR;
                }
            }
        }
        else
        {
            // Plain TCP receive
            l_iPayloadReceived = networkReceive(p_ptClient->t_ptSocket, 
                                              p_ptClient->t_aucRecvBuffer + PROTOCOL_HEADER_SIZE, 
                                              l_ulRemainingBytes);
        }
        
        if ((uint32_t)l_iPayloadReceived != l_ulRemainingBytes)
        {
            if (l_iPayloadReceived == NETWORK_BROKEN_PIPE || l_iPayloadReceived == NETWORK_ERROR || l_iPayloadReceived == 0)
            {
                X_LOG_INFO("Client %u disconnected (payload read: %d/%u)", p_ptClient->t_tId, l_iPayloadReceived, l_ulRemainingBytes);
                // IMPORTANT: Mark client as disconnected to stop the thread loop
                p_ptClient->t_bConnected = false;
                return NETWORK_ERROR;
            }
            return NETWORK_TIMEOUT;
        }
    }
    
    // Parse protocol frame
    ProtocolFrame l_tFrame;
    int l_iParseResult = protocolParseFrame(p_ptClient->t_aucRecvBuffer, l_ulFrameSize, &l_tFrame);
    
    if (l_iParseResult == PROTOCOL_OK)
    {
        // Update statistics
        p_ptClient->t_ullBytesReceived += l_ulFrameSize;
        p_ptClient->t_ulMessagesReceived++;
        
        // Handle message using callback system
        handleNetworkMessage((struct clientCtx *)p_ptClient, &l_tFrame);
    }
    else
    {
        X_LOG_WARN("Failed to parse frame from client %u: %s", 
                  p_ptClient->t_tId, protocolGetErrorString(l_iParseResult));
    }
    
    return (int)l_ulFrameSize;
}

///////////////////////////////////////////
/// clientCleanup
///////////////////////////////////////////
static void clientCleanup(clientCtx *p_ptClient)
{
    if (p_ptClient == NULL || p_ptClient->t_tId == INVALID_CLIENT_ID)
    {
        return;
    }
    
    X_LOG_INFO("Cleaning up client %u (%s)", p_ptClient->t_tId, p_ptClient->t_acClientName);
    
    // Shutdown TLS session
    if (p_ptClient->t_ptTlsSession != NULL)
    {
        tlsEngineShutdown(p_ptClient->t_ptTlsSession);
        p_ptClient->t_ptTlsSession = NULL;
    }
    
    // Close socket
    if (p_ptClient->t_ptSocket != NULL)
    {
        networkCloseSocket(p_ptClient->t_ptSocket);
        p_ptClient->t_ptSocket = NULL;
    }
    
    // Reset client context (mark as free)
    memset(p_ptClient, 0, sizeof(clientCtx));
    p_ptClient->t_tId = INVALID_CLIENT_ID;
}

//-----------------------------------------------------------------------------
// Client Communication
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// clientSendData
///////////////////////////////////////////
static int clientSendData(clientCtx *p_ptClient, const void *p_pvData, int p_iSize)
{
    if (p_ptClient == NULL || !p_ptClient->t_bConnected || p_ptClient->t_bShutdown)
    {
        return SERVER_ERROR;
    }
    
    int l_iResult;
    
    if (p_ptClient->t_ptTlsSession != NULL)
    {
        // TLS send
        l_iResult = wolfSSL_write(p_ptClient->t_ptTlsSession, p_pvData, p_iSize);
        if (l_iResult <= 0)
        {
            int l_iError = wolfSSL_get_error(p_ptClient->t_ptTlsSession, l_iResult);
            if (l_iError == SOCKET_PEER_CLOSED_E)
            {
                p_ptClient->t_bConnected = false;
            }
            else
            {
                X_LOG_WARN("TLS write error for client %u: %d", p_ptClient->t_tId, l_iError);
                return SERVER_ERROR;
            }
        }
    }
    else
    {
        // Plain TCP send
        l_iResult = networkSend(p_ptClient->t_ptSocket, (void *)p_pvData, (unsigned long)p_iSize);
        if (l_iResult <= 0)
        {
            return SERVER_ERROR;
        }
    }
    
    // Update statistics
    p_ptClient->t_ulBytesSent += (uint64_t)l_iResult;
    p_ptClient->t_ulMessagesSent++;
    
    return l_iResult;
}

//-----------------------------------------------------------------------------
// Utility Functions
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// cleanupDisconnectedClients
///////////////////////////////////////////
static void cleanupDisconnectedClients(void)
{
    mutexLock(&s_tServer.t_tClientsMutex);
    
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        clientCtx *l_ptClient = &s_tServer.t_atClients[i];
        
        // Check if client is marked as disconnected but thread hasn't cleaned up yet
        if (l_ptClient->t_tId != INVALID_CLIENT_ID && !l_ptClient->t_bConnected && !l_ptClient->t_bShutdown)
        {
            // Check if thread is still running
            if (l_ptClient->t_tClientTask.t_iState == OS_TASK_STATUS_TERMINATED)
            {
                X_LOG_INFO("Cleaning up terminated client %u", l_ptClient->t_tId);
                clientCleanup(l_ptClient);
                s_tServer.t_iClientCount--;
            }
        }
    }
    
    mutexUnlock(&s_tServer.t_tClientsMutex);
}

//-----------------------------------------------------------------------------
// Public API Implementation
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// xServerSendMessage
///////////////////////////////////////////
int xServerSendMessage(ClientID p_tClientId, uint8_t p_ucMsgType, 
                      const void *p_pvPayload, uint32_t p_ulPayloadSize)
{
    if (!s_tServer.t_bInitialized || !s_tServer.t_bRunning)
    {
        return SERVER_NOT_RUNNING;
    }
    
    if (!protocolIsValidPayloadSize(p_ulPayloadSize))
    {
        return SERVER_INVALID_PARAM;
    }
    
    mutexLock(&s_tServer.t_tClientsMutex);
    
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || !l_ptClient->t_bConnected)
    {
        mutexUnlock(&s_tServer.t_tClientsMutex);
        return SERVER_CLIENT_NOT_FOUND;
    }
    
    // Create protocol frame in client's send buffer
    uint32_t l_ulFrameSize;
    int l_iProtocolResult = protocolCreateFrame((network_message_type_t)p_ucMsgType,
                                               (const uint8_t *)p_pvPayload,
                                               p_ulPayloadSize,
                                               l_ptClient->t_aucSendBuffer,
                                               sizeof(l_ptClient->t_aucSendBuffer),
                                               &l_ulFrameSize);
    
    if (l_iProtocolResult != PROTOCOL_OK)
    {
        mutexUnlock(&s_tServer.t_tClientsMutex);
        X_LOG_ERROR("Failed to create protocol frame: %s", protocolGetErrorString(l_iProtocolResult));
        return SERVER_ERROR;
    }
    
    // Send frame
    int l_iSent = clientSendData(l_ptClient, l_ptClient->t_aucSendBuffer, (int)l_ulFrameSize);
    
    mutexUnlock(&s_tServer.t_tClientsMutex);
    
    if (l_iSent != (int)l_ulFrameSize)
    {
        X_LOG_WARN("Incomplete send to client %u: %d/%u bytes", p_tClientId, l_iSent, l_ulFrameSize);
        return SERVER_ERROR;
    }
    
    return SERVER_OK;
}

///////////////////////////////////////////
/// xServerBroadcastMessage
///////////////////////////////////////////
int xServerBroadcastMessage(uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize)
{
    if (!s_tServer.t_bInitialized || !s_tServer.t_bRunning)
    {
        return SERVER_NOT_RUNNING;
    }
    
    if (!protocolIsValidPayloadSize(p_ulPayloadSize))
    {
        return SERVER_INVALID_PARAM;
    }
    
    // Build frame once in shared broadcast buffer
    uint32_t l_ulFrameSize;
    int l_iProtocolResult = protocolCreateFrame((network_message_type_t)p_ucMsgType,
                                               (const uint8_t *)p_pvPayload,
                                               p_ulPayloadSize,
                                               s_tServer.t_aucBroadcastBuffer,
                                               sizeof(s_tServer.t_aucBroadcastBuffer),
                                               &l_ulFrameSize);
    
    if (l_iProtocolResult != PROTOCOL_OK)
    {
        X_LOG_ERROR("Failed to create broadcast frame: %s", protocolGetErrorString(l_iProtocolResult));
        return SERVER_ERROR;
    }
    
    int l_iSuccessCount = 0;
    int l_iFailureCount = 0;
    
    mutexLock(&s_tServer.t_tClientsMutex);
    
    // Send to all connected clients
    for (int i = 0; i < SERVER_MAX_CLIENTS; i++)
    {
        clientCtx *l_ptClient = &s_tServer.t_atClients[i];
        
        if (l_ptClient->t_tId != INVALID_CLIENT_ID && l_ptClient->t_bConnected && !l_ptClient->t_bShutdown)
        {
            int l_iSent = clientSendData(l_ptClient, s_tServer.t_aucBroadcastBuffer, (int)l_ulFrameSize);
            
            if (l_iSent == (int)l_ulFrameSize)
            {
                l_iSuccessCount++;
            }
            else
            {
                l_iFailureCount++;
                if (l_iSent == SERVER_ERROR)
                {
                    l_ptClient->t_bConnected = false; // Mark for cleanup
                    X_LOG_INFO("Client %u marked for cleanup due to send error", l_ptClient->t_tId);
                }
            }
        }
    }
    
    mutexUnlock(&s_tServer.t_tClientsMutex);
    
    X_LOG_INFO("Broadcast message 0x%02X: %d success, %d failed", p_ucMsgType, l_iSuccessCount, l_iFailureCount);
    return (l_iFailureCount == 0) ? SERVER_OK : SERVER_ERROR;
}

///////////////////////////////////////////
/// xServerGetClientInfo
///////////////////////////////////////////
bool xServerGetClientInfo(ClientID p_tClientId, char *p_ptcBuffer, int p_iBufferSize)
{
    if (p_ptcBuffer == NULL || p_iBufferSize <= 0)
    {
        return false;
    }
    
    mutexLock(&s_tServer.t_tClientsMutex);
    
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || !l_ptClient->t_bConnected)
    {
        mutexUnlock(&s_tServer.t_tClientsMutex);
        return false;
    }
    
    snprintf(p_ptcBuffer, (size_t)p_iBufferSize, 
            "Client %u: %s (TLS: %s, RX: %lu bytes/%u msgs, TX: %lu bytes/%u msgs)",
            l_ptClient->t_tId,
            l_ptClient->t_acClientName,
            l_ptClient->t_ptTlsSession ? "yes" : "no",
            (unsigned long)l_ptClient->t_ullBytesReceived,
            l_ptClient->t_ulMessagesReceived,
            (unsigned long)l_ptClient->t_ulBytesSent,
            l_ptClient->t_ulMessagesSent);
    
    mutexUnlock(&s_tServer.t_tClientsMutex);
    return true;
}

///////////////////////////////////////////
/// xServerGetClientCount
///////////////////////////////////////////
int xServerGetClientCount(void)
{
    mutexLock(&s_tServer.t_tClientsMutex);
    int l_iCount = s_tServer.t_iClientCount;
    mutexUnlock(&s_tServer.t_tClientsMutex);
    return l_iCount;
}

///////////////////////////////////////////
/// xServerGetclientCtx
///////////////////////////////////////////
clientCtx *xServerGetclientCtx(ClientID p_tClientId)
{
    // This is safe because we use fixed-size arrays, no allocation/deallocation
    if (p_tClientId >= SERVER_MAX_CLIENTS)
    {
        return NULL;
    }
    
    clientCtx *l_ptClient = &s_tServer.t_atClients[p_tClientId];
    return (l_ptClient->t_tId == p_tClientId) ? l_ptClient : NULL;
}

///////////////////////////////////////////
/// xServerGetClientID
///////////////////////////////////////////
ClientID xServerGetClientID(const clientCtx *p_ptContext)
{
    const clientCtx *l_ptContext = (const clientCtx *)p_ptContext;
    return (l_ptContext != NULL) ? l_ptContext->t_tId : INVALID_CLIENT_ID;
}

///////////////////////////////////////////
/// xServerCreateDefaultConfig
///////////////////////////////////////////
ServerConfig xServerCreateDefaultConfig(void)
{
    ServerConfig l_sConfig;
    memset(&l_sConfig, 0, sizeof(ServerConfig));
    
    l_sConfig.t_usPort = SERVER_DEFAULT_PORT;
    l_sConfig.t_bUseTls = false;
    l_sConfig.t_iSocketTimeout = 30; // 30 seconds
    
    // Default TLS paths (will be ignored if TLS is disabled)
    strcpy(l_sConfig.t_acCertFile, "../pki/server/fullchain.pem");
    strcpy(l_sConfig.t_acKeyFile, "../pki/server/server.key");
    strcpy(l_sConfig.t_acCaDir, "../pki/root");
    
    return l_sConfig;
}

///////////////////////////////////////////
/// xServerGetErrorString
///////////////////////////////////////////
const char *xServerGetErrorString(int p_iError)
{
    switch (p_iError)
    {
        case SERVER_OK:                   return "Success";
        case SERVER_ERROR:                return "General error";
        case SERVER_INVALID_PARAM:        return "Invalid parameter";
        case SERVER_NOT_INITIALIZED:      return "Server not initialized";
        case SERVER_ALREADY_RUNNING:      return "Server already running";
        case SERVER_NOT_RUNNING:          return "Server not running";
        case SERVER_MAX_CLIENTS_REACHED:  return "Maximum clients reached";
        case SERVER_CLIENT_NOT_FOUND:     return "Client not found";
        case SERVER_SOCKET_ERROR:         return "Socket error";
        case SERVER_TLS_ERROR:            return "TLS error";
        case SERVER_THREAD_ERROR:         return "Thread error";
        default:                          return "Unknown error";
    }
} 
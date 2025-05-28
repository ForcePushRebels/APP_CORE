////////////////////////////////////////////////////////////
//  Network Server Implementation
//  Provides a streamlined API for bidirectional Java-C communication
//
// general disclosure: copy or share the file is forbidden
// Written : 18/04/2025
////////////////////////////////////////////////////////////

#include "networkServer.h"
#include "xLog.h"
#include <stdlib.h>
#include <string.h>

//-----------------------------------------------------------------------------
// Constants & Defines
//-----------------------------------------------------------------------------

#define SERVER_THREAD_STACK_SIZE (64 * 1024)   // 64KB stack for the server thread
#define CLIENT_THREAD_STACK_SIZE (32 * 1024)   // 32KB stack for the client threads
#define SERVER_ACCEPT_TIMEOUT 500              // 500ms timeout for accept
#define SERVER_MAX_BUFFER_SIZE 4096            // Max buffer size
#define MAX_CLIENTS 10                         // Max number of clients

//-----------------------------------------------------------------------------
// Client Structure
//-----------------------------------------------------------------------------

struct client_ctx_t
{
    ClientID t_tId;            // unique client ID
    NetworkSocket *t_ptSocket; // client socket
    NetworkAddress t_tAddress; // client address
    bool t_bConnected;         // connection status
    xOsTaskCtx t_tTask;        // client task context
    void *t_ptUserData;        // user data
    serverCtx *t_ptServer;     // reference to the server
};

//-----------------------------------------------------------------------------
// Server Structure
//-----------------------------------------------------------------------------

struct server_ctx_t
{
    NetworkSocket *t_ptSocket;           // server socket
    NetworkAddress t_tAddress;           // server address
    xOsMutexCtx t_tMutex;                // synchronization mutex
    xOsTaskCtx t_tTask;                  // server task context
    bool t_bRunning;                     // server running status
    ServerConfig t_sConfig;              // server configuration
    ClientID t_tNextClientId;            // next client ID to assign
    clientCtx *t_ptClients[MAX_CLIENTS]; // array of clients
    int t_iNumClients;                   // number of connected clients
    MessageHandler t_pfHandler;          // message handler function
};

///////////////////////////////////////////
/// s_ptServerInstance
///////////////////////////////////////////
static serverCtx *s_ptServerInstance = NULL;

///////////////////////////////////////////
/// serverThreadFunc
///////////////////////////////////////////
static void *serverThreadFunc(void *p_ptArg);
static void *clientThreadFunc(void *p_ptArg);

///////////////////////////////////////////
/// client management prototypes
///////////////////////////////////////////
static void cleanupAndFreeClient(clientCtx *p_ptClient);
static void clientThreadCleanup(clientCtx *p_ptClient);
static bool parseMessageHeader(const uint8_t *p_ptucData,
                               int p_iSize,
                               uint8_t *p_ptucMsgType);
static clientCtx *findClientById(ClientID p_tClientId);
static int addClient(clientCtx *p_ptClient);
static int removeClient(clientCtx *p_ptClient);
static bool isClientConnected(clientCtx *p_ptClient);

///////////////////////////////////////////
/// networkServerInit
///////////////////////////////////////////
int networkServerInit(void)
{
    // check if the server is already initialized
    if (s_ptServerInstance != NULL)
    {
        X_LOG_TRACE("Server already initialized");
        return SERVER_OK;
    }

    // create the server
    s_ptServerInstance = (serverCtx *)malloc(sizeof(serverCtx));
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Failed to allocate server memory");
        return SERVER_MEMORY_ERROR;
    }

    // initialize the server
    memset(s_ptServerInstance, 0, sizeof(serverCtx));

    // create the mutex
    if (mutexCreate(&s_ptServerInstance->t_tMutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to create server mutex");
        free(s_ptServerInstance);
        s_ptServerInstance = NULL;
        return SERVER_ERROR;
    }

    // set the default configuration
    s_ptServerInstance->t_sConfig = networkServerCreateDefaultConfig();
    s_ptServerInstance->t_tNextClientId = 1; // start at 1, 0 is INVALID_CLIENT_ID

    X_LOG_TRACE("Server initialized");
    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerConfigure
///////////////////////////////////////////
int networkServerConfigure(const ServerConfig *p_ptConfig)
{
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Server not initialized");
        return SERVER_INVALID_STATE;
    }

    if (p_ptConfig == NULL)
    {
        return SERVER_INVALID_PARAM;
    }

    // check if the server is already running
    if (s_ptServerInstance->t_bRunning)
    {
        X_LOG_TRACE("Cannot configure server while running");
        return SERVER_INVALID_STATE;
    }

    // lock the mutex
    mutexLock(&s_ptServerInstance->t_tMutex);

    // copy the configuration
    memcpy(&s_ptServerInstance->t_sConfig, p_ptConfig, sizeof(ServerConfig));

    // configure the server address
    s_ptServerInstance->t_tAddress = networkMakeAddress(
        p_ptConfig->t_pcBindAddress ? p_ptConfig->t_pcBindAddress : "0.0.0.0",
        p_ptConfig->t_usPort);

    // configure the server task
    s_ptServerInstance->t_tTask.t_ptTask = serverThreadFunc;
    s_ptServerInstance->t_tTask.t_ptTaskArg = s_ptServerInstance;
    s_ptServerInstance->t_tTask.t_iPriority = 1;
    s_ptServerInstance->t_tTask.t_ulStackSize = SERVER_THREAD_STACK_SIZE;

    mutexUnlock(&s_ptServerInstance->t_tMutex);

    X_LOG_TRACE("Server configured on port %d", p_ptConfig->t_usPort);
    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerSetMessageHandler
///////////////////////////////////////////
void networkServerSetMessageHandler(MessageHandler p_pfHandler)
{
    if (s_ptServerInstance != NULL)
    {
        // lock the mutex
        mutexLock(&s_ptServerInstance->t_tMutex);
        s_ptServerInstance->t_pfHandler = p_pfHandler;
        mutexUnlock(&s_ptServerInstance->t_tMutex);

        X_LOG_TRACE("Message handler set");
    }
}

///////////////////////////////////////////
/// networkServerStart
///////////////////////////////////////////
int networkServerStart(void)
{
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Server not initialized");
        return SERVER_INVALID_STATE;
    }

    // check if the server is already running
    if (s_ptServerInstance->t_bRunning)
    {
        X_LOG_TRACE("Server is already running");
        return SERVER_OK;
    }

    // ABSTRACTION: Use the new connect function
    int l_iResult = networkServerConnect();
    if (l_iResult != SERVER_OK)
    {
        return l_iResult;
    }

    // mark the server as running
    mutexLock(&s_ptServerInstance->t_tMutex);
    s_ptServerInstance->t_bRunning = true;
    mutexUnlock(&s_ptServerInstance->t_tMutex);

    // create the server thread
    int l_iTaskResult = osTaskCreate(&s_ptServerInstance->t_tTask);
    if (l_iTaskResult != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("Failed to create server thread: %s", osTaskGetErrorString(l_iTaskResult));

        mutexLock(&s_ptServerInstance->t_tMutex);
        s_ptServerInstance->t_bRunning = false;
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
        mutexUnlock(&s_ptServerInstance->t_tMutex);

        return SERVER_THREAD_ERROR;
    }

    X_LOG_TRACE("Server started on port %d", s_ptServerInstance->t_sConfig.t_usPort);
    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerStop
///////////////////////////////////////////
int networkServerStop(void)
{
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Server not initialized");
        return SERVER_INVALID_STATE;
    }

    // check if the server is running
    if (!s_ptServerInstance->t_bRunning)
    {
        X_LOG_TRACE("Server is not running");
        return SERVER_NOT_RUNNING;
    }

    X_LOG_TRACE("Stopping server...");

    // lock the mutex for modifications
    mutexLock(&s_ptServerInstance->t_tMutex);

    // mark the server as stopped
    s_ptServerInstance->t_bRunning = false;

    // close the server socket to unblock accept() in the server thread
    if (s_ptServerInstance->t_ptSocket != NULL)
    {
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
    }

    mutexUnlock(&s_ptServerInstance->t_tMutex);

    // stop the server thread
    int l_iResult = osTaskStop(&s_ptServerInstance->t_tTask, OS_TASK_STOP_TIMEOUT);
    if (l_iResult != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("Failed to stop server thread gracefully: %s", osTaskGetErrorString(l_iResult));
        osTaskEnd(&s_ptServerInstance->t_tTask);
    }

    // wait for the server thread to terminate
    osTaskWait(&s_ptServerInstance->t_tTask, NULL);

    // disconnect all clients
    mutexLock(&s_ptServerInstance->t_tMutex);
    for (int i = 0; i < MAX_CLIENTS; i++)
    {
        clientCtx *l_ptClient = s_ptServerInstance->t_ptClients[i];
        if (l_ptClient != NULL)
        {
            // disconnect the client outside the lock to avoid a deadlock
            mutexUnlock(&s_ptServerInstance->t_tMutex);
            networkServerDisconnectClient(l_ptClient->t_tId);
            mutexLock(&s_ptServerInstance->t_tMutex);

            // the table may have changed during the unlock, check again
            i--; // to re-check this index in the next loop
        }
    }
    mutexUnlock(&s_ptServerInstance->t_tMutex);

    X_LOG_TRACE("Server stopped");
    return SERVER_OK;
}

void networkServerCleanup(void)
{
    if (s_ptServerInstance == NULL)
    {
        return;
    }

    // stop the server if it is running
    if (s_ptServerInstance->t_bRunning)
    {
        networkServerStop();
    }

    // destroy the mutex
    mutexDestroy(&s_ptServerInstance->t_tMutex);

    // free the server structure
    free(s_ptServerInstance);
    s_ptServerInstance = NULL;

    X_LOG_TRACE("Server resources cleaned up");
}

///////////////////////////////////////////
/// networkServerGetClientID
///////////////////////////////////////////
ClientID networkServerGetClientID(clientCtx *p_ptClient)
{
    if (p_ptClient == NULL)
    {
        return INVALID_CLIENT_ID;
    }

    return p_ptClient->t_tId;
}

///////////////////////////////////////////
/// networkServerGetClientCtx
///////////////////////////////////////////
clientCtx *networkServerGetClientCtx(ClientID p_tClientId)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return NULL;
    }

    return findClientById(p_tClientId);
}

///////////////////////////////////////////
/// findClientById
///////////////////////////////////////////
static clientCtx *findClientById(ClientID p_tClientId)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return NULL;
    }

    mutexLock(&s_ptServerInstance->t_tMutex);

    for (int i = 0; i < MAX_CLIENTS; i++)
    {
        clientCtx *l_ptClient = s_ptServerInstance->t_ptClients[i];
        if (l_ptClient != NULL && l_ptClient->t_tId == p_tClientId)
        {
            mutexUnlock(&s_ptServerInstance->t_tMutex);
            return l_ptClient;
        }
    }

    mutexUnlock(&s_ptServerInstance->t_tMutex);
    return NULL;
}

///////////////////////////////////////////
/// addClient
///////////////////////////////////////////
static int addClient(clientCtx *p_ptClient)
{
    if (s_ptServerInstance == NULL || p_ptClient == NULL)
    {
        return SERVER_INVALID_PARAM;
    }

    mutexLock(&s_ptServerInstance->t_tMutex);

    // check if we have reached the maximum number of clients
    if (s_ptServerInstance->t_iNumClients >= MAX_CLIENTS)
    {
        mutexUnlock(&s_ptServerInstance->t_tMutex);
        X_LOG_TRACE("Maximum number of clients reached");
        return SERVER_MAX_CLIENTS_REACHED;
    }

    // find a free slot
    for (int i = 0; i < MAX_CLIENTS; i++)
    {
        if (s_ptServerInstance->t_ptClients[i] == NULL)
        {
            // assign a unique ID
            p_ptClient->t_tId = s_ptServerInstance->t_tNextClientId++;

            // if we reach the limit, restart at 1
            if (s_ptServerInstance->t_tNextClientId == INVALID_CLIENT_ID)
            {
                s_ptServerInstance->t_tNextClientId = 1;
            }

            // store the client
            s_ptServerInstance->t_ptClients[i] = p_ptClient;
            s_ptServerInstance->t_iNumClients++;

            mutexUnlock(&s_ptServerInstance->t_tMutex);
            X_LOG_TRACE("Client %u added, total clients: %d", p_ptClient->t_tId, s_ptServerInstance->t_iNumClients);
            return SERVER_OK;
        }
    }

    // this should never happen (we checked t_iNumClients)
    mutexUnlock(&s_ptServerInstance->t_tMutex);
    X_LOG_TRACE("Client table full but count is %d", s_ptServerInstance->t_iNumClients);
    return SERVER_ERROR;
}

///////////////////////////////////////////
/// removeClient
///////////////////////////////////////////
static int removeClient(clientCtx *p_ptClient)
{
    if (s_ptServerInstance == NULL || p_ptClient == NULL)
    {
        return SERVER_INVALID_PARAM;
    }

    mutexLock(&s_ptServerInstance->t_tMutex);

    // search and remove the client
    for (int i = 0; i < MAX_CLIENTS; i++)
    {
        if (s_ptServerInstance->t_ptClients[i] == p_ptClient)
        {
            s_ptServerInstance->t_ptClients[i] = NULL;
            s_ptServerInstance->t_iNumClients--;

            mutexUnlock(&s_ptServerInstance->t_tMutex);
            X_LOG_TRACE("Client %u removed, total clients: %d", p_ptClient->t_tId, s_ptServerInstance->t_iNumClients);
            return SERVER_OK;
        }
    }

    mutexUnlock(&s_ptServerInstance->t_tMutex);
    X_LOG_TRACE("Client %u not found in table", p_ptClient->t_tId);
    return SERVER_CLIENT_NOT_FOUND;
}

///////////////////////////////////////////
/// networkServerGetClientAddress
///////////////////////////////////////////
bool networkServerGetClientAddress(ClientID p_tClientId, char *p_ptcBuffer, int p_iSize)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || p_ptcBuffer == NULL || p_iSize <= 0)
    {
        return false;
    }

    // copy the address to the buffer
    strncpy(p_ptcBuffer, l_ptClient->t_tAddress.t_cAddress, p_iSize - 1);
    p_ptcBuffer[p_iSize - 1] = '\0';

    return true;
}

///////////////////////////////////////////
/// networkServerGetClientPort
///////////////////////////////////////////
uint16_t networkServerGetClientPort(ClientID p_tClientId)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL)
    {
        return 0;
    }

    return l_ptClient->t_tAddress.t_usPort;
}

///////////////////////////////////////////
/// networkServerDisconnectClient
///////////////////////////////////////////
int networkServerDisconnectClient(ClientID p_tClientId)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL)
    {
        return SERVER_CLIENT_NOT_FOUND;
    }

    // Use centralized cleanup function
    cleanupAndFreeClient(l_ptClient);

    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerSetClientUserData
///////////////////////////////////////////
int networkServerSetClientUserData(ClientID p_tClientId, void *p_pvUserData)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL)
    {
        return SERVER_CLIENT_NOT_FOUND;
    }

    l_ptClient->t_ptUserData = p_pvUserData;
    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerGetClientUserData
///////////////////////////////////////////
void *networkServerGetClientUserData(ClientID p_tClientId)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL)
    {
        return NULL;
    }

    return l_ptClient->t_ptUserData;
}

///////////////////////////////////////////
/// networkServerSendToClient
///////////////////////////////////////////
int networkServerSendToClient(ClientID p_tClientId, const void *p_pvData, int p_iSize)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || p_pvData == NULL || p_iSize <= 0)
    {
        return SERVER_INVALID_PARAM;
    }

    if (!isClientConnected(l_ptClient))
    {
        return SERVER_CLIENT_DISCONNECTED;
    }

    return networkSend(l_ptClient->t_ptSocket, p_pvData, p_iSize);
}

///////////////////////////////////////////
/// networkServerSendMessage
///////////////////////////////////////////
int networkServerSendMessage(ClientID p_tClientId,
                             uint8_t p_ucMsgType,
                             const void *p_pvPayload,
                             uint32_t p_ulPayloadSize)
{
    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL)
    {
        return SERVER_CLIENT_NOT_FOUND;
    }

    if (!isClientConnected(l_ptClient))
    {
        return SERVER_CLIENT_DISCONNECTED;
    }

    // Validate the payload size can fit in uint16_t
    if (p_ulPayloadSize > UINT16_MAX)
    {
        X_LOG_TRACE("Payload size too large for uint16_t: %u bytes", p_ulPayloadSize);
        return SERVER_INVALID_PARAM;
    }

    // STEP 1: First send the size (2 bytes)
    uint16_t l_usNetworkPayloadSize = HOST_TO_NET_SHORT((uint16_t)p_ulPayloadSize);
    int l_iSizeResult = networkSend(l_ptClient->t_ptSocket, &l_usNetworkPayloadSize, sizeof(uint16_t));

    if (l_iSizeResult < 0)
    {
        X_LOG_TRACE("Failed to send payload size to client: %s", networkGetErrorString(l_iSizeResult));
        return SERVER_SOCKET_ERROR;
    }

    // STEP 2: Prepare and send the message (header + payload)
    uint32_t l_ulTotalMessageSize = 1 + p_ulPayloadSize; // 1 byte header + payload
    uint8_t *l_pucBuffer = (uint8_t *)malloc(l_ulTotalMessageSize);
    if (l_pucBuffer == NULL)
    {
        X_LOG_TRACE("Failed to allocate message buffer");
        return SERVER_MEMORY_ERROR;
    }

    // Write the message type (header)
    l_pucBuffer[0] = p_ucMsgType;

    // Write the payload
    if (p_ulPayloadSize > 0 && p_pvPayload != NULL)
    {
        memcpy(l_pucBuffer + 1, p_pvPayload, p_ulPayloadSize);
    }

    // Send the message (header + payload)
    int l_iMessageResult = networkSend(l_ptClient->t_ptSocket, l_pucBuffer, l_ulTotalMessageSize);
    free(l_pucBuffer);

    if (l_iMessageResult < 0)
    {
        X_LOG_TRACE("Failed to send message to client: %s", networkGetErrorString(l_iMessageResult));
        return SERVER_SOCKET_ERROR;
    }

    X_LOG_TRACE("Sent message type 0x%02X to client %u (%d bytes size + %d bytes message, %d bytes payload)",
                p_ucMsgType, p_tClientId, l_iSizeResult, l_iMessageResult, p_ulPayloadSize);
    return SERVER_OK;
}

///////////////////////////////////////////
/// Helper function for complete TCP read
/// 
/// CRITICAL: This function is absolutely necessary for the message protocol!
/// TCP recv() can return fewer bytes than requested, even when all data is available.
/// For example: requesting 1000 bytes might return only 300 bytes on first call.
/// 
/// This is especially critical for our protocol that reads:
/// 1. Size header (2 bytes) - must be read completely to avoid desync
/// 2. Message data (variable size) - must match exactly the announced size
/// 
/// Without this function, partial reads would break the entire protocol.
///////////////////////////////////////////
static int networkReceiveComplete(NetworkSocket *p_ptSocket, uint8_t *p_pucBuffer, int p_iSize)
{
    int l_iTotalReceived = 0;
    int l_iReceived;

    while (l_iTotalReceived < p_iSize)
    {
        l_iReceived = networkReceive(p_ptSocket, p_pucBuffer + l_iTotalReceived, p_iSize - l_iTotalReceived);

        if (l_iReceived <= 0)
        {
            // Error or connection closed
            return l_iReceived;
        }

        l_iTotalReceived += l_iReceived;
    }

    return l_iTotalReceived;
}

///////////////////////////////////////////
/// networkServerGetErrorString
///////////////////////////////////////////
const char *networkServerGetErrorString(int p_iError)
{
    switch (p_iError)
    {
    case SERVER_OK:
        return "Success";
    case SERVER_INVALID_PARAM:
        return "Invalid parameter";
    case SERVER_MEMORY_ERROR:
        return "Memory allocation error";
    case SERVER_SOCKET_ERROR:
        return "Socket error";
    case SERVER_INVALID_STATE:
        return "Invalid state";
    case SERVER_THREAD_ERROR:
        return "Thread creation error";
    case SERVER_MAX_CLIENTS_REACHED:
        return "Maximum clients reached";
    case SERVER_NOT_RUNNING:
        return "Server not running";
    case SERVER_CLIENT_DISCONNECTED:
        return "Client disconnected";
    case SERVER_TIMEOUT:
        return "Operation timed out";
    case SERVER_CLIENT_NOT_FOUND:
        return "Client not found";
    case SERVER_ERROR:
    default:
        return "Unknown error";
    }
}

///////////////////////////////////////////
/// networkServerCreateDefaultConfig
///////////////////////////////////////////
ServerConfig networkServerCreateDefaultConfig(void)
{
    ServerConfig l_tConfig;

    l_tConfig.t_usPort = DEFAULT_SERVER_PORT;
    l_tConfig.t_pcBindAddress = NULL;
    l_tConfig.t_iMaxClients = DEFAULT_MAX_CLIENTS;
    l_tConfig.t_iBacklog = DEFAULT_BACKLOG;
    l_tConfig.t_bUseTimeout = false;
    l_tConfig.t_iReceiveTimeout = DEFAULT_RECV_TIMEOUT;

    return l_tConfig;
}

//-----------------------------------------------------------------------------
// Server Connection Abstraction Implementation
//-----------------------------------------------------------------------------

///////////////////////////////////////////
/// networkServerConnect
///////////////////////////////////////////
int networkServerConnect(void)
{
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Server not initialized");
        return SERVER_INVALID_STATE;
    }

    // check if the server is already connected
    if (s_ptServerInstance->t_ptSocket != NULL)
    {
        X_LOG_TRACE("Server already connected");
        return SERVER_OK;
    }

    // lock the mutex
    mutexLock(&s_ptServerInstance->t_tMutex);

    // create the server socket
    s_ptServerInstance->t_ptSocket = networkCreateSocket(NETWORK_SOCK_TCP);
    if (s_ptServerInstance->t_ptSocket == NULL)
    {
        X_LOG_TRACE("Failed to create server socket");
        mutexUnlock(&s_ptServerInstance->t_tMutex);
        return SERVER_SOCKET_ERROR;
    }

    // bind the server socket
    int l_iResult = networkBind(s_ptServerInstance->t_ptSocket, &s_ptServerInstance->t_tAddress);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to bind server socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
        mutexUnlock(&s_ptServerInstance->t_tMutex);
        return SERVER_SOCKET_ERROR;
    }

    // start listening
    l_iResult = networkListen(s_ptServerInstance->t_ptSocket, s_ptServerInstance->t_sConfig.t_iBacklog);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to listen on server socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
        mutexUnlock(&s_ptServerInstance->t_tMutex);
        return SERVER_SOCKET_ERROR;
    }

    mutexUnlock(&s_ptServerInstance->t_tMutex);

    X_LOG_TRACE("Server connected and listening on port %d", s_ptServerInstance->t_sConfig.t_usPort);
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverThreadFunc
///////////////////////////////////////////
static void *serverThreadFunc(void *p_pvArg)
{
    serverCtx *p_ptServer = (serverCtx *)p_pvArg;
    if (p_ptServer == NULL)
    {
        X_LOG_TRACE("Invalid server context in server thread");
        return NULL;
    }

    X_LOG_TRACE("Server thread started");

    // get the task context to check the stop flag
    xOsTaskCtx *p_ptTaskCtx = &p_ptServer->t_tTask;

    // main loop of the server
    while (p_ptServer->t_bRunning &&
           atomic_load(&p_ptTaskCtx->a_iStopFlag) != OS_TASK_STOP_REQUEST)
    {
        // wait for a client connection with timeout
        int l_iActivity = networkWaitForActivity(p_ptServer->t_ptSocket, SERVER_ACCEPT_TIMEOUT);

        // check if the server should stop
        if (!p_ptServer->t_bRunning ||
            atomic_load(&p_ptTaskCtx->a_iStopFlag) == OS_TASK_STOP_REQUEST)
        {
            break;
        }

        // if no activity, continue waiting
        if (l_iActivity <= 0)
        {
            continue;
        }

        // accept the new client connection
        NetworkAddress l_tClientAddress;
        NetworkSocket *l_ptClientSocket = networkAccept(p_ptServer->t_ptSocket, &l_tClientAddress);

        if (l_ptClientSocket == NULL)
        {
            // if the server stops, it's normal
            if (!p_ptServer->t_bRunning)
            {
                break;
            }

            X_LOG_TRACE("Accept failed, continuing");
            continue;
        }

        X_LOG_TRACE("New client connection from %s:%d",
                    l_tClientAddress.t_cAddress, l_tClientAddress.t_usPort);

        // check the number of clients before creating the new instance
        mutexLock(&p_ptServer->t_tMutex);
        bool l_bMaxClients = (p_ptServer->t_iNumClients >= MAX_CLIENTS);
        mutexUnlock(&p_ptServer->t_tMutex);

        if (l_bMaxClients)
        {
            X_LOG_TRACE("Maximum number of clients reached, rejecting connection");
            networkCloseSocket(l_ptClientSocket);
            continue;
        }

        // create the client context
        clientCtx *l_ptClient = (clientCtx *)malloc(sizeof(clientCtx));
        if (l_ptClient == NULL)
        {
            X_LOG_TRACE("Failed to allocate memory for client");
            networkCloseSocket(l_ptClientSocket);
            continue;
        }

        // initialize the client
        memset(l_ptClient, 0, sizeof(clientCtx));
        l_ptClient->t_ptSocket = l_ptClientSocket;
        l_ptClient->t_tAddress = l_tClientAddress;
        l_ptClient->t_bConnected = true;
        l_ptClient->t_ptServer = p_ptServer;

        // configure the client task
        l_ptClient->t_tTask.t_ptTask = clientThreadFunc;
        l_ptClient->t_tTask.t_ptTaskArg = l_ptClient;
        l_ptClient->t_tTask.t_iPriority = 1;
        l_ptClient->t_tTask.t_ulStackSize = CLIENT_THREAD_STACK_SIZE;

        // configure the socket timeout if necessary
        if (p_ptServer->t_sConfig.t_bUseTimeout && p_ptServer->t_sConfig.t_iReceiveTimeout > 0)
        {
            networkSetTimeout(l_ptClientSocket, p_ptServer->t_sConfig.t_iReceiveTimeout, false);
        }

        // add the client to the table (this also sets its ID)
        int l_iResult = addClient(l_ptClient);
        if (l_iResult != SERVER_OK)
        {
            X_LOG_TRACE("Failed to add client: %s", networkServerGetErrorString(l_iResult));
            networkCloseSocket(l_ptClientSocket);
            free(l_ptClient);
            continue;
        }

        // create the client thread
        int l_iTaskResult = osTaskCreate(&l_ptClient->t_tTask);
        if (l_iTaskResult != OS_TASK_SUCCESS)
        {
            X_LOG_TRACE("Failed to create client thread: %s",
                        osTaskGetErrorString(l_iTaskResult));

            // Error cleanup: remove from table, close socket, free memory
            removeClient(l_ptClient);
            networkCloseSocket(l_ptClientSocket);
            free(l_ptClient);
            continue;
        }
    }

    X_LOG_TRACE("Server thread terminated");
    return NULL;
}

///////////////////////////////////////////
/// clientThreadFunc
///////////////////////////////////////////
static void *clientThreadFunc(void *p_pvArg)
{
    clientCtx *p_ptClient = (clientCtx *)p_pvArg;
    if (p_ptClient == NULL || p_ptClient->t_ptServer == NULL || p_ptClient->t_ptSocket == NULL)
    {
        X_LOG_TRACE("Invalid client context in client thread");
        return NULL;
    }

    serverCtx *p_ptServer = p_ptClient->t_ptServer;
    uint8_t p_aucBuffer[1024];
    int p_iReceived;

    X_LOG_TRACE("Client thread started for %s:%d (Client ID: %u)",
                p_ptClient->t_tAddress.t_cAddress,
                p_ptClient->t_tAddress.t_usPort,
                p_ptClient->t_tId);

    // main loop of the client
    while (p_ptClient->t_bConnected && p_ptServer->t_bRunning &&
           atomic_load(&p_ptClient->t_tTask.a_iStopFlag) != OS_TASK_STOP_REQUEST)
    {
        // STEP 1: First receive the size (2 bytes) - FIXED: use complete receive
        p_iReceived = networkReceiveComplete(p_ptClient->t_ptSocket, p_aucBuffer, sizeof(uint16_t));

        X_LOG_TRACE("Size data received: %d bytes", p_iReceived);

        if (p_iReceived == sizeof(uint16_t))
        {
            // Extract payload size
            uint16_t l_usPayloadSize;
            memcpy(&l_usPayloadSize, p_aucBuffer, sizeof(uint16_t));
            uint32_t l_ulPayloadSize = NET_TO_HOST_SHORT(l_usPayloadSize);

            // Validate payload size before allocation
            if (l_ulPayloadSize > SERVER_MAX_BUFFER_SIZE)
            {
                X_LOG_TRACE("Payload size too large: %u bytes (max: %d)", l_ulPayloadSize, SERVER_MAX_BUFFER_SIZE);
                continue;
            }

            X_LOG_TRACE("Received payload size: %u bytes", l_ulPayloadSize);

            // STEP 2: Allocate memory for the message (header + payload)
            uint8_t *l_pucMessageBuffer = (uint8_t *)malloc(1 + l_ulPayloadSize); // 1 byte header + payload
            if (l_pucMessageBuffer == NULL)
            {
                X_LOG_TRACE("Failed to allocate memory for message buffer (%u bytes)", 1 + l_ulPayloadSize);
                continue;
            }

            // STEP 3: Receive the message (header + payload) - FIXED: use complete receive
            p_iReceived = networkReceiveComplete(p_ptClient->t_ptSocket, l_pucMessageBuffer, 1 + l_ulPayloadSize);

            if (p_iReceived == 1 + (int)l_ulPayloadSize)
            {
                // Process the received message
                uint8_t l_ucMsgType;

                // Parse the message header (extract the type)
                if (parseMessageHeader(l_pucMessageBuffer, p_iReceived, &l_ucMsgType))
                {
                    // Call the message handler if it is defined
                    mutexLock(&p_ptServer->t_tMutex);
                    MessageHandler l_pfHandler = p_ptServer->t_pfHandler;
                    mutexUnlock(&p_ptServer->t_tMutex);

                    if (l_pfHandler != NULL)
                    {
                        network_message_t l_sMessage;
                        l_sMessage.t_iHeader[0] = l_ucMsgType;
                        l_sMessage.t_ptucPayload = l_pucMessageBuffer + 1; // Skip header byte
                        l_sMessage.t_iPayloadSize = l_ulPayloadSize;

                        // Call the handler function
                        l_pfHandler(p_ptClient, &l_sMessage);
                    }
                    else
                    {
                        X_LOG_TRACE("No message handler defined for message type 0x%02X", l_ucMsgType);
                    }
                }
                else
                {
                    X_LOG_TRACE("Failed to parse message header");
                }
            }
            else if (p_iReceived > 0)
            {
                X_LOG_TRACE("Incomplete message: expected %u bytes, got %d", 1 + l_ulPayloadSize, p_iReceived);
            }
            else
            {
                X_LOG_TRACE("Error receiving message data: %d", p_iReceived);
            }

            // Free the allocated memory
            free(l_pucMessageBuffer);
        }
        else if (p_iReceived > 0)
        {
            X_LOG_TRACE("Incomplete size data: expected %lu bytes, got %d", sizeof(uint16_t), p_iReceived);
        }
        else if (p_iReceived == NETWORK_TIMEOUT)
        {
            // the timeout is normal when it is enabled
            continue;
        }
        else
        {
            // error or disconnection
            X_LOG_TRACE("Client %u disconnected or error: %s",
                        p_ptClient->t_tId, networkGetErrorString(p_iReceived));
            clientThreadCleanup(p_ptClient);
            return NULL;
        }
    }

    // Normal termination (loop ended)
    clientThreadCleanup(p_ptClient);
    return NULL;
}

///////////////////////////////////////////
/// cleanupAndFreeClient
/// Centralized client cleanup: stops thread, closes socket, removes from table, frees memory
///////////////////////////////////////////
static void cleanupAndFreeClient(clientCtx *p_ptClient)
{
    if (p_ptClient == NULL)
    {
        return;
    }

    X_LOG_TRACE("Cleaning up client %u", p_ptClient->t_tId);

    // Mark client as disconnected
    p_ptClient->t_bConnected = false;

    // Close socket to unblock client thread if it's still running
    if (p_ptClient->t_ptSocket != NULL)
    {
        networkCloseSocket(p_ptClient->t_ptSocket);
        p_ptClient->t_ptSocket = NULL;
    }

    // Stop the client thread if it exists and is running
    if (p_ptClient->t_tTask.t_ptTask != NULL)
    {
        int l_iResult = osTaskStop(&p_ptClient->t_tTask, OS_TASK_STOP_TIMEOUT);
        if (l_iResult != OS_TASK_SUCCESS)
        {
            X_LOG_TRACE("Failed to stop client thread gracefully: %s", osTaskGetErrorString(l_iResult));
            osTaskEnd(&p_ptClient->t_tTask);
        }

        // Wait for thread termination
        osTaskWait(&p_ptClient->t_tTask, NULL);
    }

    // Remove from client table
    removeClient(p_ptClient);

    // Free the client structure
    free(p_ptClient);

    X_LOG_TRACE("Client cleanup completed");
}

///////////////////////////////////////////
/// clientThreadCleanup
/// Minimal cleanup when client thread terminates (cannot stop itself)
///////////////////////////////////////////
static void clientThreadCleanup(clientCtx *p_ptClient)
{
    if (p_ptClient == NULL)
    {
        return;
    }

    X_LOG_TRACE("Client %u disconnecting: %s:%d",
                p_ptClient->t_tId,
                p_ptClient->t_tAddress.t_cAddress,
                p_ptClient->t_tAddress.t_usPort);

    // Mark as disconnected
    p_ptClient->t_bConnected = false;

    // Close socket
    if (p_ptClient->t_ptSocket != NULL)
    {
        networkCloseSocket(p_ptClient->t_ptSocket);
        p_ptClient->t_ptSocket = NULL;
    }

    // Remove from client table
    removeClient(p_ptClient);

    // Free the client context (safe to do in thread termination)
    free(p_ptClient);

    X_LOG_TRACE("Client thread terminated");
}

///////////////////////////////////////////
/// parseMessageHeader
///////////////////////////////////////////
static bool parseMessageHeader(const uint8_t *p_ptucData,
                               int p_iSize,
                               uint8_t *p_ptucMsgType)
{
    if (p_ptucData == NULL || p_iSize < 1 || p_ptucMsgType == NULL)
    {
        return false;
    }

    // Extract the message type (now the first byte)
    *p_ptucMsgType = p_ptucData[0];

    return true;
}

///////////////////////////////////////////
/// isClientConnected
/// Validates if a client is properly connected and ready for communication
///////////////////////////////////////////
static bool isClientConnected(clientCtx *p_ptClient)
{
    if (p_ptClient == NULL)
    {
        return false;
    }

    return p_ptClient->t_bConnected && p_ptClient->t_ptSocket != NULL;
}

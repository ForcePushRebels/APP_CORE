////////////////////////////////////////////////////////////
//  Network Server Implementation
//  Provides a streamlined API for bidirectional Java-C communication
//
// general disclosure: copy or share the file is forbidden
// Written : 18/04/2025
////////////////////////////////////////////////////////////

#include "networkServer.h"
#include "handleNetworkMessage.h"
#include "xLog.h"
#include <stdlib.h>
#include <string.h>

//-----------------------------------------------------------------------------
// Constants & Defines
//-----------------------------------------------------------------------------

#define SERVER_THREAD_STACK_SIZE (64 * 1024) // 64KB stack for the server thread
#define CLIENT_THREAD_STACK_SIZE (32 * 1024) // 32KB stack for the client threads
#define SERVER_ACCEPT_TIMEOUT 2000           // 500ms timeout for accept
#define SERVER_MAX_BUFFER_SIZE 4096          // Max buffer size
#define MAX_CLIENTS 10                       // Max number of clients

// Hash table optimization for O(1) client lookup
#define CLIENT_HASH_TABLE_SIZE 16 // Must be power of 2 for fast modulo
#define CLIENT_HASH_MASK (CLIENT_HASH_TABLE_SIZE - 1)

//-----------------------------------------------------------------------------
// Client Structure
//-----------------------------------------------------------------------------

struct client_ctx_t
{
    ClientID t_tId;                // unique client ID
    NetworkSocket *t_ptSocket;     // client socket
    NetworkAddress t_tAddress;     // client address
    bool t_bConnected;             // connection status
    xOsTaskCtx t_tTask;            // client task context
    pthread_rwlock_t t_tRwLock;    // read-write lock for optimized concurrent access
    void *t_ptUserData;            // user data
    serverCtx *t_ptServer;         // reference to the server
    bool t_bCleanupInProgress;     // prevent double cleanup
    struct client_ctx_t *t_ptNext; // next client in hash bucket for collision handling
};

//-----------------------------------------------------------------------------
// Server Structure
//-----------------------------------------------------------------------------

struct server_ctx_t
{
    NetworkSocket *t_ptSocket;                              // server socket
    NetworkAddress t_tAddress;                              // server address
    pthread_rwlock_t t_tRwLock;                             // read-write lock for better concurrent access
    xOsTaskCtx t_tTask;                                     // server task context
    bool t_bRunning;                                        // server running status
    ServerConfig t_sConfig;                                 // server configuration
    ClientID t_tNextClientId;                               // next client ID to assign
    clientCtx *t_ptClientHashTable[CLIENT_HASH_TABLE_SIZE]; // hash table for O(1) client lookup
    int t_iNumClients;                                      // number of connected clients

    ProtocolFrameBuffer *t_ptBroadcastBuffer; // reusable buffer for broadcast optimization
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
static bool parseMessageHeader(const uint8_t *p_ptucData, int p_iSize, uint8_t *p_ptucMsgType);
static clientCtx *findClientById(ClientID p_tClientId);
static int addClient(clientCtx *p_ptClient);
static int removeClient(clientCtx *p_ptClient);
static bool isClientConnected(clientCtx *p_ptClient);

///////////////////////////////////////////
/// rwlock helper functions
///////////////////////////////////////////
static inline int rwlockReadLock(pthread_rwlock_t *p_ptRwLock);
static inline int rwlockWriteLock(pthread_rwlock_t *p_ptRwLock);
static inline int rwlockUnlock(pthread_rwlock_t *p_ptRwLock);
static inline uint32_t hashClientId(ClientID p_tClientId);

///////////////////////////////////////////
/// networkServerInit
///////////////////////////////////////////
int networkServerInit(void)
{
    int l_iReturn = 0;

    // create the server
    s_ptServerInstance = (serverCtx *)malloc(sizeof(serverCtx));
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Failed to allocate server memory");
        return SERVER_MEMORY_ERROR;
    }

    // initialize the server
    memset(s_ptServerInstance, 0, sizeof(serverCtx));

    // create the rwlock
    l_iReturn = pthread_rwlock_init(&s_ptServerInstance->t_tRwLock, NULL);
    if (l_iReturn != 0)
    {
        X_LOG_TRACE("Failed to create server rwlock");
        free(s_ptServerInstance);
        s_ptServerInstance = NULL;
        X_LOG_TRACE("Failed to create server rwlock: %s", strerror(l_iReturn));
        return SERVER_ERROR;
    }

    // Initialize broadcast buffer for performance optimization
    s_ptServerInstance->t_ptBroadcastBuffer = protocolCreateFrameBuffer(PROTOCOL_MAX_FRAME_SIZE);
    if (!s_ptServerInstance->t_ptBroadcastBuffer)
    {
        X_LOG_TRACE("Failed to create broadcast buffer");
        pthread_rwlock_destroy(&s_ptServerInstance->t_tRwLock);
        free(s_ptServerInstance);
        s_ptServerInstance = NULL;
        return SERVER_MEMORY_ERROR;
    }

    // set the default configuration
    s_ptServerInstance->t_sConfig = networkServerCreateDefaultConfig();
    s_ptServerInstance->t_tNextClientId = 1; // start at 1, 0 is INVALID_CLIENT_ID

    // Initialize the dedicated message handler system
    initMessageHandlerSystem();

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

    // lock for writing
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    // copy the configuration
    memcpy(&s_ptServerInstance->t_sConfig, p_ptConfig, sizeof(ServerConfig));

    // configure the server address
    s_ptServerInstance->t_tAddress
        = networkMakeAddress(p_ptConfig->t_pcBindAddress ? p_ptConfig->t_pcBindAddress : "0.0.0.0", p_ptConfig->t_usPort);

    // configure the server task
    s_ptServerInstance->t_tTask.t_ptTask = serverThreadFunc;
    s_ptServerInstance->t_tTask.t_ptTaskArg = s_ptServerInstance;
    s_ptServerInstance->t_tTask.t_iPriority = 1;
    s_ptServerInstance->t_tTask.t_ulStackSize = SERVER_THREAD_STACK_SIZE;

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    X_LOG_TRACE("Server configured on port %d", p_ptConfig->t_usPort);
    return SERVER_OK;
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
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);
    s_ptServerInstance->t_bRunning = true;
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    // create the server thread
    int l_iTaskResult = osTaskCreate(&s_ptServerInstance->t_tTask);
    if (l_iTaskResult != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("Failed to create server thread: %s", osTaskGetErrorString(l_iTaskResult));

        rwlockWriteLock(&s_ptServerInstance->t_tRwLock);
        s_ptServerInstance->t_bRunning = false;
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);

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

    // lock for writing
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    // mark the server as stopped
    s_ptServerInstance->t_bRunning = false;

    // close the server socket to unblock accept() in the server thread
    if (s_ptServerInstance->t_ptSocket != NULL)
    {
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
    }

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

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
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);
    for (int i = 0; i < CLIENT_HASH_TABLE_SIZE; i++)
    {
        clientCtx *l_ptClient = s_ptServerInstance->t_ptClientHashTable[i];
        while (l_ptClient != NULL)
        {
            clientCtx *l_ptNext = l_ptClient->t_ptNext;
            // disconnect the client outside the lock to avoid a deadlock
            rwlockUnlock(&s_ptServerInstance->t_tRwLock);
            networkServerDisconnectClient(l_ptClient->t_tId);
            rwlockWriteLock(&s_ptServerInstance->t_tRwLock);
            l_ptClient = l_ptNext;
        }
    }
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

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

    // cleanup the message handler system
    cleanupMessageHandlerSystem();

    // destroy the rwlock and broadcast buffer
    pthread_rwlock_destroy(&s_ptServerInstance->t_tRwLock);

    if (s_ptServerInstance->t_ptBroadcastBuffer)
    {
        protocolFreeFrameBuffer(s_ptServerInstance->t_ptBroadcastBuffer);
    }

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
/// findClientById - O(1) hash table lookup
///////////////////////////////////////////
static clientCtx *findClientById(ClientID p_tClientId)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return NULL;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    uint32_t l_ulHash = hashClientId(p_tClientId);
    clientCtx *l_ptClient = s_ptServerInstance->t_ptClientHashTable[l_ulHash];

    while (l_ptClient != NULL)
    {
        if (l_ptClient->t_tId == p_tClientId)
        {
            rwlockUnlock(&s_ptServerInstance->t_tRwLock);
            return l_ptClient;
        }
        l_ptClient = l_ptClient->t_ptNext;
    }

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    return NULL;
}

///////////////////////////////////////////
/// addClient - O(1) hash table insertion
///////////////////////////////////////////
static int addClient(clientCtx *p_ptClient)
{
    if (s_ptServerInstance == NULL || p_ptClient == NULL)
    {
        return SERVER_INVALID_PARAM;
    }

    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    // check if we have reached the maximum number of clients
    if (s_ptServerInstance->t_iNumClients >= MAX_CLIENTS)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        X_LOG_TRACE("Maximum number of clients reached");
        return SERVER_MAX_CLIENTS_REACHED;
    }

    // assign a unique ID
    p_ptClient->t_tId = s_ptServerInstance->t_tNextClientId++;

    // if we reach the limit, restart at 1
    if (s_ptServerInstance->t_tNextClientId == INVALID_CLIENT_ID)
    {
        s_ptServerInstance->t_tNextClientId = 1;
    }

    // insert into hash table
    uint32_t l_ulHash = hashClientId(p_ptClient->t_tId);
    p_ptClient->t_ptNext = s_ptServerInstance->t_ptClientHashTable[l_ulHash];
    s_ptServerInstance->t_ptClientHashTable[l_ulHash] = p_ptClient;
    s_ptServerInstance->t_iNumClients++;

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    X_LOG_TRACE("Client %u added, total clients: %d", p_ptClient->t_tId, s_ptServerInstance->t_iNumClients);
    return SERVER_OK;
}

///////////////////////////////////////////
/// removeClient - O(1) hash table removal
///////////////////////////////////////////
static int removeClient(clientCtx *p_ptClient)
{
    if (s_ptServerInstance == NULL || p_ptClient == NULL)
    {
        return SERVER_INVALID_PARAM;
    }

    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    uint32_t l_ulHash = hashClientId(p_ptClient->t_tId);
    clientCtx **l_pptCurrent = &s_ptServerInstance->t_ptClientHashTable[l_ulHash];

    while (*l_pptCurrent != NULL)
    {
        if (*l_pptCurrent == p_ptClient)
        {
            *l_pptCurrent = p_ptClient->t_ptNext;
            s_ptServerInstance->t_iNumClients--;

            rwlockUnlock(&s_ptServerInstance->t_tRwLock);
            X_LOG_TRACE("Client %u removed, total clients: %d", p_ptClient->t_tId, s_ptServerInstance->t_iNumClients);
            return SERVER_OK;
        }
        l_pptCurrent = &(*l_pptCurrent)->t_ptNext;
    }

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    X_LOG_TRACE("Client %u not found in table", p_ptClient->t_tId);
    return SERVER_CLIENT_NOT_FOUND;
}

///////////////////////////////////////////
/// networkServerGetClientAddress - Optimisé avec rwlock et hash table
///////////////////////////////////////////
bool networkServerGetClientAddress(ClientID p_tClientId, char *p_ptcBuffer, int p_iSize)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID || p_ptcBuffer == NULL || p_iSize <= 0)
    {
        return false;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return false;
    }

    // copy the address to the buffer while holding server rwlock
    strncpy(p_ptcBuffer, l_ptClient->t_tAddress.t_cAddress, p_iSize - 1);
    p_ptcBuffer[p_iSize - 1] = '\0';

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    return true;
}

///////////////////////////////////////////
/// networkServerGetClientPort - Optimisé avec rwlock et hash table
///////////////////////////////////////////
uint16_t networkServerGetClientPort(ClientID p_tClientId)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return 0;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return 0;
    }

    uint16_t l_usPort = l_ptClient->t_tAddress.t_usPort;
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    return l_usPort;
}

///////////////////////////////////////////
/// networkServerDisconnectClient - Optimisé avec rwlock et hash table
///////////////////////////////////////////
int networkServerDisconnectClient(ClientID p_tClientId)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return SERVER_CLIENT_NOT_FOUND;
    }

    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_CLIENT_NOT_FOUND;
    }

    l_ptClient->t_bCleanupInProgress = true; // Mark for cleanup
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    // Use centralized cleanup function
    cleanupAndFreeClient(l_ptClient);

    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerSetClientUserData - Optimisé avec rwlock
///////////////////////////////////////////
int networkServerSetClientUserData(ClientID p_tClientId, void *p_pvUserData)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return SERVER_CLIENT_NOT_FOUND;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_CLIENT_NOT_FOUND;
    }

    // Lock client rwlock to protect user data access
    if (pthread_rwlock_wrlock(&l_ptClient->t_tRwLock) != 0)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_ERROR;
    }

    l_ptClient->t_ptUserData = p_pvUserData;

    pthread_rwlock_unlock(&l_ptClient->t_tRwLock);
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    return SERVER_OK;
}

///////////////////////////////////////////
/// networkServerGetClientUserData - Optimisé avec rwlock
///////////////////////////////////////////
void *networkServerGetClientUserData(ClientID p_tClientId)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return NULL;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return NULL;
    }

    // Lock client rwlock to protect user data access
    if (pthread_rwlock_rdlock(&l_ptClient->t_tRwLock) != 0)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return NULL;
    }

    void *l_pvUserData = l_ptClient->t_ptUserData;

    pthread_rwlock_unlock(&l_ptClient->t_tRwLock);
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    return l_pvUserData;
}

///////////////////////////////////////////
/// networkServerSendToClient - Optimisé avec rwlock et hash table
///////////////////////////////////////////
int networkServerSendToClient(ClientID p_tClientId, const void *p_pvData, int p_iSize)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID || p_pvData == NULL || p_iSize <= 0)
    {
        return SERVER_INVALID_PARAM;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_CLIENT_NOT_FOUND;
    }

    // Check connection status more comprehensively
    if (!l_ptClient->t_bConnected || l_ptClient->t_ptSocket == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_CLIENT_DISCONNECTED;
    }

    int l_iResult = networkSend(l_ptClient->t_ptSocket, p_pvData, p_iSize);
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    return l_iResult;
}

///////////////////////////////////////////
/// networkServerSendMessage - Optimisé avec rwlock et frame buffer
///////////////////////////////////////////
int networkServerSendMessage(ClientID p_tClientId, uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize)
{
    if (s_ptServerInstance == NULL || p_tClientId == INVALID_CLIENT_ID)
    {
        return SERVER_CLIENT_NOT_FOUND;
    }

    rwlockReadLock(&s_ptServerInstance->t_tRwLock);

    clientCtx *l_ptClient = findClientById(p_tClientId);
    if (l_ptClient == NULL || l_ptClient->t_bCleanupInProgress)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_CLIENT_NOT_FOUND;
    }

    // Lock client rwlock to protect the entire send operation
    if (pthread_rwlock_wrlock(&l_ptClient->t_tRwLock) != 0)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_ERROR;
    }

    // Check connection status directly (we already have the write lock)
    if (!l_ptClient->t_bConnected || l_ptClient->t_ptSocket == NULL || l_ptClient->t_bCleanupInProgress)
    {
        pthread_rwlock_unlock(&l_ptClient->t_tRwLock);
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_CLIENT_DISCONNECTED;
    }

    // Validate the payload size can fit in protocol
    if (!protocolIsValidPayloadSize(p_ulPayloadSize))
    {
        X_LOG_TRACE("Payload size too large for protocol: %u bytes (max: %u)", p_ulPayloadSize, PROTOCOL_MAX_PAYLOAD_SIZE);
        pthread_rwlock_unlock(&l_ptClient->t_tRwLock);
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_INVALID_PARAM;
    }

    uint8_t l_aucFrameBuffer[PROTOCOL_MAX_FRAME_SIZE];
    uint32_t l_ulFrameSize;

    int l_iProtocolResult = protocolCreateFrame((network_message_type_t)p_ucMsgType,
                                                (const uint8_t *)p_pvPayload,
                                                p_ulPayloadSize,
                                                l_aucFrameBuffer,
                                                sizeof(l_aucFrameBuffer),
                                                &l_ulFrameSize);

    if (l_iProtocolResult != PROTOCOL_OK)
    {
        X_LOG_TRACE("Failed to create protocol frame: %s", protocolGetErrorString(l_iProtocolResult));
        pthread_rwlock_unlock(&l_ptClient->t_tRwLock);
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_ERROR;
    }

    // ATOMIC SEND: Single network operation for entire message
    int l_iSentBytes = networkSend(l_ptClient->t_ptSocket, l_aucFrameBuffer, l_ulFrameSize);

    pthread_rwlock_unlock(&l_ptClient->t_tRwLock);
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    if (l_iSentBytes < 0)
    {
        X_LOG_TRACE("Failed to send complete message to client: %s", networkGetErrorString(l_iSentBytes));
        return SERVER_SOCKET_ERROR;
    }

    if ((uint32_t)l_iSentBytes != l_ulFrameSize)
    {
        X_LOG_TRACE("Incomplete send: expected %u bytes, sent %d bytes", l_ulFrameSize, l_iSentBytes);
        return SERVER_SOCKET_ERROR;
    }

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

    // lock for writing
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    // create the server socket
    s_ptServerInstance->t_ptSocket = networkCreateSocket(NETWORK_SOCK_TCP);
    if (s_ptServerInstance->t_ptSocket == NULL)
    {
        X_LOG_TRACE("Failed to create server socket");
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_SOCKET_ERROR;
    }

    // bind the server socket
    int l_iResult = networkBind(s_ptServerInstance->t_ptSocket, &s_ptServerInstance->t_tAddress);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to bind server socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_SOCKET_ERROR;
    }

    // start listening
    l_iResult = networkListen(s_ptServerInstance->t_ptSocket, s_ptServerInstance->t_sConfig.t_iBacklog);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to listen on server socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(s_ptServerInstance->t_ptSocket);
        s_ptServerInstance->t_ptSocket = NULL;
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        return SERVER_SOCKET_ERROR;
    }

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    X_LOG_TRACE("Server connected and listening on port %d", s_ptServerInstance->t_sConfig.t_usPort);
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverThreadFunc
///////////////////////////////////////////
static void *serverThreadFunc(void *p_ptArg)
{
    serverCtx *p_ptServer = (serverCtx *)p_ptArg;
    if (p_ptServer == NULL)
    {
        X_LOG_TRACE("Invalid server context in server thread");
        return NULL;
    }

    X_LOG_TRACE("Server thread started");

    // get the task context to check the stop flag
    xOsTaskCtx *p_ptTaskCtx = &p_ptServer->t_tTask;

    // Cache stop flag for better performance - flag rarely changes during normal operation
    int l_iStopFlag = atomic_load_explicit(&p_ptTaskCtx->a_iStopFlag, memory_order_acquire);

    // main loop of the server
    while (p_ptServer->t_bRunning && l_iStopFlag != OS_TASK_STOP_REQUEST)
    {
        // wait for a client connection with timeout
        int l_iActivity = networkWaitForActivity(p_ptServer->t_ptSocket, SERVER_ACCEPT_TIMEOUT);

        // Refresh stop flag periodically with relaxed ordering for performance
        l_iStopFlag = atomic_load_explicit(&p_ptTaskCtx->a_iStopFlag, memory_order_relaxed);

        // check if the server should stop
        if (!p_ptServer->t_bRunning || l_iStopFlag == OS_TASK_STOP_REQUEST)
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

        X_LOG_TRACE("New client connection from %s:%d", l_tClientAddress.t_cAddress, l_tClientAddress.t_usPort);

        // check the number of clients before creating the new instance
        rwlockReadLock(&p_ptServer->t_tRwLock);
        bool l_bMaxClients = (p_ptServer->t_iNumClients >= MAX_CLIENTS);
        rwlockUnlock(&p_ptServer->t_tRwLock);

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
        l_ptClient->t_bCleanupInProgress = false;

        // Initialize client rwlock
        if (pthread_rwlock_init(&l_ptClient->t_tRwLock, NULL) != 0)
        {
            X_LOG_TRACE("Failed to create client rwlock");
            networkCloseSocket(l_ptClientSocket);
            free(l_ptClient);
            continue;
        }

        // configure the socket timeout if necessary
        if (p_ptServer->t_sConfig.t_bUseTimeout && p_ptServer->t_sConfig.t_iReceiveTimeout > 0)
        {
            networkSetTimeout(l_ptClientSocket, p_ptServer->t_sConfig.t_iReceiveTimeout, false);
        }

        // configure the client task BEFORE adding to table
        l_ptClient->t_tTask.t_ptTask = clientThreadFunc;
        l_ptClient->t_tTask.t_ptTaskArg = l_ptClient;
        l_ptClient->t_tTask.t_iPriority = 1;
        l_ptClient->t_tTask.t_ulStackSize = CLIENT_THREAD_STACK_SIZE;

        // create the client thread BEFORE adding to table
        int l_iTaskResult = osTaskCreate(&l_ptClient->t_tTask);
        if (l_iTaskResult != OS_TASK_SUCCESS)
        {
            X_LOG_TRACE("Failed to create client thread: %s", osTaskGetErrorString(l_iTaskResult));
            pthread_rwlock_destroy(&l_ptClient->t_tRwLock);
            networkCloseSocket(l_ptClientSocket);
            free(l_ptClient);
            continue;
        }

        // CRITICAL: Add client to table ONLY after full initialization
        // This prevents race conditions where other threads find an incomplete client
        int l_iResult = addClient(l_ptClient);
        if (l_iResult != SERVER_OK)
        {
            X_LOG_TRACE("Failed to add client: %s", networkServerGetErrorString(l_iResult));

            // Error cleanup: stop thread, destroy rwlock, close socket, free memory
            osTaskStop(&l_ptClient->t_tTask, OS_TASK_STOP_TIMEOUT);
            osTaskWait(&l_ptClient->t_tTask, NULL);
            pthread_rwlock_destroy(&l_ptClient->t_tRwLock);
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

    // Cache stop flag for better performance in main client loop
    int l_iClientStopFlag = atomic_load_explicit(&p_ptClient->t_tTask.a_iStopFlag, memory_order_acquire);
    int l_iLoopCounter = 0; // Simple counter for periodic flag refresh

    // main loop of the client
    while (p_ptClient->t_bConnected && p_ptServer->t_bRunning && l_iClientStopFlag != OS_TASK_STOP_REQUEST)
    {
        if (++l_iLoopCounter >= 10)
        {
            l_iLoopCounter = 0;
            l_iClientStopFlag = atomic_load_explicit(&p_ptClient->t_tTask.a_iStopFlag, memory_order_relaxed);
        }

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
            p_iReceived = networkReceiveComplete(p_ptClient->t_ptSocket, l_pucMessageBuffer, l_ulPayloadSize);

            if (p_iReceived == (int)l_ulPayloadSize)
            {
                // Process the received message
                uint8_t l_ucMsgType;

                // Parse the message header (extract the type)
                if (parseMessageHeader(l_pucMessageBuffer, p_iReceived, &l_ucMsgType))
                {
                    // Create network message structure
                    network_message_t l_sMessage;
                    l_sMessage.t_iHeader[0] = l_ucMsgType;
                    l_sMessage.t_ptucPayload = l_pucMessageBuffer + 1; // Skip header byte
                    l_sMessage.t_iPayloadSize = l_ulPayloadSize - 1;   // Subtract header byte

                    // Use the dedicated message handling system
                    handleNetworkMessage(p_ptClient, &l_sMessage);
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
            X_LOG_TRACE("Client %u disconnected or error: %s", p_ptClient->t_tId, networkGetErrorString(p_iReceived));
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

    // Check if cleanup already in progress to prevent double cleanup
    if (p_ptClient->t_bCleanupInProgress)
    {
        X_LOG_TRACE("Client %u cleanup already in progress", p_ptClient->t_tId);
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

    // Destroy client rwlock
    pthread_rwlock_destroy(&p_ptClient->t_tRwLock);

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

    // Mark for cleanup to prevent external cleanup during self-cleanup
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);
    p_ptClient->t_bCleanupInProgress = true;
    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

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

    // Destroy client rwlock
    pthread_rwlock_destroy(&p_ptClient->t_tRwLock);

    // Free the client context (safe to do in thread termination)
    free(p_ptClient);

    X_LOG_TRACE("Client thread terminated");
}

///////////////////////////////////////////
/// parseMessageHeader
///////////////////////////////////////////
static bool parseMessageHeader(const uint8_t *p_ptucData, int p_iSize, uint8_t *p_ptucMsgType)
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
/// isClientConnected - Optimisé avec rwlock
/// Validates if a client is properly connected and ready for communication
///////////////////////////////////////////
static bool isClientConnected(clientCtx *p_ptClient)
{
    if (p_ptClient == NULL)
    {
        return false;
    }

    // Lock client rwlock to safely check connection status
    if (pthread_rwlock_rdlock(&p_ptClient->t_tRwLock) != 0)
    {
        return false;
    }

    bool l_bConnected = p_ptClient->t_bConnected && p_ptClient->t_ptSocket != NULL;

    pthread_rwlock_unlock(&p_ptClient->t_tRwLock);
    return l_bConnected;
}

///////////////////////////////////////////
/// networkServerSendMessageToAllClients - Optimisé avec rwlock, hash table et buffer réutilisé
///////////////////////////////////////////
int networkServerSendMessageToAllClients(uint8_t p_ucMsgType, const void *p_pvPayload, uint32_t p_ulPayloadSize)
{
    if (s_ptServerInstance == NULL)
    {
        X_LOG_TRACE("Server not initialized");
        return SERVER_INVALID_STATE;
    }

    // Validate payload size early
    if (!protocolIsValidPayloadSize(p_ulPayloadSize))
    {
        X_LOG_TRACE("Payload size too large for broadcast: %u bytes", p_ulPayloadSize);
        return SERVER_INVALID_PARAM;
    }

    int l_iSuccessCount = 0;
    int l_iFailureCount = 0;

    // Use reusable broadcast buffer for better performance
    rwlockWriteLock(&s_ptServerInstance->t_tRwLock);

    // Build frame once for all clients (30-50% faster broadcast)
    int l_iFrameResult = protocolBuildFrame(s_ptServerInstance->t_ptBroadcastBuffer,
                                            (network_message_type_t)p_ucMsgType,
                                            (const uint8_t *)p_pvPayload,
                                            p_ulPayloadSize);

    if (l_iFrameResult != PROTOCOL_OK)
    {
        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
        X_LOG_TRACE("Failed to create broadcast frame: %s", protocolGetErrorString(l_iFrameResult));
        return SERVER_ERROR;
    }

    // Collect all client IDs first (to minimize rwlock hold time)
    ClientID l_atClientIds[MAX_CLIENTS];
    int l_iClientCount = 0;

    // Iterate through hash table for O(1) performance
    for (int i = 0; i < CLIENT_HASH_TABLE_SIZE; i++)
    {
        clientCtx *l_ptClient = s_ptServerInstance->t_ptClientHashTable[i];
        while (l_ptClient != NULL && l_iClientCount < MAX_CLIENTS)
        {
            if (l_ptClient->t_bConnected && !l_ptClient->t_bCleanupInProgress)
            {
                l_atClientIds[l_iClientCount++] = l_ptClient->t_tId;
            }
            l_ptClient = l_ptClient->t_ptNext;
        }
    }

    rwlockUnlock(&s_ptServerInstance->t_tRwLock);

    // Send to all collected clients (outside of server lock for better concurrency)
    for (int i = 0; i < l_iClientCount; i++)
    {
        rwlockReadLock(&s_ptServerInstance->t_tRwLock);
        clientCtx *l_ptClient = findClientById(l_atClientIds[i]);

        if (l_ptClient != NULL && l_ptClient->t_bConnected && !l_ptClient->t_bCleanupInProgress)
        {
            // Send pre-built frame directly
            int l_iSentBytes = networkSend(l_ptClient->t_ptSocket,
                                           s_ptServerInstance->t_ptBroadcastBuffer->t_pucBuffer,
                                           s_ptServerInstance->t_ptBroadcastBuffer->t_ulFrameSize);

            if (l_iSentBytes == (int)s_ptServerInstance->t_ptBroadcastBuffer->t_ulFrameSize)
            {
                l_iSuccessCount++;
            }
            else
            {
                l_iFailureCount++;
                X_LOG_TRACE("Failed to send broadcast to client %u", l_atClientIds[i]);
            }
        }
        else
        {
            l_iFailureCount++;
        }

        rwlockUnlock(&s_ptServerInstance->t_tRwLock);
    }

    X_LOG_TRACE(
        "Broadcast message type 0x%02X: %d clients succeeded, %d failed", p_ucMsgType, l_iSuccessCount, l_iFailureCount);

    return (l_iFailureCount == 0) ? SERVER_OK : SERVER_ERROR;
}

///////////////////////////////////////////
/// rwlock helper functions implementation
///////////////////////////////////////////
static inline int rwlockReadLock(pthread_rwlock_t *p_ptRwLock)
{
    return (pthread_rwlock_rdlock(p_ptRwLock) == 0) ? SERVER_OK : SERVER_ERROR;
}

static inline int rwlockWriteLock(pthread_rwlock_t *p_ptRwLock)
{
    return (pthread_rwlock_wrlock(p_ptRwLock) == 0) ? SERVER_OK : SERVER_ERROR;
}

static inline int rwlockUnlock(pthread_rwlock_t *p_ptRwLock)
{
    return (pthread_rwlock_unlock(p_ptRwLock) == 0) ? SERVER_OK : SERVER_ERROR;
}

static inline uint32_t hashClientId(ClientID p_tClientId)
{
    return p_tClientId & CLIENT_HASH_MASK;
}
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

// Server error codes
#define SERVER_OK 0x200B000
#define SERVER_ERROR 0x200B001
#define SERVER_MAX_CLIENTS_REACHED 0x200B002
#define SERVER_INVALID_STATE 0x200B003
#define SERVER_THREAD_ERROR 0x200B004
#define SERVER_CLIENT_DISCONNECTED 0x200B005
#define SERVER_SOCKET_ERROR 0x200B006
#define SERVER_MEMORY_ERROR 0x200B007
#define SERVER_TIMEOUT 0x200B008
#define SERVER_INVALID_PARAMETER 0x200B009
#define SERVER_NOT_RUNNING 0x200B00A

// Configuration du serveur
#define SERVER_DEFAULT_PORT 8080
#define SERVER_MAX_CLIENTS 5
#define SERVER_BUFFER_SIZE 4096
#define SERVER_SOCKET_TIMEOUT 20000 // ms

////////////////////////////////////////////////////////////
/// @brief Server states
////////////////////////////////////////////////////////////
typedef enum
{
    SERVER_STATE_IDLE,
    SERVER_STATE_RUNNING,
    SERVER_STATE_ERROR,
    SERVER_STATE_STOPPED
} ServerState;

////////////////////////////////////////////////////////////
/// @brief Server configuration
////////////////////////////////////////////////////////////
typedef struct
{
    uint16_t t_usPort;     // Port d'écoute
    int t_iBacklog;        // Nombre max de connexions en attente
    int t_iReceiveTimeout; // Timeout de réception (ms)
    bool t_bReuseAddr;     // Option SO_REUSEADDR
    int t_iMaxClients;     // Nombre max de clients
} serverConfig;

// Forward declaration using the same name as the typedef
typedef struct server_ctx_t serverCtx;

////////////////////////////////////////////////////////////
/// @brief Client thread structure for per-client processing
////////////////////////////////////////////////////////////
typedef struct client_thread_t
{
    NetworkSocket *t_pSocket;  // Socket client
    NetworkAddress t_tAddress; // Adresse client
    bool t_bConnected;         // État de connexion
    xOsTaskCtx t_Task;         // Contexte de tâche pour le thread client
    serverCtx *t_pServer;      // Référence au serveur parent
    void *t_pUserData;         // Données utilisateur (contexte client)
} clientThread;

////////////////////////////////////////////////////////////
/// @brief Server structure
////////////////////////////////////////////////////////////
struct server_ctx_t
{
    NetworkSocket *t_pSocket;  // Socket d'écoute principal
    NetworkAddress t_tAddress; // Adresse et port d'écoute
    xOsMutexCtx t_Mutex;       // Mutex pour synchronisation
    xOsTaskCtx t_Task;         // Contexte de tâche pour le thread serveur
    ServerState t_eState;      // État actuel du serveur
    serverConfig t_tConfig;    // Configuration du serveur

    // Limite de clients actifs
    int t_iMaxClients;
    int t_iActiveClients; // Nombre actuel de clients actifs

    // Callbacks utilisateur
    void (*t_pfOnClientConnect)(serverCtx *, clientThread *);
    void (*t_pfOnClientDisconnect)(serverCtx *, clientThread *);
    void (*t_pfOnDataReceived)(serverCtx *, clientThread *, void *, int);
};

////////////////////////////////////////////////////////////
/// @brief Initialize server structure with default values
/// @param p_pttServer Pointer to server structure
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverInit(serverCtx *p_pttServer);

////////////////////////////////////////////////////////////
/// @brief Configure server parameters
/// @param p_pttServer Pointer to server structure
/// @param p_usPort Port number (0 for default)
/// @param p_iBacklog Max pending connections (0 for default)
/// @param p_iMaxClients Maximum number of concurrent clients (0 for default)
/// @param p_ptkcAddress IP address to bind to (NULL for any)
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverConfigure(serverCtx *p_pttServer, uint16_t p_usPort, int p_iBacklog, int p_iMaxClients, const char *p_ptkcAddress);

////////////////////////////////////////////////////////////
/// @brief Start server (creates socket, binds and starts listener thread)
/// @param p_pttServer Pointer to server structure
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverStart(serverCtx *p_pttServer);

////////////////////////////////////////////////////////////
/// @brief Stop server (closes socket and stops thread)
/// @param p_pttServer Pointer to server structure
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverStop(serverCtx *p_pttServer);

////////////////////////////////////////////////////////////
/// @brief Server main thread function
/// @param p_pttServer Pointer to server structure cast as void*
/// @return Should never return, 0 on normal exit, error code otherwise
////////////////////////////////////////////////////////////
void *serverHandle(void *p_pArg);

////////////////////////////////////////////////////////////
/// @brief Client thread function - handles a single client connection
/// @param p_tClientThread Pointer to client thread structure cast as void*
/// @return Should terminate when client disconnects, 0 on normal exit
////////////////////////////////////////////////////////////
void *clientThreadFunction(void *p_tClientThread);

////////////////////////////////////////////////////////////
/// @brief Send data to client
/// @param p_tClientThread Pointer to client thread
/// @param p_pData Data buffer
/// @param p_iSize Data size
/// @return Bytes sent or error code
////////////////////////////////////////////////////////////
int serverSendToClient(clientThread *p_tClientThread, const void *p_pData, int p_iSize);

////////////////////////////////////////////////////////////
/// @brief Set callback for client connection event
/// @param p_pttServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnClientConnect(serverCtx *p_pttServer,
                             void (*p_pfCallback)(serverCtx *, clientThread *));

////////////////////////////////////////////////////////////
/// @brief Set callback for client disconnection event
/// @param p_pttServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnClientDisconnect(serverCtx *p_pttServer,
                                void (*p_pfCallback)(serverCtx *, clientThread *));

////////////////////////////////////////////////////////////
/// @brief Set callback for data received event
/// @param p_pttServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnDataReceived(serverCtx *p_pttServer,
                            void (*p_pfCallback)(serverCtx *, clientThread *, void *, int));

////////////////////////////////////////////////////////////
/// @brief Disconnect and cleanup client thread
/// @param p_tClientThread Pointer to client thread
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverDisconnectClient(clientThread *p_tClientThread);

////////////////////////////////////////////////////////////
/// @brief Set user data for client thread (context)
/// @param p_tClientThread Pointer to client thread
/// @param p_pUserData Pointer to user data
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetClientUserData(clientThread *p_tClientThread, void *p_pUserData);

////////////////////////////////////////////////////////////
/// @brief Get user data from client thread
/// @param p_tClientThread Pointer to client thread
/// @return Pointer to user data or NULL if not set
////////////////////////////////////////////////////////////
void *serverGetClientUserData(clientThread *p_tClientThread);

////////////////////////////////////////////////////////////
/// @brief Get error string for server error code
/// @param p_iError Server error code
/// @return Error description string
////////////////////////////////////////////////////////////
const char *serverGetErrorString(int p_iError);

////////////////////////////////////////////////////////////
/// @brief Destroy server and free all resources
/// @param p_pttServer Pointer to server structure
/// @return none
////////////////////////////////////////////////////////////
void destroyServer(serverCtx *p_pttServer);

#endif // SERVER_H

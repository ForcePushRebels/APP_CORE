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

// Configuration du serveur
#define SERVER_DEFAULT_PORT 8080
#define SERVER_MAX_CLIENTS 5
#define SERVER_BUFFER_SIZE 4096
#define SERVER_SOCKET_TIMEOUT 20000 // ms

////////////////////////////////////////////////////////////
/// @brief Server states
////////////////////////////////////////////////////////////
typedef enum {
    SERVER_STATE_IDLE,
    SERVER_STATE_RUNNING,
    SERVER_STATE_ERROR,
    SERVER_STATE_STOPPED
} ServerState;

////////////////////////////////////////////////////////////
/// @brief Server configuration
////////////////////////////////////////////////////////////
typedef struct {
    uint16_t t_usPort;         // Port d'écoute
    int t_iBacklog;            // Nombre max de connexions en attente
    int t_iReceiveTimeout;     // Timeout de réception (ms)
    bool t_bReuseAddr;         // Option SO_REUSEADDR
    int t_iMaxClients;         // Nombre max de clients
} ServerConfig;

// Forward declaration for circular references
struct Server;

////////////////////////////////////////////////////////////
/// @brief Client thread structure for per-client processing
////////////////////////////////////////////////////////////
typedef struct {
    NetworkSocket *t_pSocket;   // Socket client
    NetworkAddress t_tAddress;  // Adresse client
    bool t_bConnected;          // État de connexion
    xOsTaskCtx t_Task;           // Contexte de tâche pour le thread client
    struct Server *t_pServer;   // Référence au serveur parent
    void *t_pUserData;          // Données utilisateur (contexte client)
} ClientThread;

////////////////////////////////////////////////////////////
/// @brief Server structure
////////////////////////////////////////////////////////////
typedef struct Server {
    NetworkSocket *t_pSocket;   // Socket d'écoute principal
    NetworkAddress t_tAddress;  // Adresse et port d'écoute
    #ifdef NETWORK_TLS_ENABLED
        NetworkTlsConfig t_tTlsConfig;  // Configuration TLS
    #endif
    xOsMutexCtx t_Mutex;         // Mutex pour synchronisation
    xOsTaskCtx t_Task;           // Contexte de tâche pour le thread serveur
    ServerState t_eState;        // État actuel du serveur
    ServerConfig t_tConfig;      // Configuration du serveur
    
    // Limite de clients actifs
    int t_iMaxClients;
    int t_iActiveClients;       // Nombre actuel de clients actifs
    
    // Callbacks utilisateur
    void (*t_pfOnClientConnect)(struct Server*, ClientThread*);
    void (*t_pfOnClientDisconnect)(struct Server*, ClientThread*);
    void (*t_pfOnDataReceived)(struct Server*, ClientThread*, void*, int);
} Server;

////////////////////////////////////////////////////////////
/// @brief Initialize server structure with default values
/// @param p_tServer Pointer to server structure
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverInit(Server *p_tServer);

////////////////////////////////////////////////////////////
/// @brief Configure server parameters
/// @param p_tServer Pointer to server structure
/// @param p_usPort Port number (0 for default)
/// @param p_iBacklog Max pending connections (0 for default)
/// @param p_iMaxClients Maximum number of concurrent clients (0 for default)
/// @param p_cAddress IP address to bind to (NULL for any)
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverConfigure(Server *p_tServer, uint16_t p_usPort, int p_iBacklog, int p_iMaxClients, const char *p_cAddress);

////////////////////////////////////////////////////////////
/// @brief Start server (creates socket, binds and starts listener thread)
/// @param p_tServer Pointer to server structure
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverStart(Server *p_tServer);

////////////////////////////////////////////////////////////
/// @brief Stop server (closes socket and stops thread)
/// @param p_tServer Pointer to server structure
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverStop(Server *p_tServer);

////////////////////////////////////////////////////////////
/// @brief Server main thread function
/// @param p_tServer Pointer to server structure cast as void*
/// @return Should never return, 0 on normal exit, error code otherwise
////////////////////////////////////////////////////////////
void* serverThreadFunction(void *p_pArg);

////////////////////////////////////////////////////////////
/// @brief Client thread function - handles a single client connection
/// @param p_tClientThread Pointer to client thread structure cast as void*
/// @return Should terminate when client disconnects, 0 on normal exit
////////////////////////////////////////////////////////////
void* clientThreadFunction(void *p_tClientThread);

////////////////////////////////////////////////////////////
/// @brief Send data to client
/// @param p_tClientThread Pointer to client thread
/// @param p_pData Data buffer
/// @param p_iSize Data size
/// @return Bytes sent or error code
////////////////////////////////////////////////////////////
int serverSendToClient(ClientThread *p_tClientThread, const void *p_pData, int p_iSize);

////////////////////////////////////////////////////////////
/// @brief Set callback for client connection event
/// @param p_tServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnClientConnect(Server *p_tServer, 
                            void (*p_pfCallback)(Server*, ClientThread*));

////////////////////////////////////////////////////////////
/// @brief Set callback for client disconnection event
/// @param p_tServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnClientDisconnect(Server *p_tServer, 
                               void (*p_pfCallback)(Server*, ClientThread*));

////////////////////////////////////////////////////////////
/// @brief Set callback for data received event
/// @param p_tServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnDataReceived(Server *p_tServer, 
                           void (*p_pfCallback)(Server*, ClientThread*, void*, int));

////////////////////////////////////////////////////////////
/// @brief Disconnect and cleanup client thread
/// @param p_tClientThread Pointer to client thread
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverDisconnectClient(ClientThread *p_tClientThread);

////////////////////////////////////////////////////////////
/// @brief Set user data for client thread (context)
/// @param p_tClientThread Pointer to client thread
/// @param p_pUserData Pointer to user data
/// @return NETWORK_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetClientUserData(ClientThread *p_tClientThread, void *p_pUserData);

////////////////////////////////////////////////////////////
/// @brief Get user data from client thread
/// @param p_tClientThread Pointer to client thread
/// @return Pointer to user data or NULL if not set
////////////////////////////////////////////////////////////
void* serverGetClientUserData(ClientThread *p_tClientThread);

////////////////////////////////////////////////////////////
/// @brief Destroy server and free all resources
/// @param p_tServer Pointer to server structure
/// @return none
////////////////////////////////////////////////////////////
void destroyServer(Server *p_tServer);

#endif // SERVER_H

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
#include <arpa/inet.h>  // Pour htonl, htons, etc.

//-----------------------------------------------------------------------------
// Constants & Defines
//-----------------------------------------------------------------------------

#define SERVER_THREAD_STACK_SIZE    (1024*1024)  // 1MB stack pour le thread serveur
#define CLIENT_THREAD_STACK_SIZE    (64*1024)    // 64KB stack pour les threads clients
#define SERVER_ACCEPT_TIMEOUT       500          // 500ms timeout pour accept
#define SERVER_MAX_BUFFER_SIZE      4096         // Taille max buffer

//-----------------------------------------------------------------------------
// Client Structure
//-----------------------------------------------------------------------------

struct client_ctx_t 
{
    NetworkSocket*  t_ptSocket;        // Socket client
    NetworkAddress  t_tAddress;        // Adresse client
    bool            t_bConnected;      // Status de connexion
    xOsTaskCtx      t_tTask;           // Contexte de tâche thread client
    void*           t_ptUserData;      // Données utilisateur
    serverCtx*      t_ptServer;        // Référence vers le serveur
};

//-----------------------------------------------------------------------------
// Server Structure
//-----------------------------------------------------------------------------

struct server_ctx_t 
{
    NetworkSocket*  t_ptSocket;        // Socket serveur
    NetworkAddress  t_tAddress;        // Adresse serveur
    xOsMutexCtx     t_tMutex;          // Mutex de synchronisation
    xOsTaskCtx      t_tTask;           // Thread serveur
    bool            t_bRunning;        // État de fonctionnement du serveur
    ServerConfig    t_sConfig;         // Configuration
    clientCtx*      t_ptClient;        // Client connecté (un seul supporté)
    MessageHandler  t_pfHandler;       // Fonction de gestion des messages
};

//-----------------------------------------------------------------------------
// Thread Function Prototypes
//-----------------------------------------------------------------------------

static void* serverThreadFunc(void* p_ptArg);
static void* clientThreadFunc(void* p_ptArg);

//-----------------------------------------------------------------------------
// Helper Function Prototypes
//-----------------------------------------------------------------------------

static void cleanupClientResources(clientCtx* p_ptClient);
static bool parseMessageHeader(const uint8_t* p_ptucData, 
                               int p_iSize, 
                               uint32_t* p_ptulPayloadSize, 
                               uint8_t* p_ptucMsgType);

//-----------------------------------------------------------------------------
// Server Management Functions
//-----------------------------------------------------------------------------

serverCtx* serverCreate(void) 
{
    serverCtx* p_ptServer = (serverCtx*)malloc(sizeof(serverCtx));
    if (p_ptServer == NULL) 
    {
        X_LOG_TRACE("Failed to allocate server memory");
        return NULL;
    }

    // Initialiser le serveur
    memset(p_ptServer, 0, sizeof(serverCtx));
    
    // Créer le mutex
    if (mutexCreate(&p_ptServer->t_tMutex) != MUTEX_OK) 
    {
        X_LOG_TRACE("Failed to create server mutex");
        free(p_ptServer);
        return NULL;
    }
    
    // Définir la configuration par défaut
    p_ptServer->t_sConfig = serverCreateDefaultConfig();
    p_ptServer->t_ptClient = NULL;
    
    X_LOG_TRACE("Server created");
    return p_ptServer;
}

int serverConfigure(serverCtx* p_ptServer, const ServerConfig* p_ptConfig) 
{
    if (p_ptServer == NULL || p_ptConfig == NULL) 
    {
        return SERVER_INVALID_PARAM;
    }
    
    // Vérifier si le serveur est déjà en cours d'exécution
    if (p_ptServer->t_bRunning) 
    {
        X_LOG_TRACE("Cannot configure server while running");
        return SERVER_INVALID_STATE;
    }
    
    // Copier la configuration
    memcpy(&p_ptServer->t_sConfig, p_ptConfig, sizeof(ServerConfig));
    
    // Configurer l'adresse du serveur
    p_ptServer->t_tAddress = networkMakeAddress
    (
        p_ptConfig->t_pcBindAddress ? p_ptConfig->t_pcBindAddress : "0.0.0.0",
        p_ptConfig->t_usPort
    );
    
    // Configurer la tâche du serveur
    p_ptServer->t_tTask.t_ptTask = serverThreadFunc;
    p_ptServer->t_tTask.t_ptTaskArg = p_ptServer;
    p_ptServer->t_tTask.t_iPriority = 1;
    p_ptServer->t_tTask.t_ulStackSize = SERVER_THREAD_STACK_SIZE;
    
    X_LOG_TRACE("Server configured on port %d", p_ptConfig->t_usPort);
    return SERVER_OK;
}

void serverSetMessageHandler(serverCtx* p_ptServer, MessageHandler p_pfHandler) 
{
    if (p_ptServer != NULL) 
    {
        p_ptServer->t_pfHandler = p_pfHandler;
    }
}

int serverStart(serverCtx* p_ptServer) 
{
    if (p_ptServer == NULL) 
    {
        return SERVER_INVALID_PARAM;
    }
    
    // Vérifier si le serveur est déjà en cours d'exécution
    if (p_ptServer->t_bRunning) 
    {
        X_LOG_TRACE("Server is already running");
        return SERVER_OK;
    }
    
    // Créer le socket serveur
    p_ptServer->t_ptSocket = networkCreateSocket(NETWORK_SOCK_TCP);
    if (p_ptServer->t_ptSocket == NULL) 
    {
        X_LOG_TRACE("Failed to create server socket");
        return SERVER_SOCKET_ERROR;
    }
    
    // Lier le socket serveur
    int l_iResult = networkBind(p_ptServer->t_ptSocket, &p_ptServer->t_tAddress);
    if (l_iResult != NETWORK_OK) 
    {
        X_LOG_TRACE("Failed to bind server socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(p_ptServer->t_ptSocket);
        p_ptServer->t_ptSocket = NULL;
        return SERVER_SOCKET_ERROR;
    }
    
    // Commencer à écouter
    l_iResult = networkListen(p_ptServer->t_ptSocket, p_ptServer->t_sConfig.t_iBacklog);
    if (l_iResult != NETWORK_OK) 
    {
        X_LOG_TRACE("Failed to listen on server socket: %s", networkGetErrorString(l_iResult));
        networkCloseSocket(p_ptServer->t_ptSocket);
        p_ptServer->t_ptSocket = NULL;
        return SERVER_SOCKET_ERROR;
    }
    
    // Marquer le serveur comme en cours d'exécution
    p_ptServer->t_bRunning = true;
    
    // Créer le thread serveur
    int l_iTaskResult = osTaskCreate(&p_ptServer->t_tTask);
    if (l_iTaskResult != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to create server thread: %s", osTaskGetErrorString(l_iTaskResult));
        p_ptServer->t_bRunning = false;
        networkCloseSocket(p_ptServer->t_ptSocket);
        p_ptServer->t_ptSocket = NULL;
        return SERVER_THREAD_ERROR;
    }
    
    X_LOG_TRACE("Server started on port %d", p_ptServer->t_sConfig.t_usPort);
    return SERVER_OK;
}

int serverStop(serverCtx* p_ptServer) 
{
    if (p_ptServer == NULL) 
    {
        return SERVER_INVALID_PARAM;
    }
    
    // Vérifier si le serveur est en cours d'exécution
    if (!p_ptServer->t_bRunning) 
    {
        X_LOG_TRACE("Server is not running");
        return SERVER_NOT_RUNNING;
    }
    
    X_LOG_TRACE("Stopping server...");
    
    // Marquer le serveur comme arrêté
    p_ptServer->t_bRunning = false;
    
    // Fermer le socket serveur pour débloquer accept() dans le thread serveur
    if (p_ptServer->t_ptSocket != NULL) 
    {
        networkCloseSocket(p_ptServer->t_ptSocket);
        p_ptServer->t_ptSocket = NULL;
    }
    
    // Arrêter le thread serveur
    int l_iResult = osTaskStop(&p_ptServer->t_tTask, OS_TASK_STOP_TIMEOUT);
    if (l_iResult != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to stop server thread gracefully: %s", osTaskGetErrorString(l_iResult));
        osTaskEnd(&p_ptServer->t_tTask);
    }
    
    // Attendre que le thread serveur se termine
    osTaskWait(&p_ptServer->t_tTask, NULL);
    
    // Déconnecter le client s'il existe
    mutexLock(&p_ptServer->t_tMutex);
    
    clientCtx* p_ptClient = p_ptServer->t_ptClient;
    if (p_ptClient != NULL) 
    {
        // Déconnecter le client en dehors du verrou pour éviter un blocage
        mutexUnlock(&p_ptServer->t_tMutex);
        serverDisconnectClient(p_ptServer, p_ptClient);
        mutexLock(&p_ptServer->t_tMutex);
        p_ptServer->t_ptClient = NULL;
    }
    
    mutexUnlock(&p_ptServer->t_tMutex);
    
    X_LOG_TRACE("Server stopped");
    return SERVER_OK;
}

void serverDestroy(serverCtx* p_ptServer) 
{
    if (p_ptServer == NULL) 
    {
        return;
    }
    
    // Arrêter le serveur s'il est en cours d'exécution
    if (p_ptServer->t_bRunning) 
    {
        serverStop(p_ptServer);
    }
    
    // Détruire le mutex
    mutexDestroy(&p_ptServer->t_tMutex);
    
    // Libérer la structure du serveur
    free(p_ptServer);
    
    X_LOG_TRACE("Server destroyed");
}

//-----------------------------------------------------------------------------
// Client Management Functions
//-----------------------------------------------------------------------------

bool serverGetClientAddress(clientCtx* p_ptClient, char* p_pcBuffer, int p_iSize) 
{
    if (p_ptClient == NULL || p_pcBuffer == NULL || p_iSize <= 0) 
    {
        return false;
    }
    
    strncpy(p_pcBuffer, p_ptClient->t_tAddress.t_cAddress, p_iSize - 1);
    p_pcBuffer[p_iSize - 1] = '\0';
    
    return true;
}

uint16_t serverGetClientPort(clientCtx* p_ptClient) 
{
    if (p_ptClient == NULL) 
    {
        return 0;
    }
    
    return p_ptClient->t_tAddress.t_usPort;
}

int serverDisconnectClient(serverCtx* p_ptServer, clientCtx* p_ptClient) 
{
    if (p_ptServer == NULL || p_ptClient == NULL) 
    {
        return SERVER_INVALID_PARAM;
    }
    
    // Marquer le client comme déconnecté
    p_ptClient->t_bConnected = false;
    
    // Fermer le socket pour débloquer le thread client
    if (p_ptClient->t_ptSocket != NULL) 
    {
        networkCloseSocket(p_ptClient->t_ptSocket);
        p_ptClient->t_ptSocket = NULL;
    }
    
    // Arrêter le thread client
    int l_iResult = osTaskStop(&p_ptClient->t_tTask, OS_TASK_STOP_TIMEOUT);
    if (l_iResult != OS_TASK_SUCCESS) 
    {
        X_LOG_TRACE("Failed to stop client thread gracefully: %s", osTaskGetErrorString(l_iResult));
        osTaskEnd(&p_ptClient->t_tTask);
    }
    
    // Attendre que le thread client se termine
    osTaskWait(&p_ptClient->t_tTask, NULL);
    
    // Verrouiller le mutex du serveur
    mutexLock(&p_ptServer->t_tMutex);
    
    // Réinitialiser le pointeur client du serveur
    p_ptServer->t_ptClient = NULL;
    
    mutexUnlock(&p_ptServer->t_tMutex);
    
    // Nettoyer les ressources du client
    cleanupClientResources(p_ptClient);
    
    return SERVER_OK;
}

void serverSetClientUserData(clientCtx* p_ptClient, void* p_pvUserData) 
{
    if (p_ptClient != NULL) 
    {
        p_ptClient->t_ptUserData = p_pvUserData;
    }
}

void* serverGetClientUserData(clientCtx* p_ptClient) 
{
    if (p_ptClient == NULL) 
    {
        return NULL;
    }
    
    return p_ptClient->t_ptUserData;
}

int serverSendToClient(clientCtx* p_ptClient, const void* p_pvData, int p_iSize) 
{
    if (p_ptClient == NULL || p_pvData == NULL || p_iSize <= 0) 
    {
        return SERVER_INVALID_PARAM;
    }
    
    if (!p_ptClient->t_bConnected || p_ptClient->t_ptSocket == NULL) 
    {
        return SERVER_CLIENT_DISCONNECTED;
    }
    
    return networkSend(p_ptClient->t_ptSocket, p_pvData, p_iSize);
}

//-----------------------------------------------------------------------------
// Message Functions
//-----------------------------------------------------------------------------

int serverSendMessage(serverCtx* p_ptServer, 
                      clientCtx* p_ptClient, 
                      uint8_t p_ucMsgType, 
                      const void* p_ptPayload, 
                      uint32_t p_ulPayloadSize) 
{
    if (p_ptServer == NULL || p_ptClient == NULL) 
    {
        return SERVER_INVALID_PARAM;
    }
    
    if (!p_ptClient->t_bConnected || p_ptClient->t_ptSocket == NULL) 
    {
        return SERVER_CLIENT_DISCONNECTED;
    }
    
    // Calculer la taille totale du message
    uint32_t l_ulTotalSize = 5 + p_ulPayloadSize; // 4 octets pour taille + 1 octet pour type + payload
    
    // Allouer un buffer pour le message
    uint8_t* l_pucBuffer = (uint8_t*)malloc(l_ulTotalSize);
    if (l_pucBuffer == NULL) 
    {
        X_LOG_TRACE("Failed to allocate message buffer");
        return SERVER_MEMORY_ERROR;
    }
    
    // Écrire la taille du payload (en ordre réseau)
    uint32_t l_ulNetworkPayloadSize = htonl(p_ulPayloadSize);
    memcpy(l_pucBuffer, &l_ulNetworkPayloadSize, 4);
    
    // Écrire le type de message
    l_pucBuffer[4] = p_ucMsgType;
    
    // Écrire le payload
    if (p_ulPayloadSize > 0 && p_ptPayload != NULL) 
    {
        memcpy(l_pucBuffer + 5, p_ptPayload, p_ulPayloadSize);
    }
    
    // Envoyer le message
    int l_iResult = networkSend(p_ptClient->t_ptSocket, l_pucBuffer, l_ulTotalSize);
    free(l_pucBuffer);
    
    if (l_iResult < 0) 
    {
        X_LOG_TRACE("Failed to send message to client: %s", networkGetErrorString(l_iResult));
        return SERVER_SOCKET_ERROR;
    }
    
    X_LOG_TRACE("Sent message type 0x%02X to client (%d bytes)", p_ucMsgType, l_iResult);
    return SERVER_OK;
}

const char* serverGetErrorString(int p_iError) 
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
        case SERVER_ERROR:
        default:
            return "Unknown error";
    }
}

ServerConfig serverCreateDefaultConfig(void) 
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
// Thread Functions
//-----------------------------------------------------------------------------

static void* serverThreadFunc(void* p_pvArg) 
{
    serverCtx* p_ptServer = (serverCtx*)p_pvArg;
    if (p_ptServer == NULL) 
    {
        X_LOG_TRACE("Invalid server context in server thread");
        return NULL;
    }
    
    X_LOG_TRACE("Server thread started");
    
    // Obtenir le contexte de tâche pour vérifier le drapeau d'arrêt
    xOsTaskCtx* p_ptTaskCtx = &p_ptServer->t_tTask;
    
    // Boucle principale du serveur
    while (p_ptServer->t_bRunning && 
           atomic_load(&p_ptTaskCtx->a_iStopFlag) != OS_TASK_STOP_REQUEST) 
    {
        
        // Verrouiller le mutex du serveur
        mutexLock(&p_ptServer->t_tMutex);
        
        // Vérifier si un client est déjà connecté
        bool l_bClientConnected = (p_ptServer->t_ptClient != NULL);
        
        mutexUnlock(&p_ptServer->t_tMutex);
        
        // Si un client est déjà connecté, attendre
        if (l_bClientConnected) 
        {
            // Dormir brièvement pour éviter d'occuper le CPU
            usleep(100000); // 100ms
            continue;
        }
        
        // Attendre une connexion client avec timeout
        int l_iActivity = networkWaitForActivity(p_ptServer->t_ptSocket, SERVER_ACCEPT_TIMEOUT);
        
        // Vérifier si le serveur doit s'arrêter
        if (!p_ptServer->t_bRunning || 
            atomic_load(&p_ptTaskCtx->a_iStopFlag) == OS_TASK_STOP_REQUEST) 
        {
            break;
        }
        
        // Si pas d'activité, continuer à attendre
        if (l_iActivity <= 0) 
        {
            continue;
        }
        
        // Accepter la nouvelle connexion client
        NetworkAddress l_tClientAddress;
        NetworkSocket* l_ptClientSocket = networkAccept(p_ptServer->t_ptSocket, &l_tClientAddress);
        
        if (l_ptClientSocket == NULL) 
        {
            // Si le serveur s'arrête, c'est normal
            if (!p_ptServer->t_bRunning) 
            {
                break;
            }
            
            X_LOG_TRACE("Accept failed, continuing");
            continue;
        }
        
        X_LOG_TRACE("New client connection from %s:%d",
                  l_tClientAddress.t_cAddress, l_tClientAddress.t_usPort);
        
        // Créer le contexte client
        clientCtx* l_ptClient = (clientCtx*)malloc(sizeof(clientCtx));
        if (l_ptClient == NULL) 
        {
            X_LOG_TRACE("Failed to allocate memory for client");
            networkCloseSocket(l_ptClientSocket);
            continue;
        }
        
        // Initialiser le client
        memset(l_ptClient, 0, sizeof(clientCtx));
        l_ptClient->t_ptSocket = l_ptClientSocket;
        l_ptClient->t_tAddress = l_tClientAddress;
        l_ptClient->t_bConnected = true;
        l_ptClient->t_ptServer = p_ptServer;
        
        // Configurer la tâche client
        l_ptClient->t_tTask.t_ptTask = clientThreadFunc;
        l_ptClient->t_tTask.t_ptTaskArg = l_ptClient;
        l_ptClient->t_tTask.t_iPriority = 1;
        l_ptClient->t_tTask.t_ulStackSize = CLIENT_THREAD_STACK_SIZE;
        
        // Configurer le timeout du socket si nécessaire
        if (p_ptServer->t_sConfig.t_bUseTimeout && p_ptServer->t_sConfig.t_iReceiveTimeout > 0) 
        {
            networkSetTimeout(l_ptClientSocket, p_ptServer->t_sConfig.t_iReceiveTimeout, false);
        }
        
        // Affecter le client au serveur
        mutexLock(&p_ptServer->t_tMutex);
        p_ptServer->t_ptClient = l_ptClient;
        mutexUnlock(&p_ptServer->t_tMutex);
        
        // Créer le thread client
        int l_iTaskResult = osTaskCreate(&l_ptClient->t_tTask);
        if (l_iTaskResult != OS_TASK_SUCCESS) 
        {
            X_LOG_TRACE("Failed to create client thread: %s",
                      osTaskGetErrorString(l_iTaskResult));
            
            // Retirer le client du serveur
            mutexLock(&p_ptServer->t_tMutex);
            p_ptServer->t_ptClient = NULL;
            mutexUnlock(&p_ptServer->t_tMutex);
            
            // Nettoyer
            networkCloseSocket(l_ptClientSocket);
            free(l_ptClient);
            continue;
        }
    }
    
    X_LOG_TRACE("Server thread terminated");
    return NULL;
}

static void* clientThreadFunc(void* p_pvArg) 
{
    clientCtx* p_ptClient = (clientCtx*)p_pvArg;
    if (p_ptClient == NULL || p_ptClient->t_ptServer == NULL || p_ptClient->t_ptSocket == NULL) 
    {
        X_LOG_TRACE("Invalid client context in client thread");
        return NULL;
    }
    
    serverCtx* p_ptServer = p_ptClient->t_ptServer;
    uint8_t p_aucBuffer[SERVER_MAX_BUFFER_SIZE];
    int p_iReceived;
    
    X_LOG_TRACE("Client thread started for %s:%d",
              p_ptClient->t_tAddress.t_cAddress, p_ptClient->t_tAddress.t_usPort);
    
    // Boucle principale du client
    while (p_ptClient->t_bConnected && p_ptServer->t_bRunning &&
           atomic_load(&p_ptClient->t_tTask.a_iStopFlag) != OS_TASK_STOP_REQUEST) 
    {
        
        // Recevoir des données
        p_iReceived = networkReceive(p_ptClient->t_ptSocket, p_aucBuffer, SERVER_MAX_BUFFER_SIZE);
        
        if (p_iReceived > 0) 
        {
            // Traiter les données reçues
            uint32_t l_ulPayloadSize;
            uint8_t l_ucMsgType;
            
            // Analyser l'en-tête du message (extraire la taille et le type)
            if (parseMessageHeader(p_aucBuffer, p_iReceived, &l_ulPayloadSize, &l_ucMsgType)) 
            {
                // Valider la taille du payload
                if (5 + (int)l_ulPayloadSize <= p_iReceived) 
                {
                    // Appeler le gestionnaire de messages s'il est défini
                    if (p_ptServer->t_pfHandler != NULL) 
                    {
                        network_message_t l_sMessage;
                        l_sMessage.t_iPayloadSize = l_ulPayloadSize;
                        l_sMessage.t_iHeader[0] = l_ucMsgType;
                        l_sMessage.t_ptucPayload = p_aucBuffer + 5;
                        l_sMessage.t_ulCrc = 0;
                        p_ptServer->t_pfHandler(p_ptServer, p_ptClient, &l_sMessage);
                    } 
                    else 
                    {
                        X_LOG_TRACE("No message handler defined for message type 0x%02X", l_ucMsgType);
                    }
                } 
                else 
                {
                    X_LOG_TRACE("Incomplete message: expected %u bytes, got %d", 5 + l_ulPayloadSize, p_iReceived);
                }
            } 
            else 
            {
                X_LOG_TRACE("Failed to parse message header");
            }
        }
        else if (p_iReceived == NETWORK_TIMEOUT) 
        {
            // Le timeout est normal quand il est activé
            continue;
        }
        else 
        {
            // Erreur ou déconnexion
            X_LOG_TRACE("Client disconnected or error: %s",
                      networkGetErrorString(p_iReceived));
            p_ptClient->t_bConnected = false;
        }
    }
    
    // Le client se déconnecte
    X_LOG_TRACE("Client disconnecting: %s:%d",
              p_ptClient->t_tAddress.t_cAddress, p_ptClient->t_tAddress.t_usPort);
    
    // Retirer le client du serveur
    mutexLock(&p_ptServer->t_tMutex);
    p_ptServer->t_ptClient = NULL;
    mutexUnlock(&p_ptServer->t_tMutex);
    
    // Fermer le socket
    if (p_ptClient->t_ptSocket != NULL) 
    {
        networkCloseSocket(p_ptClient->t_ptSocket);
        p_ptClient->t_ptSocket = NULL;
    }
    
    // Libérer le contexte client
    free(p_ptClient);
    
    X_LOG_TRACE("Client thread terminated");
    return NULL;
}

//-----------------------------------------------------------------------------
// Helper Functions
//-----------------------------------------------------------------------------

static void cleanupClientResources(clientCtx* p_ptClient) 
{
    if (p_ptClient == NULL) 
    {
        return;
    }
    
    // Fermer le socket s'il est encore ouvert
    if (p_ptClient->t_ptSocket != NULL) 
    {
        networkCloseSocket(p_ptClient->t_ptSocket);
        p_ptClient->t_ptSocket = NULL;
    }
}

static bool parseMessageHeader(const uint8_t* p_ptucData, 
                               int p_iSize, 
                               uint32_t* p_ptulPayloadSize, 
                               uint8_t* p_ptucMsgType) 
{
    if (p_ptucData == NULL || p_iSize < 5 || p_ptulPayloadSize == NULL || p_ptucMsgType == NULL) 
    {
        return false;
    }
    
    // Extraire la taille du payload (ordre réseau)
    uint32_t l_ulNetSize;
    memcpy(&l_ulNetSize, p_ptucData, 4);
    *p_ptulPayloadSize = ntohl(l_ulNetSize);
    
    // Extraire le type de message
    *p_ptucMsgType = p_ptucData[4];
    
    return true;
} 
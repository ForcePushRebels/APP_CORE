////////////////////////////////////////////////////////////
//  server source file
//  implements server functions
//
// general discloser: copy or share the file is forbidden
// Written : 18/04/2025
////////////////////////////////////////////////////////////

#include "networkServer.h"
#include "networkEncode.h"
#include "xLog.h"
#include <stdlib.h>
#include <string.h>

// Allocation dynamique d'un clientThread
static clientThread *createClientThread(serverCtx *p_pttServer, NetworkSocket *p_pSocket,
                                        NetworkAddress *p_pAddress)
{
    clientThread *l_pClientThread = (clientThread *)malloc(sizeof(clientThread));
    if (l_pClientThread == NULL)
    {
        X_LOG_TRACE("Failed to allocate memory for client thread (error: %s)",
                    serverGetErrorString(SERVER_MEMORY_ERROR));
        return NULL;
    }

    memset(l_pClientThread, 0, sizeof(clientThread));
    l_pClientThread->t_pSocket = p_pSocket;
    l_pClientThread->t_tAddress = *p_pAddress;
    l_pClientThread->t_bConnected = true;
    l_pClientThread->t_pServer = p_pttServer;

    return l_pClientThread;
}

// Libération des ressources d'un clientThread
static void destroyClientThread(clientThread *p_ptClientThread)
{
    if (p_ptClientThread == NULL)
        return;

    // Fermeture du socket si encore ouvert
    if (p_ptClientThread->t_pSocket != NULL)
    {
        networkCloseSocket(p_ptClientThread->t_pSocket);
        p_ptClientThread->t_pSocket = NULL;
    }

    free(p_ptClientThread);
}

////////////////////////////////////////////////////////////
/// @brief Initialize server structure with default values
/// @param p_pttServer Pointer to server structure
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverInit(serverCtx *p_pttServer)
{
    X_ASSERT(p_pttServer != NULL);
    X_LOG_TRACE("Initializing server structure");

    // Initialisation de la structure
    memset(p_pttServer, 0, sizeof(serverCtx));

    // Création du socket serveur
    p_pttServer->t_pSocket = networkCreateSocket(NETWORK_SOCK_TCP);
    if (p_pttServer->t_pSocket == NULL)
    {
        X_LOG_TRACE("Failed to create server socket");
        return SERVER_SOCKET_ERROR;
    }

    // Initialisation des valeurs par défaut
    p_pttServer->t_tAddress = networkMakeAddress("127.0.0.1", SERVER_DEFAULT_PORT);
    p_pttServer->t_eState = SERVER_STATE_IDLE;

    // Initialisation de la configuration par défaut
    p_pttServer->t_tConfig.t_usPort = SERVER_DEFAULT_PORT;
    p_pttServer->t_tConfig.t_iBacklog = SERVER_MAX_CLIENTS;
    p_pttServer->t_tConfig.t_iReceiveTimeout = SERVER_SOCKET_TIMEOUT;
    p_pttServer->t_tConfig.t_bReuseAddr = true;

    // Initialisation du mutex
    if (mutexCreate(&p_pttServer->t_Mutex) != MUTEX_OK)
    {
        X_LOG_TRACE("Failed to create server mutex");
        networkCloseSocket(p_pttServer->t_pSocket);
        return SERVER_ERROR;
    }

    // Configuration du thread serveur
    p_pttServer->t_Task.t_ptTask = (void *(*)(void *))serverHandle;
    p_pttServer->t_Task.t_ptTaskArg = p_pttServer;
    p_pttServer->t_Task.t_iPriority = 1;
    p_pttServer->t_Task.t_ulStackSize = 1024 * 1024;

    // Initialiser les limites de clients
    p_pttServer->t_iMaxClients = SERVER_MAX_CLIENTS;
    p_pttServer->t_iActiveClients = 0;

    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Configure server parameters
/// @param p_pttServer Pointer to server structure
/// @param p_usPort Port number (0 for default)
/// @param p_iBacklog Max pending connections (0 for default)
/// @param p_iMaxClients Maximum number of concurrent clients
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverConfigure(serverCtx *p_pttServer, uint16_t p_usPort, int p_iBacklog, int p_iMaxClients, const char *p_cAddress)
{
    X_ASSERT(p_pttServer != NULL);

    // Configuration du port
    if (p_usPort > 0)
    {
        p_pttServer->t_tConfig.t_usPort = p_usPort;
        p_pttServer->t_tAddress.t_usPort = p_usPort;
    }

    // Configuration du backlog
    if (p_iBacklog > 0)
    {
        p_pttServer->t_tConfig.t_iBacklog = p_iBacklog;
    }

    // Configuration du nombre max de clients
    if (p_iMaxClients > 0)
    {
        p_pttServer->t_iMaxClients = p_iMaxClients;
    }

    // Configuration de l'adresse
    if (p_cAddress != NULL)
    {
        // Copier l'adresse au lieu d'affecter le pointeur
        memcpy(p_pttServer->t_tAddress.t_cAddress, p_cAddress,
               strlen(p_cAddress) < INET_ADDRSTRLEN ? strlen(p_cAddress) : INET_ADDRSTRLEN - 1);

        // S'assurer que la chaîne est terminée par un caractère nul
        p_pttServer->t_tAddress.t_cAddress[INET_ADDRSTRLEN - 1] = '\0';
    }

    // Configuration des valeurs par défaut pour les champs non explicitement spécifiés
    if (p_pttServer->t_tConfig.t_usPort == 0)
    {
        p_pttServer->t_tConfig.t_usPort = SERVER_DEFAULT_PORT;
    }

    if (p_pttServer->t_tConfig.t_iBacklog == 0)
    {
        p_pttServer->t_tConfig.t_iBacklog = SERVER_MAX_CLIENTS;
    }

    if (p_pttServer->t_tConfig.t_iReceiveTimeout == 0)
    {
        p_pttServer->t_tConfig.t_iReceiveTimeout = SERVER_SOCKET_TIMEOUT;
    }

    // La valeur t_bReuseAddr est déjà initialisée dans serverInit()

    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Start server (creates socket, binds and starts listener thread)
/// @param p_pttServer Pointer to server structure
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverStart(serverCtx *p_pttServer)
{
    X_ASSERT(p_pttServer != NULL);

    int l_iResult;

    // Vérifier si le serveur est déjà en cours d'exécution
    if (p_pttServer->t_eState == SERVER_STATE_RUNNING)
    {
        X_LOG_TRACE("Server is already running");
        return SERVER_OK;
    }

    // Liaison du socket à l'adresse
    l_iResult = networkBind(p_pttServer->t_pSocket, &p_pttServer->t_tAddress);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to bind server socket: %s", networkGetErrorString(l_iResult));
        return SERVER_SOCKET_ERROR;
    }

    // Mise en écoute du socket
    l_iResult = networkListen(p_pttServer->t_pSocket, p_pttServer->t_tConfig.t_iBacklog);
    if (l_iResult != NETWORK_OK)
    {
        X_LOG_TRACE("Failed to listen on server socket: %s", networkGetErrorString(l_iResult));
        return SERVER_SOCKET_ERROR;
    }

    X_LOG_TRACE("Server listening on port %d", p_pttServer->t_tConfig.t_usPort);

    // Changer l'état du serveur
    p_pttServer->t_eState = SERVER_STATE_RUNNING;

    // Créer le thread serveur
    int l_iTaskResult = osTaskCreate(&p_pttServer->t_Task);
    if (l_iTaskResult != OS_TASK_SUCCESS)
    {
        X_LOG_TRACE("Failed to create server thread: %d", l_iTaskResult);
        p_pttServer->t_eState = SERVER_STATE_ERROR;
        return SERVER_THREAD_ERROR;
    }

    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Stop server (closes socket and stops thread)
/// @param p_pttServer Pointer to server structure
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverStop(serverCtx *p_pttServer)
{
    X_ASSERT(p_pttServer != NULL);

    // Vérifier si le serveur est en cours d'exécution
    if (p_pttServer->t_eState != SERVER_STATE_RUNNING)
    {
        return SERVER_NOT_RUNNING;
    }

    // Protéger l'accès aux données du serveur
    mutexLock(&p_pttServer->t_Mutex);

    // Changer l'état du serveur
    p_pttServer->t_eState = SERVER_STATE_STOPPED;

    // Sauvegarder le socket pour pouvoir le fermer
    NetworkSocket *l_pSocket = p_pttServer->t_pSocket;
    p_pttServer->t_pSocket = NULL;

    mutexUnlock(&p_pttServer->t_Mutex);

    // Fermer le socket d'écoute pour débloquer le thread serveur
    // s'il est en attente sur accept()
    if (l_pSocket != NULL)
    {
        networkCloseSocket(l_pSocket);
    }

    // Attendre la fin du thread serveur (max 5 secondes)
    int l_iWaitCount = 0;
    const int l_iMaxWait = 50; // 5 secondes (50 * 100ms)

    while (p_pttServer->t_eState != SERVER_STATE_IDLE && l_iWaitCount < l_iMaxWait)
    {
        usleep(100000); // 100ms
        l_iWaitCount++;
    }

    // Si le thread ne s'est pas terminé, forcer sa terminaison
    if (p_pttServer->t_eState != SERVER_STATE_IDLE)
    {
        X_LOG_TRACE("Forcing server thread termination");
        osTaskEnd(&p_pttServer->t_Task);
    }

    p_pttServer->t_eState = SERVER_STATE_STOPPED;
    X_LOG_TRACE("Server stopped");

    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Server main thread function
/// @param p_pArg Pointer to server structure cast as void*
/// @return Should never return, 0 on normal exit
////////////////////////////////////////////////////////////
void *serverHandle(void *p_pArg)
{
    serverCtx *l_pServer = (serverCtx *)p_pArg;
    X_ASSERT(l_pServer != NULL);

    X_LOG_TRACE("Server thread started");

    while (l_pServer->t_eState == SERVER_STATE_RUNNING)
    {
        NetworkSocket *l_pSocket = NULL;

        // Protéger l'accès pour récupérer le socket
        mutexLock(&l_pServer->t_Mutex);
        l_pSocket = l_pServer->t_pSocket;
        mutexUnlock(&l_pServer->t_Mutex);

        // Vérifier si le socket d'écoute est valide
        if (l_pSocket == NULL)
        {
            X_LOG_TRACE("Server socket is NULL, stopping server thread");
            break;
        }

        // Attendre une connexion cliente (avec timeout pour pouvoir vérifier l'état)
        int activity = networkWaitForActivity(l_pSocket, 1000); // Attendre 1 seconde max

        // Vérifier si le serveur est toujours en cours d'exécution
        if (l_pServer->t_eState != SERVER_STATE_RUNNING)
        {
            X_LOG_TRACE("Server state changed, stopping accept loop");
            break;
        }

        // Si pas d'activité, continuer la boucle
        if (activity <= 0)
        {
            continue;
        }

        // Accepter la connexion
        NetworkAddress l_tClientAddress;
        NetworkSocket *l_pClientSocket = networkAccept(l_pSocket, &l_tClientAddress);

        if (l_pClientSocket == NULL)
        {
            // Si le serveur est en train de s'arrêter, c'est normal
            if (l_pServer->t_eState != SERVER_STATE_RUNNING)
            {
                X_LOG_TRACE("Server is stopping, exiting accept loop");
                break;
            }

            X_LOG_TRACE("Accept failed, continuing...");
            continue;
        }

        X_LOG_TRACE("New client connection from %s:%d",
                    l_tClientAddress.t_cAddress,
                    l_tClientAddress.t_usPort);

        // Vérifier si le nombre maximum de clients est atteint
        mutexLock(&l_pServer->t_Mutex);

        if (l_pServer->t_iActiveClients >= l_pServer->t_iMaxClients)
        {
            X_LOG_TRACE("Maximum number of clients reached, rejecting connection (error: %s)",
                        serverGetErrorString(SERVER_MAX_CLIENTS_REACHED));
            networkCloseSocket(l_pClientSocket);
            mutexUnlock(&l_pServer->t_Mutex);
            continue;
        }

        // Incrémentation du nombre de clients actifs
        l_pServer->t_iActiveClients++;

        mutexUnlock(&l_pServer->t_Mutex);

        // Configurer le timeout pour ce client
        if (l_pServer->t_tConfig.t_iReceiveTimeout > 0)
        {
            networkSetTimeout(l_pClientSocket, l_pServer->t_tConfig.t_iReceiveTimeout, false);
        }

        // Créer une structure pour le thread client
        clientThread *l_pClientThread = createClientThread(l_pServer, l_pClientSocket, &l_tClientAddress);
        if (l_pClientThread == NULL)
        {
            X_LOG_TRACE("Failed to create client thread structure (error: %s)",
                        serverGetErrorString(SERVER_MEMORY_ERROR));
            networkCloseSocket(l_pClientSocket);

            mutexLock(&l_pServer->t_Mutex);
            l_pServer->t_iActiveClients--;
            mutexUnlock(&l_pServer->t_Mutex);

            continue;
        }

        // Configurer le thread client
        l_pClientThread->t_Task.t_ptTask = (void *(*)(void *))clientThreadFunction;
        l_pClientThread->t_Task.t_ptTaskArg = l_pClientThread;
        l_pClientThread->t_Task.t_iPriority = 1;
        l_pClientThread->t_Task.t_ulStackSize = 64 * 1024; // 64KB

        // Créer le thread client
        int l_iTaskResult = osTaskCreate(&l_pClientThread->t_Task);
        if (l_iTaskResult != OS_TASK_SUCCESS)
        {
            X_LOG_TRACE("Failed to create client thread: %d (error: %s)",
                        l_iTaskResult, serverGetErrorString(SERVER_THREAD_ERROR));
            destroyClientThread(l_pClientThread);

            mutexLock(&l_pServer->t_Mutex);
            l_pServer->t_iActiveClients--;
            mutexUnlock(&l_pServer->t_Mutex);

            continue;
        }

        X_LOG_TRACE("Client thread created successfully");

        // Appeler le callback de connexion client si défini
        if (l_pServer->t_pfOnClientConnect != NULL)
        {
            l_pServer->t_pfOnClientConnect(l_pServer, l_pClientThread);
        }
    }

    // Indiquer que le thread serveur est terminé
    mutexLock(&l_pServer->t_Mutex);
    l_pServer->t_eState = SERVER_STATE_IDLE;
    mutexUnlock(&l_pServer->t_Mutex);

    X_LOG_TRACE("Server thread terminated");
    return NULL;
}

////////////////////////////////////////////////////////////
/// @brief Client thread function - handles a single client connection
/// @param p_pArg Pointer to client thread structure cast as void*
/// @return 0 on normal termination
////////////////////////////////////////////////////////////
void *clientThreadFunction(void *p_pArg)
{
    clientThread *l_pClientThread = (clientThread *)p_pArg;
    X_ASSERT(l_pClientThread != NULL);
    X_ASSERT(l_pClientThread->t_pServer != NULL);
    X_ASSERT(l_pClientThread->t_pSocket != NULL);

    serverCtx *l_pServer = l_pClientThread->t_pServer;
    char l_cBuffer[SERVER_BUFFER_SIZE];
    int l_iReceived;
    bool l_bServerRunning = true;

    X_LOG_TRACE("Client thread started for %s:%d",
                l_pClientThread->t_tAddress.t_cAddress,
                l_pClientThread->t_tAddress.t_usPort);

    // Boucle principale de traitement des données client
    while (l_pClientThread->t_bConnected && l_bServerRunning)
    {
        // Vérifier l'état du serveur avant chaque itération
        mutexLock(&l_pServer->t_Mutex);
        l_bServerRunning = (l_pServer->t_eState == SERVER_STATE_RUNNING);
        mutexUnlock(&l_pServer->t_Mutex);

        if (!l_bServerRunning)
        {
            X_LOG_TRACE("Server is not running, stopping client thread");
            break;
        }

        // Recevoir des données du client
        l_iReceived = networkReceive(l_pClientThread->t_pSocket, l_cBuffer, SERVER_BUFFER_SIZE - 1);

        if (l_iReceived > 0)
        {
            // Ajouter un caractère nul pour terminer la chaîne
            l_cBuffer[l_iReceived] = '\0';

            X_LOG_TRACE("Received %d bytes from client", l_iReceived);

            // Appeler le callback de réception si défini
            mutexLock(&l_pServer->t_Mutex);
            void (*onDataReceived)(serverCtx *, clientThread *, void *, int) = l_pServer->t_pfOnDataReceived;
            mutexUnlock(&l_pServer->t_Mutex);

            if (onDataReceived != NULL)
            {
                onDataReceived(l_pServer, l_pClientThread, l_cBuffer, l_iReceived);
            }
        }
        else if (l_iReceived == NETWORK_TIMEOUT)
        {
            X_LOG_TRACE("Client read timeout (error: %s)", serverGetErrorString(SERVER_TIMEOUT));
            continue;
        }
        else
        {
            // Erreur ou déconnexion
            X_LOG_TRACE("Client disconnected or error: %s (server error: %s)",
                        networkGetErrorString(l_iReceived), serverGetErrorString(SERVER_CLIENT_DISCONNECTED));
            l_pClientThread->t_bConnected = false;
        }
    }

    // Appeler le callback de déconnexion si défini
    mutexLock(&l_pServer->t_Mutex);
    void (*onClientDisconnect)(serverCtx *, clientThread *) = l_pServer->t_pfOnClientDisconnect;
    int *p_iActiveClients = &l_pServer->t_iActiveClients;
    mutexUnlock(&l_pServer->t_Mutex);

    if (onClientDisconnect != NULL)
    {
        onClientDisconnect(l_pServer, l_pClientThread);
    }

    // Décrémenter le compteur de clients actifs
    mutexLock(&l_pServer->t_Mutex);
    if (*p_iActiveClients > 0)
    {
        (*p_iActiveClients)--;
    }
    mutexUnlock(&l_pServer->t_Mutex);

    // Libérer les ressources du thread client
    destroyClientThread(l_pClientThread);

    X_LOG_TRACE("Client thread terminated");
    return NULL;
}

////////////////////////////////////////////////////////////
/// @brief Send data to client
/// @param p_tClientThread Pointer to client thread
/// @param p_pData Data buffer
/// @param p_iSize Data size
/// @return Bytes sent or error code
////////////////////////////////////////////////////////////
int serverSendToClient(clientThread *p_tClientThread, const void *p_pData, int p_iSize)
{
    X_ASSERT(p_tClientThread != NULL);
    X_ASSERT(p_tClientThread->t_pSocket != NULL);
    X_ASSERT(p_pData != NULL);

    if (!p_tClientThread->t_bConnected)
    {
        return SERVER_CLIENT_DISCONNECTED;
    }

    int result = networkSend(p_tClientThread->t_pSocket, p_pData, p_iSize);
    if (result < 0)
    {
        // Si c'est une erreur réseau, on renvoie une erreur serveur
        if (result == NETWORK_ERROR)
            return SERVER_SOCKET_ERROR;
        else if (result == NETWORK_TIMEOUT)
            return SERVER_TIMEOUT;
        else if (result == NETWORK_INVALID_PARAM)
            return SERVER_INVALID_PARAMETER;
        return result; // Erreur réseau spécifique
    }

    return result; // Nombre d'octets envoyés
}

////////////////////////////////////////////////////////////
/// @brief Set callback for client connection event
/// @param p_pttServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnClientConnect(serverCtx *p_pttServer, void (*p_pfCallback)(serverCtx *, clientThread *))
{
    X_ASSERT(p_pttServer != NULL);

    p_pttServer->t_pfOnClientConnect = p_pfCallback;
    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Set callback for client disconnection event
/// @param p_pttServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnClientDisconnect(serverCtx *p_pttServer, void (*p_pfCallback)(serverCtx *, clientThread *))
{
    X_ASSERT(p_pttServer != NULL);

    p_pttServer->t_pfOnClientDisconnect = p_pfCallback;
    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Set callback for data received event
/// @param p_pttServer Pointer to server structure
/// @param p_pfCallback Callback function
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetOnDataReceived(serverCtx *p_pttServer, void (*p_pfCallback)(serverCtx *, clientThread *, void *, int))
{
    X_ASSERT(p_pttServer != NULL);

    p_pttServer->t_pfOnDataReceived = p_pfCallback;
    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Disconnect and cleanup client thread
/// @param p_tClientThread Pointer to client thread
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverDisconnectClient(clientThread *p_tClientThread)
{
    X_ASSERT(p_tClientThread != NULL);

    p_tClientThread->t_bConnected = false;

    // Fermer le socket forcera la sortie de la boucle de réception
    if (p_tClientThread->t_pSocket != NULL)
    {
        networkCloseSocket(p_tClientThread->t_pSocket);
        p_tClientThread->t_pSocket = NULL;
    }

    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Set user data for client thread (context)
/// @param p_tClientThread Pointer to client thread
/// @param p_pUserData Pointer to user data
/// @return SERVER_OK on success, error code otherwise
////////////////////////////////////////////////////////////
int serverSetClientUserData(clientThread *p_tClientThread, void *p_pUserData)
{
    X_ASSERT(p_tClientThread != NULL);

    p_tClientThread->t_pUserData = p_pUserData;
    return SERVER_OK;
}

////////////////////////////////////////////////////////////
/// @brief Get user data from client thread
/// @param p_tClientThread Pointer to client thread
/// @return Pointer to user data or NULL if not set
////////////////////////////////////////////////////////////
void *serverGetClientUserData(clientThread *p_tClientThread)
{
    X_ASSERT(p_tClientThread != NULL);

    return p_tClientThread->t_pUserData;
}

// Libération des ressources d'un Server
void destroyServer(serverCtx *p_pttServer)
{
    if (p_pttServer == NULL)
        return;

    // S'assurer que le serveur est arrêté
    if (p_pttServer->t_eState == SERVER_STATE_RUNNING)
    {
        serverStop(p_pttServer);
    }

    // Fermer le socket serveur s'il est encore ouvert
    if (p_pttServer->t_pSocket != NULL)
    {
        networkCloseSocket(p_pttServer->t_pSocket);
        p_pttServer->t_pSocket = NULL;
    }

    // Détruire le mutex
    mutexDestroy(&p_pttServer->t_Mutex);

    // Réinitialiser l'état du serveur
    p_pttServer->t_eState = SERVER_STATE_IDLE;
    p_pttServer->t_iActiveClients = 0;

    // Les callbacks sont simplement réinitialisés à NULL
    p_pttServer->t_pfOnClientConnect = NULL;
    p_pttServer->t_pfOnClientDisconnect = NULL;
    p_pttServer->t_pfOnDataReceived = NULL;
}

////////////////////////////////////////////////////////////
/// @brief Get error string for server error code
/// @param p_iError Server error code
/// @return Error description string
////////////////////////////////////////////////////////////
const char *serverGetErrorString(int p_iError)
{
    switch (p_iError)
    {
    case SERVER_OK:
        return "Success";
    case SERVER_ERROR:
        return "General server error";
    case SERVER_MAX_CLIENTS_REACHED:
        return "Maximum number of clients reached";
    case SERVER_INVALID_STATE:
        return "Invalid server state";
    case SERVER_THREAD_ERROR:
        return "Thread creation or management error";
    case SERVER_CLIENT_DISCONNECTED:
        return "Client is disconnected";
    case SERVER_SOCKET_ERROR:
        return "Socket operation error";
    case SERVER_MEMORY_ERROR:
        return "Memory allocation error";
    case SERVER_TIMEOUT:
        return "Operation timed out";
    case SERVER_INVALID_PARAMETER:
        return "Invalid parameter";
    case SERVER_NOT_RUNNING:
        return "Server is not running";
    default:
        // Check if it's a network error
        if (p_iError >= 0x17A2B40 && p_iError <= 0x17A2B4F)
        {
            return networkGetErrorString(p_iError);
        }
        return "Unknown error";
    }
}

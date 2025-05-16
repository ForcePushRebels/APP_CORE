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
#include <stdbool.h>

///////////////////////////////////////////
/// Internal defines
///////////////////////////////////////////
#define SERVER_THREAD_STACK_SIZE    (1024*1024)  // 1MB stack for server thread
#define CLIENT_THREAD_STACK_SIZE    (64*1024)    // 64KB stack for client threads
#define SERVER_ACCEPT_TIMEOUT       500          // 500ms timeout for accept operations

///////////////////////////////////////////
/// Client context structure
///////////////////////////////////////////
struct client_ctx_t {
    NetworkSocket*  socket;         // Client socket
    NetworkAddress  address;        // Client address
    bool            connected;      // Connection status
    xOsTaskCtx      task;           // Client thread task context
    serverCtx*      server;         // Reference to parent server
    void*           userData;       // User data associated with client
    struct client_ctx_t* next;      // Next client in linked list (for cleanup)
    struct client_ctx_t* prev;      // Previous client in linked list (for cleanup)
};

///////////////////////////////////////////
/// Server context structure
///////////////////////////////////////////
struct server_ctx_t {
    NetworkSocket*  socket;         // Server socket
    NetworkAddress  address;        // Server address
    xOsMutexCtx     mutex;          // Synchronization mutex
    xOsTaskCtx      task;           // Server thread context
    ServerState     state;          // Current server state
    ServerConfig    config;         // Server configuration
    int             activeClients;  // Number of active clients
    
    // Client management
    clientCtx*      clientList;     // Linked list of clients for cleanup

    // Callback functions
    ServerClientConnectCallback    onClientConnect;
    ServerClientDisconnectCallback onClientDisconnect;
    ServerDataReceivedCallback     onDataReceived;
};

///////////////////////////////////////////
/// Thread function prototypes
///////////////////////////////////////////
static void* serverThreadFunction(void* arg);
static void* clientThreadFunction(void* arg);

///////////////////////////////////////////
/// Helper function prototypes
///////////////////////////////////////////
static void addClientToList(serverCtx* server, clientCtx* client);
static void removeClientFromList(serverCtx* server, clientCtx* client);
static void cleanupClientResources(clientCtx* client);

///////////////////////////////////////////
/// serverCreate
///////////////////////////////////////////
serverCtx* serverCreate(void) {
    serverCtx* server = (serverCtx*)malloc(sizeof(serverCtx));
    if (server == NULL) {
        X_LOG_TRACE("Failed to allocate memory for server context");
        return NULL;
    }
    
    // Initialize server context
    memset(server, 0, sizeof(serverCtx));
    
    // Create mutex for synchronization
    if (mutexCreate(&server->mutex) != MUTEX_OK) 
    {
        X_LOG_TRACE("Failed to create server mutex");
        free(server);
        return NULL;
    }
    
    // Set initial state
    server->state = SERVER_STATE_UNINITIALIZED;
    
    // Set default configuration
    ServerConfig defaultConfig = serverCreateDefaultConfig();
    memcpy(&server->config, &defaultConfig, sizeof(ServerConfig));
    
    X_LOG_TRACE("Server context created");
    return server;
}

///////////////////////////////////////////
/// serverConfigure
///////////////////////////////////////////
int serverConfigure(serverCtx* server, const ServerConfig* config) {
    if (server == NULL || config == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    // Check server state
    if (server->state != SERVER_STATE_UNINITIALIZED && server->state != SERVER_STATE_INITIALIZED) {
        X_LOG_TRACE("Cannot configure server in current state");
        return SERVER_INVALID_STATE;
    }
    
    // Copy configuration
    memcpy(&server->config, config, sizeof(ServerConfig));
    
    // Create server address
    server->address = networkMakeAddress(
        config->bindAddress ? config->bindAddress : "0.0.0.0",
        config->port
    );
    
    // Create server socket
    server->socket = networkCreateSocket(NETWORK_SOCK_TCP);
    if (server->socket == NULL) {
        X_LOG_TRACE("Failed to create server socket");
        return SERVER_SOCKET_ERROR;
    }
    
    // Configure server task
    server->task.t_ptTask = serverThreadFunction;
    server->task.t_ptTaskArg = server;
    server->task.t_iPriority = 1;
    server->task.t_ulStackSize = SERVER_THREAD_STACK_SIZE;
    
    // Update server state
    server->state = SERVER_STATE_INITIALIZED;
    
    X_LOG_TRACE("Server configured on port %d", config->port);
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverSetOnClientConnect
///////////////////////////////////////////
int serverSetOnClientConnect(serverCtx* server, ServerClientConnectCallback callback) {
    if (server == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    server->onClientConnect = callback;
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverSetOnClientDisconnect
///////////////////////////////////////////
int serverSetOnClientDisconnect(serverCtx* server, ServerClientDisconnectCallback callback) {
    if (server == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    server->onClientDisconnect = callback;
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverSetOnDataReceived
///////////////////////////////////////////
int serverSetOnDataReceived(serverCtx* server, ServerDataReceivedCallback callback) {
    if (server == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    server->onDataReceived = callback;
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverStart
///////////////////////////////////////////
int serverStart(serverCtx* server) {
    if (server == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    // Check if server is already running
    if (server->state == SERVER_STATE_RUNNING) {
        X_LOG_TRACE("Server is already running");
        return SERVER_OK;
    }
    
    // Check if server is initialized
    if (server->state != SERVER_STATE_INITIALIZED) {
        X_LOG_TRACE("Server is not initialized");
        return SERVER_INVALID_STATE;
    }
    
    // Bind server socket
    int result = networkBind(server->socket, &server->address);
    if (result != NETWORK_OK) {
        X_LOG_TRACE("Failed to bind server socket: %s", networkGetErrorString(result));
        return SERVER_SOCKET_ERROR;
    }
    
    // Start listening
    result = networkListen(server->socket, server->config.backlog);
    if (result != NETWORK_OK) {
        X_LOG_TRACE("Failed to listen on server socket: %s", networkGetErrorString(result));
        return SERVER_SOCKET_ERROR;
    }
    
    // Update server state
    server->state = SERVER_STATE_RUNNING;
    
    // Create server thread
    int taskResult = osTaskCreate(&server->task);
    if (taskResult != OS_TASK_SUCCESS) {
        X_LOG_TRACE("Failed to create server thread: %s", osTaskGetErrorString(taskResult));
        server->state = SERVER_STATE_ERROR;
        return SERVER_THREAD_ERROR;
    }
    
    X_LOG_TRACE("Server started and listening on port %d", server->config.port);
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverStop
///////////////////////////////////////////
int serverStop(serverCtx* server) {
    if (server == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    // Check if server is running
    if (server->state != SERVER_STATE_RUNNING) {
        X_LOG_TRACE("Server is not running");
        return SERVER_NOT_RUNNING;
    }
    
    X_LOG_TRACE("Stopping server...");
    
    // Lock server mutex
    mutexLock(&server->mutex);
    
    // Change server state
    server->state = SERVER_STATE_STOPPING;
    
    // Save socket reference
    NetworkSocket* serverSocket = server->socket;
    server->socket = NULL;
    
    mutexUnlock(&server->mutex);
    
    // Close server socket to unblock accept() call in server thread
    if (serverSocket != NULL) {
        networkCloseSocket(serverSocket);
    }
    
    // Stop server thread
    int result = osTaskStop(&server->task, OS_TASK_STOP_TIMEOUT);
    if (result != OS_TASK_SUCCESS) {
        X_LOG_TRACE("Failed to stop server thread gracefully: %s", osTaskGetErrorString(result));
        osTaskEnd(&server->task);
    }
    
    // Wait for server thread to terminate
    osTaskWait(&server->task, NULL);
    
    // Now stop all client threads
    mutexLock(&server->mutex);
    
    // Disconnect all clients
    clientCtx* client = server->clientList;
    while (client != NULL) {
        clientCtx* nextClient = client->next;
        serverDisconnectClient(client);
        client = nextClient;
    }
    
    server->state = SERVER_STATE_INITIALIZED;
    mutexUnlock(&server->mutex);
    
    X_LOG_TRACE("Server stopped");
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverDestroy
///////////////////////////////////////////
void serverDestroy(serverCtx* server) {
    if (server == NULL) {
        return;
    }
    
    // Stop server if it's running
    if (server->state == SERVER_STATE_RUNNING) {
        serverStop(server);
    }
    
    // Close server socket if it's still open
    if (server->socket != NULL) {
        networkCloseSocket(server->socket);
        server->socket = NULL;
    }
    
    // Destroy mutex
    mutexDestroy(&server->mutex);
    
    // Free server context
    free(server);
    
    X_LOG_TRACE("Server destroyed");
}

///////////////////////////////////////////
/// serverSendToClient
///////////////////////////////////////////
int serverSendToClient(clientCtx* client, const void* data, int size) {
    if (client == NULL || data == NULL || size <= 0) {
        return SERVER_INVALID_PARAMETER;
    }
    
    if (!client->connected) {
        return SERVER_CLIENT_DISCONNECTED;
    }
    
    // Send data
    int result = networkSend(client->socket, data, size);
    if (result < 0) {
        // Map network error to server error
        if (result == NETWORK_ERROR) {
            return SERVER_SOCKET_ERROR;
        } else if (result == NETWORK_TIMEOUT) {
            return SERVER_TIMEOUT;
        } else if (result == NETWORK_INVALID_PARAM) {
            return SERVER_INVALID_PARAMETER;
        }
        return result; // Return specific network error
    }
    
    return result; // Return number of bytes sent
}

///////////////////////////////////////////
/// serverDisconnectClient
///////////////////////////////////////////
int serverDisconnectClient(clientCtx* client) {
    if (client == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    // Signal client thread to disconnect
    client->connected = false;
    
    // Close socket to unblock client thread
    if (client->socket != NULL) {
        networkCloseSocket(client->socket);
        client->socket = NULL;
    }
    
    // Stop client thread
    int result = osTaskStop(&client->task, OS_TASK_STOP_TIMEOUT);
    if (result != OS_TASK_SUCCESS) {
        X_LOG_TRACE("Failed to stop client thread gracefully: %s", osTaskGetErrorString(result));
        osTaskEnd(&client->task);
    }
    
    // Wait for client thread to terminate
    osTaskWait(&client->task, NULL);
    
    // Clean up client resources
    cleanupClientResources(client);
    
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverSetClientUserData
///////////////////////////////////////////
int serverSetClientUserData(clientCtx* client, void* userData) {
    if (client == NULL) {
        return SERVER_INVALID_PARAMETER;
    }
    
    client->userData = userData;
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverGetClientUserData
///////////////////////////////////////////
void* serverGetClientUserData(clientCtx* client) {
    if (client == NULL) {
        return NULL;
    }
    
    return client->userData;
}

///////////////////////////////////////////
/// serverGetClientAddress
///////////////////////////////////////////
int serverGetClientAddress(clientCtx* client, char* address, int size) {
    if (client == NULL || address == NULL || size <= 0) {
        return SERVER_INVALID_PARAMETER;
    }
    
    strncpy(address, client->address.t_cAddress, size - 1);
    address[size - 1] = '\0';
    
    return SERVER_OK;
}

///////////////////////////////////////////
/// serverGetClientPort
///////////////////////////////////////////
uint16_t serverGetClientPort(clientCtx* client) {
    if (client == NULL) {
        return 0;
    }
    
    return client->address.t_usPort;
}

///////////////////////////////////////////
/// serverGetErrorString
///////////////////////////////////////////
const char* serverGetErrorString(int errorCode) {
    switch (errorCode) {
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
            if (errorCode >= 0x17A2B40 && errorCode <= 0x17A2B4F) {
                return networkGetErrorString(errorCode);
            }
            return "Unknown error";
    }
}

///////////////////////////////////////////
/// serverCreateDefaultConfig
///////////////////////////////////////////
ServerConfig serverCreateDefaultConfig(void) {
    ServerConfig config;
    memset(&config, 0, sizeof(ServerConfig));
    
    config.port = SERVER_DEFAULT_PORT;
    config.bindAddress = NULL;
    config.maxClients = SERVER_MAX_CLIENTS;
    config.backlog = SERVER_MAX_CLIENTS;
    config.useTimeout = false;
    config.receiveTimeout = SERVER_SOCKET_TIMEOUT;
    config.reuseAddr = true;
    
    return config;
}

///////////////////////////////////////////
/// serverThreadFunction
///////////////////////////////////////////
static void* serverThreadFunction(void* arg) {
    serverCtx* server = (serverCtx*)arg;
    if (server == NULL) {
        X_LOG_TRACE("Invalid server context in server thread");
        return NULL;
    }
    
    X_LOG_TRACE("Server thread started");
    
    // Get task context to check stop flag
    xOsTaskCtx* taskCtx = &server->task;
    
    // Main server loop
    while (server->state == SERVER_STATE_RUNNING &&
           atomic_load(&taskCtx->a_iStopFlag) != OS_TASK_STOP_REQUEST) {
        
        // Lock mutex to access shared server data
        mutexLock(&server->mutex);
        
        // Get server socket
        NetworkSocket* serverSocket = server->socket;
        
        // Check active clients count
        int activeClients = server->activeClients;
        int maxClients = server->config.maxClients;
        
        mutexUnlock(&server->mutex);
        
        // Check if server socket is valid
        if (serverSocket == NULL) {
            X_LOG_TRACE("Server socket is NULL, stopping server thread");
            break;
        }
        
        // Check if maximum clients reached
        if (activeClients >= maxClients) {
            // Sleep briefly to avoid busy waiting
            usleep(100);
            continue;
        }
        
        // Wait for client connection with timeout
        int activity = networkWaitForActivity(serverSocket, SERVER_ACCEPT_TIMEOUT);
        
        // Check server state again
        if (server->state != SERVER_STATE_RUNNING ||
            atomic_load(&taskCtx->a_iStopFlag) == OS_TASK_STOP_REQUEST) {
            X_LOG_TRACE("Server stopping, exiting accept loop");
            break;
        }
        
        // If no activity, continue waiting
        if (activity <= 0) {
            continue;
        }
        
        // Accept new client connection
        NetworkAddress clientAddress;
        NetworkSocket* clientSocket = networkAccept(serverSocket, &clientAddress);
        
        if (clientSocket == NULL) {
            // If server is stopping, this is normal
            if (server->state != SERVER_STATE_RUNNING) {
                X_LOG_TRACE("Server stopping, accept returned NULL");
                break;
            }
            
            X_LOG_TRACE("Accept failed, continuing");
            continue;
        }
        
        X_LOG_TRACE("New client connection from %s:%d",
                   clientAddress.t_cAddress, clientAddress.t_usPort);
        
        // Create client context
        clientCtx* client = (clientCtx*)malloc(sizeof(clientCtx));
        if (client == NULL) {
            X_LOG_TRACE("Failed to allocate memory for client context");
            networkCloseSocket(clientSocket);
            continue;
        }
        
        // Initialize client context
        memset(client, 0, sizeof(clientCtx));
        client->socket = clientSocket;
        client->address = clientAddress;
        client->connected = true;
        client->server = server;
        client->userData = NULL;
        
        // Configure client task
        client->task.t_ptTask = clientThreadFunction;
        client->task.t_ptTaskArg = client;
        client->task.t_iPriority = 1;
        client->task.t_ulStackSize = CLIENT_THREAD_STACK_SIZE;
        
        // Configure socket timeout if needed
        if (server->config.useTimeout && server->config.receiveTimeout > 0) {
            networkSetTimeout(clientSocket, server->config.receiveTimeout, false);
        }
        
        // Lock server mutex to update client count
        mutexLock(&server->mutex);
        
        // Check max clients again (race condition check)
        if (server->activeClients >= server->config.maxClients) {
            mutexUnlock(&server->mutex);
            X_LOG_TRACE("Maximum clients reached, rejecting connection");
            free(client);
            networkCloseSocket(clientSocket);
            continue;
        }
        
        // Increment client count and add client to list
        server->activeClients++;
        addClientToList(server, client);
        
        mutexUnlock(&server->mutex);
        
        // Create client thread
        int taskResult = osTaskCreate(&client->task);
        if (taskResult != OS_TASK_SUCCESS) {
            X_LOG_TRACE("Failed to create client thread: %s",
                       osTaskGetErrorString(taskResult));
            
            // Decrease client count and remove from list
            mutexLock(&server->mutex);
            server->activeClients--;
            removeClientFromList(server, client);
            mutexUnlock(&server->mutex);
            
            // Clean up client resources
            networkCloseSocket(clientSocket);
            free(client);
            continue;
        }
        
        // Call client connect callback if defined
        if (server->onClientConnect != NULL) {
            server->onClientConnect(server, client);
        }
    }
    
    // Server thread is exiting
    X_LOG_TRACE("Server thread terminated");
    return NULL;
}

///////////////////////////////////////////
/// clientThreadFunction
///////////////////////////////////////////
static void* clientThreadFunction(void* arg) {
    clientCtx* client = (clientCtx*)arg;
    if (client == NULL || client->server == NULL || client->socket == NULL) {
        X_LOG_TRACE("Invalid client context in client thread");
        return NULL;
    }
    
    serverCtx* server = client->server;
    char buffer[SERVER_BUFFER_SIZE];
    int received;
    bool serverRunning = true;
    
    X_LOG_TRACE("Client thread started for %s:%d",
               client->address.t_cAddress, client->address.t_usPort);
    
    // Get task context to check stop flag
    xOsTaskCtx* taskCtx = &client->task;
    
    // Main client loop
    while (client->connected && serverRunning &&
           atomic_load(&taskCtx->a_iStopFlag) != OS_TASK_STOP_REQUEST) {
        
        // Check server state
        mutexLock(&server->mutex);
        serverRunning = (server->state == SERVER_STATE_RUNNING);
        mutexUnlock(&server->mutex);
        
        if (!serverRunning ||
            atomic_load(&taskCtx->a_iStopFlag) == OS_TASK_STOP_REQUEST) {
            X_LOG_TRACE("Server not running or stop requested, ending client thread");
            break;
        }
        
        // Receive data from client
        received = networkReceive(client->socket, buffer, SERVER_BUFFER_SIZE - 1);
        
        if (received > 0) {
            // Ensure null termination for text data
            buffer[received] = '\0';
            
            X_LOG_TRACE("Received %d bytes from client", received);
            
            // Call data received callback if defined
            if (server->onDataReceived != NULL) {
                server->onDataReceived(server, client, buffer, received);
            }
        }
        else if (received == NETWORK_TIMEOUT) {
            // Timeout is normal when timeout is enabled
            continue;
        }
        else {
            // Error or disconnection
            X_LOG_TRACE("Client disconnected or error: %s",
                       networkGetErrorString(received));
            client->connected = false;
        }
    }
    
    // Call client disconnect callback if defined
    if (server->onClientDisconnect != NULL) {
        server->onClientDisconnect(server, client);
    }
    
    // Update client count
    mutexLock(&server->mutex);
    if (server->activeClients > 0) {
        server->activeClients--;
    }
    removeClientFromList(server, client);
    mutexUnlock(&server->mutex);
    
    // Clean up socket
    if (client->socket != NULL) {
        networkCloseSocket(client->socket);
        client->socket = NULL;
    }
    
    X_LOG_TRACE("Client thread terminated");
    
    // Free client context only if server is not in STOPPING state
    // Otherwise serverStop will handle cleanup
    if (server->state != SERVER_STATE_STOPPING) {
        free(client);
    }
    
    return NULL;
}

///////////////////////////////////////////
/// addClientToList
///////////////////////////////////////////
static void addClientToList(serverCtx* server, clientCtx* client) {
    // Caller must hold server mutex
    
    // Insert at head of list
    if (server->clientList != NULL) {
        server->clientList->prev = client;
    }
    
    client->next = server->clientList;
    client->prev = NULL;
    server->clientList = client;
}

///////////////////////////////////////////
/// removeClientFromList
///////////////////////////////////////////
static void removeClientFromList(serverCtx* server, clientCtx* client) {
    // Caller must hold server mutex
    
    if (client->prev != NULL) {
        client->prev->next = client->next;
    } else {
        // This is the head
        server->clientList = client->next;
    }
    
    if (client->next != NULL) {
        client->next->prev = client->prev;
    }
    
    // Clear links
    client->next = NULL;
    client->prev = NULL;
}

///////////////////////////////////////////
/// cleanupClientResources
///////////////////////////////////////////
static void cleanupClientResources(clientCtx* client) 
{
    if (client == NULL) {
        return;
    }
    
    // Close socket if still open
    if (client->socket != NULL) {
        networkCloseSocket(client->socket);
        client->socket = NULL;
    }
    
    // Free client context
    free(client);
}

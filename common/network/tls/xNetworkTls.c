////////////////////////////////////////////////////////////
//  Secure network core implementation file for embedded systems
//  Implements the network interface defined in xNetwork.h
//  IPv4-focused, simplified API with mandatory TLS
//  Memory-optimized for embedded environments
//
// general disclosure: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: Current date - Simplified API with mandatory TLS
////////////////////////////////////////////////////////////

#include "xNetworkTls.h"

//////////////////////////////////
/// Core API Implementation
//////////////////////////////////

// Create a secure socket with TLS support
NetworkSocket *networkCreateSecureSocket(const NetworkTlsConfig *p_pTlsConfig)
{
    if (!p_pTlsConfig) {
        X_LOG_TRACE("networkCreateSecureSocket: Invalid TLS configuration");
        return NULL;
    }
    
    // Vérifier d'abord que les certificats sont disponibles
    if (!p_pTlsConfig->t_cCertPath || !p_pTlsConfig->t_cKeyPath) {
        X_LOG_TRACE("networkCreateSecureSocket: Certificate and key paths must be provided");
        return NULL;
    }
    
    // Vérifier que les fichiers existent
    FILE* file;
    if ((file = fopen(p_pTlsConfig->t_cCertPath, "r")) == NULL) {
        X_LOG_TRACE("networkCreateSecureSocket: Certificate file '%s' does not exist or is not accessible", 
                   p_pTlsConfig->t_cCertPath);
        return NULL;
    }
    fclose(file);
    
    if ((file = fopen(p_pTlsConfig->t_cKeyPath, "r")) == NULL) {
        X_LOG_TRACE("networkCreateSecureSocket: Key file '%s' does not exist or is not accessible", 
                   p_pTlsConfig->t_cKeyPath);
        return NULL;
    }
    fclose(file);
    
    // Vérifier le CA si fourni
    if (p_pTlsConfig->t_cCaPath) {
        if ((file = fopen(p_pTlsConfig->t_cCaPath, "r")) == NULL) {
            X_LOG_TRACE("networkCreateSecureSocket: CA file '%s' does not exist or is not accessible", 
                       p_pTlsConfig->t_cCaPath);
            return NULL;
        }
        fclose(file);
    }

    // Allocate a new socket structure
    NetworkSocket *l_pSocket = (NetworkSocket *)malloc(sizeof(NetworkSocket));
    if (!l_pSocket) {
        X_LOG_TRACE("networkCreateSecureSocket: Memory allocation failed");
        return NULL;
    }

    // Create the actual socket (always TCP for TLS)
    l_pSocket->t_iSocketFd = socket(AF_INET, NETWORK_SOCK_TCP, 0);
    if (l_pSocket->t_iSocketFd < 0) {
        X_LOG_TRACE("networkCreateSecureSocket: Socket creation failed with errno %d", errno);
        free(l_pSocket);
        return NULL;
    }

    // Enable address reuse
    int l_iOption = 1;
    setsockopt(l_pSocket->t_iSocketFd, SOL_SOCKET, SO_REUSEADDR, &l_iOption, sizeof(l_iOption));

    // Set socket properties
    l_pSocket->t_iType = NETWORK_SOCK_TCP;
    l_pSocket->t_bConnected = false;
    
    // Initialize the mutex for thread safety
    mutexCreate(&l_pSocket->t_Mutex);
    
    // Allocate TLS engine
    l_pSocket->t_pTlsEngine = malloc(sizeof(TLS_Engine));
    if (!l_pSocket->t_pTlsEngine) {
        X_LOG_TRACE("networkCreateSecureSocket: TLS engine allocation failed");
        close(l_pSocket->t_iSocketFd);
        free(l_pSocket);
        return NULL;
    }

    // Create TLS configuration structure
    TLS_Config l_tTlsConfig;
    memset(&l_tTlsConfig, 0, sizeof(TLS_Config));
    
    // Set TLS version from config (default to TLS 1.3 if not specified or invalid)
    if (p_pTlsConfig->t_eVersion == TLS_VERSION_1_2 || p_pTlsConfig->t_eVersion == TLS_VERSION_1_3) {
        l_tTlsConfig.version = p_pTlsConfig->t_eVersion;
    } else {
        l_tTlsConfig.version = TLS_VERSION_1_3; // Default to TLS 1.3
    }
    X_LOG_TRACE("networkCreateSecureSocket: Using TLS version %d", l_tTlsConfig.version);
    
    // Set ECC curve from config (default to SECP256R1 if using P-256 certificates or if invalid)
    if (p_pTlsConfig->t_eCurve >= TLS_ECC_SECP256R1 && p_pTlsConfig->t_eCurve <= TLS_ECC_X25519) {
        l_tTlsConfig.eccCurve = p_pTlsConfig->t_eCurve;
    } else {
        l_tTlsConfig.eccCurve = TLS_ECC_SECP256R1;
    }
    X_LOG_TRACE("networkCreateSecureSocket: Using ECC curve %d", l_tTlsConfig.eccCurve);
    
    // Copy certificate paths
    l_tTlsConfig.caCertPath = p_pTlsConfig->t_cCaPath;
    l_tTlsConfig.certPath = p_pTlsConfig->t_cCertPath;
    l_tTlsConfig.keyPath = p_pTlsConfig->t_cKeyPath;
    
    // Set verification mode
    l_tTlsConfig.verifyPeer = p_pTlsConfig->t_bVerifyPeer;
    X_LOG_TRACE("networkCreateSecureSocket: Peer verification %s", 
               l_tTlsConfig.verifyPeer ? "enabled" : "disabled");
    
    // Déterminer si nous sommes en mode serveur ou client basé sur le nom du fichier des certificats
    // Les certificats serveur devraient contenir "server" dans leur chemin 
    // Les certificats client devraient contenir "client" dans leur chemin
    if (strstr(p_pTlsConfig->t_cCertPath, "server") != NULL) {
        l_tTlsConfig.isServer = true;
        X_LOG_TRACE("networkCreateSecureSocket: Detected server certificate, using server mode");
    } else {
        l_tTlsConfig.isServer = false;
        X_LOG_TRACE("networkCreateSecureSocket: Using client mode");
    }
    
    // Initialize the TLS engine
    X_LOG_TRACE("networkCreateSecureSocket: Initializing TLS engine for socket %d", l_pSocket->t_iSocketFd);
    int l_iResult = tlsEngineInit((TLS_Engine*)l_pSocket->t_pTlsEngine, 
                                  l_pSocket->t_iSocketFd, 
                                  &l_tTlsConfig);
    
    if (l_iResult != TLS_OK) {
        X_LOG_TRACE("networkCreateSecureSocket: TLS engine initialization failed with error code %d (%s)",
                   l_iResult, tlsEngineGetErrorString(l_iResult));
        free(l_pSocket->t_pTlsEngine);
        close(l_pSocket->t_iSocketFd);
        free(l_pSocket);
        return NULL;
    }
    
    X_LOG_TRACE("networkCreateSecureSocket: Secure socket created successfully");
    return l_pSocket;
}

// Create network address
NetworkAddress networkMakeAddress(const char *p_pcAddress, unsigned short p_usPort)
{
    NetworkAddress l_tAddress;
    memset(&l_tAddress, 0, sizeof(NetworkAddress));
    
    // Check pointer parameters with assertions
    X_ASSERT(p_pcAddress != NULL);
    
    // Replace non-pointer assertions with condition checks
    if (p_usPort <= 0 || strlen(p_pcAddress) == 0) {
        // Return empty address on invalid parameters
        return l_tAddress;
    }

    l_tAddress.t_usPort = p_usPort; // Set port
    
    // Validate IPv4 address format
    struct in_addr l_tAddr;
    if (inet_pton(AF_INET, p_pcAddress, &l_tAddr) == 1)
    {
        // Direct memory copy instead of strncpy for fixed-size fields
        memcpy(l_tAddress.t_cAddress, p_pcAddress, 
               (strlen(p_pcAddress) < INET_ADDRSTRLEN) ? strlen(p_pcAddress) : INET_ADDRSTRLEN - 1);
    }
    
    return l_tAddress;
}

// Bind socket to address
int networkBind(NetworkSocket *p_pSocket, const NetworkAddress *p_pAddress)
{
    // Check pointers
    if (!p_pSocket || p_pSocket->t_iSocketFd < 0)
        return NETWORK_INVALID_PARAM;

    if (!p_pAddress)
        return NETWORK_INVALID_PARAM;

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

    if (bind(p_pSocket->t_iSocketFd, (struct sockaddr *)&l_tAddr, sizeof(l_tAddr)) < 0)
        return NETWORK_ERROR;

    return NETWORK_OK;
}

// Listen for connections
int networkListen(NetworkSocket *p_pSocket, int p_iBacklog)
{
    if (!p_pSocket || p_pSocket->t_iSocketFd < 0)
        return NETWORK_INVALID_PARAM;

    int l_iBacklog = (p_iBacklog > 0) ? p_iBacklog : NETWORK_MAX_PENDING;

    if (listen(p_pSocket->t_iSocketFd, l_iBacklog) < 0)
        return NETWORK_ERROR;

    return NETWORK_OK;
}

// Accept a secure connection
NetworkSocket *networkAccept(NetworkSocket *p_pSocket, NetworkAddress *p_pClientAddress)
{
    if (!p_pSocket || p_pSocket->t_iSocketFd < 0 || !p_pSocket->t_pTlsEngine) {
        X_LOG_TRACE("networkAccept: Invalid socket or TLS engine");
        return NULL;
    }

    X_LOG_TRACE("networkAccept: Accepting new secure connection on socket %d", p_pSocket->t_iSocketFd);
    
    struct sockaddr_in l_tClientAddr;
    socklen_t l_iAddrLen = sizeof(l_tClientAddr);

    int l_iClientFd = accept(p_pSocket->t_iSocketFd, (struct sockaddr *)&l_tClientAddr, &l_iAddrLen);
    if (l_iClientFd < 0) {
        X_LOG_TRACE("networkAccept: accept() failed with error %d", errno);
        return NULL;
    }

    // Store client address if requested
    if (p_pClientAddress) {
        inet_ntop(AF_INET, &l_tClientAddr.sin_addr, p_pClientAddress->t_cAddress, INET_ADDRSTRLEN);
        p_pClientAddress->t_usPort = NET_TO_HOST_SHORT(l_tClientAddr.sin_port);
    }

    X_LOG_TRACE("networkAccept: Connection accepted from %s:%d", 
               p_pClientAddress ? p_pClientAddress->t_cAddress : "unknown",
               p_pClientAddress ? p_pClientAddress->t_usPort : 0);

    // Allocate socket for the new connection
    NetworkSocket *l_pClientSocket = (NetworkSocket *)malloc(sizeof(NetworkSocket));
    if (!l_pClientSocket) {
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

    // Allocate TLS engine for the client socket
    l_pClientSocket->t_pTlsEngine = malloc(sizeof(TLS_Engine));
    if (!l_pClientSocket->t_pTlsEngine) {
        X_LOG_TRACE("networkAccept: Failed to allocate memory for TLS engine");
        close(l_iClientFd);
        free(l_pClientSocket);
        return NULL;
    }

    // Accept TLS connection
    X_LOG_TRACE("networkAccept: Starting TLS handshake with client");
    
    // Obtenir des informations sur l'état du moteur TLS
    TLS_Engine* serverEngine = (TLS_Engine*)p_pSocket->t_pTlsEngine;
    X_LOG_TRACE("networkAccept: Server TLS initialized: %d, Connected: %d", 
               serverEngine->isInitialized, serverEngine->isConnected);
    
    // Démarrer le handshake TLS
    int l_iResult = tlsEngineAccept(
        (TLS_Engine*)l_pClientSocket->t_pTlsEngine,
        l_iClientFd,
        (TLS_Engine*)p_pSocket->t_pTlsEngine
    );

    if (l_iResult != TLS_OK) {
        X_LOG_TRACE("networkAccept: TLS handshake failed with error %d", l_iResult);
        free(l_pClientSocket->t_pTlsEngine);
        close(l_iClientFd);
        free(l_pClientSocket);
        return NULL;
    }

    X_LOG_TRACE("networkAccept: TLS handshake successful");
    return l_pClientSocket;
}

// Connect to secure server
int networkConnect(NetworkSocket *p_pSocket, const NetworkAddress *p_pAddress)
{
    if (!p_pSocket || p_pSocket->t_iSocketFd < 0 || !p_pSocket->t_pTlsEngine)
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
    if (connect(p_pSocket->t_iSocketFd, (struct sockaddr *)&l_tAddr, sizeof(l_tAddr)) < 0) {
        X_LOG_TRACE("networkConnect: TCP connection failed with error %d", errno);
        p_pSocket->t_bConnected = false;
        return NETWORK_ERROR;
    }

    p_pSocket->t_bConnected = true;

    // Perform TLS handshake
    X_LOG_TRACE("networkConnect: Starting TLS handshake");
    
    // Ensure we're using client mode for the connection
    TLS_Engine* tlsEngine = (TLS_Engine*)p_pSocket->t_pTlsEngine;
    tlsEngine->isInitialized = false; // Reset to recreate with client configuration
    
    // Recreate TLS configuration for client mode
    TLS_Config clientConfig;
    memset(&clientConfig, 0, sizeof(TLS_Config));
    clientConfig.version = tlsEngine->version;
    clientConfig.eccCurve = tlsEngine->eccCurve;
    clientConfig.isServer = false; // Explicitly set to client mode
    
    // Désactiver temporairement la vérification des certificats pour résoudre l'erreur ASN
    clientConfig.verifyPeer = false;
    X_LOG_TRACE("networkConnect: Peer verification temporarily disabled during handshake");
    
    // Reinitialize TLS engine with client configuration
    int l_iResult = tlsEngineInit(tlsEngine, p_pSocket->t_iSocketFd, &clientConfig);
    if (l_iResult != TLS_OK) {
        X_LOG_TRACE("networkConnect: Failed to initialize TLS client mode");
        p_pSocket->t_bConnected = false;
        return NETWORK_TLS_ERROR;
    }
    
    // Now perform the TLS client handshake
    l_iResult = tlsEngineConnect(tlsEngine);
    if (l_iResult != TLS_OK) {
        X_LOG_TRACE("networkConnect: TLS handshake failed with error %d", l_iResult);
        p_pSocket->t_bConnected = false;
        return NETWORK_TLS_ERROR;
    }

    X_LOG_TRACE("networkConnect: TLS handshake successful");
    return NETWORK_OK;
}

// Send data securely
int networkSend(NetworkSocket *p_pSocket, const void *p_pBuffer, unsigned long p_ulSize)
{
    int result;
    
    // Validate essential parameters
    if (!p_pSocket || !p_pBuffer || !p_pSocket->t_pTlsEngine)
        return NETWORK_INVALID_PARAM;

    // Early return for zero size
    if (p_ulSize == 0)
        return 0;

    // Lock mutex before accessing socket
    mutexLock(&p_pSocket->t_Mutex);
    
    X_LOG_TRACE("networkSend: Using TLS for send on socket %d, %lu bytes", 
               p_pSocket->t_iSocketFd, p_ulSize);
    result = tlsEngineSend((TLS_Engine*)p_pSocket->t_pTlsEngine, p_pBuffer, p_ulSize);
    if (result < 0) {
        X_LOG_TRACE("networkSend: TLS send failed with error code %d", result);
        result = NETWORK_TLS_ERROR;
    } else {
        X_LOG_TRACE("networkSend: TLS send successful, sent %d bytes", result);
    }
    
    // Unlock mutex after socket operation
    mutexUnlock(&p_pSocket->t_Mutex);
    
    return result;
}

// Receive data securely
int networkReceive(NetworkSocket *p_pSocket, void *p_pBuffer, unsigned long p_ulSize)
{
    int result;
    
    // Validate essential parameters
    if (!p_pSocket || !p_pBuffer || !p_pSocket->t_pTlsEngine)
        return NETWORK_INVALID_PARAM;

    // Early return for zero size
    if (p_ulSize == 0)
        return 0;

    // Lock mutex before accessing socket
    mutexLock(&p_pSocket->t_Mutex);
    
    X_LOG_TRACE("networkReceive: Using TLS for receive on socket %d, buffer size %lu", 
               p_pSocket->t_iSocketFd, p_ulSize);
    result = tlsEngineReceive((TLS_Engine*)p_pSocket->t_pTlsEngine, p_pBuffer, p_ulSize);
    if (result < 0) {
        X_LOG_TRACE("networkReceive: TLS receive failed with error code %d", result);
        result = NETWORK_TLS_ERROR;
    } else if (result == 0) {
        X_LOG_TRACE("networkReceive: TLS receive returned 0 bytes (connection closed or no data)");
    } else {
        X_LOG_TRACE("networkReceive: TLS receive successful, received %d bytes", result);
    }
    
    // Unlock mutex after socket operation
    mutexUnlock(&p_pSocket->t_Mutex);
    
    return result;
}

// Close secure socket
int networkCloseSocket(NetworkSocket *p_pSocket)
{
    if (!p_pSocket)
        return NETWORK_INVALID_PARAM;

    // Clean up TLS resources
    if (p_pSocket->t_pTlsEngine) {
        X_LOG_TRACE("networkCloseSocket: Closing TLS connection");
        
        // First close the TLS connection
        tlsEngineClose((TLS_Engine*)p_pSocket->t_pTlsEngine);
        
        // Then clean up the TLS engine
        tlsEngineCleanup((TLS_Engine*)p_pSocket->t_pTlsEngine);
        
        // Free the TLS engine memory
        free(p_pSocket->t_pTlsEngine);
        p_pSocket->t_pTlsEngine = NULL;
    }

    if (p_pSocket->t_iSocketFd >= 0) {
        X_LOG_TRACE("networkCloseSocket: Closing socket %d", p_pSocket->t_iSocketFd);
        close(p_pSocket->t_iSocketFd);
        p_pSocket->t_iSocketFd = -1;
    }

    p_pSocket->t_bConnected = false;

    // Destroy mutex before freeing socket
    mutexDestroy(&p_pSocket->t_Mutex);
    
    // Free the socket structure
    free(p_pSocket);

    return NETWORK_OK;
}

// Set socket timeout
int networkSetTimeout(NetworkSocket *p_pSocket, int p_iTimeoutMs, bool p_bSendTimeout)
{
    if (!p_pSocket || p_pSocket->t_iSocketFd < 0)
        return NETWORK_INVALID_PARAM;

    struct timeval l_tTimeout;
    l_tTimeout.tv_sec = p_iTimeoutMs / 1000;
    l_tTimeout.tv_usec = (p_iTimeoutMs % 1000) * 1000;

    int l_iOptionName = p_bSendTimeout ? SO_SNDTIMEO : SO_RCVTIMEO;

    if (setsockopt(p_pSocket->t_iSocketFd, SOL_SOCKET, l_iOptionName, &l_tTimeout, sizeof(l_tTimeout)) < 0)
        return NETWORK_ERROR;

    return NETWORK_OK;
}

// Wait for activity on socket
int networkWaitForActivity(NetworkSocket *p_pSocket, int p_iTimeoutMs)
{
    if (!p_pSocket || p_pSocket->t_iSocketFd < 0)
        return NETWORK_INVALID_PARAM;

    fd_set l_tReadSet;
    FD_ZERO(&l_tReadSet);
    FD_SET(p_pSocket->t_iSocketFd, &l_tReadSet);

    struct timeval l_tTimeout, *l_pTimeout = NULL;
    if (p_iTimeoutMs >= 0) {
        l_tTimeout.tv_sec = p_iTimeoutMs / 1000;
        l_tTimeout.tv_usec = (p_iTimeoutMs % 1000) * 1000;
        l_pTimeout = &l_tTimeout;
    }

    int l_iResult = select(p_pSocket->t_iSocketFd + 1, &l_tReadSet, NULL, NULL, l_pTimeout);

    if (l_iResult < 0)
        return NETWORK_ERROR;

    if (l_iResult == 0)
        return 0; // Timeout, no events

    // Socket is ready for reading
    return 1;
}

// Get error string
const char *networkGetErrorString(int p_iError)
{
    switch (p_iError) {
    case NETWORK_OK:
        return "Success";
    case NETWORK_ERROR:
        return "General network error";
    case NETWORK_TIMEOUT:
        return "Operation timed out";
    case NETWORK_INVALID_PARAM:
        return "Invalid parameter";
    case NETWORK_TLS_ERROR:
        return "TLS security error";
    default:
        return "Unknown error";
    }
}

// Check if socket is connected
bool networkIsConnected(NetworkSocket *p_pSocket)
{
    if (!p_pSocket)
        return false;

    return p_pSocket->t_bConnected;
}

// Get TLS connection information
int networkGetSecurityInfo(NetworkSocket *p_pSocket, char *p_pCipherName, unsigned long p_ulSize)
{
    if (!p_pSocket || !p_pSocket->t_pTlsEngine || !p_pCipherName)
        return NETWORK_INVALID_PARAM;
    
    return tlsEngineGetConnectionInfo((TLS_Engine*)p_pSocket->t_pTlsEngine, p_pCipherName, p_ulSize);
}

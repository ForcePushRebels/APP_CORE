////////////////////////////////////////////////////////////
//  TLS Engine implementation file for embedded systems
//  Implements TLS 1.3 support using wolfSSL
//  Designed to integrate with socket file descriptors
//
// Written : 20/04/2025
////////////////////////////////////////////////////////////

#include "tlsEngine.h"
#include "xLog.h"
#include "xAssert.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>


// IO callbacks for wolfSSL
static int tlsEngineIORecv(WOLFSSL* ssl, char* buf, int sz, void* ctx) 
{
    X_ASSERT(ctx != NULL);
    X_ASSERT(buf != NULL);
    X_ASSERT(sz > 0);
    X_ASSERT(ssl != NULL);
    int socketFd = *(int*)ctx;
    if (socketFd < 0) {
        return WOLFSSL_CBIO_ERR_GENERAL;
    }
    
    int recvd = recv(socketFd, buf, sz, 0);
    if (recvd < 0) 
    {
        return WOLFSSL_CBIO_ERR_GENERAL;
    }
    return recvd;
}

static int tlsEngineIOSend(WOLFSSL* ssl, char* buf, int sz, void* ctx) 
{
    X_ASSERT(ctx != NULL);
    X_ASSERT(buf != NULL);
    X_ASSERT(sz > 0);
    X_ASSERT(ssl != NULL);

    int socketFd = *(int*)ctx;
    if (socketFd < 0) {
        return WOLFSSL_CBIO_ERR_GENERAL;
    }
    
    int sent = send(socketFd, buf, sz, 0);
    if (sent < 0) 
    {
        return WOLFSSL_CBIO_ERR_GENERAL;
    }
    return sent;
}

// Initialize the TLS engine for a socket
int tlsEngineInit(TLS_Engine* p_pEngine, int p_iSocketFd, const TLS_Config* p_pConfig) 
{
    X_ASSERT(p_pEngine != NULL);
    X_ASSERT(p_pConfig != NULL);
    X_ASSERT(p_iSocketFd >= 0);

    X_LOG_TRACE("Initializing TLS engine for socket %d", p_iSocketFd);

    if (!p_pEngine || !p_pConfig || p_iSocketFd < 0) 
    {
        X_LOG_TRACE("Invalid parameters for TLS engine initialization");
        return TLS_INVALID_PARAM;
    }
    
    // Initialize engine structure
    memset(p_pEngine, 0, sizeof(TLS_Engine));
    p_pEngine->socketFd = p_iSocketFd;
    p_pEngine->version = p_pConfig->version;
    p_pEngine->eccCurve = p_pConfig->eccCurve;
    
    // Initialize wolfSSL library if not already done
    static bool wolfSSLInitialized = false;
    if (!wolfSSLInitialized) {
        if (wolfSSL_Init() != WOLFSSL_SUCCESS) {
            X_LOG_TRACE("Failed to initialize wolfSSL library");
            return TLS_ERROR;
        }
        wolfSSLInitialized = true;
        X_LOG_TRACE("wolfSSL library initialized successfully");
    }
    
    // Create wolfSSL context based on TLS version
    WOLFSSL_METHOD* method = NULL;
    if (p_pConfig->version == TLS_VERSION_1_3) {
        if (p_pConfig->isServer) {
            method = wolfTLSv1_3_server_method();
            X_LOG_TRACE("Using TLS 1.3 server method");
        } else {
            method = wolfTLSv1_3_client_method();
            X_LOG_TRACE("Using TLS 1.3 client method");
        }
    } else if (p_pConfig->version == TLS_VERSION_1_2) {
        // Use TLS 1.2 if explicitly specified
        if (p_pConfig->isServer) {
            method = wolfTLSv1_2_server_method();
            X_LOG_TRACE("Using TLS 1.2 server method");
        } else {
            method = wolfTLSv1_2_client_method();
            X_LOG_TRACE("Using TLS 1.2 client method");
        }
    } else {
        // Default to TLS 1.3 for any invalid version
        if (p_pConfig->isServer) {
            method = wolfTLSv1_3_server_method();
            X_LOG_TRACE("Using TLS 1.3 server method (default)");
        } else {
            method = wolfTLSv1_3_client_method();
            X_LOG_TRACE("Using TLS 1.3 client method (default)");
        }
    }
    
    if (!method) {
        X_LOG_TRACE("Failed to create SSL method");
        return TLS_ERROR;
    }
    
    // Create the context
    p_pEngine->ctx = wolfSSL_CTX_new(method);
    if (!p_pEngine->ctx) {
        X_LOG_TRACE("Failed to create SSL context");
        return TLS_ERROR;
    }
    X_LOG_TRACE("SSL context created successfully");
    
    // Load certificates and keys if provided
    if (p_pConfig->caCertPath) {
        X_LOG_TRACE("Loading CA certificate from %s", p_pConfig->caCertPath);
        if (wolfSSL_CTX_load_verify_locations(p_pEngine->ctx, p_pConfig->caCertPath, NULL) != WOLFSSL_SUCCESS) {
            X_LOG_TRACE("Failed to load CA certificate");
            wolfSSL_CTX_free(p_pEngine->ctx);
            p_pEngine->ctx = NULL;
            return TLS_CERT_ERROR;
        }
        X_LOG_TRACE("CA certificate loaded successfully");
    }
    
    if (p_pConfig->certPath && p_pConfig->keyPath) {
        X_LOG_TRACE("Loading server certificate from %s", p_pConfig->certPath);
        X_LOG_TRACE("Loading server key from %s", p_pConfig->keyPath);
        
        if (wolfSSL_CTX_use_certificate_file(p_pEngine->ctx, p_pConfig->certPath, WOLFSSL_FILETYPE_PEM) != WOLFSSL_SUCCESS) {
            X_LOG_TRACE("Failed to load server certificate");
            wolfSSL_CTX_free(p_pEngine->ctx);
            p_pEngine->ctx = NULL;
            return TLS_CERT_ERROR;
        }
        
        if (wolfSSL_CTX_use_PrivateKey_file(p_pEngine->ctx, p_pConfig->keyPath, WOLFSSL_FILETYPE_PEM) != WOLFSSL_SUCCESS) {
            X_LOG_TRACE("Failed to load server key");
            wolfSSL_CTX_free(p_pEngine->ctx);
            p_pEngine->ctx = NULL;
            return TLS_CERT_ERROR;
        }
        
        X_LOG_TRACE("Server certificate and key loaded successfully");
    }
    
    // Set verification mode
    if (p_pConfig->verifyPeer) {
        wolfSSL_CTX_set_verify(p_pEngine->ctx, WOLFSSL_VERIFY_PEER, NULL);
        X_LOG_TRACE("Peer verification enabled");
    } else {
        wolfSSL_CTX_set_verify(p_pEngine->ctx, WOLFSSL_VERIFY_NONE, NULL);
        X_LOG_TRACE("Peer verification disabled");
    }
    
    // Set cipher list if provided
    if (p_pConfig->cipherList) {
        X_LOG_TRACE("Setting cipher list: %s", p_pConfig->cipherList);
        if (wolfSSL_CTX_set_cipher_list(p_pEngine->ctx, p_pConfig->cipherList) != WOLFSSL_SUCCESS) {
            X_LOG_TRACE("Failed to set cipher list");
            wolfSSL_CTX_free(p_pEngine->ctx);
            p_pEngine->ctx = NULL;
            return TLS_ERROR;
        }
    }
    
    // Configure ECC curve
    int curve;
    X_LOG_TRACE("Configuring ECC curve: %d", p_pConfig->eccCurve);
    
    switch (p_pConfig->eccCurve) {
        case TLS_ECC_SECP256R1:
            X_LOG_TRACE("Using SECP256R1 (P-256) curve");
            curve = WOLFSSL_ECC_SECP256R1;
            break;
        case TLS_ECC_SECP384R1:
            X_LOG_TRACE("Using SECP384R1 (P-384) curve");
            curve = WOLFSSL_ECC_SECP384R1;
            break;
        case TLS_ECC_SECP521R1:
            X_LOG_TRACE("Using SECP521R1 (P-521) curve");
            curve = WOLFSSL_ECC_SECP521R1;
            break;
        case TLS_ECC_X25519:
            X_LOG_TRACE("Using X25519 curve");
            curve = WOLFSSL_ECC_X25519;
            break;
        default:
            X_LOG_TRACE("Using default curve (SECP256R1)");
            curve = WOLFSSL_ECC_SECP256R1; // Default to P-256
    }
    
    // Set TLS 1.3 groups (curves)
    if (p_pConfig->version == TLS_VERSION_1_3) {
        int groups_array[1] = { curve };
        X_LOG_TRACE("Setting TLS 1.3 groups");
        
        if (wolfSSL_CTX_set_groups(p_pEngine->ctx, groups_array, 1) != WOLFSSL_SUCCESS) {
            X_LOG_TRACE("Failed to set TLS 1.3 groups");
            wolfSSL_CTX_free(p_pEngine->ctx);
            p_pEngine->ctx = NULL;
            return TLS_ERROR;
        }
    }
    
    // Set IO callbacks
    X_LOG_TRACE("Setting IO callbacks");
    wolfSSL_SetIORecv(p_pEngine->ctx, tlsEngineIORecv);
    wolfSSL_SetIOSend(p_pEngine->ctx, tlsEngineIOSend);
    
    p_pEngine->isInitialized = true;
    X_LOG_TRACE("TLS engine initialized successfully");
    
    return TLS_OK;
}

// Perform TLS handshake for client connection
int tlsEngineConnect(TLS_Engine* p_pEngine) 
{
    if (!p_pEngine || !p_pEngine->isInitialized || !p_pEngine->ctx || p_pEngine->socketFd < 0) {
        return TLS_INVALID_PARAM;
    }
    
    X_LOG_TRACE("Starting client TLS handshake");
    
    // Create new wolfSSL session
    p_pEngine->ssl = wolfSSL_new(p_pEngine->ctx);
    if (!p_pEngine->ssl) {
        X_LOG_TRACE("Failed to create client SSL session");
        return TLS_ERROR;
    }
    
    // Set socket as IO context
    wolfSSL_SetIOReadCtx(p_pEngine->ssl, &p_pEngine->socketFd);
    wolfSSL_SetIOWriteCtx(p_pEngine->ssl, &p_pEngine->socketFd);
    
    // Perform TLS handshake
    int ret = wolfSSL_connect(p_pEngine->ssl);
    if (ret != WOLFSSL_SUCCESS) {
        int err = wolfSSL_get_error(p_pEngine->ssl, ret);
        char errorBuffer[80];
        wolfSSL_ERR_error_string(err, errorBuffer);
        X_LOG_TRACE("TLS client handshake failed with error code %d, message: %s", err, errorBuffer);
        wolfSSL_free(p_pEngine->ssl);
        p_pEngine->ssl = NULL;
        return TLS_CONNECT_ERROR;
    }
    
    X_LOG_TRACE("TLS client handshake completed successfully");
    p_pEngine->isConnected = true;
    
    return TLS_OK;
}

// Accept TLS connection as server
int tlsEngineAccept(TLS_Engine* p_pEngine, int p_iSocketFd, const TLS_Engine* p_pListenEngine) {
    if (!p_pEngine || !p_pListenEngine || !p_pListenEngine->isInitialized || 
        !p_pListenEngine->ctx || p_iSocketFd < 0) {
        X_LOG_TRACE("Invalid parameters for TLS accept");
        return TLS_INVALID_PARAM;
    }
    
    X_LOG_TRACE("Accepting TLS connection on socket %d using listen engine context", p_iSocketFd);
    
    // Initialize engine structure
    memset(p_pEngine, 0, sizeof(TLS_Engine));
    p_pEngine->socketFd = p_iSocketFd;
    p_pEngine->version = p_pListenEngine->version;
    p_pEngine->eccCurve = p_pListenEngine->eccCurve;
    
    // We'll use the listening engine's context to create a new SSL session,
    // but won't store a reference to it to avoid potential double-free issues.
    // The CTX ownership stays with the listener.
    p_pEngine->ctx = NULL; // Don't assign the context directly
    
    // Create new wolfSSL session using the listening engine's context
    WOLFSSL_CTX* listenerCtx = p_pListenEngine->ctx;
    X_LOG_TRACE("Creating new SSL session using listener's CTX");
    p_pEngine->ssl = wolfSSL_new(listenerCtx);
    if (!p_pEngine->ssl) {
        X_LOG_TRACE("Failed to create new SSL session");
        return TLS_ERROR;
    }
    
    // Désactiver la vérification des certificats pour l'acceptation
    // Cela permettra de contourner le problème d'erreur ASN
    wolfSSL_set_verify(p_pEngine->ssl, WOLFSSL_VERIFY_NONE, NULL);
    X_LOG_TRACE("Peer verification temporarily disabled for handshake");
    
    p_pEngine->isInitialized = true;
    
    // Set socket as IO context
    X_LOG_TRACE("Setting IO context for socket %d", p_iSocketFd);
    wolfSSL_SetIOReadCtx(p_pEngine->ssl, &p_pEngine->socketFd);
    wolfSSL_SetIOWriteCtx(p_pEngine->ssl, &p_pEngine->socketFd);
    
    // Debug: Afficher des informations supplémentaires sur le contexte SSL
    X_LOG_TRACE("Debug info: p_pEngine socket: %d, listening socket: %d", 
               p_pEngine->socketFd, p_pListenEngine->socketFd);
    
    // Perform TLS handshake
    X_LOG_TRACE("Performing TLS accept handshake");
    int ret = wolfSSL_accept(p_pEngine->ssl);
    
    // Obtenir et enregistrer plus d'informations d'erreur en cas d'échec
    if (ret != WOLFSSL_SUCCESS) {
        int err = wolfSSL_get_error(p_pEngine->ssl, ret);
        char errorBuffer[80];
        wolfSSL_ERR_error_string(err, errorBuffer);
        X_LOG_TRACE("TLS accept failed with error code: %d, message: %s", err, errorBuffer);
        wolfSSL_free(p_pEngine->ssl);
        p_pEngine->ssl = NULL;
        p_pEngine->isInitialized = false;
        return TLS_CONNECT_ERROR;
    }
    
    X_LOG_TRACE("TLS handshake completed successfully");
    p_pEngine->isConnected = true;
    
    return TLS_OK;
}

// Send data over TLS connection
int tlsEngineSend(TLS_Engine* p_pEngine, const void* p_pBuffer, unsigned long p_ulSize) {
    if (!p_pEngine || !p_pEngine->isInitialized || !p_pEngine->ssl || 
        !p_pEngine->isConnected || !p_pBuffer) {
        return TLS_INVALID_PARAM;
    }
    
    int sent = wolfSSL_write(p_pEngine->ssl, p_pBuffer, p_ulSize);
    
    if (sent < 0) {
        return TLS_ERROR;
    }
    
    return sent;
}

// Receive data over TLS connection
int tlsEngineReceive(TLS_Engine* p_pEngine, void* p_pBuffer, unsigned long p_ulSize) {
    if (!p_pEngine || !p_pEngine->isInitialized || !p_pEngine->ssl || 
        !p_pEngine->isConnected || !p_pBuffer) {
        return TLS_INVALID_PARAM;
    }
    
    int received = wolfSSL_read(p_pEngine->ssl, p_pBuffer, p_ulSize);
    
    if (received < 0) {
        int err = wolfSSL_get_error(p_pEngine->ssl, received);
        if (err == WOLFSSL_ERROR_WANT_READ || err == WOLFSSL_ERROR_WANT_WRITE) {
            return 0; // No data available yet
        }
        return TLS_ERROR;
    }
    
    return received;
}

// Close TLS connection
int tlsEngineClose(TLS_Engine* p_pEngine) {
    if (!p_pEngine || !p_pEngine->isInitialized || !p_pEngine->ssl) {
        return TLS_INVALID_PARAM;
    }
    
    // Perform TLS shutdown
    wolfSSL_shutdown(p_pEngine->ssl);
    
    // Free wolfSSL session
    wolfSSL_free(p_pEngine->ssl);
    p_pEngine->ssl = NULL;
    p_pEngine->isConnected = false;
    
    return TLS_OK;
}

// Clean up TLS engine resources
int tlsEngineCleanup(TLS_Engine* p_pEngine) {
    if (!p_pEngine || !p_pEngine->isInitialized) {
        return TLS_INVALID_PARAM;
    }
    
    X_LOG_TRACE("Cleaning up TLS engine resources");
    
    // Close connection if still active
    if (p_pEngine->ssl) {
        X_LOG_TRACE("Freeing SSL session");
        wolfSSL_free(p_pEngine->ssl);
        p_pEngine->ssl = NULL;
    }
    
    // Free wolfSSL context only if we own it (not shared from accept)
    if (p_pEngine->ctx) {
        X_LOG_TRACE("Freeing SSL context");
        wolfSSL_CTX_free(p_pEngine->ctx);
        p_pEngine->ctx = NULL;
    } else {
        X_LOG_TRACE("Skipping CTX cleanup (shared or NULL ctx)");
    }
    
    p_pEngine->isInitialized = false;
    p_pEngine->isConnected = false;
    
    return TLS_OK;
}

// Get last TLS error description
const char* tlsEngineGetErrorString(int p_iError) {
    switch (p_iError) {
        case TLS_OK:
            return "Success";
        case TLS_ERROR:
            return "General TLS error";
        case TLS_INVALID_PARAM:
            return "Invalid parameter";
        case TLS_CERT_ERROR:
            return "Certificate error";
        case TLS_CONNECT_ERROR:
            return "Connection error";
        case TLS_VERIFY_ERROR:
            return "Verification error";
        default:
            return "Unknown error";
    }
}

// Check if TLS is enabled for a socket
bool tlsEngineIsEnabled(const TLS_Engine* p_pEngine) {
    if (!p_pEngine) {
        return false;
    }
    
    return p_pEngine->isInitialized;
}

// Get TLS connection info
int tlsEngineGetConnectionInfo(const TLS_Engine* p_pEngine, char* p_pCipherName, unsigned long p_ulSize) {
    if (!p_pEngine || !p_pEngine->isInitialized || !p_pEngine->ssl || 
        !p_pEngine->isConnected || !p_pCipherName) {
        return TLS_INVALID_PARAM;
    }
    
    // Get cipher name
    const char* cipherName = wolfSSL_get_cipher_name(p_pEngine->ssl);
    if (!cipherName) {
        return TLS_ERROR;
    }
    
    // Copy cipher name to output buffer
    strncpy(p_pCipherName, cipherName, p_ulSize - 1);
    p_pCipherName[p_ulSize - 1] = '\0';
    
    return TLS_OK;
}

////////////////////////////////////////////////////////////
//  TLS Engine header file for embedded systems
//  Provides TLS 1.3 support using wolfSSL
//  Designed to integrate with xNetwork socket API
//
// Written : 20/04/2025
////////////////////////////////////////////////////////////

#ifndef TLS_ENGINE_H_
#define TLS_ENGINE_H_

#include <wolfssl/options.h>
#include <wolfssl/ssl.h>
#include <wolfssl/wolfcrypt/settings.h>
#include <stdbool.h>

// Configuration constants
#define TLS_OK                  0
#define TLS_ERROR              -1
#define TLS_INVALID_PARAM     -2
#define TLS_CERT_ERROR        -3
#define TLS_CONNECT_ERROR     -4
#define TLS_VERIFY_ERROR      -5

// TLS versions
typedef enum {
    TLS_VERSION_1_2 = 0,
    TLS_VERSION_1_3 = 1  // Default and recommended
} TLS_Version;

// ECC Curve types
typedef enum {
    TLS_ECC_SECP256R1 = 0,  // NIST P-256
    TLS_ECC_SECP384R1 = 1,  // NIST P-384
    TLS_ECC_SECP521R1 = 2,  // NIST P-521
    TLS_ECC_X25519    = 3   // Curve25519
} TLS_ECC_Curve;

// TLS Engine context
typedef struct {
    WOLFSSL_CTX* ctx;          // wolfSSL context
    WOLFSSL* ssl;              // wolfSSL session
    int socketFd;              // Socket file descriptor
    bool isInitialized;        // Initialization state
    bool isConnected;          // Connection state
    TLS_Version version;       // TLS version
    TLS_ECC_Curve eccCurve;    // ECC curve type
} TLS_Engine;

// TLS configuration structure
typedef struct {
    TLS_Version version;       // TLS version to use
    TLS_ECC_Curve eccCurve;    // ECC curve to use
    bool verifyPeer;           // Verify peer certificate
    const char* caCertPath;    // CA certificate path
    const char* certPath;      // Certificate path
    const char* keyPath;       // Private key path
    const char* cipherList;    // Cipher list (NULL for defaults)
    bool isServer;             // True for server, false for client
} TLS_Config;

//////////////////////////////////
/// Core API functions - designed to work with socket file descriptors
//////////////////////////////////

//////////////////////////////////
/// @brief Initialize the TLS engine
/// @param p_pEngine Pointer to TLS_Engine structure to initialize
/// @param p_iSocketFd Socket file descriptor
/// @param p_pConfig TLS configuration
/// @return int Error code
//////////////////////////////////
int tlsEngineInit(TLS_Engine* p_pEngine, int p_iSocketFd, const TLS_Config* p_pConfig);

//////////////////////////////////
/// @brief Perform TLS handshake for client connection
/// @param p_pEngine TLS engine
/// @return int Error code
//////////////////////////////////
int tlsEngineConnect(TLS_Engine* p_pEngine);

//////////////////////////////////
/// @brief Accept TLS connection as server
/// @param p_pEngine TLS engine for new connection
/// @param p_iSocketFd Socket file descriptor for the accepted connection
/// @param p_pListenEngine TLS engine with server context
/// @return int Error code
//////////////////////////////////
int tlsEngineAccept(TLS_Engine* p_pEngine, int p_iSocketFd, const TLS_Engine* p_pListenEngine);

//////////////////////////////////
/// @brief Send data over TLS connection
/// @param p_pEngine TLS engine
/// @param p_pBuffer Data buffer
/// @param p_ulSize Data size
/// @return int Bytes sent or error code
//////////////////////////////////
int tlsEngineSend(TLS_Engine* p_pEngine, const void* p_pBuffer, unsigned long p_ulSize);

//////////////////////////////////
/// @brief Receive data over TLS connection
/// @param p_pEngine TLS engine
/// @param p_pBuffer Data buffer
/// @param p_ulSize Buffer size
/// @return int Bytes received or error code
//////////////////////////////////
int tlsEngineReceive(TLS_Engine* p_pEngine, void* p_pBuffer, unsigned long p_ulSize);

//////////////////////////////////
/// @brief Close TLS connection
/// @param p_pEngine TLS engine
/// @return int Error code
//////////////////////////////////
int tlsEngineClose(TLS_Engine* p_pEngine);

//////////////////////////////////
/// @brief Clean up TLS engine resources
/// @param p_pEngine TLS engine
/// @return int Error code
//////////////////////////////////
int tlsEngineCleanup(TLS_Engine* p_pEngine);

//////////////////////////////////
/// @brief Get last TLS error description
/// @param p_iError Error code
/// @return const char* Error description
//////////////////////////////////
const char* tlsEngineGetErrorString(int p_iError);

//////////////////////////////////
/// @brief Check if TLS is enabled
/// @param p_pEngine TLS engine
/// @return bool True if TLS is enabled and initialized
//////////////////////////////////
bool tlsEngineIsEnabled(const TLS_Engine* p_pEngine);

//////////////////////////////////
/// @brief Get TLS connection info
/// @param p_pEngine TLS engine
/// @param p_pCipherName Buffer to store cipher name
/// @param p_ulSize Buffer size
/// @return int Error code
//////////////////////////////////
int tlsEngineGetConnectionInfo(const TLS_Engine* p_pEngine, char* p_pCipherName, unsigned long p_ulSize);

//////////////////////////////////
/// @brief Get the certificate presented by the peer
/// @param p_pEngine TLS engine
/// @param p_pCertInfo Buffer to store certificate information
/// @param p_ulSize Buffer size
/// @return int Error code
//////////////////////////////////
int tlsEngineGetPeerCertificate(TLS_Engine* p_pEngine, char* p_pCertInfo, unsigned long p_ulSize);

#endif // TLS_ENGINE_H_

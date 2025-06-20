////////////////////////////////////////////////////////////
//  tlsEngine header file
//  defines the TLS engine types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/06/2025
////////////////////////////////////////////////////////////

#ifndef X_TLS_ENGINE_H
#define X_TLS_ENGINE_H

#include <wolfssl/options.h>
#include <wolfssl/ssl.h>
#include <stdbool.h>

#include "certificateManagement.h"
#include "xLog.h"
#include "xOsMemory.h"
#include "xAssert.h"
#include "xNetwork.h"

////////////////////////////////////////////////////////////
// TLS Engine Types
////////////////////////////////////////////////////////////

typedef enum {
    TLS_MODE_CLIENT = 0,
    TLS_MODE_SERVER
} xTlsMode_t;

typedef struct xTlsEngine {
    WOLFSSL_CTX *p_ctx;   // WolfSSL context
    xTlsMode_t   t_mode;  // Client or server
} xTlsEngine_t;

////////////////////////////////////////////////////////////
// TLS Engine Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// @brief Create a TLS engine (context + cert/key + CA)
/// @param p_ppEngine Output: created TLS engine
/// @param p_eMode TLS_MODE_CLIENT or TLS_MODE_SERVER
/// @param p_pcCertFile Path to certificate file (can be NULL for client)
/// @param p_pcKeyFile Path to private key file (can be NULL for client)
/// @param p_pcCADir   Path to directory containing CA certificates
/// @param p_bIsPEM    True if certificate/key/CA are PEM, false for DER
/// @return int Error code (CERT_OK on success or CERT_ERROR_*)
//////////////////////////////////
int tlsEngineCreate(xTlsEngine_t **p_ppEngine,
                    xTlsMode_t p_eMode,
                    const char *p_pcCertFile,
                    const char *p_pcKeyFile,
                    const char *p_pcCADir,
                    bool p_bIsPEM);

//////////////////////////////////
/// @brief Attach a socket FD to TLS engine and perform handshake
/// @param p_ptEngine TLS engine
/// @param p_iSocketFd Socket file descriptor
/// @param p_ppSsl     Output: WolfSSL session object
/// @return int Error code (CERT_OK on success or CERT_ERROR_*)
//////////////////////////////////
int tlsEngineAttachSocket(xTlsEngine_t *p_ptEngine,
                          int p_iSocketFd,
                          WOLFSSL **p_ppSsl);

//////////////////////////////////
/// @brief Shutdown a TLS session and free resources
/// @param p_ptSsl WolfSSL session to shutdown
/// @return int Error code
//////////////////////////////////
int tlsEngineShutdown(WOLFSSL *p_ptSsl);

//////////////////////////////////
/// @brief Destroy a TLS engine (free context)
/// @param p_ptEngine TLS engine to destroy
/// @return int Error code
//////////////////////////////////
int tlsEngineDestroy(xTlsEngine_t *p_ptEngine);

#endif // X_TLS_ENGINE_H
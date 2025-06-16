////////////////////////////////////////////////////////////
//  tlsEngine source file
//  implements the TLS engine types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/06/2025
////////////////////////////////////////////////////////////

#include "tlsEngine.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

// Compatibility macro for older wolfSSL versions
#ifndef WOLFSSL_SUCCESS
#define WOLFSSL_SUCCESS SSL_SUCCESS
#endif

////////////////////////////////////////////////////////////
// Static helpers
////////////////////////////////////////////////////////////

//////////////////////////////////
/// loadCertAndKey
//////////////////////////////////
static int loadCertAndKey(WOLFSSL_CTX *p_ctx,
                          xTlsMode_t p_mode,
                          const char *p_certFile,
                          const char *p_keyFile,
                          bool p_isPem)
{
    if (!p_ctx)
        return CERT_ERROR_INVALID_PARAM;

    if (!p_certFile)
    {
        // Nothing to load (common for client)
        return CERT_OK;
    }

    int l_iTypeCert = p_isPem ? WOLFSSL_FILETYPE_PEM : WOLFSSL_FILETYPE_ASN1;

    int l_iResult;

    if (p_mode == TLS_MODE_SERVER)
    {
        // For server, load full chain (PEM only)
        l_iResult = wolfSSL_CTX_use_certificate_chain_file(p_ctx, p_certFile);
    }
    else
    {
        l_iResult = wolfSSL_CTX_use_certificate_file(p_ctx, p_certFile, l_iTypeCert);
    }

    if (l_iResult != WOLFSSL_SUCCESS)
    {
        X_LOG_TRACE("Failed to load certificate/chain file: %s", p_certFile);
        return CERT_ERROR_INVALID_CERT;
    }

    // Private key may be optional for client
    if (p_keyFile)
    {
        // Private key must be PEM for wolfSSL (most versions)
        if (wolfSSL_CTX_use_PrivateKey_file(p_ctx, p_keyFile, WOLFSSL_FILETYPE_PEM) != WOLFSSL_SUCCESS)
        {
            X_LOG_TRACE("Failed to load key file (PEM required): %s", p_keyFile);
            return CERT_ERROR_INVALID_CERT;
        }
    }

    return CERT_OK;
}

////////////////////////////////////////////////////////////
// Public API
////////////////////////////////////////////////////////////

//////////////////////////////////
/// tlsEngineCreate
//////////////////////////////////
int tlsEngineCreate(xTlsEngine_t **p_ppEngine,
                    xTlsMode_t p_eMode,
                    const char *p_pcCertFile,
                    const char *p_pcKeyFile,
                    const char *p_pcCADir,
                    bool p_bIsPEM)
{
    if (!p_ppEngine || !p_pcCADir)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    // Ensure certificate system initialized
    int l_iCertInit = xCertificateInit();
    if (l_iCertInit != CERT_OK)
    {
        return l_iCertInit;
    }

    // Select method based on mode
    WOLFSSL_METHOD *l_ptMethod = (p_eMode == TLS_MODE_SERVER) ?
                                    wolfTLSv1_3_server_method() :
                                    wolfTLSv1_3_client_method();
    if (!l_ptMethod)
    {
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    WOLFSSL_CTX *l_ptCtx = wolfSSL_CTX_new(l_ptMethod);
    if (!l_ptCtx)
    {
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    // Limit cipher suites to TLS1.3 strong suites (optional)
    wolfSSL_CTX_set_cipher_list(l_ptCtx, "TLS13-AES256-GCM-SHA384:TLS13-CHACHA20-POLY1305-SHA256:TLS13-AES128-GCM-SHA256");

    // Load end-entity certificate/key if provided
    int l_iRes = loadCertAndKey(l_ptCtx, p_eMode, p_pcCertFile, p_pcKeyFile, p_bIsPEM);
    if (l_iRes != CERT_OK)
    {
        wolfSSL_CTX_free(l_ptCtx);
        return l_iRes;
    }

    // Load root CA into context (per-context)
    l_iRes = xCertificateLoadRootCAIntoContext(l_ptCtx, p_pcCADir, p_bIsPEM);

    if (l_iRes != CERT_OK)
    {
        wolfSSL_CTX_free(l_ptCtx);
        return l_iRes;
    }

    // Allocate engine structure
    xTlsEngine_t *l_ptEngine = (xTlsEngine_t *)malloc(sizeof(xTlsEngine_t));
    if (!l_ptEngine)
    {
        wolfSSL_CTX_free(l_ptCtx);
        return CERT_ERROR_MEMORY_ALLOC;
    }

    l_ptEngine->p_ctx = l_ptCtx;
    l_ptEngine->t_mode = p_eMode;

    *p_ppEngine = l_ptEngine;

    X_LOG_TRACE("TLS engine created successfully (mode=%s)",
                (p_eMode == TLS_MODE_SERVER) ? "SERVER" : "CLIENT");

    return CERT_OK;
}

//////////////////////////////////
/// tlsEngineAttachSocket
//////////////////////////////////
int tlsEngineAttachSocket(xTlsEngine_t *p_ptEngine,
                          int p_iSocketFd,
                          WOLFSSL **p_ppSsl)
{
    if (!p_ptEngine || !p_ppSsl)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    WOLFSSL *l_ptSsl = wolfSSL_new(p_ptEngine->p_ctx);
    if (!l_ptSsl)
    {
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    if (wolfSSL_set_fd(l_ptSsl, p_iSocketFd) != WOLFSSL_SUCCESS)
    {
        wolfSSL_free(l_ptSsl);
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    int l_iHandshakeRes = (p_ptEngine->t_mode == TLS_MODE_SERVER) ?
                            wolfSSL_accept(l_ptSsl) :
                            wolfSSL_connect(l_ptSsl);

    if (l_iHandshakeRes != WOLFSSL_SUCCESS)
    {
        int l_iErr = wolfSSL_get_error(l_ptSsl, l_iHandshakeRes);
        X_LOG_TRACE("TLS handshake failed (err=%d)", l_iErr);
        wolfSSL_free(l_ptSsl);
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    *p_ppSsl = l_ptSsl;
    X_LOG_TRACE("TLS handshake completed successfully (fd=%d)", p_iSocketFd);

    return CERT_OK;
}

//////////////////////////////////
/// tlsEngineShutdown
//////////////////////////////////
int tlsEngineShutdown(WOLFSSL *p_ptSsl)
{
    if (!p_ptSsl)
    {
        return CERT_OK;
    }

    // Shutdown TLS proprement avec retry
    int l_iShutdownResult;
    int l_iRetryCount = 0;
    const int MAX_SHUTDOWN_RETRIES = 5;
    
    do {
        l_iShutdownResult = wolfSSL_shutdown(p_ptSsl);
        
        if (l_iShutdownResult == WOLFSSL_SUCCESS)
        {
            // Shutdown complet (close_notify envoyé et reçu)
            break;
        }
        else if (l_iShutdownResult == WOLFSSL_SHUTDOWN_NOT_DONE)
        {
            // Premier appel OK, attendre la réponse du peer
            l_iRetryCount++;
            if (l_iRetryCount >= MAX_SHUTDOWN_RETRIES)
            {
                X_LOG_TRACE("TLS shutdown timeout after %d retries", MAX_SHUTDOWN_RETRIES);
                break;
            }
            
            // Petit délai pour laisser le peer répondre
            struct timespec delay = {0, 10000000}; // 10ms
            nanosleep(&delay, NULL);
        }
        else
        {
            // Erreur ou peer déjà fermé
            int l_iError = wolfSSL_get_error(p_ptSsl, l_iShutdownResult);
            if (l_iError == WOLFSSL_ERROR_WANT_READ || l_iError == WOLFSSL_ERROR_WANT_WRITE)
            {
                // Retry nécessaire
                l_iRetryCount++;
                if (l_iRetryCount >= MAX_SHUTDOWN_RETRIES)
                {
                    X_LOG_TRACE("TLS shutdown WANT_READ/WRITE timeout");
                    break;
                }
                struct timespec delay = {0, 5000000}; // 5ms
                nanosleep(&delay, NULL);
            }
            else
            {
                // Erreur définitive ou connexion déjà fermée
                X_LOG_TRACE("TLS shutdown error: %d", l_iError);
                break;
            }
        }
    } while (l_iRetryCount < MAX_SHUTDOWN_RETRIES);

    // Fermer le socket TCP après le shutdown TLS
    int l_iFd = wolfSSL_get_fd(p_ptSsl);
    if (l_iFd >= 0)
    {
        // Fermeture gracieuse TCP
        shutdown(l_iFd, SHUT_WR);  // Fermer l'écriture d'abord
        
        // Optionnel : lire les données restantes pour vider le buffer
        char l_buffer[256];
        int l_iTimeout = 0;
        while (l_iTimeout < 10) // Max 10ms d'attente
        {
            fd_set readfds;
            struct timeval tv = {0, 1000}; // 1ms timeout
            
            FD_ZERO(&readfds);
            FD_SET(l_iFd, &readfds);
            
            int l_iSelect = select(l_iFd + 1, &readfds, NULL, NULL, &tv);
            if (l_iSelect > 0 && FD_ISSET(l_iFd, &readfds))
            {
                if (recv(l_iFd, l_buffer, sizeof(l_buffer), MSG_DONTWAIT) <= 0)
                    break;
            }
            else
            {
                break;
            }
            l_iTimeout++;
        }
        
        shutdown(l_iFd, SHUT_RDWR); // Fermeture complète
    }

    wolfSSL_free(p_ptSsl);
    X_LOG_TRACE("TLS session shutdown completed");
    
    return CERT_OK;
}

//////////////////////////////////
/// tlsEngineDestroy
//////////////////////////////////
int tlsEngineDestroy(xTlsEngine_t *p_ptEngine)
{
    if (!p_ptEngine)
    {
        return CERT_OK;
    }

    if (p_ptEngine->p_ctx)
    {
        wolfSSL_CTX_free(p_ptEngine->p_ctx);
    }

    free(p_ptEngine);
    return CERT_OK;
}
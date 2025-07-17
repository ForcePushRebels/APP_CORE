#ifndef RUST_TLS_ENGINE_H
#define RUST_TLS_ENGINE_H

#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#define CERT_OK 2092957696

#define CERT_ERROR_INVALID_PARAM 2092957697
#define CERT_ERROR_MEMORY_ALLOC 2092957698
#define CERT_ERROR_INVALID_CERT 2092957699
#define CERT_ERROR_CERT_EXPIRED 2092957702
#define CERT_ERROR_NOT_YET_VALID 2092957703
#define CERT_ERROR_WOLFSSL_ERROR 2092957706

#define WOLFSSL_SUCCESS 1
#define WOLFSSL_FAILURE 0
#define WOLFSSL_SHUTDOWN_NOT_DONE 2
#define WOLFSSL_ERROR_WANT_READ -2
#define WOLFSSL_ERROR_WANT_WRITE -3
#define WOLFSSL_FILETYPE_ASN1 2
#define WOLFSSL_FILETYPE_PEM 1

/**
 * TLS mode - enum xTlsMode_t available
 */
typedef enum XTlsMode {
  TLS_MODE_CLIENT = 0,
  TLS_MODE_SERVER = 1,
} XTlsMode;

/**
 * WolfSSL context - opaque structure
 */
typedef struct WOLFSSL_CTX {
  uint8_t _private[0];
} WOLFSSL_CTX;

/**
 * TLS Engine structure - must match xTlsEngine_t exactly
 */
typedef struct xTlsEngine_t {
  struct WOLFSSL_CTX *p_ctx;
  enum XTlsMode t_mode;
} xTlsEngine_t;

/**
 * WolfSSL session - opaque structure
 */
typedef struct WOLFSSL {
  uint8_t _private[0];
} WOLFSSL;

/**
 * Certificate structure 
 */
typedef struct CryptoParam {
  uint8_t *cert_data;
  uint32_t cert_size;
  uint8_t *der_data;
  uint32_t der_size;
  char subject[256];
  char issuer[256];
  bool is_ca;
  int64_t not_before;
  int64_t not_after;
  int status;
  int cert_type;
} xCertificate_t;

/**
 * WolfSSL method - opaque structure
 */
typedef struct WOLFSSL_METHOD {
  uint8_t _private[0];
} WOLFSSL_METHOD;


///////////////////////////////////////////
/// @brief Initializes the certificate management system
/// @param none 
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int xCertificateInit(void);


///////////////////////////////////////////
/// @brief Cleans up the certificate management system
/// @param none 
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int xCertificateCleanup(void);

/**
 * Creates a TLS engine
 */
///////////////////////////////////////////
/// @brief Creates a TLS engine
/// @param p_pptCryptoEngine : pointer to cryptoEngine 
/// @param p_eMode : tls Mode (client or server)
/// @param p_ptcCertFile : Contain the certificate for the key negociation 
/// @param p_ptcKeyFile : contain the private key for key negociation process
/// @param p_ptcCADir : path to the directory with root CA certificate
/// @param _p_bIsPEM : PEM fil eor DER File 
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int tlsEngineCreate(struct xTlsEngine_t **p_pptCryptoEngine,
                    enum XTlsMode p_eMode,
                    const char *p_ptcCertFile,
                    const char *p_ptcKeyFile,
                    const char *p_ptcCADir,
                    bool _p_bIsPEM);

///////////////////////////////////////////
/// @brief Attaches a socket to the TLS engine and performs the handshake
/// @param p_pptCryptoEngine : pointer to cryptoEngine 
/// @param p_iSocketFd : socket fd 
/// @param p_pptSslCtx : wolfssl tls structure
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int tlsEngineAttachSocket(struct xTlsEngine_t *p_ptEngine,
                          int p_iSocketFd,
                          struct WOLFSSL **p_pptSslCtx);

///////////////////////////////////////////
/// @brief Shuts down a TLS session
/// @param p_pptSslCtx : wolfssl tls structure
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int tlsEngineShutdown(struct WOLFSSL *p_ptSsl);


///////////////////////////////////////////
/// @brief Destroys a TLS engine
/// @param p_ptEngine : pointer to cryptoEngine 
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int tlsEngineDestroy(struct xTlsEngine_t *p_ptEngine);


///////////////////////////////////////////
/// @brief Loads a certificate from a file
/// @param p_pcFilePath : path to the file to load
/// @param _p_bIsPEM : PEM fil eor DER File 
/// @param p_pptCertificate : pointer to the certificate structure
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int xCertificateLoadFromFile(const char *p_pcFilePath,
                             bool _p_bIsPEM,
                             struct xCertificate_t **p_pptCertificate);


///////////////////////////////////////////
/// @brief Frees a certificate
/// @param p_ptCertificate : pointer to the certificate structure
/// @return error code or WOLFSSL_SUCCESS
///////////////////////////////////////////
int xCertificateFree(struct xCertificate_t *p_ptCertificate);


///////////////////////////////////////////
/// @brief Gets the description of an error
/// @param p_iError : error code id 
/// @return the string of the error code
///////////////////////////////////////////
const char *xCertificateGetErrorString(int p_iError);

/**
 * Initializes WolfSSL
 */
extern int wolfSSL_Init(void);

/**
 * Cleans up WolfSSL
 */
extern int wolfSSL_Cleanup(void);

/**
 * Gets the TLS 1.3 server method
 */
extern const struct WOLFSSL_METHOD *wolfTLSv1_3_server_method(void);

/**
 * Gets the TLS 1.3 client method
 */
extern const struct WOLFSSL_METHOD *wolfTLSv1_3_client_method(void);

/**
 * Creates a new WolfSSL context
 */
extern struct WOLFSSL_CTX *wolfSSL_CTX_new(const struct WOLFSSL_METHOD *method);

/**
 * Frees a WolfSSL context
 */
extern void wolfSSL_CTX_free(struct WOLFSSL_CTX *ctx);

/**
 * Configures the cipher suite list
 */
extern int wolfSSL_CTX_set_cipher_list(struct WOLFSSL_CTX *ctx, const char *list);

/**
 * Loads a certificate from a file
 */
extern int wolfSSL_CTX_use_certificate_file(struct WOLFSSL_CTX *ctx, const char *file, int format);

/**
 * Loads a certificate chain from a file (PEM)
 */
extern int wolfSSL_CTX_use_certificate_chain_file(struct WOLFSSL_CTX *ctx, const char *file);

/**
 * Loads a private key from a file
 */
extern int wolfSSL_CTX_use_PrivateKey_file(struct WOLFSSL_CTX *ctx, const char *file, int format);

/**
 * Loads CA certificates for verification
 */
extern int wolfSSL_CTX_load_verify_locations(struct WOLFSSL_CTX *ctx,
                                             const char *file,
                                             const char *path);

/**
 * Creates a new WolfSSL session
 */
extern struct WOLFSSL *wolfSSL_new(struct WOLFSSL_CTX *ctx);

/**
 * Frees a WolfSSL session
 */
extern void wolfSSL_free(struct WOLFSSL *ssl);

/**
 * Associates a socket with a WolfSSL session
 */
extern int wolfSSL_set_fd(struct WOLFSSL *ssl, int fd);

/**
 * Gets the file descriptor associated with a session
 */
extern int wolfSSL_get_fd(struct WOLFSSL *ssl);

/**
 * Performs the TLS handshake on the server side
 */
extern int wolfSSL_accept(struct WOLFSSL *ssl);

/**
 * Performs the TLS handshake on the client side
 */
extern int wolfSSL_connect(struct WOLFSSL *ssl);

/**
 * Shuts down a TLS session cleanly
 */
extern int wolfSSL_shutdown(struct WOLFSSL *ssl);

/**
 * Gets the detailed error code
 */
extern int wolfSSL_get_error(struct WOLFSSL *ssl, int ret);

/**
 * Reads data from a TLS session
 */
extern int wolfSSL_read(struct WOLFSSL *ssl, void *data, int sz);

/**
 * Writes data to a TLS session
 */
extern int wolfSSL_write(struct WOLFSSL *ssl, const void *data, int sz);

#endif  /* RUST_TLS_ENGINE_H */

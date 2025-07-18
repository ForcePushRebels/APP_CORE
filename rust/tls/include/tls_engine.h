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
 * Mode TLS - identique à l'enum C xTlsMode_t
 */
typedef enum XTlsMode {
  TLS_MODE_CLIENT = 0,
  TLS_MODE_SERVER = 1,
} XTlsMode;

/**
 * Contexte WolfSSL - structure opaque
 */
typedef struct WOLFSSL_CTX {
  uint8_t _private[0];
} WOLFSSL_CTX;

/**
 * Structure TLS Engine - doit correspondre exactement à xTlsEngine_t
 */
typedef struct xTlsEngine_t {
  struct WOLFSSL_CTX *p_ctx;
  enum XTlsMode t_mode;
} xTlsEngine_t;

/**
 * Session WolfSSL - structure opaque
 */
typedef struct WOLFSSL {
  uint8_t _private[0];
} WOLFSSL;

/**
 * Structure Certificate - version simplifiée pour l'interface C
 */
typedef struct xCertificate_t {
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
 * Méthode WolfSSL - structure opaque
 */
typedef struct WOLFSSL_METHOD {
  uint8_t _private[0];
} WOLFSSL_METHOD;

/**
 * X509 Certificate - opaque structure
 */
typedef struct WOLFSSL_X509 {
  uint8_t _private[0];
} WOLFSSL_X509;

/**
 * X509 Name - opaque structure
 */
typedef struct WOLFSSL_X509_NAME {
  uint8_t _private[0];
} WOLFSSL_X509_NAME;

typedef struct ASN1_TIME {
  uint8_t _private[0];
} ASN1_TIME;

/**
 * Initialise le système de gestion des certificats
 * NOM IDENTIQUE: xCertificateInit
 */
int xCertificateInit(void);

/**
 * Nettoie le système de gestion des certificats
 * NOM IDENTIQUE: xCertificateCleanup
 */
int xCertificateCleanup(void);

/**
 * Crée un moteur TLS
 * NOM IDENTIQUE: tlsEngineCreate
 */
int tlsEngineCreate(struct xTlsEngine_t **p_pptCryptoEngine,
                    enum XTlsMode p_eMode,
                    const char *p_ptcCertFile,
                    const char *p_ptcKeyFile,
                    const char *p_ptcCADir,
                    bool _p_bIsPEM);

/**
 * Attache un socket au moteur TLS et effectue le handshake
 * NOM IDENTIQUE: tlsEngineAttachSocket
 */
int tlsEngineAttachSocket(struct xTlsEngine_t *p_ptEngine,
                          int p_iSocketFd,
                          struct WOLFSSL **p_pptSslCtx);

/**
 * Ferme une session TLS
 * NOM IDENTIQUE: tlsEngineShutdown
 */
int tlsEngineShutdown(struct WOLFSSL *p_ptSsl);

/**
 * Détruit un moteur TLS
 * NOM IDENTIQUE: tlsEngineDestroy
 */
int tlsEngineDestroy(struct xTlsEngine_t *p_ptEngine);

/**
 * Charge un certificat depuis un fichier
 * NOM IDENTIQUE: xCertificateLoadFromFile
 */
int xCertificateLoadFromFile(const char *p_pcFilePath,
                             bool _p_bIsPEM,
                             struct xCertificate_t **p_pptCertificate);

/**
 * Libère un certificat
 * NOM IDENTIQUE: xCertificateFree
 */
int xCertificateFree(struct xCertificate_t *p_ptCertificate);

/**
 * Obtient la description d'une erreur
 * NOM IDENTIQUE: xCertificateGetErrorString
 */
const char *xCertificateGetErrorString(int p_iError);

/**
 * Initialise WolfSSL
 */
extern int wolfSSL_Init(void);

/**
 * Nettoie WolfSSL
 */
extern int wolfSSL_Cleanup(void);

/**
 * Obtient la méthode TLS 1.3 serveur
 */
extern const struct WOLFSSL_METHOD *wolfTLSv1_3_server_method(void);

/**
 * Obtient la méthode TLS 1.3 client
 */
extern const struct WOLFSSL_METHOD *wolfTLSv1_3_client_method(void);

/**
 * Crée un nouveau contexte WolfSSL
 */
extern struct WOLFSSL_CTX *wolfSSL_CTX_new(const struct WOLFSSL_METHOD *method);

/**
 * Libère un contexte WolfSSL
 */
extern void wolfSSL_CTX_free(struct WOLFSSL_CTX *ctx);

/**
 * Configure la liste des cipher suites
 */
extern int wolfSSL_CTX_set_cipher_list(struct WOLFSSL_CTX *ctx, const char *list);

/**
 * Charge un certificat depuis un fichier
 */
extern int wolfSSL_CTX_use_certificate_file(struct WOLFSSL_CTX *ctx, const char *file, int format);

/**
 * Charge une chaîne de certificats depuis un fichier (PEM)
 */
extern int wolfSSL_CTX_use_certificate_chain_file(struct WOLFSSL_CTX *ctx, const char *file);

/**
 * Charge une clé privée depuis un fichier
 */
extern int wolfSSL_CTX_use_PrivateKey_file(struct WOLFSSL_CTX *ctx, const char *file, int format);

/**
 * Charge les certificats CA pour la vérification
 */
extern int wolfSSL_CTX_load_verify_locations(struct WOLFSSL_CTX *ctx,
                                             const char *file,
                                             const char *path);

/**
 * Crée une nouvelle session WolfSSL
 */
extern struct WOLFSSL *wolfSSL_new(struct WOLFSSL_CTX *ctx);

/**
 * Libère une session WolfSSL
 */
extern void wolfSSL_free(struct WOLFSSL *ssl);

/**
 * Associe un socket à une session WolfSSL
 */
extern int wolfSSL_set_fd(struct WOLFSSL *ssl, int fd);

/**
 * Obtient le file descriptor associé à une session
 */
extern int wolfSSL_get_fd(struct WOLFSSL *ssl);

/**
 * Effectue le handshake TLS côté serveur
 */
extern int wolfSSL_accept(struct WOLFSSL *ssl);

/**
 * Effectue le handshake TLS côté client
 */
extern int wolfSSL_connect(struct WOLFSSL *ssl);

/**
 * Ferme proprement une session TLS
 */
extern int wolfSSL_shutdown(struct WOLFSSL *ssl);

/**
 * Obtient le code d'erreur détaillé
 */
extern int wolfSSL_get_error(struct WOLFSSL *ssl, int ret);

/**
 * Lit des données depuis une session TLS
 */
extern int wolfSSL_read(struct WOLFSSL *ssl, void *data, int sz);

/**
 * Écrit des données vers une session TLS
 */
extern int wolfSSL_write(struct WOLFSSL *ssl, const void *data, int sz);

extern struct WOLFSSL_X509 *wolfSSL_X509_load_certificate_file(const char *file, int format);

extern void wolfSSL_X509_free(struct WOLFSSL_X509 *x509);

extern struct WOLFSSL_X509_NAME *wolfSSL_X509_get_subject_name(struct WOLFSSL_X509 *x509);

extern struct WOLFSSL_X509_NAME *wolfSSL_X509_get_issuer_name(struct WOLFSSL_X509 *x509);

extern char *wolfSSL_X509_NAME_oneline(struct WOLFSSL_X509_NAME *name, char *buf, int size);

extern int wolfSSL_X509_get_isCA(struct WOLFSSL_X509 *x509);

extern struct ASN1_TIME *wolfSSL_X509_get_notBefore(struct WOLFSSL_X509 *x509);

extern struct ASN1_TIME *wolfSSL_X509_get_notAfter(struct WOLFSSL_X509 *x509);

extern struct WOLFSSL_X509 *wolfSSL_X509_load_certificate_buffer(const uint8_t *buf,
                                                                 int sz,
                                                                 int format);

extern int wolfSSL_ASN1_TIME_to_string(struct ASN1_TIME *time, char *buf, int len);

extern int64_t wolfSSL_ASN1_TIME_get_posix(struct ASN1_TIME *time);

#endif  /* RUST_TLS_ENGINE_H */

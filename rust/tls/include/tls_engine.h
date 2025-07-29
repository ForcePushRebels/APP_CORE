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

#define MLKEM512_KEY_SIZE 1632

#define MLKEM512_CIPHER_SIZE 768

#define MLKEM512_SHARED_SECRET_SIZE 32

#define MLKEM768_KEY_SIZE 2400

#define MLKEM768_CIPHER_SIZE 1088

#define MLKEM768_SHARED_SECRET_SIZE 32

#define MLKEM1024_KEY_SIZE 3168

#define MLKEM1024_CIPHER_SIZE 1568

#define MLKEM1024_SHARED_SECRET_SIZE 32

#define MLDSA44_KEY_SIZE 2560

#define MLDSA44_SIG_SIZE 2420

#define MLDSA44_PRIV_KEY_SIZE 2560

#define MLDSA44_PUB_KEY_SIZE 1312

#define MLDSA65_KEY_SIZE 4032

#define MLDSA65_SIG_SIZE 3309

#define MLDSA65_PRIV_KEY_SIZE 4032

#define MLDSA65_PUB_KEY_SIZE 1952

#define MLDSA87_KEY_SIZE 4896

#define MLDSA87_SIG_SIZE 4627

#define MLDSA87_PRIV_KEY_SIZE 4896

#define MLDSA87_PUB_KEY_SIZE 2592

#define WOLFSSL_P256_MLKEM512 12032

#define WOLFSSL_P384_MLKEM768 12033

#define WOLFSSL_P521_MLKEM1024 12034

#define WOLFSSL_MLDSA44 3585

#define WOLFSSL_MLDSA65 3586

#define WOLFSSL_MLDSA87 3587

#define WOLFSSL_ECDSA_P256_MLDSA44 3841

#define WOLFSSL_ECDSA_P384_MLDSA65 3842

#define WOLFSSL_ECDSA_P521_MLDSA87 3843

/**
 * Méthode WolfSSL - structure opaque
 */
typedef struct WOLFSSL_METHOD {
  uint8_t _private[0];
} WOLFSSL_METHOD;

/**
 * Contexte WolfSSL - structure opaque
 */
typedef struct WOLFSSL_CTX {
  uint8_t _private[0];
} WOLFSSL_CTX;

/**
 * Session WolfSSL - structure opaque
 */
typedef struct WOLFSSL {
  uint8_t _private[0];
} WOLFSSL;

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

/**
 * ASN1 Time - opaque structure
 */
typedef struct ASN1_TIME {
  uint8_t _private[0];
} ASN1_TIME;

/**
 * ML-KEM key structure - opaque
 */
typedef struct MLKEM_KEY {
  uint8_t _private[0];
} MLKEM_KEY;

/**
 * ML-DSA key structure - opaque
 */
typedef struct MLDSA_KEY {
  uint8_t _private[0];
} MLDSA_KEY;

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

/**
 * Initialise une clé ML-KEM
 */
extern int wc_MlKemKey_Init(struct MLKEM_KEY *key, int level);

/**
 * Libère une clé ML-KEM
 */
extern void wc_MlKemKey_Free(struct MLKEM_KEY *key);

/**
 * Génère une paire de clés ML-KEM
 */
extern int wc_MlKemKey_MakeKey(struct MLKEM_KEY *key, void *rng);

/**
 * Encapsule un secret avec ML-KEM
 */
extern int wc_MlKemKey_Encap(struct MLKEM_KEY *key,
                             uint8_t *cipher_text,
                             int *cipher_text_len,
                             uint8_t *shared_secret,
                             int *shared_secret_len,
                             void *rng);

/**
 * Décapsule un secret avec ML-KEM
 */
extern int wc_MlKemKey_Decap(struct MLKEM_KEY *key,
                             uint8_t *shared_secret,
                             int *shared_secret_len,
                             const uint8_t *cipher_text,
                             int cipher_text_len);

/**
 * Exporte la clé publique ML-KEM
 */
extern int wc_MlKemKey_ExportPub(struct MLKEM_KEY *key, uint8_t *pub_key, int *pub_key_len);

/**
 * Importe une clé publique ML-KEM
 */
extern int wc_MlKemKey_ImportPub(struct MLKEM_KEY *key, const uint8_t *pub_key, int pub_key_len);

/**
 * Exporte la clé privée ML-KEM
 */
extern int wc_MlKemKey_ExportPriv(struct MLKEM_KEY *key, uint8_t *priv_key, int *priv_key_len);

/**
 * Importe une clé privée ML-KEM
 */
extern int wc_MlKemKey_ImportPriv(struct MLKEM_KEY *key, const uint8_t *priv_key, int priv_key_len);

/**
 * Initialise une clé ML-DSA
 */
extern int wc_MlDsaKey_Init(struct MLDSA_KEY *key, int level);

/**
 * Libère une clé ML-DSA
 */
extern void wc_MlDsaKey_Free(struct MLDSA_KEY *key);

/**
 * Génère une paire de clés ML-DSA
 */
extern int wc_MlDsaKey_MakeKey(struct MLDSA_KEY *key, void *rng);

/**
 * Signe des données avec ML-DSA
 */
extern int wc_MlDsaKey_Sign(struct MLDSA_KEY *key,
                            uint8_t *sig,
                            int *sig_len,
                            const uint8_t *msg,
                            int msg_len,
                            void *rng);

/**
 * Vérifie une signature ML-DSA
 */
extern int wc_MlDsaKey_Verify(struct MLDSA_KEY *key,
                              const uint8_t *sig,
                              int sig_len,
                              const uint8_t *msg,
                              int msg_len);

/**
 * Exporte la clé publique ML-DSA
 */
extern int wc_MlDsaKey_ExportPub(struct MLDSA_KEY *key, uint8_t *pub_key, int *pub_key_len);

/**
 * Importe une clé publique ML-DSA
 */
extern int wc_MlDsaKey_ImportPub(struct MLDSA_KEY *key, const uint8_t *pub_key, int pub_key_len);

/**
 * Exporte la clé privée ML-DSA
 */
extern int wc_MlDsaKey_ExportPriv(struct MLDSA_KEY *key, uint8_t *priv_key, int *priv_key_len);

/**
 * Importe une clé privée ML-DSA
 */
extern int wc_MlDsaKey_ImportPriv(struct MLDSA_KEY *key, const uint8_t *priv_key, int priv_key_len);

/**
 * Configure les groupes hybrides supportés pour TLS 1.3
 */
extern int wolfSSL_CTX_set_groups_list(struct WOLFSSL_CTX *ctx, const char *list);

/**
 * Configure les algorithmes de signature hybrides supportés
 */
extern int wolfSSL_CTX_set_sigalgs_list(struct WOLFSSL_CTX *ctx, const char *list);

/**
 * Active les extensions post-quantiques X.509
 */
extern int wolfSSL_CTX_enable_pq_x509_extensions(struct WOLFSSL_CTX *ctx);

/**
 * Charge un certificat hybride depuis un fichier
 */
extern int wolfSSL_CTX_use_certificate_chain_file_pq(struct WOLFSSL_CTX *ctx,
                                                     const char *file,
                                                     int format);

/**
 * Vérifie si une session utilise des algorithmes post-quantiques
 */
extern int wolfSSL_is_pq_handshake(struct WOLFSSL *ssl);

/**
 * Obtient le groupe négocié pour la session
 */
extern int wolfSSL_get_negotiated_group(struct WOLFSSL *ssl);

/**
 * Obtient l'algorithme de signature négocié
 */
extern int wolfSSL_get_negotiated_sigalg(struct WOLFSSL *ssl);

/**
 * Vérifie si un certificat contient des clés publiques alternatives (hybrides)
 */
extern int wolfSSL_X509_has_alt_public_key(struct WOLFSSL_X509 *x509);

/**
 * Obtient la clé publique alternative d'un certificat
 */
extern int wolfSSL_X509_get_alt_public_key(struct WOLFSSL_X509 *x509,
                                           uint8_t *key_buf,
                                           int *key_len,
                                           int *key_type);

/**
 * Vérifie si un certificat contient des signatures alternatives
 */
extern int wolfSSL_X509_has_alt_signature(struct WOLFSSL_X509 *x509);

/**
 * Obtient la signature alternative d'un certificat
 */
extern int wolfSSL_X509_get_alt_signature(struct WOLFSSL_X509 *x509,
                                          uint8_t *sig_buf,
                                          int *sig_len,
                                          int *sig_alg);

extern struct WOLFSSL_X509 *wolfSSL_X509_load_certificate_buffer(const uint8_t *buf,
                                                                 int sz,
                                                                 int format);

extern int wolfSSL_ASN1_TIME_to_string(struct ASN1_TIME *time, char *buf, int len);

extern int64_t wolfSSL_ASN1_TIME_get_posix(struct ASN1_TIME *time);

#endif  /* RUST_TLS_ENGINE_H */

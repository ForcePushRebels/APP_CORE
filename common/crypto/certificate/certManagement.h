////////////////////////////////////////////////////////////
//  xCertificate implementation file
//  defines the Certificate types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/06/2025
////////////////////////////////////////////////////////////

#ifndef X_CERTIFICATE_MANAGEMENT_H
#define X_CERTIFICATE_MANAGEMENT_H

#include <wolfssl/options.h>
#include <wolfssl/ssl.h>
#include <wolfssl/wolfcrypt/settings.h>
#include <wolfssl/wolfcrypt/random.h>
#include <wolfssl/wolfcrypt/hash.h>
#include <wolfssl/wolfcrypt/sha.h>
#include <wolfssl/wolfcrypt/ecc.h>
#include <wolfssl/wolfcrypt/rsa.h>
#include <wolfssl/wolfcrypt/asn.h>
#include <wolfssl/wolfcrypt/asn_public.h>
#include <wolfssl/wolfcrypt/coding.h>
#include <wolfssl/wolfcrypt/error-crypt.h>
#include <stdbool.h>
#include <stdint.h>
#include <time.h>

// CERT_TYPE definition for X.509 certificates
#ifndef CERT_TYPE
#define CERT_TYPE 0
#endif

// Certificate chain validation constants
#define CERT_MAX_CHAIN_DEPTH 5
#define CERT_MAX_CERT_SIZE ((uint32_t)(4*1024))
#define CERT_MAX_KEY_SIZE ((uint32_t)(2*1024))
#define CERT_MAX_SUBJECT_SIZE 256
#define CERT_MAX_ISSUER_SIZE 256

// Certificate validation error codes
#define CERT_OK                     0x7CC00000
#define CERT_ERROR_INVALID_PARAM    0x7CC00001
#define CERT_ERROR_MEMORY_ALLOC     0x7CC00002
#define CERT_ERROR_INVALID_CERT     0x7CC00003
#define CERT_ERROR_CERT_EXPIRED     0x7CC00006
#define CERT_ERROR_NOT_YET_VALID    0x7CC00007
#define CERT_ERROR_WOLFSSL_ERROR    0x7CC0000A

// Certificate types
typedef enum {
    CERT_TYPE_ROOT_CA = 0,
    CERT_TYPE_INTERMEDIATE_CA,
    CERT_TYPE_END_ENTITY
} xCertificateType_t;

// Certificate status
typedef enum {
    CERT_STATUS_UNKNOWN = 0,
    CERT_STATUS_VALID,
    CERT_STATUS_EXPIRED,
    CERT_STATUS_NOT_YET_VALID,
    CERT_STATUS_REVOKED,
    CERT_STATUS_INVALID_SIGNATURE
} xCertificateStatus_t;

// Certificate structure
typedef struct xCertificate {
    DecodedCert t_tDecodedCert;                 // WolfSSL decoded certificate object
    xCertificateType_t t_eType;                 // Certificate type
    xCertificateStatus_t t_eStatus;             // Validation status
    char t_acSubject[CERT_MAX_SUBJECT_SIZE];    // Subject name
    char t_acIssuer[CERT_MAX_ISSUER_SIZE];      // Issuer name
    uint8_t *p_pucCertData;                     // Raw certificate data (original format)
    uint32_t t_ulCertSize;                      // Certificate data size
    uint8_t *p_pucDerData;                      // DER format data (for WolfSSL)
    uint32_t t_ulDerSize;                       // DER data size
    bool t_bIsCA;                               // Is Certificate Authority
    time_t t_tNotBefore;                        // Valid from
    time_t t_tNotAfter;                         // Valid until
} xCertificate_t;

// Note: Certificate chain and store structures removed - use wolfSSL built-in store via wolfSSL_CTX_load_verify_locations()

////////////////////////////////////////////////////////////
// Certificate Management Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// @brief Initialize the certificate management system
/// @return int Error code
//////////////////////////////////
int certManagementInit(void);

//////////////////////////////////
/// @brief Cleanup certificate management resources
/// @return int Error code
//////////////////////////////////
int certManagementCleanup(void);

//////////////////////////////////
/// @brief Load certificate from DER/PEM data
/// @param p_pucCertData Certificate data buffer
/// @param p_ulDataSize Size of certificate data
/// @param p_bIsPEM True if data is PEM format, false for DER
/// @param p_ppttCertificate Output: loaded certificate
/// @return int Error code
//////////////////////////////////
int loadCertificate(const uint8_t *p_pucCertData, 
                     uint32_t p_ulDataSize,
                     bool p_bIsPEM,
                     xCertificate_t **p_ppttCertificate);

//////////////////////////////////
/// @brief Load certificate from file
/// @param p_ptcFilePath Path to certificate file
/// @param p_bIsPEM True if file is PEM format, false for DER
/// @param p_ppttCertificate Output: loaded certificate
/// @return int Error code
//////////////////////////////////
int loadCertificateFromFile(const char *p_ptcFilePath,
                             bool p_bIsPEM,
                             xCertificate_t **p_ppttCertificate);

//////////////////////////////////
/// @brief Load the root CA certificate (highest in hierarchy)
/// @param p_ptcCADirectoryPath Path to directory containing CA certificates
/// @param p_bIsPEM True if certificates are in PEM format, false for DER
/// @param p_ppttRootCA Output: loaded root CA certificate
/// @return int Error code
//////////////////////////////////
int loadCertificateRootCA(const char *p_ptcCADirectoryPath,
                           bool p_bIsPEM,
                           xCertificate_t **p_ppttRootCA);

//////////////////////////////////
/// @brief Free certificate resources
/// @param p_ptCertificate Certificate to free
/// @return int Error code
//////////////////////////////////
int freeCertRessources(xCertificate_t *p_ptCertificate);

////////////////////////////////////////////////////////////
// Certificate Validation Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// @brief Check certificate validity period
/// @param p_ptCertificate Certificate to check
/// @return int Error code
//////////////////////////////////
int processCertValidity(xCertificate_t *p_ptCertificate);

////////////////////////////////////////////////////////////
// Certificate Information Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// @brief Get certificate subject name
/// @param p_ptCertificate Certificate
/// @param p_ptcBuffer Output buffer for subject name
/// @param p_ulBufferSize Buffer size
/// @return int Error code
//////////////////////////////////
int getCertSubject(xCertificate_t *p_ptCertificate,
                           char *p_ptcBuffer,
                           uint32_t p_ulBufferSize);

//////////////////////////////////
/// @brief Get certificate issuer name
/// @param p_ptCertificate Certificate
/// @param p_ptcBuffer Output buffer for issuer name
/// @param p_ulBufferSize Buffer size
/// @return int Error code
//////////////////////////////////
int getCertIssuer(xCertificate_t *p_ptCertificate,
                          char *p_ptcBuffer,
                          uint32_t p_ulBufferSize);

//////////////////////////////////
/// @brief Check if certificate is a CA
/// @param p_ptCertificate Certificate to check
/// @param p_pbIsCA Output: true if certificate is CA
/// @return int Error code
//////////////////////////////////
int isCertCA(xCertificate_t *p_ptCertificate,
                     bool *p_pbIsCA);

//////////////////////////////////
/// @brief Get certificate error description
/// @param p_iError Error code
/// @return const char* Error description
//////////////////////////////////
const char *getCertErrorString(int p_iError);

//////////////////////////////////
/// @brief Load root CA certificate into TLS context
/// @param p_ptSSLContext WolfSSL context to load CA into
/// @param p_ptcCADirectoryPath Path to directory containing CA certificates
/// @param p_bIsPEM True if certificates are in PEM format, false for DER
/// @return int Error code
//////////////////////////////////
int loadRootCAIntoCtx(WOLFSSL_CTX *p_ptSSLContext,
                      const char *p_ptcCADirectoryPath,
                      bool p_bIsPEM);

//////////////////////////////////
/// @brief Extract real validity dates from certificate
/// @param p_ptDecodedCert WolfSSL decoded certificate
/// @param p_ptNotBefore Output: certificate valid from date
/// @param p_ptNotAfter Output: certificate valid until date
/// @return int Error code
//////////////////////////////////
int extractCertValidityDate(DecodedCert *p_ptDecodedCert,
                                     time_t *p_ptNotBefore,
                                     time_t *p_ptNotAfter);

#endif // X_CERTIFICATE_MANAGEMENT_H

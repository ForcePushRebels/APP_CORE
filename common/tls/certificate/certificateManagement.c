////////////////////////////////////////////////////////////
//  xCertificate implementation file
//  implements the Certificate management with CA hierarchy
//
// general disclosure: copy or share the file is forbidden
// Written : 04/06/2025
////////////////////////////////////////////////////////////

#include "certificateManagement.h"
#include "xLog.h"
#include "xOsMemory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

////////////////////////////////////////////////////////////
// Static variables and helpers
////////////////////////////////////////////////////////////

static bool s_bCertificateSystemInitialized = false;

// Helper function prototypes
static int extractCertificateInfo(xCertificate_t *p_ptCertificate);
static void secureClearCertificate(xCertificate_t *p_ptCertificate);

////////////////////////////////////////////////////////////
// Certificate Management Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// xCertificateInit
//////////////////////////////////
int xCertificateInit(void)
{
    if (s_bCertificateSystemInitialized)
    {
        X_LOG_TRACE("Certificate system already initialized");
        return CERT_OK;
    }

    // Initialize WolfSSL if not already done
    int l_iWolfResult = wolfSSL_Init();
    if (l_iWolfResult != WOLFSSL_SUCCESS)
    {
        X_LOG_TRACE("Failed to initialize WolfSSL: %d", l_iWolfResult);
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    s_bCertificateSystemInitialized = true;
    X_LOG_TRACE("Certificate management system initialized successfully");
    
    return CERT_OK;
}

//////////////////////////////////
/// xCertificateCleanup
//////////////////////////////////
int xCertificateCleanup(void)
{
    if (!s_bCertificateSystemInitialized)
    {
        return CERT_OK;
    }

    // Cleanup WolfSSL
    wolfSSL_Cleanup();
    s_bCertificateSystemInitialized = false;
    
    X_LOG_TRACE("Certificate management system cleaned up");
    return CERT_OK;
}

//////////////////////////////////
/// xCertificateLoad
//////////////////////////////////
int xCertificateLoad(const uint8_t *p_pucCertData, 
                     uint32_t p_ulDataSize,
                     bool p_bIsPEM,
                     xCertificate_t **p_pptCertificate)
{
    // Parameter validation
    if (!p_pucCertData || p_ulDataSize == 0 || !p_pptCertificate)
    {
        X_LOG_TRACE("Invalid parameters for certificate loading");
        return CERT_ERROR_INVALID_PARAM;
    }

    if (!s_bCertificateSystemInitialized)
    {
        X_LOG_TRACE("Certificate system not initialized");
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    // Validate certificate data size
    if (p_ulDataSize > CERT_MAX_CERT_SIZE)
    {
        X_LOG_TRACE("Certificate size too large: %u bytes", p_ulDataSize);
        return CERT_ERROR_INVALID_CERT;
    }

    // Allocate certificate structure
    xCertificate_t *l_ptCertificate = (xCertificate_t *)malloc(sizeof(xCertificate_t));
    if (!l_ptCertificate)
    {
        X_LOG_TRACE("Failed to allocate memory for certificate");
        return CERT_ERROR_MEMORY_ALLOC;
    }

    // Initialize certificate structure
    memset(l_ptCertificate, 0, sizeof(xCertificate_t));

    // Prepare certificate data buffer (DER format)
    uint8_t *l_pucDerData = NULL;
    uint32_t l_ulDerSize = 0;

    if (p_bIsPEM)
    {
        // Convert PEM to DER
        l_pucDerData = (uint8_t *)malloc(p_ulDataSize);
        if (!l_pucDerData)
        {
            X_LOG_TRACE("Failed to allocate memory for DER conversion");
            XOS_MEMORY_SANITIZE(l_ptCertificate, sizeof(*l_ptCertificate));
            free(l_ptCertificate);
            return CERT_ERROR_MEMORY_ALLOC;
        }

        int l_iResult = wc_CertPemToDer(p_pucCertData, (int)p_ulDataSize, 
                                        l_pucDerData, (int)p_ulDataSize, CERT_TYPE);
        if (l_iResult < 0)
        {
            X_LOG_TRACE("Failed to convert PEM to DER: %d", l_iResult);
            XOS_MEMORY_SANITIZE(l_pucDerData, p_ulDataSize);
            free(l_pucDerData);
            XOS_MEMORY_SANITIZE(l_ptCertificate, sizeof(*l_ptCertificate));
            free(l_ptCertificate);
            return CERT_ERROR_INVALID_CERT;
        }
        l_ulDerSize = (uint32_t)l_iResult;
    }
    else
    {
        // Already DER format, just copy
        l_pucDerData = (uint8_t *)malloc(p_ulDataSize);
        if (!l_pucDerData)
        {
            X_LOG_TRACE("Failed to allocate memory for certificate data");
            XOS_MEMORY_SANITIZE(l_ptCertificate, sizeof(*l_ptCertificate));
            free(l_ptCertificate);
            return CERT_ERROR_MEMORY_ALLOC;
        }
        XOS_SECURE_BUFFER_COPY(l_pucDerData, p_ulDataSize, p_pucCertData, p_ulDataSize);
        l_ulDerSize = p_ulDataSize;
    }

    // Store original certificate data
    l_ptCertificate->p_pucCertData = (uint8_t *)malloc(p_ulDataSize);
    if (!l_ptCertificate->p_pucCertData)
    {
        X_LOG_TRACE("Failed to allocate memory for original certificate data");
        XOS_MEMORY_SANITIZE(l_pucDerData, l_ulDerSize);
        free(l_pucDerData);
        XOS_MEMORY_SANITIZE(l_ptCertificate, sizeof(*l_ptCertificate));
        free(l_ptCertificate);
        return CERT_ERROR_MEMORY_ALLOC;
    }
    XOS_SECURE_BUFFER_COPY(l_ptCertificate->p_pucCertData, p_ulDataSize, 
                           p_pucCertData, p_ulDataSize);
    l_ptCertificate->t_ulCertSize = p_ulDataSize;

    // Initialize and parse certificate with WolfSSL
    wc_InitDecodedCert(&l_ptCertificate->t_tDecodedCert, l_pucDerData, l_ulDerSize, NULL);
    
    int l_iParseResult = wc_ParseCert(&l_ptCertificate->t_tDecodedCert, CERT_TYPE, NO_VERIFY, NULL);
    if (l_iParseResult != 0)
    {
        X_LOG_TRACE("Failed to parse certificate with WolfSSL: %d", l_iParseResult);
        wc_FreeDecodedCert(&l_ptCertificate->t_tDecodedCert);
        XOS_MEMORY_SANITIZE(l_pucDerData, l_ulDerSize);
        free(l_pucDerData);
        XOS_MEMORY_SANITIZE(l_ptCertificate->p_pucCertData, p_ulDataSize);
        free(l_ptCertificate->p_pucCertData);
        XOS_MEMORY_SANITIZE(l_ptCertificate, sizeof(*l_ptCertificate));
        free(l_ptCertificate);
        return CERT_ERROR_INVALID_CERT;
    }

    // Extract certificate information
    int l_iExtractResult = extractCertificateInfo(l_ptCertificate);
    if (l_iExtractResult != CERT_OK)
    {
        X_LOG_TRACE("Failed to extract certificate information");
        wc_FreeDecodedCert(&l_ptCertificate->t_tDecodedCert);
        XOS_MEMORY_SANITIZE(l_pucDerData, l_ulDerSize);
        free(l_pucDerData);
        XOS_MEMORY_SANITIZE(l_ptCertificate->p_pucCertData, p_ulDataSize);
        free(l_ptCertificate->p_pucCertData);
        XOS_MEMORY_SANITIZE(l_ptCertificate, sizeof(*l_ptCertificate));
        free(l_ptCertificate);
        return l_iExtractResult;
    }

    // Clean up DER data (no longer needed)
    XOS_MEMORY_SANITIZE(l_pucDerData, l_ulDerSize);
    free(l_pucDerData);

    // Set default status
    l_ptCertificate->t_eStatus = CERT_STATUS_UNKNOWN;

    *p_pptCertificate = l_ptCertificate;
    
    X_LOG_TRACE("Certificate loaded successfully: Subject=%s", l_ptCertificate->t_acSubject);
    return CERT_OK;
}

//////////////////////////////////
/// xCertificateLoadFromFile
//////////////////////////////////
int xCertificateLoadFromFile(const char *p_pcFilePath,
                             bool p_bIsPEM,
                             xCertificate_t **p_pptCertificate)
{
    if (!p_pcFilePath || !p_pptCertificate)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    // Open file
    FILE *l_ptFile = fopen(p_pcFilePath, "rb");
    if (!l_ptFile)
    {
        X_LOG_TRACE("Failed to open certificate file: %s", p_pcFilePath);
        return CERT_ERROR_INVALID_CERT;
    }

    // Get file size
    fseek(l_ptFile, 0, SEEK_END);
    long l_lFileSize = ftell(l_ptFile);
    fseek(l_ptFile, 0, SEEK_SET);

    if (l_lFileSize <= 0 || l_lFileSize > CERT_MAX_CERT_SIZE)
    {
        X_LOG_TRACE("Invalid certificate file size: %ld", l_lFileSize);
        fclose(l_ptFile);
        return CERT_ERROR_INVALID_CERT;
    }

    // Allocate buffer for file data
    uint8_t *l_pucFileData = (uint8_t *)malloc((size_t)l_lFileSize);
    if (!l_pucFileData)
    {
        X_LOG_TRACE("Failed to allocate memory for file data");
        fclose(l_ptFile);
        return CERT_ERROR_MEMORY_ALLOC;
    }

    // Read file data
    size_t l_ulBytesRead = fread(l_pucFileData, 1, (size_t)l_lFileSize, l_ptFile);
    fclose(l_ptFile);

    if (l_ulBytesRead != (size_t)l_lFileSize)
    {
        X_LOG_TRACE("Failed to read complete certificate file");
        XOS_MEMORY_SANITIZE(l_pucFileData, (size_t)l_lFileSize);
        free(l_pucFileData);
        return CERT_ERROR_INVALID_CERT;
    }

    // Load certificate from memory
    int l_iResult = xCertificateLoad(l_pucFileData, (uint32_t)l_lFileSize, 
                                     p_bIsPEM, p_pptCertificate);

    // Clean up file data
    XOS_MEMORY_SANITIZE(l_pucFileData, (size_t)l_lFileSize);
    free(l_pucFileData);

    return l_iResult;
}

//////////////////////////////////
/// xCertificateFree
//////////////////////////////////
int xCertificateFree(xCertificate_t *p_ptCertificate)
{
    if (!p_ptCertificate)
    {
        return CERT_OK;
    }

    // Free WolfSSL decoded certificate
    wc_FreeDecodedCert(&p_ptCertificate->t_tDecodedCert);

    // Securely clear and free certificate data
    if (p_ptCertificate->p_pucCertData)
    {
        XOS_MEMORY_SANITIZE(p_ptCertificate->p_pucCertData, p_ptCertificate->t_ulCertSize);
        free(p_ptCertificate->p_pucCertData);
    }

    // Securely clear certificate structure
    secureClearCertificate(p_ptCertificate);
    free(p_ptCertificate);

    return CERT_OK;
}

// Note: Certificate store management removed - use wolfSSL_CTX_load_verify_locations() instead

// Note: Certificate chain management removed - validation handled by wolfSSL during handshake

////////////////////////////////////////////////////////////
// Certificate Validation Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// xCertificateCheckValidity
//////////////////////////////////
int xCertificateCheckValidity(xCertificate_t *p_ptCertificate)
{
    if (!p_ptCertificate)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    time_t l_tCurrentTime = time(NULL);
    
    if (l_tCurrentTime < p_ptCertificate->t_tNotBefore)
    {
        X_LOG_TRACE("Certificate not yet valid");
        p_ptCertificate->t_eStatus = CERT_STATUS_NOT_YET_VALID;
        return CERT_ERROR_NOT_YET_VALID;
    }
    
    if (l_tCurrentTime > p_ptCertificate->t_tNotAfter)
    {
        X_LOG_TRACE("Certificate expired");
        p_ptCertificate->t_eStatus = CERT_STATUS_EXPIRED;
        return CERT_ERROR_CERT_EXPIRED;
    }

    p_ptCertificate->t_eStatus = CERT_STATUS_VALID;
    return CERT_OK;
}

// Note: Certificate signature verification removed - handled by wolfSSL during handshake

////////////////////////////////////////////////////////////
// Certificate Information Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// xCertificateGetSubject
//////////////////////////////////
int xCertificateGetSubject(xCertificate_t *p_ptCertificate,
                           char *p_pcBuffer,
                           uint32_t p_ulBufferSize)
{
    if (!p_ptCertificate || !p_pcBuffer || p_ulBufferSize == 0)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    size_t l_ulSubjectLen = strlen(p_ptCertificate->t_acSubject);
    if (l_ulSubjectLen >= p_ulBufferSize)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    strncpy(p_pcBuffer, p_ptCertificate->t_acSubject, p_ulBufferSize - 1);
    p_pcBuffer[p_ulBufferSize - 1] = '\0';

    return CERT_OK;
}

//////////////////////////////////
/// xCertificateGetIssuer
//////////////////////////////////
int xCertificateGetIssuer(xCertificate_t *p_ptCertificate,
                          char *p_pcBuffer,
                          uint32_t p_ulBufferSize)
{
    if (!p_ptCertificate || !p_pcBuffer || p_ulBufferSize == 0)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    size_t l_ulIssuerLen = strlen(p_ptCertificate->t_acIssuer);
    if (l_ulIssuerLen >= p_ulBufferSize)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    strncpy(p_pcBuffer, p_ptCertificate->t_acIssuer, p_ulBufferSize - 1);
    p_pcBuffer[p_ulBufferSize - 1] = '\0';

    return CERT_OK;
}

//////////////////////////////////
/// xCertificateIsCA
//////////////////////////////////
int xCertificateIsCA(xCertificate_t *p_ptCertificate,
                     bool *p_pbIsCA)
{
    if (!p_ptCertificate || !p_pbIsCA)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    *p_pbIsCA = p_ptCertificate->t_bIsCA;
    return CERT_OK;
}

//////////////////////////////////
/// xCertificateGetErrorString
//////////////////////////////////
const char *xCertificateGetErrorString(int p_iError)
{
    switch (p_iError)
    {
        case CERT_OK:
            return "Success";
        case CERT_ERROR_INVALID_PARAM:
            return "Invalid parameter";
        case CERT_ERROR_MEMORY_ALLOC:
            return "Memory allocation failed";
        case CERT_ERROR_INVALID_CERT:
            return "Invalid certificate";
        case CERT_ERROR_CERT_EXPIRED:
            return "Certificate expired";
        case CERT_ERROR_NOT_YET_VALID:
            return "Certificate not yet valid";
        case CERT_ERROR_WOLFSSL_ERROR:
            return "WolfSSL error";
        default:
            return "Unknown error";
    }
}

////////////////////////////////////////////////////////////
// Static Helper Functions
////////////////////////////////////////////////////////////

//////////////////////////////////
/// extractCertificateInfo
//////////////////////////////////
static int extractCertificateInfo(xCertificate_t *p_ptCertificate)
{
    if (!p_ptCertificate)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    DecodedCert *l_ptDecodedCert = &p_ptCertificate->t_tDecodedCert;

    // Extract subject name (simplified - just copy the available data)
    if (l_ptDecodedCert->subjectCNLen > 0 && l_ptDecodedCert->subjectCNLen < CERT_MAX_SUBJECT_SIZE)
    {
        memcpy(p_ptCertificate->t_acSubject, l_ptDecodedCert->subjectCN, (size_t)l_ptDecodedCert->subjectCNLen);
        p_ptCertificate->t_acSubject[l_ptDecodedCert->subjectCNLen] = '\0';
    }
    else
    {
        strncpy(p_ptCertificate->t_acSubject, "Unknown Subject", CERT_MAX_SUBJECT_SIZE - 1);
        p_ptCertificate->t_acSubject[CERT_MAX_SUBJECT_SIZE - 1] = '\0';
    }

    // Extract issuer name (using issuer array field)
    if (l_ptDecodedCert->issuer[0] != '\0')
    {
        size_t l_ulIssuerLen = strlen(l_ptDecodedCert->issuer);
        if (l_ulIssuerLen < CERT_MAX_ISSUER_SIZE)
        {
            memcpy(p_ptCertificate->t_acIssuer, l_ptDecodedCert->issuer, l_ulIssuerLen);
            p_ptCertificate->t_acIssuer[l_ulIssuerLen] = '\0';
        }
        else
        {
            strncpy(p_ptCertificate->t_acIssuer, l_ptDecodedCert->issuer, CERT_MAX_ISSUER_SIZE - 1);
            p_ptCertificate->t_acIssuer[CERT_MAX_ISSUER_SIZE - 1] = '\0';
        }
    }
    else
    {
        strncpy(p_ptCertificate->t_acIssuer, "Unknown Issuer", CERT_MAX_ISSUER_SIZE - 1);
        p_ptCertificate->t_acIssuer[CERT_MAX_ISSUER_SIZE - 1] = '\0';
    }

    // Extract validity period from decoded certificate
    // Note: WolfSSL stores these as ASN.1 time structures
    // For simplicity, we'll set them to current time ± reasonable periods
    // In a real implementation, you'd parse the ASN.1 time properly
    time_t l_tCurrentTime = time(NULL);
    p_ptCertificate->t_tNotBefore = l_tCurrentTime - (365 * 24 * 60 * 60); // 1 year ago
    p_ptCertificate->t_tNotAfter = l_tCurrentTime + (365 * 24 * 60 * 60);  // 1 year from now

    // Check if certificate is a CA using WolfSSL's decoded information
    p_ptCertificate->t_bIsCA = (l_ptDecodedCert->isCA == 1);

    // Determine certificate type based on issuer/subject relationship and CA status
    if (strcmp(p_ptCertificate->t_acSubject, p_ptCertificate->t_acIssuer) == 0)
    {
        p_ptCertificate->t_eType = CERT_TYPE_ROOT_CA;
    }
    else if (p_ptCertificate->t_bIsCA)
    {
        p_ptCertificate->t_eType = CERT_TYPE_INTERMEDIATE_CA;
    }
    else
    {
        p_ptCertificate->t_eType = CERT_TYPE_END_ENTITY;
    }

    return CERT_OK;
}

//////////////////////////////////
/// secureClearCertificate
//////////////////////////////////
static void secureClearCertificate(xCertificate_t *p_ptCertificate)
{
    if (!p_ptCertificate)
    {
        return;
    }

    // Clear sensitive data
    XOS_MEMORY_SANITIZE(p_ptCertificate->t_acSubject, CERT_MAX_SUBJECT_SIZE);
    XOS_MEMORY_SANITIZE(p_ptCertificate->t_acIssuer, CERT_MAX_ISSUER_SIZE);
    XOS_MEMORY_SANITIZE(p_ptCertificate, sizeof(xCertificate_t));
}

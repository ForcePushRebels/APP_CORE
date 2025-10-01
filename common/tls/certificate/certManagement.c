////////////////////////////////////////////////////////////
//  xCertificate implementation file
//  implements the Certificate management with CA hierarchy
//
// general disclosure: copy or share the file is forbidden
// Written : 12/06/2025
////////////////////////////////////////////////////////////

// Define _GNU_SOURCE for timegm if not already defined
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "certManagement.h"
#include "xLog.h"
#include "xOsMemory.h"
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <time.h>

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
/// certManagementInit
//////////////////////////////////
int certManagementInit(void)
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
/// certManagementCleanup
//////////////////////////////////
int certManagementCleanup(void)
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
/// loadCertificate
//////////////////////////////////
int loadCertificate(const uint8_t *p_pucCertData, uint32_t p_ulDataSize, bool p_bIsPEM, xCertificate_t **p_ppttCertificate)
{
    // Parameter validation
    if (!p_pucCertData || p_ulDataSize == 0 || !p_ppttCertificate)
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

        int l_iResult = wc_CertPemToDer(p_pucCertData, (int)p_ulDataSize, l_pucDerData, (int)p_ulDataSize, CERT_TYPE);
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
    XOS_SECURE_BUFFER_COPY(l_ptCertificate->p_pucCertData, p_ulDataSize, p_pucCertData, p_ulDataSize);
    l_ptCertificate->t_ulCertSize = p_ulDataSize;

    // Store DER data in certificate structure
    l_ptCertificate->p_pucDerData = l_pucDerData;
    l_ptCertificate->t_ulDerSize = l_ulDerSize;

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

    // Don't free DER data here - it's needed by WolfSSL DecodedCert structure

    // Set default status
    l_ptCertificate->t_eStatus = CERT_STATUS_UNKNOWN;

    *p_ppttCertificate = l_ptCertificate;

    X_LOG_TRACE("Certificate loaded successfully: Subject=%s", l_ptCertificate->t_acSubject);
    return CERT_OK;
}

//////////////////////////////////
/// loadCertificateFromFile
//////////////////////////////////
int loadCertificateFromFile(const char *p_ptcFilePath, bool p_bIsPEM, xCertificate_t **p_ppttCertificate)
{
    if (!p_ptcFilePath || !p_ppttCertificate)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    // Open file
    FILE *l_ptFile = fopen(p_ptcFilePath, "rb");
    if (!l_ptFile)
    {
        X_LOG_TRACE("Failed to open certificate file: %s", p_ptcFilePath);
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
    int l_iResult = loadCertificate(l_pucFileData, (uint32_t)l_lFileSize, p_bIsPEM, p_ppttCertificate);

    // Clean up file data
    XOS_MEMORY_SANITIZE(l_pucFileData, (size_t)l_lFileSize);
    free(l_pucFileData);

    return l_iResult;
}

//////////////////////////////////
/// loadCertificateRootCA
//////////////////////////////////
int loadCertificateRootCA(const char *p_ptcCADirectoryPath, bool p_bIsPEM, xCertificate_t **p_ppttRootCA)
{
    if (!p_ptcCADirectoryPath || !p_ppttRootCA)
    {
        X_LOG_TRACE("Invalid parameters for root CA loading");
        return CERT_ERROR_INVALID_PARAM;
    }

    if (!s_bCertificateSystemInitialized)
    {
        X_LOG_TRACE("Certificate system not initialized");
        return CERT_ERROR_WOLFSSL_ERROR;
    }

    DIR *l_ptDirectory = opendir(p_ptcCADirectoryPath);
    if (!l_ptDirectory)
    {
        X_LOG_TRACE("Failed to open CA directory: %s", p_ptcCADirectoryPath);
        return CERT_ERROR_INVALID_CERT;
    }

    struct dirent *l_ptEntry;
    xCertificate_t *l_ptBestRootCA = NULL;
    char l_acFilePath[512];
    memset(l_acFilePath, 0, sizeof(l_acFilePath));
    const char *l_pcExtension = p_bIsPEM ? ".pem" : ".der";
    const char *l_pcAltExtension = p_bIsPEM ? ".crt" : ".cer";

    // Parcourir tous les fichiers du répertoire
    while ((l_ptEntry = readdir(l_ptDirectory)) != NULL)
    {
        // Vérifier l'extension du fichier
        size_t l_ulNameLen = strlen(l_ptEntry->d_name);
        size_t l_ulExtLen = strlen(l_pcExtension);
        size_t l_ulAltExtLen = strlen(l_pcAltExtension);

        bool l_bValidExtension = false;
        if (l_ulNameLen > l_ulExtLen && strcasecmp(l_ptEntry->d_name + l_ulNameLen - l_ulExtLen, l_pcExtension) == 0)
        {
            l_bValidExtension = true;
        }
        else if (l_ulNameLen > l_ulAltExtLen
                 && strcasecmp(l_ptEntry->d_name + l_ulNameLen - l_ulAltExtLen, l_pcAltExtension) == 0)
        {
            l_bValidExtension = true;
        }

        if (!l_bValidExtension)
        {
            continue;
        }

        // Construire le chemin complet du fichier
        snprintf(l_acFilePath, sizeof(l_acFilePath), "%s/%s", p_ptcCADirectoryPath, l_ptEntry->d_name);

        // Vérifier que c'est un fichier régulier
        struct stat l_tFileStat;
        if (stat(l_acFilePath, &l_tFileStat) != 0 || !S_ISREG(l_tFileStat.st_mode))
        {
            continue;
        }

        // Charger le certificat
        xCertificate_t *l_ptCertificate = NULL;
        int l_iResult = loadCertificateFromFile(l_acFilePath, p_bIsPEM, &l_ptCertificate);
        if (l_iResult != CERT_OK)
        {
            X_LOG_TRACE("Failed to load certificate from %s: %d", l_acFilePath, l_iResult);
            continue;
        }

        // Vérifier si c'est un certificat CA
        if (!l_ptCertificate->t_bIsCA)
        {
            X_LOG_TRACE("Certificate %s is not a CA certificate", l_ptEntry->d_name);
            freeCertRessources(l_ptCertificate);
            continue;
        }

        // Vérifier si c'est un certificat racine (auto-signé)
        if (strcmp(l_ptCertificate->t_acSubject, l_ptCertificate->t_acIssuer) == 0)
        {
            X_LOG_TRACE("Found root CA certificate: %s", l_ptEntry->d_name);

            // Si on n'a pas encore de certificat racine, ou si celui-ci est plus récent
            if (!l_ptBestRootCA || l_ptCertificate->t_tNotAfter > l_ptBestRootCA->t_tNotAfter)
            {
                // Libérer l'ancien certificat racine s'il existe
                if (l_ptBestRootCA)
                {
                    freeCertRessources(l_ptBestRootCA);
                }
                l_ptBestRootCA = l_ptCertificate;
            }
            else
            {
                // Garder l'ancien, libérer le nouveau
                freeCertRessources(l_ptCertificate);
            }
        }
        else
        {
            // Ce n'est pas un certificat racine
            freeCertRessources(l_ptCertificate);
        }
    }

    closedir(l_ptDirectory);

    if (!l_ptBestRootCA)
    {
        X_LOG_TRACE("No root CA certificate found in directory: %s", p_ptcCADirectoryPath);
        return CERT_ERROR_INVALID_CERT;
    }

    // Vérifier la validité du certificat racine trouvé
    int l_iValidityResult = processCertValidity(l_ptBestRootCA);
    if (l_iValidityResult != CERT_OK)
    {
        X_LOG_TRACE("Root CA certificate is not valid: %x", l_iValidityResult);
        freeCertRessources(l_ptBestRootCA);
        return l_iValidityResult;
    }

    *p_ppttRootCA = l_ptBestRootCA;
    X_LOG_TRACE("Root CA certificate loaded successfully: Subject=%s", l_ptBestRootCA->t_acSubject);

    return CERT_OK;
}

//////////////////////////////////
/// freeCertRessources
//////////////////////////////////
int freeCertRessources(xCertificate_t *p_ptCertificate)
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

    // Securely clear and free DER data
    if (p_ptCertificate->p_pucDerData)
    {
        XOS_MEMORY_SANITIZE(p_ptCertificate->p_pucDerData, p_ptCertificate->t_ulDerSize);
        free(p_ptCertificate->p_pucDerData);
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
/// processCertValidity
//////////////////////////////////
int processCertValidity(xCertificate_t *p_ptCertificate)
{
    if (!p_ptCertificate)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    time_t l_tCurrentTime = time(NULL);

    if (l_tCurrentTime < p_ptCertificate->t_tNotBefore)
    {
        X_LOG_TRACE("Certificate not yet valid : %s", p_ptCertificate->t_acSubject);
        p_ptCertificate->t_eStatus = CERT_STATUS_NOT_YET_VALID;
        return CERT_ERROR_NOT_YET_VALID;
    }

    if (l_tCurrentTime > p_ptCertificate->t_tNotAfter)
    {
        X_LOG_TRACE("Certificate expired : %s", p_ptCertificate->t_acSubject);
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
/// getCertSubject
//////////////////////////////////
int getCertSubject(xCertificate_t *p_ptCertificate, char *p_ptcBuffer, uint32_t p_ulBufferSize)
{
    if (!p_ptCertificate || !p_ptcBuffer || p_ulBufferSize == 0)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    size_t l_ulSubjectLen = strlen(p_ptCertificate->t_acSubject);
    if (l_ulSubjectLen >= p_ulBufferSize)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    strncpy(p_ptcBuffer, p_ptCertificate->t_acSubject, p_ulBufferSize - 1);
    p_ptcBuffer[p_ulBufferSize - 1] = '\0';

    return CERT_OK;
}

//////////////////////////////////
/// getCertIssuer
//////////////////////////////////
int getCertIssuer(xCertificate_t *p_ptCertificate, char *p_ptcBuffer, uint32_t p_ulBufferSize)
{
    if (!p_ptCertificate || !p_ptcBuffer || p_ulBufferSize == 0)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    size_t l_ulIssuerLen = strlen(p_ptCertificate->t_acIssuer);
    if (l_ulIssuerLen >= p_ulBufferSize)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    strncpy(p_ptcBuffer, p_ptCertificate->t_acIssuer, p_ulBufferSize - 1);
    p_ptcBuffer[p_ulBufferSize - 1] = '\0';

    return CERT_OK;
}

//////////////////////////////////
/// isCertCA
//////////////////////////////////
int isCertCA(xCertificate_t *p_ptCertificate, bool *p_pbIsCA)
{
    if (!p_ptCertificate || !p_pbIsCA)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    *p_pbIsCA = p_ptCertificate->t_bIsCA;
    return CERT_OK;
}

//////////////////////////////////
/// getCertErrorString
//////////////////////////////////
const char *getCertErrorString(int p_iError)
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

    // Extract subject name (use full subject DN for consistency)
    if (l_ptDecodedCert->subject[0] != '\0')
    {
        size_t l_ulSubjectLen = strlen(l_ptDecodedCert->subject);
        if (l_ulSubjectLen < CERT_MAX_SUBJECT_SIZE)
        {
            memcpy(p_ptCertificate->t_acSubject, l_ptDecodedCert->subject, l_ulSubjectLen);
            p_ptCertificate->t_acSubject[l_ulSubjectLen] = '\0';
        }
        else
        {
            strncpy(p_ptCertificate->t_acSubject, l_ptDecodedCert->subject, CERT_MAX_SUBJECT_SIZE - 1);
            p_ptCertificate->t_acSubject[CERT_MAX_SUBJECT_SIZE - 1] = '\0';
        }
    }
    else if (l_ptDecodedCert->subjectCNLen > 0 && l_ptDecodedCert->subjectCNLen < CERT_MAX_SUBJECT_SIZE)
    {
        // Fallback to CN only if full subject not available
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

    // Extract real validity period from decoded certificate
    int l_iDateResult
        = extractCertValidityDate(l_ptDecodedCert, &p_ptCertificate->t_tNotBefore, &p_ptCertificate->t_tNotAfter);
    if (l_iDateResult != CERT_OK)
    {
        X_LOG_TRACE("Failed to extract validity dates, using fallback values");
        // Fallback to current time ± reasonable periods if extraction fails
        time_t l_tCurrentTime = time(NULL);
        p_ptCertificate->t_tNotBefore = l_tCurrentTime - (365 * 24 * 60 * 60); // 1 year ago
        p_ptCertificate->t_tNotAfter = l_tCurrentTime + (365 * 24 * 60 * 60);  // 1 year from now
    }

    // Check if certificate is a CA using WolfSSL's decoded information
    p_ptCertificate->t_bIsCA = (l_ptDecodedCert->isCA == 1);

    // Determine certificate type based on issuer/subject relationship and CA status
    X_LOG_TRACE(
        "Certificate comparison - Subject: '%s', Issuer: '%s'", p_ptCertificate->t_acSubject, p_ptCertificate->t_acIssuer);

    if (strcmp(p_ptCertificate->t_acSubject, p_ptCertificate->t_acIssuer) == 0)
    {
        p_ptCertificate->t_eType = CERT_TYPE_ROOT_CA;
        X_LOG_TRACE("Certificate identified as ROOT CA (self-signed)");
    }
    else if (p_ptCertificate->t_bIsCA)
    {
        p_ptCertificate->t_eType = CERT_TYPE_INTERMEDIATE_CA;
        X_LOG_TRACE("Certificate identified as INTERMEDIATE CA");
    }
    else
    {
        p_ptCertificate->t_eType = CERT_TYPE_END_ENTITY;
        X_LOG_TRACE("Certificate identified as END ENTITY");
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

//////////////////////////////////
/// extractCertValidityDate
//////////////////////////////////
int extractCertValidityDate(DecodedCert *p_ptDecodedCert, time_t *p_ptNotBefore, time_t *p_ptNotAfter)
{
    if (!p_ptDecodedCert || !p_ptNotBefore || !p_ptNotAfter)
    {
        return CERT_ERROR_INVALID_PARAM;
    }

    // Extract validity dates from WolfSSL decoded certificate
    // WolfSSL stores dates in beforeDate and afterDate arrays
    struct tm l_tTmBefore = {0};
    struct tm l_tTmAfter = {0};

    // Parse beforeDate (format: YYMMDDHHMMSSZ or YYYYMMDDHHMMSSZ)
    if (p_ptDecodedCert->beforeDateLen >= 13) // At least YYMMDDHHMMSSZ
    {
        const char *l_pcBeforeDate = (const char *)p_ptDecodedCert->beforeDate;
        X_LOG_TRACE("Raw beforeDate: '%.*s' (len=%d)",
                    p_ptDecodedCert->beforeDateLen,
                    l_pcBeforeDate,
                    p_ptDecodedCert->beforeDateLen);
        int l_iYear, l_iMonth, l_iDay, l_iHour, l_iMin, l_iSec;

        if (p_ptDecodedCert->beforeDateLen == 13) // YYMMDDHHMMSSZ
        {
            if (sscanf(l_pcBeforeDate, "%2d%2d%2d%2d%2d%2d", &l_iYear, &l_iMonth, &l_iDay, &l_iHour, &l_iMin, &l_iSec) == 6)
            {
                l_tTmBefore.tm_year = (l_iYear < 50) ? l_iYear + 100 : l_iYear; // Y2K handling
                l_tTmBefore.tm_mon = l_iMonth - 1;
                l_tTmBefore.tm_mday = l_iDay;
                l_tTmBefore.tm_hour = l_iHour;
                l_tTmBefore.tm_min = l_iMin;
                l_tTmBefore.tm_sec = l_iSec;
            }
        }
        else if (p_ptDecodedCert->beforeDateLen == 15) // YYYYMMDDHHMMSSZ
        {
            if (sscanf(l_pcBeforeDate, "%4d%2d%2d%2d%2d%2d", &l_iYear, &l_iMonth, &l_iDay, &l_iHour, &l_iMin, &l_iSec) == 6)
            {
                l_tTmBefore.tm_year = l_iYear - 1900;
                l_tTmBefore.tm_mon = l_iMonth - 1;
                l_tTmBefore.tm_mday = l_iDay;
                l_tTmBefore.tm_hour = l_iHour;
                l_tTmBefore.tm_min = l_iMin;
                l_tTmBefore.tm_sec = l_iSec;
            }
        }
    }

    // Parse afterDate (same format as beforeDate)
    if (p_ptDecodedCert->afterDateLen >= 13)
    {
        const char *l_pcAfterDate = (const char *)p_ptDecodedCert->afterDate;
        X_LOG_TRACE(
            "Raw afterDate: '%.*s' (len=%d)", p_ptDecodedCert->afterDateLen, l_pcAfterDate, p_ptDecodedCert->afterDateLen);
        int l_iYear, l_iMonth, l_iDay, l_iHour, l_iMin, l_iSec;

        if (p_ptDecodedCert->afterDateLen == 13) // YYMMDDHHMMSSZ
        {
            if (sscanf(l_pcAfterDate, "%2d%2d%2d%2d%2d%2d", &l_iYear, &l_iMonth, &l_iDay, &l_iHour, &l_iMin, &l_iSec) == 6)
            {
                l_tTmAfter.tm_year = (l_iYear < 50) ? l_iYear + 100 : l_iYear; // Y2K handling
                l_tTmAfter.tm_mon = l_iMonth - 1;
                l_tTmAfter.tm_mday = l_iDay;
                l_tTmAfter.tm_hour = l_iHour;
                l_tTmAfter.tm_min = l_iMin;
                l_tTmAfter.tm_sec = l_iSec;
            }
        }
        else if (p_ptDecodedCert->afterDateLen == 15) // YYYYMMDDHHMMSSZ
        {
            if (sscanf(l_pcAfterDate, "%4d%2d%2d%2d%2d%2d", &l_iYear, &l_iMonth, &l_iDay, &l_iHour, &l_iMin, &l_iSec) == 6)
            {
                l_tTmAfter.tm_year = l_iYear - 1900;
                l_tTmAfter.tm_mon = l_iMonth - 1;
                l_tTmAfter.tm_mday = l_iDay;
                l_tTmAfter.tm_hour = l_iHour;
                l_tTmAfter.tm_min = l_iMin;
                l_tTmAfter.tm_sec = l_iSec;
            }
        }
    }

    // Convert to time_t (UTC)
    *p_ptNotBefore = timegm(&l_tTmBefore);
    *p_ptNotAfter = timegm(&l_tTmAfter);

    // Debug: Log extracted dates
    X_LOG_TRACE(
        "Certificate validity dates extracted - NotBefore: %ld, NotAfter: %ld", (long)*p_ptNotBefore, (long)*p_ptNotAfter);

    // Debug: Log current time for comparison
    time_t l_tCurrentTime = time(NULL);
    X_LOG_TRACE("Current time: %ld (NotBefore valid: %s, NotAfter valid: %s)",
                (long)l_tCurrentTime,
                (l_tCurrentTime >= *p_ptNotBefore) ? "YES" : "NO",
                (l_tCurrentTime <= *p_ptNotAfter) ? "YES" : "NO");

    return CERT_OK;
}

//////////////////////////////////
/// loadRootCAIntoCtx
//////////////////////////////////
int loadRootCAIntoCtx(WOLFSSL_CTX *p_ptSSLContext, const char *p_ptcCADirectoryPath, bool p_bIsPEM)
{
    if (!p_ptSSLContext || !p_ptcCADirectoryPath)
    {
        X_LOG_TRACE("Invalid parameters for loading root CA into context");
        return CERT_ERROR_INVALID_PARAM;
    }

    char l_acCAFilePath[512];
    memset(l_acCAFilePath, 0, sizeof(l_acCAFilePath));

    // Build CA file path (support both directory and file paths)
    if (strstr(p_ptcCADirectoryPath, ".pem") != NULL || strstr(p_ptcCADirectoryPath, ".der") != NULL)
    {
        // Already a file path, use as-is
        snprintf(l_acCAFilePath, sizeof(l_acCAFilePath), "%s", p_ptcCADirectoryPath);
    }
    else
    {
        // Directory path, append ca.pem or ca.der
        if (p_bIsPEM)
        {
            snprintf(l_acCAFilePath, sizeof(l_acCAFilePath), "%s/ca.pem", p_ptcCADirectoryPath);
        }
        else
        {
            snprintf(l_acCAFilePath, sizeof(l_acCAFilePath), "%s/ca.der", p_ptcCADirectoryPath);
        }
    }

    X_LOG_TRACE("Attempting to load root CA from: %s", l_acCAFilePath);

    // Try loading as file first
    int l_iWolfResult = wolfSSL_CTX_load_verify_locations(p_ptSSLContext, l_acCAFilePath, NULL);
    if (l_iWolfResult == WOLFSSL_SUCCESS)
    {
        X_LOG_TRACE("Root CA loaded successfully from file: %s", l_acCAFilePath);
        return CERT_OK;
    }

    // If file failed and it's a directory, try loading from directory
    if (strstr(p_ptcCADirectoryPath, ".pem") == NULL && strstr(p_ptcCADirectoryPath, ".der") == NULL)
    {
        X_LOG_TRACE("File load failed, trying directory: %s", p_ptcCADirectoryPath);
        l_iWolfResult = wolfSSL_CTX_load_verify_locations(p_ptSSLContext, NULL, p_ptcCADirectoryPath);
        if (l_iWolfResult == WOLFSSL_SUCCESS)
        {
            X_LOG_TRACE("Root CA loaded successfully from directory: %s", p_ptcCADirectoryPath);
            return CERT_OK;
        }
    }

    X_LOG_TRACE("Failed to load CA from %s (error: %d)", l_acCAFilePath, l_iWolfResult);
    return CERT_ERROR_WOLFSSL_ERROR;
}

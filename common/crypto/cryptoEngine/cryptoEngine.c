////////////////////////////////////////////////////////////
//  cryptoEngine source file
//  implements the crypto engine types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/06/2025
////////////////////////////////////////////////////////////

#include "cryptoEngine.h"

////////////////////////////////////////////////////////////
//  getCryptoError
////////////////////////////////////////////////////////////
void getCryptoError(int p_iError, char *p_ptcError)
{
    if (p_ptcError == NULL)
    {
        return;
    }

    memset(p_ptcError, 0, MAX_ERROR_STRING_LENGTH);

    wc_ErrorString(p_iError, p_ptcError);
}
////////////////////////////////////////////////////////////
//  AES256 Cipher and Decipher primitives functions
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
//  AES256 implementation using the context defined in header
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
/// aes256_ctx_init
////////////////////////////////////////////////////////////
int aes256_ctx_init(aes256_ctx_t *p_ptCtx, const uint8_t *p_ptKey, size_t p_iKeyLen)
{
    // Validate input parameters
    if (!p_ptCtx || !p_ptKey || p_iKeyLen != 32)
    {
        return CRYPTO_ERROR_INVALID_PARAM;
    }

    // Clear the context structure
    memset(p_ptCtx, 0, sizeof(aes256_ctx_t));

    // Initialize the AES context for GCM mode
    int ret = wc_AesInit(&p_ptCtx->t_tAes, NULL, 0);
    if (ret != 0) // WolfSSL functions return 0 on success
    {
        return CRYPTO_ERROR_WOLFSSL;
    }

    // Set the AES-GCM key
    ret = wc_AesGcmSetKey(&p_ptCtx->t_tAes, p_ptKey, (word32)p_iKeyLen);
    if (ret != 0) // WolfSSL functions return 0 on success
    {
        wc_AesFree(&p_ptCtx->t_tAes);
        return CRYPTO_ERROR_WOLFSSL;
    }

    // Store the key in the context for reference
    memcpy(p_ptCtx->t_aucKey, p_ptKey, p_iKeyLen);

    return CRYPTO_OK;
}

////////////////////////////////////////////////////////////
/// aes256_free
////////////////////////////////////////////////////////////
int aes256_free(aes256_ctx_t *p_ptCtx)
{
    if (!p_ptCtx)
    {
        return CRYPTO_ERROR_INVALID_PARAM;
    }

    // Free WolfSSL resources BEFORE clearing memory
    wc_AesFree(&p_ptCtx->t_tAes);

    // Clear sensitive data from memory after freeing resources
    memset(p_ptCtx, 0, sizeof(aes256_ctx_t));

    return CRYPTO_OK;
}

////////////////////////////////////////////////////////////
/// aes256_cipher
////////////////////////////////////////////////////////////
int aes256_cipher(aes256_ctx_t *p_ptCtx,
                  const uint8_t *p_ptInput,
                  size_t p_iInputLen,
                  const uint8_t *p_ptIv,
                  uint8_t *p_ptOutput,
                  uint8_t *p_ptTag,
                  const uint8_t *p_ptAad,
                  size_t p_iAadLen)
{
    // Validate required parameters
    if (!p_ptCtx || !p_ptInput || !p_ptIv || !p_ptOutput || !p_ptTag)
    {
        return CRYPTO_ERROR_INVALID_PARAM;
    }

    // Validate AAD parameters consistency
    if ((p_ptAad == NULL && p_iAadLen != 0) || (p_ptAad != NULL && p_iAadLen == 0))
    {
        return CRYPTO_ERROR_INVALID_PARAM;
    }

    // Perform AES-GCM encryption with provided IV
    int ret = wc_AesGcmEncrypt(&p_ptCtx->t_tAes,
                               p_ptOutput,
                               p_ptInput,
                               (word32)p_iInputLen,
                               p_ptIv,
                               AES256_GCM_IV_LENGTH,
                               p_ptTag,
                               AES256_GCM_TAG_LENGTH,
                               p_ptAad,
                               (word32)p_iAadLen);
    if (ret != 0) // WolfSSL functions return 0 on success
    {
        return CRYPTO_ERROR_WOLFSSL;
    }

    return CRYPTO_OK;
}

////////////////////////////////////////////////////////////
/// aes256_decipher
////////////////////////////////////////////////////////////
int aes256_decipher(aes256_ctx_t *p_ptCtx,
                    const uint8_t *p_ptInput,
                    size_t p_iInputLen,
                    const uint8_t *p_ptIv,
                    const uint8_t *p_ptTag,
                    uint8_t *p_ptOutput,
                    const uint8_t *p_ptAad,
                    size_t p_iAadLen)
{
    // Validate required parameters
    if (!p_ptCtx || !p_ptInput || !p_ptIv || !p_ptTag || !p_ptOutput)
    {
        return CRYPTO_ERROR_INVALID_PARAM;
    }

    // Validate AAD parameters consistency
    if ((p_ptAad == NULL && p_iAadLen != 0) || (p_ptAad != NULL && p_iAadLen == 0))
    {
        return CRYPTO_ERROR_INVALID_PARAM;
    }

    // Perform AES-GCM decryption with authentication verification
    int result = wc_AesGcmDecrypt(&p_ptCtx->t_tAes,
                                  p_ptOutput,
                                  p_ptInput,
                                  (word32)p_iInputLen,
                                  p_ptIv,
                                  AES256_GCM_IV_LENGTH,
                                  p_ptTag,
                                  AES256_GCM_TAG_LENGTH,
                                  p_ptAad,
                                  (word32)p_iAadLen);

    if (result != 0) // WolfSSL functions return 0 on success
    {
        // Clear output buffer on authentication failure for security
        if (p_ptOutput != NULL && p_iInputLen > 0)
        {
            memset(p_ptOutput, 0, p_iInputLen);
        }

        // Return specific error for authentication failure
        return (result == AES_GCM_AUTH_E) ? CRYPTO_ERROR_AUTH_FAILED : CRYPTO_ERROR_WOLFSSL;
    }

    return CRYPTO_OK;
}

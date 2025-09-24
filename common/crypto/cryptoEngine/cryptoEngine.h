////////////////////////////////////////////////////////////
//  cryptoEngine source file
//  implements the crypto engine types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/06/2025
////////////////////////////////////////////////////////////

#include <wolfssl/options.h>
#include <wolfssl/ssl.h> // for SSL_SUCCESS
#include <wolfssl/wolfcrypt/aes.h>
#include <wolfssl/wolfcrypt/error-crypt.h>
#include <wolfssl/wolfcrypt/hmac.h>
#include <wolfssl/wolfcrypt/pwdbased.h>
#include <wolfssl/wolfcrypt/random.h>
#include <wolfssl/wolfcrypt/settings.h>
#include <wolfssl/wolfcrypt/sha3.h>

#include <stdint.h>
#include <string.h>

#define MAX_ERROR_STRING_LENGTH WOLFSSL_MAX_ERROR_SZ
#define AES256_GCM_TAG_LENGTH 16
#define AES256_GCM_IV_LENGTH 12
#define AES256_KEY_LENGTH 32

// Additional error codes for improved error handling
#define CRYPTO_ERROR_AUTH_FAILED 0x7CC00021

////////////////////////////////////////////////////////////
//  Crypto error codes
////////////////////////////////////////////////////////////
#define CRYPTO_OK 0x7CC00000
#define CRYPTO_ERROR_INVALID_PARAM 0x7CC00001
#define CRYPTO_ERROR_INVALID_PARAM_SIZE 0x7CC00002
#define CRYPTO_ERROR_INVALID_KEY 0x7CC00003
#define CRYPTO_ERROR_MEMORY_ALLOC 0x7CC00004
#define CRYPTO_ERROR_WOLFSSL 0x7CC00020

////////////////////////////////////////////////////////////
/// @brief Get crypto error string
/// @param [in] p_iError Error code
/// @param [out] p_ptcError Output buffer for error string
/// @return void
////////////////////////////////////////////////////////////
void getCryptoError(int p_iError, char *p_ptcError);

////////////////////////////////////////////////////////////
//  AES256 Cipher and Decipher primitives functions
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
//  AES256-GCM Context Structure
////////////////////////////////////////////////////////////
typedef struct aes256_ctx
{
    Aes t_tAes;
    uint8_t t_aucKey[32];
    // Note: IV and tag are now provided as function parameters
    // This makes the API more flexible and prevents accidental reuse
    // RNG removed since IV is provided externally
} aes256_ctx_t;

////////////////////////////////////////////////////////////
/// aes256_ctx_init
/// @brief Initialize AES256-GCM context
/// @param [in] p_ptCtx AES256 context to initialize
/// @param [in] p_ptKey AES256 key (must be 32 bytes)
/// @param [in] p_iKeyLen AES256 key length (must be 32)
/// @return int Error code (CRYPTO_OK on success)
/// @note IV should be set per encryption/decryption operation, not during init
////////////////////////////////////////////////////////////
int aes256_ctx_init(aes256_ctx_t *p_ptCtx, const uint8_t *p_ptKey, size_t p_iKeyLen);

////////////////////////////////////////////////////////////
/// aes256_free
/// @brief Free AES256-GCM context
/// @param [in] p_ptCtx AES256 context to free
/// @return int Error code (CRYPTO_OK on success)
////////////////////////////////////////////////////////////
int aes256_free(aes256_ctx_t *p_ptCtx);

////////////////////////////////////////////////////////////
/// aes256_cipher
/// @brief Encrypt data using AES256-GCM
/// @param [in] p_ptCtx AES256 context
/// @param [in] p_ptInput Input data
/// @param [in] p_iInputLen Input data length
/// @param [in] p_ptIv Initialization Vector (must be AES256_GCM_IV_LENGTH bytes)
/// @param [out] p_ptOutput Output data
/// @param [out] p_ptTag Authentication tag output (AES256_GCM_TAG_LENGTH bytes)
/// @param [in] p_ptAad Additional authenticated data (can be NULL)
/// @param [in] p_iAadLen AAD length (0 if p_ptAad is NULL)
/// @return int Error code (CRYPTO_OK on success)
/// @warning IV MUST be unique for each encryption with the same key
////////////////////////////////////////////////////////////
int aes256_cipher(aes256_ctx_t *p_ptCtx,
                  const uint8_t *p_ptInput,
                  size_t p_iInputLen,
                  const uint8_t *p_ptIv,
                  uint8_t *p_ptOutput,
                  uint8_t *p_ptTag,
                  const uint8_t *p_ptAad,
                  size_t p_iAadLen);

////////////////////////////////////////////////////////////
/// aes256_decipher
/// @brief Decrypt data using AES256-GCM
/// @param [in] p_ptCtx AES256 context
/// @param [in] p_ptInput Input ciphertext data
/// @param [in] p_iInputLen Input data length
/// @param [in] p_ptIv Initialization Vector (must be AES256_GCM_IV_LENGTH bytes)
/// @param [in] p_ptTag Authentication tag (AES256_GCM_TAG_LENGTH bytes)
/// @param [out] p_ptOutput Output plaintext data
/// @param [in] p_ptAad Additional authenticated data (can be NULL)
/// @param [in] p_iAadLen AAD length (0 if p_ptAad is NULL)
/// @return int Error code (CRYPTO_OK on success, CRYPTO_ERROR_AUTH_FAILED if tag verification fails)
/// @warning Authentication tag verification failure indicates data tampering
////////////////////////////////////////////////////////////
int aes256_decipher(aes256_ctx_t *p_ptCtx,
                    const uint8_t *p_ptInput,
                    size_t p_iInputLen,
                    const uint8_t *p_ptIv,
                    const uint8_t *p_ptTag,
                    uint8_t *p_ptOutput,
                    const uint8_t *p_ptAad,
                    size_t p_iAadLen);

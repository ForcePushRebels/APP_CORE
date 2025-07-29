/*
 * FFI Bindings vers WolfSSL
 * 
 * Ce module définit les liaisons FFI vers les fonctions WolfSSL utilisées
 * dans l'implémentation originale C. Les signatures doivent correspondre
 * exactement aux fonctions WolfSSL.
 */

use std::os::raw::{c_char, c_int, c_void};

// ============================================================================
// Types opaques WolfSSL
// ============================================================================

/// Contexte WolfSSL - structure opaque
#[repr(C)]
pub struct WOLFSSL_CTX {
    _private: [u8; 0],
}

/// Session WolfSSL - structure opaque
#[repr(C)]
pub struct WOLFSSL {
    _private: [u8; 0],
}

/// Méthode WolfSSL - structure opaque
#[repr(C)]
pub struct WOLFSSL_METHOD {
    _private: [u8; 0],
}

/// X509 Certificate - opaque structure
#[repr(C)]
pub struct WOLFSSL_X509 {
    _private: [u8; 0],
}

/// X509 Name - opaque structure
#[repr(C)]
pub struct WOLFSSL_X509_NAME {
    _private: [u8; 0],
}

/// ASN1 Time - opaque structure
#[repr(C)]
pub struct ASN1_TIME {
    _private: [u8; 0],
}

/// Post quantum key - opaque structure²
#[repr(C)]
pub struct PQ_KEY {
    _private: [u8; 0],
}

/// ML-KEM key structure - opaque
#[repr(C)]
pub struct MLKEM_KEY {
    _private: [u8; 0],
}

/// ML-DSA key structure - opaque
#[repr(C)]
pub struct MLDSA_KEY {
    _private: [u8; 0],
}

/// SLH-DSA key structure - opaque
#[repr(C)]
pub struct SLHDSA_KEY {
    _private: [u8; 0],
}

// ============================================================================
// Constantes WolfSSL (doivent correspondre aux headers C)
// ============================================================================

pub const WOLFSSL_SUCCESS: c_int = 1;
pub const WOLFSSL_FAILURE: c_int = 0;
pub const WOLFSSL_SHUTDOWN_NOT_DONE: c_int = 2;
pub const WOLFSSL_ERROR_WANT_READ: c_int = -2;
pub const WOLFSSL_ERROR_WANT_WRITE: c_int = -3;

pub const WOLFSSL_FILETYPE_ASN1: c_int = 2;
pub const WOLFSSL_FILETYPE_PEM: c_int = 1;

// ============================================================================
// Constantes Post-Quantiques NIST - wolfSSL 5.8.2
// ============================================================================

// ML-KEM (ex-Kyber) parameter sets - FIPS 203
pub const MLKEM512_KEY_SIZE: c_int = 1632;
pub const MLKEM512_CIPHER_SIZE: c_int = 768;
pub const MLKEM512_SHARED_SECRET_SIZE: c_int = 32;

pub const MLKEM768_KEY_SIZE: c_int = 2400;
pub const MLKEM768_CIPHER_SIZE: c_int = 1088;
pub const MLKEM768_SHARED_SECRET_SIZE: c_int = 32;

pub const MLKEM1024_KEY_SIZE: c_int = 3168;
pub const MLKEM1024_CIPHER_SIZE: c_int = 1568;
pub const MLKEM1024_SHARED_SECRET_SIZE: c_int = 32;

// ML-DSA (ex-Dilithium) parameter sets - FIPS 204
pub const MLDSA44_KEY_SIZE: c_int = 2560;
pub const MLDSA44_SIG_SIZE: c_int = 2420;
pub const MLDSA44_PRIV_KEY_SIZE: c_int = 2560;
pub const MLDSA44_PUB_KEY_SIZE: c_int = 1312;

pub const MLDSA65_KEY_SIZE: c_int = 4032;
pub const MLDSA65_SIG_SIZE: c_int = 3309;
pub const MLDSA65_PRIV_KEY_SIZE: c_int = 4032;
pub const MLDSA65_PUB_KEY_SIZE: c_int = 1952;

pub const MLDSA87_KEY_SIZE: c_int = 4896;
pub const MLDSA87_SIG_SIZE: c_int = 4627;
pub const MLDSA87_PRIV_KEY_SIZE: c_int = 4896;
pub const MLDSA87_PUB_KEY_SIZE: c_int = 2592;

// Hybrid key exchange group IDs pour TLS 1.3
pub const WOLFSSL_P256_MLKEM512: c_int = 0x2F00;
pub const WOLFSSL_P384_MLKEM768: c_int = 0x2F01;
pub const WOLFSSL_P521_MLKEM1024: c_int = 0x2F02;

// Signature algorithm IDs pour TLS 1.3
pub const WOLFSSL_MLDSA44: c_int = 0x0E01;
pub const WOLFSSL_MLDSA65: c_int = 0x0E02;
pub const WOLFSSL_MLDSA87: c_int = 0x0E03;

// Hybrid signature algorithm IDs
pub const WOLFSSL_ECDSA_P256_MLDSA44: c_int = 0x0F01;
pub const WOLFSSL_ECDSA_P384_MLDSA65: c_int = 0x0F02;
pub const WOLFSSL_ECDSA_P521_MLDSA87: c_int = 0x0F03;

// ============================================================================
// Liaisons FFI vers WolfSSL - fonctions principales
// ============================================================================

unsafe extern "C" {
    /// Initialise WolfSSL
    pub fn wolfSSL_Init() -> c_int;
    
    /// Nettoie WolfSSL
    pub fn wolfSSL_Cleanup() -> c_int;
    
    /// Obtient la méthode TLS 1.3 serveur
    pub fn wolfTLSv1_3_server_method() -> *const WOLFSSL_METHOD;
    
    /// Obtient la méthode TLS 1.3 client
    pub fn wolfTLSv1_3_client_method() -> *const WOLFSSL_METHOD;
    
    /// Crée un nouveau contexte WolfSSL
    pub fn wolfSSL_CTX_new(method: *const WOLFSSL_METHOD) -> *mut WOLFSSL_CTX;
    
    /// Libère un contexte WolfSSL
    pub fn wolfSSL_CTX_free(ctx: *mut WOLFSSL_CTX);
    
    /// Configure la liste des cipher suites
    pub fn wolfSSL_CTX_set_cipher_list(ctx: *mut WOLFSSL_CTX, list: *const c_char) -> c_int;
    
    /// Charge un certificat depuis un fichier
    pub fn wolfSSL_CTX_use_certificate_file(
        ctx: *mut WOLFSSL_CTX, 
        file: *const c_char, 
        format: c_int
    ) -> c_int;
    
    /// Charge une chaîne de certificats depuis un fichier (PEM)
    pub fn wolfSSL_CTX_use_certificate_chain_file(
        ctx: *mut WOLFSSL_CTX, 
        file: *const c_char
    ) -> c_int;
    
    /// Charge une clé privée depuis un fichier
    pub fn wolfSSL_CTX_use_PrivateKey_file(
        ctx: *mut WOLFSSL_CTX, 
        file: *const c_char, 
        format: c_int
    ) -> c_int;
    
    /// Charge les certificats CA pour la vérification
    pub fn wolfSSL_CTX_load_verify_locations(
        ctx: *mut WOLFSSL_CTX, 
        file: *const c_char, 
        path: *const c_char
    ) -> c_int;
    
    /// Crée une nouvelle session WolfSSL
    pub fn wolfSSL_new(ctx: *mut WOLFSSL_CTX) -> *mut WOLFSSL;
    
    /// Libère une session WolfSSL
    pub fn wolfSSL_free(ssl: *mut WOLFSSL);
    
    /// Associe un socket à une session WolfSSL
    pub fn wolfSSL_set_fd(ssl: *mut WOLFSSL, fd: c_int) -> c_int;
    
    /// Obtient le file descriptor associé à une session
    pub fn wolfSSL_get_fd(ssl: *mut WOLFSSL) -> c_int;
    
    /// Effectue le handshake TLS côté serveur
    pub fn wolfSSL_accept(ssl: *mut WOLFSSL) -> c_int;
    
    /// Effectue le handshake TLS côté client
    pub fn wolfSSL_connect(ssl: *mut WOLFSSL) -> c_int;
    
    /// Ferme proprement une session TLS
    pub fn wolfSSL_shutdown(ssl: *mut WOLFSSL) -> c_int;
    
    /// Obtient le code d'erreur détaillé
    pub fn wolfSSL_get_error(ssl: *mut WOLFSSL, ret: c_int) -> c_int;
    
    /// Lit des données depuis une session TLS
    pub fn wolfSSL_read(ssl: *mut WOLFSSL, data: *mut c_void, sz: c_int) -> c_int;
    
    /// Écrit des données vers une session TLS
    pub fn wolfSSL_write(ssl: *mut WOLFSSL, data: *const c_void, sz: c_int) -> c_int;

    pub fn wolfSSL_X509_load_certificate_file(file: *const c_char, format: c_int) -> *mut WOLFSSL_X509;
    pub fn wolfSSL_X509_free(x509: *mut WOLFSSL_X509);
    pub fn wolfSSL_X509_get_subject_name(x509: *mut WOLFSSL_X509) -> *mut WOLFSSL_X509_NAME;
    pub fn wolfSSL_X509_get_issuer_name(x509: *mut WOLFSSL_X509) -> *mut WOLFSSL_X509_NAME;
    pub fn wolfSSL_X509_NAME_oneline(name: *mut WOLFSSL_X509_NAME, buf: *mut c_char, size: c_int) -> *mut c_char;
    pub fn wolfSSL_X509_get_isCA(x509: *mut WOLFSSL_X509) -> c_int;
    pub fn wolfSSL_X509_get_notBefore(x509: *mut WOLFSSL_X509) -> *mut ASN1_TIME;
    pub fn wolfSSL_X509_get_notAfter(x509: *mut WOLFSSL_X509) -> *mut ASN1_TIME;

    // ============================================================================
    // Fonctions ML-KEM (Key Encapsulation Mechanism) - FIPS 203
    // ============================================================================
    
    /// Initialise une clé ML-KEM
    pub fn wc_MlKemKey_Init(key: *mut MLKEM_KEY, level: c_int) -> c_int;
    
    /// Libère une clé ML-KEM
    pub fn wc_MlKemKey_Free(key: *mut MLKEM_KEY);
    
    /// Génère une paire de clés ML-KEM
    pub fn wc_MlKemKey_MakeKey(key: *mut MLKEM_KEY, rng: *mut c_void) -> c_int;
    
    /// Encapsule un secret avec ML-KEM
    pub fn wc_MlKemKey_Encap(
        key: *mut MLKEM_KEY,
        cipher_text: *mut u8,
        cipher_text_len: *mut c_int,
        shared_secret: *mut u8,
        shared_secret_len: *mut c_int,
        rng: *mut c_void
    ) -> c_int;
    
    /// Décapsule un secret avec ML-KEM
    pub fn wc_MlKemKey_Decap(
        key: *mut MLKEM_KEY,
        shared_secret: *mut u8,
        shared_secret_len: *mut c_int,
        cipher_text: *const u8,
        cipher_text_len: c_int
    ) -> c_int;
    
    /// Exporte la clé publique ML-KEM
    pub fn wc_MlKemKey_ExportPub(
        key: *mut MLKEM_KEY,
        pub_key: *mut u8,
        pub_key_len: *mut c_int
    ) -> c_int;
    
    /// Importe une clé publique ML-KEM
    pub fn wc_MlKemKey_ImportPub(
        key: *mut MLKEM_KEY,
        pub_key: *const u8,
        pub_key_len: c_int
    ) -> c_int;
    
    /// Exporte la clé privée ML-KEM
    pub fn wc_MlKemKey_ExportPriv(
        key: *mut MLKEM_KEY,
        priv_key: *mut u8,
        priv_key_len: *mut c_int
    ) -> c_int;
    
    /// Importe une clé privée ML-KEM
    pub fn wc_MlKemKey_ImportPriv(
        key: *mut MLKEM_KEY,
        priv_key: *const u8,
        priv_key_len: c_int
    ) -> c_int;

    // ============================================================================
    // Fonctions ML-DSA (Digital Signature Algorithm) - FIPS 204
    // ============================================================================
    
    /// Initialise une clé ML-DSA
    pub fn wc_MlDsaKey_Init(key: *mut MLDSA_KEY, level: c_int) -> c_int;
    
    /// Libère une clé ML-DSA
    pub fn wc_MlDsaKey_Free(key: *mut MLDSA_KEY);
    
    /// Génère une paire de clés ML-DSA
    pub fn wc_MlDsaKey_MakeKey(key: *mut MLDSA_KEY, rng: *mut c_void) -> c_int;
    
    /// Signe des données avec ML-DSA
    pub fn wc_MlDsaKey_Sign(
        key: *mut MLDSA_KEY,
        sig: *mut u8,
        sig_len: *mut c_int,
        msg: *const u8,
        msg_len: c_int,
        rng: *mut c_void
    ) -> c_int;
    
    /// Vérifie une signature ML-DSA
    pub fn wc_MlDsaKey_Verify(
        key: *mut MLDSA_KEY,
        sig: *const u8,
        sig_len: c_int,
        msg: *const u8,
        msg_len: c_int
    ) -> c_int;
    
    /// Exporte la clé publique ML-DSA
    pub fn wc_MlDsaKey_ExportPub(
        key: *mut MLDSA_KEY,
        pub_key: *mut u8,
        pub_key_len: *mut c_int
    ) -> c_int;
    
    /// Importe une clé publique ML-DSA
    pub fn wc_MlDsaKey_ImportPub(
        key: *mut MLDSA_KEY,
        pub_key: *const u8,
        pub_key_len: c_int
    ) -> c_int;
    
    /// Exporte la clé privée ML-DSA
    pub fn wc_MlDsaKey_ExportPriv(
        key: *mut MLDSA_KEY,
        priv_key: *mut u8,
        priv_key_len: *mut c_int
    ) -> c_int;
    
    /// Importe une clé privée ML-DSA
    pub fn wc_MlDsaKey_ImportPriv(
        key: *mut MLDSA_KEY,
        priv_key: *const u8,
        priv_key_len: c_int
    ) -> c_int;

    // ============================================================================
    // Fonctions TLS 1.3 Post-Quantique
    // ============================================================================
    
    /// Configure les groupes hybrides supportés pour TLS 1.3
    pub fn wolfSSL_CTX_set_groups_list(ctx: *mut WOLFSSL_CTX, list: *const c_char) -> c_int;
    
    /// Configure les algorithmes de signature hybrides supportés
    pub fn wolfSSL_CTX_set_sigalgs_list(ctx: *mut WOLFSSL_CTX, list: *const c_char) -> c_int;
    
    /// Active les extensions post-quantiques X.509
    pub fn wolfSSL_CTX_enable_pq_x509_extensions(ctx: *mut WOLFSSL_CTX) -> c_int;
    
    /// Charge un certificat hybride depuis un fichier
    pub fn wolfSSL_CTX_use_certificate_chain_file_pq(
        ctx: *mut WOLFSSL_CTX,
        file: *const c_char,
        format: c_int
    ) -> c_int;
    
    /// Vérifie si une session utilise des algorithmes post-quantiques
    pub fn wolfSSL_is_pq_handshake(ssl: *mut WOLFSSL) -> c_int;
    
    /// Obtient le groupe négocié pour la session
    pub fn wolfSSL_get_negotiated_group(ssl: *mut WOLFSSL) -> c_int;
    
    /// Obtient l'algorithme de signature négocié
    pub fn wolfSSL_get_negotiated_sigalg(ssl: *mut WOLFSSL) -> c_int;

    // ============================================================================
    // Fonctions X.509 hybrides et extensions post-quantiques
    // ============================================================================
    
    /// Vérifie si un certificat contient des clés publiques alternatives (hybrides)
    pub fn wolfSSL_X509_has_alt_public_key(x509: *mut WOLFSSL_X509) -> c_int;
    
    /// Obtient la clé publique alternative d'un certificat
    pub fn wolfSSL_X509_get_alt_public_key(
        x509: *mut WOLFSSL_X509,
        key_buf: *mut u8,
        key_len: *mut c_int,
        key_type: *mut c_int
    ) -> c_int;
    
    /// Vérifie si un certificat contient des signatures alternatives
    pub fn wolfSSL_X509_has_alt_signature(x509: *mut WOLFSSL_X509) -> c_int;
    
    /// Obtient la signature alternative d'un certificat
    pub fn wolfSSL_X509_get_alt_signature(
        x509: *mut WOLFSSL_X509,
        sig_buf: *mut u8,
        sig_len: *mut c_int,
        sig_alg: *mut c_int
    ) -> c_int;
}

// ============================================================================
// Wrappers Rust pour simplifier l'usage
// ============================================================================

/// Wrapper Rust pour wolfSSL_Init
pub fn wolfssl_init() -> c_int {
    unsafe { wolfSSL_Init() }
}

/// Wrapper Rust pour wolfSSL_Cleanup
pub fn wolfssl_cleanup() -> c_int {
    unsafe { wolfSSL_Cleanup() }
}

/// Wrapper Rust pour wolfTLSv1_3_server_method
pub fn wolfssl_tlsv1_3_server_method() -> *const WOLFSSL_METHOD {
    unsafe { wolfTLSv1_3_server_method() }
}

/// Wrapper Rust pour wolfTLSv1_3_client_method
pub fn wolfssl_tlsv1_3_client_method() -> *const WOLFSSL_METHOD {
    unsafe { wolfTLSv1_3_client_method() }
}

/// Wrapper Rust pour wolfSSL_CTX_new
pub fn wolfssl_ctx_new(method: *const WOLFSSL_METHOD) -> *mut WOLFSSL_CTX {
    unsafe { wolfSSL_CTX_new(method) }
}

/// Wrapper Rust pour wolfSSL_CTX_free
pub fn wolfssl_ctx_free(ctx: *mut WOLFSSL_CTX) {
    unsafe { wolfSSL_CTX_free(ctx) }
}

/// Wrapper Rust pour wolfSSL_CTX_set_cipher_list
pub fn wolfssl_ctx_set_cipher_list(ctx: *mut WOLFSSL_CTX, list: *const c_char) -> c_int {
    unsafe { wolfSSL_CTX_set_cipher_list(ctx, list) }
}

/// Wrapper Rust pour wolfSSL_CTX_use_certificate_file
pub fn wolfssl_ctx_use_certificate_file(
    ctx: *mut WOLFSSL_CTX, 
    file: *const c_char, 
    format: c_int
) -> c_int {
    unsafe { wolfSSL_CTX_use_certificate_file(ctx, file, format) }
}

/// Wrapper Rust pour wolfSSL_CTX_use_certificate_chain_file
pub fn wolfssl_ctx_use_certificate_chain_file(
    ctx: *mut WOLFSSL_CTX, 
    file: *const c_char
) -> c_int {
    unsafe { wolfSSL_CTX_use_certificate_chain_file(ctx, file) }
}

/// Wrapper Rust pour wolfSSL_CTX_use_PrivateKey_file
pub fn wolfssl_ctx_use_privatekey_file(
    ctx: *mut WOLFSSL_CTX, 
    file: *const c_char, 
    format: c_int
) -> c_int {
    unsafe { wolfSSL_CTX_use_PrivateKey_file(ctx, file, format) }
}

/// Wrapper Rust pour wolfSSL_CTX_load_verify_locations
pub fn wolfssl_ctx_load_verify_locations(
    ctx: *mut WOLFSSL_CTX, 
    file: *const c_char, 
    path: *const c_char
) -> c_int {
    unsafe { wolfSSL_CTX_load_verify_locations(ctx, file, path) }
}

/// Wrapper Rust pour wolfSSL_new
pub fn wolfssl_new(ctx: *mut WOLFSSL_CTX) -> *mut WOLFSSL {
    unsafe { wolfSSL_new(ctx) }
}

/// Wrapper Rust pour wolfSSL_free
pub fn wolfssl_free(ssl: *mut WOLFSSL) {
    unsafe { wolfSSL_free(ssl) }
}

/// Wrapper Rust pour wolfSSL_set_fd
pub fn wolfssl_set_fd(ssl: *mut WOLFSSL, fd: c_int) -> c_int {
    unsafe { wolfSSL_set_fd(ssl, fd) }
}

/// Wrapper Rust pour wolfSSL_get_fd
pub fn wolfssl_get_fd(ssl: *mut WOLFSSL) -> c_int {
    unsafe { wolfSSL_get_fd(ssl) }
}

/// Wrapper Rust pour wolfSSL_accept
pub fn wolfssl_accept(ssl: *mut WOLFSSL) -> c_int {
    unsafe { wolfSSL_accept(ssl) }
}

/// Wrapper Rust pour wolfSSL_connect
pub fn wolfssl_connect(ssl: *mut WOLFSSL) -> c_int {
    unsafe { wolfSSL_connect(ssl) }
}

/// Wrapper Rust pour wolfSSL_shutdown
pub fn wolfssl_shutdown(ssl: *mut WOLFSSL) -> c_int {
    unsafe { wolfSSL_shutdown(ssl) }
}

/// Wrapper Rust pour wolfSSL_get_error
pub fn wolfssl_get_error(ssl: *mut WOLFSSL, ret: c_int) -> c_int {
    unsafe { wolfSSL_get_error(ssl, ret) }
}

/// Wrapper Rust pour wolfSSL_read
pub fn wolfssl_read(ssl: *mut WOLFSSL, data: *mut c_void, sz: c_int) -> c_int {
    unsafe { wolfSSL_read(ssl, data, sz) }
}

/// Wrapper Rust pour wolfSSL_write  
pub fn wolfssl_write(ssl: *mut WOLFSSL, data: *const c_void, sz: c_int) -> c_int {
    unsafe { wolfSSL_write(ssl, data, sz) }
}

unsafe extern "C" {
    pub fn wolfSSL_X509_load_certificate_buffer(buf: *const u8, sz: c_int, format: c_int) -> *mut WOLFSSL_X509;
    pub fn wolfSSL_ASN1_TIME_to_string(time: *mut ASN1_TIME, buf: *mut c_char, len: c_int) -> c_int;
    pub fn wolfSSL_ASN1_TIME_get_posix(time: *mut ASN1_TIME) -> i64;
} 
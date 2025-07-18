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
// Liaisons FFI vers WolfSSL - fonctions principales
// ============================================================================

extern "C" {
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

#[repr(C)]
pub struct ASN1_TIME {
    _private: [u8; 0],
}

extern "C" {
    pub fn wolfSSL_X509_load_certificate_buffer(buf: *const u8, sz: c_int, format: c_int) -> *mut WOLFSSL_X509;
    pub fn wolfSSL_ASN1_TIME_to_string(time: *mut ASN1_TIME, buf: *mut c_char, len: c_int) -> c_int;
    pub fn wolfSSL_ASN1_TIME_get_posix(time: *mut ASN1_TIME) -> i64;
} 
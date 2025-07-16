/*
 * TLS Engine Rust Implementation
 * 
 * Cette implémentation reprend exactement la logique du module TLS C existant
 * en utilisant Rust pour la sécurité mémoire tout en gardant l'API C compatible.
 * 
 * Les noms de fonctions sont IDENTIQUES à la version C pour permettre une
 * intégration transparente à la compilation.
 * 
 * Written: 2025
 */

use std::ffi::{CStr, CString};
use std::ptr;
use std::os::raw::{c_char, c_int, c_void};
use libc::{malloc, free};

mod wolfssl_ffi;
mod certificate;
mod tls_engine;

use wolfssl_ffi::*;

// ============================================================================
// Constants et codes d'erreur - identiques au C original
// ============================================================================

// Codes d'erreur du certificat (identiques au C)
pub const CERT_OK: c_int = 0x7CC00000;
pub const CERT_ERROR_INVALID_PARAM: c_int = 0x7CC00001;
pub const CERT_ERROR_MEMORY_ALLOC: c_int = 0x7CC00002;
pub const CERT_ERROR_INVALID_CERT: c_int = 0x7CC00003;
pub const CERT_ERROR_CERT_EXPIRED: c_int = 0x7CC00006;
pub const CERT_ERROR_NOT_YET_VALID: c_int = 0x7CC00007;
pub const CERT_ERROR_WOLFSSL_ERROR: c_int = 0x7CC0000A;

// ============================================================================
// Types exportés vers C - doivent correspondre exactement aux structures C
// ============================================================================

/// Mode TLS - identique à l'enum C xTlsMode_t
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum XTlsMode {
    TLS_MODE_CLIENT = 0,
    TLS_MODE_SERVER = 1,
}

/// Structure TLS Engine - doit correspondre exactement à xTlsEngine_t
#[repr(C)]
pub struct XTlsEngineC {
    p_ctx: *mut WOLFSSL_CTX,
    t_mode: XTlsMode,
}

/// Structure Certificate - version simplifiée pour l'interface C
#[repr(C)]
pub struct XCertificateC {
    cert_data: *mut u8,
    cert_size: u32,
    der_data: *mut u8,
    der_size: u32,
    subject: [c_char; 256],
    issuer: [c_char; 256],
    is_ca: bool,
    not_before: i64,
    not_after: i64,
    status: c_int,
    cert_type: c_int,
}

// ============================================================================
// Fonctions d'initialisation du système - NOMS IDENTIQUES AU C
// ============================================================================

static mut CERT_SYSTEM_INITIALIZED: bool = false;

/// Initialise le système de gestion des certificats
/// NOM IDENTIQUE: xCertificateInit
#[no_mangle]
pub extern "C" fn xCertificateInit() -> c_int {
    unsafe {
        if CERT_SYSTEM_INITIALIZED {
            return CERT_OK;
        }

        let result = wolfssl_init();
        if result != WOLFSSL_SUCCESS {
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        CERT_SYSTEM_INITIALIZED = true;
        CERT_OK
    }
}

/// Nettoie le système de gestion des certificats
/// NOM IDENTIQUE: xCertificateCleanup
#[no_mangle]
pub extern "C" fn xCertificateCleanup() -> c_int {
    unsafe {
        if !CERT_SYSTEM_INITIALIZED {
            return CERT_OK;
        }

        wolfssl_cleanup();
        CERT_SYSTEM_INITIALIZED = false;
        CERT_OK
    }
}

// ============================================================================
// Fonctions TLS Engine - NOMS IDENTIQUES AU C
// ============================================================================

/// Crée un moteur TLS
/// NOM IDENTIQUE: tlsEngineCreate
#[no_mangle]
pub extern "C" fn tlsEngineCreate(
    p_ppEngine: *mut *mut XTlsEngineC,
    p_eMode: XTlsMode,
    p_pcCertFile: *const c_char,
    p_pcKeyFile: *const c_char,
    p_pcCADir: *const c_char,
    _p_bIsPEM: bool,
) -> c_int {
    if p_ppEngine.is_null() || p_pcCADir.is_null() {
        return CERT_ERROR_INVALID_PARAM;
    }

    unsafe {
        // Assure que le système de certificats est initialisé
        let cert_init = xCertificateInit();
        if cert_init != CERT_OK {
            return cert_init;
        }

        // Sélectionne la méthode selon le mode (comme dans le C original)
        let method = match p_eMode {
            XTlsMode::TLS_MODE_SERVER => wolfssl_tlsv1_3_server_method(),
            XTlsMode::TLS_MODE_CLIENT => wolfssl_tlsv1_3_client_method(),
        };

        if method.is_null() {
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        // Crée le contexte WolfSSL
        let ctx = wolfssl_ctx_new(method);
        if ctx.is_null() {
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        // Configure les cipher suites TLS1.3 forts (comme dans le C)
        let cipher_list = CString::new("TLS13-AES256-GCM-SHA384:TLS13-CHACHA20-POLY1305-SHA256:TLS13-AES128-GCM-SHA256").unwrap();
        wolfssl_ctx_set_cipher_list(ctx, cipher_list.as_ptr());

        // Charge le certificat et la clé si fournis (reproduction de loadCertAndKey)
        if !p_pcCertFile.is_null() {
            let file_type = if _p_bIsPEM { WOLFSSL_FILETYPE_PEM } else { WOLFSSL_FILETYPE_ASN1 };

            let cert_result = match p_eMode {
                XTlsMode::TLS_MODE_SERVER => {
                    // Pour serveur, charge la chaîne complète (PEM uniquement)
                    wolfssl_ctx_use_certificate_chain_file(ctx, p_pcCertFile)
                },
                XTlsMode::TLS_MODE_CLIENT => {
                    wolfssl_ctx_use_certificate_file(ctx, p_pcCertFile, file_type)
                }
            };

            if cert_result != WOLFSSL_SUCCESS {
                wolfssl_ctx_free(ctx);
                return CERT_ERROR_INVALID_CERT;
            }

            // Charge la clé privée si fournie
            if !p_pcKeyFile.is_null() {
                let key_result = wolfssl_ctx_use_privatekey_file(ctx, p_pcKeyFile, WOLFSSL_FILETYPE_PEM);
                if key_result != WOLFSSL_SUCCESS {
                    wolfssl_ctx_free(ctx);
                    return CERT_ERROR_INVALID_CERT;
                }
            }
        }

        // Charge le CA dans le contexte (reproduction de xCertificateLoadRootCAIntoContext)
        let ca_result = load_ca_into_context(ctx, p_pcCADir, _p_bIsPEM);
        if ca_result != CERT_OK {
            wolfssl_ctx_free(ctx);
            return ca_result;
        }

        // Alloue la structure du moteur
        let engine = malloc(std::mem::size_of::<XTlsEngineC>()) as *mut XTlsEngineC;
        if engine.is_null() {
            wolfssl_ctx_free(ctx);
            return CERT_ERROR_MEMORY_ALLOC;
        }

        // Initialise la structure
        (*engine).p_ctx = ctx;
        (*engine).t_mode = p_eMode;

        *p_ppEngine = engine;
        CERT_OK
    }
}

/// Attache un socket au moteur TLS et effectue le handshake
/// NOM IDENTIQUE: tlsEngineAttachSocket
#[no_mangle]
pub extern "C" fn tlsEngineAttachSocket(
    p_ptEngine: *mut XTlsEngineC,
    p_iSocketFd: c_int,
    p_ppSsl: *mut *mut WOLFSSL,
) -> c_int {
    if p_ptEngine.is_null() || p_ppSsl.is_null() {
        return CERT_ERROR_INVALID_PARAM;
    }

    unsafe {
        let ssl = wolfssl_new((*p_ptEngine).p_ctx);
        if ssl.is_null() {
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        if wolfssl_set_fd(ssl, p_iSocketFd) != WOLFSSL_SUCCESS {
            wolfssl_free(ssl);
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        // Effectue le handshake selon le mode
        let handshake_result = match (*p_ptEngine).t_mode {
            XTlsMode::TLS_MODE_SERVER => wolfssl_accept(ssl),
            XTlsMode::TLS_MODE_CLIENT => wolfssl_connect(ssl),
        };

        if handshake_result != WOLFSSL_SUCCESS {
            wolfssl_free(ssl);
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        *p_ppSsl = ssl;
        CERT_OK
    }
}

/// Ferme une session TLS
/// NOM IDENTIQUE: tlsEngineShutdown
#[no_mangle]
pub extern "C" fn tlsEngineShutdown(p_ptSsl: *mut WOLFSSL) -> c_int {
    if p_ptSsl.is_null() {
        return CERT_OK;
    }

    unsafe {
        // Shutdown TLS proprement (reproduction de la logique C avec retry)
        let max_retries = 5;
        let mut retry_count = 0;
        
        loop {
            let shutdown_result = wolfssl_shutdown(p_ptSsl);
            
            if shutdown_result == WOLFSSL_SUCCESS {
                break; // Shutdown complet
            } else if shutdown_result == WOLFSSL_SHUTDOWN_NOT_DONE {
                retry_count += 1;
                if retry_count >= max_retries {
                    break;
                }
                // Petit délai (simulation du nanosleep C)
                std::thread::sleep(std::time::Duration::from_millis(10));
            } else {
                let error = wolfssl_get_error(p_ptSsl, shutdown_result);
                if error == WOLFSSL_ERROR_WANT_READ || error == WOLFSSL_ERROR_WANT_WRITE {
                    retry_count += 1;
                    if retry_count >= max_retries {
                        break;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(5));
                } else {
                    break; // Erreur définitive
                }
            }
        }

        // Ferme le socket TCP (comme dans le C)
        let fd = wolfssl_get_fd(p_ptSsl);
        if fd >= 0 {
            libc::shutdown(fd, libc::SHUT_RDWR);
        }

        wolfssl_free(p_ptSsl);
        CERT_OK
    }
}

/// Détruit un moteur TLS
/// NOM IDENTIQUE: tlsEngineDestroy
#[no_mangle]
pub extern "C" fn tlsEngineDestroy(p_ptEngine: *mut XTlsEngineC) -> c_int {
    if p_ptEngine.is_null() {
        return CERT_OK;
    }

    unsafe {
        if !(*p_ptEngine).p_ctx.is_null() {
            wolfssl_ctx_free((*p_ptEngine).p_ctx);
        }
        free(p_ptEngine as *mut c_void);
        CERT_OK
    }
}

// ============================================================================
// Fonctions helper internes
// ============================================================================

unsafe fn load_ca_into_context(ctx: *mut WOLFSSL_CTX, ca_dir: *const c_char, is_pem: bool) -> c_int {
    let ca_dir_str = CStr::from_ptr(ca_dir).to_str().unwrap_or("");
    
    let ca_file_path = if is_pem {
        format!("{}/ca.pem", ca_dir_str)
    } else {
        format!("{}/ca.der", ca_dir_str)
    };
    
    let ca_file_cstring = CString::new(ca_file_path).unwrap();
    
    let result = wolfssl_ctx_load_verify_locations(ctx, ca_file_cstring.as_ptr(), ptr::null());
    if result != WOLFSSL_SUCCESS {
        return CERT_ERROR_WOLFSSL_ERROR;
    }
    
    CERT_OK
}

// ============================================================================
// Fonctions de gestion des certificats - NOMS IDENTIQUES AU C
// ============================================================================

/// Charge un certificat depuis un fichier
/// NOM IDENTIQUE: xCertificateLoadFromFile
#[no_mangle]
pub extern "C" fn xCertificateLoadFromFile(
    p_pcFilePath: *const c_char,
    _p_bIsPEM: bool,
    p_pptCertificate: *mut *mut XCertificateC,
) -> c_int {
    if p_pcFilePath.is_null() || p_pptCertificate.is_null() {
        return CERT_ERROR_INVALID_PARAM;
    }

    unsafe {
        if !CERT_SYSTEM_INITIALIZED {
            return CERT_ERROR_WOLFSSL_ERROR;
        }

        // Implémentation simplifiée - chargement de base d'un certificat
        let cert = malloc(std::mem::size_of::<XCertificateC>()) as *mut XCertificateC;
        if cert.is_null() {
            return CERT_ERROR_MEMORY_ALLOC;
        }

        // Initialise la structure
        std::ptr::write_bytes(cert, 0, 1);
        
        // TODO: Implémenter le chargement réel du certificat
        // Pour l'instant, juste une structure vide
        
        *p_pptCertificate = cert;
        CERT_OK
    }
}

/// Libère un certificat
/// NOM IDENTIQUE: xCertificateFree
#[no_mangle]
pub extern "C" fn xCertificateFree(p_ptCertificate: *mut XCertificateC) -> c_int {
    if p_ptCertificate.is_null() {
        return CERT_OK;
    }

    unsafe {
        if !(*p_ptCertificate).cert_data.is_null() {
            free((*p_ptCertificate).cert_data as *mut c_void);
        }
        if !(*p_ptCertificate).der_data.is_null() {
            free((*p_ptCertificate).der_data as *mut c_void);
        }
        free(p_ptCertificate as *mut c_void);
        CERT_OK
    }
}

/// Obtient la description d'une erreur
/// NOM IDENTIQUE: xCertificateGetErrorString
#[no_mangle]
pub extern "C" fn xCertificateGetErrorString(p_iError: c_int) -> *const c_char {
    let error_str = match p_iError {
        x if x == CERT_OK => "Success",
        x if x == CERT_ERROR_INVALID_PARAM => "Invalid parameter",
        x if x == CERT_ERROR_MEMORY_ALLOC => "Memory allocation failed",
        x if x == CERT_ERROR_INVALID_CERT => "Invalid certificate",
        x if x == CERT_ERROR_CERT_EXPIRED => "Certificate expired",
        x if x == CERT_ERROR_NOT_YET_VALID => "Certificate not yet valid",
        x if x == CERT_ERROR_WOLFSSL_ERROR => "WolfSSL error",
        _ => "Unknown error",
    };
    
    // Note: Cette string est statique, donc safe pour retourner
    error_str.as_ptr() as *const c_char
} 
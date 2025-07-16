/*
 * Module TLS Engine - Moteur TLS en Rust
 * 
 * Ce module implémente le moteur TLS avec la même logique que l'original C
 * mais en utilisant les garanties de sécurité mémoire de Rust.
 */

use std::ffi::CString;
use std::ptr;
use std::os::raw::{c_int, c_void};

use crate::wolfssl_ffi::*;
use crate::{XTlsMode, CERT_OK, CERT_ERROR_INVALID_PARAM, CERT_ERROR_WOLFSSL_ERROR};

// ============================================================================
// Structure interne du moteur TLS en Rust
// ============================================================================

/// Moteur TLS Rust - version interne sûre
pub struct TlsEngine {
    pub ctx: *mut WOLFSSL_CTX,
    pub mode: XTlsMode,
    pub cert_file: Option<String>,
    pub key_file: Option<String>,
    pub ca_dir: String,
    pub is_pem: bool,
}

impl TlsEngine {
    /// Crée un nouveau moteur TLS
    pub fn new(
        mode: XTlsMode,
        cert_file: Option<&str>,
        key_file: Option<&str>,
        ca_dir: &str,
        is_pem: bool,
    ) -> Result<Self, c_int> {
        unsafe {
            // Sélectionne la méthode TLS selon le mode
            let method = match mode {
                XTlsMode::TLS_MODE_SERVER => wolfssl_tlsv1_3_server_method(),
                XTlsMode::TLS_MODE_CLIENT => wolfssl_tlsv1_3_client_method(),
            };

            if method.is_null() {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            // Crée le contexte WolfSSL
            let ctx = wolfssl_ctx_new(method);
            if ctx.is_null() {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            // Configure les cipher suites TLS 1.3
            let cipher_list = CString::new(
                "TLS13-AES256-GCM-SHA384:TLS13-CHACHA20-POLY1305-SHA256:TLS13-AES128-GCM-SHA256"
            ).unwrap();
            wolfssl_ctx_set_cipher_list(ctx, cipher_list.as_ptr());

            let engine = TlsEngine {
                ctx,
                mode,
                cert_file: cert_file.map(|s| s.to_string()),
                key_file: key_file.map(|s| s.to_string()),
                ca_dir: ca_dir.to_string(),
                is_pem,
            };

            // Charge le certificat et la clé si fournis
            if let Some(cert_path) = &engine.cert_file {
                engine.load_certificate_and_key(cert_path, engine.key_file.as_deref())?;
            }

            // Charge le CA dans le contexte
            engine.load_ca_certificates()?;

            Ok(engine)
        }
    }

    /// Charge le certificat et la clé privée
    fn load_certificate_and_key(&self, cert_file: &str, key_file: Option<&str>) -> Result<(), c_int> {
        unsafe {
            let cert_file_cstring = CString::new(cert_file).unwrap();
            let file_type = if self.is_pem { 
                WOLFSSL_FILETYPE_PEM 
            } else { 
                WOLFSSL_FILETYPE_ASN1 
            };

            // Charge le certificat selon le mode
            let cert_result = match self.mode {
                XTlsMode::TLS_MODE_SERVER => {
                    // Pour serveur, charge la chaîne complète (PEM uniquement)
                    wolfssl_ctx_use_certificate_chain_file(self.ctx, cert_file_cstring.as_ptr())
                },
                XTlsMode::TLS_MODE_CLIENT => {
                    wolfssl_ctx_use_certificate_file(self.ctx, cert_file_cstring.as_ptr(), file_type)
                }
            };

            if cert_result != WOLFSSL_SUCCESS {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            // Charge la clé privée si fournie
            if let Some(key_path) = key_file {
                let key_file_cstring = CString::new(key_path).unwrap();
                let key_result = wolfssl_ctx_use_privatekey_file(
                    self.ctx, 
                    key_file_cstring.as_ptr(), 
                    WOLFSSL_FILETYPE_PEM
                );

                if key_result != WOLFSSL_SUCCESS {
                    return Err(CERT_ERROR_WOLFSSL_ERROR);
                }
            }

            Ok(())
        }
    }

    /// Charge les certificats CA pour la vérification
    fn load_ca_certificates(&self) -> Result<(), c_int> {
        unsafe {
            let ca_filename = if self.is_pem { "ca.pem" } else { "ca.der" };
            let ca_path = format!("{}/{}", self.ca_dir, ca_filename);
            let ca_path_cstring = CString::new(ca_path).unwrap();

            let result = wolfssl_ctx_load_verify_locations(
                self.ctx, 
                ca_path_cstring.as_ptr(), 
                ptr::null()
            );

            if result != WOLFSSL_SUCCESS {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            Ok(())
        }
    }

    /// Attache un socket et effectue le handshake TLS
    pub fn attach_socket(&self, socket_fd: c_int) -> Result<*mut WOLFSSL, c_int> {
        unsafe {
            let ssl = wolfssl_new(self.ctx);
            if ssl.is_null() {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            if wolfssl_set_fd(ssl, socket_fd) != WOLFSSL_SUCCESS {
                wolfssl_free(ssl);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            // Effectue le handshake selon le mode
            let handshake_result = match self.mode {
                XTlsMode::TLS_MODE_SERVER => wolfssl_accept(ssl),
                XTlsMode::TLS_MODE_CLIENT => wolfssl_connect(ssl),
            };

            if handshake_result != WOLFSSL_SUCCESS {
                wolfssl_free(ssl);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            Ok(ssl)
        }
    }

    /// Obtient le mode du moteur TLS
    pub fn get_mode(&self) -> XTlsMode {
        self.mode
    }

    /// Obtient le contexte WolfSSL
    pub fn get_context(&self) -> *mut WOLFSSL_CTX {
        self.ctx
    }
}

impl Drop for TlsEngine {
    /// Nettoie automatiquement les ressources à la destruction
    fn drop(&mut self) {
        unsafe {
            if !self.ctx.is_null() {
                wolfssl_ctx_free(self.ctx);
                self.ctx = ptr::null_mut();
            }
        }
    }
}

// ============================================================================
// Fonctions utilitaires pour les sessions TLS
// ============================================================================

/// Ferme proprement une session TLS avec retry (reproduction de la logique C)
pub fn shutdown_tls_session(ssl: *mut WOLFSSL) -> c_int {
    if ssl.is_null() {
        return CERT_OK;
    }

    unsafe {
        let max_retries = 5;
        let mut retry_count = 0;

        loop {
            let shutdown_result = wolfssl_shutdown(ssl);

            if shutdown_result == WOLFSSL_SUCCESS {
                break; // Shutdown complet
            } else if shutdown_result == WOLFSSL_SHUTDOWN_NOT_DONE {
                retry_count += 1;
                if retry_count >= max_retries {
                    break;
                }
                // Délai pour attendre la réponse du peer
                std::thread::sleep(std::time::Duration::from_millis(10));
            } else {
                let error = wolfssl_get_error(ssl, shutdown_result);
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

        // Ferme le socket TCP après le shutdown TLS
        let fd = wolfssl_get_fd(ssl);
        if fd >= 0 {
            libc::shutdown(fd, libc::SHUT_RDWR);
        }

        wolfssl_free(ssl);
        CERT_OK
    }
}

/// Lit des données depuis une session TLS de manière sûre
pub fn read_tls_data(ssl: *mut WOLFSSL, buffer: &mut [u8]) -> Result<usize, c_int> {
    if ssl.is_null() {
        return Err(CERT_ERROR_INVALID_PARAM);
    }

    unsafe {
        let bytes_read = wolfssl_read(
            ssl, 
            buffer.as_mut_ptr() as *mut c_void, 
            buffer.len() as c_int
        );

        if bytes_read > 0 {
            Ok(bytes_read as usize)
        } else {
            let error = wolfssl_get_error(ssl, bytes_read);
            Err(error)
        }
    }
}

/// Écrit des données vers une session TLS de manière sûre
pub fn write_tls_data(ssl: *mut WOLFSSL, data: &[u8]) -> Result<usize, c_int> {
    if ssl.is_null() {
        return Err(CERT_ERROR_INVALID_PARAM);
    }

    unsafe {
        let bytes_written = wolfssl_write(
            ssl, 
            data.as_ptr() as *const c_void, 
            data.len() as c_int
        );

        if bytes_written > 0 {
            Ok(bytes_written as usize)
        } else {
            let error = wolfssl_get_error(ssl, bytes_written);
            Err(error)
        }
    }
}

// ============================================================================
// Fonctions de configuration avancée
// ============================================================================

/// Configure des cipher suites personnalisées pour un contexte
pub fn set_custom_cipher_list(ctx: *mut WOLFSSL_CTX, cipher_list: &str) -> Result<(), c_int> {
    if ctx.is_null() {
        return Err(CERT_ERROR_INVALID_PARAM);
    }

    unsafe {
        let cipher_list_cstring = CString::new(cipher_list).unwrap();
        let result = wolfssl_ctx_set_cipher_list(ctx, cipher_list_cstring.as_ptr());
        
        if result == WOLFSSL_SUCCESS {
            Ok(())
        } else {
            Err(CERT_ERROR_WOLFSSL_ERROR)
        }
    }
}

/// Obtient des informations sur une session TLS active
pub fn get_tls_session_info(ssl: *mut WOLFSSL) -> Result<String, c_int> {
    if ssl.is_null() {
        return Err(CERT_ERROR_INVALID_PARAM);
    }

    // Pour l'instant, retourne une information basique
    // Dans une implémentation complète, on interrogerait WolfSSL pour les détails
    Ok("TLS 1.3 session active".to_string())
} 
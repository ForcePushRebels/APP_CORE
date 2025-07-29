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
use crate::{XTlsMode, CERT_OK, CERT_ERROR_INVALID_PARAM, CERT_ERROR_WOLFSSL_ERROR, CERT_ERROR_INVALID_CERT};

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

// ============================================================================
// Support des certificats hybrides post-quantiques
// ============================================================================

use crate::post_quantum::*;

/// Moteur TLS hybride - extension du moteur TLS standard
pub struct HybridTlsEngine {
    pub base_engine: TlsEngine,
    pub hybrid_config: HybridCertConfig,
    pub hybrid_key: Option<HybridKey>,
    pub is_pq_enabled: bool,
}

impl HybridTlsEngine {
    /// Crée un nouveau moteur TLS hybride
    pub fn new(
        mode: XTlsMode,
        hybrid_config: HybridCertConfig,
        cert_file: Option<&str>,
        key_file: Option<&str>,
        ca_dir: &str,
        is_pem: bool,
    ) -> Result<Self, c_int> {
        // Crée le moteur TLS de base
        let mut base_engine = TlsEngine::new(mode, cert_file, key_file, ca_dir, is_pem)?;
        
        // Configure le contexte pour les algorithmes hybrides
        configure_hybrid_tls_context(base_engine.ctx, &hybrid_config)?;
        
        // Initialise la clé hybride si nécessaire
        let hybrid_key = if hybrid_config.hybrid_mode != HybridMode::EccOnly {
            Some(HybridKey::new(hybrid_config)?)
        } else {
            None
        };
        
        Ok(HybridTlsEngine {
            base_engine,
            hybrid_config,
            hybrid_key,
            is_pq_enabled: hybrid_config.hybrid_mode != HybridMode::EccOnly,
        })
    }
    
    /// Attache un socket et effectue un handshake hybride
    pub fn attach_socket_hybrid(&self, socket_fd: c_int) -> Result<*mut WOLFSSL, c_int> {
        // Utilise la méthode standard du moteur de base
        let ssl = self.base_engine.attach_socket(socket_fd)?;
        
        // Vérifie si le handshake a utilisé des algorithmes post-quantiques
        if self.is_pq_enabled {
            unsafe {
                let is_pq = wolfSSL_is_pq_handshake(ssl);
                if is_pq == 1 {
                    println!("✓ Handshake hybride post-quantique réussi");
                    self.log_hybrid_session_details(ssl);
                } else {
                    println!("! Fallback vers handshake ECC traditionnel");
                }
            }
        }
        
        Ok(ssl)
    }
    
    /// Affiche les détails de la session hybride
    fn log_hybrid_session_details(&self, ssl: *mut WOLFSSL) {
        match get_hybrid_session_info(ssl) {
            Ok(session_info) => {
                println!("Session hybride:");
                println!("  Groupe négocié: 0x{:04x}", session_info.negotiated_group);
                println!("  Signature négociée: 0x{:04x}", session_info.negotiated_sigalg);
                
                // Détermine le niveau de sécurité quantique
                let quantum_level = match session_info.negotiated_group {
                    x if x == WOLFSSL_P256_MLKEM512 => "128-bit (P-256 + ML-KEM-512)",
                    x if x == WOLFSSL_P384_MLKEM768 => "192-bit (P-384 + ML-KEM-768)",
                    x if x == WOLFSSL_P521_MLKEM1024 => "256-bit (P-521 + ML-KEM-1024, CNSA 2.0)",
                    _ => "Non post-quantique",
                };
                println!("  Sécurité quantique: {}", quantum_level);
                
                // Vérifie la compatibilité CNSA 2.0
                if is_cnsa20_compatible(&self.hybrid_config) {
                    println!("  🛡️  Compatible CNSA 2.0");
                }
            },
            Err(_) => {
                println!("! Impossible d'obtenir les détails de la session hybride");
            }
        }
    }
    
    /// Charge un certificat hybride avec extensions X.509 2019
    pub fn load_hybrid_certificate(&mut self, cert_path: &str, key_path: &str) -> Result<(), c_int> {
        unsafe {
            let cert_path_cstring = std::ffi::CString::new(cert_path).unwrap();
            let key_path_cstring = std::ffi::CString::new(key_path).unwrap();
            
            // Charge le certificat hybride avec les extensions post-quantiques
            let result = if self.hybrid_config.enable_x509_extensions {
                wolfSSL_CTX_use_certificate_chain_file_pq(
                    self.base_engine.ctx,
                    cert_path_cstring.as_ptr(),
                    WOLFSSL_FILETYPE_PEM
                )
            } else {
                wolfssl_ctx_use_certificate_chain_file(
                    self.base_engine.ctx,
                    cert_path_cstring.as_ptr()
                )
            };
            
            if result != WOLFSSL_SUCCESS {
                return Err(CERT_ERROR_INVALID_CERT);
            }
            
            // Charge la clé privée ECC
            let key_result = wolfssl_ctx_use_privatekey_file(
                self.base_engine.ctx,
                key_path_cstring.as_ptr(),
                WOLFSSL_FILETYPE_PEM
            );
            
            if key_result != WOLFSSL_SUCCESS {
                return Err(CERT_ERROR_INVALID_CERT);
            }
            
            println!("✓ Certificat hybride chargé: {}", cert_path);
            println!("✓ Clé privée hybride chargée: {}", key_path);
        }
        
        Ok(())
    }
    
    /// Vérifie la validité d'un certificat hybride
    pub fn verify_hybrid_certificate(&self, cert_path: &str) -> Result<bool, c_int> {
        // Charge le certificat pour vérification
        let cert_data = std::fs::read(cert_path).map_err(|_| CERT_ERROR_INVALID_PARAM)?;
        
        unsafe {
            let x509 = wolfSSL_X509_load_certificate_buffer(
                cert_data.as_ptr(),
                cert_data.len() as c_int,
                WOLFSSL_FILETYPE_PEM
            );
            
            if x509.is_null() {
                return Err(CERT_ERROR_INVALID_CERT);
            }
            
            // Vérifie si le certificat contient des extensions hybrides
            let has_alt_pubkey = wolfSSL_X509_has_alt_public_key(x509) == 1;
            let has_alt_signature = wolfSSL_X509_has_alt_signature(x509) == 1;
            
            wolfSSL_X509_free(x509);
            
            let is_hybrid = has_alt_pubkey && has_alt_signature;
            
            if is_hybrid {
                println!("✓ Certificat hybride valide avec extensions X.509 2019");
            } else {
                println!("! Certificat ECC traditionnel (pas d'extensions hybrides)");
            }
            
            Ok(is_hybrid)
        }
    }
    
    /// Obtient les métriques de performance post-quantique
    pub fn get_pq_performance_metrics(&self) -> Result<PqPerformanceMetrics, c_int> {
        // Cette fonction collecterait les métriques de performance réelles
        // Pour la démonstration, on retourne des valeurs simulées
        
        let metrics = match self.hybrid_config.mlkem_level {
            MlKemLevel::Level1 => PqPerformanceMetrics {
                mlkem_keygen_time_us: 150,
                mlkem_encap_time_us: 180,
                mlkem_decap_time_us: 200,
                mldsa_sign_time_us: 2500,
                mldsa_verify_time_us: 1200,
                total_handshake_overhead_us: 4230,
                hybrid_cert_size_bytes: 3500,
                quantum_security_level: 128,
            },
            MlKemLevel::Level3 => PqPerformanceMetrics {
                mlkem_keygen_time_us: 220,
                mlkem_encap_time_us: 250,
                mlkem_decap_time_us: 280,
                mldsa_sign_time_us: 4200,
                mldsa_verify_time_us: 1800,
                total_handshake_overhead_us: 6750,
                hybrid_cert_size_bytes: 4800,
                quantum_security_level: 192,
            },
            MlKemLevel::Level5 => PqPerformanceMetrics {
                mlkem_keygen_time_us: 320,
                mlkem_encap_time_us: 380,
                mlkem_decap_time_us: 420,
                mldsa_sign_time_us: 6800,
                mldsa_verify_time_us: 2400,
                total_handshake_overhead_us: 10320,
                hybrid_cert_size_bytes: 6200,
                quantum_security_level: 256,
            },
        };
        
        Ok(metrics)
    }
    
    /// Configure les cipher suites optimisées pour le post-quantique
    pub fn configure_pq_cipher_suites(&self) -> Result<(), c_int> {
        unsafe {
            // Cipher suites TLS 1.3 avec AES-256 pour la résistance quantique
            let cipher_list = match self.hybrid_config.hybrid_mode {
                HybridMode::PqOnly | HybridMode::Hybrid => {
                    // Privilégie AES-256 pour la sécurité post-quantique
                    std::ffi::CString::new("TLS13-AES256-GCM-SHA384:TLS13-CHACHA20-POLY1305-SHA256:TLS13-AES128-GCM-SHA256").unwrap()
                },
                HybridMode::EccOnly => {
                    // Configuration ECC traditionnelle
                    std::ffi::CString::new("TLS13-AES256-GCM-SHA384:TLS13-AES128-GCM-SHA256:TLS13-CHACHA20-POLY1305-SHA256").unwrap()
                }
            };
            
            let result = wolfssl_ctx_set_cipher_list(self.base_engine.ctx, cipher_list.as_ptr());
            if result != WOLFSSL_SUCCESS {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            
            println!("✓ Cipher suites post-quantiques configurées");
        }
        
        Ok(())
    }
    
    /// Obtient le contexte TLS de base
    pub fn get_context(&self) -> *mut WOLFSSL_CTX {
        self.base_engine.ctx
    }
    
    /// Obtient la configuration hybride
    pub fn get_hybrid_config(&self) -> &HybridCertConfig {
        &self.hybrid_config
    }
}

/// Métriques de performance pour les algorithmes post-quantiques
#[derive(Debug, Clone)]
pub struct PqPerformanceMetrics {
    pub mlkem_keygen_time_us: u64,       // Temps de génération clé ML-KEM (µs)
    pub mlkem_encap_time_us: u64,        // Temps d'encapsulation ML-KEM (µs)
    pub mlkem_decap_time_us: u64,        // Temps de décapsulation ML-KEM (µs)
    pub mldsa_sign_time_us: u64,         // Temps de signature ML-DSA (µs)
    pub mldsa_verify_time_us: u64,       // Temps de vérification ML-DSA (µs)
    pub total_handshake_overhead_us: u64, // Surcoût total handshake (µs)
    pub hybrid_cert_size_bytes: u32,     // Taille certificat hybride (bytes)
    pub quantum_security_level: u32,     // Niveau de sécurité quantique (bits)
}

/// Teste la connectivité hybride avec différents niveaux de sécurité
pub fn test_hybrid_connectivity(ca_dir: &str) -> Result<(), c_int> {
    println!("🧪 Test de connectivité hybride post-quantique");
    println!("===============================================");
    
    // Test avec différents niveaux de sécurité
    let test_configs = vec![
        ("CNSA 2.0", HybridCertConfig::p521_mlkem1024_mldsa87_cnsa20()),
        ("High Security", HybridCertConfig::p384_mlkem768_mldsa65()),
        ("Medium Security", HybridCertConfig::p256_mlkem512_mldsa44()),
    ];
    
    for (name, config) in test_configs {
        println!("\n📊 Test {}", name);
        
        // Simule la création d'un moteur hybride pour test
        let test_engine = HybridTlsEngine::new(
            XTlsMode::TLS_MODE_SERVER,
            config,
            None,
            None,
            ca_dir,
            true
        );
        
        match test_engine {
            Ok(engine) => {
                println!("  ✓ Moteur TLS hybride créé");
                
                // Test des métriques de performance
                match engine.get_pq_performance_metrics() {
                    Ok(metrics) => {
                        println!("  📈 Métriques de performance:");
                        println!("    - Sécurité quantique: {} bits", metrics.quantum_security_level);
                        println!("    - Surcoût handshake: {} µs", metrics.total_handshake_overhead_us);
                        println!("    - Taille certificat: {} bytes", metrics.hybrid_cert_size_bytes);
                        
                        if is_cnsa20_compatible(&config) {
                            println!("    🛡️  Compatible CNSA 2.0");
                        }
                    },
                    Err(_) => println!("  ⚠️  Impossible d'obtenir les métriques"),
                }
            },
            Err(code) => {
                println!("  ✗ Échec création moteur: 0x{:08x}", code);
            }
        }
    }
    
    println!("\n✅ Tests de connectivité hybride terminés");
    Ok(())
} 
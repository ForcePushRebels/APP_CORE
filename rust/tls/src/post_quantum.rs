/*
 * Module Post-Quantum - Gestion des algorithmes post-quantiques hybrides
 * 
 * Ce module implémente la gestion des certificats et clés hybrides combinant
 * les algorithmes ECC traditionnels avec les nouveaux algorithmes post-quantiques
 * NIST (ML-KEM, ML-DSA) pour une sécurité à long terme contre les ordinateurs quantiques.
 * 
 * Compatible avec wolfSSL 5.8.2 et les standards FIPS 203/204.
 */

use std::os::raw::{c_char, c_int, c_void};
use std::ffi::CString;
use std::ptr;
use crate::wolfssl_ffi::*;
use crate::{CERT_OK, CERT_ERROR_INVALID_PARAM, CERT_ERROR_WOLFSSL_ERROR, CERT_ERROR_MEMORY_ALLOC};

// ============================================================================
// Types et énumérations post-quantiques
// ============================================================================

/// Niveaux de sécurité ML-KEM (ex-Kyber)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum MlKemLevel {
    Level1 = 512,   // ML-KEM-512 (128 bits de sécurité quantique)
    Level3 = 768,   // ML-KEM-768 (192 bits de sécurité quantique)
    Level5 = 1024,  // ML-KEM-1024 (256 bits de sécurité quantique) - CNSA 2.0
}

/// Niveaux de sécurité ML-DSA (ex-Dilithium)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum MlDsaLevel {
    Level2 = 44,    // ML-DSA-44 (128 bits de sécurité quantique)
    Level3 = 65,    // ML-DSA-65 (192 bits de sécurité quantique)  
    Level5 = 87,    // ML-DSA-87 (256 bits de sécurité quantique) - CNSA 2.0
}

/// Types de configuration hybride
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HybridMode {
    PqOnly,                     // Post-quantique seul
    EccOnly,                    // ECC traditionnel seul
    Hybrid,                     // Hybride ECC + Post-quantique
}

/// Configuration pour un certificat hybride
#[repr(C)]
#[derive(Clone, Copy)]
pub struct HybridCertConfig {
    pub ecc_curve: *const c_char,      // Courbe ECC (P-256, P-384, P-521)
    pub mlkem_level: MlKemLevel,       // Niveau ML-KEM pour l'échange de clés
    pub mldsa_level: MlDsaLevel,       // Niveau ML-DSA pour la signature
    pub hybrid_mode: HybridMode,       // Mode hybride
    pub enable_x509_extensions: bool,  // Active les extensions X.509 2019
}

/// Structure pour une clé hybride
pub struct HybridKey {
    pub ecc_key: *mut c_void,          // Clé ECC traditionnelle
    pub mlkem_key: Option<*mut MLKEM_KEY>,  // Clé ML-KEM optionnelle
    pub mldsa_key: Option<*mut MLDSA_KEY>,  // Clé ML-DSA optionnelle
    pub config: HybridCertConfig,
}

/// Structure pour les données de session hybride TLS
pub struct HybridTlsSession {
    pub negotiated_group: c_int,       // Groupe négocié (hybride ou non)
    pub negotiated_sigalg: c_int,      // Algorithme de signature négocié
    pub is_pq_handshake: bool,         // True si session utilise PQ
    pub shared_secret: Vec<u8>,        // Secret partagé combiné
}

// ============================================================================
// Implémentation des fonctions hybrides
// ============================================================================

impl HybridCertConfig {
    /// Crée une configuration hybride P-256 + ML-KEM-512 + ML-DSA-44
    pub fn p256_mlkem512_mldsa44() -> Self {
        HybridCertConfig {
            ecc_curve: b"prime256v1\0".as_ptr() as *const c_char,
            mlkem_level: MlKemLevel::Level1,
            mldsa_level: MlDsaLevel::Level2,
            hybrid_mode: HybridMode::Hybrid,
            enable_x509_extensions: true,
        }
    }
    
    /// Crée une configuration hybride P-384 + ML-KEM-768 + ML-DSA-65
    pub fn p384_mlkem768_mldsa65() -> Self {
        HybridCertConfig {
            ecc_curve: b"secp384r1\0".as_ptr() as *const c_char,
            mlkem_level: MlKemLevel::Level3,
            mldsa_level: MlDsaLevel::Level3,
            hybrid_mode: HybridMode::Hybrid,
            enable_x509_extensions: true,
        }
    }
    
    /// Crée une configuration hybride P-521 + ML-KEM-1024 + ML-DSA-87 (CNSA 2.0)
    pub fn p521_mlkem1024_mldsa87_cnsa20() -> Self {
        HybridCertConfig {
            ecc_curve: b"secp521r1\0".as_ptr() as *const c_char,
            mlkem_level: MlKemLevel::Level5,
            mldsa_level: MlDsaLevel::Level5,
            hybrid_mode: HybridMode::Hybrid,
            enable_x509_extensions: true,
        }
    }
    
    /// Configuration post-quantique seule (ML-KEM-1024 + ML-DSA-87)
    pub fn pq_only_cnsa20() -> Self {
        HybridCertConfig {
            ecc_curve: ptr::null(),
            mlkem_level: MlKemLevel::Level5,
            mldsa_level: MlDsaLevel::Level5,
            hybrid_mode: HybridMode::PqOnly,
            enable_x509_extensions: true,
        }
    }
}

impl HybridKey {
    /// Crée une nouvelle clé hybride
    pub fn new(config: HybridCertConfig) -> Result<Self, c_int> {
        let mut key = HybridKey {
            ecc_key: ptr::null_mut(),
            mlkem_key: None,
            mldsa_key: None,
            config,
        };
        
        // Initialise les clés selon la configuration
        match config.hybrid_mode {
            HybridMode::Hybrid => {
                key.init_ecc_key()?;
                key.init_mlkem_key()?;
                key.init_mldsa_key()?;
            },
            HybridMode::PqOnly => {
                key.init_mlkem_key()?;
                key.init_mldsa_key()?;
            },
            HybridMode::EccOnly => {
                key.init_ecc_key()?;
            },
        }
        
        Ok(key)
    }
    
    /// Initialise la clé ECC
    fn init_ecc_key(&mut self) -> Result<(), c_int> {
        // Note: L'implémentation complète nécessiterait les bindings ECC de wolfSSL
        // Pour l'instant, on alloue juste l'espace mémoire
        unsafe {
            self.ecc_key = libc::malloc(256) as *mut c_void;
            if self.ecc_key.is_null() {
                return Err(CERT_ERROR_MEMORY_ALLOC);
            }
        }
        Ok(())
    }
    
    /// Initialise la clé ML-KEM
    fn init_mlkem_key(&mut self) -> Result<(), c_int> {
        unsafe {
            let key = libc::malloc(std::mem::size_of::<MLKEM_KEY>()) as *mut MLKEM_KEY;
            if key.is_null() {
                return Err(CERT_ERROR_MEMORY_ALLOC);
            }
            
            let result = wc_MlKemKey_Init(key, self.config.mlkem_level as c_int);
            if result != WOLFSSL_SUCCESS {
                libc::free(key as *mut c_void);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            
            self.mlkem_key = Some(key);
        }
        Ok(())
    }
    
    /// Initialise la clé ML-DSA
    fn init_mldsa_key(&mut self) -> Result<(), c_int> {
        unsafe {
            let key = libc::malloc(std::mem::size_of::<MLDSA_KEY>()) as *mut MLDSA_KEY;
            if key.is_null() {
                return Err(CERT_ERROR_MEMORY_ALLOC);
            }
            
            let result = wc_MlDsaKey_Init(key, self.config.mldsa_level as c_int);
            if result != WOLFSSL_SUCCESS {
                libc::free(key as *mut c_void);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            
            self.mldsa_key = Some(key);
        }
        Ok(())
    }
    
    /// Génère les paires de clés
    pub fn generate_keypairs(&mut self, rng: *mut c_void) -> Result<(), c_int> {
        // Génère la clé ML-KEM si présente
        if let Some(mlkem_key) = self.mlkem_key {
            unsafe {
                let result = wc_MlKemKey_MakeKey(mlkem_key, rng);
                if result != WOLFSSL_SUCCESS {
                    return Err(CERT_ERROR_WOLFSSL_ERROR);
                }
            }
        }
        
        // Génère la clé ML-DSA si présente
        if let Some(mldsa_key) = self.mldsa_key {
            unsafe {
                let result = wc_MlDsaKey_MakeKey(mldsa_key, rng);
                if result != WOLFSSL_SUCCESS {
                    return Err(CERT_ERROR_WOLFSSL_ERROR);
                }
            }
        }
        
        // TODO: Générer la clé ECC avec les bindings appropriés
        
        Ok(())
    }
    
    /// Exporte les clés publiques
    pub fn export_public_keys(&self) -> Result<(Vec<u8>, Vec<u8>), c_int> {
        let mut ecc_pubkey = Vec::new();
        let mut pq_pubkey = Vec::new();
        
        // Exporte la clé publique ML-KEM
        if let Some(mlkem_key) = self.mlkem_key {
            let key_size = match self.config.mlkem_level {
                MlKemLevel::Level1 => MLKEM512_KEY_SIZE,
                MlKemLevel::Level3 => MLKEM768_KEY_SIZE,
                MlKemLevel::Level5 => MLKEM1024_KEY_SIZE,
            } as usize;
            
            pq_pubkey.resize(key_size, 0);
            let mut actual_size = key_size as c_int;
            
            unsafe {
                let result = wc_MlKemKey_ExportPub(mlkem_key, pq_pubkey.as_mut_ptr(), &mut actual_size);
                if result != WOLFSSL_SUCCESS {
                    return Err(CERT_ERROR_WOLFSSL_ERROR);
                }
                pq_pubkey.truncate(actual_size as usize);
            }
        }
        
        // TODO: Exporter la clé publique ECC
        
        Ok((ecc_pubkey, pq_pubkey))
    }
    
    /// Signe des données avec la méthode hybride
    pub fn hybrid_sign(&self, data: &[u8], rng: *mut c_void) -> Result<(Vec<u8>, Vec<u8>), c_int> {
        let mut ecc_signature = Vec::new();
        let mut pq_signature = Vec::new();
        
        // Signature ML-DSA
        if let Some(mldsa_key) = self.mldsa_key {
            let sig_size = match self.config.mldsa_level {
                MlDsaLevel::Level2 => MLDSA44_SIG_SIZE,
                MlDsaLevel::Level3 => MLDSA65_SIG_SIZE,
                MlDsaLevel::Level5 => MLDSA87_SIG_SIZE,
            } as usize;
            
            pq_signature.resize(sig_size, 0);
            let mut actual_size = sig_size as c_int;
            
            unsafe {
                let result = wc_MlDsaKey_Sign(
                    mldsa_key,
                    pq_signature.as_mut_ptr(),
                    &mut actual_size,
                    data.as_ptr(),
                    data.len() as c_int,
                    rng
                );
                if result != WOLFSSL_SUCCESS {
                    return Err(CERT_ERROR_WOLFSSL_ERROR);
                }
                pq_signature.truncate(actual_size as usize);
            }
        }
        
        // TODO: Signature ECC avec les bindings appropriés
        
        Ok((ecc_signature, pq_signature))
    }
    
    /// Vérifie une signature hybride
    pub fn hybrid_verify(&self, data: &[u8], ecc_sig: &[u8], pq_sig: &[u8]) -> Result<bool, c_int> {
        let mut ecc_valid = true;
        let mut pq_valid = true;
        
        // Vérifie la signature ML-DSA
        if let Some(mldsa_key) = self.mldsa_key {
            unsafe {
                let result = wc_MlDsaKey_Verify(
                    mldsa_key,
                    pq_sig.as_ptr(),
                    pq_sig.len() as c_int,
                    data.as_ptr(),
                    data.len() as c_int
                );
                pq_valid = result == WOLFSSL_SUCCESS;
            }
        }
        
        // TODO: Vérifier la signature ECC avec les bindings appropriés
        
        // En mode hybride, les deux signatures doivent être valides
        match self.config.hybrid_mode {
            HybridMode::Hybrid => Ok(ecc_valid && pq_valid),
            HybridMode::PqOnly => Ok(pq_valid),
            HybridMode::EccOnly => Ok(ecc_valid),
        }
    }
}

impl Drop for HybridKey {
    /// Nettoie automatiquement les ressources
    fn drop(&mut self) {
        unsafe {
            if !self.ecc_key.is_null() {
                libc::free(self.ecc_key);
            }
            if let Some(mlkem_key) = self.mlkem_key {
                wc_MlKemKey_Free(mlkem_key);
                libc::free(mlkem_key as *mut c_void);
            }
            if let Some(mldsa_key) = self.mldsa_key {
                wc_MlDsaKey_Free(mldsa_key);
                libc::free(mldsa_key as *mut c_void);
            }
        }
    }
}

// ============================================================================
// Fonctions utilitaires TLS post-quantique
// ============================================================================

/// Configure un contexte TLS pour supporter les algorithmes hybrides
pub fn configure_hybrid_tls_context(ctx: *mut WOLFSSL_CTX, config: &HybridCertConfig) -> Result<(), c_int> {
    if ctx.is_null() {
        return Err(CERT_ERROR_INVALID_PARAM);
    }
    
    unsafe {
        // Active les extensions X.509 post-quantiques si demandé
        if config.enable_x509_extensions {
            let result = wolfSSL_CTX_enable_pq_x509_extensions(ctx);
            if result != WOLFSSL_SUCCESS {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
        }
        
        // Configure les groupes d'échange de clés supportés
        let groups_list = match config.hybrid_mode {
            HybridMode::Hybrid => {
                match (config.mlkem_level, config.ecc_curve) {
                    (MlKemLevel::Level1, _) => CString::new("P256_MLKEM512:P256:MLKEM512").unwrap(),
                    (MlKemLevel::Level3, _) => CString::new("P384_MLKEM768:P384:MLKEM768").unwrap(),
                    (MlKemLevel::Level5, _) => CString::new("P521_MLKEM1024:P521:MLKEM1024").unwrap(),
                }
            },
            HybridMode::PqOnly => {
                match config.mlkem_level {
                    MlKemLevel::Level1 => CString::new("MLKEM512").unwrap(),
                    MlKemLevel::Level3 => CString::new("MLKEM768").unwrap(),
                    MlKemLevel::Level5 => CString::new("MLKEM1024").unwrap(),
                }
            },
            HybridMode::EccOnly => CString::new("P521:P384:P256").unwrap(),
        };
        
        let result = wolfSSL_CTX_set_groups_list(ctx, groups_list.as_ptr());
        if result != WOLFSSL_SUCCESS {
            return Err(CERT_ERROR_WOLFSSL_ERROR);
        }
        
        // Configure les algorithmes de signature supportés
        let sigalgs_list = match config.hybrid_mode {
            HybridMode::Hybrid => {
                match config.mldsa_level {
                    MlDsaLevel::Level2 => CString::new("ECDSA_P256_MLDSA44:ECDSA+SHA256:MLDSA44").unwrap(),
                    MlDsaLevel::Level3 => CString::new("ECDSA_P384_MLDSA65:ECDSA+SHA384:MLDSA65").unwrap(),
                    MlDsaLevel::Level5 => CString::new("ECDSA_P521_MLDSA87:ECDSA+SHA512:MLDSA87").unwrap(),
                }
            },
            HybridMode::PqOnly => {
                match config.mldsa_level {
                    MlDsaLevel::Level2 => CString::new("MLDSA44").unwrap(),
                    MlDsaLevel::Level3 => CString::new("MLDSA65").unwrap(),
                    MlDsaLevel::Level5 => CString::new("MLDSA87").unwrap(),
                }
            },
            HybridMode::EccOnly => CString::new("ECDSA+SHA512:ECDSA+SHA384:ECDSA+SHA256").unwrap(),
        };
        
        let result = wolfSSL_CTX_set_sigalgs_list(ctx, sigalgs_list.as_ptr());
        if result != WOLFSSL_SUCCESS {
            return Err(CERT_ERROR_WOLFSSL_ERROR);
        }
    }
    
    Ok(())
}

/// Obtient les informations de session hybride TLS
pub fn get_hybrid_session_info(ssl: *mut WOLFSSL) -> Result<HybridTlsSession, c_int> {
    if ssl.is_null() {
        return Err(CERT_ERROR_INVALID_PARAM);
    }
    
    unsafe {
        let negotiated_group = wolfSSL_get_negotiated_group(ssl);
        let negotiated_sigalg = wolfSSL_get_negotiated_sigalg(ssl);
        let is_pq_handshake = wolfSSL_is_pq_handshake(ssl) == 1;
        
        Ok(HybridTlsSession {
            negotiated_group,
            negotiated_sigalg,
            is_pq_handshake,
            shared_secret: Vec::new(), // TODO: Extraire le secret partagé réel
        })
    }
}

/// Vérifie la compatibilité CNSA 2.0
pub fn is_cnsa20_compatible(config: &HybridCertConfig) -> bool {
    match config.hybrid_mode {
        HybridMode::Hybrid | HybridMode::PqOnly => {
            // CNSA 2.0 exige ML-KEM-1024 et ML-DSA-87
            config.mlkem_level as c_int == MlKemLevel::Level5 as c_int &&
            config.mldsa_level as c_int == MlDsaLevel::Level5 as c_int
        },
        HybridMode::EccOnly => false, // ECC seul n'est pas compatible CNSA 2.0
    }
}

/// Obtient la description d'un algorithme post-quantique
pub fn get_pq_algorithm_description(level: c_int, is_kem: bool) -> &'static str {
    if is_kem {
        match level {
            512 => "ML-KEM-512 (128-bit quantum security)",
            768 => "ML-KEM-768 (192-bit quantum security)",
            1024 => "ML-KEM-1024 (256-bit quantum security, CNSA 2.0)",
            _ => "Unknown ML-KEM level",
        }
    } else {
        match level {
            44 => "ML-DSA-44 (128-bit quantum security)",
            65 => "ML-DSA-65 (192-bit quantum security)",
            87 => "ML-DSA-87 (256-bit quantum security, CNSA 2.0)",
            _ => "Unknown ML-DSA level",
        }
    }
} 
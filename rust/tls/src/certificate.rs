/*
 * Module Certificate - Gestion des certificats en Rust
 * 
 * Ce module implémente la gestion des certificats avec la même logique
 * que l'original C mais avec la sécurité mémoire de Rust.
 */

use std::os::raw::{c_char, c_int};
use std::ffi::CString;
use crate::wolfssl_ffi::*;
use crate::{CERT_OK, CERT_ERROR_INVALID_PARAM, CERT_ERROR_WOLFSSL_ERROR};
use std::ptr;

// ============================================================================
// Types et énumérations
// ============================================================================

/// Type de certificat
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CertificateType {
    RootCA = 0,
    IntermediateCA = 1,
    EndEntity = 2,
}

/// Statut de certificat
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CertificateStatus {
    Unknown = 0,
    Valid = 1,
    Expired = 2,
    NotYetValid = 3,
    Revoked = 4,
    InvalidSignature = 5,
}

/// Structure interne Rust pour un certificat
pub struct Certificate {
    pub cert_data: Vec<u8>,
    pub der_data: Vec<u8>,
    pub subject: String,
    pub issuer: String,
    pub is_ca: bool,
    pub not_before: i64,
    pub not_after: i64,
    pub status: CertificateStatus,
    pub cert_type: CertificateType,
}

impl Certificate {
    /// Crée un nouveau certificat vide
    pub fn new() -> Self {
        Certificate {
            cert_data: Vec::new(),
            der_data: Vec::new(),
            subject: String::new(),
            issuer: String::new(),
            is_ca: false,
            not_before: 0,
            not_after: 0,
            status: CertificateStatus::Unknown,
            cert_type: CertificateType::EndEntity,
        }
    }

    /// Charge un certificat depuis des données en mémoire
    pub fn load_from_data(data: &[u8], is_pem: bool) -> Result<Self, c_int> {
        unsafe {
            let format = if is_pem { WOLFSSL_FILETYPE_PEM } else { WOLFSSL_FILETYPE_ASN1 };
            let x509 = wolfSSL_X509_load_certificate_buffer(data.as_ptr(), data.len() as c_int, format);
            if x509.is_null() {
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }

            let mut cert = Certificate::new();
            cert.cert_data = data.to_vec();
            cert.der_data = data.to_vec(); // TODO: Convert if PEM

            // Extract subject
            let subject_name = wolfSSL_X509_get_subject_name(x509);
            let mut subject_buf = vec![0u8; 256];
            if wolfSSL_X509_NAME_oneline(subject_name, subject_buf.as_mut_ptr() as *mut c_char, 256) == ptr::null_mut() {
                wolfSSL_X509_free(x509);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            cert.subject = CString::from_vec_unchecked(subject_buf).into_string().unwrap_or_default();

            // Extract issuer
            let issuer_name = wolfSSL_X509_get_issuer_name(x509);
            let mut issuer_buf = vec![0u8; 256];
            if wolfSSL_X509_NAME_oneline(issuer_name, issuer_buf.as_mut_ptr() as *mut c_char, 256) == ptr::null_mut() {
                wolfSSL_X509_free(x509);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            cert.issuer = CString::from_vec_unchecked(issuer_buf).into_string().unwrap_or_default();

            cert.is_ca = wolfSSL_X509_get_isCA(x509) == 1;
            let not_before = wolfSSL_X509_get_notBefore(x509);
            if not_before.is_null() {
                wolfSSL_X509_free(x509);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            cert.not_before = wolfSSL_ASN1_TIME_get_posix(not_before);

            let not_after = wolfSSL_X509_get_notAfter(x509);
            if not_after.is_null() {
                wolfSSL_X509_free(x509);
                return Err(CERT_ERROR_WOLFSSL_ERROR);
            }
            cert.not_after = wolfSSL_ASN1_TIME_get_posix(not_after);
            cert.status = CertificateStatus::Unknown;
            cert.cert_type = determine_certificate_type(&cert);

            wolfSSL_X509_free(x509);

            Ok(cert)
        }
    }

    /// Charge un certificat depuis un fichier
    pub fn load_from_file(file_path: &str, is_pem: bool) -> Result<Self, c_int> {
        match std::fs::read(file_path) {
            Ok(data) => Self::load_from_data(&data, is_pem),
            Err(_) => Err(CERT_ERROR_INVALID_PARAM),
        }
    }

    /// Vérifie la validité temporelle du certificat
    pub fn check_validity(&mut self) -> c_int {
        let current_time = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs() as i64;
        if current_time < self.not_before {
            self.status = CertificateStatus::NotYetValid;
            return crate::CERT_ERROR_NOT_YET_VALID;
        }
        if current_time > self.not_after {
            self.status = CertificateStatus::Expired;
            return crate::CERT_ERROR_CERT_EXPIRED;
        }
        self.status = CertificateStatus::Valid;
        CERT_OK
    }

    /// Obtient le sujet du certificat
    pub fn get_subject(&self) -> &str {
        &self.subject
    }

    /// Obtient l'émetteur du certificat
    pub fn get_issuer(&self) -> &str {
        &self.issuer
    }

    /// Vérifie si le certificat est une CA
    pub fn is_ca(&self) -> bool {
        self.is_ca
    }
}

// ============================================================================
// Fonctions utilitaires pour la gestion des CA
// ============================================================================

/// Trouve et charge le certificat CA racine depuis un répertoire
pub fn load_root_ca_from_directory(ca_dir: &str, is_pem: bool) -> Result<Certificate, c_int> {
    let ca_filename = if is_pem { "ca.pem" } else { "ca.der" };
    let ca_path = format!("{}/{}", ca_dir, ca_filename);
    
    Certificate::load_from_file(&ca_path, is_pem)
}

/// Vérifie si un certificat est auto-signé (potentiellement CA racine)
pub fn is_self_signed(cert: &Certificate) -> bool {
    cert.subject == cert.issuer
}

/// Détermine le type d'un certificat basé sur ses propriétés
pub fn determine_certificate_type(cert: &Certificate) -> CertificateType {
    if is_self_signed(cert) {
        CertificateType::RootCA
    } else if cert.is_ca {
        CertificateType::IntermediateCA
    } else {
        CertificateType::EndEntity
    }
} 
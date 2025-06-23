////////////////////////////////////////////////////////////
//  Network message handler
//  Handles incoming network messages according to protocol
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
// Updated: Added callback system and response message support
////////////////////////////////////////////////////////////

use crate::x_log::write_log;
use crate::network::converter::{Converter, NetworkMessageType, convert_to_struct};
use crate::network::server::SERVER_OK;
use std::sync::{Arc, Mutex};

/// Type pour les callbacks de gestion de messages
pub type MessageCallback = dyn Fn(NetworkMessageType, &Converter) -> Option<Converter> + Send + Sync;

/// Structure pour gérer les callbacks et les réponses
pub struct NetworkHandler {
    callbacks: Arc<Mutex<std::collections::HashMap<NetworkMessageType, Box<MessageCallback>>>>,
}

impl NetworkHandler {
    pub fn new() -> Self {
        Self {
            callbacks: Arc::new(Mutex::new(std::collections::HashMap::new())),
        }
    }

    /// Enregistre un callback pour un type de message spécifique
    pub fn register_callback<F>(&self, msg_type: NetworkMessageType, callback: F)
    where
        F: Fn(NetworkMessageType, &Converter) -> Option<Converter> + Send + Sync + 'static,
    {
        if let Ok(mut callbacks) = self.callbacks.lock() {
            callbacks.insert(msg_type, Box::new(callback));
            write_log(&format!("Callback enregistré pour le type de message: {:?}", msg_type));
        } else {
            write_log("Erreur lors de l'enregistrement du callback: mutex empoisonné");
        }
    }

    /// Traite un message avec le callback approprié
    pub fn handle_message(&self, msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
        match self.callbacks.lock() {
            Ok(callbacks) => {
                if let Some(callback) = callbacks.get(&msg_type) {
                    callback(msg_type, message)
                } else {
                    write_log(&format!("Aucun callback enregistré pour le type: {:?}", msg_type));
                    None
                }
            }
            Err(poisoned) => {
                write_log("Mutex des callbacks empoisonné, récupération...");
                let callbacks = poisoned.into_inner();
                if let Some(callback) = callbacks.get(&msg_type) {
                    callback(msg_type, message)
                } else {
                    None
                }
            }
        }
    }
}

/// Instance globale du gestionnaire de réseau
static mut NETWORK_HANDLER: Option<NetworkHandler> = None;
static INIT: std::sync::Once = std::sync::Once::new();

/// Initialise le gestionnaire de réseau global
pub fn init_network_handler() -> &'static NetworkHandler {
    unsafe {
        INIT.call_once(|| {
            NETWORK_HANDLER = Some(NetworkHandler::new());
        });
        NETWORK_HANDLER.as_ref().unwrap()
    }
}

/// Obtient une référence au gestionnaire de réseau global
pub fn get_network_handler() -> &'static NetworkHandler {
    unsafe {
        NETWORK_HANDLER.as_ref().expect("Network handler non initialisé - appelez init_network_handler() d'abord")
    }
}

/// Structure pour encapsuler une réponse réseau
#[derive(Debug)]
pub struct NetworkResponse {
    pub response_message: Option<Converter>,
    pub status_code: u32,
}

impl NetworkResponse {
    pub fn success(message: Option<Converter>) -> Self {
        Self {
            response_message: message,
            status_code: SERVER_OK,
        }
    }

    pub fn error(status_code: u32) -> Self {
        Self {
            response_message: None,
            status_code,
        }
    }
}

/// Traite un message réseau reçu et retourne une réponse optionnelle
pub fn handle_incoming_message(buffer: &[u8], bytes_received: usize) -> NetworkResponse {
    write_log(&format!("Traitement d'un message de {} bytes", bytes_received));
    
    // Convertir le buffer en structure
    match convert_to_struct(&buffer[..bytes_received]) {
        Ok(message) => {
            write_log(&format!(
                "Message décodé - Longueur: {}, ID: 0x{:02X}, Data: {} bytes",
                message.length, message.idx, message.data.len()
            ));
            
            // Traiter selon le type de message
            match message.get_message_type() {
                Some(msg_type) => {
                    let handler = get_network_handler();
                    let response_message = handler.handle_message(msg_type, &message);
                    NetworkResponse::success(response_message)
                },
                None => {
                    write_log(&format!("Type de message inconnu: 0x{:02X}", message.idx));
                    NetworkResponse::success(None)
                }
            }
        },
        Err(error) => {
            write_log(&format!("Erreur lors du décodage du message: 0x{:08X}", error));
            NetworkResponse::error(error)
        }
    }
}

/// Fonctions de callback par défaut pour les différents types de messages

/// Callback par défaut pour les commandes de mouvement
pub fn default_movement_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    if message.data.len() < 2 {
        write_log("Commande de mouvement invalide: données insuffisantes");
        return None;
    }
    
    let direction = message.data[0];
    let speed = message.data[1];
    
    write_log(&format!("Mouvement - Direction: {}, Vitesse: {}", direction, speed));
    
    // Ici vous pouvez appeler les fonctions du module hardware
    // Exemple :
    // match direction {
    //     1 => motors::move_forward(speed),
    //     2 => motors::move_backward(speed),
    //     3 => motors::turn_left(speed),
    //     4 => motors::turn_right(speed),
    //     0 => motors::stop(),
    //     _ => { write_log("Direction inconnue"); }
    // }
    
    // Retourner un message de confirmation
    Some(create_status_info_message(1)) // 1 = mouvement en cours
}

/// Callback par défaut pour le contrôle de mission
pub fn default_mission_control_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    if message.data.is_empty() {
        write_log("Commande de mission invalide: aucune donnée");
        return None;
    }
    
    let command = message.data[0];
    write_log(&format!("Contrôle de mission - Commande: {}", command));
    
    // Traiter selon la commande
    let status = match command {
        1 => { write_log("Démarrage de mission"); 2 }, // mission active
        2 => { write_log("Arrêt de mission"); 0 },     // mission arrêtée
        3 => { write_log("Pause de mission"); 3 },     // mission en pause
        4 => { write_log("Reprise de mission"); 2 },   // mission active
        _ => { write_log("Commande de mission inconnue"); 255 }, // erreur
    };
    
    Some(create_status_info_message(status))
}

/// Callback par défaut pour les points sélectionnés
pub fn default_selected_points_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log(&format!("Points sélectionnés reçus: {} bytes de données", message.data.len()));
    
    // Les points sont généralement encodés comme des coordonnées (x,y)
    let num_points = if message.data.len() % 4 == 0 {
        let num = message.data.len() / 4;
        write_log(&format!("Nombre de points (u16): {}", num));
        num
    } else if message.data.len() % 8 == 0 {
        let num = message.data.len() / 8;
        write_log(&format!("Nombre de points (u32): {}", num));
        num
    } else {
        write_log("Format de points non reconnu");
        0
    };
    
    // Retourner un message de confirmation avec le nombre de points traités
    if num_points > 0 {
        Some(create_status_info_message(4)) // 4 = points reçus et traités
    } else {
        None
    }
}

/// Callback par défaut pour l'upload de carte
pub fn default_upload_map_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log(&format!("Données de carte reçues: {} bytes", message.data.len()));
    
    // Ici vous pourriez sauvegarder les données de carte
    // ou les traiter selon votre format de carte
    
    // Retourner un message de confirmation
    Some(create_status_info_message(5)) // 5 = carte reçue et traitée
}

/// Callback par défaut pour la découverte de robot
pub fn default_robot_discovery_callback(_msg_type: NetworkMessageType, _message: &Converter) -> Option<Converter> {
    write_log("Réponse à la découverte de robot - Envoi du manifeste");
    
    // Créer un message de manifeste simple
    let manifest_data = b"Robot-MRPiZ-v1.0".to_vec();
    let length = 1 + manifest_data.len() as u16;
    
    Some(Converter::new(length, NetworkMessageType::IdManifest as u8, manifest_data))
}

/// Callback par défaut pour le manifeste
pub fn default_manifest_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log(&format!("Manifeste reçu: {} bytes", message.data.len()));
    
    // Parser le manifeste selon votre format
    if let Ok(manifest_str) = std::str::from_utf8(&message.data) {
        write_log(&format!("Manifeste: {}", manifest_str));
    }
    
    None // Pas de réponse nécessaire pour le manifeste
}

/// Initialise les callbacks par défaut
pub fn setup_default_callbacks() {
    let handler = init_network_handler();
    
    handler.register_callback(NetworkMessageType::IdSetMovement, default_movement_callback);
    handler.register_callback(NetworkMessageType::IdMissionControl, default_mission_control_callback);
    handler.register_callback(NetworkMessageType::IdSelectedPoints, default_selected_points_callback);
    handler.register_callback(NetworkMessageType::IdUploadMap, default_upload_map_callback);
    handler.register_callback(NetworkMessageType::IdIsAnyRobotHere, default_robot_discovery_callback);
    handler.register_callback(NetworkMessageType::IdManifest, default_manifest_callback);
    
    write_log("Callbacks par défaut configurés");
}

/// Crée un message de réponse pour l'information de batterie
pub fn create_battery_info_message(level: u8, voltage: f32) -> Converter {
    let mut data = Vec::with_capacity(5);
    data.push(level);
    data.extend_from_slice(&voltage.to_le_bytes());
    
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfBattery as u8, data)
}

/// Crée un message de réponse pour le statut
pub fn create_status_info_message(status: u8) -> Converter {
    let data = vec![status];
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfStatus as u8, data)
}

/// Crée un message de réponse pour la position
pub fn create_position_info_message(x: f32, y: f32, theta: f32) -> Converter {
    let mut data = Vec::with_capacity(12);
    data.extend_from_slice(&x.to_le_bytes());
    data.extend_from_slice(&y.to_le_bytes());
    data.extend_from_slice(&theta.to_le_bytes());
    
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfPos as u8, data)
}

/// Crée un message de réponse pour le temps
pub fn create_time_info_message(timestamp: u64) -> Converter {
    let data = timestamp.to_le_bytes().to_vec();
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfTime as u8, data)
}

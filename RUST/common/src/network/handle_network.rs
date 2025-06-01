////////////////////////////////////////////////////////////
//  Network message handler
//  Handles incoming network messages according to protocol
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////

use crate::x_log::write_log;
use crate::network::converter::{ConverterConfig, NetworkMessageType, convert_to_struct};
use crate::network::server::SERVER_OK;

/// Traite un message réseau reçu
pub fn handle_incoming_message(buffer: &[u8], bytes_received: usize) -> u32 {
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
                Some(msg_type) => handle_message_by_type(msg_type, &message),
                None => {
                    write_log(&format!("Type de message inconnu: 0x{:02X}", message.idx));
                    SERVER_OK
                }
            }
        },
        Err(error) => {
            write_log(&format!("Erreur lors du décodage du message: 0x{:08X}", error));
            error
        }
    }
}

/// Traite un message selon son type
fn handle_message_by_type(msg_type: NetworkMessageType, message: &ConverterConfig) -> u32 {
    match msg_type {
        // Messages envoyés par Android
        NetworkMessageType::IdSetMovement => {
            write_log("Traitement commande de mouvement");
            handle_movement_command(message)
        },
        NetworkMessageType::IdMissionControl => {
            write_log("Traitement commande de mission");
            handle_mission_control(message)
        },
        NetworkMessageType::IdSelectedPoints => {
            write_log("Traitement points sélectionnés");
            handle_selected_points(message)
        },
        NetworkMessageType::IdUploadMap => {
            write_log("Traitement upload de carte");
            handle_upload_map(message)
        },
        
        // Messages UDP
        NetworkMessageType::IdIsAnyRobotHere => {
            write_log("Réponse à la découverte de robot");
            handle_robot_discovery(message)
        },
        NetworkMessageType::IdManifest => {
            write_log("Traitement du manifeste");
            handle_manifest(message)
        },
        
        // Messages envoyés par le bot (normalement on ne les reçoit pas)
        _ => {
            write_log(&format!("Message de type bot reçu (inattendu): {:?}", msg_type));
            SERVER_OK
        }
    }
}

/// Traite une commande de mouvement
fn handle_movement_command(message: &ConverterConfig) -> u32 {
    if message.data.len() < 2 {
        write_log("Commande de mouvement invalide: données insuffisantes");
        return SERVER_OK;
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
    
    SERVER_OK
}

/// Traite une commande de contrôle de mission
fn handle_mission_control(message: &ConverterConfig) -> u32 {
    if message.data.is_empty() {
        write_log("Commande de mission invalide: aucune donnée");
        return SERVER_OK;
    }
    
    let command = message.data[0];
    write_log(&format!("Contrôle de mission - Commande: {}", command));
    
    // Traiter selon la commande
    match command {
        1 => write_log("Démarrage de mission"),
        2 => write_log("Arrêt de mission"),
        3 => write_log("Pause de mission"),
        4 => write_log("Reprise de mission"),
        _ => write_log("Commande de mission inconnue"),
    }
    
    SERVER_OK
}

/// Traite les points sélectionnés
fn handle_selected_points(message: &ConverterConfig) -> u32 {
    write_log(&format!("Points sélectionnés reçus: {} bytes de données", message.data.len()));
    
    // Les points sont généralement encodés comme des coordonnées (x,y)
    // Chaque point pourrait être 4 bytes (2 x u16) ou 8 bytes (2 x u32)
    if message.data.len() % 4 == 0 {
        let num_points = message.data.len() / 4;
        write_log(&format!("Nombre de points (u16): {}", num_points));
    } else if message.data.len() % 8 == 0 {
        let num_points = message.data.len() / 8;
        write_log(&format!("Nombre de points (u32): {}", num_points));
    } else {
        write_log("Format de points non reconnu");
    }
    
    SERVER_OK
}

/// Traite l'upload de carte
fn handle_upload_map(message: &ConverterConfig) -> u32 {
    write_log(&format!("Données de carte reçues: {} bytes", message.data.len()));
    
    // Ici vous pourriez sauvegarder les données de carte
    // ou les traiter selon votre format de carte
    
    SERVER_OK
}

/// Traite la découverte de robot
fn handle_robot_discovery(_message: &ConverterConfig) -> u32 {
    write_log("Réponse à la découverte de robot - Envoi du manifeste");
    
    // Ici vous devriez créer et envoyer un message de manifeste
    // avec les informations du robot
    
    SERVER_OK
}

/// Traite le manifeste
fn handle_manifest(message: &ConverterConfig) -> u32 {
    write_log(&format!("Manifeste reçu: {} bytes", message.data.len()));
    
    // Parser le manifeste selon votre format
    
    SERVER_OK
}

/// Crée un message de réponse pour l'information de batterie
pub fn create_battery_info_message(level: u8, voltage: f32) -> ConverterConfig {
    let mut data = Vec::with_capacity(5);
    data.push(level);
    data.extend_from_slice(&voltage.to_le_bytes());
    
    let length = 1 + data.len() as u16; // idx + data
    
    ConverterConfig::new(length, NetworkMessageType::IdInfBattery as u8, data)
}

/// Crée un message de réponse pour le statut
pub fn create_status_info_message(status: u8) -> ConverterConfig {
    let data = vec![status];
    let length = 1 + data.len() as u16; // idx + data
    
    ConverterConfig::new(length, NetworkMessageType::IdInfStatus as u8, data)
}

/// Crée un message de réponse pour la position
pub fn create_position_info_message(x: f32, y: f32, theta: f32) -> ConverterConfig {
    let mut data = Vec::with_capacity(12);
    data.extend_from_slice(&x.to_le_bytes());
    data.extend_from_slice(&y.to_le_bytes());
    data.extend_from_slice(&theta.to_le_bytes());
    
    let length = 1 + data.len() as u16; // idx + data
    
    ConverterConfig::new(length, NetworkMessageType::IdInfPos as u8, data)
}

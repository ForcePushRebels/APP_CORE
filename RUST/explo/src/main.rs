////////////////////////////////////////////////////////////
//  Robot exploration application entry point
//  Main program for the MRPiZ robot control
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
// Updated: Added network callback system example
////////////////////////////////////////////////////////////

use common::{
    x_assert::x_assert,
    x_log::{initialize, write_log, LogConfig, XOS_LOG_OK},
    hardware::{
        Robot, luminosity
    },
    x_watchdog::{init_watchdog, refresh_watchdog, Watchdog},
    network::{
        server::{Server, ServerConfig}, 
        handle_network::{setup_default_callbacks, get_network_handler},
        converter::{NetworkMessageType, Converter},
    },
    sensor_manager::SensorManager,
};

// Exemple de callback personnalisé pour les commandes de mouvement
fn custom_movement_callback(msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log("=== Callback personnalisé de mouvement appelé ===");
    
    if message.data.len() < 2 {
        write_log("Commande de mouvement invalide: données insuffisantes");
        return None;
    }
    
    let direction = message.data[0];
    let speed = message.data[1];
    
    write_log(&format!("Mouvement personnalisé - Direction: {}, Vitesse: {}", direction, speed));
    
    // Ici vous pouvez appeler vos fonctions hardware spécifiques
    match direction {
        1 => write_log("Avancer !"),
        2 => write_log("Reculer !"),
        3 => write_log("Tourner à gauche !"),
        4 => write_log("Tourner à droite !"),
        0 => write_log("Arrêt !"),
        _ => write_log("Direction inconnue"),
    }
    
    // Retourner un message de statut personnalisé
    let status_data = vec![direction, speed]; // Echo des commandes
    let length = 1 + status_data.len() as u16;
    Some(Converter::new(length, NetworkMessageType::IdInfStatus as u8, status_data))
}

fn main() {
    // Initialisation du système de logs
    let log_config = LogConfig::new("explo.log");
    let log_result = initialize(log_config);
    x_assert(log_result == XOS_LOG_OK);
    
    write_log("=== Démarrage du programme d'exploration ===");
    
    // Initialiser le système de callbacks réseau
    write_log("Configuration des callbacks réseau...");
    
    // 1. Configurer les callbacks par défaut
    setup_default_callbacks();
    
    // 2. Remplacer le callback de mouvement par notre version personnalisée
    let handler = get_network_handler();
    handler.register_callback(NetworkMessageType::IdSetMovement, custom_movement_callback);
    
    // 3. Ajouter un callback personnalisé pour la découverte de robots
    handler.register_callback(NetworkMessageType::IdIsAnyRobotHere, |_msg_type, _message| {
        write_log("Robot découvert ! Envoi des informations...");
        
        // Créer un manifeste personnalisé
        let manifest_data = format!("Robot-MRPiZ-ExplorBot-v1.0-ID:{}", 
                                    std::process::id()).into_bytes();
        let length = 1 + manifest_data.len() as u16;
        
        Some(Converter::new(length, NetworkMessageType::IdManifest as u8, manifest_data))
    });
    
    write_log("Callbacks réseau configurés !");
    
    // Création et initialisation du robot
    let mut robot = Robot::new();
    
    match robot.init() 
    {
        Ok(()) => {
            write_log("Robot initialisé avec succès");
            
            // Initialiser le capteur de luminosité
            if let Err(e) = luminosity::init() 
            {
                write_log(&format!("Erreur init capteur luminosité: {}", e));
            }
            
        },
        Err(e) => 
        {
            write_log(&format!("Erreur d'initialisation du robot: {}", e));
            x_assert(false); // Force l'arrêt si le robot ne peut pas être initialisé
        }
    }

    // Démarrage du watchdog avec handler personnalisé
    let mut watchdog = Watchdog::new(300); 
    watchdog.set_expiry_handler(|| {
        write_log("Watchdog expiré - Le système ne répond plus !");
    });
    
    if let Err(e) = init_watchdog(watchdog) {
        write_log(&format!("Erreur init watchdog: {}", e));
    }

    //demarrer le gestionnaire de capteurs
    let mut sensor_manager = SensorManager::new();
    x_assert(sensor_manager.start().is_ok());

    //start the server sur un thread dédié
    let mut server = Server::new(ServerConfig::new("0.0.0.0".to_string(), 8080)).unwrap();
    x_assert(server.start().is_ok());

    write_log("Serveur réseau démarré sur le port 8080");
    write_log("Le robot est prêt à recevoir des commandes !");

    let mut result: Result<(), &'static str>;
    loop 
    {
        result = refresh_watchdog();
        if result.is_err() {
            write_log("Watchdog expiré - Le système ne répond plus !");
            break;
        }
        
        // Petite pause pour éviter de surcharger le CPU
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    
    write_log("=== Fin du programme d'exploration ===");
    
    // Arrêt propre du serveur
    if server.stop() != common::network::server::SERVER_OK {
        write_log("Erreur lors de l'arrêt du serveur");
    }
}


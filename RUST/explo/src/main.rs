////////////////////////////////////////////////////////////
//  Robot exploration application entry point
//  Main program for the MRPiZ robot control
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use common::{
    x_assert::x_assert,
    x_log::{initialize, write_log, LogConfig, XOS_LOG_OK},
    hardware::{
        Robot,luminosity, sleep_ms
    },
    x_watchdog::{init_watchdog, refresh_watchdog, Watchdog},
    network::{
        server::{Server, ServerConfig}, 
    },
};
use std::thread;

fn main() {
    // Initialisation du système de logs
    let log_config = LogConfig::new("explo.log");
    let log_result = initialize(log_config);
    x_assert(log_result == XOS_LOG_OK);
    
    write_log("=== Démarrage du programme d'exploration ===");
    
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

    //start the server sur un thread dédié
    let mut server = Server::new(ServerConfig::new("127.0.0.1".to_string(), 8080)).unwrap();
    let server_thread = thread::spawn(move || {
        server.start();
    });


    
    loop 
    {
        refresh_watchdog();
    }
    
    write_log("=== Fin du programme d'exploration ===");
    
    // Note: Le thread du serveur continue à tourner en arrière-plan
    // Dans un vrai système, vous pourriez vouloir l'arrêter proprement
}


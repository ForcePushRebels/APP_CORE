////////////////////////////////////////////////////////////
//  Robot exploration application entry point
//  Main program for the MRPiZ robot control
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use common::hardwareAbstraction::init_robot;
use common::xLog;

fn main() {
    xLog::log_info("main", "Démarrage du robot");
    
    // Initialisation du système de journalisation
    let log_config = xLog::LogConfig {
        min_level: xLog::LogLevel::Debug,
        include_timestamp: true,
        include_source_info: true,
    };
    
    match xLog::init(log_config) {
        Ok(_) => {},
        Err(e) => {
            eprintln!("Erreur d'initialisation du système de journalisation: {}", e);
            std::process::exit(1);
        }
    }
    
    // Configuration des assertions
    common::xAssert::configure(true, true);
    
    // Initialisation du robot
    if let Err(e) = init_robot() {
        xLog::log_error("main", &format!("Erreur d'initialisation du robot: {}", e));
        return;
    }
    
    // Exemple : avancer
    if let Err(e) = common::hardwareAbstraction::move_forward(50) {
        xLog::log_error("main", &format!("Erreur lors de l'avance: {}", e));
    }
    
    // Attendre quelques secondes
    common::xOs::sleep_ms(3000);
    
    // Arrêter le robot
    if let Err(e) = common::hardwareAbstraction::stop() {
        xLog::log_error("main", &format!("Erreur lors de l'arrêt: {}", e));
    }
    
    // Afficher la date et l'heure
    let date_heure = common::xOs::horodateur::get_date_heure();
    let date_formatee = common::xOs::horodateur::formater_date_heure(date_heure);
    xLog::log_info("main", &format!("Date et heure actuelles: {}", date_formatee));
    
    xLog::log_info("main", "Fin du programme");
} 
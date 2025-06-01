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
        Robot, motors, sensors, led, battery, luminosity, sleep_ms,
        led::Color
    },
    x_watchdog::{init, Watchdog},
};

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

    //demarage du watchdog
    let mut watchdog = Watchdog::new(1000);
    watchdog.start();

    // Démarrer l'exploration
    explore_environment();
    
    write_log("=== Fin du programme d'exploration ===");
}

/// Fonction principale d'exploration de l'environnement
fn explore_environment() {
    write_log("Début de l'exploration");
    
    // Allumer la LED verte pour indiquer le début
    if let Err(e) = led::set_color(Color::Green) {
        write_log(&format!("Erreur LED: {}", e));
    }
    
    // Vérifier l'état de la batterie
    check_battery_status();
    
    // Test du capteur de luminosité
    test_luminosity_sensor();
    
    // Exploration simple : avancer jusqu'à détecter un obstacle
    simple_forward_exploration();
    
    // Test de rotation
    test_rotation();
    
    // Arrêter le robot et éteindre la LED
    if let Err(e) = motors::stop() {
        write_log(&format!("Erreur arrêt moteurs: {}", e));
    }
    
    if let Err(e) = led::set_color(Color::Off) {
        write_log(&format!("Erreur extinction LED: {}", e));
    }
    
    write_log("Exploration terminée");
}

/// Vérifie et affiche l'état de la batterie
fn check_battery_status() {
    write_log("=== Vérification de la batterie ===");
    
    match battery::get_level() {
        Ok(level) => write_log(&format!("Niveau de batterie: {}%", level)),
        Err(e) => write_log(&format!("Erreur lecture niveau batterie: {}", e)),
    }
    
    match battery::get_voltage() {
        Ok(voltage) => write_log(&format!("Tension batterie: {:.2}V", voltage)),
        Err(e) => write_log(&format!("Erreur lecture tension batterie: {}", e)),
    }
}

/// Teste le capteur de luminosité
fn test_luminosity_sensor() {
    write_log("=== Test du capteur de luminosité ===");
    
    // Allumer l'éclairage du capteur
    if let Err(e) = luminosity::set_light(true) {
        write_log(&format!("Erreur allumage capteur: {}", e));
        return;
    }
    
    sleep_ms(500); // Attendre que le capteur se stabilise
    
    // Lire la valeur
    match luminosity::read() {
        Ok(value) => write_log(&format!("Luminosité: {}", value)),
        Err(e) => write_log(&format!("Erreur lecture luminosité: {}", e)),
    }
    
    // Éteindre l'éclairage
    if let Err(e) = luminosity::set_light(false) {
        write_log(&format!("Erreur extinction capteur: {}", e));
    }
}

/// Exploration simple : avancer jusqu'à détecter un obstacle
fn simple_forward_exploration() {
    write_log("=== Exploration vers l'avant ===");
    
    led::set_color(Color::Blue).ok();
    
    const OBSTACLE_THRESHOLD: u8 = 100; // Seuil de détection d'obstacle
    const MOVE_SPEED: u8 = 30;          // Vitesse de déplacement
    
    for step in 1..=20 { // Maximum 20 étapes
        write_log(&format!("Étape d'exploration: {}", step));
        
        // Lire tous les capteurs de proximité
        match sensors::read_all_proximity() {
            Ok(sensors_values) => {
                write_log(&format!("Capteurs: [{}, {}, {}, {}, {}]", 
                    sensors_values[0], sensors_values[1], sensors_values[2], 
                    sensors_values[3], sensors_values[4]
                ));
                
                // Vérifier le capteur central pour obstacle
                if sensors_values[2] > OBSTACLE_THRESHOLD {
                    write_log("Obstacle détecté devant ! Arrêt de l'exploration");
                    led::set_color(Color::Red).ok();
                    break;
                }
                
                // Avancer un peu
                if let Err(e) = motors::move_forward(MOVE_SPEED) {
                    write_log(&format!("Erreur mouvement: {}", e));
                    break;
                }
                
                sleep_ms(1000); // Avancer pendant 1 seconde
                
                // Arrêter
                motors::stop().ok();
                sleep_ms(500); // Pause entre les étapes
                
            },
            Err(e) => {
                write_log(&format!("Erreur lecture capteurs: {}", e));
                break;
            }
        }
    }
    
    // S'assurer que le robot s'arrête
    motors::stop().ok();
}

/// Test de rotation du robot
fn test_rotation() {
    write_log("=== Test de rotation ===");
    
    led::set_color(Color::Green).ok();
    
    const ROTATION_SPEED: u8 = 25;
    const ROTATION_TIME: u32 = 1500; // millisecondes
    
    // Rotation à gauche
    write_log("Rotation à gauche");
    if let Err(e) = motors::turn_left(ROTATION_SPEED) {
        write_log(&format!("Erreur rotation gauche: {}", e));
    } else {
        sleep_ms(ROTATION_TIME);
        motors::stop().ok();
    }
    
    sleep_ms(1000); // Pause
    
    // Rotation à droite
    write_log("Rotation à droite");
    if let Err(e) = motors::turn_right(ROTATION_SPEED) {
        write_log(&format!("Erreur rotation droite: {}", e));
    } else {
        sleep_ms(ROTATION_TIME);
        motors::stop().ok();
    }
    
    sleep_ms(500); // Pause finale
}

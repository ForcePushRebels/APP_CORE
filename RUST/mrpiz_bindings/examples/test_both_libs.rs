////////////////////////////////////////////////////////////
//  Exemple d'utilisation des bindings MRPiZ et LumPiZ
//  Démontre l'utilisation des deux bibliothèques
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use mrpiz_bindings;
use std::thread;
use std::time::Duration;

fn main() {
    println!("=== Test des bindings MRPiZ et LumPiZ ===");
    
    // Test de MRPiZ
    println!("\n--- Test MRPiZ ---");
    unsafe {
        // Initialisation de MRPiZ
        let result = mrpiz_bindings::mrpiz::mrpiz_init_intox("127.0.0.1\0".as_ptr() as *const _, 12345);
        if result == 0 {
            println!("✓ MRPiZ initialisé avec succès");
            
            // Test des moteurs
            println!("Test des moteurs...");
            mrpiz_bindings::mrpiz::mrpiz_motor_set(
                mrpiz_bindings::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, 
                50
            );
            println!("✓ Moteurs configurés à 50%");
            
            thread::sleep(Duration::from_millis(100));
            
            // Arrêt des moteurs
            mrpiz_bindings::mrpiz::mrpiz_motor_set(
                mrpiz_bindings::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, 
                0
            );
            println!("✓ Moteurs arrêtés");
            
            // Test des capteurs de proximité
            println!("Test des capteurs de proximité...");
            let sensor_value = mrpiz_bindings::mrpiz::mrpiz_proxy_sensor_get(
                mrpiz_bindings::mrpiz::mrpiz_proxy_sensor_id::MRPIZ_PROXY_SENSOR_FRONT_CENTER
            );
            println!("✓ Capteur central: {}", sensor_value);
            
            // Test de la LED
            println!("Test de la LED...");
            mrpiz_bindings::mrpiz::mrpiz_led_rgb_set(
                mrpiz_bindings::mrpiz::mrpiz_led_rgb_color_t::MRPIZ_LED_RED
            );
            println!("✓ LED rouge activée");
            
            // Test de la batterie
            println!("Test de la batterie...");
            let battery_level = mrpiz_bindings::mrpiz::mrpiz_battery_level();
            let battery_voltage = mrpiz_bindings::mrpiz::mrpiz_battery_voltage();
            println!("✓ Niveau batterie: {}%, Tension: {:.2}V", battery_level, battery_voltage);
            
            // Fermeture de MRPiZ
            mrpiz_bindings::mrpiz::mrpiz_close();
            println!("✓ MRPiZ fermé");
        } else {
            println!("✗ Erreur lors de l'initialisation de MRPiZ: {}", result);
        }
    }
    
    // Test de LumPiZ
    println!("\n--- Test LumPiZ ---");
    unsafe {
        // Initialisation du capteur de luminosité
        let result = mrpiz_bindings::lumpiz::lumpiz_luminosity_init();
        if result == 0 {
            println!("✓ LumPiZ initialisé avec succès");
            
            // Allumage du capteur
            println!("Test du capteur de luminosité...");
            mrpiz_bindings::lumpiz::lumpiz_luminosity_light_set(
                mrpiz_bindings::lumpiz::lumpiz_luminosity_state_t::LUMPIZ_LUMINOSITY_ON
            );
            println!("✓ Capteur de luminosité allumé");
            
            thread::sleep(Duration::from_millis(100));
            
            // Lecture de la valeur
            let luminosity_value = mrpiz_bindings::lumpiz::lumpiz_luminosity_get();
            println!("✓ Valeur de luminosité: {}", luminosity_value);
            
            // Vérification de l'état
            let state = mrpiz_bindings::lumpiz::lumpiz_luminosity_light_get();
            println!("✓ État du capteur: {}", state);
            
            // Extinction du capteur
            mrpiz_bindings::lumpiz::lumpiz_luminosity_light_set(
                mrpiz_bindings::lumpiz::lumpiz_luminosity_state_t::LUMPIZ_LUMINOSITY_OFF
            );
            println!("✓ Capteur de luminosité éteint");
        } else {
            println!("✗ Erreur lors de l'initialisation de LumPiZ: {}", result);
        }
    }
    
    println!("\n=== Test terminé ===");
} 
////////////////////////////////////////////////////////////
//  Robot exploration application entry point
//  Main program for the MRPiZ robot control
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use common::hardwareAbstraction::{
    init_robot,
    init_robot_with_error_info,
    move_forward,
    move_backward,
    turn_left,
    turn_right,
    get_battery_level,
    get_battery_voltage,
    set_led_color,
    read_proximity_sensor,
};
use common::xAssert::xAssert;
use common::xLog::{
    initialize, 
    write_log,
    LogConfig, 
    XOS_LOG_ERROR, 
    XOS_LOG_MUTEX_ERROR, 
    XOS_LOG_OK,
};

fn main() {
    write_log("Démarrage du robot");

    // Initialisation du système de journalisation
    let log_config = LogConfig::new("explo.log");

    let mut log_initialized: u8 = 0;
    log_initialized = initialize(log_config);

    // Vérifier si l'initialisation a réussi
    xAssert(log_initialized == XOS_LOG_OK);

    // Initialisation du robot avec détails d'erreurs
    if init_robot().is_err() 
    {
        write_log("Erreur lors de l'initialisation du robot");
        return;
    }
    
    write_log("Robot initialisé avec succès");

    xAssert(move_forward(50).is_ok());
    // Attendre quelques secondes
    //common::xOs::sleep_ms(3000);

    xAssert(move_backward(50).is_ok());

    // Afficher la date et l'heure
    write_log("Fin du programme");

    loop {
        let battery_level = get_battery_level();
        write_log(&format!("Niveau de batterie: {}", battery_level.unwrap()));
        common::xOs::sleep_ms(1000);
    }
}

////////////////////////////////////////////////////////////
//  Robot exploration application entry point
//  Main program for the MRPiZ robot control
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use common::hardwareAbstraction::{
    get_battery_level, get_battery_voltage, init_robot, init_robot_with_error_info, move_backward,
    move_forward, read_proximity_sensor, set_led_color, turn_left, turn_right,
};
use common::xAssert::xAssert;
use common::xLog::{
    initialize, write_log, LogConfig, XOS_LOG_ERROR, XOS_LOG_MUTEX_ERROR, XOS_LOG_OK,
};
use common::xWatchdog::Watchdog;

fn main() {
    write_log("Démarrage du robot");

    // Initialisation du système de journalisation
    let log_config = LogConfig::new("explo.log");

    let mut log_initialized: u8 = 0;
    log_initialized = initialize(log_config);

    // Vérifier si l'initialisation a réussi
    xAssert(log_initialized == XOS_LOG_OK);

    // robot initialization
    if let Err(e) = init_robot() {
        write_log(&format!("Erreur d'initialisation du robot: {}", e));
        return;
    }

    // watchdog initialization
    let watchdog = Watchdog::new(1000, ThreadConfig::new("watchdog", 1, 1024, || {}));
    xAssert(init(watchdog).is_ok());

    // Exemple : avancer
    if let Err(e) = move_forward(50) {
        write_log(&format!("Erreur lors de l'avance: {}", e));
    }

    // Attendre quelques secondes
    common::xOs::sleep_ms(3000);

    // Arrêter le robot
    if let Err(e) = common::hardwareAbstraction::stop() {
        write_log(&format!("Erreur lors de l'arrêt: {}", e));
    }

    // Afficher la date et l'heure
    write_log("Fin du programme");
}

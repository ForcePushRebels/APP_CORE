////////////////////////////////////////////////////////////
//  Common library entry point
//  Exports core modules for robot control
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

pub mod x_assert;
pub mod x_log;
pub mod hardware; 
pub mod network;
pub mod sensor_manager;

// Module optionnel - décommentez si vous voulez utiliser le watchdog
pub mod x_watchdog;

pub use network::server::ServerConfig;


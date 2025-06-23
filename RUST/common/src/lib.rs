////////////////////////////////////////////////////////////
//  Common library
//  Exports common functionality for the project
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
// Updated: Added network module with callback system
////////////////////////////////////////////////////////////

pub mod hardware;
pub mod sensor_manager;
pub mod x_assert;
pub mod x_log;
pub mod x_watchdog;
pub mod network;

// Re-export des fonctions principales pour faciliter l'utilisation
pub use x_log::write_log;
pub use x_watchdog::{init_watchdog, refresh_watchdog, stop_watchdog, Watchdog};
pub use network::handle_network::{
    NetworkHandler, NetworkResponse, MessageCallback, 
    init_network_handler, get_network_handler, setup_default_callbacks
};
pub use network::server::{Server, ServerConfig};
pub use network::converter::{Converter, NetworkMessageType};


////////////////////////////////////////////////////////////
//  Server header file
//  Defines server configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////

/// Configuration du serveur
pub struct ServerConfig {
    pub port: u16,
    pub max_clients: u32,
    pub max_connections: u32,
    pub max_threads: u32,
    pub max_buffer_size: u32,
    pub max_packet_size: u32,
}

impl ServerConfig {
    pub fn new(port: u16) -> Self {
        Self {
            port,
            max_clients: 10,
            max_connections: 100,
            max_threads: 4,
            max_buffer_size: 4096,
            max_packet_size: 1024,
        }
    }
}
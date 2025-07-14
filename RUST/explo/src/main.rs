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

// Example of custom callback for movement commands
fn custom_movement_callback(msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log("=== Custom movement callback called ===");
    
    if message.data.len() < 2 {
        write_log("Invalid movement command: insufficient data");
        return None;
    }
    
    let direction = message.data[0];
    let speed = message.data[1];
    
    write_log(&format!("Custom movement - Direction: {}, Speed: {}", direction, speed));
    
    // Here you can call your specific hardware functions
    match direction {
        1 => write_log("Move forward!"),
        2 => write_log("Move backward!"),
        3 => write_log("Turn left!"),
        4 => write_log("Turn right!"),
        0 => write_log("Stop!"),
        _ => write_log("Unknown direction"),
    }
    
    // Return a custom status message
    let status_data = vec![direction, speed]; // Echo of commands
    let length = 1 + status_data.len() as u16;
    Some(Converter::new(length, NetworkMessageType::IdInfStatus as u8, status_data))
}

fn main() {
    // Initialize logging system
    let log_config = LogConfig::new("explo.log");
    let log_result = initialize(log_config);
    x_assert(log_result == XOS_LOG_OK);
    
    write_log("=== Starting exploration program ===");
    
    // Initialize network callback system
    write_log("Configuring network callbacks...");
    
    // 1. Configure default callbacks
    setup_default_callbacks();
    
    // 2. Replace movement callback with our custom version
    let handler = get_network_handler();
    handler.register_callback(NetworkMessageType::IdSetMovement, custom_movement_callback);
    
    // 3. Add custom callback for robot discovery
    handler.register_callback(NetworkMessageType::IdIsAnyRobotHere, |_msg_type, _message| {
        write_log("Robot discovered! Sending information...");
        
        // Create a custom manifest
        let manifest_data = format!("Robot-MRPiZ-ExplorBot-v1.0-ID:{}", 
                                    std::process::id()).into_bytes();
        let length = 1 + manifest_data.len() as u16;
        
        Some(Converter::new(length, NetworkMessageType::IdManifest as u8, manifest_data))
    });
    
    write_log("Network callbacks configured!");
    
    // Create and initialize robot
    let mut robot = Robot::new();
    
    match robot.init() 
    {
        Ok(()) => {
            write_log("Robot initialized successfully");
            
            // Initialize luminosity sensor
            if let Err(e) = luminosity::init() 
            {
                write_log(&format!("Luminosity sensor init error: {}", e));
            }
            
        },
        Err(e) => 
        {
            write_log(&format!("Robot initialization error: {}", e));
            x_assert(false); // Force stop if robot cannot be initialized
        }
    }

    // Start watchdog with custom handler
    let mut watchdog = Watchdog::new(300); 
    watchdog.set_expiry_handler(|| {
        write_log("Watchdog expired - System not responding!");
    });
    
    if let Err(e) = init_watchdog(watchdog) {
        write_log(&format!("Watchdog init error: {}", e));
    }

    // Start sensor manager
    let mut sensor_manager = SensorManager::new();
    x_assert(sensor_manager.start().is_ok());

    // Start server on dedicated thread
    let mut server = Server::new(ServerConfig::new("0.0.0.0".to_string(), 8080)).unwrap();
    x_assert(server.start().is_ok());

    write_log("Network server started on port 8080");
    write_log("Robot is ready to receive commands!");

    let mut result: Result<(), &'static str>;
    loop 
    {
        result = refresh_watchdog();
        if result.is_err() {
            write_log("Watchdog expired - System not responding!");
            break;
        }
        
        // Small pause to avoid CPU overload
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    
    write_log("=== End of exploration program ===");
    
    // Clean server shutdown
    if server.stop() != common::network::server::SERVER_OK {
        write_log("Error during server shutdown");
    }
}


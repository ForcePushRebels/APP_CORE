////////////////////////////////////////////////////////////
//  Network message handler
//  Handles incoming network messages according to protocol
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
// Updated: Added callback system and response message support
////////////////////////////////////////////////////////////

use crate::x_log::write_log;
use crate::network::converter::{Converter, NetworkMessageType, convert_to_struct};
use crate::network::server::SERVER_OK;
use std::sync::{Arc, Mutex};

/// Type for message handling callbacks
pub type MessageCallback = dyn Fn(NetworkMessageType, &Converter) -> Option<Converter> + Send + Sync;

/// Structure to manage callbacks and responses
pub struct NetworkHandler {
    callbacks: Arc<Mutex<std::collections::HashMap<NetworkMessageType, Box<MessageCallback>>>>,
}

impl NetworkHandler {
    pub fn new() -> Self {
        Self {
            callbacks: Arc::new(Mutex::new(std::collections::HashMap::new())),
        }
    }

    /// Registers a callback for a specific message type
    pub fn register_callback<F>(&self, msg_type: NetworkMessageType, callback: F)
    where
        F: Fn(NetworkMessageType, &Converter) -> Option<Converter> + Send + Sync + 'static,
    {
        if let Ok(mut callbacks) = self.callbacks.lock() {
            callbacks.insert(msg_type, Box::new(callback));
            write_log(&format!("Callback registered for message type: {:?}", msg_type));
        } else {
            write_log("Error registering callback: poisoned mutex");
        }
    }

    /// Processes a message with the appropriate callback
    pub fn handle_message(&self, msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
        match self.callbacks.lock() {
            Ok(callbacks) => {
                if let Some(callback) = callbacks.get(&msg_type) {
                    callback(msg_type, message)
                } else {
                    write_log(&format!("No callback registered for type: {:?}", msg_type));
                    None
                }
            }
            Err(poisoned) => {
                write_log("Callbacks mutex poisoned, recovering...");
                let callbacks = poisoned.into_inner();
                if let Some(callback) = callbacks.get(&msg_type) {
                    callback(msg_type, message)
                } else {
                    None
                }
            }
        }
    }
}

/// Global network handler instance
static mut NETWORK_HANDLER: Option<NetworkHandler> = None;
static INIT: std::sync::Once = std::sync::Once::new();

/// Initializes the global network handler
pub fn init_network_handler() -> &'static NetworkHandler {
    unsafe {
        INIT.call_once(|| {
            NETWORK_HANDLER = Some(NetworkHandler::new());
        });
        NETWORK_HANDLER.as_ref().unwrap()
    }
}

/// Gets a reference to the global network handler
pub fn get_network_handler() -> &'static NetworkHandler {
    unsafe {
        NETWORK_HANDLER.as_ref().expect("Network handler not initialized - call init_network_handler() first")
    }
}

/// Structure to encapsulate a network response
#[derive(Debug)]
pub struct NetworkResponse {
    pub response_message: Option<Converter>,
    pub status_code: u32,
}

impl NetworkResponse {
    pub fn success(message: Option<Converter>) -> Self {
        Self {
            response_message: message,
            status_code: SERVER_OK,
        }
    }

    pub fn error(status_code: u32) -> Self {
        Self {
            response_message: None,
            status_code,
        }
    }
}

/// Processes a received network message and returns an optional response
pub fn handle_incoming_message(buffer: &[u8], bytes_received: usize) -> NetworkResponse {
    write_log(&format!("Processing message of {} bytes", bytes_received));
    
    // Convert buffer to structure
    match convert_to_struct(&buffer[..bytes_received]) {
        Ok(message) => {
            write_log(&format!(
                "Message decoded - Length: {}, ID: 0x{:02X}, Data: {} bytes",
                message.length, message.idx, message.data.len()
            ));
            
            // Process according to message type
            match message.get_message_type() {
                Some(msg_type) => {
                    let handler = get_network_handler();
                    let response_message = handler.handle_message(msg_type, &message);
                    NetworkResponse::success(response_message)
                },
                None => {
                    write_log(&format!("Unknown message type: 0x{:02X}", message.idx));
                    NetworkResponse::success(None)
                }
            }
        },
        Err(error) => {
            write_log(&format!("Error decoding message: 0x{:08X}", error));
            NetworkResponse::error(error)
        }
    }
}

/// Default callback functions for different message types

/// Default callback for movement commands
pub fn default_movement_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    if message.data.len() < 2 {
        write_log("Invalid movement command: insufficient data");
        return None;
    }
    
    let direction = message.data[0];
    let speed = message.data[1];
    
    write_log(&format!("Movement - Direction: {}, Speed: {}", direction, speed));
    
    // Here you can call hardware module functions
    // Example:
    // match direction {
    //     1 => motors::move_forward(speed),
    //     2 => motors::move_backward(speed),
    //     3 => motors::turn_left(speed),
    //     4 => motors::turn_right(speed),
    //     0 => motors::stop(),
    //     _ => { write_log("Unknown direction"); }
    // }
    
    // Return confirmation message
    Some(create_status_info_message(1)) // 1 = movement in progress
}

/// Default callback for mission control
pub fn default_mission_control_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    if message.data.is_empty() {
        write_log("Invalid mission command: no data");
        return None;
    }
    
    let command = message.data[0];
    write_log(&format!("Mission control - Command: {}", command));
    
    // Process according to command
    let status = match command {
        1 => { write_log("Mission start"); 2 }, // mission active
        2 => { write_log("Mission stop"); 0 },     // mission stopped
        3 => { write_log("Mission pause"); 3 },     // mission paused
        4 => { write_log("Mission resume"); 2 },   // mission active
        _ => { write_log("Unknown mission command"); 255 }, // error
    };
    
    Some(create_status_info_message(status))
}

/// Default callback for selected points
pub fn default_selected_points_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log(&format!("Selected points received: {} bytes of data", message.data.len()));
    
    // Points are generally encoded as coordinates (x,y)
    let num_points = if message.data.len() % 4 == 0 {
        let num = message.data.len() / 4;
        write_log(&format!("Number of points (u16): {}", num));
        num
    } else if message.data.len() % 8 == 0 {
        let num = message.data.len() / 8;
        write_log(&format!("Number of points (u32): {}", num));
        num
    } else {
        write_log("Unrecognized point format");
        0
    };
    
    // Return confirmation message with number of processed points
    if num_points > 0 {
        Some(create_status_info_message(4)) // 4 = points received and processed
    } else {
        None
    }
}

/// Default callback for map upload
pub fn default_upload_map_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log(&format!("Map data received: {} bytes", message.data.len()));
    
    // Here you could save the map data
    // or process it according to your map format
    
    // Return confirmation message
    Some(create_status_info_message(5)) // 5 = map received and processed
}

/// Default callback for robot discovery
pub fn default_robot_discovery_callback(_msg_type: NetworkMessageType, _message: &Converter) -> Option<Converter> {
    write_log("Robot discovery response - Sending manifest");
    
    // Create simple manifest message
    let manifest_data = b"Robot-MRPiZ-v1.0".to_vec();
    let length = 1 + manifest_data.len() as u16;
    
    Some(Converter::new(length, NetworkMessageType::IdManifest as u8, manifest_data))
}

/// Default callback for manifest
pub fn default_manifest_callback(_msg_type: NetworkMessageType, message: &Converter) -> Option<Converter> {
    write_log(&format!("Manifest received: {} bytes", message.data.len()));
    
    // Parse manifest according to your format
    if let Ok(manifest_str) = std::str::from_utf8(&message.data) {
        write_log(&format!("Manifest: {}", manifest_str));
    }
    
    None // No response needed for manifest
}

/// Initialize default callbacks
pub fn setup_default_callbacks() {
    let handler = init_network_handler();
    
    handler.register_callback(NetworkMessageType::IdSetMovement, default_movement_callback);
    handler.register_callback(NetworkMessageType::IdMissionControl, default_mission_control_callback);
    handler.register_callback(NetworkMessageType::IdSelectedPoints, default_selected_points_callback);
    handler.register_callback(NetworkMessageType::IdUploadMap, default_upload_map_callback);
    handler.register_callback(NetworkMessageType::IdIsAnyRobotHere, default_robot_discovery_callback);
    handler.register_callback(NetworkMessageType::IdManifest, default_manifest_callback);
    
    write_log("Default callbacks configured");
}

/// Creates a response message for battery information
pub fn create_battery_info_message(level: u8, voltage: f32) -> Converter {
    let mut data = Vec::with_capacity(5);
    data.push(level);
    data.extend_from_slice(&voltage.to_le_bytes());
    
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfBattery as u8, data)
}

/// Creates a response message for status
pub fn create_status_info_message(status: u8) -> Converter {
    let data = vec![status];
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfStatus as u8, data)
}

/// Creates a response message for position
pub fn create_position_info_message(x: f32, y: f32, theta: f32) -> Converter {
    let mut data = Vec::with_capacity(12);
    data.extend_from_slice(&x.to_le_bytes());
    data.extend_from_slice(&y.to_le_bytes());
    data.extend_from_slice(&theta.to_le_bytes());
    
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfPos as u8, data)
}

/// Creates a response message for time
pub fn create_time_info_message(timestamp: u64) -> Converter {
    let data = timestamp.to_le_bytes().to_vec();
    let length = 1 + data.len() as u16; // idx + data
    
    Converter::new(length, NetworkMessageType::IdInfTime as u8, data)
}

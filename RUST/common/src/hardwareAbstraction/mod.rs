////////////////////////////////////////////////////////////
//  Hardware Abstraction header file
//  Defines hardware interface types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

pub mod mrpiz;
pub use mrpiz::*;

/// Initialise les interfaces matérielles
pub fn init_hardware() -> Result<(), &'static str> {
    unimplemented!()
}

/// Accède au périphérique spécifié
pub fn access_device(device_id: u32) -> Result<&'static mut dyn DeviceInterface, &'static str> {
    unimplemented!()
}

/// Trait pour les interfaces de périphériques
pub trait DeviceInterface {
    fn read(&self) -> Result<u32, &'static str>;
    fn write(&mut self, value: u32) -> Result<(), &'static str>;
} 
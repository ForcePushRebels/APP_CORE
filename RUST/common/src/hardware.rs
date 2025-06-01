////////////////////////////////////////////////////////////
//  Hardware interface for MRPiZ and LumPiZ
//  Simplified hardware abstraction layer
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use mrpiz_bindings as ffi;
use std::io::{Result, Error, ErrorKind};
use crate::x_log::write_log;

/// Configuration INTOX pour la simulation
const INTOX_ADDRESS: &str = "127.0.0.1\0";
const INTOX_PORT: i32 = 12345;

/// Structure pour gérer l'état du robot
pub struct Robot {
    initialized: bool,
}

impl Robot {
    /// Crée une nouvelle instance du robot
    pub fn new() -> Self {
        Self {
            initialized: false,
        }
    }

    /// Initialise la connexion avec le robot MRPiZ
    pub fn init(&mut self) -> Result<()> {
        if self.initialized {
            return Ok(());
        }

        let ret = unsafe { 
            ffi::mrpiz::mrpiz_init_intox(INTOX_ADDRESS.as_ptr() as *const _, INTOX_PORT) 
        };
        
        write_log(&format!("Robot initialization result: {}", ret));
        
        if ret == 0 {
            self.initialized = true;
            write_log("Robot initialized successfully");
            Ok(())
        } else {
            let error_msg = unsafe { 
                std::ffi::CStr::from_ptr(ffi::mrpiz::mrpiz_error_msg())
                    .to_string_lossy()
                    .into_owned() 
            };
            write_log(&format!("Robot initialization failed: {}", error_msg));
            Err(Error::new(ErrorKind::Other, format!("Robot init failed: {}", error_msg)))
        }
    }

    /// Ferme la connexion avec le robot
    pub fn close(&mut self) {
        if self.initialized {
            unsafe { ffi::mrpiz::mrpiz_close() };
            self.initialized = false;
            write_log("Robot connection closed");
        }
    }

    /// Vérifie si le robot est initialisé
    pub fn is_initialized(&self) -> bool {
        self.initialized
    }
}

impl Drop for Robot {
    fn drop(&mut self) {
        self.close();
    }
}

/// Fonctions de contrôle des moteurs
pub mod motors {
    use super::*;

    /// Fait avancer le robot à la vitesse spécifiée (0-100)
    pub fn move_forward(speed: u8) -> Result<()> {
        let speed = speed.min(100) as i32;
        let ret = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, speed) 
        };
        if ret == 0 {
            write_log(&format!("Moving forward at speed: {}", speed));
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to move forward"))
        }
    }

    /// Fait reculer le robot à la vitesse spécifiée (0-100)
    pub fn move_backward(speed: u8) -> Result<()> {
        let speed = speed.min(100) as i32;
        let ret = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, -speed) 
        };
        if ret == 0 {
            write_log(&format!("Moving backward at speed: {}", speed));
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to move backward"))
        }
    }

    /// Fait tourner le robot à gauche
    pub fn turn_left(speed: u8) -> Result<()> {
        let speed = speed.min(100) as i32;
        let ret_left = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_LEFT, -speed) 
        };
        let ret_right = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_RIGHT, speed) 
        };
        if ret_left == 0 && ret_right == 0 {
            write_log(&format!("Turning left at speed: {}", speed));
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to turn left"))
        }
    }

    /// Fait tourner le robot à droite
    pub fn turn_right(speed: u8) -> Result<()> {
        let speed = speed.min(100) as i32;
        let ret_left = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_LEFT, speed) 
        };
        let ret_right = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_RIGHT, -speed) 
        };
        if ret_left == 0 && ret_right == 0 {
            write_log(&format!("Turning right at speed: {}", speed));
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to turn right"))
        }
    }

    /// Arrête tous les moteurs
    pub fn stop() -> Result<()> {
        let ret = unsafe { 
            ffi::mrpiz::mrpiz_motor_set(ffi::mrpiz::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, 0) 
        };
        if ret == 0 {
            write_log("Motors stopped");
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to stop motors"))
        }
    }
}

/// Fonctions de lecture des capteurs
pub mod sensors {
    use super::*;

    /// ID des capteurs de proximité
    #[derive(Copy, Clone)]
    pub enum ProximitySensor {
        FrontLeft,
        FrontCenterLeft,
        FrontCenter,
        FrontCenterRight,
        FrontRight,
    }

    impl ProximitySensor {
        fn to_ffi_id(&self) -> ffi::mrpiz::mrpiz_proxy_sensor_id::Type {
            match self {
                ProximitySensor::FrontLeft => ffi::mrpiz::mrpiz_proxy_sensor_id::MRPIZ_PROXY_SENSOR_FRONT_LEFT,
                ProximitySensor::FrontCenterLeft => ffi::mrpiz::mrpiz_proxy_sensor_id::MRPIZ_PROXY_SENSOR_FRONT_CENTER_LEFT,
                ProximitySensor::FrontCenter => ffi::mrpiz::mrpiz_proxy_sensor_id::MRPIZ_PROXY_SENSOR_FRONT_CENTER,
                ProximitySensor::FrontCenterRight => ffi::mrpiz::mrpiz_proxy_sensor_id::MRPIZ_PROXY_SENSOR_FRONT_CENTER_RIGHT,
                ProximitySensor::FrontRight => ffi::mrpiz::mrpiz_proxy_sensor_id::MRPIZ_PROXY_SENSOR_FRONT_RIGHT,
            }
        }
    }

    /// Lit la valeur d'un capteur de proximité (0-255)
    pub fn read_proximity(sensor: ProximitySensor) -> Result<u8> {
        let ret = unsafe { 
            ffi::mrpiz::mrpiz_proxy_sensor_get(sensor.to_ffi_id()) 
        };
        if ret >= 0 {
            Ok(ret as u8)
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to read proximity sensor"))
        }
    }

    /// Lit toutes les valeurs des capteurs de proximité
    pub fn read_all_proximity() -> Result<[u8; 5]> {
        let sensors = [
            ProximitySensor::FrontLeft,
            ProximitySensor::FrontCenterLeft,
            ProximitySensor::FrontCenter,
            ProximitySensor::FrontCenterRight,
            ProximitySensor::FrontRight,
        ];
        
        let mut values = [0u8; 5];
        for (i, sensor) in sensors.iter().enumerate() {
            values[i] = read_proximity(*sensor)?;
        }
        Ok(values)
    }
}

/// Fonctions de contrôle de la LED
pub mod led {
    use super::*;

    /// Couleurs disponibles pour la LED
    pub enum Color {
        Off,
        Red,
        Green,
        Blue,
    }

    impl Color {
        fn to_ffi_color(&self) -> ffi::mrpiz::mrpiz_led_rgb_color_t::Type {
            match self {
                Color::Off => ffi::mrpiz::mrpiz_led_rgb_color_t::MRPIZ_LED_OFF,
                Color::Red => ffi::mrpiz::mrpiz_led_rgb_color_t::MRPIZ_LED_RED,
                Color::Green => ffi::mrpiz::mrpiz_led_rgb_color_t::MRPIZ_LED_GREEN,
                Color::Blue => ffi::mrpiz::mrpiz_led_rgb_color_t::MRPIZ_LED_BLUE,
            }
        }
    }

    /// Change la couleur de la LED
    pub fn set_color(color: Color) -> Result<()> {
        let ret = unsafe { 
            ffi::mrpiz::mrpiz_led_rgb_set(color.to_ffi_color()) 
        };
        if ret == 0 {
            write_log(&format!("LED color changed"));
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to set LED color"))
        }
    }
}

/// Fonctions de lecture de la batterie
pub mod battery {
    use super::*;

    /// Récupère le niveau de batterie (0-100%)
    pub fn get_level() -> Result<u8> {
        let ret = unsafe { ffi::mrpiz::mrpiz_battery_level() };
        if ret >= 0 {
            Ok(ret as u8)
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to read battery level"))
        }
    }

    /// Récupère la tension de la batterie (en volts)
    pub fn get_voltage() -> Result<f32> {
        let ret = unsafe { ffi::mrpiz::mrpiz_battery_voltage() };
        if ret >= 0.0 {
            Ok(ret)
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to read battery voltage"))
        }
    }
}

/// Module pour le capteur de luminosité (LumPiZ)
pub mod luminosity {
    use super::*;

    /// Initialise le capteur de luminosité
    pub fn init() -> Result<()> {
        let ret = unsafe { ffi::lumpiz::lumpiz_luminosity_init() };
        if ret == 0 {
            write_log("Luminosity sensor initialized");
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to initialize luminosity sensor"))
        }
    }

    /// Active ou désactive l'éclairage du capteur
    pub fn set_light(enabled: bool) -> Result<()> {
        let state = if enabled {
            ffi::lumpiz::lumpiz_luminosity_state_t::LUMPIZ_LUMINOSITY_ON
        } else {
            ffi::lumpiz::lumpiz_luminosity_state_t::LUMPIZ_LUMINOSITY_OFF
        };
        
        let ret = unsafe { ffi::lumpiz::lumpiz_luminosity_light_set(state) };
        if ret == 0 {
            write_log(&format!("Luminosity sensor light: {}", if enabled { "ON" } else { "OFF" }));
            Ok(())
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to set luminosity sensor light"))
        }
    }

    /// Lit la valeur du capteur de luminosité (0-1300)
    pub fn read() -> Result<u16> {
        let ret = unsafe { ffi::lumpiz::lumpiz_luminosity_get() };
        if ret >= 0 {
            Ok(ret as u16)
        } else {
            Err(Error::new(ErrorKind::Other, "Failed to read luminosity"))
        }
    }

    /// Vérifie si l'éclairage du capteur est activé
    pub fn is_light_on() -> Result<bool> {
        let ret = unsafe { ffi::lumpiz::lumpiz_luminosity_light_get() };
        Ok(ret == ffi::lumpiz::lumpiz_luminosity_state_t::LUMPIZ_LUMINOSITY_ON as u32)
    }
}

/// Fonction utilitaire pour faire une pause
pub fn sleep_ms(milliseconds: u32) {
    std::thread::sleep(std::time::Duration::from_millis(milliseconds as u64));
} 
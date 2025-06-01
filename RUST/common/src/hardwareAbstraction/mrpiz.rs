////////////////////////////////////////////////////////////
//  MRPiZ Robot interface header file
//  Defines robot control functions and bindings to C library
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use mrpiz_bindings as ffi;
use std::io::{Result, Error, ErrorKind};
use crate::xLog::write_log;

/// Initialise le robot MRPiZ
pub fn init_robot() -> Result<()> {
    let ret = unsafe { ffi::mrpiz_init_intox("127.0.0.1\0".as_ptr() as *const _, 12345) };
    write_log(&format!("mrpiz_init ret: {}", ret));
    if ret == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_init failed"))
    }
}

pub fn init_robot_with_error_info() -> Result<()> 
{
    let ret = unsafe { ffi::mrpiz_init_intox("127.0.0.1\0".as_ptr() as *const _, 12345) };
    write_log(&format!("mrpiz_init ret: {}", ret));
    
    if ret == 0 {
        Ok(())
    } else {
        // Get the error message
        let error_msg = unsafe { 
            std::ffi::CStr::from_ptr(ffi::mrpiz_error_msg())
                .to_string_lossy()
                .into_owned() 
        };
        let error_string = format!("mrpiz_init failed: {}", error_msg);
        write_log(&format!("Error message: {}", error_msg));
        Err(Error::new(ErrorKind::Other, error_string))
    }
}

/// Ferme la connexion avec le robot
pub fn close_robot() {
    unsafe { ffi::mrpiz_close() };
}

/// Fait avancer le robot à la vitesse spécifiée (0-100)
pub fn move_forward(speed: u8) -> Result<()> {
    let ret = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, speed as i32) };
    if ret == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_motor_set (forward) failed"))
    }
}

/// Fait reculer le robot à la vitesse spécifiée (0-100)
pub fn move_backward(speed: u8) -> Result<()> {
    let ret = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, -(speed as i32)) };
    if ret == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_motor_set (backward) failed"))
    }
}

/// Fait tourner le robot à gauche à la vitesse spécifiée (0-100)
pub fn turn_left(speed: u8) -> Result<()> {
    let ret_left = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_LEFT, -(speed as i32)) };
    let ret_right = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_RIGHT, speed as i32) };
    if ret_left == 0 && ret_right == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_motor_set (turn_left) failed"))
    }
}

/// Fait tourner le robot à droite à la vitesse spécifiée (0-100)
pub fn turn_right(speed: u8) -> Result<()> {
    let ret_left = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_LEFT, speed as i32) };
    let ret_right = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_RIGHT, -(speed as i32)) };
    if ret_left == 0 && ret_right == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_motor_set (turn_right) failed"))
    }
}

/// Arrête le robot (arrête les deux moteurs)
pub fn stop() -> Result<()> {
    let ret = unsafe { ffi::mrpiz_motor_set(ffi::mrpiz_motor_id::MRPIZ_MOTOR_BOTH, 0) };
    if ret == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_motor_set (stop) failed"))
    }
}

/// Récupère le niveau de batterie (0-100%)
pub fn get_battery_level() -> Result<u8> {
    let ret = unsafe { ffi::mrpiz_battery_level() };
    if ret >= 0 {
        Ok(ret as u8)
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_battery_level failed"))
    }
}

/// Récupère la tension de la batterie (en volts)
pub fn get_battery_voltage() -> Result<f32> {
    let ret = unsafe { ffi::mrpiz_battery_voltage() };
    if ret >= 0.0 {
        Ok(ret)
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_battery_voltage failed"))
    }
}

/// Contrôle la LED RGB (rouge, vert, bleu, ou off)
pub fn set_led_color(color: i32) -> Result<()> {
    let ret = unsafe { ffi::mrpiz_led_rgb_set(color as ffi::mrpiz_led_rgb_color_t::Type) };
    if ret == 0 {
        Ok(())
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_led_rgb_set failed"))
    }
}

/// Lit la valeur d'un capteur de proximité
pub fn read_proximity_sensor(sensor_id: i32) -> Result<u8> {
    let ret = unsafe { ffi::mrpiz_proxy_sensor_get(sensor_id as ffi::mrpiz_proxy_sensor_id::Type) };
    if ret >= 0 {
        Ok(ret as u8)
    } else {
        Err(Error::new(ErrorKind::Other, "mrpiz_proxy_sensor_get failed"))
    }
}

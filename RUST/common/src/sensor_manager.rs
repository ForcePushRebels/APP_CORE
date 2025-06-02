////////////////////////////////////////////////////////////
//  Sensor manager source file
//  Defines sensor manager configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////


use crate::x_log::write_log;
use crate::x_assert::x_assert;
use crate::hardware::sensors::ProximitySensor;
use crate::hardware::luminosity::{get_floor_luminosity, Luminosity};

use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::thread;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::JoinHandle;

pub struct SensorManager {
    pub proximity_sensors: [ProximitySensor; 5],
    pub luminosity_sensor: [Luminosity; 1],
    pub running: Arc<AtomicBool>,
    pub thread: Option<JoinHandle<()>>,
}

impl SensorManager {
    pub fn new() -> Self {
        SensorManager {
            proximity_sensors: [ProximitySensor::FrontLeft, ProximitySensor::FrontCenterLeft, ProximitySensor::FrontCenter, ProximitySensor::FrontCenterRight, ProximitySensor::FrontRight],
            luminosity_sensor: [Luminosity::get_floor_luminosity()],
            running: Arc::new(AtomicBool::new(false)),
            thread: None,
        }
    }

    //start a thread to read all sensors periodically
    pub fn start(&mut self) -> Result<(), u32> {
        self.running.store(true, Ordering::Relaxed);
        self.thread = Some(thread::spawn(move || {
            while self.running.load(Ordering::Relaxed) {

                //read all proximity sensors
                for i in 0..5 {
                    self.proximity_sensors[i] = sensors::read_proximity(self.proximity_sensors[i]);
                }
                
                //read luminosity sensor
                self.luminosity_sensor = sensors::get_floor_luminosity();
                thread::sleep(Duration::from_secs(1));
            }
        }));
        Ok(())
    }

    //stop the thread
    pub fn stop(&mut self) -> Result<(), u32> {
        self.running.store(false, Ordering::Relaxed);
        if let Some(thread) = self.thread.take() {
            thread.join().unwrap();
        }
        Ok(())
    }
}

//stop the thread when the sensor manager is dropped
impl Drop for SensorManager {
    fn drop(&mut self) {
        self.stop().unwrap();
    }
}










////////////////////////////////////////////////////////////
//  Sensor manager source file
//  Defines sensor manager configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////

use crate::hardware::luminosity;
use crate::hardware::sensors::ProximitySensor;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

/// Structure pour stocker les données des capteurs
pub struct SensorData {
    pub proximity_values: [u8; 5],
    pub luminosity_value: u16,
}

impl SensorData {
    pub fn new() -> Self {
        Self {
            proximity_values: [0; 5],
            luminosity_value: 0,
        }
    }
}

pub struct SensorManager {
    pub proximity_sensors: [ProximitySensor; 5],
    pub sensor_data: Arc<std::sync::Mutex<SensorData>>,
    pub running: Arc<AtomicBool>,
    pub thread: Option<JoinHandle<()>>,
}

impl SensorManager {
    pub fn new() -> Self {
        SensorManager {
            proximity_sensors: [
                ProximitySensor::FrontLeft,
                ProximitySensor::FrontCenterLeft,
                ProximitySensor::FrontCenter,
                ProximitySensor::FrontCenterRight,
                ProximitySensor::FrontRight,
            ],
            sensor_data: Arc::new(std::sync::Mutex::new(SensorData::new())),
            running: Arc::new(AtomicBool::new(false)),
            thread: None,
        }
    }

    /// Démarre un thread pour lire tous les capteurs périodiquement
    pub fn start(&mut self) -> Result<(), u32> {
        if self.running.load(Ordering::Relaxed) {
            return Err(1); // Déjà démarré
        }

        self.running.store(true, Ordering::Relaxed);

        let running = self.running.clone();
        let sensor_data = self.sensor_data.clone();
        let sensors = self.proximity_sensors.clone();

        self.thread = Some(thread::spawn(move || {
            while running.load(Ordering::Relaxed) {
                // Lire tous les capteurs de proximité
                let mut proximity_values = [0u8; 5];
                for (index, sensor) in sensors.iter().enumerate() {
                    match crate::hardware::sensors::read_proximity(*sensor) {
                        Ok(value) => proximity_values[index] = value,
                        Err(_) => {
                            // En cas d'erreur, garder la valeur précédente
                        }
                    }
                }

                // Lire le capteur de luminosité
                let luminosity_value = match luminosity::get_floor_luminosity() {
                    Ok(value) => value,
                    Err(_) => 0, // Valeur par défaut en cas d'erreur
                };

                // Mettre à jour les données partagées
                if let Ok(mut data) = sensor_data.lock() {
                    data.proximity_values = proximity_values;
                    data.luminosity_value = luminosity_value;
                }

                thread::sleep(Duration::from_millis(100)); // Lecture toutes les 100ms
            }
        }));

        Ok(())
    }

    /// Arrête le thread de lecture des capteurs
    pub fn stop(&mut self) -> Result<(), u32> {
        self.running.store(false, Ordering::Relaxed);
        if let Some(thread) = self.thread.take() {
            thread.join().unwrap_or(());
        }
        Ok(())
    }

    /// Récupère les dernières données des capteurs
    pub fn get_sensor_data(&self) -> Option<SensorData> {
        if let Ok(data) = self.sensor_data.lock() {
            Some(SensorData {
                proximity_values: data.proximity_values,
                luminosity_value: data.luminosity_value,
            })
        } else {
            None
        }
    }
}

/// Arrête le thread automatiquement quand le sensor manager est détruit
impl Drop for SensorManager {
    fn drop(&mut self) {
        let _ = self.stop();
    }
}

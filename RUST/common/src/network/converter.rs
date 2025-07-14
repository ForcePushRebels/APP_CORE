////////////////////////////////////////////////////////////
//  Converter source file
//  Defines converter configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////

use crate::x_log::write_log;
use crate::x_assert::x_assert;
use crate::network::server::SERVER_BUFFER_ERROR;

#[derive(Debug)]
pub struct Converter {
    pub length: u16,
    pub idx: u8, 
    pub data: Vec<u8>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NetworkMessageType {
    //====== SEND BY ANDROID ======
    IdSetMovement = 0x01,
    IdMissionControl = 0x02,
    IdSelectedPoints = 0x05,
    IdUploadMap = 0x07,

    //====== SEND BY BOT ======
    IdInfBattery = 0x0A,
    IdInfStatus = 0x0B,
    IdInfPos = 0x0C,
    IdInfTime = 0x0D,

    IdMapFragment = 0x20,
    IdMapFull = 0x21,

    //=============================
    // UDP:
    IdIsAnyRobotHere = 0x30,
    IdManifest = 0x31,
}

impl NetworkMessageType {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0x01 => Some(NetworkMessageType::IdSetMovement),
            0x02 => Some(NetworkMessageType::IdMissionControl),
            0x05 => Some(NetworkMessageType::IdSelectedPoints),
            0x07 => Some(NetworkMessageType::IdUploadMap),
            0x0A => Some(NetworkMessageType::IdInfBattery),
            0x0B => Some(NetworkMessageType::IdInfStatus),
            0x0C => Some(NetworkMessageType::IdInfPos),
            0x0D => Some(NetworkMessageType::IdInfTime),
            0x20 => Some(NetworkMessageType::IdMapFragment),
            0x21 => Some(NetworkMessageType::IdMapFull),
            0x30 => Some(NetworkMessageType::IdIsAnyRobotHere),
            0x31 => Some(NetworkMessageType::IdManifest),
            _ => None,
        }
    }
}

impl Converter {
    pub fn new(length: u16, idx: u8, data: Vec<u8>) -> Self {
        x_assert(length > 0);
        x_assert(data.len() <= length as usize);
        
        Self { length, idx, data }
    }

    /// Convertit la structure en bytes selon le format :
    /// Bytes 0-1 : length (u16, little-endian)
    /// Byte 2 : idx (u8)
    /// Bytes 3+ : data (Vec<u8>)
    pub fn convert_to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(3 + self.data.len());
        
        // Bytes 0-1 : longueur (u16, little-endian)
        bytes.extend_from_slice(&self.length.to_le_bytes());
        
        // Byte 2 : idx (u8)
        bytes.push(self.idx);
        
        // Bytes 3+ : data
        bytes.extend_from_slice(&self.data);
        
        bytes
    }

    pub fn get_message_type(&self) -> Option<NetworkMessageType> {
        if self.idx < 0x01 || self.idx > 0x31 {
            write_log("Erreur: idx invalide");
            return None;
        }
        NetworkMessageType::from_u8(self.idx)
    }
}

/// Convertit un buffer de bytes en Converter selon le format :
/// Bytes 0-1 : length (u16, little-endian)
/// Byte 2 : idx (u8)
/// Bytes 3+ : data (Vec<u8>)
pub fn convert_to_struct(bytes: &[u8]) -> Result<Converter, u32> {
    // Check that we have at least 3 bytes (length + idx)
    if bytes.len() < 3 {
        write_log("Error: buffer too small to contain valid message (minimum 3 bytes)");
        return Err(SERVER_BUFFER_ERROR);
    }

    // Bytes 0-1: length (u16, little-endian)
    let length_bytes: [u8; 2] = bytes[0..2].try_into()
        .map_err(|_| SERVER_BUFFER_ERROR)?;
    let length = u16::from_le_bytes(length_bytes);

    // Byte 2: idx (u8)
    let idx = bytes[2];
    
    // Bytes 3+: data
    let data = bytes[3..].to_vec();

    // Validation: length must match total message size
    // length = 1 (idx) + data.len()
    let expected_length = 1 + data.len() as u16;
    if length != expected_length {
        write_log(&format!(
            "Error: inconsistent length - received: {}, expected: {} (1 + {} bytes of data)",
            length, expected_length, data.len()
        ));
        return Err(SERVER_BUFFER_ERROR);
    }

    Ok(Converter { length, idx, data })
}



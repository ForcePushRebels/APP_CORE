////////////////////////////////////////////////////////////
//  ID card handler
//  Handles ID card operations
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 01/06/2025
////////////////////////////////////////////////////////////

use crate::x_log::write_log;
use crate::network::handle_network;
use crate::network::handle_network::create_manifest_message;
use local_ip_address::local_ip;

struct IdCard 
{
    id: i32,
    name: String,
    ipAddr: u32,
}

const NAME: &str = "Dora l'exploratrice";
const ROLE: &i32 = 0; //explorateur

//get the ipv4 of the system
pub fn get_ip_addr() -> u32 {
    let ip_addr = local_ip().unwrap();
    let ip_addr_str = ip_addr.to_string();
    let ip_addr_parts = ip_addr_str.split(".").collect::<Vec<&str>>();
    let ip_addr_parts_int = ip_addr_parts.iter().map(|x| x.parse::<u16>().unwrap()).collect::<Vec<u16>>();
    let ip_addr_int = ip_addr_parts_int[0] << 24 | ip_addr_parts_int[1] << 16 | ip_addr_parts_int[2] << 8 | ip_addr_parts_int[3];
    return ip_addr_int;
}

pub fn handle_id_card(message: &Converter) -> u32 {
    let id_card = IdCard {
        id: 0,
        name: NAME.to_string(),
        ipAddr: get_ip_addr(),
    };

    //send the manifest to the android
    let manifest = create_manifest_message(id_card);
    handle_network::send_message(manifest);

    SERVER_OK
}

//register callback for the id card
pub fn register_callback(callback: fn(message: &Converter) -> u32) {
    handle_network::register_callback(NetworkMessageType::IdCard, callback);
}
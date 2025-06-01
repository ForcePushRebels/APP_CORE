////////////////////////////////////////////////////////////
//  OS abstraction header file
//  Defines OS-related types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

pub mod xThreads;
pub mod mutex;

/// État du système
pub struct SystemState 
{
    pub uptime_ms: u64,
    pub memory_usage_bytes: u64,
    pub cpu_usage_percent: f32,
}

/// Initialise les ressources du système d'exploitation
pub fn init() -> Result<(), &'static str> 
{
    unimplemented!()
}

/// Récupère l'état actuel du système
pub fn get_system_state() -> SystemState 
{
    unimplemented!()
}

/// Alloue de la mémoire
pub fn alloc(size: usize, alignment: usize) -> Result<*mut u8, &'static str> 
{
    unimplemented!()
}

/// Libère de la mémoire précédemment allouée
pub fn free(ptr: *mut u8) -> Result<(), &'static str> 
{
    unimplemented!()
}

/// Met le système en sommeil pour une durée spécifiée
pub fn sleep_ms(milliseconds: u32) 
{
    std::thread::sleep(std::time::Duration::from_millis(milliseconds as u64));
} 
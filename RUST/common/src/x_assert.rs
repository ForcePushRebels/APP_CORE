////////////////////////////////////////////////////////////
//  Assert header file
//  Defines assertion types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

//standard imports
use std::process;
use std::process::Command;
use std::sync::atomic::{AtomicU8, Ordering};

//project imports
use crate::x_log;

pub enum AssertMode {
    Continue,
    Exit,
    Loop,
    Restart,
}

// Configuration globale
static ASSERT_MODE: AtomicU8 = AtomicU8::new(1); // 1 = Exit par défaut

pub fn set_assert_mode(mode: AssertMode) {
    let mode_value = match mode {
        AssertMode::Continue => 0,
        AssertMode::Exit => 1,
        AssertMode::Loop => 2,
        AssertMode::Restart => 3,
    };

    ASSERT_MODE.store(mode_value, Ordering::SeqCst);
}

// Fonction publique simplifiée pour les assertions
#[track_caller]
pub fn x_assert(condition: bool) -> bool {
    if !condition {
        let loc = std::panic::Location::caller();
        _assert_impl(loc.file(), loc.line() as u32, None);
    }
    condition
}

// Macro pour les assertions
#[macro_export]
macro_rules! X_ASSERT {
    ($condition:expr) => {
        if !($condition) {
            $crate::x_assert::_assert_impl(file!(), line!(), None);
        }
    };

    ($condition:expr, $message:expr) => {
        if !($condition) {
            let loc = std::panic::Location::caller();
            $crate::x_assert::_assert_impl(loc.file(), loc.line(), Some($message));
        }
    };
}

// Simplified assertion logging function
fn x_log_assert(message: &str) 
{
    eprintln!("ASSERT | {}", message);
    x_log::write_log(&format!("ASSERT | {}", message));
}

// Implémentation interne de l'assertion
pub fn _assert_impl(file: &str, line: u32, message_opt: Option<&str>) {
    let mut message = String::new();
    message.push_str(file);
    message.push_str(" | ");
    message.push_str(&line.to_string());
    message.push_str(" | ");

    if let Some(msg) = message_opt {
        message.push_str(msg);
    }

    x_log::write_log(&message);

    // Management of different assertion modes
    let mode = ASSERT_MODE.load(Ordering::SeqCst);
    match mode {
        0 => {
            // Continue
            x_log_assert("Continuing execution after assertion");
        }
        1 => {
            // Exit
            x_log_assert("Exiting after assertion");
            process::exit(1);
        }
        2 => {
            // Loop
            x_log_assert("Entering infinite loop after assertion");
            loop {
                // Infinite loop
                std::thread::sleep(std::time::Duration::from_secs(1));
            }
        }
        3 => {
            // Restart
            x_log_assert("Restarting application after assertion");

            // Récupérer les arguments du programme actuel
            let args: Vec<String> = std::env::args().collect();
            let executable = &args[0];

            // Lancer une nouvelle instance du programme
            match Command::new(executable).args(&args[1..]).spawn() {
                Ok(_) => {
                    process::exit(0);
                }
                Err(e) => {
                    x_log_assert(&format!("Failed to restart program: {}", e));
                    process::exit(1);
                }
            }
        }
        _ => {
            // Default to exit
            x_log_assert("Unknown assertion mode, exiting");
            process::exit(1);
        }
    }
}

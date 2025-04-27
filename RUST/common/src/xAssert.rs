////////////////////////////////////////////////////////////
//  Assert header file
//  Defines assertion types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////


use std::process;

pub enum AssertMode 
{
    Continue,
    Exit,
    Loop,
    Restart,
}

// Configuration globale
pub static mut ASSERT_MODE: AssertMode = AssertMode::Exit;

// Macro pour les assertions
#[macro_export]
macro_rules! X_ASSERT 
{
    ($condition:expr) => {
        if !($condition) {
            $crate::xAssert(file!(), line!(), None);
        }
    };
    ($condition:expr, $message:expr) => {
        if !($condition) {
            $crate::xAssert(file!(), line!(), Some($message));
        }
    };
}

// Macro pour les assertions avec retour
#[macro_export]
macro_rules! X_ASSERT_return {
    ($condition:expr, $ret:expr) => {
        if !($condition) {
            return $crate::xAssertReturn(file!(), line!(), None, $ret);
        }
    };
    ($condition:expr, $message:expr, $ret:expr) => {
        if !($condition) {
            return $crate::xAssertReturn(file!(), line!(), Some($message), $ret);
        }
    };
}

// Fonction simplifiée de journalisation d'assertion
fn x_log_assert(message: &str) {
    eprintln!("ASSERT | {}", message);
}

pub fn xAssert(file: &str, line: u32, message: Option<&str>) {
    x_log_assert("Assertion failed");
    x_log_assert(&format!("Location: {}:{}", file, line));
    
    if let Some(msg) = message {
        x_log_assert(&format!("Message: {}", msg));
    }

    // Gestion des différents modes d'assertion
    unsafe {
        match ASSERT_MODE {
            AssertMode::Exit => {
                process::exit(1);
            },
            AssertMode::Loop => {
                loop {
                    // Boucle infinie
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
            },
            AssertMode::Restart => {
                x_log_assert("Restarting application");
                // Implémentation du redémarrage selon votre application
            },
            AssertMode::Continue => {
                // Continue l'exécution
            }
        }
    }
}

pub fn xAssertReturn<T>(file: &str, line: u32, message: Option<&str>, ret: T) -> T {
    x_log_assert("Assertion failed with return value");
    x_log_assert(&format!("Location: {}:{}", file, line));
    
    if let Some(msg) = message {
        x_log_assert(&format!("Message: {}", msg));
    }

    ret
}

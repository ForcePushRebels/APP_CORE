////////////////////////////////////////////////////////////
//  MRPiZ and LumPiZ C bindings entry point
//  Includes automatically generated FFI bindings for both libraries
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
// Updated: Support for both MRPiZ v0.6.1 and LumPiZ v0.1.1
////////////////////////////////////////////////////////////

#![allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]

/// Bindings pour la bibliothèque MRPiZ v0.6.1
/// Contrôle des moteurs, capteurs de proximité, LED et batterie
pub mod mrpiz {
    include!(concat!(env!("OUT_DIR"), "/mrpiz_bindings.rs"));
}

/// Bindings pour la bibliothèque LumPiZ v0.1.1  
/// Contrôle du capteur de luminosité
pub mod lumpiz {
    include!(concat!(env!("OUT_DIR"), "/lumpiz_bindings.rs"));
}

// Ré-export des fonctions principales pour faciliter l'utilisation
pub use mrpiz::*;
pub use lumpiz::*; 
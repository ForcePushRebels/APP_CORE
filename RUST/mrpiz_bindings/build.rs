////////////////////////////////////////////////////////////
//  MRPiZ C bindings build script
//  Generates Rust bindings for MRPiZ C library
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use std::path::PathBuf;
use std::env;

fn main() {
    // Chemin relatif vers le répertoire lib_mrpiz
    let lib_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent().unwrap()
        .join("lib_mrpiz").join("lib");

    let include_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
        .parent().unwrap()
        .join("lib_mrpiz").join("include").join("mrpiz");

    // Chemin des bibliothèques statiques
    println!("cargo:rustc-link-search=native={}", lib_path.display());
    println!("cargo:rustc-link-lib=static=intox");
    println!("cargo:rustc-link-lib=static=intoxmrpiz");
    println!("cargo:rustc-link-lib=static=mrpiz");

    // Chemins vers les headers
    let mrpiz_h = include_path.join("mrpiz.h");
    let error_h = include_path.join("error.h");

    // Définition des macros pour le mode simulateur (INTOX)
    // Ces macros doivent être identiques à celles utilisées dans ton CMake pour le build PC
    let bindings = bindgen::Builder::default()
        .header(mrpiz_h.to_str().unwrap())
        .header(error_h.to_str().unwrap())
        .clang_arg(format!("-I{}", include_path.display()))
        .clang_arg("-DINTOX")
        .clang_arg("-Dintox_address=\"127.0.0.1\"") // adapte si besoin
        .clang_arg("-Dintox_port=12345")            // adapte si besoin
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Génère les enums C comme modules de constantes
        .constified_enum_module("mrpiz_motor_id")
        .constified_enum_module("mrpiz_proxy_sensor_id")
        .constified_enum_module("mrpiz_led_rgb_color_t")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}

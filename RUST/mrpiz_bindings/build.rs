////////////////////////////////////////////////////////////
//  MRPiZ and LumPiZ C bindings build script
//  Generates Rust bindings for MRPiZ and LumPiZ C libraries
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
// Updated: Support for both MRPiZ v0.6.1 and LumPiZ v0.1.1
////////////////////////////////////////////////////////////

use std::path::PathBuf;
use std::env;

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    
    // Chemins pour MRPiZ v0.6.1 (dans le dossier libs)
    let mrpiz_lib_path = manifest_dir.parent().unwrap().join("libs").join("lib_mrpiz-x86_64-v0.6.1").join("lib");
    let mrpiz_include_path = manifest_dir.parent().unwrap().join("libs").join("lib_mrpiz-x86_64-v0.6.1").join("include").join("mrpiz");

    // Chemins pour LumPiZ v0.1.1 (dans le dossier lib_mrpiz)
    let lumpiz_lib_path = manifest_dir.parent().unwrap().join("lib_mrpiz").join("lib_lumpiz-x86_64-v0.1.1").join("lib");
    let lumpiz_include_path = manifest_dir.parent().unwrap().join("lib_mrpiz").join("lib_lumpiz-x86_64-v0.1.1").join("include").join("lumpiz");

    // Configuration des bibliothèques MRPiZ
    println!("cargo:rustc-link-search=native={}", mrpiz_lib_path.display());
    println!("cargo:rustc-link-lib=static=intox");
    println!("cargo:rustc-link-lib=static=intoxmrpiz");
    println!("cargo:rustc-link-lib=static=mrpiz");

    // Configuration des bibliothèques LumPiZ
    println!("cargo:rustc-link-search=native={}", lumpiz_lib_path.display());
    println!("cargo:rustc-link-lib=static=intoxlumpiz");
    println!("cargo:rustc-link-lib=static=lumpiz");

    // Génération des bindings pour MRPiZ
    let mrpiz_h = mrpiz_include_path.join("mrpiz.h");
    let mrpiz_error_h = mrpiz_include_path.join("error.h");

    let mrpiz_bindings = bindgen::Builder::default()
        .header(mrpiz_h.to_str().unwrap())
        .header(mrpiz_error_h.to_str().unwrap())
        .clang_arg(format!("-I{}", mrpiz_include_path.display()))
        .clang_arg("-DINTOX")
        .clang_arg("-Dintox_address=\"127.0.0.1\"")
        .clang_arg("-Dintox_port=12345")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Génère les enums C comme modules de constantes pour MRPiZ
        .constified_enum_module("mrpiz_motor_id")
        .constified_enum_module("mrpiz_proxy_sensor_id")
        .constified_enum_module("mrpiz_led_rgb_color_t")
        .constified_enum_module("mrpiz_error_t")
        .generate()
        .expect("Unable to generate MRPiZ bindings");

    // Génération des bindings pour LumPiZ
    let lumpiz_h = lumpiz_include_path.join("lumpiz.h");
    let lumpiz_error_h = lumpiz_include_path.join("error.h");

    let lumpiz_bindings = bindgen::Builder::default()
        .header(lumpiz_h.to_str().unwrap())
        .header(lumpiz_error_h.to_str().unwrap())
        .clang_arg(format!("-I{}", lumpiz_include_path.display()))
        .clang_arg("-DINTOX")
        .clang_arg("-Dintox_address=\"127.0.0.1\"")
        .clang_arg("-Dintox_port=12345")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Génère les enums C comme modules de constantes pour LumPiZ
        .constified_enum_module("lumpiz_luminosity_state_t")
        .constified_enum_module("lumpiz_error_t")
        .generate()
        .expect("Unable to generate LumPiZ bindings");

    // Écriture des fichiers de bindings
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    
    mrpiz_bindings
        .write_to_file(out_path.join("mrpiz_bindings.rs"))
        .expect("Couldn't write MRPiZ bindings!");

    lumpiz_bindings
        .write_to_file(out_path.join("lumpiz_bindings.rs"))
        .expect("Couldn't write LumPiZ bindings!");
}

use std::env;
use std::path::PathBuf;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    
    // Génère le header C pour l'interopérabilité
    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .with_pragma_once(true)
        .with_include_guard("RUST_TLS_ENGINE_H")
        .with_no_includes()
        .with_sys_include("stdint.h")
        .with_sys_include("stdbool.h")
        .with_sys_include("stddef.h")
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("include/rust_tls_engine.h");

    // Lie avec WolfSSL (assuming it's available on the system)
    println!("cargo:rustc-link-lib=wolfssl");
    
    // Set the include directory for the generated header
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    println!("cargo:include={}", out_path.display());
} 
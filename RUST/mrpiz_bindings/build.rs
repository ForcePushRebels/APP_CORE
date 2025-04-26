////////////////////////////////////////////////////////////
//  MRPiZ C bindings build script
//  Generates Rust bindings for MRPiZ C library
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

fn main() {
    println!("cargo:rustc-link-search=native=/home/christophe/pato/APP_CORE/RUST/lib_mrpiz/lib");
    println!("cargo:rustc-link-lib=static=intox");
    println!("cargo:rustc-link-lib=static=intoxmrpiz");
    println!("cargo:rustc-link-lib=static=mrpiz");

    let bindings = bindgen::Builder::default()
        .header("/home/christophe/pato/APP_CORE/RUST/lib_mrpiz/include/mrpiz/mrpiz.h")
        .header("/home/christophe/pato/APP_CORE/RUST/lib_mrpiz/include/mrpiz/error.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
} 
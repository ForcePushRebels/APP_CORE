# TLS Engine Integration Module
# Supports both C and Rust implementations with conditional compilation

function(setup_tls_engine TARGET_NAME)
    set(USE_RUST_TLS ${USE_RUST_TLS_ENGINE})
    
    if(USE_RUST_TLS)
        # Check Rust availability
        find_program(RUSTC rustc)
        find_program(CARGO cargo)
        
        if(NOT RUSTC OR NOT CARGO)
            message(FATAL_ERROR "Rust toolchain not found. Install Rust from https://rustup.rs/")
        endif()
        
        execute_process(
            COMMAND ${RUSTC} --version
            OUTPUT_VARIABLE RUST_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        
        set(RUST_TLS_DIR "${CMAKE_SOURCE_DIR}/rust/tls")
        set(RUST_TARGET_DIR "${RUST_TLS_DIR}/target")
        
        # Determine target architecture
        if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|amd64|AMD64)$")
            set(RUST_TARGET "x86_64-unknown-linux-gnu")
        elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm|ARM)$")
            set(RUST_TARGET "arm-unknown-linux-gnueabihf")
        elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(aarch64|arm64)$")
            set(RUST_TARGET "aarch64-unknown-linux-gnu")
        else()
            message(FATAL_ERROR "Unsupported architecture: ${CMAKE_SYSTEM_PROCESSOR}")
        endif()
        
        # Build profile selection
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            set(RUST_PROFILE "debug")
            set(CARGO_BUILD_FLAGS "")
        else()
            set(RUST_PROFILE "release")
            set(CARGO_BUILD_FLAGS "--release")
        endif()
        
        set(RUST_LIB_PATH "${RUST_TARGET_DIR}/${RUST_TARGET}/${RUST_PROFILE}/libtls_engine.a")
        
        # Build Rust TLS module
        execute_process(
            COMMAND ${CARGO} build --target ${RUST_TARGET} ${CARGO_BUILD_FLAGS}
            WORKING_DIRECTORY ${RUST_TLS_DIR}
            RESULT_VARIABLE CARGO_RESULT
            OUTPUT_VARIABLE CARGO_OUTPUT
            ERROR_VARIABLE CARGO_ERROR
        )
        
        if(NOT CARGO_RESULT EQUAL 0)
            message(FATAL_ERROR "Rust compilation failed:\n${CARGO_ERROR}\n${CARGO_OUTPUT}")
        endif()
        
        # Find WolfSSL (still needed for other C code)
        find_package(PkgConfig REQUIRED)
        pkg_check_modules(WOLFSSL REQUIRED wolfssl)
        
        # Configure target for Rust TLS
        get_target_property(TARGET_TYPE ${TARGET_NAME} TYPE)
        if(TARGET_TYPE STREQUAL "EXECUTABLE")
            target_include_directories(${TARGET_NAME} PRIVATE "${RUST_TLS_DIR}/include")
            target_link_libraries(${TARGET_NAME} ${RUST_LIB_PATH})
            target_link_libraries(${TARGET_NAME} ${WOLFSSL_LIBRARIES})
            target_include_directories(${TARGET_NAME} PRIVATE ${WOLFSSL_INCLUDE_DIRS})
            target_compile_options(${TARGET_NAME} PRIVATE ${WOLFSSL_CFLAGS_OTHER})
        else()
            message(FATAL_ERROR "Unsupported target type: ${TARGET_TYPE}")
        endif()
        
    else()
        # Find and link WolfSSL for C implementation
        find_package(PkgConfig REQUIRED)
        pkg_check_modules(WOLFSSL REQUIRED wolfssl)
        
        # Link WolfSSL for C implementation
        get_target_property(TARGET_TYPE ${TARGET_NAME} TYPE)
        if(TARGET_TYPE STREQUAL "EXECUTABLE")
            target_link_libraries(${TARGET_NAME} ${WOLFSSL_LIBRARIES})
            target_include_directories(${TARGET_NAME} PRIVATE ${WOLFSSL_INCLUDE_DIRS})
            target_compile_options(${TARGET_NAME} PRIVATE ${WOLFSSL_CFLAGS_OTHER})
        endif()
        
    endif()
endfunction()

# Function to exclude C TLS sources when using Rust
function(exclude_c_tls_sources SOURCES_VAR)
    if(USE_RUST_TLS_ENGINE)
        set(TLS_C_SOURCES
            "${CMAKE_SOURCE_DIR}/common/tls/certificate/certificateManagement.c"
            "${CMAKE_SOURCE_DIR}/common/tls/engine/tlsEngine.c"
        )
        
        set(FILTERED_SOURCES)
        foreach(SOURCE ${${SOURCES_VAR}})
            set(EXCLUDE_SOURCE FALSE)
            foreach(TLS_SOURCE ${TLS_C_SOURCES})
                if("${SOURCE}" STREQUAL "${TLS_SOURCE}")
                    set(EXCLUDE_SOURCE TRUE)
                    break()
                endif()
            endforeach()
            
            if(NOT EXCLUDE_SOURCE)
                list(APPEND FILTERED_SOURCES "${SOURCE}")
            endif()
        endforeach()
        
        set(${SOURCES_VAR} ${FILTERED_SOURCES} PARENT_SCOPE)
    endif()
endfunction()

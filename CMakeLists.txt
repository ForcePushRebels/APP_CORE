cmake_minimum_required(VERSION 3.10)
project(EXPLO)

# Force Ninja generator
set(CMAKE_GENERATOR "Ninja" CACHE INTERNAL "" FORCE)

# CMake presets configuration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Detect architecture
if(TARGET_AUDREN)
    # For Audren target, force aarch64 architecture
    set(TARGET_ARCH "aarch64")
    set(CMAKE_SYSTEM_PROCESSOR "aarch64")
elseif(TARGET_RASPI_ZERO_W)
    # For cross-compilation to Raspberry Pi, force ARM architecture
    set(TARGET_ARCH "armv6")
    set(CMAKE_SYSTEM_PROCESSOR "arm")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
    set(TARGET_ARCH "aarch64")
else()
    set(TARGET_ARCH "x86_64")
endif()

message(STATUS "Target architecture: ${TARGET_ARCH}")

# Target options
option(TARGET_RASPI_ZERO_W "Build for Raspberry Pi Zero W target (cross-compile)" OFF)
option(TARGET_AUDREN "Build for Audren target (aarch64 with GCC and custom wolfSSL)" OFF)
option(DEBUG "Enable debug build" OFF)
option(USE_3D_SIMULATION "Use 3D simulation instead of 2D (changes Intox port from 12345 to 12348)" OFF)

# Auto-detect and choose compiler: prefer Clang, fallback to GCC, force GCC for Audren
set(COMPILER_DETECTED "UNKNOWN")

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Simulation configuration
if(USE_3D_SIMULATION)
    set(INTOX_PORT 12301)
    set(SIMULATION_TYPE "3D")
    add_compile_definitions(SIMULATION_3D)
else()
    set(INTOX_PORT 12345)
    set(SIMULATION_TYPE "2D")
    add_compile_definitions(SIMULATION_2D)
endif()

set(INTOX_ADDRESS "127.0.0.1")

# Audren configuration (aarch64 with GCC and custom wolfSSL)
if(TARGET_AUDREN)
    # Force GCC compiler for Audren target
    find_program(GCC_FOUND gcc)
    if(GCC_FOUND)
        set(CMAKE_C_COMPILER "gcc")
        set(COMPILER_DETECTED "GCC")
        message(STATUS "Selected compiler: GCC for Audren target")
        message(STATUS "  C Compiler: ${GCC_FOUND}")
        find_program(GPP_FOUND g++)
        if(GPP_FOUND)
            set(CMAKE_CXX_COMPILER "g++")
            message(STATUS "  C++ Compiler: ${GPP_FOUND}")
        endif()
    else()
        message(FATAL_ERROR "GCC compiler not found for Audren target!")
    endif()

    add_compile_definitions(
        INTOX
        AUDREN
        # Define the required Intox variables with dynamic port
        intox_address="${INTOX_ADDRESS}"
        intox_port=${INTOX_PORT}
    )

    # Output executable name for Audren
    if(DEBUG)
        set(EXPLO_EXE_NAME "explo_audren_debug")
        set(INTER_EXE_NAME "inter_audren_debug")
    else()
        set(EXPLO_EXE_NAME "explo_audren_release")
        set(INTER_EXE_NAME "inter_audren_release")
    endif()

    # Use aarch64 Intox libraries (like x86_64 but with aarch64 versions)
    set(MRPIZ_LIB "intoxmrpiz;intox")
    set(LUMPIZ_LIB "intoxlumpiz")
    set(EXTRA_LIBS "m") # math library
elseif(TARGET_RASPI_ZERO_W)
    # Cross-compilation toolchain setup - Force x-tools usage only
    set(CMAKE_SYSTEM_NAME Linux)
    set(CMAKE_SYSTEM_PROCESSOR arm)

    # Force x-tools toolchain paths
    set(CROSS_TOOLS "/opt/x-tools/armv6-rpi-linux-gnueabihf" CACHE PATH "x-tools cross-compilation directory")
    set(CROSS_GCC "${CROSS_TOOLS}/bin/armv6-rpi-linux-gnueabihf-gcc" CACHE FILEPATH "ARM v6 Cross GCC compiler")
    set(CROSS_GPP "${CROSS_TOOLS}/bin/armv6-rpi-linux-gnueabihf-g++" CACHE FILEPATH "ARM v6 Cross G++ compiler")
    set(CROSS_SYSROOT "${CROSS_TOOLS}/armv6-rpi-linux-gnueabihf/sysroot" CACHE PATH "ARM v6 Cross-compilation sysroot")

    # Verify x-tools installation
    if(NOT EXISTS "${CROSS_GCC}")
        message(FATAL_ERROR "x-tools ARM v6 compiler not found at ${CROSS_GCC}. Please install x-tools for armv6-rpi-linux-gnueabihf in /opt/x-tools.")
    endif()

    # Set compilers directly (no find_program needed for x-tools)
    set(CMAKE_C_COMPILER "${CROSS_GCC}")
    set(CMAKE_CXX_COMPILER "${CROSS_GPP}")
    set(COMPILER_DETECTED "GCC")

    message(STATUS "Using x-tools ARM v6 Raspberry Pi toolchain:")
    message(STATUS "  Toolchain directory: ${CROSS_TOOLS}")
    message(STATUS "  C Compiler: ${CMAKE_C_COMPILER}")
    message(STATUS "  C++ Compiler: ${CMAKE_CXX_COMPILER}")
    message(STATUS "  Sysroot: ${CROSS_SYSROOT}")
    
    add_compile_definitions(RASPI_ZERO_W)
    set(CMAKE_SYSROOT "${CROSS_SYSROOT}")
    set(CMAKE_FIND_ROOT_PATH "${CROSS_SYSROOT}")
    set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
    set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
    set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
    
    # Output executable name
    if(DEBUG)
        set(EXPLO_EXE_NAME "explo_raspi_debug")
        set(INTER_EXE_NAME "inter_raspi_debug")
    else()
        set(EXPLO_EXE_NAME "explo_raspi_release")
        set(INTER_EXE_NAME "inter_raspi_release")
    endif()
    
    # Use appropriate libraries for Raspberry Pi embedded target
    set(MRPIZ_LIB "libmrpiz.a")
    set(LUMPIZ_LIB "liblumpiz.a")
    set(EXTRA_LIBS "m") # math library
else()
    # PC development build with Intox simulator
    # Try to use Clang first, fallback to GCC
    find_program(CLANG_FOUND clang)
    if(CLANG_FOUND)
        set(CMAKE_C_COMPILER "clang")
        set(COMPILER_DETECTED "CLANG")
        message(STATUS "Selected compiler: Clang for PC development")
        message(STATUS "  C Compiler: ${CLANG_FOUND}")
        find_program(CLANGPP_FOUND clang++)
        if(CLANGPP_FOUND)
            set(CMAKE_CXX_COMPILER "clang++")
            message(STATUS "  C++ Compiler: ${CLANGPP_FOUND}")
        endif()
    else()
        find_program(GCC_FOUND gcc)
        if(GCC_FOUND)
            set(CMAKE_C_COMPILER "gcc")
            set(COMPILER_DETECTED "GCC")
            message(STATUS "Selected compiler: GCC for PC development")
            message(STATUS "  C Compiler: ${GCC_FOUND}")
            find_program(GPP_FOUND g++)
            if(GPP_FOUND)
                set(CMAKE_CXX_COMPILER "g++")
                message(STATUS "  C++ Compiler: ${GPP_FOUND}")
            endif()
        else()
            message(FATAL_ERROR "Neither Clang nor GCC compiler found!")
        endif()
    endif()
    
    add_compile_definitions(
        INTOX
        # Define the required Intox variables with dynamic port
        intox_address="${INTOX_ADDRESS}"
        intox_port=${INTOX_PORT}
    )

    # Output executable name with simulation type
    set(EXPLO_EXE_NAME "explo_intox_${SIMULATION_TYPE}")
    set(INTER_EXE_NAME "inter_intox_${SIMULATION_TYPE}")
    
    # Link with Intox simulator libraries
    set(MRPIZ_LIB "intoxmrpiz;intox")
    set(LUMPIZ_LIB "intoxlumpiz")
    # Extra libs for PC target
    set(EXTRA_LIBS "")
endif()

# Set C standard and flags
set(CMAKE_C_STANDARD 11)

# Compiler-specific flags based on detected compiler
if(COMPILER_DETECTED STREQUAL "CLANG")
    message(STATUS "Applying Clang-specific compiler flags")
    add_compile_options(
        -g
        -O0
        -fno-omit-frame-pointer
        # Common warnings supported by Clang
        -Wall
        -Wextra
        -Wformat=2
        -Wformat-security
        -Warray-bounds
        -Wnull-dereference
        -Wuninitialized
        -Wdangling-else
        -Wcast-align
        -Wbad-function-cast
        -Wpointer-arith
        -Wwrite-strings
        -Wconversion
        -Wsign-conversion
        -Wfloat-equal
        # Clang-specific warnings
        -Wlogical-op-parentheses
        -Wshift-overflow
        -Wshift-sign-overflow
        -Winteger-overflow
        -Wdivision-by-zero
        -Wfloat-overflow-conversion
        -Wfloat-zero-conversion
        -Winfinite-recursion
        -Wreturn-stack-address
        -Wvla
        -Walloca
        -Wcomma
        -Wconditional-uninitialized
        -Wloop-analysis
        -Wswitch-enum
        -Wtautological-compare
        -Wunreachable-code
        # Clang-specific disables
        -Wno-unused-variable
        -Wno-unused-value
        -Wno-padded
        -Wno-packed
        -Wno-disabled-macro-expansion
        -Wno-reserved-id-macro
        -Wno-c23-extensions
        -MMD
        -MP
    )
else()
    message(STATUS "Applying GCC-specific compiler flags")
    add_compile_options(
        -g
        -O0
        -fno-omit-frame-pointer
        # Common warnings supported by GCC
        -Wall
        -Wextra
        -Wformat=2
        -Wformat-security
        -Warray-bounds=2
        -Wnull-dereference
        -Wuninitialized
        -Wdangling-else
        -Wcast-align=strict
        -Wpointer-arith
        -Wwrite-strings
        -Wconversion
        -Wsign-conversion
        -Wfloat-equal
        # GCC-specific warnings
        -Wformat-overflow=2
        -Wformat-truncation=2
        -Wstringop-overflow=4
        -Wstringop-truncation
        -Wshift-overflow=2
        -Wshift-count-overflow
        -Wshift-count-negative
        -Wduplicated-cond
        -Wduplicated-branches
        -Wlogical-op
        -Wsequence-point
        -Wmaybe-uninitialized
        -Wuse-after-free=3
        -Wvla-larger-than=1024
        -Walloca-larger-than=1024
        -Wstrict-overflow=3
        -Wstrict-aliasing=2
        -Wtrampolines
        -Wjump-misses-init
        -Wunsafe-loop-optimizations
        -Wsuggest-attribute=pure
        -Wsuggest-attribute=const
        -Wsuggest-attribute=noreturn
        # GCC-specific disables
        -Wno-unused-variable
        -Wno-unused-value
        -MMD
        -MP
    )
endif()

# AddressSanitizer support - conditional compilation
option(USE_ASAN "Enable AddressSanitizer" OFF)

if(USE_ASAN)
    message(STATUS "AddressSanitizer enabled")
    add_compile_options(-fsanitize=address)
    add_link_options(
        -fsanitize=address
        -fno-omit-frame-pointer
        -static-libasan
    )
    add_compile_definitions(USE_ASAN)
else()
    message(STATUS "AddressSanitizer disabled")
endif()

# Add C standard and source definitions
add_compile_definitions(
    _DEFAULT_SOURCE
    _GNU_SOURCE
    _BSD_SOURCE
    _XOPEN_SOURCE
    _XOPEN_SOURCE_EXTENDED
   
)


# Debug/Release build type
if(DEBUG)
    add_compile_options(-Og -g -gdwarf-4)
    add_compile_definitions(DEBUG)
else()
    add_compile_options(-O2)
    add_compile_definitions(NDEBUG)
endif()

# TLS support (always enabled)
add_compile_definitions(_USE_TLS)

# Configure wolfSSL based on target
if(TARGET_AUDREN)
    # For Audren, try custom wolfSSL library in libs/wolfssl/audren first, fallback to system
    set(WOLFSSL_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/wolfssl/audren")
    set(WOLFSSL_AUDREN_TYPE "system")  # Default to system

    # Check if custom wolfSSL exists
    set(WOLFSSL_CUSTOM_FOUND FALSE)
    set(_WOLFSSL_CANDIDATE_INCLUDE_DIRS
        "${WOLFSSL_ROOT_DIR}/include"
        "${WOLFSSL_ROOT_DIR}/build/include"
        "${WOLFSSL_ROOT_DIR}/build-aarch64/include"
    )
    set(WOLFSSL_INCLUDE_DIR "")
    foreach(_dir ${_WOLFSSL_CANDIDATE_INCLUDE_DIRS})
        if(EXISTS "${_dir}/wolfssl/ssl.h")
            set(WOLFSSL_INCLUDE_DIR "${_dir}")
            set(WOLFSSL_CUSTOM_FOUND TRUE)
            break()
        endif()
    endforeach()

    if(WOLFSSL_CUSTOM_FOUND)
        set(_WOLFSSL_CANDIDATE_LIB_DIRS
            "${WOLFSSL_ROOT_DIR}/lib"
            "${WOLFSSL_ROOT_DIR}/build/lib"
            "${WOLFSSL_ROOT_DIR}/build-aarch64/lib"
        )
        set(WOLFSSL_LIBRARY "")
        foreach(_libdir ${_WOLFSSL_CANDIDATE_LIB_DIRS})
            if(EXISTS "${_libdir}/libwolfssl.a")
                set(WOLFSSL_LIBRARY "${_libdir}/libwolfssl.a")
                break()
            elseif(EXISTS "${_libdir}/libwolfssl.so")
                set(WOLFSSL_LIBRARY "${_libdir}/libwolfssl.so")
                break()
            endif()
        endforeach()

        if(WOLFSSL_LIBRARY)
            include_directories(${WOLFSSL_INCLUDE_DIR})
            set(WOLFSSL_LIBRARIES "${WOLFSSL_LIBRARY}")
            set(WOLFSSL_AUDREN_TYPE "custom")
            message(STATUS "TLS support enabled - Using custom wolfSSL for Audren")
            message(STATUS "  wolfSSL include: ${WOLFSSL_INCLUDE_DIR}")
            message(STATUS "  wolfSSL library: ${WOLFSSL_LIBRARY}")
        else()
            message(WARNING "Custom wolfSSL library not found in ${WOLFSSL_ROOT_DIR}, falling back to system wolfSSL")
            set(WOLFSSL_CUSTOM_FOUND FALSE)
        endif()
    else()
        message(WARNING "Custom wolfSSL not found in ${WOLFSSL_ROOT_DIR}, falling back to system wolfSSL")
    endif()

    # Fallback to system wolfSSL if custom not found
    if(NOT WOLFSSL_CUSTOM_FOUND)
        find_package(PkgConfig REQUIRED)
        pkg_check_modules(WOLFSSL REQUIRED wolfssl)
        include_directories(${WOLFSSL_INCLUDE_DIRS})
        message(STATUS "TLS support enabled - Using system wolfSSL for Audren")
        message(STATUS "  wolfSSL library: ${WOLFSSL_LIBRARIES}")
        if(WOLFSSL_LIBRARY_DIRS)
            message(STATUS "  wolfSSL library directories: ${WOLFSSL_LIBRARY_DIRS}")
        endif()
    endif()
elseif(TARGET_RASPI_ZERO_W)
    # For Raspberry Pi, use local wolfSSL library in libs/wolfssl (support common build subdirs)
    set(WOLFSSL_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/wolfssl")

    # Manually locate headers and library to bypass sysroot-only search
    set(_WOLFSSL_CANDIDATE_INCLUDE_DIRS
        "${WOLFSSL_ROOT_DIR}/include"
        "${WOLFSSL_ROOT_DIR}/build/include"
        "${WOLFSSL_ROOT_DIR}/build-armv6/include"
        "${WOLFSSL_ROOT_DIR}/build-armhf/include"
        "${WOLFSSL_ROOT_DIR}/build-arm/include"
    )
    set(WOLFSSL_INCLUDE_DIR "")
    foreach(_dir ${_WOLFSSL_CANDIDATE_INCLUDE_DIRS})
        if(EXISTS "${_dir}/wolfssl/ssl.h")
            set(WOLFSSL_INCLUDE_DIR "${_dir}")
            break()
        endif()
    endforeach()

    set(_WOLFSSL_CANDIDATE_LIB_DIRS
        "${WOLFSSL_ROOT_DIR}/lib"
        "${WOLFSSL_ROOT_DIR}/build/lib"
        "${WOLFSSL_ROOT_DIR}/build-armv6/lib"
        "${WOLFSSL_ROOT_DIR}/build-armhf/lib"
        "${WOLFSSL_ROOT_DIR}/build-arm/lib"
    )
    set(WOLFSSL_LIBRARY "")
    foreach(_libdir ${_WOLFSSL_CANDIDATE_LIB_DIRS})
        if(EXISTS "${_libdir}/libwolfssl.a")
            set(WOLFSSL_LIBRARY "${_libdir}/libwolfssl.a")
            break()
        elseif(EXISTS "${_libdir}/libwolfssl.so")
            set(WOLFSSL_LIBRARY "${_libdir}/libwolfssl.so")
            break()
        endif()
    endforeach()

    if(NOT WOLFSSL_INCLUDE_DIR)
        message(FATAL_ERROR "wolfSSL headers not found under ${WOLFSSL_ROOT_DIR}. Expected wolfssl/ssl.h in an include directory.")
    endif()

    if(NOT WOLFSSL_LIBRARY)
        message(FATAL_ERROR "wolfSSL library not found under ${WOLFSSL_ROOT_DIR}. Expected libwolfssl.(a|so) in a lib directory.")
    endif()

    include_directories(${WOLFSSL_INCLUDE_DIR})
    set(WOLFSSL_LIBRARIES "${WOLFSSL_LIBRARY}")
    message(STATUS "TLS support enabled - Using local wolfSSL")
    message(STATUS "  wolfSSL include: ${WOLFSSL_INCLUDE_DIR}")
    message(STATUS "  wolfSSL library: ${WOLFSSL_LIBRARY}")
else()
    # For PC development, use local wolfSSL library in libs/wolfssl/x86-64
    set(WOLFSSL_ROOT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/wolfssl/x86-64")

    # Manually locate headers and library
    set(_WOLFSSL_CANDIDATE_INCLUDE_DIRS
        "${WOLFSSL_ROOT_DIR}/include"
        "${WOLFSSL_ROOT_DIR}/build/include"
        "${WOLFSSL_ROOT_DIR}/build-x86-64/include"
        "${WOLFSSL_ROOT_DIR}/build-x86_64/include"
    )
    set(WOLFSSL_INCLUDE_DIR "")
    foreach(_dir ${_WOLFSSL_CANDIDATE_INCLUDE_DIRS})
        if(EXISTS "${_dir}/wolfssl/ssl.h")
            set(WOLFSSL_INCLUDE_DIR "${_dir}")
            break()
        endif()
    endforeach()

    set(_WOLFSSL_CANDIDATE_LIB_DIRS
        "${WOLFSSL_ROOT_DIR}/lib"
        "${WOLFSSL_ROOT_DIR}/build/lib"
        "${WOLFSSL_ROOT_DIR}/build-x86-64/lib"
        "${WOLFSSL_ROOT_DIR}/build-x86_64/lib"
    )
    set(WOLFSSL_LIBRARY "")
    foreach(_libdir ${_WOLFSSL_CANDIDATE_LIB_DIRS})
        if(EXISTS "${_libdir}/libwolfssl.a")
            set(WOLFSSL_LIBRARY "${_libdir}/libwolfssl.a")
            break()
        elseif(EXISTS "${_libdir}/libwolfssl.so")
            set(WOLFSSL_LIBRARY "${_libdir}/libwolfssl.so")
            break()
        endif()
    endforeach()

    if(NOT WOLFSSL_INCLUDE_DIR)
        message(FATAL_ERROR "wolfSSL headers not found under ${WOLFSSL_ROOT_DIR}. Expected wolfssl/ssl.h in an include directory.")
    endif()

    if(NOT WOLFSSL_LIBRARY)
        message(FATAL_ERROR "wolfSSL library not found under ${WOLFSSL_ROOT_DIR}. Expected libwolfssl.(a|so) in a lib directory.")
    endif()

    include_directories(${WOLFSSL_INCLUDE_DIR})
    set(WOLFSSL_LIBRARIES "${WOLFSSL_LIBRARY}")
    message(STATUS "TLS support enabled - Using local wolfSSL for x86-64")
    message(STATUS "  wolfSSL include: ${WOLFSSL_INCLUDE_DIR}")
    message(STATUS "  wolfSSL library: ${WOLFSSL_LIBRARY}")
endif()

# Add libraries based on target architecture
if(TARGET_RASPI_ZERO_W)
    # For Raspberry Pi ARMv6, try to find armhf-specific libraries first
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-armhf-v0.6.1")
        set(LIB_ARCH_SUFFIX "armhf")
        message(STATUS "Using ARMhf-specific libraries for Raspberry Pi")
    elseif(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-armv6-v0.6.1")
        set(LIB_ARCH_SUFFIX "armv6")
        message(STATUS "Using ARMv6-specific libraries for Raspberry Pi")
    else()
        # Fallback to aarch64 libraries if no armhf-specific libs are available
        set(LIB_ARCH_SUFFIX "aarch64")
        message(STATUS "Using arm libraries for Raspberry Pi (no armhf-specific libs found)")
    endif()
else()
    set(LIB_ARCH_SUFFIX "${TARGET_ARCH}")
endif()

set(MRPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-${LIB_ARCH_SUFFIX}-v0.6.1/lib")
set(MRPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-${LIB_ARCH_SUFFIX}-v0.6.1/include/mrpiz")
set(LUMPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-${LIB_ARCH_SUFFIX}-v0.1.1/lib")
set(LUMPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-${LIB_ARCH_SUFFIX}-v0.1.1/include/lumpiz")

include_directories(${MRPIZ_INCLUDE_DIR})
include_directories(${LUMPIZ_INCLUDE_DIR})

# Recursively find all header files for include directories
file(GLOB_RECURSE HEADER_FILES 
    "${CMAKE_CURRENT_SOURCE_DIR}/common/*.h"
    "${CMAKE_CURRENT_SOURCE_DIR}/EXPLO/*.h"
    "${CMAKE_CURRENT_SOURCE_DIR}/INTER/*.h"
)

# Extract unique directories from header files for includes
foreach(HEADER ${HEADER_FILES})
    get_filename_component(HEADER_DIR ${HEADER} DIRECTORY)
    list(APPEND INCLUDE_DIRS ${HEADER_DIR})
endforeach()
list(REMOVE_DUPLICATES INCLUDE_DIRS)
include_directories(${INCLUDE_DIRS})

# Common source files (TLS always included)
file(GLOB_RECURSE COMMON_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/common/*.c")

set(COMMON_SOURCES_COUNT 0)
foreach(SOURCE ${COMMON_SOURCES})
    math(EXPR COMMON_SOURCES_COUNT "${COMMON_SOURCES_COUNT} + 1")
endforeach()

# Create build directory if it doesn't exist
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR})

# EXPLO executable
set(EXPLO_SOURCES "")
set(EXPLO_SOURCES_COUNT 0)
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/EXPLO")
    file(GLOB_RECURSE EXPLO_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/EXPLO/*.c")
endif()

# If no source files found, create a placeholder
if(NOT EXPLO_SOURCES)
    set(EXPLO_MAIN "${CMAKE_CURRENT_SOURCE_DIR}/EXPLO/main.c")
    file(WRITE ${EXPLO_MAIN} "int main() { return 0; }\n")
    set(EXPLO_SOURCES ${EXPLO_MAIN})
endif()

foreach(SOURCE ${EXPLO_SOURCES})
    math(EXPR EXPLO_SOURCES_COUNT "${EXPLO_SOURCES_COUNT} + 1")
endforeach()

add_executable(explo ${COMMON_SOURCES} ${EXPLO_SOURCES})
set_target_properties(explo PROPERTIES OUTPUT_NAME ${EXPLO_EXE_NAME})

# Link MRPIZ libraries
if(TARGET_RASPI_ZERO_W)
    target_link_libraries(explo "${MRPIZ_LIB_DIR}/${MRPIZ_LIB}")
else()
    foreach(LIB ${MRPIZ_LIB})
        target_link_libraries(explo "${MRPIZ_LIB_DIR}/lib${LIB}.a")
    endforeach()
endif()

# Link LUMPIZ libraries
if(TARGET_RASPI_ZERO_W)
    target_link_libraries(explo "${LUMPIZ_LIB_DIR}/${LUMPIZ_LIB}")
else()
    foreach(LIB ${LUMPIZ_LIB})
        target_link_libraries(explo "${LUMPIZ_LIB_DIR}/lib${LIB}.a")
    endforeach()
endif()

if(EXTRA_LIBS)
    target_link_libraries(explo ${EXTRA_LIBS})
endif()

# INTER executable
set(INTER_SOURCES "")
set(INTER_SOURCES_COUNT 0)
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/INTER")
    file(GLOB_RECURSE INTER_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/INTER/*.c")
endif()

# If no source files found, create a placeholder
if(NOT INTER_SOURCES)
    set(INTER_MAIN "${CMAKE_CURRENT_SOURCE_DIR}/INTER/main.c")
    file(WRITE ${INTER_MAIN} "int main() { return 0; }\n")
    set(INTER_SOURCES ${INTER_MAIN})
endif()

foreach(SOURCE ${INTER_SOURCES})
    math(EXPR INTER_SOURCES_COUNT "${INTER_SOURCES_COUNT} + 1")
endforeach()

add_executable(inter ${COMMON_SOURCES} ${INTER_SOURCES})
set_target_properties(inter PROPERTIES OUTPUT_NAME ${INTER_EXE_NAME})

# Link MRPIZ libraries
if(TARGET_RASPI_ZERO_W)
    target_link_libraries(inter "${MRPIZ_LIB_DIR}/${MRPIZ_LIB}")
else()
    foreach(LIB ${MRPIZ_LIB})
        target_link_libraries(inter "${MRPIZ_LIB_DIR}/lib${LIB}.a")
    endforeach()
endif()

# Link LUMPIZ libraries
if(TARGET_RASPI_ZERO_W)
    target_link_libraries(inter "${LUMPIZ_LIB_DIR}/${LUMPIZ_LIB}")
else()
    foreach(LIB ${LUMPIZ_LIB})
        target_link_libraries(inter "${LUMPIZ_LIB_DIR}/lib${LIB}.a")
    endforeach()
endif()

# Find math library
find_library(MATH_LIBRARY m)

# Find pthread library
find_package(Threads REQUIRED)

# Link math library to executables
if(MATH_LIBRARY)
    target_link_libraries(explo ${MATH_LIBRARY})
    target_link_libraries(inter ${MATH_LIBRARY})
endif()

# Link pthread library to executables
target_link_libraries(explo Threads::Threads)
target_link_libraries(inter Threads::Threads)


if(EXTRA_LIBS)
    target_link_libraries(inter ${EXTRA_LIBS})
endif()

# Additional system libs for Raspberry Pi Zero W
if(TARGET_RASPI_ZERO_W)
    target_link_libraries(explo pthread rt atomic)
    target_link_libraries(inter pthread rt atomic)
endif()

# Add TLS libraries (always enabled)
target_link_libraries(explo ${WOLFSSL_LIBRARIES})
target_link_libraries(inter ${WOLFSSL_LIBRARIES})
message(STATUS "Linking with wolfSSL libraries: ${WOLFSSL_LIBRARIES}")

# Add build type definitions
target_compile_definitions(explo PRIVATE EXPLO_BUILD)
target_compile_definitions(inter PRIVATE INTER_BUILD)

# Override POSIX level for Raspberry Pi target only
if(TARGET_RASPI_ZERO_W)
    target_compile_definitions(explo PRIVATE _POSIX_C_SOURCE=200809L)
    target_compile_definitions(inter PRIVATE _POSIX_C_SOURCE=200809L)
else()
    target_compile_definitions(explo PRIVATE _POSIX_C_SOURCE=199309L)
    target_compile_definitions(inter PRIVATE _POSIX_C_SOURCE=199309L)
endif()

# Calculate total files
math(EXPR TOTAL_FILES_COUNT "${COMMON_SOURCES_COUNT} + ${EXPLO_SOURCES_COUNT} + ${INTER_SOURCES_COUNT}")

# Summary
message(STATUS "")
message(STATUS "")
message(STATUS "╔══════════════════════════════════════════════════════════════════════════════╗")
message(STATUS "║                          BUILD CONFIGURATION SUMMARY                         ║")
message(STATUS "╚══════════════════════════════════════════════════════════════════════════════╝")
message(STATUS "")
message(STATUS "TARGET CONFIGURATION:")
message(STATUS "   Architecture: ${TARGET_ARCH}")
if(TARGET_AUDREN)
    message(STATUS "   Platform: Audren (aarch64 with GCC, Intox simulator and custom wolfSSL)")
    message(STATUS "   Intox Address: ${INTOX_ADDRESS}")
    message(STATUS "   Intox Port: ${INTOX_PORT}")
elseif(TARGET_RASPI_ZERO_W)
    message(STATUS "   Platform: Raspberry Pi Zero W (ARMv6 cross-compile)")
    message(STATUS "   Sysroot: ${CMAKE_SYSROOT}")
else()
    message(STATUS "   Platform: PC Development with Intox Simulator")
    message(STATUS "   Simulation Type: ${SIMULATION_TYPE}")
    message(STATUS "   Intox Address: ${INTOX_ADDRESS}")
    message(STATUS "   Intox Port: ${INTOX_PORT}")
endif()
message(STATUS "")
message(STATUS "COMPILER CONFIGURATION:")
message(STATUS "   Family: ${COMPILER_DETECTED}")
message(STATUS "   C Compiler: ${CMAKE_C_COMPILER}")
if(CMAKE_CXX_COMPILER)
    message(STATUS "   C++ Compiler: ${CMAKE_CXX_COMPILER}")
endif()
message(STATUS "")
message(STATUS "BUILD CONFIGURATION:")
message(STATUS "   Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "   Debug Mode: ${DEBUG}")
if(TARGET_AUDREN)
    message(STATUS "   TLS Support: Always Enabled (${WOLFSSL_AUDREN_TYPE} wolfSSL)")
elseif(TARGET_RASPI_ZERO_W)
    message(STATUS "   TLS Support: Always Enabled (local wolfSSL)")
else()
    message(STATUS "   TLS Support: Always Enabled (system wolfSSL ${WOLFSSL_VERSION})")
endif()
message(STATUS "")
message(STATUS "SOURCE FILES:")
message(STATUS "   Common modules: ${COMMON_SOURCES_COUNT}")
message(STATUS "   EXPLO modules: ${EXPLO_SOURCES_COUNT}")
message(STATUS "   INTER modules: ${INTER_SOURCES_COUNT}")
message(STATUS "   ───────────────────")
message(STATUS "   Total files: ${TOTAL_FILES_COUNT}")
message(STATUS "")
message(STATUS "OUTPUT:")
message(STATUS "   EXPLO executable: ${EXPLO_EXE_NAME}")
message(STATUS "   INTER executable: ${INTER_EXE_NAME}")
message(STATUS "")
message(STATUS "LIBRARIES:")
if(TARGET_AUDREN OR TARGET_RASPI_ZERO_W)
    message(STATUS "   MRPIZ: ${MRPIZ_LIB_DIR}/${MRPIZ_LIB}")
    message(STATUS "   LUMPIZ: ${LUMPIZ_LIB_DIR}/${LUMPIZ_LIB}")
else()
    message(STATUS "   MRPIZ: ${MRPIZ_LIB_DIR}")
    message(STATUS "   LUMPIZ: ${LUMPIZ_LIB_DIR}")
endif()
message(STATUS "")
message(STATUS "╚══════════════════════════════════════════════════════════════════════════════╝")
message(STATUS "")


add_custom_command(
    TARGET explo                                             # cible concernée
    POST_BUILD                                               # après un build réussi[5]
    COMMAND bash "${CMAKE_CURRENT_SOURCE_DIR}/gen_pki.sh" "$<TARGET_FILE:explo>" || true 
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Génération des certificats PKI pour explo"
    VERBATIM)

add_custom_command(
    TARGET inter
    POST_BUILD
    COMMAND bash "${CMAKE_CURRENT_SOURCE_DIR}/gen_pki.sh" "$<TARGET_FILE:inter>" || true
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Génération des certificats PKI pour inter"
    VERBATIM)

# CMake presets for different configurations
# Create a CMakePresets.json file
set(PRESETS_FILE "${CMAKE_CURRENT_SOURCE_DIR}/CMakePresets.json")
file(WRITE ${PRESETS_FILE} [=[
{
  "version": 3,
  "cmakeMinimumRequired": {
    "major": 3,
    "minor": 10,
    "patch": 0
  },
  "configurePresets": [
    {
      "name": "x86_64-debug",
      "displayName": "x86_64 Debug",
      "description": "Debug build for x86_64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/x86_64-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "x86_64-release",
      "displayName": "x86_64 Release",
      "description": "Release build for x86_64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/x86_64-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "x86_64-3d-debug",
      "displayName": "x86_64 3D Debug",
      "description": "3D Debug build for x86_64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/x86_64-3d-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "ON"
      }
    },
    {
      "name": "x86_64-3d-release",
      "displayName": "x86_64 3D Release",
      "description": "3D Release build for x86_64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/x86_64-3d-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "ON"
      }
    },
    {
      "name": "raspi-debug",
      "displayName": "Raspberry Pi Zero W Debug",
      "description": "Debug build for Raspberry Pi Zero W",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/raspi-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_RASPI_ZERO_W": "ON",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "raspi-release",
      "displayName": "Raspberry Pi Zero W Release",
      "description": "Release build for Raspberry Pi Zero W",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/raspi-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_RASPI_ZERO_W": "ON",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "production",
      "displayName": "Production Build",
      "description": "Optimized production build with TLS",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/production",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "audren-debug",
      "displayName": "Audren Debug",
      "description": "Debug build for Audren target (aarch64 with GCC and custom wolfSSL)",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/audren-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_AUDREN": "ON",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "audren-release",
      "displayName": "Audren Release",
      "description": "Release build for Audren target (aarch64 with GCC and custom wolfSSL)",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/audren-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_AUDREN": "ON",
        "TARGET_RASPI_ZERO_W": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    }
  ],
  "buildPresets": [
    {
      "name": "x86_64-debug",
      "configurePreset": "x86_64-debug"
    },
    {
      "name": "x86_64-release",
      "configurePreset": "x86_64-release"
    },
    {
      "name": "x86_64-3d-debug",
      "configurePreset": "x86_64-3d-debug"
    },
    {
      "name": "x86_64-3d-release",
      "configurePreset": "x86_64-3d-release"
    },
    {
      "name": "raspi-debug",
      "configurePreset": "raspi-debug"
    },
    {
      "name": "raspi-release",
      "configurePreset": "raspi-release"
    },
    {
      "name": "production",
      "configurePreset": "production"
    },
    {
      "name": "audren-debug",
      "configurePreset": "audren-debug"
    },
    {
      "name": "audren-release",
      "configurePreset": "audren-release"
    }
  ]
}
]=])
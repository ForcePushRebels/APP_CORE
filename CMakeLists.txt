cmake_minimum_required(VERSION 3.10)
project(EXPLO)

# Force Ninja generator
set(CMAKE_GENERATOR "Ninja" CACHE INTERNAL "" FORCE)

# CMake presets configuration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find zlib package
find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIRS})
message(STATUS "Found zlib: ${ZLIB_LIBRARIES}")

# Detect architecture
if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64")
    set(TARGET_ARCH "aarch64")
else()
    set(TARGET_ARCH "x86_64")
endif()

message(STATUS "Detected architecture: ${TARGET_ARCH}")

# Target options
option(TARGET_RASPI_ARM_V6 "Build for Raspberry Pi ARM v6 target with custom toolchain (cross-compile)" OFF)
option(DEBUG "Enable debug build" OFF)
option(USE_3D_SIMULATION "Use 3D simulation instead of 2D (changes Intox port from 12345 to 12348)" OFF)

# Auto-detect and choose compiler: prefer Clang, fallback to GCC
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

# Raspberry Pi ARM v6 configuration (for cross-compilation)
if(TARGET_RASPI_ARM_V6)
    # Cross-compilation toolchain setup
    set(CMAKE_SYSTEM_NAME Linux)
    set(CMAKE_SYSTEM_PROCESSOR arm)
    
    # ARM v6 toolchain configuration
    set(CROSS_TOOLS "${CMAKE_CURRENT_SOURCE_DIR}/libs/x-tools/armv6-rpi-linux-gnueabihf" CACHE PATH "Path to ARM v6 cross-compilation tools")
    set(CROSS_GCC "${CROSS_TOOLS}/bin/armv6-rpi-linux-gnueabihf-gcc" CACHE FILEPATH "ARM v6 Cross GCC compiler")
    set(CROSS_CLANG "${CROSS_TOOLS}/bin/clang" CACHE FILEPATH "ARM v6 Cross Clang compiler")
    set(CROSS_SYSROOT "${CROSS_TOOLS}/armv6-rpi-linux-gnueabihf/sysroot" CACHE PATH "ARM v6 Cross-compilation sysroot")
    # Use the specific toolchain file
    set(CMAKE_TOOLCHAIN_FILE "${CROSS_TOOLS}/armv6-rpi-linux-gnueabihf.toolchain.cmake" CACHE FILEPATH "ARM v6 toolchain file")
    
    # Try to use Clang first, fallback to GCC
    find_program(CLANG_FOUND ${CROSS_CLANG})
    if(CLANG_FOUND)
        set(CMAKE_C_COMPILER "${CROSS_CLANG}")
        set(CMAKE_C_COMPILER_TARGET "arm-linux-gnueabihf")
        set(COMPILER_DETECTED "CLANG")
        message(STATUS "Using Clang for cross-compilation: ${CROSS_CLANG}")
    else()
        find_program(GCC_FOUND ${CROSS_GCC})
        if(GCC_FOUND)
            set(CMAKE_C_COMPILER "${CROSS_GCC}")
            set(COMPILER_DETECTED "GCC")
            message(STATUS "Clang not found, using GCC for cross-compilation: ${CROSS_GCC}")
        else()
            message(FATAL_ERROR "Neither Clang nor GCC cross-compiler found!")
        endif()
    endif()
    
    # Add target-specific definitions
    add_compile_definitions(RASPI_ARM_V6)
    
    set(CMAKE_SYSROOT "${CROSS_SYSROOT}")
    set(CMAKE_FIND_ROOT_PATH "${CROSS_SYSROOT}")
    set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
    set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
    set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
    
    # Output executable name
    if(DEBUG)
        set(EXPLO_EXE_NAME "explo_armv6_debug")
        set(INTER_EXE_NAME "inter_armv6_debug")
    else()
        set(EXPLO_EXE_NAME "explo_armv6_release")
        set(INTER_EXE_NAME "inter_armv6_release")
    endif()
    
    # Use appropriate libraries for Raspberry Pi
    set(MRPIZ_LIB "mrpiz")
    set(LUMPIZ_LIB "lumpiz")
    set(EXTRA_LIBS "m") # math library
else()
    # PC development build with Intox simulator
    # Try to use Clang first, fallback to GCC
    find_program(CLANG_FOUND clang)
    if(CLANG_FOUND)
        set(CMAKE_C_COMPILER "clang")
        set(COMPILER_DETECTED "CLANG")
        message(STATUS "Using Clang compiler: ${CLANG_FOUND}")
    else()
        find_program(GCC_FOUND gcc)
        if(GCC_FOUND)
            set(CMAKE_C_COMPILER "gcc")
            set(COMPILER_DETECTED "GCC")
            message(STATUS "Clang not found, using GCC compiler: ${GCC_FOUND}")
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
    # Always include zlib in extra libs
    set(EXTRA_LIBS "${ZLIB_LIBRARIES}")
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
    _POSIX_C_SOURCE=199309L
   
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
find_package(PkgConfig REQUIRED)
pkg_check_modules(WOLFSSL REQUIRED wolfssl)
include_directories(${WOLFSSL_INCLUDE_DIRS})
message(STATUS "TLS support enabled - Will use wolfSSL")

# Add libraries based on target architecture
if(TARGET_RASPI_ARM_V6)
    # For ARM v6, use the aarch64 libraries (adjust if you have specific armv6 libs)
    set(MRPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-aarch64-v0.6.1/lib")
    set(MRPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-aarch64-v0.6.1/include/mrpiz")
    set(LUMPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-aarch64-v0.1.1/lib")
    set(LUMPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-aarch64-v0.1.1/include/lumpiz")
else()
    set(MRPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-${TARGET_ARCH}-v0.6.1/lib")
    set(MRPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-${TARGET_ARCH}-v0.6.1/include/mrpiz")
    set(LUMPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-${TARGET_ARCH}-v0.1.1/lib")
    set(LUMPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-${TARGET_ARCH}-v0.1.1/include/lumpiz")
endif()

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

message(STATUS "Common source files:")
set(COMMON_SOURCES_COUNT 0)

foreach(SOURCE ${COMMON_SOURCES})
    message(STATUS "  INCLUDED: ${SOURCE}")
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

message(STATUS "EXPLO source files:")
foreach(SOURCE ${EXPLO_SOURCES})
    message(STATUS "  INCLUDED: ${SOURCE}")
    math(EXPR EXPLO_SOURCES_COUNT "${EXPLO_SOURCES_COUNT} + 1")
endforeach()

add_executable(explo ${COMMON_SOURCES} ${EXPLO_SOURCES})
set_target_properties(explo PROPERTIES OUTPUT_NAME ${EXPLO_EXE_NAME})

# Link MRPIZ libraries
foreach(LIB ${MRPIZ_LIB})
    target_link_libraries(explo "${MRPIZ_LIB_DIR}/lib${LIB}.a")
endforeach()

# Link LUMPIZ libraries
foreach(LIB ${LUMPIZ_LIB})
    target_link_libraries(explo "${LUMPIZ_LIB_DIR}/lib${LIB}.a")
endforeach()

# Link zlib directly
target_link_libraries(explo ${ZLIB_LIBRARIES})

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

message(STATUS "INTER source files:")
foreach(SOURCE ${INTER_SOURCES})
    message(STATUS "  INCLUDED: ${SOURCE}")
    math(EXPR INTER_SOURCES_COUNT "${INTER_SOURCES_COUNT} + 1")
endforeach()

add_executable(inter ${COMMON_SOURCES} ${INTER_SOURCES})
set_target_properties(inter PROPERTIES OUTPUT_NAME ${INTER_EXE_NAME})

# Link MRPIZ libraries
foreach(LIB ${MRPIZ_LIB})
    target_link_libraries(inter "${MRPIZ_LIB_DIR}/lib${LIB}.a")
endforeach()

# Link LUMPIZ libraries
foreach(LIB ${LUMPIZ_LIB})
    target_link_libraries(inter "${LUMPIZ_LIB_DIR}/lib${LIB}.a")
endforeach()

# Link zlib directly
target_link_libraries(inter ${ZLIB_LIBRARIES})

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

# Add TLS libraries (always enabled)
target_link_libraries(explo ${WOLFSSL_LIBRARIES})
target_link_libraries(inter ${WOLFSSL_LIBRARIES})
message(STATUS "Linking with wolfSSL libraries: ${WOLFSSL_LIBRARIES}")

# Add build type definitions
target_compile_definitions(explo PRIVATE EXPLO_BUILD)
target_compile_definitions(inter PRIVATE INTER_BUILD)

# Summary
message(STATUS "")
message(STATUS "Build configuration summary:")
message(STATUS "----------------------------------------")
message(STATUS "  Architecture: ${TARGET_ARCH}")
if(TARGET_RASPI_ARM_V6)
    message(STATUS "  Target: Raspberry Pi ARM v6 (cross-compile)")
    message(STATUS "  Compiler: ${COMPILER_DETECTED}")
    message(STATUS "  Toolchain: ${CMAKE_TOOLCHAIN_FILE}")
else()
    message(STATUS "  Target: PC Development")
    message(STATUS "  Simulation: ${SIMULATION_TYPE}")
    message(STATUS "  Intox Address: ${INTOX_ADDRESS}")
    message(STATUS "  Intox Port: ${INTOX_PORT}")
    message(STATUS "  Compiler: ${COMPILER_DETECTED}")
endif()
message(STATUS "  TLS Support: Always Enabled")
message(STATUS "  Debug Build: ${DEBUG}")
message(STATUS "  ZLIB: ${ZLIB_LIBRARIES}")
message(STATUS "----------------------------------------")
message(STATUS "  Common files included: ${COMMON_SOURCES_COUNT}")
message(STATUS "  EXPLO files included: ${EXPLO_SOURCES_COUNT}")
message(STATUS "  INTER files included: ${INTER_SOURCES_COUNT}")
message(STATUS "----------------------------------------")
message(STATUS "  Total files included: ${COMMON_SOURCES_COUNT} + ${EXPLO_SOURCES_COUNT} + ${INTER_SOURCES_COUNT}")
message(STATUS "----------------------------------------")
message(STATUS "  EXPLO executable: ${EXPLO_EXE_NAME}")
message(STATUS "  INTER executable: ${INTER_EXE_NAME}")
message(STATUS "  MRPIZ library directory: ${MRPIZ_LIB_DIR}")
message(STATUS "  LUMPIZ library directory: ${LUMPIZ_LIB_DIR}")
message(STATUS "----------------------------------------")


add_custom_command(
    TARGET explo                                             # cible concernée
    POST_BUILD                                               # après un build réussi[5]
    COMMAND bash "${CMAKE_CURRENT_SOURCE_DIR}/gen_pki.sh" "$<TARGET_FILE:explo>"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Génération des certificats PKI pour explo"
    VERBATIM)

add_custom_command(
    TARGET inter
    POST_BUILD
    COMMAND bash "${CMAKE_CURRENT_SOURCE_DIR}/gen_pki.sh" "$<TARGET_FILE:inter>"
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
        "USE_3D_SIMULATION": "ON"
      }
    },
    {
      "name": "armv6-debug",
      "displayName": "Raspberry Pi ARM v6 Debug",
      "description": "Debug build for Raspberry Pi ARM v6 with custom toolchain",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/armv6-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_RASPI_ARM_V6": "ON",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "armv6-release",
      "displayName": "Raspberry Pi ARM v6 Release",
      "description": "Release build for Raspberry Pi ARM v6 with custom toolchain",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/armv6-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_RASPI_ARM_V6": "ON",
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
      "name": "armv6-debug",
      "configurePreset": "armv6-debug"
    },
    {
      "name": "armv6-release",
      "configurePreset": "armv6-release"
    },
    {
      "name": "production",
      "configurePreset": "production"
    }
  ]
}
]=])
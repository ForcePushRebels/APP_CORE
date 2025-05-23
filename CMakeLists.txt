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
option(TARGET_MRPIZ "Build for MRPiZ target instead of PC" OFF)
option(USE_TLS "Enable TLS support" OFF)
option(DEBUG "Enable debug build" OFF)
option(USE_3D_SIMULATION "Use 3D simulation instead of 2D (changes Intox port from 12345 to 12348)" OFF)

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Simulation configuration
if(USE_3D_SIMULATION)
    set(INTOX_PORT 12348)
    set(SIMULATION_TYPE "3D")
    add_compile_definitions(SIMULATION_3D)
else()
    set(INTOX_PORT 12345)
    set(SIMULATION_TYPE "2D")
    add_compile_definitions(SIMULATION_2D)
endif()

set(INTOX_ADDRESS "127.0.0.1")

# MRPiZ configuration (for cross-compilation)
if(TARGET_MRPIZ)
    # Uncomment and adjust these for your cross-compilation setup
    # set(CROSS_TOOLS "/path/to/cross-tools")
    # set(CROSS_GCC "${CROSS_TOOLS}/path/to/gcc")
    # set(CROSS_SYSROOT "/path/to/rootfs")
    
    # set(CMAKE_C_COMPILER "${CROSS_GCC}")
    # add_compile_definitions(MRPIZ)
    # set(CMAKE_SYSROOT "${CROSS_SYSROOT}")
    
    # Output executable name
    set(EXPLO_EXE_NAME "explo_mrpiz")
    set(INTER_EXE_NAME "inter_mrpiz")
    
    # Link with MRPiZ lib
    set(MRPIZ_LIB "mrpiz")
    set(LUMPIZ_LIB "lumpiz")
    set(EXTRA_LIBS "m") # math library
else()
    # PC development build with Intox simulator
    set(CMAKE_C_COMPILER "gcc")
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

# Common flags from the Makefile
add_compile_options(
    -Wall
    -Wextra
    -pedantic
    -Wno-unused-variable
    -Wno-unused-value
    -MMD
    -MP
)

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
    add_compile_options(-Og -g)
    add_compile_definitions(DEBUG)
else()
    add_compile_options(-O3)
    add_compile_definitions(NDEBUG)
endif()

# TLS support
if(USE_TLS)
    add_compile_definitions(_USE_TLS)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(WOLFSSL REQUIRED wolfssl)
    include_directories(${WOLFSSL_INCLUDE_DIRS})
    message(STATUS "TLS support enabled - Will use wolfSSL")
else()
    message(STATUS "TLS support disabled")
endif()

# Add libraries based on target architecture
set(MRPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-${TARGET_ARCH}-v0.6.1/lib")
set(MRPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_mrpiz-${TARGET_ARCH}-v0.6.1/include/mrpiz")
set(LUMPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-${TARGET_ARCH}-v0.1.1/lib")
set(LUMPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/libs/lib_lumpiz-${TARGET_ARCH}-v0.1.1/include/lumpiz")

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

# Common source files - conditionally exclude TLS if not enabled
file(GLOB_RECURSE ALL_COMMON_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/common/*.c")

message(STATUS "Common source files:")
set(EXCLUDED_FILES "")
set(COMMON_SOURCES "")
set(COMMON_SOURCES_COUNT 0)
set(EXCLUDED_FILES_COUNT 0)

foreach(SOURCE ${ALL_COMMON_SOURCES})
    if(NOT USE_TLS AND ${SOURCE} MATCHES "tls")
        list(APPEND EXCLUDED_FILES ${SOURCE})
        message(STATUS "  EXCLUDED (TLS disabled): ${SOURCE}")
        math(EXPR EXCLUDED_FILES_COUNT "${EXCLUDED_FILES_COUNT} + 1")
    else()
        list(APPEND COMMON_SOURCES ${SOURCE})
        message(STATUS "  INCLUDED: ${SOURCE}")
        math(EXPR COMMON_SOURCES_COUNT "${COMMON_SOURCES_COUNT} + 1")
    endif()
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

# Link lib math for all project
target_link_libraries(explo m)

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

if(EXTRA_LIBS)
    target_link_libraries(inter ${EXTRA_LIBS})
endif()

# Add TLS libraries if enabled
if(USE_TLS)
    target_link_libraries(explo ${WOLFSSL_LIBRARIES})
    target_link_libraries(inter ${WOLFSSL_LIBRARIES})
    message(STATUS "Linking with wolfSSL libraries: ${WOLFSSL_LIBRARIES}")
endif()

# Summary
message(STATUS "")
message(STATUS "Build configuration summary:")
message(STATUS "----------------------------------------")
message(STATUS "  Architecture: ${TARGET_ARCH}")
if(TARGET_MRPIZ)
    message(STATUS "  Target: MRPiZ")
else()
    message(STATUS "  Target: PC Development")
    message(STATUS "  Simulation: ${SIMULATION_TYPE}")
    message(STATUS "  Intox Address: ${INTOX_ADDRESS}")
    message(STATUS "  Intox Port: ${INTOX_PORT}")
endif()
message(STATUS "  TLS Support: ${USE_TLS}")
message(STATUS "  Debug Build: ${DEBUG}")
message(STATUS "  ZLIB: ${ZLIB_LIBRARIES}")
message(STATUS "----------------------------------------")
message(STATUS "  Common files included: ${COMMON_SOURCES_COUNT}")
message(STATUS "  EXPLO files included: ${EXPLO_SOURCES_COUNT}")
message(STATUS "  INTER files included: ${INTER_SOURCES_COUNT}")
message(STATUS "----------------------------------------")
message(STATUS "  Total files included: ${COMMON_SOURCES_COUNT} + ${EXPLO_SOURCES_COUNT} + ${INTER_SOURCES_COUNT}")
if(EXCLUDED_FILES_COUNT GREATER 0)
    message(STATUS "  Files excluded (TLS disabled): ${EXCLUDED_FILES_COUNT}")
endif()
message(STATUS "----------------------------------------")
message(STATUS "  EXPLO executable: ${EXPLO_EXE_NAME}")
message(STATUS "  INTER executable: ${INTER_EXE_NAME}")
message(STATUS "  MRPIZ library directory: ${MRPIZ_LIB_DIR}")
message(STATUS "  LUMPIZ library directory: ${LUMPIZ_LIB_DIR}")
message(STATUS "----------------------------------------")

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
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
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
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
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
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
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
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "ON"
      }
    },
    {
      "name": "x86_64-mrpiz",
      "displayName": "x86_64 MRPiZ",
      "description": "MRPiZ build for x86_64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/x86_64-mrpiz",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_MRPIZ": "ON",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "aarch64-debug",
      "displayName": "ARM64 Debug",
      "description": "Debug build for ARM64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/aarch64-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "aarch64-release",
      "displayName": "ARM64 Release",
      "description": "Release build for ARM64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/aarch64-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "aarch64-3d-debug",
      "displayName": "ARM64 3D Debug",
      "description": "3D Debug build for ARM64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/aarch64-3d-debug",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Debug",
        "DEBUG": "ON",
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "ON"
      }
    },
    {
      "name": "aarch64-3d-release",
      "displayName": "ARM64 3D Release",
      "description": "3D Release build for ARM64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/aarch64-3d-release",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "ON"
      }
    },
    {
      "name": "aarch64-mrpiz",
      "displayName": "ARM64 MRPiZ",
      "description": "MRPiZ build for ARM64 architecture",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/aarch64-mrpiz",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_MRPIZ": "ON",
        "USE_TLS": "OFF",
        "USE_3D_SIMULATION": "OFF"
      }
    },
    {
      "name": "tls-enabled",
      "displayName": "TLS Enabled",
      "description": "Build with TLS support enabled",
      "generator": "Ninja",
      "binaryDir": "${sourceDir}/build/tls-enabled",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "DEBUG": "OFF",
        "TARGET_MRPIZ": "OFF",
        "USE_TLS": "ON",
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
      "name": "x86_64-mrpiz",
      "configurePreset": "x86_64-mrpiz"
    },
    {
      "name": "aarch64-debug",
      "configurePreset": "aarch64-debug"
    },
    {
      "name": "aarch64-release",
      "configurePreset": "aarch64-release"
    },
    {
      "name": "aarch64-3d-debug",
      "configurePreset": "aarch64-3d-debug"
    },
    {
      "name": "aarch64-3d-release",
      "configurePreset": "aarch64-3d-release"
    },
    {
      "name": "aarch64-mrpiz",
      "configurePreset": "aarch64-mrpiz"
    },
    {
      "name": "tls-enabled",
      "configurePreset": "tls-enabled"
    }
  ]
}
]=])



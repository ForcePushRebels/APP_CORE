cmake_minimum_required(VERSION 3.10)
project(EXPLO)

# Force Ninja generator
set(CMAKE_GENERATOR "Ninja" CACHE INTERNAL "" FORCE)

# CMake presets configuration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Detect architecture
if(TARGET_RASPI_ZERO_W)
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

# Raspberry Pi Zero W configuration (for cross-compilation)
if(TARGET_RASPI_ZERO_W)
    # Cross-compilation toolchain setup
    set(CMAKE_SYSTEM_NAME Linux)
    set(CMAKE_SYSTEM_PROCESSOR arm)
    
    # Toolchain paths - search for ARM v6 compiler in system
    find_program(CROSS_GCC_FOUND armv6-rpi-linux-gnueabihf-gcc)
    if(CROSS_GCC_FOUND)
        get_filename_component(CROSS_TOOLS "${CROSS_GCC_FOUND}" DIRECTORY)
        get_filename_component(CROSS_TOOLS "${CROSS_TOOLS}" DIRECTORY)
        set(CROSS_GCC "${CROSS_GCC_FOUND}" CACHE FILEPATH "ARM v6 Cross GCC compiler")
        set(CROSS_GPP "${CROSS_TOOLS}/bin/armv6-rpi-linux-gnueabihf-g++" CACHE FILEPATH "ARM v6 Cross G++ compiler")
        set(CROSS_CLANG "${CROSS_TOOLS}/bin/clang" CACHE FILEPATH "ARM v6 Cross Clang compiler")
        
        # Try to find sysroot in the toolchain directory structure
        if(EXISTS "${CROSS_TOOLS}/armv6-rpi-linux-gnueabihf/sysroot")
            set(CROSS_SYSROOT "${CROSS_TOOLS}/armv6-rpi-linux-gnueabihf/sysroot" CACHE PATH "ARM v6 Cross-compilation sysroot")
        else()
            set(CROSS_SYSROOT "${CROSS_TOOLS}/armv6-rpi-linux-gnueabihf" CACHE PATH "ARM v6 Cross-compilation sysroot")
        endif()
        
        message(STATUS "Found ARM v6 Raspberry Pi toolchain:")
        message(STATUS "  Toolchain directory: ${CROSS_TOOLS}")
        message(STATUS "  GCC compiler: ${CROSS_GCC}")
        message(STATUS "  G++ compiler: ${CROSS_GPP}")
        message(STATUS "  Sysroot: ${CROSS_SYSROOT}")
    else()
        # Fallback to /usr if system compiler not found
        set(CROSS_TOOLS "/usr" CACHE PATH "Path to cross-compilation tools")
        set(CROSS_GCC "${CROSS_TOOLS}/bin/arm-linux-gnueabihf-gcc" CACHE FILEPATH "Cross GCC compiler")
        set(CROSS_GPP "${CROSS_TOOLS}/bin/arm-linux-gnueabihf-g++" CACHE FILEPATH "Cross G++ compiler")
        set(CROSS_CLANG "${CROSS_TOOLS}/bin/clang" CACHE FILEPATH "Cross Clang compiler")
        set(CROSS_SYSROOT "/usr/arm-linux-gnueabihf" CACHE PATH "Cross-compilation sysroot")
        message(WARNING "ARM v6 toolchain not found in system, falling back to standard ARM toolchain")
    endif()
    
    # Try to use Clang first, fallback to GCC
    find_program(CLANG_FOUND ${CROSS_CLANG})
    if(CLANG_FOUND)
        set(CMAKE_C_COMPILER "${CROSS_CLANG}")
        set(CMAKE_CXX_COMPILER "${CROSS_CLANG}++")
        set(CMAKE_C_COMPILER_TARGET "armv6-rpi-linux-gnueabihf")
        set(CMAKE_CXX_COMPILER_TARGET "armv6-rpi-linux-gnueabihf")
        set(COMPILER_DETECTED "CLANG")
            message(STATUS "Selected compiler: Clang for ARMv6 Raspberry Pi cross-compilation")
            message(STATUS "  C Compiler: ${CROSS_CLANG}")
            message(STATUS "  C++ Compiler: ${CROSS_CLANG}++")
    else()
        find_program(GCC_FOUND ${CROSS_GCC})
        if(GCC_FOUND)
            set(CMAKE_C_COMPILER "${CROSS_GCC}")
            # Set C++ compiler if G++ exists
            find_program(GPP_FOUND ${CROSS_GPP})
            if(GPP_FOUND)
                set(CMAKE_CXX_COMPILER "${CROSS_GPP}")
            endif()
            set(COMPILER_DETECTED "GCC")
            message(STATUS "Selected compiler: GCC for ARMv6 Raspberry Pi cross-compilation")
            message(STATUS "  C Compiler: ${CROSS_GCC}")
            if(GPP_FOUND)
                message(STATUS "  C++ Compiler: ${CROSS_GPP}")
            else()
                message(STATUS "  C++ Compiler: Not found")
            endif()
        else()
            message(FATAL_ERROR "Neither Clang nor GCC cross-compiler found!")
        endif()
    endif()
    
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

# Add TLS libraries (always enabled)
target_link_libraries(explo ${WOLFSSL_LIBRARIES})
target_link_libraries(inter ${WOLFSSL_LIBRARIES})
message(STATUS "Linking with wolfSSL libraries: ${WOLFSSL_LIBRARIES}")

# Add build type definitions
target_compile_definitions(explo PRIVATE EXPLO_BUILD)
target_compile_definitions(inter PRIVATE INTER_BUILD)

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
if(TARGET_RASPI_ZERO_W)
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
message(STATUS "   TLS Support: Always Enabled (wolfSSL ${WOLFSSL_VERSION})")
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
if(TARGET_RASPI_ZERO_W)
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
    }
  ]
}
]=])
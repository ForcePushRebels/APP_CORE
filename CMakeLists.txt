cmake_minimum_required(VERSION 3.10)
project(EXPLO)

# Force Ninja generator
set(CMAKE_GENERATOR "Ninja" CACHE INTERNAL "" FORCE)

# Target options
option(TARGET_MRPIZ "Build for MRPiZ target instead of PC" OFF)
option(USE_TLS "Enable TLS support" OFF)
option(DEBUG "Enable debug build" OFF)

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

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
    set(EXTRA_LIBS "m") # math library
else()
    # PC development build with Intox simulator
    set(CMAKE_C_COMPILER "gcc")
    add_compile_definitions(
        INTOX
        # Define the required Intox variables
        intox_address="127.0.0.1"
        intox_port=12345
    )
    
    # Output executable name
    set(EXPLO_EXE_NAME "explo_intox")
    set(INTER_EXE_NAME "inter_intox")
    
    # Link with Intox simulator libraries
    set(MRPIZ_LIB "intoxmrpiz;intox")
    set(EXTRA_LIBS "")
endif()

# Set C standard and flags
set(CMAKE_C_STANDARD 11)

# Common flags from the Makefile
add_compile_options(
    -Wall
    -Wextra
    -Werror
    -pedantic
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

# Add mrpiz library
set(MRPIZ_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lib_mrpiz-x86_64-v0.5/lib")
set(MRPIZ_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lib_mrpiz-x86_64-v0.5/include/mrpiz")
include_directories(${MRPIZ_INCLUDE_DIR})

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

foreach(LIB ${MRPIZ_LIB})
    target_link_libraries(explo "${MRPIZ_LIB_DIR}/lib${LIB}.a")
endforeach()

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

foreach(LIB ${MRPIZ_LIB})
    target_link_libraries(inter "${MRPIZ_LIB_DIR}/lib${LIB}.a")
endforeach()

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
if(TARGET_MRPIZ)
    message(STATUS "  Target: MRPiZ")
else()
    message(STATUS "  Target: PC Development")
    message(STATUS "  Intox Address: 127.0.0.1")
    message(STATUS "  Intox Port: 12345")
endif()
message(STATUS "  TLS Support: ${USE_TLS}")
message(STATUS "  Debug Build: ${DEBUG}")
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
message(STATUS "----------------------------------------")



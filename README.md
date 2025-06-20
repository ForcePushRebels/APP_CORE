# APP_CORE

**Autonomous Robot Core System** - A professional C-based framework for robotic exploration and control systems with advanced networking, TLS security, and multi-platform support.

## 🚀 Overview

APP_CORE is a robust, embedded systems-ready robotics framework designed for autonomous exploration robots. The system features a modular architecture with secure network communication, real-time sensor management, and cross-platform compatibility for both development (x86_64) and production (ARM/Raspberry Pi) environments.

### Key Features

- **🔐 Enterprise-grade Security**: TLS 1.3 encryption with ECC certificates
- **🌐 Advanced Networking**: Multi-threaded TCP server with protocol abstraction
- **🎯 Real-time Control**: Precision motor control and sensor management
- **🗺️ Autonomous Navigation**: Integrated mapping and exploration algorithms
- **🔧 Cross-platform**: Supports x86_64 development and ARM production targets
- **📊 Simulation Ready**: Compatible with 2D/3D Intox simulation environment
- **⚡ Zero-allocation Design**: Memory-safe with fixed-size buffers

## 🏗️ Architecture

The system is organized into two main executable components:

- **EXPLO**: Exploration module
- **INTER**: Intervention module

### Core Modules

```
common/
├── network/          # Secure network communication stack
├── propulsion/       # Motor control and positioning systems  
├── SensorManager/    # Unified sensor data acquisition
├── supervisor/       # High-level system coordination
├── tls/             # TLS encryption and certificate management
├── xOs/             # Operating system abstraction layer
└── xLog/            # Advanced logging framework
```

## 📋 Prerequisites

### Development Environment

- **CMake** 3.10 or higher
- **Ninja** build system
- **Compiler**: Clang (preferred) or GCC with C11 support
- **Dependencies**:
  - `libz` (zlib development headers)
  - `libwolfssl-dev` (WolfSSL for embedded TLS)

### Cross-compilation (Raspberry Pi)

- ARM cross-compilation toolchain (`arm-linux-gnueabihf-gcc`)
- Cross-compiled dependencies for target architecture

## 🛠️ Installation

### 1. Clone Repository

```bash
git clone git@github.com:ForcePushRebels/APP_CORE.git
cd APP_CORE
```

### 2. Install Dependencies

```bash
sudo apt update
sudo apt install -y cmake ninja-build clang libz-dev libwolfssl-dev
```

### 3. Generate TLS Certificates

```bash
./gen_pki.sh
```

This creates a complete 3-tier ECC PKI infrastructure in the `pki/` directory with Y2038-safe certificates.

## 🔨 Compilation

The project uses CMake presets for streamlined builds across different configurations.

### Available Build Configurations

| Configuration | Description | Target | Simulation |
|---------------|-------------|---------|------------|
| `x86_64-debug` | Development debug build | x86_64 | 2D Intox |
| `x86_64-release` | Optimized x86_64 build | x86_64 | 2D Intox |
| `x86_64-3d-debug` | 3D simulation debug | x86_64 | 3D Intox |
| `x86_64-3d-release` | 3D simulation release | x86_64 | 3D Intox |
| `raspi-debug` | Raspberry Pi debug | ARM | Hardware |
| `raspi-release` | Raspberry Pi release | ARM | Hardware |
| `production` | Production build | x86_64 | Hardware |

### Quick Build

#### Development Build (Recommended)
```bash
cmake --preset x86_64-debug
cmake --build build/x86_64-debug
```

#### Production Build
```bash
cmake --preset production
cmake --build build/production
```

#### Raspberry Pi Cross-compilation
```bash
cmake --preset raspi-release
cmake --build build/raspi-release
```

### Manual Configuration

For custom builds or CI/CD environments:

```bash
# Configure
mkdir build && cd build
cmake .. -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DDEBUG=OFF \
    -DTARGET_RASPI_ZERO_W=OFF \
    -DUSE_3D_SIMULATION=OFF

# Compile
ninja -j$(nproc)
```

## 🚀 Usage

### Running the System

```bash
# Start the exploration module
./build/x86_64-debug/bin/explo_intox_2D

# In another terminal, start the interface module
./build/x86_64-debug/bin/inter_intox_2D
```



## 📄 License

This project is proprietary software. Distribution or sharing of this code is strictly forbidden without explicit authorization.

## 🏢 Project Information

- **Project**: Autonomous Robot Core System
- **Language**: C11
- **Build System**: CMake + Ninja
- **Target Platforms**: Linux (x86_64, ARM)
- **Real-time**: Soft real-time capable
- **Security**: TLS 1.3 with ECC certificates (WolfSSL)

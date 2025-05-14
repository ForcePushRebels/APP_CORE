#!/bin/bash

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# Function to print colored status messages
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_section() {
    echo -e "\n${CYAN}${BOLD}$1${NC}"
    echo -e "${CYAN}${BOLD}$(printf '=%.0s' $(seq 1 ${#1}))${NC}"
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if Ninja is available
print_section "CHECKING PREREQUISITES"
if ! command_exists ninja; then
    print_error "Ninja build system is required but not found!"
    print_error "Please install Ninja using your system's package manager:"
    print_error "  Ubuntu/Debian: sudo apt-get install ninja-build"
    print_error "  Fedora: sudo dnf install ninja-build"
    print_error "  macOS: brew install ninja"
    exit 1
else
    NINJA_VERSION=$(ninja --version)
    print_success "Found Ninja build system version $NINJA_VERSION"
fi

# Create build directory if it doesn't exist
print_section "PREPARING BUILD ENVIRONMENT"
print_status "Creating build directory..."

mkdir -p build
cd build || {
    print_error "Failed to create or enter build directory"
    exit 1
}

# Default options
USE_TLS=OFF
TARGET_MRPIZ=OFF
DEBUG=OFF

# Parse command line arguments
print_status "Parsing command line arguments..."
while [[ $# -gt 0 ]]; do
  case $1 in
    --tls)
      USE_TLS=ON
      print_status "TLS support enabled"
      shift
      ;;
    --mrpiz)
      TARGET_MRPIZ=ON
      print_status "Target set to MRPiZ"
      shift
      ;;
    --debug)
      DEBUG=ON
      print_status "Debug build enabled"
      shift
      ;;
    *)
      print_error "Unknown option: $1"
      echo -e "Usage: $0 [--tls] [--mrpiz] [--debug]"
      exit 1
      ;;
  esac
done

# Configure with CMake
print_section "CONFIGURING BUILD"
print_status "Running CMake with Ninja generator..."
cmake -DUSE_TLS=$USE_TLS -DTARGET_MRPIZ=$TARGET_MRPIZ -DDEBUG=$DEBUG .. || {
    print_error "CMake configuration failed"
    exit 1
}

# Build
print_section "BUILDING PROJECT"
print_status "Running build with $(nproc) parallel jobs..."
cmake --build . -- -j$(nproc)

# Check if the build succeeded
if [ $? -eq 0 ]; then
    print_section "BUILD RESULTS"
    print_success "Build completed successfully!"
    
    # Check if executables were actually created
    if [ -f "bin/explo_intox" ] || [ -f "bin/explo_mrpiz" ]; then
        print_success "Executables created in: $(pwd)/bin/"
        ls -la bin/
    else
        print_warning "Build completed but no executables were found in bin/ directory."
    fi
    
    # If building for MRPiZ, provide upload instructions
    if [ "$TARGET_MRPIZ" = "ON" ]; then
        print_section "DEPLOYMENT INSTRUCTIONS"
        echo -e "To upload to MRPiZ:"
        echo -e "  ${BOLD}scp bin/explo_mrpiz pi@<MRPIZ_ADDRESS>:explo_mrpiz${NC}"
    fi
    
    exit 0
else
    print_section "BUILD RESULTS"
    print_error "Build failed! Check the errors above."
    exit 1
fi

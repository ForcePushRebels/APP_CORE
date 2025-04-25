#!/bin/bash

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
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

# Process command line arguments
USE_TLS=""
TARGET_MRPIZ=""
DEBUG=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --tls)
      USE_TLS="--tls"
      shift
      ;;
    --mrpiz)
      TARGET_MRPIZ="--mrpiz"
      shift
      ;;
    --debug)
      DEBUG="--debug"
      shift
      ;;
    *)
      print_error "Unknown option: $1"
      echo -e "Usage: $0 [--tls] [--mrpiz] [--debug]"
      exit 1
      ;;
  esac
done

# Clean build directory
print_section "CLEANING BUILD DIRECTORY"
if [ -d "build" ]; then
    print_status "Removing existing build directory..."
    rm -rf build
    print_success "Build directory removed"
else
    print_status "No existing build directory found. Creating a new one."
fi

# Compile the project
print_section "COMPILING PROJECT"
print_status "Running compile script with options: ${USE_TLS} ${TARGET_MRPIZ} ${DEBUG}"
./compile.sh ${USE_TLS} ${TARGET_MRPIZ} ${DEBUG}

# Check if compilation was successful
if [ $? -ne 0 ]; then
    print_error "Compilation failed. Cannot run application."
    exit 1
fi

# Determine which executable to run
if [ -n "$TARGET_MRPIZ" ]; then
    EXECUTABLE="build/bin/explo_mrpiz"
    print_warning "Running on MRPiZ target is not directly supported in this script."
    print_warning "You need to upload the executable to your MRPiZ device first."
    print_warning "Use: scp ${EXECUTABLE} pi@<MRPIZ_ADDRESS>:explo_mrpiz"
    exit 0
else
    EXECUTABLE="build/bin/explo_intox"
fi

# Check if executable exists
if [ ! -f "$EXECUTABLE" ]; then
    print_error "Executable not found: $EXECUTABLE"
    exit 1
fi

# Run the application
print_section "RUNNING EXPLO APPLICATION"
print_status "Starting application: $EXECUTABLE"
print_status "Press Ctrl+C to stop the application"
echo -e "${BOLD}Application output:${NC}"
echo -e "${YELLOW}-------------- Application Start --------------${NC}"

# Run the application and handle Ctrl+C gracefully
"$EXECUTABLE"
EXIT_CODE=$?

echo -e "${YELLOW}--------------- Application End ---------------${NC}"
if [ $EXIT_CODE -eq 0 ]; then
    print_success "Application exited successfully (code: $EXIT_CODE)"
else
    print_warning "Application exited with code: $EXIT_CODE"
fi 
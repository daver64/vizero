#!/bin/bash
# Unix build script for Vizero

set -e  # Exit on any error

# Handle clean option
if [[ "$1" == "clean" ]]; then
    echo "Cleaning build directory..."
    if [[ -d "build" ]]; then
        # Kill any running vizero processes
        pkill -f vizero 2>/dev/null || true
        
        # Wait a moment for processes to terminate
        sleep 1
        
        # Remove build directory
        rm -rf build
        echo "Build directory cleaned successfully."
    else
        echo "Build directory doesn't exist."
    fi
    [[ "$2" == "" ]] && exit 0
fi

echo "Building Vizero for Unix..."

# Check for required tools
if ! command -v cmake &> /dev/null; then
    echo "Error: CMake is required but not installed"
    exit 1
fi

if ! command -v make &> /dev/null; then
    echo "Error: Make is required but not installed"
    exit 1
fi

# Create build directory
mkdir -p build
cd build

# Configure with CMake
echo "Configuring build..."
cmake .. -DCMAKE_BUILD_TYPE=Release

# Build
echo "Building..."
make -j$(nproc)

echo "Build completed successfully!"
echo "Executable: vizero"
echo "Plugins: plugins/"
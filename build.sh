#!/bin/bash
# Unix build script for Vizero

set -e  # Exit on any error

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
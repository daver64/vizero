@echo off
REM Windows build script for Vizero

REM Handle clean option
if "%1"=="clean" (
    echo Cleaning build directory...
    if exist build (
        rmdir /s /q build
        echo Build directory cleaned.
    ) else (
        echo Build directory doesn't exist.
    )
    if "%2"=="" exit /b 0
)

echo Building Vizero for Windows...

REM Check environment variables
if not defined SDL2_ROOT (
    if not defined SDL_ROOT (
        echo Error: SDL2_ROOT or SDL_ROOT environment variable must be set
        exit /b 1
    )
)

if not defined BOOST_ROOT (
    echo Error: BOOST_ROOT environment variable must be set
    exit /b 1
)

if not defined GLEW_ROOT (
    echo Error: GLEW_ROOT environment variable must be set
    exit /b 1
)

REM Create build directory
if not exist build mkdir build
cd build

REM Configure with CMake
echo Configuring build...
cmake .. -G "Visual Studio 17 2022" -A x64
if errorlevel 1 (
    echo CMake configuration failed
    exit /b 1
)

REM Build
echo Building...
cmake --build . --config Release --parallel
if errorlevel 1 (
    echo Build failed
    cd ..
    exit /b 1
)

REM Return to project root
cd ..

echo Build completed successfully!
echo Executable: vizero.exe
echo Plugins: plugins\
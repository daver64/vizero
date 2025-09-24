@echo off
REM Windows build script for Vizero

REM Handle clean option
if "%1"=="clean" (
    echo Cleaning build directory...
    if exist build (
        echo Attempting to close any running vizero processes...
        taskkill /f /im vizero.exe >nul 2>&1
        taskkill /f /im vizero_tests.exe >nul 2>&1
        
        REM Wait a moment for processes to fully terminate
        timeout /t 2 /nobreak >nul 2>&1
        
        REM Try to remove specific file types first
        echo Removing build artifacts...
        if exist build\*.exe del /q build\*.exe 2>nul
        if exist build\*.dll del /q build\*.dll 2>nul
        if exist build\*.pdb del /q build\*.pdb 2>nul
        if exist build\*.ilk del /q build\*.ilk 2>nul
        if exist build\*.obj del /q build\*.obj 2>nul
        if exist build\*.lib del /q build\*.lib 2>nul
        
        REM Clean Visual Studio temp files
        if exist build\.vs rmdir /s /q build\.vs 2>nul
        if exist build\x64 rmdir /s /q build\x64 2>nul
        if exist build\Debug rmdir /s /q build\Debug 2>nul
        if exist build\Release rmdir /s /q build\Release 2>nul
        
        REM Wait again before trying to remove the entire directory
        timeout /t 1 /nobreak >nul 2>&1
        
        REM Final attempt to remove the entire build directory
        rmdir /s /q build 2>nul
        if exist build (
            echo Build directory could not be completely removed - some files may be locked.
        ) else (
            echo Build directory cleaned successfully.
        )
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
echo Console executable: vizero.exe
echo GUI executable: vizero-gui.exe
echo Plugins: plugins\
# Vizero Development Guide

## Recent Architecture & Bugfixes (2025)

- **Robust buffer/cursor replacement**: Buffer and cursor management now prevents double-free/use-after-free bugs when splitting and loading files. All windows and buffer arrays are always in sync, and no window or array references freed memory.
- **Window manager helpers**: All access to window manager internals is now via safe helper functions, improving code safety and maintainability. Direct struct access is avoided in favor of API helpers in `editor_window.h/cpp`.
- **Input routing and window focus**: All input and editing operations now follow the currently focused window. After any window focus change (e.g., `:wincmd`, `Ctrl+w`, or window navigation), keypresses and text input go to the correct window and buffer, matching vi-like behavior.
- **Crash/corruption fixes**: Resolved crashes and data corruption after split and file load operations.

## Quick Start

### Prerequisites

1. **SDL2**: Download from https://www.libsdl.org/
2. **GLEW**: Download from http://glew.sourceforge.net/
3. **Boost**: Download from https://www.boost.org/
4. **CMake**: Version 3.16 or later

### Environment Setup

#### Windows
```cmd
set SDL2_ROOT=C:\path\to\SDL2
set BOOST_ROOT=C:\path\to\boost
set BOOST_LIBRARY_DIR=C:\path\to\boost\lib64-msvc-14.3
set GLEW_ROOT=C:\path\to\glew
```

#### Linux/FreeBSD
```bash
# Option 1: System packages
sudo apt-get install libsdl2-dev libglew-dev libboost-all-dev  # Ubuntu/Debian
sudo dnf install SDL2-devel glew-devel boost-devel            # Fedora
sudo pkg install sdl2 glew boost-all                          # FreeBSD

# Option 2: Custom installations
export SDL2_ROOT=/usr/local/SDL2
export BOOST_ROOT=/usr/local/boost
export GLEW_ROOT=/usr/local/glew
```

### Building

#### Windows
```cmd
git clone <repository-url> vizero
cd vizero
build.bat
```

#### Unix
```bash
git clone <repository-url> vizero
cd vizero
chmod +x build.sh
./build.sh
```

### Running

```bash
# Basic usage
./vizero

# Open a file
./vizero README.md

# With custom plugin directory
./vizero --plugin-dir ./custom_plugins file.txt
```

## Development Workflow

### Code Organization

- **Headers**: `include/vizero/` - Public API headers
- **Core**: `src/core/` - Application, window, renderer, input
- **Text**: `src/text/` - Buffer, cursor, line management
- **Editor**: `src/editor/` - Modes, commands, state
- **Plugin**: `src/plugin/` - Plugin system implementation
- **Utils**: `src/utils/` - Utility functions

### Adding New Features

1. **Core Features**: Add to appropriate `src/` subdirectory
2. **Plugin Features**: Create new plugin in `plugins/`
3. **API Changes**: Update headers in `include/vizero/`

### Plugin Development

#### Creating a New Plugin

1. Create directory: `plugins/my_plugin/`
2. Add source files: `my_plugin.c`
3. Update `plugins/CMakeLists.txt`:
   ```cmake
   add_vizero_plugin(my_plugin
       my_plugin/my_plugin.c
   )
   ```

#### Plugin Template

```c
#include "vizero/plugin_interface.h"

VIZERO_PLUGIN_DEFINE_INFO(
    "My Plugin",
    "1.0.0",
    "Your Name",
    "Description of plugin functionality",
    VIZERO_PLUGIN_TYPE_GENERIC
);

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, 
                                        vizero_editor_t* editor, 
                                        const vizero_editor_api_t* api) {
    // Setup plugin callbacks
    plugin->callbacks.on_buffer_open = my_on_buffer_open;
    plugin->callbacks.on_command = my_on_command;
    // ... other callbacks
    
    return 0; // Success
}

VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {
    // Cleanup resources
}

// Implement callback functions
static int my_on_buffer_open(vizero_buffer_t* buffer, const char* filename) {
    // Handle buffer open event
    return 0;
}

static int my_on_command(vizero_editor_t* editor, const char* command, const char* args) {
    if (strcmp(command, "mycommand") == 0) {
        // Handle custom command
        return 1; // Command handled
    }
    return 0; // Command not handled
}
```

## Testing

### Building Tests
```bash
cd build
cmake .. -DBUILD_TESTS=ON
make test
```

### Manual Testing
```bash
# Test basic functionality
./vizero
# Press 'i' to enter insert mode
# Type some text
# Press ESC to return to normal mode
# Type ':q' to quit

# Test plugin loading
./vizero --plugin-dir ./plugins
# Should see plugin load messages
```

## Debugging

### Debug Build
```bash
cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
make
```

### Using GDB (Linux/FreeBSD)
```bash
gdb ./vizero
(gdb) run file.txt
(gdb) bt  # backtrace on crash
```

### Using Visual Studio Debugger (Windows)
1. Open `vizero.sln` in Visual Studio
2. Set vizero as startup project
3. Set breakpoints and run with F5

## Performance Profiling

### Using Valgrind (Linux)
```bash
valgrind --tool=callgrind ./vizero file.txt
kcachegrind callgrind.out.*
```

### Using perf (Linux)
```bash
perf record ./vizero file.txt
perf report
```

## Code Style

- Use 4-space indentation
- Function names: `vizero_module_function_name`
- Struct names: `vizero_struct_name_t`
- Constants: `VIZERO_CONSTANT_NAME`
- Follow existing patterns in the codebase

## Contributing Guidelines

1. Fork the repository
2. Create feature branch: `git checkout -b feature/my-feature`
3. Make changes following code style
4. Add tests for new functionality
5. Update documentation
6. Submit pull request

## Common Issues

### Build Errors

**SDL2 not found**
- Verify SDL2_ROOT environment variable
- Check SDL2 installation path
- Ensure lib/x64 subdirectory exists on Windows

**Boost not found**
- Verify BOOST_ROOT environment variable
- Check Boost installation and compilation
- Set BOOST_LIBRARY_DIR if needed

**GLEW linking errors**
- Verify GLEW_ROOT environment variable
- Check GLEW installation
- Ensure correct architecture (x64/x86)

### Runtime Issues

**Plugin not loading**
- Check plugin directory exists
- Verify plugin exports required functions
- Check plugin API version compatibility

**Crashes on startup**
- Check SDL2 DLLs are in PATH or executable directory
- Verify graphics drivers support OpenGL 3.3
- Run with debugger to identify crash location

**Input not following window focus?**
- This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window. If you encounter issues, check that you are using the latest code and that all buffer/cursor access goes through the window manager helpers.

**Crashes after split or file load?**
- These have been resolved with robust buffer/cursor management. If you see new issues, check for direct struct access or missing helper usage in new code.
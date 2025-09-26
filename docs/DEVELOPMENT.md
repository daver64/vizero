# Vizero Development Guide


## September 2025: Major Architecture, Features & Bugfixes

- **Complete Buffer Management System**: Fixed buffer duplication issues, corrected `:bn`/`:bp` navigation, and added direct buffer switching with `:b1`, `:b2`, etc. commands.
- **Interactive Buffer Selector**: `:buffers` command now opens a visual buffer browser with arrow key navigation and Enter to switch buffers.
- **Insert Mode Cursor**: Insert mode now displays traditional underline cursor instead of block cursor for better vi compatibility.
- **Logo Display System**: Professional logo display on startup when no files are loaded, using SDL2_image integration.
- **Search System Improvements**: Fixed double-character input issues in search mode (`/`, `?`) and command mode (`:`) through improved SDL event handling.
- **Word Wrap (linewrap) by Default**: Lines wrap at word boundaries, with hanging indent for wrapped lines. Toggle with `:set linewrap on|off`. Rendering and movement logic are unified for robust cursor/scrolling.
- **Multi-Language Syntax Highlighting**: Comprehensive syntax highlighting for C/C++/Assembly, C#, Markdown, XML, Python, and Common Lisp with REPL-aware buffer support. Implemented as modular plugins in the renderer system.
- **Status Bar Improvements**: Status bar now features a right-aligned time/date panel, auto-reverting status messages, and clear error/info popups. Panel system supports left/right alignment.
- **Robust Cursor and Scrolling**: Cursor always visible, including on empty lines. Vertical scrolling and cursor movement are robust, with preferred column logic for up/down and correct mapping between logical and visual cursor positions.
- **Window Focus and Input Routing**: All input and editing operations always follow the currently focused window, matching vi-like behavior. After any window focus change (e.g., `Ctrl+w`), all input goes to the correct window and buffer.
- **Crash/Corruption Fixes**: Resolved all known crashes and data corruption after split and file load operations. Buffer and window arrays are always in sync.
- **Merged Rendering Logic**: Word wrap, syntax highlighting, and cursor/scrolling are now unified in the renderer for consistent behavior.
- **Settings System**: All settings (including word wrap, line numbers, compiler preferences, etc.) are persistent and saved to `%APPDATA%\Vizero\settings.ini`. Configure compilers with `:set c_compiler gcc|msvc`, `:set cpp_compiler g++|msvc`, and `:set assembler nasm|fasm`.
- **Build Warnings Eliminated**: All known build warnings have been resolved. The build is clean on MSVC, GCC, and Clang.
- **Colour Theme System**: Complete theming infrastructure with Default, Monokai, and Solarized Dark themes. Switch themes with `:colourscheme <theme>` command for immediate visual feedback.
- **Session Management Infrastructure**: Session management command framework exists with `:mksession`, `:session`, `:sessions`, and `:session-save` commands parsed and recognized, but actual implementation is marked as TODO.
- **Language Server Protocol Integration**: Full LSP support with clangd for C/C++ development, providing intelligent code completion (Ctrl+Space), real-time diagnostics, and graceful degradation when language servers are unavailable.
- **LISP REPL commands not working?**: Check that commands are properly registered in the `lisp_commands[]` array. The `lisp-slime-connect` handler exists but is not registered. **LSP completion crashes?**: These have been fixed with robust JSON parsing and 32KB buffer support. If you see issues, check `src/lsp/lsp_client.cpp` message processing.
- **Production-Ready clangd Plugin**: Complete implementation with automatic clangd discovery, memory-safe operation, and comprehensive error handling.
- **Cross-Platform Plugin Loading**: Plugin system now correctly handles platform-specific extensions (.dll on Windows, .so on Linux/Unix) by automatically converting manifest entries at runtime, enabling seamless cross-platform plugin deployment.
- **Plugin Command Registration**: Complete command registration system allowing plugins to register custom vi commands (e.g., `:irc`, `:git`, `:ssh`). Commands are automatically discovered and routed through the standard vi command parser with full argument support and error handling.

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
- **LSP**: `src/lsp/` - Language Server Protocol client implementation
- **Plugin**: `src/plugin/` - Plugin system implementation
- **Utils**: `src/utils/` - Utility functions
- **UI**: `src/ui/` - Editor windows, completion popups, rendering


### Adding New Features
1. **Core Features**: Add to appropriate `src/` subdirectory. For editor/renderer changes, see `src/ui/editor_window.cpp` and related files.
2. **Language Server Features**: Extend `src/lsp/lsp_client.cpp` and plugin callbacks in `include/vizero/plugin_interface.h`. See `plugins/clangd/` for complete LSP implementation example.
3. **Plugin Features**: Create new plugin in `plugins/`. For syntax highlighting, see any of the language plugins (C, Python, Lisp, Markdown, XML, C#). For LSP plugins, see the clangd plugin structure.
4. **API Changes**: Update headers in `include/vizero/`.


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
4. For syntax highlighting, see any of the language plugins (Python, Lisp, C, Markdown, XML, C#) for examples of colour mapping, tokenization, and REPL buffer detection.

#### Plugin Template

```c
#include "vizero/plugin_interface.h"

VIZERO_PLUGIN_DEFINE_INFO(
    "My Plugin",
    "1.0.0",
    "Your Name",
    "Description of plugin functionality",
    VIZERO_PLUGIN_TYPE_GENERIC  // or VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER
);

VIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, 
                                        vizero_editor_t* editor, 
                                        const vizero_editor_api_t* api) {
    // Setup plugin callbacks
    plugin->callbacks.on_buffer_open = my_on_buffer_open;
    plugin->callbacks.on_command = my_on_command;
    
    // For LSP plugins, also set:
    // plugin->callbacks.lsp_completion = my_lsp_completion;
    // plugin->callbacks.lsp_hover = my_lsp_hover;
    // etc.
    
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

// For LSP plugins
static int my_lsp_completion(vizero_buffer_t* buffer, vizero_position_t position, 
                            vizero_completion_list_t** result) {
    // Implement code completion
    // See plugins/clangd/clangd_plugin.c for full example
    return 0;
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

gdb ./vizero

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
4. For word wrap, cursor, or rendering bugs, set breakpoints in `src/ui/editor_window.cpp` and `src/ui/editor_window_move_visual_row.c`.

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


**Word wrap not working?**: Word wrap is enabled by default. Use `:set linewrap off` to disable. For bugs, check `src/ui/editor_window.cpp` and `src/ui/editor_window_move_visual_row.c`.
**Markdown highlighting missing?**: Ensure you are editing a `.md` file. See the Markdown plugin for implementation details.
**Status bar not updating?**: The right-aligned time/date panel is always visible. Status messages revert to default after a short timeout.
**Cursor disappears or scrolling broken?**: The cursor is always visible, including on empty lines. Up/down movement preserves the preferred column, and scrolling is smooth in all window modes.
**Input not following window focus?**: This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window. If you encounter issues, check that you are using the latest code and that all buffer/cursor access goes through the window manager helpers.
**Crashes after split or file load?**: These have been resolved with robust buffer/cursor management. If you see new issues, check for direct struct access or missing helper usage in new code.
**LSP completion not working?**: Check that clangd is installed in `vizero/clangd/bin/clangd.exe` or system PATH. The editor gracefully handles missing language servers.
**LSP completion crashes?**: These have been fixed with robust JSON parsing and 32KB buffer support. If you see issues, check `src/lsp/lsp_client.cpp` message processing.

# Vizero Development Guide

## October 2025: Claude LLM Integration & Performance Revolution

### AI-Powered Development
- **Claude LLM Integration**: Direct integration with Claude Haiku for AI-powered coding assistance within the editor
- **Interactive AI Commands**: `:claude-chat` and `:claude-ask` commands for real-time AI help with code
- **Secure API Handling**: Local API key storage with secure HTTPS communication to Anthropic servers

### Performance Optimization Revolution
- **Event-Driven Rendering**: Intelligent rendering system reducing idle CPU usage by 90% (30% → 3% on Linux)
- **Smart Update Detection**: Tracks input events, LSP updates, file changes, and animations to render only when necessary
- **Full-Screen Plugin Support**: Optimized 20 FPS updates for IRC/REPL plugins while maintaining ultra-low idle usage
- **Responsive Performance**: Immediate 60 FPS response during user interaction, near-zero CPU when idle

## September 2025: Phase 4 Advanced Features & Polish (Version 0.0.5)

## September 2025: Major Architecture, Features & Bugfixes

### Core Architecture Improvements

- **Complete Buffer Management System**: Fixed buffer duplication issues, corrected `:bn`/`:bp` navigation, and added direct buffer switching with `:b1`, `:b2`, etc. commands.

- **Complete Buffer Management System**: Fixed buffer duplication issues, corrected `:bn`/`:bp` navigation, and added direct buffer switching with `:b1`, `:b2`, etc. commands.- **Interactive Buffer Selector**: `:buffers` command now opens a visual buffer browser with arrow key navigation and Enter to switch buffers.

- **Interactive Buffer Selector**: `:buffers` command now opens a visual buffer browser with arrow key navigation and Enter to switch buffers.- **Insert Mode Cursor**: Insert mode now displays traditional underline cursor instead of block cursor for better vi compatibility.

- **Insert Mode Cursor**: Insert mode now displays traditional underline cursor instead of block cursor for better vi compatibility.- **Logo Display System**: Professional logo display on startup when no files are loaded, using SDL2_image integration.

- **Logo Display System**: Professional logo display on startup when no files are loaded, using SDL2_image integration.- **Search System Improvements**: Fixed double-character input issues in search mode (`/`, `?`) and command mode (`:`) through improved SDL event handling.

- **Search System Improvements**: Fixed double-character input issues in search mode (`/`, `?`) and command mode (`:`) through improved SDL event handling.- **Word Wrap (linewrap) by Default**: Lines wrap at word boundaries, with hanging indent for wrapped lines. Toggle with `:set linewrap on|off`. Rendering and movement logic are unified for robust cursor/scrolling.

- **Word Wrap (linewrap) by Default**: Lines wrap at word boundaries, with hanging indent for wrapped lines. Toggle with `:set linewrap on|off`. Rendering and movement logic are unified for robust cursor/scrolling.- **Multi-Language Syntax Highlighting**: Comprehensive syntax highlighting for C/C++/Assembly, C#, Markdown, XML, Python, and Common Lisp with REPL-aware buffer support. Implemented as modular plugins in the renderer system.

- **Multi-Language Syntax Highlighting**: Comprehensive syntax highlighting for C/C++/Assembly, C#, Markdown, XML, Python, and Common Lisp with REPL-aware buffer support. Implemented as modular plugins in the renderer system.- **Status Bar Improvements**: Status bar now features a right-aligned time/date panel, auto-reverting status messages, and clear error/info popups. Panel system supports left/right alignment.

- **Status Bar Improvements**: Status bar now features a right-aligned time/date panel, auto-reverting status messages, and clear error/info popups. Panel system supports left/right alignment.- **Robust Cursor and Scrolling**: Cursor always visible, including on empty lines. Vertical scrolling and cursor movement are robust, with preferred column logic for up/down and correct mapping between logical and visual cursor positions.

- **Robust Cursor and Scrolling**: Cursor always visible, including on empty lines. Vertical scrolling and cursor movement are robust, with preferred column logic for up/down and correct mapping between logical and visual cursor positions.- **Window Focus and Input Routing**: All input and editing operations always follow the currently focused window, matching vi-like behavior. After any window focus change (e.g., `Ctrl+w`), all input goes to the correct window and buffer.

- **Window Focus and Input Routing**: All input and editing operations always follow the currently focused window, matching vi-like behavior. After any window focus change (e.g., `Ctrl+w`), all input goes to the correct window and buffer.- **Crash/Corruption Fixes**: Resolved all known crashes and data corruption after split and file load operations. Buffer and window arrays are always in sync.

- **Crash/Corruption Fixes**: Resolved all known crashes and data corruption after split and file load operations. Buffer and window arrays are always in sync.- **Merged Rendering Logic**: Word wrap, syntax highlighting, and cursor/scrolling are now unified in the renderer for consistent behavior.

- **Settings System**: All settings (including word wrap, line numbers, compiler preferences, etc.) are persistent and saved to `%APPDATA%\Vizero\settings.ini`. Configure compilers with `:set c_compiler gcc|msvc`, `:set cpp_compiler g++|msvc`, and `:set assembler nasm|fasm`.

### Phase 4: Advanced Features & Polish- **Build Warnings Eliminated**: All known build warnings have been resolved. The build is clean on MSVC, GCC, and Clang.

- **Colour Theme System**: Complete theming infrastructure with Default, Monokai, and Solarized Dark themes. Switch themes with `:colourscheme <theme>` command for immediate visual feedback.

#### Advanced Editing Features- **Session Management Infrastructure**: Session management command framework exists with `:mksession`, `:session`, `:sessions`, and `:session-save` commands parsed and recognized, but actual implementation is marked as TODO.

- **Multiple Cursors**: Full multi-cursor support with synchronized text operations across all cursor positions. Implementation in `src/editor/editor_state.cpp` with complete API in `include/vizero/editor_state.h`.- **Language Server Protocol Integration**: Full LSP support with clangd for C/C++ development, providing intelligent code completion (Ctrl+Space), real-time diagnostics, and graceful degradation when language servers are unavailable.

- **Block/Rectangular Selection**: Column-wise text selection and manipulation with copy/cut/paste operations preserving rectangular structure.- **LISP REPL commands not working?**: Check that commands are properly registered in the `lisp_commands[]` array. The `lisp-slime-connect` handler exists but is not registered. **LSP completion crashes?**: These have been fixed with robust JSON parsing and 32KB buffer support. If you see issues, check `src/lsp/lsp_client.cpp` message processing.

- **Advanced Find/Replace**: Enhanced search system with regex support, capture groups, interactive replacement, case sensitivity controls, and whole-word matching.- **Production-Ready clangd Plugin**: Complete implementation with automatic clangd discovery, memory-safe operation, and comprehensive error handling.

- **Code Folding System**: Language-aware code folding infrastructure with support for functions, classes, blocks, comments, and custom regions. API defined in `include/vizero/code_folding.h`.- **Cross-Platform Plugin Loading**: Plugin system now correctly handles platform-specific extensions (.dll on Windows, .so on Linux/Unix) by automatically converting manifest entries at runtime, enabling seamless cross-platform plugin deployment.

- **Smart Indentation**: Multi-language intelligent indentation system with configurable styles, bracket matching, and automatic formatting. Complete implementation framework in `include/vizero/smart_indent.h`.- **Plugin Command Registration**: Complete command registration system allowing plugins to register custom vi commands (e.g., `:irc`, `:git`, `:ssh`). Commands are automatically discovered and routed through the standard vi command parser with full argument support and error handling.



#### UI/UX Enhancements  ## Quick Start

- **Command Palette**: Comprehensive command system with fuzzy search, categorization, and extensible command registration. Full API in `include/vizero/command_palette.h`.

- **Centralized Version Management**: Unified version system (0.0.5) with single source of truth in `include/vizero/version.h` for all components and plugins.### Prerequisites

- **Enhanced Plugin Architecture**: Improved plugin system with version consistency, robust error handling, and comprehensive API interfaces.

1. **SDL2**: Download from https://www.libsdl.org/

#### System Maturity2. **GLEW**: Download from http://glew.sourceforge.net/

- **Build System Integration**: All new features compile successfully with CMake/MSBuild compatibility maintained across platforms.3. **Boost**: Download from https://www.boost.org/

- **Memory Management**: Comprehensive bounds checking, resource cleanup, and graceful error recovery for all new systems.4. **CMake**: Version 3.16 or later

- **API Design**: Consistent naming conventions, extensible interfaces, and comprehensive documentation with usage examples.

### Environment Setup

### Previous Major Features

#### Windows

- **Merged Rendering Logic**: Word wrap, syntax highlighting, and cursor/scrolling are now unified in the renderer for consistent behavior.```cmd

- **Settings System**: All settings (including word wrap, line numbers, compiler preferences, etc.) are persistent and saved to `%APPDATA%\Vizero\settings.ini`. Configure compilers with `:set c_compiler gcc|msvc`, `:set cpp_compiler g++|msvc`, and `:set assembler nasm|fasm`.set SDL2_ROOT=C:\path\to\SDL2

- **Build Warnings Eliminated**: All known build warnings have been resolved. The build is clean on MSVC, GCC, and Clang.set BOOST_ROOT=C:\path\to\boost

- **Colour Theme System**: Complete theming infrastructure with Default, Monokai, and Solarized Dark themes. Switch themes with `:colourscheme <theme>` command for immediate visual feedback.set BOOST_LIBRARY_DIR=C:\path\to\boost\lib64-msvc-14.3

- **Session Management Infrastructure**: Session management command framework exists with `:mksession`, `:session`, `:sessions`, and `:session-save` commands parsed and recognized, but actual implementation is marked as TODO.set GLEW_ROOT=C:\path\to\glew

- **Language Server Protocol Integration**: Full LSP support with clangd for C/C++ development, providing intelligent code completion (Ctrl+Space), real-time diagnostics, and graceful degradation when language servers are unavailable.```

- **Production-Ready clangd Plugin**: Complete implementation with automatic clangd discovery, memory-safe operation, and comprehensive error handling.

- **Cross-Platform Plugin Loading**: Plugin system now correctly handles platform-specific extensions (.dll on Windows, .so on Linux/Unix) by automatically converting manifest entries at runtime, enabling seamless cross-platform plugin deployment.#### Linux/FreeBSD

- **Plugin Command Registration**: Complete command registration system allowing plugins to register custom vi commands (e.g., `:irc`, `:git`, `:ssh`). Commands are automatically discovered and routed through the standard vi command parser with full argument support and error handling.```bash

# Option 1: System packages

## Quick Startsudo apt-get install libsdl2-dev libglew-dev libboost-all-dev  # Ubuntu/Debian

sudo dnf install SDL2-devel glew-devel boost-devel            # Fedora

### Prerequisitessudo pkg install sdl2 glew boost-all                          # FreeBSD



1. **SDL2**: Download from https://www.libsdl.org/# Option 2: Custom installations

2. **GLEW**: Download from http://glew.sourceforge.net/export SDL2_ROOT=/usr/local/SDL2

3. **Boost**: Download from https://www.boost.org/export BOOST_ROOT=/usr/local/boost

4. **CMake**: Version 3.16 or laterexport GLEW_ROOT=/usr/local/glew

```

### Environment Setup

### Building

#### Windows

```cmd#### Windows

set SDL2_ROOT=C:\path\to\SDL2```cmd

set BOOST_ROOT=C:\path\to\boostgit clone <repository-url> vizero

set BOOST_LIBRARY_DIR=C:\path\to\boost\lib64-msvc-14.3cd vizero

set GLEW_ROOT=C:\path\to\glewbuild.bat

``````



#### Linux/FreeBSD#### Unix

```bash```bash

# Option 1: System packagesgit clone <repository-url> vizero

sudo apt-get install libsdl2-dev libglew-dev libboost-all-dev  # Ubuntu/Debiancd vizero

sudo dnf install SDL2-devel glew-devel boost-devel            # Fedorachmod +x build.sh

./build.sh

# Option 2: Environment variables```

export SDL2_ROOT=/usr/local

export BOOST_ROOT=/usr/local### Running

export GLEW_ROOT=/usr/local

``````bash

# Basic usage

### Building./vizero



#### Windows (MSVC)# Open a file

```cmd./vizero README.md

mkdir build

cd build# With custom plugin directory

cmake .. -G "Visual Studio 17 2022" -A x64./vizero --plugin-dir ./custom_plugins file.txt

cmake --build . --config Release```

```

## Development Workflow

#### Linux/FreeBSD

```bash### Code Organization

mkdir build

cd build- **Headers**: `include/vizero/` - Public API headers

cmake ..- **Core**: `src/core/` - Application, window, renderer, input

make -j$(nproc)- **Text**: `src/text/` - Buffer, cursor, line management

```- **Editor**: `src/editor/` - Modes, commands, state

- **LSP**: `src/lsp/` - Language Server Protocol client implementation

#### Cross-Platform Build Script- **Plugin**: `src/plugin/` - Plugin system implementation

```bash- **Utils**: `src/utils/` - Utility functions

# Windows- **UI**: `src/ui/` - Editor windows, completion popups, rendering

build.bat



# Unix-like systems  ### Adding New Features

./build.sh1. **Core Features**: Add to appropriate `src/` subdirectory. For editor/renderer changes, see `src/ui/editor_window.cpp` and related files.

```2. **Language Server Features**: Extend `src/lsp/lsp_client.cpp` and plugin callbacks in `include/vizero/plugin_interface.h`. See `plugins/clangd/` for complete LSP implementation example.

3. **Plugin Features**: Create new plugin in `plugins/`. For syntax highlighting, see any of the language plugins (C, Python, Lisp, Markdown, XML, C#). For LSP plugins, see the clangd plugin structure.

## Architecture Overview4. **API Changes**: Update headers in `include/vizero/`.



### Core Components

### Plugin Development

#### Application Layer (`src/core/`)

- **SDL2 Integration**: Window management, input handling, and OpenGL context#### Creating a New Plugin

- **Settings System**: Persistent configuration with INI file storage1. Create directory: `plugins/my_plugin/`

- **Version Management**: Centralized version control across all components2. Add source files: `my_plugin.c`

3. Update `plugins/CMakeLists.txt`:

#### Editor Engine (`src/editor/`)   ```cmake

- **Editor State**: Multi-buffer management with vi-compatible modes   add_vizero_plugin(my_plugin

- **Command Parser**: Comprehensive vi command parsing and execution       my_plugin/my_plugin.c

- **Mode Manager**: Normal, Insert, Visual, and Command mode handling   )

   ```

#### Text Processing (`src/text/`)4. For syntax highlighting, see any of the language plugins (Python, Lisp, C, Markdown, XML, C#) for examples of colour mapping, tokenization, and REPL buffer detection.

- **Buffer System**: Efficient line-based text storage with undo history5. For REPL functionality, see `plugins/lisp_repl/` for interactive language REPL or `plugins/sql_repl/` for database integration examples.

- **Cursor Management**: Multi-cursor support with synchronized operations

- **Search Engine**: Advanced search with regex, capture groups, and highlighting#### Plugin Template



#### UI System (`src/ui/`)```c

- **Editor Window**: Multi-window support with split functionality#include "vizero/plugin_interface.h"

- **Status Bar**: Information display with theme integration

- **Rendering**: Hardware-accelerated OpenGL text renderingVIZERO_PLUGIN_DEFINE_INFO(

    "My Plugin",

#### Plugin System (`src/plugin/`)    "1.0.0",

- **Dynamic Loading**: Cross-platform plugin architecture (.dll/.so)    "Your Name",

- **API Interface**: Comprehensive C API for plugin development      "Description of plugin functionality",

- **Version Management**: Plugin version consistency and compatibility    VIZERO_PLUGIN_TYPE_GENERIC  // or VIZERO_PLUGIN_TYPE_LANGUAGE_SERVER

);

#### Advanced Features (Phase 4)

- **Multiple Cursors**: Synchronized editing across multiple positionsVIZERO_PLUGIN_API int vizero_plugin_init(vizero_plugin_t* plugin, 

- **Block Selection**: Rectangular text selection and manipulation                                        vizero_editor_t* editor, 

- **Code Folding**: Language-aware structure folding                                        const vizero_editor_api_t* api) {

- **Smart Indentation**: Context-sensitive auto-indentation    // Setup plugin callbacks

- **Command Palette**: Searchable command interface    plugin->callbacks.on_buffer_open = my_on_buffer_open;

    plugin->callbacks.on_command = my_on_command;

### Key Design Patterns    

    // For LSP plugins, also set:

#### Window-Buffer Relationship    // plugin->callbacks.lsp_completion = my_lsp_completion;

Each window maintains its own buffer reference and cursor state. The window manager routes all input to the focused window, ensuring vi-like behavior across splits.    // plugin->callbacks.lsp_hover = my_lsp_hover;

    // etc.

#### Plugin Architecture    

Function pointer-based callbacks with versioned API:    // For command registration (REPL/database plugins):

```c    // plugin->callbacks.commands = my_commands;

typedef struct {    // plugin->callbacks.command_count = my_command_count;

    int version;    

    int (*initialize)(void);    return 0; // Success

    void (*cleanup)(void);}

    int (*highlight_syntax)(/* parameters */);

    /* Additional callbacks */VIZERO_PLUGIN_API void vizero_plugin_cleanup(vizero_plugin_t* plugin) {

} vizero_plugin_callbacks_t;    // Cleanup resources

```}



#### Memory Management// Implement callback functions

- **RAII Pattern**: Structured create/destroy pairs for all resourcesstatic int my_on_buffer_open(vizero_buffer_t* buffer, const char* filename) {

- **Safe Allocation**: Comprehensive bounds checking and error handling    // Handle buffer open event

- **Plugin Isolation**: Plugin memory managed independently    return 0;

}

## Plugin Development

static int my_on_command(vizero_editor_t* editor, const char* command, const char* args) {

### Basic Plugin Structure    if (strcmp(command, "mycommand") == 0) {

        // Handle custom command

```c        return 1; // Command handled

#include "vizero/plugin_interface.h"    }

#include "vizero/version.h"    return 0; // Command not handled

}

// Use centralized version system

VIZERO_PLUGIN_DEFINE_INFO(// For LSP plugins

    "My Plugin",static int my_lsp_completion(vizero_buffer_t* buffer, vizero_position_t position, 

    VIZERO_PLUGIN_VERSION,                            vizero_completion_list_t** result) {

    "Author Name",    // Implement code completion

    "Plugin description",    // See plugins/clangd/clangd_plugin.c for full example

    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER    return 0;

);}



static int my_plugin_init(void) {// For REPL/database plugins with command registration

    // Initialize pluginstatic vizero_command_t my_commands[] = {

    return 0;    {

}        .name = "mycommand",

        .handler = my_command_handler,

static void my_plugin_cleanup(void) {        .description = "Custom command description"

    // Cleanup resources    },

}    // Add more commands as needed

};

// Export required functions

VIZERO_PLUGIN_EXPORT int vizero_plugin_init(vizero_plugin_callbacks_t* callbacks) {static int my_command_handler(vizero_editor_t* editor, const char* args) {

    callbacks->initialize = my_plugin_init;    // Implementation of custom command

    callbacks->cleanup = my_plugin_cleanup;    // See plugins/sql_repl/sql_repl_plugin.c for database integration example

    return 0;    // See plugins/lisp_repl/lisp_repl_plugin.c for interactive REPL example

}    return 1;

}

VIZERO_PLUGIN_EXPORT void vizero_plugin_cleanup(void) {```

    my_plugin_cleanup();

}## Testing



VIZERO_PLUGIN_EXPORT const vizero_plugin_info_t* vizero_plugin_get_info(void) {### Building Tests

    return &plugin_info;```bash

}cd build

```cmake .. -DBUILD_TESTS=ON

make test

### Plugin Types```



#### Syntax Highlighting Plugins### Manual Testing

Provide language-specific syntax highlighting:```bash

```c# Test basic functionality

static int highlight_syntax(vizero_buffer_t* buffer, size_t start_line, size_t end_line,./vizero

                           vizero_syntax_token_t* tokens, size_t max_tokens, size_t* token_count) {# Press 'i' to enter insert mode

    // Analyze text and generate syntax tokens# Type some text

    // Fill tokens array with color/style information# Press ESC to return to normal mode

    return 0;# Type ':q' to quit

}

```# Test plugin loading

./vizero --plugin-dir ./plugins

#### Interactive Plugins (REPL, IRC, SQL)# Should see plugin load messages

Provide interactive functionality with custom commands:```

```c

static vizero_plugin_command_t commands[] = {gdb ./vizero

    {"connect", "Connect to server", handle_connect},

    {"disconnect", "Disconnect from server", handle_disconnect},## Debugging

    {NULL, NULL, NULL}

};### Debug Build

```bash

// Register commands in plugin initializationcd build

callbacks->commands = commands;cmake .. -DCMAKE_BUILD_TYPE=Debug

```make

```

### Phase 4 Plugin Enhancements

### Using GDB (Linux/FreeBSD)

#### Version Consistency```bash

All plugins now use the centralized version system:gdb ./vizero

```c(gdb) run file.txt

#include "vizero/version.h"(gdb) bt  # backtrace on crash

```

VIZERO_PLUGIN_DEFINE_INFO(

    "Plugin Name",### Using Visual Studio Debugger (Windows)

    VIZERO_PLUGIN_VERSION,  // Uses centralized version (0.0.5)1. Open `vizero.sln` in Visual Studio

    "Author",2. Set vizero as startup project

    "Description", 3. Set breakpoints and run with F5

    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER4. For word wrap, cursor, or rendering bugs, set breakpoints in `src/ui/editor_window.cpp` and `src/ui/editor_window_move_visual_row.c`.

);

```## Performance Profiling



#### Enhanced API Access### Using Valgrind (Linux)

Plugins can access advanced Phase 4 features:```bash

- Multiple cursor operationsvalgrind --tool=callgrind ./vizero file.txt

- Block selection functionality  kcachegrind callgrind.out.*

- Code folding integration```

- Smart indentation hooks

- Command palette registration### Using perf (Linux)

```bash

## Build Systemperf record ./vizero file.txt

perf report

### CMake Configuration```



The build system uses CMake with comprehensive cross-platform support:## Code Style



#### Main CMakeLists.txt Features- Use 4-space indentation

- **Dependency Detection**: Automatic detection of SDL2, GLEW, Boost libraries- Function names: `vizero_module_function_name`

- **Platform Handling**: Windows/Unix path and library differences- Struct names: `vizero_struct_name_t`

- **Plugin Support**: Automatic plugin compilation and installation- Constants: `VIZERO_CONSTANT_NAME`

- **Version Integration**: Centralized version management- Follow existing patterns in the codebase



#### Plugin Build System## Contributing Guidelines

Plugins use a standardized CMake macro:

```cmake1. Fork the repository

add_vizero_plugin(plugin_name2. Create feature branch: `git checkout -b feature/my-feature`

    SOURCES plugin_source.c3. Make changes following code style

    LIBRARIES additional_libs4. Add tests for new functionality

    DEFINITIONS PLUGIN_SPECIFIC_DEFINES5. Update documentation

)6. Submit pull request

```

## Common Issues

### Cross-Platform Considerations

### Build Errors

#### Library Handling

- **Windows**: Uses environment variables (SDL2_ROOT, BOOST_ROOT, GLEW_ROOT)**SDL2 not found**

- **Unix**: Prefers system packages, falls back to environment variables  - Verify SDL2_ROOT environment variable

- **Boost**: Complex version-specific library name resolution- Check SDL2 installation path

- Ensure lib/x64 subdirectory exists on Windows

#### Plugin Extensions

- **Runtime Detection**: Automatically handles .dll (Windows) vs .so (Unix)**Boost not found**

- **Manifest Conversion**: Plugin manifests specify .dll, runtime converts as needed- Verify BOOST_ROOT environment variable

- Check Boost installation and compilation

## Testing and Quality Assurance- Set BOOST_LIBRARY_DIR if needed



### Build Validation**GLEW linking errors**

- **Warning-Free Builds**: All compilation warnings eliminated- Verify GLEW_ROOT environment variable

- **Multi-Compiler Support**: Tested with MSVC, GCC, and Clang- Check GLEW installation

- **Cross-Platform**: Windows and Unix build verification- Ensure correct architecture (x64/x86)



### Feature Testing### Runtime Issues

- **Buffer Management**: Multi-buffer operations and window focus handling

- **Memory Safety**: No crashes during buffer/window operations**Plugin not loading**

- **Plugin Loading**: Dynamic plugin loading and unloading- Check plugin directory exists

- Verify plugin exports required functions

### Phase 4 Testing- Check plugin API version compatibility

- **Multiple Cursors**: Synchronized operations across all positions

- **Block Selection**: Rectangular operations with proper clipboard integration**Crashes on startup**

- **Code Folding**: Language-aware folding with visual feedback- Check SDL2 DLLs are in PATH or executable directory

- **Command Palette**: Fuzzy search and command execution- Verify graphics drivers support OpenGL 3.3

- Run with debugger to identify crash location

## Performance Optimization



### Rendering Performance**Word wrap not working?**: Word wrap is enabled by default. Use `:set linewrap off` to disable. For bugs, check `src/ui/editor_window.cpp` and `src/ui/editor_window_move_visual_row.c`.

- **Hardware Acceleration**: OpenGL-based text rendering at 60fps**Markdown highlighting missing?**: Ensure you are editing a `.md` file. See the Markdown plugin for implementation details.

- **Efficient Updates**: Minimal redraws with change tracking**Status bar not updating?**: The right-aligned time/date panel is always visible. Status messages revert to default after a short timeout.

- **Large File Handling**: Optimized for files with thousands of lines**Cursor disappears or scrolling broken?**: The cursor is always visible, including on empty lines. Up/down movement preserves the preferred column, and scrolling is smooth in all window modes.

**Input not following window focus?**: This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window. If you encounter issues, check that you are using the latest code and that all buffer/cursor access goes through the window manager helpers.

### Memory Efficiency  **Crashes after split or file load?**: These have been resolved with robust buffer/cursor management. If you see new issues, check for direct struct access or missing helper usage in new code.

- **Buffer Management**: Efficient line storage with minimal overhead**LSP completion not working?**: Check that clangd is installed in `vizero/clangd/bin/clangd.exe` or system PATH. The editor gracefully handles missing language servers.

- **Plugin Isolation**: Prevent plugin memory leaks from affecting core**LSP completion crashes?**: These have been fixed with robust JSON parsing and 32KB buffer support. If you see issues, check `src/lsp/lsp_client.cpp` message processing.

- **Resource Cleanup**: Comprehensive cleanup on shutdown

### Search Performance
- **Compiled Regex**: Cached regex compilation for repeated searches
- **Incremental Search**: Real-time search feedback without performance impact
- **Large File Search**: Optimized algorithms for multi-megabyte files

## Contributing Guidelines

### Code Style
- **C Style**: Follow existing conventions for C code
- **C++ Style**: Modern C++ practices where appropriate
- **Documentation**: Comprehensive header documentation with examples

### Pull Request Process
1. **Fork Repository**: Create personal fork for development
2. **Feature Branch**: Create branch for specific feature/fix
3. **Testing**: Ensure all features work and build successfully
4. **Documentation**: Update relevant documentation files
5. **Pull Request**: Submit with comprehensive description

### Development Areas

#### High Priority
- **Code Folding Implementation**: Complete C implementation of folding logic
- **Smart Indentation Engine**: Language-specific indentation rules
- **Command Palette UI**: Visual command palette interface
- **Session Persistence**: Complete session save/restore functionality

#### Medium Priority  
- **Additional Language Plugins**: More syntax highlighting languages
- **Theme System Extensions**: More built-in themes and customization
- **LSP Extensions**: Support for additional language servers
- **Performance Optimizations**: Large file handling improvements

#### Low Priority
- **UI Polish**: Visual improvements and animations
- **Plugin Marketplace**: Plugin discovery and installation system
- **Advanced Vi Features**: Additional vi/vim compatibility features

## Troubleshooting

### Common Build Issues

#### SDL2 Not Found
```
CMake Error: Could not find SDL2
```
**Solution**: Set SDL2_ROOT environment variable to SDL2 installation directory

#### Boost Library Issues
```
CMake Error: Unable to find the requested Boost libraries
```
**Solution**: Set BOOST_ROOT and BOOST_LIBRARY_DIR environment variables

#### Plugin Loading Failures
```
Failed to load plugin: [plugin_name]
```
**Solution**: Check plugin dependencies and ensure proper compilation

### Runtime Issues

#### Window Focus Problems
**Symptom**: Input not following focused window
**Status**: Fixed in current version - all input now follows focused window correctly

#### Buffer Corruption
**Symptom**: Crashes after split or file operations  
**Status**: Resolved with robust buffer/cursor management

#### Plugin Crashes
**Symptom**: Editor crashes when using plugin features
**Solution**: Check plugin error messages, ensure plugin compatibility with version 0.0.5

### Performance Issues

#### Slow Syntax Highlighting
**Symptom**: Lag when editing large files
**Solution**: Consider disabling syntax highlighting for very large files (`:syntax off`)

#### Memory Usage
**Symptom**: High memory usage with multiple buffers
**Solution**: Close unused buffers (`:bd`), restart editor periodically for long sessions

## Future Development

### Roadmap

#### Phase 5: Advanced IDE Features (Planned)
- **Project Management**: Full project file organization
- **Debugging Integration**: GDB/LLDB integration for C/C++
- **Git Integration**: Version control operations within editor
- **Terminal Integration**: Embedded terminal for shell operations

#### Long-term Goals
- **Language Server Extensions**: Support for more programming languages
- **Collaborative Editing**: Real-time collaborative editing features
- **Plugin Marketplace**: Online plugin repository and manager
- **Mobile Support**: Touch-friendly interface for tablets

### Architecture Evolution

#### Plugin System Maturity
- **Plugin API Versioning**: Backward compatibility for plugin updates
- **Hot Reload**: Live plugin reloading during development
- **Plugin Dependencies**: Plugin-to-plugin dependency management

#### Performance Enhancements
- **Multithreading**: Background processing for syntax highlighting and LSP
- **Memory Optimization**: Reduced memory footprint for large projects
- **Startup Performance**: Faster editor initialization

---

*Vizero Development Guide - Version 0.0.5*
*Phase 4: Advanced Features & Polish Complete*
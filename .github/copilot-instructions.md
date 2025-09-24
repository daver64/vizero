# Vizero AI Coding Instructions

Vizero is a modern vi clone built with SDL2/OpenGL featuring hardware-accelerated rendering, multi-buffer support, and a dynamic plugin system. This document guides AI agents in understanding the codebase architecture and development patterns.

## Architecture Overview

### Core Components
- **Application Layer** (`src/core/`): SDL2 window management, OpenGL rendering, input handling
- **Editor State** (`src/editor/`): Vi-compatible modes, command parsing, multi-buffer management
- **Text Engine** (`src/text/`): Efficient buffer/cursor operations with undo system
- **Window Manager** (`src/ui/editor_window.cpp`): Multi-window/split support with focus routing
- **Plugin System** (`src/plugin/`): Dynamic loading with C API for syntax highlighting and extensions

### Key Architectural Patterns

**Window-Buffer Relationship**: Each `vizero_editor_window_t` owns a buffer and cursor. The window manager routes all input to the focused window, ensuring vi-like behavior across splits.

**Plugin Architecture**: Uses function pointers in `vizero_plugin_callbacks_t` with versioned API. Plugins export `vizero_plugin_init()`, `vizero_plugin_cleanup()`, and `vizero_plugin_get_info()`. See `plugins/example/example_plugin.c` for template.

**Settings System**: Persistent configuration in `%APPDATA%\Vizero\settings.ini` with helper functions in `src/core/settings.cpp`.

## Critical Development Workflows

### Building
- **Windows**: `build.bat` (requires SDL2_ROOT, BOOST_ROOT, GLEW_ROOT env vars)
- **Unix**: `build.sh` (auto-detects system packages or uses env vars)
- Both support `clean` argument and parallel builds

### Window/Buffer Management
Always use window manager helpers instead of direct struct access:
```c
// CORRECT - use helper functions
vizero_editor_window_t* window = vizero_window_manager_get_focused_window(manager);
vizero_buffer_t* buffer = vizero_editor_window_get_buffer(window);

// AVOID - direct struct access can cause corruption
```

### Plugin Development
1. Create directory in `plugins/my_plugin/`
2. Use `VIZERO_PLUGIN_DEFINE_INFO()` macro for metadata
3. Implement required exports: `vizero_plugin_init()`, `vizero_plugin_cleanup()`, `vizero_plugin_get_info()`
4. For syntax highlighting: implement `highlight_syntax` callback returning token array
5. Add to `plugins/CMakeLists.txt` using `add_vizero_plugin()` macro

## Project-Specific Conventions

### Error Handling
Functions return `int` (0=success, -1=error) or NULL pointers. Always check return values, especially for buffer/window operations.

### Memory Management
- Buffers/cursors: Use create/destroy pairs, never free() directly
- Plugin memory: Cleanup in `vizero_plugin_cleanup()`
- Window manager: Owns cursors for active windows, handles cleanup ordering

### Syntax Highlighting
New pattern (2025): Caller allocates token buffer, plugin fills it:
```c
vizero_syntax_token_t tokens[64];
size_t token_count;
vizero_plugin_manager_highlight_syntax(manager, buffer, start, end, tokens, 64, &token_count);
```

### Word Wrap Implementation
Word wrap is default-enabled with hanging indent. Logic is unified in `src/ui/editor_window.cpp` with visual-to-logical position mapping in `src/ui/editor_window_move_visual_row.c`.

## Integration Points

### CMake Configuration
- Cross-platform with Windows/Unix detection
- Complex Boost library finding with manual MSVC library naming
- Plugin system uses shared libraries (`.dll`/`.so`)
- DLL copying for Windows runtime

### OpenGL Rendering
Hardware-accelerated text rendering at 60fps with immediate mode. Color values in plugins use 0-255 range, converted to 0.0-1.0 for OpenGL.

### Vi Command System
Commands parsed in `src/editor/command_parser.cpp` with mode-specific handling in `src/editor/mode_manager.cpp`. New commands should follow existing `:command args` pattern.

## Essential Files for Understanding

- `include/vizero/plugin_interface.h` - Complete plugin API
- `src/ui/editor_window.cpp` - Window management and rendering
- `src/editor/editor_state.cpp` - Core editor logic
- `plugins/syntax_highlight/syntax_highlight.c` - Plugin implementation example
- `CMakeLists.txt` - Build system with dependency handling

## Common Gotchas

- Plugin tokens must use line-relative column positions, not absolute
- Window focus changes must update input routing - test with `:wincmd` and `Ctrl+w`
- Boost library names are version/compiler specific on Windows
- OpenGL context required for all rendering operations
- Settings changes need `vizero_settings_save_to_file()` for persistence
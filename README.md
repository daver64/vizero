# Vizero

A fast, lightweight desktop text editor inspired by vi, built with modern graphics technology. Vizero aims to replicate the essential vi editing experience while providing a clean, responsive interface without being a strict vi clone.

## Project Goals

Vizero is designed to be:
- **Fast and Lightweight**: Minimal resource usage with hardware-accelerated rendering
- **Vi-Inspired**: Core vi editing paradigms without strict compatibility constraints  
- **Modern**: Built with SDL2/OpenGL for smooth, responsive graphics
- **Extensible**: Plugin system for customization and language support
- **Cross-Platform**: Native performance on Windows, Linux, and FreeBSD

## Current Features

### ✅ Completed
- **Core Text Editing**: Multi-line text buffers with efficient line management
- **Vi-Style Navigation**: hjkl movement, word/line jumping, screen positioning
- **Modal Editing**: Normal, Insert, and Visual modes with proper state management
- **File Operations**: Open, save (:w), and quit (:q) commands
- **Advanced Scrolling**: Smooth scrolling with cursor tracking and viewport management
- **Plugin Architecture**: Dynamic plugin loading system with C API
- **Syntax Highlighting**: Word-level tokenization for Assembly and C languages
- **Hardware Rendering**: SDL2/OpenGL-based text rendering with color support

### 🔄 In Progress
- Enhanced syntax highlighting for more languages
- Visual mode text selection and operations
- Search and replace functionality
- Multiple buffer management

### 📋 Planned
- Undo/redo system
- Configuration file support
- Additional vi commands (delete, yank, paste)
- Command-line integration
- More language plugins
- Themes and customization

## Architecture

Vizero uses a modular C/C++ architecture with clear separation of concerns:

- **Core Application**: SDL2 window management and main loop
- **Text Engine**: Efficient buffer and line management
- **Renderer**: OpenGL-accelerated text and graphics rendering  
- **Editor Logic**: Vi-style mode management and command processing
- **Plugin System**: Dynamic loading with hot-pluggable syntax highlighting

## Quick Start

### Build Requirements
- SDL2 development libraries
- OpenGL/GLEW
- CMake 3.10+
- C/C++ compiler (MSVC, GCC, or Clang)

### Building
```bash
mkdir build && cd build
cmake ..
cmake --build . --config Release
```

### Usage
```bash
./vizero filename.asm    # Opens file with assembly syntax highlighting
./vizero filename.c      # Opens file with C syntax highlighting
```

### Basic Vi Commands
- `hjkl` - Navigate left/down/up/right
- `i` - Enter insert mode
- `ESC` - Return to normal mode  
- `:w` - Save file
- `:q` - Quit editor
- `:wq` - Save and quit

## Plugin Development

Vizero supports C-based plugins for syntax highlighting and editor extensions:

```c
#include "vizero/plugin_interface.h"

VIZERO_PLUGIN_DEFINE_INFO(
    "My Language Plugin",
    "1.0.0", 
    "Author",
    "Syntax highlighting for MyLang",
    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER
);

// Implement syntax highlighting callbacks
static int highlight_syntax(vizero_buffer_t* buffer, /* ... */) {
    // Generate syntax tokens
}
```

## Contributing

Vizero welcomes contributions! Key areas for development:
- Additional language syntax highlighting plugins
- Vi command implementations  
- Performance optimizations
- Cross-platform compatibility
- Documentation and examples

## License

MIT License - see LICENSE file for details.

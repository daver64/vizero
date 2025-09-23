# Vizero

A modern vi clone built with SDL2 and OpenGL, featuring hardware-accelerated rendering, comprehensive search and replace capabilities, multi-buffer support, and integrated compiler tools.

## Features

### ✅ Complete Vi Editing Experience
- **Modal Editing**: Normal, Insert, Visual, and Command modes
- **Vi Navigation**: hjkl movement, word jumping, page navigation
- **Search & Replace**: Full regex support with visual highlighting
- **Multi-Buffer Support**: Work with up to 128 files simultaneously
- **Advanced Cursor Operations**: Word boundaries, line start/end navigation
- **Complete Undo System**: 1000-operation undo history per buffer

### ✅ Modern Interface
- **Hardware Acceleration**: SDL2/OpenGL rendering at 60fps
- **Visual Search Highlighting**: Current match in orange, others in yellow
- **Responsive UI**: Smooth scrolling and real-time feedback
- **Fullscreen Support**: F11 toggle with seamless scaling
- **Smart Popup System**: Auto-dismissing status and error messages

### ✅ File & Buffer Management
- **Multi-Buffer Navigation**: `:bn`, `:bp`, `:b1`, `:b2`, etc.
- **Cross-Platform File Handling**: Automatic line ending normalization
- **File Reading**: `:r filename` inserts files at cursor position
- **Smart Buffer Switching**: Detects already-open files

### ✅ Developer Tools
- **Compiler Integration**: Built-in C/C++/Assembly compilation
- **Plugin System**: Dynamic syntax highlighting and extensions
- **Settings Persistence**: Configuration saved to `%APPDATA%\Vizero\`
- **Command Execution**: Direct compiler invocation from editor

### ✅ Standard Features
- **Clipboard Integration**: Full Ctrl+C/X/V system clipboard support
- **Smart Indentation**: Context-aware tab handling (4 spaces)
- **Line Numbers**: Toggle with `:linenum on/off`
- **Selection Support**: Visual mode with Shift+Arrow keys

## Quick Start

### Installation
```bash
# Build from source
mkdir build && cd build
cmake ..
cmake --build . --config Release
```

### Basic Usage
```bash
# Open a single file
./vizero filename.c

# Open multiple files
./vizero file1.c file2.h data.txt
```

### Essential Commands

#### File Operations
```
:e filename    # Open file in new buffer
:w             # Save current buffer  
:wa            # Save all buffers
:q             # Quit (warns if unsaved)
:wq            # Save and quit
:r filename    # Read file at cursor
```

#### Buffer Management  
```
:ls            # List all buffers
:bn            # Next buffer
:bp            # Previous buffer  
:b1, :b2, :b3  # Jump to buffer number
```

#### Search & Replace
```
/pattern       # Search forward (with highlighting)
?pattern       # Search backward
n              # Next match
N              # Previous match
:s/old/new/    # Replace on current line
:%s/old/new/g  # Replace all in file
```

#### Compilation
```
:cc main.c -o program.exe    # Compile C
:cpp main.cpp -o program.exe # Compile C++
:asm code.asm -o code.o      # Assemble
```

## Advanced Features

### Buffer Workflow Example
```bash
# Start with main file
vizero main.c

# Open related files
:e header.h
:e utils.c
:e data.txt

# Navigate between buffers
:ls                    # Shows: 1:main.c 2:header.h 3:utils.c 4:data.txt*
:b1                    # Jump to main.c
:bn                    # Next buffer (header.h)
:bp                    # Previous buffer (main.c)
```

### Search & Replace Examples
```bash
# Find all functions
/function.*\(

# Replace printf with cout globally
:%s/printf/cout/g

# Case-sensitive word search
/\bTODO\b
```

### Plugin Development
```c
#include "vizero/plugin_interface.h"

VIZERO_PLUGIN_DEFINE_INFO(
    "Custom Highlighter",
    "1.0.0",
    "Your Name", 
    "Syntax highlighting for custom language",
    VIZERO_PLUGIN_TYPE_SYNTAX_HIGHLIGHTER
);

static int highlight_syntax(/* ... */) {
    // Implement syntax highlighting
    return 0;
}
```

## Architecture

- **Core**: SDL2 window management and OpenGL rendering
- **Text Engine**: Efficient multi-buffer line management  
- **Editor**: Vi-compatible mode system and command processing
- **Search**: C++ regex engine with visual feedback
- **Plugins**: Dynamic loading system with C API
- **Settings**: INI-based persistent configuration

## Requirements

### Build Dependencies
- SDL2 development libraries
- OpenGL/GLEW  
- CMake 3.10+
- C/C++ compiler (MSVC, GCC, or Clang)
- Boost libraries (system, filesystem)

### Runtime
- Modern GPU with OpenGL 3.3+ support
- 50MB RAM minimum
- Any modern Windows/Linux system

## Keyboard Reference

### Normal Mode
| Key | Action |
|-----|--------|
| `hjkl` | Vi-style movement |
| `Arrow Keys` | Standard movement |  
| `w/b` | Word forward/backward |
| `0/$` | Line start/end |
| `Page Up/Down` | Page navigation |
| `i` | Enter Insert Mode |
| `/` | Search forward |
| `n/N` | Next/previous search result |
| `:` | Enter Command Mode |

### Command Mode
| Command | Action |
|---------|--------|
| `:e file` | Open file |
| `:w` | Save |
| `:q` | Quit |
| `:bn/:bp` | Next/previous buffer |
| `:b[N]` | Switch to buffer N |
| `:ls` | List buffers |
| `:cc` | Compile C |
| `:set` | Configure settings |

## Contributing

Areas for contribution:
- Additional language plugins
- Vi command implementations  
- Performance optimizations
- Cross-platform testing
- Documentation improvements

## License

MIT License - see LICENSE file for details.

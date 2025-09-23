# Vizero Editor Manual

**Vizero** is a modern vi clone built with SDL2 and OpenGL, featuring hardware-accelerated rendering, comprehensive search and replace capabilities, robust multi-buffer and multi-window support, and integrated compiler support.

---

## Recent Improvements (2025)

- **Robust buffer/cursor management**: Prevents double-free/use-after-free bugs when splitting and loading files. Windows and buffer arrays are always in sync, and no window or array references freed memory.
- **Window focus and input routing**: All input and editing operations now follow the currently focused window, matching vi-like behavior. After using `:wincmd`, `Ctrl+w`, or any window focus command, keypresses and text input go to the correct window.
- **Crash/corruption fixes**: Resolved crashes and data corruption after split and file load operations.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Editor Modes](#editor-modes)
3. [Basic Navigation](#basic-navigation)
4. [Text Editing](#text-editing)
5. [Search and Replace](#search-and-replace)
6. [Selection and Clipboard](#selection-and-clipboard)
7. [Undo System](#undo-system)
8. [Buffer Management](#buffer-management)
9. [File Operations](#file-operations)
10. [Compiler Integration](#compiler-integration)
11. [Settings and Configuration](#settings-and-configuration)
12. [Advanced Features](#advanced-features)
13. [Keyboard Reference](#keyboard-reference)

---

## Getting Started

### Launching Vizero
```bash
./vizero [filename]
```

### Basic Workflow
1. Start in **Normal Mode** (command mode)
2. Press `i` to enter **Insert Mode** for typing
3. Press `Esc` to return to **Normal Mode**
4. Press `:` to enter **Command Mode** for file operations
5. Use `:w` to save, `:q` to quit

---

## Editor Modes

### Normal Mode
- **Purpose**: Navigation and text manipulation
- **Indicator**: Cursor is a block
- **Entry**: Press `Esc` from any other mode

### Insert Mode  
- **Purpose**: Text input and editing
- **Indicator**: Cursor is a line
- **Entry**: Press `i` from Normal Mode

### Command Mode
- **Purpose**: File operations and advanced commands
- **Indicator**: Status bar shows `:` followed by command
- **Entry**: Press `:` from Normal Mode
- **Exit**: Press `Esc` to cancel or `Enter` to execute

---

## Basic Navigation

### Cursor Movement
| Key | Action |
|-----|--------|
| `h` / `Left` | Move left |
| `j` / `Down` | Move down |
| `k` / `Up` | Move up |
| `l` / `Right` | Move right |

### Page Navigation
| Key | Action |
|-----|--------|
| `Ctrl+F` / `Page Down` | Move page down |
| `Ctrl+B` / `Page Up` | Move page up |

### Smart Tab Navigation
- **Tab** in Normal Mode: Jump to first non-whitespace character on line
- **Tab** in Insert Mode: Insert 4 spaces or jump to first non-whitespace

---

## Text Editing

### Entering Insert Mode
| Key | Action |
|-----|--------|
| `i` | Insert at cursor position |

### Insert Mode Operations
| Key | Action |
|-----|--------|
| `Enter` | Create new line |
| `Backspace` | Delete character before cursor |
| `Delete` | Delete character at cursor |
| `Tab` | Insert 4 spaces or smart indent |
| `Esc` | Return to Normal Mode |

---

## Search and Replace

### Search Commands

#### Forward Search
```
/pattern
```
- Search forward for pattern using regex
- **Examples**:
  - `/hello` - Find "hello"
  - `/\d+` - Find any sequence of digits
  - `/function.*\(` - Find "function" followed by opening parenthesis

#### Backward Search
```
?pattern
```
- Search backward for pattern using regex
- **Examples**:
  - `?return` - Find "return" searching backward
  - `?\w+\.h` - Find word ending with ".h"

#### Search Navigation
| Key | Action |
|-----|--------|
| `n` | Jump to next match (same direction) |
| `N` | Jump to previous match (opposite direction) |
| `/` | Start forward search from Normal Mode |

#### Visual Highlighting
- **Current Match**: Highlighted in orange background
- **Other Matches**: Highlighted in yellow background
- **Real-time Updates**: Highlights update as you navigate
- **Automatic Clearing**: Highlights cleared when search ends

### Replace Commands

#### Single Line Substitute
```
s/pattern/replacement/[flags]
```
- Replace on current line only
- **Flags**:
  - `g` - Replace all occurrences on line
- **Examples**:
  - `s/old/new/` - Replace first "old" with "new"
  - `s/printf/cout/g` - Replace all "printf" with "cout" on line

#### Global Substitute (All Lines)
```
%s/pattern/replacement/[flags]
```
- Replace in entire buffer
- **Flags**:
  - `g` - Replace all occurrences on every line
- **Examples**:
  - `%s/old/new/` - Replace first occurrence on each line
  - `%s/printf/cout/g` - Replace all "printf" in entire file

### Regex Patterns
Vizero supports full C++ std::regex (ECMAScript syntax):

| Pattern | Matches |
|---------|---------|
| `\w+` | Any word |
| `\d{1,3}` | 1-3 digits |
| `[A-Z]\w*` | Capitalized words |
| `\b\w+\b` | Whole words only |
| `.*\.cpp$` | Lines ending with .cpp |
| `^#include` | Lines starting with #include |

---

## Selection and Clipboard

### Text Selection
| Key | Action |
|-----|--------|
| `Shift + Arrow Keys` | Extend selection |
| `Ctrl+A` | Select all text |

### Clipboard Operations
| Key | Action |
|-----|--------|
| `Ctrl+C` | Copy selection |
| `Ctrl+X` | Cut selection |
| `Ctrl+V` | Paste at cursor |

**Note**: Clipboard operations work with Windows system clipboard.

---

## Undo System

| Key | Action |
|-----|--------|
| `Ctrl+Z` | Undo last operation |

- **Capacity**: 1000 operations
- **Granularity**: Character-level for typing, operation-level for commands
- **Coverage**: All text modifications, insertions, deletions

---

## Buffer Management

Vizero supports multiple file buffers and windows, allowing you to work with several files and window splits simultaneously. Each buffer maintains its own cursor position, undo history, and modification state. **All buffer and cursor operations always follow the currently focused window**—after any window focus change (e.g., `:wincmd`, `Ctrl+w`, or window navigation), all input and editing go to the correct window and buffer, just like in vi/vim.

### Opening Files in New Buffers
| Command | Action |
|---------|--------|
| `:e filename` | Open file in new buffer (or switch if already open) |
| `:edit filename` | Same as `:e` |

### Buffer Navigation
| Command | Action |
|---------|--------|
| `:bn` | Switch to next buffer |
| `:bnext` | Same as `:bn` |
| `:bp` | Switch to previous buffer |
| `:bprev` | Same as `:bp` |
| `:b1`, `:b2`, `:b3` | Switch directly to buffer number N |
| `:ls` | List all open buffers with numbers |
| `:buffers` | Same as `:ls` |

### Buffer Features
- **Capacity**: Up to 128 simultaneous buffers
- **Independent State**: Each buffer has its own cursor and undo history
- **Smart Switching**: Automatically detects if file is already open
- **Status Display**: Current buffer marked with `*` in buffer list

### Example Workflow
```
vizero main.c          # Start with main.c as buffer 1
:e header.h            # Open header.h as buffer 2  
:e data.txt            # Open data.txt as buffer 3
:ls                    # Show: "1:main.c 2:header.h 3:data.txt*"
:b1                    # Jump to main.c
:bn                    # Go to next buffer (header.h)
:bp                    # Go back to previous buffer (main.c)
```

---

## File Operations

### Basic File Commands
| Command | Action |
|---------|--------|
| `:w` | Save current buffer |
| `:wa` | Save all modified buffers |
| `:w filename` | Save current buffer as filename |
| `:q` | Quit (fails if unsaved changes) |
| `:q!` | Force quit (discard changes) |
| `:wq` | Save current buffer and quit |
| `:r filename` | Read file into buffer at cursor position |

### File Information
| Command | Action |
|---------|--------|
| `:file` | Show current filename and status |

---

## Compiler Integration

### Compilation Commands
| Command | Action |
|---------|--------|
| `:cc [args]` | Compile with GCC |
| `:cpp [args]` | Compile with G++ |
| `:asm [args]` | Assemble with NASM/FASM |

### Compiler Features
- **Automatic Detection**: Detects available compilers (GCC, MSVC, NASM, FASM)
- **Real-time Feedback**: Popup windows show compilation results
- **Error Capture**: Compiler output displayed in popup with timeout
- **Flexible Arguments**: Pass any compiler flags and options

### Examples
```
:cc -o hello hello.c
:cpp -std=c++17 -O2 main.cpp
:asm -f win64 program.asm
```

---

## Settings and Configuration

### Settings Commands
| Command | Action |
|---------|--------|
| `:set name=value` | Set configuration option |
| `:show` | Display all current settings |
| `:show name` | Display specific setting |

### Available Settings
| Setting | Default | Description |
|---------|---------|-------------|
| `tab_size` | 4 | Number of spaces per tab |
| `auto_indent` | true | Automatic indentation |
| `show_line_numbers` | false | Display line numbers |
| `wrap_text` | false | Text wrapping |

### Persistence
- Settings automatically saved to `%APPDATA%\Vizero\settings.ini`
- Loaded automatically on startup
- Cross-session persistence

---

## Advanced Features

### Fullscreen Mode
| Key | Action |
|-----|--------|
| `F11` | Toggle fullscreen mode |

- **Responsive UI**: Interface scales with screen size
- **Seamless Toggle**: Instant switch between windowed and fullscreen

### Popup System
- **Compiler Output**: Displays build results
- **Error Messages**: Shows command errors and status
- **Auto-dismiss**: Popups timeout after 5 seconds
- **Manual Dismiss**: Press `Esc` to close immediately

### Hardware Acceleration
- **OpenGL Rendering**: Smooth, fast text rendering
- **SDL2 Backend**: Cross-platform window management
- **60 FPS**: Responsive interface updates

### Cross-Platform File Handling
- **Line Ending Normalization**: Automatically converts `\r\n` to `\n`
- **Binary Safe**: Handles files with mixed line endings
- **Unicode Support**: Proper text encoding handling

### Plugin System
- **Dynamic Loading**: Load plugins at runtime
- **Syntax Highlighting**: Extensible language support
- **API Integration**: Full editor API access for plugins

### Search Match Highlighting
- **Real-time Visual Feedback**: All search matches highlighted
- **Color-coded Results**: Current match vs. other matches
- **Performance Optimized**: Efficient highlighting for large files

---

## Keyboard Reference

### Normal Mode
| Key | Action |
|-----|--------|
| `h,j,k,l` | Move cursor |
| `Arrow Keys` | Move cursor |
| `Shift + Arrow` | Extend selection |
| `Page Up/Down` | Page navigation |
| `Ctrl+F/B` | Page down/up (vi style) |
| `Tab` | Smart indent navigation |
| `i` | Enter Insert Mode |
| `:` | Enter Command Mode |
| `/` | Forward search |
| `n` | Next search result |
| `N` | Previous search result |
| `Ctrl+C` | Copy selection |
| `Ctrl+X` | Cut selection |
| `Ctrl+V` | Paste |
| `Ctrl+Z` | Undo |
| `Ctrl+A` | Select all |
| `F11` | Toggle fullscreen |
| `Esc` | Dismiss popup |

### Insert Mode
| Key | Action |
|-----|--------|
| `Esc` | Return to Normal Mode |
| `Enter` | New line |
| `Backspace` | Delete before cursor |
| `Delete` | Delete at cursor |
| `Tab` | Smart indent or 4 spaces |
| `Arrow Keys` | Navigate |
| `Ctrl+C/X/V` | Clipboard operations |
| `Ctrl+Z` | Undo |

### Command Mode
| Key | Action |
|-----|--------|
| `Enter` | Execute command |
| `Esc` | Cancel command |
| `Backspace` | Edit command |

---

## Tips and Best Practices

### Efficient Workflow
1. **Use keyboard navigation**: Arrow keys and vi keys work in all modes
2. **Master search**: Use regex patterns for powerful text finding
3. **Leverage clipboard**: Modern Ctrl+C/X/V works everywhere
4. **Smart tabbing**: Use Tab for both indentation and navigation
5. **Fullscreen coding**: F11 for distraction-free editing

### Search Power
- **Combine patterns**: Use `%s/\s+$//g` to clean trailing whitespace
- **Word boundaries**: Use `\b` for exact word matching
- **Capture groups**: Use `(\w+)` in patterns and `\1` in replacements

### Compiler Integration
- **Quick builds**: Use `:cc` without leaving the editor
- **Check errors**: Popup shows compilation status immediately
- **Flexible builds**: Pass any compiler flags you need

---

## Troubleshooting

### Common Issues
- **File won't save**: Check file permissions
- **Search not working**: Verify regex syntax
- **Compilation fails**: Check compiler installation and PATH
- **Settings not saved**: Verify %APPDATA% directory access

- **Input not following window focus?** This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window.
- **Crashes after split or file load?** These have been resolved with robust buffer/cursor management.

### Getting Help
- Check this manual for command reference
- Experiment with commands in a test file
- Use `:show` to see current settings
- Try `:file` to check current file status

---

*Vizero - A powerful, modern vi clone for developers*
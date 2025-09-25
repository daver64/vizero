# Vizero Editor Manual


**Vizero** is a modern vi clone built with SDL2 and OpenGL, featuring hardware-accelerated rendering, comprehensive search and replace, robust multi-buffer and multi-window support, integrated compiler tools, Markdown syntax highlighting, and advanced word wrap.

---



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
10. [Window Management](#window-management)
11. [Language Server Protocol (LSP)](#language-server-protocol-lsp)
12. [IRC Client Integration](#irc-client-integration)
13. [Compiler Integration](#compiler-integration)
14. [Settings and Configuration](#settings-and-configuration)
15. [Advanced Features](#advanced-features)
16. [Working Directory](#working-directory)
17. [Keyboard Reference](#keyboard-reference)
## Working Directory

### Changing the Working Directory

Use the `:chdir <path>` command to change the editor's current working directory at runtime. This affects file open/save dialogs and relative path resolution for all subsequent file operations.

**Usage:**
```
:chdir c:\\users\\daver\\source\\foo\\
```
or
```
:chdir /home/username/projects/
```

On success, a status message will confirm the new directory. On failure, an error message will be shown.

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
- **Indicator**: Cursor is an underline
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
| `o` | Open new line below and enter insert mode |
| `O` | Open new line above and enter insert mode |

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

### Advanced Line Range Operations

#### Line Range Syntax
Line ranges use vi-style addressing:
- `.` - Current line
- `$` - Last line  
- `1,5` - Lines 1 through 5
- `.,+3` - Current line plus 3 more
- `1,$` - All lines (equivalent to `%`)

#### Line Range Commands

**Delete Lines**
```
:1,5d          # Delete lines 1-5
:.,+3d         # Delete current line + 3 more  
:.,$d          # Delete from current line to end
```

**Yank (Copy) Lines**
```
:1,5y          # Copy lines 1-5 to clipboard
:.,+2y         # Copy current line + 2 more
```

**Substitute in Range**
```
:1,5s/old/new/g      # Replace in lines 1-5
:.,+10s/printf/cout/g # Replace in current + 10 lines
```

### Global Commands

Global commands operate on all lines matching (or not matching) a pattern.

#### Global Delete/Print
```
:g/pattern/d         # Delete all lines containing pattern
:g/pattern/p         # Print all lines containing pattern
:v/pattern/d         # Delete all lines NOT containing pattern
```

**Examples:**
```
:g/TODO/d            # Delete all lines with "TODO"
:g/printf/p          # Show all lines with "printf"
:v/^$/d              # Delete all non-empty lines
:g/function.*{/p     # Show all function definitions
```

#### Global Substitute
```
:%g/pattern/s//replacement/g   # Global substitute with pattern
```

**Example:**
```
:%g/printf/s//cout/g     # Replace printf with cout on matching lines
```

### Marks and Navigation Information

#### View Navigation History
```
:marks           # Show marks information popup
:jumps           # Show jump history information
:changes         # Show change history information
```

These commands show informational popups about navigation features that are planned for future implementation, including mark setting/jumping and history navigation.

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

Vizero supports multiple file buffers and windows, allowing you to work with several files and window splits simultaneously. Each buffer maintains its own cursor position, undo history, and modification state. **All buffer and cursor operations always follow the currently focused window**—after any window focus change (e.g., `Ctrl+w` or window navigation), all input and editing go to the correct window and buffer, just like in vi/vim.

### Opening Files in New Buffers
| Command | Action |
|---------|--------|
| `:e filename` | Open file in new buffer (or switch if already open) |
| `:edit filename` | Same as `:e` |
| `:new` | Create new empty buffer |
| `:enew` | Create new unnamed buffer |

### Buffer Navigation
| Command | Action |
|---------|--------|
| `:bn`, `:bnext` | Switch to next buffer |
| `:bp`, `:bprev` | Switch to previous buffer |
| `:bN` | Switch directly to buffer number N (e.g., `:b1`, `:b2`) |
| `:buffers` | Interactive buffer selector (use arrows + Enter) |
| `:bd`, `:bdelete` | Delete/close current buffer |
| `:bd N`, `:bdelete N` | Delete buffer number N |
| `:n`, `:next` | Edit next file (same as `:bn`) |
| `:prev`, `:previous` | Edit previous file (same as `:bp`) |

### Interactive Buffer Selector
The `:buffers` command opens an interactive popup window showing all open buffers:
- Use **↑/↓ arrow keys** to navigate through the buffer list  
- Press **Enter** to switch to the selected buffer
- Press **ESC** to cancel and close the selector
- Current buffer is marked with `[CURRENT]`, selected buffer with `>`
### Buffer Deletion
| Command | Action |
|---------|--------|
| `:bd` | Delete/close current buffer |
| `:bd N` | Delete buffer number N |
## Window Management

Vizero supports horizontal and vertical window splits, as well as closing splits and creating new empty buffers in windows. All window commands operate on the currently focused window.

### Window Commands
| Command/Key | Action |
|-------------|--------|
| `:split`, `:sp` | Split window horizontally |
| `:vsplit`, `:vsp` | Split window vertically |
| `:close`, `:clo` | Close current window (only in split mode) |
| `:only` | Close all windows except current |
| `:new` | Create new empty buffer in current window |
| `:help`, `:h` | Load manual in current buffer |
| `Ctrl+W` then `h/j/k/l` or arrow key | Switch focus to left/down/up/right window |
| `Ctrl+W` then `1`-`9` | Switch focus to window number |

**Notes:**
- After splitting, each window has its own independent buffer and cursor.
- All editing, navigation, and commands apply to the focused window/buffer.

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
| `:r filename` | Read file into buffer at cursor position (only affects current window/buffer) |
### Other Useful Commands
| Command | Action |
|---------|--------|
| `:file` | Show current filename and status |
| `:file name` | Set filename for current buffer |
| `:syntax on/off` | Enable or disable syntax highlighting |
| `:tabs N` | Set tab size to N |
| `:set key value` | Set configuration option |
| `:show` | Show all settings |
| `:show key` | Show value of specific setting |

### File Information
| Command | Action |
|---------|--------|
| `:file` | Show current filename and status |
| `:pwd` | Print working directory |

### Directory Operations
| Command | Action |
|---------|--------|
| `:ls` | List files in current directory |
| `:chdir <path>` | Change working directory |

---

## Language Server Protocol (LSP)

Vizero includes comprehensive Language Server Protocol support, providing intelligent code completion, diagnostics, and other language-aware features for C/C++ development through integration with clangd.

### LSP Features

#### Code Completion
| Key | Action |
|-----|--------|
| `Ctrl+Space` | Trigger intelligent code completion |

- **Context-aware Suggestions**: Shows functions, variables, types, and members based on current context
- **Real-time Results**: Completion suggestions appear instantly from language server
- **Large Response Handling**: Supports completion lists with thousands of items (up to 42KB+ responses)
- **Rich Information**: Displays function signatures, documentation, and parameter details

#### Automatic Features
- **Real-time Diagnostics**: Error and warning underlining as you type
- **Hover Information**: Type information and documentation on cursor hover
- **Smart Navigation**: Go-to-definition and symbol lookup

### LSP Requirements

#### For C/C++ Development
- **clangd Language Server**: Download from [LLVM releases](https://github.com/llvm/llvm-project/releases)
- **Installation**: Place `clangd.exe` in `vizero/clangd/bin/` directory or system PATH
- **Project Setup**: Works best with `compile_commands.json` or CMake projects

#### Graceful Degradation
When clangd is not available:
- ✅ **Editor continues to work normally** - All other features remain functional
- ✅ **Clear feedback** - Informative messages explain LSP unavailability  
- ✅ **No crashes** - Robust error handling prevents any instability
- ✅ **Syntax highlighting** - Basic syntax coloring still works via built-in plugins

### LSP Status Messages

#### When clangd is Available
```
[CLANGD] Found clangd at: C:\path\to\clangd.exe
[CLANGD] clangd process started successfully
[CLANGD] clangd initialization complete
```

#### When clangd is Not Available
```
[CLANGD] clangd executable not found, disabling LSP functionality
[CLANGD] Plugin will load but LSP features will be unavailable
```

### Code Completion Usage

1. **Start typing** in a C/C++ file
2. **Press Ctrl+Space** to trigger completion
3. **Navigate suggestions** with arrow keys
4. **Press Enter** to accept a completion
5. **Press Esc** to dismiss completion popup

#### Example Workflow
```c
#include <stdio.h>

int main() {
    pri    // Press Ctrl+Space here
    // Shows: printf, print, etc.
    
    FILE* f = fo    // Press Ctrl+Space here  
    // Shows: fopen, fork, etc.
    
    return 0;
}
```

### LSP Integration Architecture

- **Plugin-based**: LSP functionality provided by clangd plugin
- **Non-blocking**: Completion requests don't freeze the editor
- **Memory Safe**: Robust JSON parsing prevents crashes on malformed responses
- **Configurable**: Future support for additional language servers

### Troubleshooting LSP

#### No Code Completion
1. **Check clangd installation**: Ensure `clangd.exe` is in `vizero/clangd/bin/` or PATH
2. **Verify file type**: LSP only works with C/C++ files (`.c`, `.cpp`, `.h`, `.hpp`)
3. **Check status messages**: Look for clangd initialization messages at startup

#### LSP Startup Issues
- **Permission problems**: Ensure clangd executable has proper permissions
- **Path issues**: Use absolute paths if relative paths don't work
- **Version compatibility**: Use recent clangd versions (LLVM 12+)

---

## IRC Client Integration

Vizero includes a comprehensive IRC (Internet Relay Chat) client plugin that seamlessly integrates with the editor's buffer system. You can chat on IRC channels while editing code, using familiar vi commands to switch between your IRC sessions and source files.

### Getting Started with IRC

#### Connecting to IRC Servers
```
:connect [server] [port] [nickname]
```

**Examples:**
```
:connect                                    # Connect to default server (irc.libera.chat:6667) with username "vi"
:connect irc.libera.chat                   # Connect to Libera Chat with default port and nickname
:connect irc.libera.chat 6667 mynick      # Connect with custom nickname
:connect irc.freenode.net 6667 coder      # Connect to different server
```

**Default Settings:**
- **Server**: `irc.libera.chat`
- **Port**: `6667`
- **Nickname**: `vi`

#### Basic IRC Commands
Once connected, you can use standard IRC commands by typing them directly (without the `/` prefix):

| Command | Description |
|---------|-------------|
| `join #channel` | Join an IRC channel |
| `part` | Leave current channel |
| `part #channel` | Leave specific channel |
| `quit` | Disconnect from IRC server |
| `nick newnick` | Change your nickname |
| `msg nick message` | Send private message to user |
| `who #channel` | List users in channel |
| `topic new topic` | Set channel topic (if you have permission) |

**Examples:**
```
join #vizero                    # Join the #vizero channel
join #programming              # Join programming discussion channel
part                          # Leave current channel
nick MyNewNick               # Change nickname to MyNewNick
msg alice Hey there!         # Send private message to alice
```

### IRC Buffer Integration

#### Buffer Management
The IRC plugin creates dedicated IRC buffers that integrate seamlessly with Vizero's multi-buffer system:

- **IRC Buffer Creation**: When you connect to IRC, a special IRC buffer is created
- **Buffer Switching**: Use standard vi buffer commands (`:bn`, `:bp`) to switch between IRC and file buffers
- **Visual Integration**: IRC interface only displays when you're in an IRC buffer

#### Switching Between IRC and Files

| Command | Action |
|---------|--------|
| `:bn`, `:bnext` | Switch to next buffer (from IRC to file, or file to IRC) |
| `:bp`, `:bprev` | Switch to previous buffer |
| `:buffers` | Interactive buffer selector (shows both IRC and file buffers) |

**Example Workflow:**
```bash
# Start Vizero with a source file
vizero main.c

# Connect to IRC (creates IRC buffer)
:connect

# Join a programming channel
join #programming

# Switch to your source file buffer
:bn

# Now you're back in main.c - edit your code
# ...make changes to your source file...

# Switch back to IRC to chat
:bp

# You're back in IRC - continue chatting
Hello everyone, working on some C code!

# Switch back to source file
:bn
```

### IRC Interface Layout

When viewing an IRC buffer, Vizero displays a full IRC client interface:

```
┌─────────────────┬──────────────────────────────────────┬─────────────────┐
│   Channel List  │           Message Area               │   Nick List     │
│   #vizero      │ <alice> Hey everyone!                │   @admin        │
│   #programming │ <bob> Anyone know C++?               │   alice         │
│   #general     │ <charlie> Check out this code...     │   bob           │
│                │ <you> Working on a vi clone          │   charlie       │
│                │                                       │   you           │
├─────────────────┼──────────────────────────────────────┼─────────────────┤
│                 │ Type your message here...            │                 │
└─────────────────┴──────────────────────────────────────┴─────────────────┘
```

#### Interface Components

**Channel List (Left Panel)**
- Shows all joined channels
- Active channel highlighted
- Click or use commands to switch channels

**Message Area (Center)**
- Displays chat messages with timestamps
- Shows joins/parts and system messages
- Scrolls automatically with new messages
- colour-coded nicknames for easy identification

**Nick List (Right Panel)**
- Shows users in current channel
- Operators marked with `@`
- Updates automatically when users join/leave

**Input Box (Bottom)**
- Type messages directly
- Supports IRC commands (without `/` prefix)
- Press Enter to send messages or execute commands

### IRC Features

#### Multi-Channel Support
- **Join Multiple Channels**: Use `join #channel` for each channel
- **Channel Switching**: Automatic switching in interface
- **Independent Histories**: Each channel maintains its own message history
- **Channel Status**: See user counts and topics

#### Real-Time Communication
- **Live Messages**: Instant display of incoming messages
- **Connection Status**: Clear indicators of connection state
- **Server Messages**: MOTD (Message of the Day) and server notifications
- **User Activity**: Join/part notifications and nick changes

#### Vi Command Integration
The IRC plugin seamlessly integrates with vi commands:

- **Command Interception**: When in IRC buffer, vi commands like `:bn` are intercepted and executed
- **Buffer Awareness**: Plugin knows when you're in IRC vs file buffers
- **Automatic Switching**: Display automatically switches between IRC interface and normal editor
- **Preserved State**: IRC connection and channels maintained while editing files

### Advanced IRC Usage

#### Private Messages
```
msg nickname Hello there!           # Send private message
```

#### Channel Management
```
join #channel                       # Join channel
part                               # Leave current channel
part #channel                      # Leave specific channel
topic This is the new topic        # Set channel topic (if op)
```

#### Connection Management
```
quit                               # Disconnect and close IRC
nick MyNewNick                     # Change nickname
```

#### Server Interaction
```
who #channel                       # List channel users
whois nickname                     # Get info about user
```

### IRC Configuration

#### Connection Settings
IRC settings are handled through command parameters:

```
:connect server.example.com 6667 mynickname    # Custom server, port, nick
:connect server.example.com 6697 mynickname    # SSL port (6697)
:connect                                        # Use defaults
```

#### Color and Display
The IRC interface uses Vizero's colour themes and adapts to your editor settings:
- **Theme Integration**: Matches your current colour scheme
- **Readable Layout**: Optimized for programming contexts
- **Status Integration**: Connection status shown in editor status bar

### Practical Use Cases

#### Development Workflow
1. **Code Review Sessions**: Chat with team while reviewing code
2. **Debugging Help**: Get real-time help while editing problematic code
3. **Project Coordination**: Discuss features while implementing them
4. **Learning**: Ask questions in programming channels while coding

#### Example Development Session
```bash
# Start editing your project
vizero src/main.c src/utils.h

# Connect to IRC for help
:connect

# Join relevant channels
join #c-programming
join #projectname

# Ask for help while coding
Hey, I'm having trouble with malloc() in this function...

# Switch back to code to make changes
:bn

# Edit based on suggestions...

# Switch back to IRC to report progress
:bp

# Continue the conversation
Thanks! That fixed the memory leak issue.

# Switch between buffers as needed
:bn    # Back to code
:bp    # Back to IRC
```

### Troubleshooting IRC

#### Connection Issues
- **Cannot Connect**: Check server name and port (6667 for standard, 6697 for SSL)
- **Nickname in Use**: Try connecting with a different nickname
- **Server Unreachable**: Try alternative IRC servers like `irc.freenode.net`

#### Display Issues
- **IRC Interface Not Showing**: Ensure you're in the IRC buffer (use `:bn`/`:bp` to switch)
- **Messages Not Updating**: Check connection status; try reconnecting
- **Layout Problems**: IRC interface adapts to window size; try resizing

#### Buffer Switching Issues
- **Can't Switch to Files**: Use `:bn` to cycle through buffers
- **Lost IRC Buffer**: Use `:buffers` to see all buffers and select IRC buffer
- **Commands Not Working**: Ensure you're in IRC buffer when using IRC commands

### IRC Security and Best Practices

#### Connection Security
- **Use SSL**: Connect to port 6697 for encrypted connections when available
- **Server Trust**: Connect only to well-known IRC networks
- **Nickname Protection**: Register your nickname on networks that support it

#### Chat Etiquette
- **Channel Rules**: Read channel topics and follow guidelines
- **Appropriate Content**: Keep discussions relevant to channel purpose
- **Respect Others**: Follow standard IRC etiquette and respect other users

#### Privacy Considerations
- **Public Channels**: Remember that channel messages are visible to all users
- **Private Messages**: Use `msg` command for one-on-one conversations
- **Logging**: IRC conversations may be logged by servers or other users

### IRC Command Reference

#### Connection Commands
| Command | Description |
|---------|-------------|
| `:connect [server] [port] [nick]` | Connect to IRC server |
| `quit` | Disconnect from IRC |

#### Channel Commands
| Command | Description |
|---------|-------------|
| `join #channel` | Join a channel |
| `part` | Leave current channel |
| `part #channel` | Leave specific channel |
| `topic <text>` | Set channel topic |
| `who #channel` | List channel users |

#### Communication Commands
| Command | Description |
|---------|-------------|
| `<message>` | Send message to current channel |
| `msg <nick> <message>` | Send private message |
| `nick <newnick>` | Change nickname |

#### Buffer Navigation (Vi Commands)
| Command | Description |
|---------|-------------|
| `:bn`, `:bnext` | Switch to next buffer (IRC ↔ files) |
| `:bp`, `:bprev` | Switch to previous buffer |
| `:buffers` | Interactive buffer selector |

The IRC integration in Vizero provides a powerful combination of real-time communication and code editing, making it an ideal tool for collaborative development, getting programming help, and staying connected with development communities while working on your projects.

---

## Compiler Integration

### Compilation Commands
| Command | Action |
|---------|--------|
| `:cc [args]` | Compile with GCC |
| `:cpp [args]` | Compile with G++ |
| `:asm [args]` | Assemble with NASM/FASM |
| `:make` | Run make command in new window |

### External Commands
| Command | Action |
|---------|--------|
| `:!command` | Execute shell command in new window |
| `:r !command` | Read command output into buffer at cursor |

### Compiler Configuration
| Command | Action |
|---------|--------|
| `:set c_compiler <name>` | Set C compiler (gcc, msvc) |
| `:set cpp_compiler <name>` | Set C++ compiler (g++, msvc) |
| `:set assembler <name>` | Set assembler (nasm, fasm) |
| `:set c_compiler_path <path>` | Set C compiler executable path |
| `:set cpp_compiler_path <path>` | Set C++ compiler executable path |
| `:set assembler_path <path>` | Set assembler executable path |
| `:show c_compiler` | Show current C compiler setting |
| `:show cpp_compiler` | Show current C++ compiler setting |
| `:show assembler` | Show current assembler setting |

### Compiler Features
- **Configurable Compilers**: Set preferred compilers with `:set` commands
- **Automatic Detection**: Detects available compilers (GCC, MSVC, NASM, FASM)
- **Real-time Feedback**: Popup windows show compilation results
- **Error Capture**: Compiler output displayed in popup with timeout
- **Flexible Arguments**: Pass any compiler flags and options
- **Persistent Settings**: Compiler preferences saved between sessions

### Examples
```
# Configure compilers
:set c_compiler gcc
:set cpp_compiler g++
:set assembler nasm

# Set specific paths if needed
:set c_compiler_path "C:\MinGW\bin\gcc.exe"
:set cpp_compiler_path "C:\MinGW\bin\g++.exe"

# Compile with configured compilers
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
| `:set readonly on/off` | Set buffer read-only status |
| `:ro`, `:readonly` | Make current buffer read-only |
| `:noro`, `:noreadonly` | Make current buffer writable |
| `:show` | Display all current settings |
| `:show name` | Display specific setting |
| `:version` | Show version information popup |
| `:colourscheme <theme>` | Switch colour theme (Default/Monokai/Solarized Dark) |

### Session Management Commands
| Command | Action |
|---------|--------|
| `:mksession <name>` | Create/save session with specified name |
| `:session <name>` | Load session with specified name |
| `:sessions` | List all available sessions |
| `:session-save` | Save current session state |

**Read-Only Status:** The status bar displays the current buffer's read-only status with pale green 'rw' for read-write buffers and pale red 'ro' for read-only buffers.


### Available Settings
| Setting | Default | Description |
|---------|---------|-------------|
| `tab_size` | 4 | Number of spaces per tab |
| `auto_indent` | true | Automatic indentation |
| `show_line_numbers` | false | Display line numbers |
| `linewrap` | true | Word wrap at word boundaries |
| `colour_theme` | "Default" | Current colour theme (Default/Monokai/Solarized Dark) |
| `c_compiler` | "gcc" | C compiler (gcc, msvc) |
| `cpp_compiler` | "g++" | C++ compiler (g++, msvc) |
| `assembler` | "nasm" | Assembler (nasm, fasm) |
| `c_compiler_path` | "" | Path to C compiler executable |
| `cpp_compiler_path` | "" | Path to C++ compiler executable |
| `assembler_path` | "" | Path to assembler executable |


### Persistence
- Settings automatically saved to `%APPDATA%\Vizero\settings.ini`
- Loaded automatically on startup
- Cross-session persistence

---

## Advanced Features

### Fullscreen Mode
| Key | Action |
|-----|--------|
| `Ctrl+W` then `h/j/k/l` or arrow key | Switch window focus (when split) |
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
- **Syntax Highlighting**: Extensible language support for C/C++, C#, Python, PHP, XML, Markdown, Lisp
- **API Integration**: Full editor API access for plugins

### Search Match Highlighting
- **Real-time Visual Feedback**: All search matches highlighted
- **colour-coded Results**: Current match vs. other matches
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
| `o` | Open line below and enter Insert Mode |
| `O` | Open line above and enter Insert Mode |
| `:` | Enter Command Mode |
| `/` | Forward search |
| `n` | Next search result |
| `N` | Previous search result |
| `Ctrl+Space` | Trigger code completion (LSP) |
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
| `Ctrl+Space` | Trigger code completion (LSP) |
| `Ctrl+C/X/V` | Clipboard operations |
| `Ctrl+Z` | Undo |


### Command Mode
| Key | Action |
|-----|--------|
| `Enter` | Execute command |
| `Esc` | Cancel command |
| `Backspace` | Edit command |

#### Command Reference

### File Operations
| Command | Description |
|---------|-------------|
| `:w`    | Write (save) current buffer |
| `:wa`   | Write all modified buffers |
| `:q`    | Quit (if no unsaved changes) |
| `:q!`   | Force quit (discard changes) |
| `:wq`, `:x` | Write and quit |
| `:e <file>` | Open file in new buffer |
| `:r <file>` | Read file into buffer at cursor |
| `:file` | Show current filename and status |
| `:file <name>` | Set filename for current buffer |

### Buffer Management
| Command | Description |
|---------|-------------|
| `:buffers` | Interactive buffer selector (arrows + Enter) |
| `:bn`, `:bnext` | Next buffer |
| `:bp`, `:bprev` | Previous buffer |
| `:b1`, `:b2`, `:b3` | Switch to buffer number |
| `:bd`, `:bdelete` | Delete current buffer |
| `:bd N`, `:bdelete N` | Delete buffer N |
| `:new` | Create new empty buffer |

### Window Management
| Command | Description |
|---------|-------------|
| `:split`, `:sp` | Split window horizontally |
| `:vsplit`, `:vsp` | Split window vertically |
| `:close`, `:clo` | Close current window |
| `:only` | Close all windows except current |
| `:enew` | Edit new unnamed buffer |

### Navigation
| Command | Description |
|---------|-------------|
| `:<number>` | Go to line number (e.g., `:42`) |
| `:$` | Go to last line |
| `gg` | Go to first line |
| `G` | Go to last line |

### Search and Replace
| Command | Description |
|---------|-------------|
| `/<pattern>` | Search forward |
| `?<pattern>` | Search backward |
| `:s/old/new/[g]` | Replace on current line |
| `:%s/old/new/[g]` | Replace in entire file |

### Directory Operations
| Command | Description |
|---------|-------------|
| `:ls` | List files in current directory |
| `:chdir <path>` | Change working directory |
| `:pwd` | Print working directory |

### External Commands
| Command | Description |
|---------|-------------|
| `:!<command>` | Execute shell command in new window |
| `:r !<command>` | Read command output into buffer at cursor |

### Advanced Navigation & Editing
| Command | Description |
|---------|-------------|
| `:marks` | Show all marks |
| `:jumps` | Show jump history |
| `:changes` | Show change history |
| `:d` | Delete current line |
| `:y` | Yank (copy) current line |
| `:p` | Put (paste) after cursor |
| `:P` | Put (paste) before cursor |
| `:j` | Join current line with next |
| `:u` | Undo last change |
| `:redo` | Redo last undone change |
| `:n`, `:next` | Edit next file |
| `:prev`, `:previous` | Edit previous file |

### Line Range Operations
| Command | Description |
|---------|-------------|
| `<start>,<end>d` | Delete lines from start to end |
| `<start>,<end>y` | Yank lines from start to end |
| `<line>d` | Delete specific line number |
| `<line>y` | Yank specific line number |

### Global Commands
| Command | Description |
|---------|-------------|
| `g/<pattern>/d` | Delete all lines matching pattern |
| `v/<pattern>/d` | Delete all lines NOT matching pattern |

### Compilation
| Command | Description |
|---------|-------------|
| `:cc <args>` | Compile C file |
| `:cpp <args>` | Compile C++ file |
| `:asm <args>` | Assemble file |
| `:result` | Show last compilation result |

### Execution
| Command | Description |
|---------|-------------|
| `:run <program>` | Run program in new window |
| `:run` | Run last compiled executable |

### Settings
| Command | Description |
|---------|-------------|
| `:set <key> <value>` | Set configuration option |
| `:set linewrap on/off` | Enable/disable word wrap |
| `:show` | Show all settings |
| `:show <key>` | Show specific setting value |
| `:linenum on/off` | Enable/disable line numbers |
| `:syntax on/off` | Enable/disable syntax highlighting |
| `:tabs <N>` | Set tab size to N spaces |
| `:colourscheme <theme>` | Switch colour theme (Default/Monokai/Solarized Dark) |

### Session Management
| Command | Description |
|---------|-------------|
| `:mksession <name>` | Create/save session with specified name |
| `:session <name>` | Load session with specified name |
| `:sessions` | List all available sessions |
| `:session-save` | Save current session state |

### IRC Client
| Command | Description |
|---------|-------------|
| `:connect [server] [port] [nick]` | Connect to IRC server |
| `join #channel` | Join IRC channel (in IRC buffer) |
| `part` | Leave current IRC channel |
| `quit` | Disconnect from IRC |
| `nick <name>` | Change IRC nickname |
| `msg <nick> <message>` | Send private message |
| `<message>` | Send message to current channel |

### Help and Information
| Command | Description |
|---------|-------------|
| `:help`, `:h` | Load this manual in current buffer (close with :q or :bd to return) |

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
- **Word wrap not working?**: Word wrap is enabled by default. Use `:set linewrap off` to disable.
- **Markdown highlighting missing?**: Ensure you are editing a `.md` file. colours are optimized for readability.
- **Status bar not updating?**: The right-aligned time/date panel is always visible. Status messages revert to default after a short timeout.
- **Cursor disappears or scrolling broken?**: The cursor is always visible, including on empty lines. Up/down movement preserves the preferred column, and scrolling is smooth in all window modes.
- **Input not following window focus?**: This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window.
- **Crashes after split or file load?**: These have been resolved with robust buffer/cursor management.

### Getting Help
- Check this manual for command reference
- Experiment with commands in a test file
- Use `:show` to see current settings
- Try `:file` to check current file status

---

## Summary

Vizero is a remarkably comprehensive vi clone with an extensive command set that covers all essential vi/vim functionality and more. With over 45 implemented commands, it provides:

- **Complete file and buffer management** - Open, save, switch between multiple files
- **Advanced text editing** - Full search/replace with regex, undo system, clipboard integration  
- **Modern window management** - Split windows with proper focus handling
- **Developer tools** - Integrated compilation, execution, and directory operations
- **Flexible configuration** - Persistent settings with word wrap, syntax highlighting, and colour themes
- **Session management** - Save and restore workspace sessions for project continuity
- **Vi compatibility** - Standard navigation, modes, and command structure

The editor successfully combines classic vi behavior with modern conveniences, making it both familiar to vi users and accessible to newcomers.

---

*Vizero - A powerful, modern vi clone for developers*

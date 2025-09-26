# Lisp REPL Plugin - Interactive Mode Documentation

## Overview
The Lisp REPL Plugin provides seamless integration between Vizero and SBCL (Steel Bank Common Lisp), enabling interactive Lisp development within the editor. The current implementation features a fully interactive REPL with direct buffer typing, automatic expression evaluation, and comprehensive vi-style integration.

## Features

### Interactive Mode (Current Implementation)
- **Interactive Buffer Typing**: Direct typing into REPL buffer with automatic expression evaluation
- **SBCL Detection**: Automatically detects SBCL installation from common locations
- **Process Management**: Robust SBCL subprocess management with proper I/O redirection
- **Vi-Style Integration**: Seamless Escape+colon command mode integration
- **Buffer Switching Support**: Automatic state restoration when switching between buffers
- **Expression Evaluation**: Real-time parentheses balancing and result display
- **Cross-Platform Support**: Works on Windows and Unix-like systems
- **Graceful Error Handling**: Clean degradation when SBCL is not available

### Enhanced Features
- **Improved Multiline Input**: Enter key now properly handles incomplete expressions by inserting newlines
- **Single-line expressions** - balanced parentheses trigger immediate evaluation
- **Multiline expressions** - unbalanced parentheses allow continuation on next line
- **Smart parentheses detection** - real-time balance checking for automatic evaluation

## Commands

### `:lisp-connect`
Establishes connection to SBCL REPL and enters interactive mode.
- Detects SBCL installation automatically from common system locations
- Starts SBCL subprocess with optimized settings (512MB dynamic space, no debugger)
- Creates dedicated `*lisp-repl*` buffer for interactive development
- Enables direct typing with automatic expression evaluation
- Provides comprehensive state management across buffer switches

**Usage:**
```
:lisp-connect
```

### `:lisp-disconnect` 
Cleanly disconnects from SBCL REPL and exits interactive mode.
- Sends graceful quit command to SBCL process
- Terminates subprocess with proper cleanup
- Restores normal editing mode
- Preserves REPL buffer content for reference

**Usage:**
```
:lisp-disconnect
```

### `:lisp-status`
Shows detailed connection and process status information.
- SBCL process state (running/stopped/not found)
- SBCL executable path and version detection
- Interactive mode status
- Buffer switching state information
- Helpful installation guidance when SBCL is not available

**Usage:**
```
:lisp-status
```

### `:lisp-slime-connect`
Connects to a SLIME/Swank server via TCP socket for remote REPL interaction.
- Establishes TCP connection to running Swank server
- Uses SLIME :emacs-rex protocol for evaluation
- Intelligent response parsing from S-expressions
- Creates interactive REPL buffer with proper formatting
- Cross-platform socket support (Windows/Unix)
- Automatically disconnects from direct SBCL if connected
- Supports hostname and port arguments with defaults

**Usage:**
```
:lisp-slime-connect hostname port
:lisp-slime-connect localhost 4005    ; Example with local Swank server
:lisp-slime-connect 4005              ; Use localhost with custom port
:lisp-slime-connect                   ; Use defaults (localhost:4005)
```

**Prerequisites:**
- Swank server must be running on the target host/port
- See SLIME Installation Guide for server setup instructions

## SBCL Detection

The plugin automatically detects SBCL from these locations (in order):

### Windows
1. `sbcl.exe` (in PATH)
2. `C:\Program Files\Steel Bank Common Lisp\sbcl.exe`
3. `C:\Program Files (x86)\Steel Bank Common Lisp\sbcl.exe`

### Unix/Linux/macOS
1. `sbcl` (in PATH)
2. `/usr/local/bin/sbcl`
3. `/usr/bin/sbcl`
4. `/opt/sbcl/bin/sbcl`
5. `/home/user/.local/bin/sbcl`

## Installation

### SBCL Installation
The plugin requires SBCL to be installed on the system.

**Windows:**
1. Download SBCL from https://sbcl.org/
2. Install using the MSI installer
3. Ensure `sbcl.exe` is in your PATH or use default install location

**Ubuntu/Debian:**
```bash
sudo apt-get install sbcl
```

**macOS:**
```bash
brew install sbcl
```

**From Source:**
```bash
# Download and compile SBCL from source
# Follow instructions at https://sbcl.org/
```

### Plugin Usage
1. Ensure SBCL is installed and accessible
2. Load a Lisp file in Vizero or start with any file
3. Use `:lisp-status` to verify SBCL detection
4. Use `:lisp-connect` to start the REPL
5. Use `:lisp-eval` to evaluate expressions
6. Use `:lisp-disconnect` when finished

## Interactive Mode Usage

### Direct Buffer Typing
Once connected, the REPL buffer becomes fully interactive:
- **Type directly**: Just start typing Lisp expressions
- **Automatic evaluation**: Press Enter when parentheses are balanced
- **Real-time feedback**: Parentheses balancing and syntax awareness
- **Result display**: Evaluation results appear immediately in the buffer

### Vi-Style Integration
The REPL seamlessly integrates with Vizero's vi-style interface:
- **Escape+colon sequences**: Access all Vizero commands (`:buffers`, `:w`, etc.)
- **Buffer switching**: Switch to other files and return without losing REPL state
- **Command mode**: Full access to Vizero's command system while in REPL
- **State preservation**: Interactive mode automatically restores when returning to REPL buffer

### Example Interactive Session
```lisp
:lisp-connect
; Interactive mode enabled - type directly in buffer

(+ 1 2 3)
; → 6

; Multiline expressions now work naturally:
(defun factorial (n)
  (if (<= n 1)      ; Press Enter here - continues on next line
      1
      (* n (factorial (- n 1)))))  ; Press Enter here - evaluates when balanced
; → FACTORIAL

; Complex nested expressions work too:
(defun fibonacci (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))
; → FIBONACCI

(factorial 5)
; → 120

; Press Escape then :buffers to switch files
; Return to REPL buffer and continue typing
(* 2 21)
; → 42
```

## Architecture

### Interactive REPL Engine  
- **Direct Buffer Integration**: REPL buffer accepts direct keyboard input
- **Expression Parser**: Real-time parentheses balancing and expression completion detection
- **State Machine**: Complex state management for interactive mode, command mode, and buffer switching
- **Auto-Evaluation**: Automatic expression evaluation when parentheses are balanced and Enter is pressed

### Process Management
- **Windows**: CreateProcess with comprehensive pipe redirection and handle management
- **Unix**: fork/exec with robust pipe setup and signal handling
- **I/O Management**: Non-blocking reads from stdout/stderr with large buffer support
- **Graceful Termination**: Clean shutdown with proper process cleanup

### Buffer State Management
- **Interactive Mode Tracking**: Persistent state across buffer switches
- **Automatic Restoration**: Smart detection of REPL buffer return with state restoration
- **Command Mode Integration**: Seamless transition between interactive typing and vi commands
- **Multi-Buffer Awareness**: Maintains REPL functionality while editing other files

### Cross-Platform Compatibility
- **Windows**: Full support with proper console handling and DLL management
- **Unix/Linux**: Native fork/exec process management with signal handling
- **Path Detection**: Comprehensive SBCL installation detection across platforms
- **Error Handling**: Graceful degradation with helpful error messages

## Configuration

### SBCL Configuration
The plugin starts SBCL with these default options:
- `--dynamic-space-size 512`: 512MB heap size
- Standard I/O redirection for communication

### Working Directory
SBCL starts in the same directory as the current Vizero session, allowing easy access to project files.

## Error Handling

### Common Issues
1. **SBCL Not Found**: Install SBCL or add to PATH
2. **Permission Denied**: Check SBCL executable permissions
3. **Process Start Failure**: Verify SBCL installation integrity
4. **Communication Timeout**: Check system resources

### Diagnostic Commands
```
:lisp-status    ; Check overall status
:lisp-connect   ; Attempt connection (shows errors)
```

## Current Limitations

1. **Single SBCL Instance**: One REPL session per Vizero instance
2. **No Code Completion**: Lisp-specific autocompletion not yet implemented
3. **Basic Error Handling**: SBCL errors displayed as plain text
4. **No Package Navigation**: Package switching requires manual commands

## Future Enhancements

### Phase 3: SLIME Protocol Integration ✅ **COMPLETED**
- **✅ `:lisp-slime-connect` Command**: Fully implemented and registered command
- **✅ Command Registration**: Properly registered in lisp_commands[] array
- **✅ TCP Socket Connectivity**: Robust connection to Swank servers with proper error handling
- **✅ SLIME Protocol Support**: Full :emacs-rex message protocol implementation
- **✅ Response Parsing**: Intelligent extraction of results from S-expression responses
- **✅ Interactive Buffer Integration**: SLIME responses displayed directly in REPL buffer
- **✅ Dual Connection Architecture**: Seamless switching between direct SBCL and SLIME modes

### Phase 4: Advanced SLIME Features (Future)
- **Advanced Debugging**: Interactive debugger with stack traces and condition handling
- **Code Completion**: Context-aware Lisp symbol completion via SLIME
- **Inspector Integration**: Object inspection and modification capabilities
- **Macro Expansion**: Interactive macro expansion and analysis

### Additional Planned Features
- **Multiple REPL Sessions**: Support for concurrent Lisp environments
- **Enhanced Error Display**: Syntax highlighting for error messages and stack traces
- **Package Browser**: Visual package exploration and symbol lookup
- **Quicklisp Integration**: Automatic library loading and management
- **Remote REPL**: Connection to remote Lisp instances via SLIME protocol
- **History Search**: Searchable command history with regex support
- **Thread Management**: Support for multi-threaded Lisp applications

## Examples

### Basic Interactive Session
```
:lisp-status
; SBCL Status: Not running

:lisp-connect
; Found SBCL at: C:\Program Files\Steel Bank Common Lisp\sbcl.exe
; Starting SBCL process...
; Interactive mode enabled - type directly in buffer

; Now typing directly in the REPL buffer:
(+ 1 2 3)
; → 6

(defun greet (name)
  (format t "Hello, ~A!~%" name))
; → GREET

(greet "World")
; Hello, World!
; → NIL

; Press Escape, then :buffers to switch to other files
; When you return to *lisp-repl* buffer, interactive mode resumes automatically

(* 7 6)
; → 42

:lisp-disconnect
; SBCL process stopped
```

### Development Workflow
1. **Start Development**: Open your Lisp project files in Vizero
2. **Connect REPL**: Use `:lisp-connect` to start interactive session
3. **Load Code**: Type `(load "my-file.lisp")` directly in REPL buffer
4. **Test Functions**: Call functions interactively with immediate feedback
5. **Switch Contexts**: Use Escape+`:buffers` to edit source files
6. **Continue Testing**: Return to REPL buffer and continue development
7. **Iterate Rapidly**: Make changes in source files, reload, and test immediately

### Advanced Usage
```lisp
; Multi-line function definition
(defun fibonacci (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))

; Test the function
(mapcar #'fibonacci '(0 1 2 3 4 5 6 7 8 9))
; → (0 1 1 2 3 5 8 13 21 34)

; Package operations
(defpackage :my-utils
  (:use :cl)
  (:export #:my-function))

(in-package :my-utils)
; Now in MY-UTILS package

; Switch back
(in-package :cl-user)
```

### SLIME Integration (Phase 3 - Implemented)
```lisp
; SLIME protocol connection:
:lisp-slime-connect hostname port    ; Connect to Swank server
:lisp-slime-connect localhost 4005   ; Example connection

; Features now available:
; - TCP socket connection to Swank servers
; - SLIME :emacs-rex protocol evaluation
; - Intelligent response parsing from S-expressions
; - Interactive REPL buffer with formatted results
; - Dual connection architecture (direct SBCL + SLIME)
; - Cross-platform socket support (Windows/Unix)
```

### Future Enhancements
```lisp
; Planned for Phase 4:
; - Interactive debugger integration
; - Object inspection via SLIME
; - Advanced completion with symbol lookup  
; - Multiple simultaneous connections
; - Connection profiles and bookmarks
```

## Technical Details

### Plugin Interface
- **Plugin Type**: Generic (VIZERO_PLUGIN_TYPE_GENERIC)
- **Load Strategy**: Always loaded (`always_load: true`)
- **Dependencies**: SBCL runtime required
- **Platform Support**: Windows, Linux, macOS

### Memory Management
- **Process Handles**: Properly closed on cleanup
- **Buffer Memory**: Automatic cleanup on plugin unload
- **Message Storage**: Circular buffer with automatic overflow handling

### Thread Safety
- All operations run on main thread
- No thread synchronization required in Phase 1
- Future phases may add background threads for I/O

This completes the Phase 1 implementation of the Lisp REPL plugin, providing a solid foundation for SBCL integration within Vizero.
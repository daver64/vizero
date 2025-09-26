# Lisp REPL Plugin - Phase 1 Documentation

## Overview
The Lisp REPL Plugin provides integration between Vizero and SBCL (Steel Bank Common Lisp), enabling interactive Lisp development within the editor. Phase 1 focuses on basic SBCL process management and REPL functionality.

## Features

### Phase 1 (Current Implementation)
- **SBCL Detection**: Automatically detects SBCL installation from common locations
- **Process Management**: Starts and manages SBCL subprocess with proper I/O redirection
- **Basic REPL Commands**: Core commands for connecting, evaluating, and managing the REPL
- **Cross-Platform Support**: Works on Windows and Unix-like systems
- **Package Management**: Track and change Lisp packages
- **Status Monitoring**: Real-time connection and process status

## Commands

### `/lisp-connect`
Establishes connection to SBCL REPL.
- Detects SBCL installation automatically
- Starts SBCL subprocess with 512MB dynamic space
- Creates dedicated REPL buffer
- Initializes with welcome message

**Usage:**
```
:lisp-connect
```

### `/lisp-disconnect` 
Cleanly disconnects from SBCL REPL.
- Sends quit command to SBCL
- Terminates subprocess gracefully
- Clears connection state

**Usage:**
```
:lisp-disconnect
```

### `/lisp-eval <expression>`
Evaluates a Lisp expression in the current REPL session.
- Sends expression to SBCL for evaluation
- Displays results in REPL buffer
- Handles multi-line expressions (Phase 2 enhancement)

**Usage:**
```
:lisp-eval (+ 1 2 3)
:lisp-eval (defun hello () (format t "Hello World~%"))
:lisp-eval (hello)
```

### `/lisp-package [package-name]`
Changes or displays the current Lisp package.
- Without arguments: shows current package
- With argument: changes to specified package
- Updates REPL prompt to reflect current package

**Usage:**
```
:lisp-package           ; Show current package
:lisp-package cl-user   ; Change to CL-USER
:lisp-package my-pkg    ; Change to MY-PKG
```

### `/lisp-status`
Shows detailed connection and process status.
- SBCL process state (running/stopped)
- SBCL executable path
- Current package information
- Process ID (on Unix systems)

**Usage:**
```
:lisp-status
```

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

## Architecture

### Process Management
- **Windows**: Uses CreateProcess with pipe redirection
- **Unix**: Uses fork/exec with pipe redirection
- **I/O**: Non-blocking reads from stdout/stderr
- **Termination**: Graceful shutdown with SIGTERM, fallback to SIGKILL

### Buffer Management
- **Main Buffer**: `*lisp-repl*` for REPL interaction
- **Message History**: Stores up to 2000 messages with timestamps
- **Message Types**: Input, Output, Error, Result, Info, Debug
- **Colour Coding**: Different colours for different message types

### State Management
- **Connection State**: Tracks SBCL process status
- **Package State**: Current Lisp package for prompt display
- **Evaluation Counter**: Tracks number of evaluations
- **Input Buffer**: Temporary storage for multi-line input (Phase 2)

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

## Limitations (Phase 1)

1. **Basic Rendering**: Messages shown in status area only
2. **Limited Input**: Command-based evaluation only
3. **No Completion**: Code completion not implemented
4. **No Debugging**: SLIME debugger integration pending
5. **Single Session**: One SBCL instance per Vizero session

## Future Enhancements (Phase 2+)

### Phase 2: Enhanced UI
- Full-window REPL interface
- Real-time input/output display
- Syntax highlighting in REPL buffer
- Multi-line input support with parentheses matching

### Phase 3: SLIME Protocol
- Full SLIME protocol implementation
- Code completion and navigation
- Interactive debugger
- Inspector integration
- Macro expansion

### Phase 4: Advanced Features
- Multiple REPL sessions
- Remote REPL connection
- Project-aware package loading
- Quicklisp integration

## Examples

### Basic Usage Session
```
:lisp-status
# Output: SBCL Status: Not running

:lisp-connect
# Output: Found SBCL at: /usr/bin/sbcl
# Output: Starting SBCL process...
# Output: SBCL process started successfully
# Output: Switched to buffer: *lisp-repl*

:lisp-eval (+ 1 2 3)
# Output: Evaluating: (+ 1 2 3)
# Output: 6

:lisp-package
# Output: CL-USER

:lisp-eval (defpackage :my-test (:use :cl))
:lisp-package my-test
# Output: Package changed to MY-TEST

:lisp-disconnect
# Output: SBCL process stopped
```

### Development Workflow
1. Open Lisp source file in Vizero
2. Start REPL with `:lisp-connect`
3. Load file with `:lisp-eval (load "filename.lisp")`
4. Test functions with `:lisp-eval (my-function arg1 arg2)`
5. Iterate and develop interactively

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
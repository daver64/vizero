# SLIME Installation Guide for Vizero LISP REPL

## Overview
This guide covers installing SLIME (Superior Lisp Interaction Mode for Emacs) for use with Vizero's planned Phase 3 REPL integration.

## Installation Options

### Option 1: Quicklisp Installation (Recommended)

**Step 1: Download and Install Quicklisp**
```bash
# Download Quicklisp installer
Invoke-WebRequest -Uri "https://beta.quicklisp.org/quicklisp.lisp" -OutFile "quicklisp.lisp"

# Start SBCL
sbcl\sbcl.exe
```

```lisp
; In SBCL, load the downloaded installer
(load "quicklisp.lisp")
; This loads the installer and shows: "To continue with installation, evaluate: (quicklisp-quickstart:install)"

; Install Quicklisp (this step is required!)
(quicklisp-quickstart:install)

; If Quicklisp is already installed, you'll see an error.
; Choose option 0: [LOAD-SETUP] to load existing installation

; Make Quicklisp available in future sessions (if needed)
(ql:add-to-init-file)
```

**Important**: You must run `(quicklisp-quickstart:install)` or load existing setup before `ql:` commands work!

### Troubleshooting: "Package QL does not exist"

If you get this error, try these solutions:

**Solution 1: Load existing Quicklisp directly**
```lisp
; Load the setup file directly:
(load "C:/Users/daver/quicklisp/setup.lisp")
```

**Solution 2: Fresh start**
```lisp
; Exit SBCL completely:
(sb-ext:exit)

; Start SBCL again:
sbcl\sbcl.exe

; Load existing Quicklisp:
(load "C:/Users/daver/quicklisp/setup.lisp")

; Now try Swank:
(ql:quickload :swank)
```

**Solution 3: Clean slate installation**
```bash
# Remove corrupted Quicklisp installation:
Remove-Item -Recurse -Force C:\Users\daver\quicklisp

# Remove old installer:
Remove-Item quicklisp.lisp

# Download fresh installer:
Invoke-WebRequest -Uri "https://beta.quicklisp.org/quicklisp.lisp" -OutFile "quicklisp.lisp"

# Start SBCL fresh:
sbcl\sbcl.exe
```

```lisp
; Clean installation sequence:
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(ql:quickload :swank)
```

**Step 2: Install SLIME via Quicklisp**
```lisp
; In SBCL REPL (Quicklisp should now be loaded)
(ql:quickload :swank)

; Test installation (note: space between :dont-close and t)
(swank:create-server :port 4005 :dont-close t)
; Should start Swank server on port 4005
```

**Step 3: Verify Installation**
```lisp
; Check if Swank is available
(find-package :swank)
; Should return #<PACKAGE "SWANK">

; Start a test server (note: space between :dont-close and t)
(swank:create-server :port 4005 :dont-close t)
; Server should start successfully
```

### Option 2: Manual Installation

**Step 1: Download SLIME**
```bash
# Clone SLIME repository
git clone https://github.com/slime/slime.git
cd slime
```

**Step 2: Install Swank (SLIME backend)**
```bash
# Copy swank to SBCL contrib directory
cp -r swank/ path/to/sbcl/contrib/
```

**Step 3: Load in SBCL**
```lisp
; Add to ~/.sbclrc or equivalent
(require :swank)
```

## Vizero Integration Architecture

### Current State (Phase 2)
```
Vizero REPL Plugin → Direct SBCL Process → stdin/stdout pipes
```

### Planned State (Phase 3)
```
Vizero REPL Plugin → SLIME Connection → Swank Server → SBCL Process
                  ↗                              ↘
    Direct Mode (existing)              Advanced Features
```

## SLIME Protocol Overview

### Connection Process
1. **Start SBCL with Swank**: `(swank:create-server :port 4005)`
2. **Connect via TCP**: Vizero connects to localhost:4005
3. **Protocol Handshake**: Exchange version and capability info
4. **Command Processing**: Send/receive S-expressions over TCP

### Key SLIME Features for Integration

#### 1. Evaluation with Enhanced Results
```lisp
; SLIME provides richer evaluation results
(:emacs-rex (swank:eval-and-grab-output "(+ 1 2 3)") "COMMON-LISP-USER" :repl-thread 1)
; Returns: (:return (:ok ("6" "")) 1)
```

#### 2. Completion
```lisp
; Symbol completion
(:emacs-rex (swank:simple-completions "def" "COMMON-LISP-USER") nil :repl-thread 2)
; Returns list of symbols starting with "def"
```

#### 3. Documentation
```lisp
; Function documentation
(:emacs-rex (swank:documentation-symbol "defun") "COMMON-LISP-USER" :repl-thread 3)
; Returns documentation string
```

#### 4. Debugging
```lisp
; Interactive debugger
; When error occurs, SLIME provides stack trace navigation
; Restart selection and variable inspection
```

## Integration Complexity Assessment

### Low Complexity (Straightforward)
- **Basic SLIME Connection**: TCP socket connection to Swank server
- **Simple Evaluation**: Send expressions, receive results
- **Connection Management**: Start/stop Swank server

### Medium Complexity (Moderate Effort)
- **Protocol Parsing**: Parse S-expression responses from SLIME
- **Thread Management**: Handle SLIME's threaded communication model
- **Error Handling**: Process SLIME error messages and conditions

### High Complexity (Significant Effort)
- **Interactive Debugger**: Full debugger UI with stack traces
- **Completion Integration**: Real-time completion in Vizero buffer
- **Inspector Interface**: Object inspection and modification
- **Multiple Connection Management**: Handle multiple SLIME sessions

## Implementation Strategy

### Phase 3.1: Basic SLIME Connection
```c
// New connection type in plugin
typedef enum {
    LISP_CONNECTION_DIRECT,   // Current implementation
    LISP_CONNECTION_SLIME     // New SLIME connection
} lisp_connection_type_t;

// SLIME-specific state
typedef struct {
    int socket_fd;
    int connection_id;
    char read_buffer[8192];
    bool swank_server_running;
} slime_connection_t;
```

### Phase 3.2: Enhanced Features
- **Completion**: Hook into Vizero's completion system
- **Documentation**: Show function docs on hover
- **Debugging**: Interactive debugger interface

### Phase 3.3: Advanced Integration
- **Inspector**: Object browser interface
- **Profiling**: Performance analysis tools
- **Remote Connections**: Connect to remote SLIME servers

## Code Structure Changes Required

### Minimal Changes (Good Architecture!)
The current plugin architecture is well-designed for extension:

```c
// Current command handlers remain unchanged
static int lisp_cmd_connect(vizero_editor_t* editor, const char* args);

// Add new SLIME-specific handlers
static int lisp_cmd_slime_connect(vizero_editor_t* editor, const char* args);
static int lisp_cmd_slime_eval(vizero_editor_t* editor, const char* args);
static int lisp_cmd_slime_complete(vizero_editor_t* editor, const char* args);

// Unified interface - both modes use same buffer/UI
static int lisp_send_expression(const char* expr, lisp_connection_type_t type);
```

## Timeline Estimate

### Phase 3.1: Basic SLIME (2-3 weeks)
- TCP socket connection to Swank
- Basic evaluation through SLIME protocol
- Simple command: `:lisp-slime-connect`

### Phase 3.2: Enhanced Features (3-4 weeks)
- Completion integration
- Documentation lookup
- Error handling improvements

### Phase 3.3: Advanced Features (4-6 weeks)
- Interactive debugger
- Inspector interface
- Remote connections

## Testing Strategy

### Unit Tests
```c
// Test SLIME protocol parsing
void test_slime_parse_response();
void test_slime_connection();
void test_slime_evaluation();
```

### Integration Tests
```lisp
; Test SLIME server startup
(swank:create-server :port 4005 :dont-close t)

; Test evaluation
(:emacs-rex (swank:eval-and-grab-output "(+ 1 2)") "COMMON-LISP-USER" :repl-thread 1)
```

## Benefits of SLIME Integration

### For Users
- **Better Error Messages**: Stack traces with navigation
- **Code Completion**: Context-aware symbol completion
- **Documentation Access**: Instant function documentation
- **Interactive Debugging**: Step through code, inspect variables

### For Development
- **Proven Protocol**: SLIME is mature and well-tested
- **Rich Ecosystem**: Many Lisp tools support SLIME
- **Remote Development**: Connect to Lisp processes anywhere
- **Advanced Features**: Profiling, tracing, inspection built-in

## Conclusion

**Installation**: Straightforward with Quicklisp (30 minutes)
**Integration**: Moderate complexity, but well-architected current code makes it manageable
**Timeline**: 2-3 months for full implementation
**Benefits**: Significant enhancement to Lisp development experience

The current Vizero REPL plugin architecture is excellent for this extension - the separation between connection management and UI means SLIME integration can be added alongside the existing direct SBCL connection without major refactoring.
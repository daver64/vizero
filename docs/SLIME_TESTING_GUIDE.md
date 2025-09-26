# SLIME Integration Testing Guide

## Prerequisites
1. SBCL installed with Quicklisp and Swank
2. Vizero built with SLIME integration

## Testing Workflow

### Step 1: Start SBCL with Swank Server
```bash
# Terminal 1: Start SBCL
sbcl
```

```lisp
; In SBCL REPL:
(ql:quickload :swank)
(swank:create-server :port 4005 :dont-close t)
; Should show: "Swank started at port: 4005"
```

### Step 2: Start Vizero and Connect via SLIME
```bash
# Terminal 2: Start Vizero
vizero.exe
```

```
; In Vizero:
:lisp-status
; Should show: "Connection: None"

:lisp-slime-connect
; Should connect to localhost:4005

:lisp-status
; Should show: "Connection: SLIME | Server: localhost:4005 | Status: Connected"
```

### Step 3: Test Interactive SLIME REPL
```lisp
; Type directly in the REPL buffer:
(+ 1 2 3)
; Should evaluate via SLIME and show result

(defun test-function (x)
  (* x x))
; Should define function via SLIME

(test-function 5)
; Should return 25 via SLIME
```

### Step 4: Test Command Mode Integration
```
; Press Escape, then:
:buffers
; Should show buffer list

; Use arrows to navigate, Enter to switch
; Return to SLIME REPL buffer
; Interactive mode should restore automatically
```

### Step 5: Test Disconnect
```
:lisp-disconnect
; Should disconnect from SLIME server

:lisp-status
; Should show: "Connection: None"
```

## Command Reference

### SLIME Commands
- `:lisp-slime-connect [host] [port]` - Connect to SLIME server (default: localhost:4005)
- `:lisp-slime-connect 4005` - Connect to localhost on port 4005
- `:lisp-slime-connect remote-host 4005` - Connect to remote SLIME server
- `:lisp-disconnect` - Disconnect from current connection (SLIME or direct)
- `:lisp-status` - Show connection status and available commands

### Comparison: Direct vs SLIME

#### Direct SBCL Connection
```
:lisp-connect
; Starts SBCL process directly
; Uses stdin/stdout pipes
; Good for simple development
```

#### SLIME Connection
```
:lisp-slime-connect
; Connects to existing SBCL+Swank
; Uses TCP socket
; Better for advanced development
; Supports remote connections
; Foundation for future enhancements (debugging, completion, etc.)
```

## Architecture Notes

### Connection Types
1. **LISP_CONNECTION_NONE** - No active connection
2. **LISP_CONNECTION_DIRECT** - Direct SBCL process (existing)
3. **LISP_CONNECTION_SLIME** - SLIME protocol connection (new)

### Unified User Experience
- Same `*lisp-repl*` buffer for both connection types
- Same interactive typing and vi-style integration
- Same buffer switching with state restoration
- Connection type is transparent to user

### Future Enhancements (Ready for Implementation)
- SLIME completion: `(:emacs-rex (swank:simple-completions "def" "COMMON-LISP-USER") ...)`
- SLIME documentation: `(:emacs-rex (swank:documentation-symbol "defun") ...)`
- SLIME debugging: Interactive debugger with stack traces
- Remote development: Connect to SLIME servers on other machines

## Troubleshooting

### "Failed to connect to SLIME server"
1. Make sure SBCL is running with Swank: `(swank:create-server :port 4005 :dont-close t)`
2. Check port is not blocked by firewall
3. Try specific port: `:lisp-slime-connect 4005`

### "Connection: None" after successful connect
1. Check Vizero console for error messages
2. Verify SLIME server is still running in SBCL
3. Try reconnecting: `:lisp-disconnect` then `:lisp-slime-connect`

### Interactive mode not working
1. Make sure you're in the SLIME REPL buffer
2. Try `:lisp-status` to verify connection
3. Switch buffers and return to restore state

## Implementation Status

✅ **Implementation Exists**:
- TCP socket connection to SLIME/Swank
- Basic SLIME protocol message formatting
- Connection management (connect/disconnect)
- Interactive mode integration
- Buffer switching state preservation
- Cross-platform support (Windows/Unix)

⚠️ **Not Accessible**:
- `:lisp-slime-connect` command (handler exists but not registered)

🔄 **In Progress**:
- SLIME response parsing and result display
- Error handling and reconnection logic

📋 **Future** (Phase 3.2+):
- Advanced SLIME features (completion, debugging, inspection)
- Multiple SLIME sessions
- Remote connection management
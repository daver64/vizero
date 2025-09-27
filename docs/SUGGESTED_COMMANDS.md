
# Vizero Command Status and Suggestions

## Currently Implemented Commands (✅ Complete)

### File Operations
- ✅ `:w` - Write (save) current buffer
- ✅ `:wa` - Write all modified buffers
- ✅ `:q` - Quit (if no unsaved changes)
- ✅ `:q!` - Force quit (discard changes)
- ✅ `:wq`, `:x` - Write and quit
- ✅ `:e filename` - Open file in new buffer
- ✅ `:r filename` - Read file into buffer at cursor
- ✅ `:file` - Show current filename and status
- ✅ `:file name` - Set filename for current buffer

### Buffer/Window Management
- ✅ `:ls`, `:buffers` - List all buffers or files in directory
- ✅ `:bd`, `:bdelete` - Delete/close current buffer
- ✅ `:bd N`, `:bdelete N` - Delete buffer number N
- ✅ `:bn`, `:bnext` - Next buffer
- ✅ `:bp`, `:bprev` - Previous buffer
- ✅ `:b1`, `:b2`, `:b3` - Switch to buffer number
- ✅ `:new` - Create new empty buffer
- ✅ `:split`, `:sp` - Split window horizontally
- ✅ `:vsplit`, `:vsp` - Split window vertically
- ✅ `:close`, `:clo` - Close current window

### Navigation
- ✅ `:<number>` - Go to line number (e.g., `:42`)
- ✅ `:$` - Go to last line
- ✅ `gg` - Go to first line (implemented as normal mode command)
- ✅ `G` - Go to last line (implemented as normal mode command)

### Normal Mode Commands
- ✅ `i` - Enter insert mode at cursor
- ✅ `o` - Open new line below and enter insert mode
- ✅ `O` - Open new line above and enter insert mode
- ✅ `dd` - Delete current line
- ✅ `v` - Enter visual mode

### Search/Replace
- ✅ `/<pattern>` - Forward search with highlighting
- ✅ `?<pattern>` - Backward search with highlighting
- ✅ `:s/pattern/replacement/[g]` - Substitute on current line
- ✅ `:%s/pattern/replacement/[g]` - Global substitute (entire file)

### Directory Operations
- ✅ `:chdir <path>` - Change working directory

### Compilation & Execution
- ✅ `:cc <args>` - Compile C file
- ✅ `:cpp <args>` - Compile C++ file
- ✅ `:asm <args>` - Assemble file
- ✅ `:result`, `:compileresult` - Show last compilation result
- ✅ `:run <program>` - Run program in new window
- ✅ `:run` - Run last compiled executable

### Settings & Configuration
- ✅ `:set <key> <value>` - Set configuration option
- ✅ `:set linewrap on/off` - Enable/disable word wrap
- ✅ `:set readonly on/off` - Set buffer read-only status
- ✅ `:show` - Show all settings
- ✅ `:show <key>` - Show specific setting value
- ✅ `:linenum on/off` - Enable/disable line numbers
- ✅ `:syntax on/off` - Enable/disable syntax highlighting
- ✅ `:tabs <N>` - Set tab size to N spaces
- ✅ `:colourscheme <theme>` - Switch colour themes (Default/Monokai/Solarized Dark)

### Compiler Integration
- ✅ `:set c_compiler <name>` - Set C compiler (gcc, msvc)
- ✅ `:set cpp_compiler <name>` - Set C++ compiler (g++, msvc)
- ✅ `:set assembler <name>` - Set assembler (nasm, fasm)
- ✅ `:set c_compiler_path <path>` - Set C compiler executable path
- ✅ `:set cpp_compiler_path <path>` - Set C++ compiler executable path
- ✅ `:set assembler_path <path>` - Set assembler executable path

### Buffer Properties
- ✅ `:ro`, `:readonly` - Make current buffer read-only
- ✅ `:noro`, `:noreadonly` - Make current buffer writable
- ✅ Read-only status displayed in status bar (pale green 'rw' / pale red 'ro')

### Help & Information
- ✅ `:help`, `:h` - Load manual.md in current buffer (close with :q or :bd to return)

## Additional Suggested Commands for Future Implementation

## September 2025: Major Features and Fixes

- **Word Wrap (linewrap) by Default**: Lines wrap at word boundaries, with hanging indent for wrapped lines. Toggle with `:set linewrap on|off`.
- **Markdown Syntax Highlighting**: Built-in Markdown highlighting with improved colour contrast for headings, code, and emphasis.
- **Status Bar Improvements**: Status bar now features a right-aligned time/date panel, auto-reverting status messages, clear error/info popups, and read-only status indicator (pale green 'rw' / pale red 'ro').
- **Robust Cursor and Scrolling**: Cursor always visible, including on empty lines. Vertical scrolling and cursor movement are robust, with preferred column logic for up/down and correct mapping between logical and visual cursor positions.
- **Window Focus and Input Routing**: All input and editing operations always follow the currently focused window, matching vi-like behavior. After any window focus change (e.g., `Ctrl+w`), all input goes to the correct window and buffer.
- **Crash/Corruption Fixes**: Resolved all known crashes and data corruption after split and file load operations. Buffer and window arrays are always in sync.

## Recently Implemented Commands (✅ Now Complete)

### File Operations  
- ✅ `:n`, `:next` - Edit next file in argument list (same as :bn)
- ✅ `:prev`, `:previous` - Edit previous file in argument list (same as :bp)
- ✅ `:pwd` - Print working directory

### Buffer/Window Management
- ✅ `:only` - Close all windows except current
- ✅ `:enew` - Edit new unnamed buffer

### Text Manipulation (Command Mode)
- ✅ `:d` - Delete current line
- ✅ `:y` - Yank (copy) current line
- ✅ `:p` - Put (paste) after cursor
- ✅ `:P` - Put (paste) before cursor
- ✅ `:j` - Join current line with next
- ✅ `:u` - Undo last change
- ✅ `:redo` - Redo last undone change

### Line Range Operations
- ✅ `:1,5d` - Delete lines 1-5
- ✅ `:1,5y` - Yank lines 1-5
- ✅ `:1,5s/old/new/g` - Substitute in range
- ✅ `:.,.+5d` - Delete from current line to 5 lines down
- ✅ `<line>d` - Delete specific line number
- ✅ `<line>y` - Yank specific line number

### Advanced Search/Replace
- ✅ `:g/pattern/d` - Delete all lines matching pattern
- ✅ `:v/pattern/d` - Delete all lines NOT matching pattern

### Marks and Navigation
- ✅ `:marks` - List all marks (shows info popup)
- ✅ `:jumps` - List jump history (shows info popup)
- ✅ `:changes` - List change history (shows info popup)

### External Commands
- ✅ `:!command` - Execute shell command in new window
- ✅ `:r !command` - Read output of shell command into buffer
- ✅ `:make` - Run make command in new window

### Help and Information
- ✅ `:version` - Show version information and build details

### Compiler Configuration
- ✅ `:set c_compiler gcc|msvc` - Set C compiler
- ✅ `:set cpp_compiler g++|msvc` - Set C++ compiler
- ✅ `:set assembler nasm|fasm` - Set assembler
- ✅ `:set <compiler>_path <path>` - Set compiler executable paths

### LISP REPL Plugin (✅ Complete)
- ✅ `:lisp-connect` - Start interactive SBCL REPL
- ✅ `:lisp-disconnect` - Stop LISP REPL session
- ✅ `:lisp-status` - Show REPL connection status
- ✅ `:lisp-slime-connect <host> <port>` - Connect to SLIME/Swank server
- ✅ `:lisp-eval <expression>` - Evaluate Lisp expression
- ✅ `:lisp-package <package>` - Change current package
- ✅ `:lisp-complete <prefix>` - Show completion suggestions
- ✅ `:lisp-inspect <object>` - Inspect Lisp object
- ✅ `:lisp-trace <function>` - Trace function calls
- ✅ `:lisp-load <file>` - Load Lisp file
- ✅ `:lisp-compile <file>` - Compile Lisp file
- ✅ `:lisp-quicklisp` - Load Quicklisp package manager
- ✅ `:lisp-help` - Show LISP REPL help

### SQL REPL Plugin (✅ Complete)
- ✅ `:sql-connect <uri>` - Connect to database (postgresql://user:pass@host:port/db)
- ✅ `:sql-disconnect` - Disconnect from database
- ✅ `:sql-status` - Show database connection status
- ✅ `:sql-query <SELECT_statement>` - Execute SELECT query with formatted results
- ✅ `:sql-exec <DDL_or_DML>` - Execute INSERT/UPDATE/DELETE/CREATE/DROP statements
- ✅ `:sql-show-tables` - List all tables in database (schema-aware)
- ✅ `:sql-describe <table>` - Show table structure and column information
- ✅ `:sql-begin` - Begin database transaction
- ✅ `:sql-commit` - Commit current transaction
- ✅ `:sql-rollback` - Rollback current transaction

## Commands Still To Be Implemented (❌ Not Implemented)

### Session Management (Framework Complete, Implementation Pending)
- ❌ `:mksession <name>` - Create/save session (shows "not yet implemented" message)
- ❌ `:session <name>` - Load session (shows "not yet implemented" message) 
- ❌ `:sessions` - List available sessions (shows "not yet implemented" message)
- ❌ `:session-save` - Save current session (shows "not yet implemented" message)

### Advanced Features
- ❌ `:grep pattern files` - Run grep and load results
- ❌ `:source filename` - Execute commands from file

### Theme System (Partially Implemented)
- ✅ `:colourscheme Default|Monokai|Solarized\ Dark` - Switch colour themes (implemented)
- ❌ Custom theme creation/loading

### Configuration Aliases
- ❌ `:set number` - Show line numbers (alias for :linenum on)
- ❌ `:set nonumber` - Hide line numbers (alias for :linenum off)
- ❌ `:set hlsearch` - Highlight search results (search highlighting is always on)
- ❌ `:set nohlsearch` - Don't highlight search results

## Priority Implementation Order for Remaining Commands

### High Priority (Most Useful)
1. ❌ Session management implementation - Framework exists, needs actual save/load logic
2. ❌ `:grep pattern files` - Search integration (cross-platform solution needed)

### Medium Priority (Quality of Life)
1. ❌ `:source filename` - Script execution for configuration
2. ❌ Configuration aliases (`:set number`, `:set nonumber`)
3. ❌ Custom theme creation/loading

### Lower Priority (Advanced Features)
1. ❌ Advanced search highlighting controls
2. ❌ Extended mark navigation in normal mode

## Summary of Current State

**Vizero has an exceptionally comprehensive command set!** The core vi/vim functionality is now nearly complete:

### ✅ **Fully Implemented Areas:**
- Complete file operations (open, save, quit, read)
- Full buffer management (create, switch, delete, list, interactive selector)
- Complete window management (split, close, focus, only)
- Advanced search and replace with regex support
- Line range operations (delete, yank, substitute)
- Global commands (g/pattern/d, v/pattern/d)
- Comprehensive settings system with persistence
- Integrated compilation and execution with configurable compilers
- External command execution (`:!command`, `:r !command`)
- Directory operations (ls, chdir, pwd)
- Navigation commands and history displays
- Text manipulation (delete, yank, paste, join, undo/redo)
- Syntax highlighting and word wrap
- Help system and version information
- Colour theme system (Default/Monokai/Solarized Dark)

### ❌ **Notable Missing Features:**
- Session management implementation (framework exists, shows placeholder messages)
- Grep integration for cross-file search
- Configuration aliases and extended search controls
- Script execution (`:source filename`)

The editor is remarkably feature-complete for a vi clone and includes many modern conveniences like interactive buffer selection, compiler integration, and real-time syntax highlighting!

## Implementation Notes

- Many of these commands would require extending the current command parser
- Some require new subsystems (window management, marks, session handling)
- The `:!` command integration would need careful security consideration
- Line range parsing would need to be added to the command system
- Help system would need documentation integration

## Troubleshooting

- **Word wrap not working?**: Word wrap is enabled by default. Use `:set linewrap off` to disable.
- **Markdown highlighting missing?**: Ensure you are editing a `.md` file. colours are optimized for readability.
- **Input not following window focus?**: This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window.


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
- ✅ `:show` - Show all settings
- ✅ `:show <key>` - Show specific setting value
- ✅ `:linenum on/off` - Enable/disable line numbers
- ✅ `:syntax on/off` - Enable/disable syntax highlighting
- ✅ `:tabs <N>` - Set tab size to N spaces

### Help & Information
- ✅ `:help`, `:h` - Show comprehensive help popup

## Additional Suggested Commands for Future Implementation

## September 2025: Major Features and Fixes

- **Word Wrap (linewrap) by Default**: Lines wrap at word boundaries, with hanging indent for wrapped lines. Toggle with `:set linewrap on|off`.
- **Markdown Syntax Highlighting**: Built-in Markdown highlighting with improved color contrast for headings, code, and emphasis.
- **Status Bar Improvements**: Status bar now features a right-aligned time/date panel, auto-reverting status messages, and clear error/info popups.
- **Robust Cursor and Scrolling**: Cursor always visible, including on empty lines. Vertical scrolling and cursor movement are robust, with preferred column logic for up/down and correct mapping between logical and visual cursor positions.
- **Window Focus and Input Routing**: All input and editing operations always follow the currently focused window, matching vi-like behavior. After any window focus change (e.g., `:wincmd`, `Ctrl+w`), all input goes to the correct window and buffer.
- **Crash/Corruption Fixes**: Resolved all known crashes and data corruption after split and file load operations. Buffer and window arrays are always in sync.

## Commands Still To Be Implemented (❌ Not Implemented)

### File Operations
- ✅ `:n` - Edit next file in argument list (same as :bn)
- ✅ `:prev` - Edit previous file in argument list (same as :bp)
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
- ✅ `:u` - Undo last change (Ctrl+Z works in normal mode)
- ✅ `:redo` - Redo last undone change

### Line Range Operations
- ✅ `:1,5d` - Delete lines 1-5
- ✅ `:1,5y` - Yank lines 1-5  
- ✅ `:1,5s/old/new/g` - Substitute in range
- ✅ `:.,.+5d` - Delete from current line to 5 lines down

### Advanced Search/Replace  
- ✅ `:g/pattern/d` - Delete all lines matching pattern
- ✅ `:g/pattern/p` - Print all lines matching pattern
- ✅ `:v/pattern/d` - Delete all lines NOT matching pattern
- ✅ `:%g/pattern/s//replacement/g` - Global substitute with pattern

### Marks and Navigation
- ✅ `:marks` - List all marks (shows info popup)
- ✅ `:jumps` - List jump history (shows info popup)
- ✅ `:changes` - List change history (shows info popup)

### External Commands
- ✅ `:!command` - Execute shell command
- ✅ `:r !command` - Read output of shell command
- ✅ `:make` - Run make command
- ❌ `:grep pattern files` - Run grep and load results

### Help and Information
- ✅ `:version` - Show version information

### Session Management
- ❌ `:mksession` - Save current session
- ❌ `:source filename` - Execute commands from file

### Advanced Configuration  
- ❌ `:colorscheme name` - Set color scheme
- ❌ `:set number` - Show line numbers (alias for :linenum on)
- ❌ `:set nonumber` - Hide line numbers (alias for :linenum off) 
- ❌ `:set hlsearch` - Highlight search results (search highlighting is always on)
- ❌ `:set nohlsearch` - Don't highlight search results

## Priority Implementation Order for Remaining Commands

### High Priority (Most Used)
1. ❌ `:grep pattern files` - Search integration (cross-platform solution needed)

### Lower Priority (Advanced Features)  
1. ❌ `:mksession` - Session management
2. ❌ `:colorscheme` - Theme support
3. ❌ `:jumps` / `:changes` - History navigation
4. ❌ `:source filename` - Script execution
5. ❌ `:grep` integration

## Summary of Current State

**Vizero has an exceptionally comprehensive command set!** The core vi/vim functionality is largely complete:

### ✅ **Fully Implemented Areas:**
- Complete file operations (open, save, quit, read)
- Full buffer management (create, switch, delete, list)
- Complete window management (split, close, focus)
- Advanced search and replace with regex support
- Comprehensive settings system with persistence
- Integrated compilation and execution
- Directory operations
- Navigation commands
- Syntax highlighting and word wrap
- Help system

### ❌ **Notable Missing Features:**
- Shell command execution (`:!command`)
- Session management (`:mksession`)
- Full mark navigation system (mark setting/jumping in normal mode)
- Jump/change history navigation (Ctrl+O, Ctrl+I, g;, g,)

The editor is remarkably feature-complete for a vi clone and includes many modern conveniences!

## Implementation Notes

- Many of these commands would require extending the current command parser
- Some require new subsystems (window management, marks, session handling)
- The `:!` command integration would need careful security consideration
- Line range parsing would need to be added to the command system
- Help system would need documentation integration

## Troubleshooting

- **Word wrap not working?**: Word wrap is enabled by default. Use `:set linewrap off` to disable.
- **Markdown highlighting missing?**: Ensure you are editing a `.md` file. Colors are optimized for readability.
- **Input not following window focus?**: This is now fixed: after any window focus change, all input and editing will go to the correct (focused) window.
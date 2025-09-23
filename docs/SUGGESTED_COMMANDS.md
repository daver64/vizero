# Suggested Additional Commands for Vizero

## Essential Vi/Vim Commands to Implement

### File Operations
- `:n` - Edit next file in argument list
- `:prev` - Edit previous file in argument list
- `:split filename` - Split window horizontally
- `:vsplit filename` - Split window vertically
- `:cd directory` - Change working directory
- `:pwd` - Print working directory

### Buffer/Window Management
- `:bd` - Delete/close current buffer
- `:only` - Close all windows except current
- `:close` - Close current window
- `:new` - Create new empty buffer
- `:enew` - Edit new unnamed buffer

### Text Manipulation
- `:d` - Delete current line
- `:y` - Yank (copy) current line
- `:p` - Put (paste) after cursor
- `:P` - Put (paste) before cursor
- `:j` - Join current line with next
- `:u` - Undo last change
- `:redo` - Redo last undone change

### Line Range Operations
- `:1,5d` - Delete lines 1-5
- `:1,5y` - Yank lines 1-5
- `:1,5s/old/new/g` - Substitute in range
- `:10` - Go to line 10
- `:$` - Go to last line
- `:.,.+5d` - Delete from current line to 5 lines down

### Advanced Search/Replace
- `:g/pattern/d` - Delete all lines matching pattern
- `:g/pattern/p` - Print all lines matching pattern
- `:v/pattern/d` - Delete all lines NOT matching pattern
- `:%g/pattern/s//replacement/g` - Global substitute with pattern

### Marks and Navigation
- `:marks` - List all marks
- `:jumps` - List jump history
- `:changes` - List change history

### External Commands
- `:!command` - Execute shell command
- `:r !command` - Read output of shell command
- `:make` - Run make command
- `:grep pattern files` - Run grep and load results

### Help and Information
- `:help` - Show help
- `:version` - Show version information
- `:syntax` - Toggle syntax highlighting
- `:syntax on/off` - Enable/disable syntax highlighting

### Session Management
- `:mksession` - Save current session
- `:source filename` - Execute commands from file

### Configuration
- `:colorscheme name` - Set color scheme
- `:set number` - Show line numbers (alias for :linenum on)
- `:set nonumber` - Hide line numbers (alias for :linenum off)
- `:set wrap` - Enable line wrapping
- `:set nowrap` - Disable line wrapping
- `:set hlsearch` - Highlight search results
- `:set nohlsearch` - Don't highlight search results

## Priority Implementation Order

### High Priority (Most Used)
1. `:bd` - Buffer delete
2. `:new` - New buffer
3. `:!command` - Shell commands
4. `:help` - Help system
5. `:syntax on/off` - Syntax highlighting toggle
6. Line number operations (`:10`, `:$`)

### Medium Priority
1. `:split` / `:vsplit` - Window splitting
2. `:cd` / `:pwd` - Directory operations
3. `:g/pattern/d` - Global operations
4. `:marks` - Mark management
5. `:make` - Build integration

### Lower Priority (Advanced Features)
1. `:mksession` - Session management
2. `:colorscheme` - Theme support
3. `:jumps` / `:changes` - History navigation
4. Complex range operations

## Implementation Notes

- Many of these commands would require extending the current command parser
- Some require new subsystems (window management, marks, session handling)
- The `:!` command integration would need careful security consideration
- Line range parsing would need to be added to the command system
- Help system would need documentation integration
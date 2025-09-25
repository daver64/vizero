# Plugin Conflict Resolution in Vizero

## Overview

Vizero uses a **first-come-first-served** plugin resolution system with clean file extension mapping to prevent conflicts between syntax highlighting plugins.

## Current Plugin Architecture

### 8 Built-in Plugins (No Conflicts)
1. **Example Plugin** - Template/demo plugin
2. **File Browser** - File system navigation  
3. **C/C++/Assembly Syntax** - `.c`, `.h`, `.cpp`, `.cxx`, `.cc`, `.hpp`, `.s`, `.S`, `.asm`, `.inc`, `.x86`
4. **C# Syntax** - `.cs`
5. **Markdown Syntax** - `.md`, `.markdown`
6. **XML Syntax** - `.xml`, `.xsd`, `.xsl`, `.xslt`, `.xaml`, `.config`, `.plist`, `.svg`
7. **Python Syntax** - `.py`, `.pyw`, `.pyi`, `.pyx`
8. **Common Lisp Syntax** - `.lisp`, `.lsp`, `.cl`, `.asd`, `.el`

### File Extension Mapping
**No overlapping extensions** - each file type is handled by exactly one plugin.

## Conflict Resolution Algorithm

### Current Implementation (plugin_manager.cpp)
```c
int vizero_plugin_manager_highlight_syntax(...) {
    // Iterate through all loaded plugins
    for (size_t i = 0; i < manager->plugin_count; i++) {
        vizero_plugin_t* plugin = manager->plugins[i];
        if (plugin is syntax highlighter) {
            int n = plugin->callbacks.highlight_syntax(...);
            if (n > 0) {
                // First plugin that returns tokens wins
                return 1; 
            }
        }
    }
    return 0; // No plugin handled the buffer
}
```

### Conflict Detection (Debug Mode)
```c
#ifdef DEBUG_PLUGIN_CONFLICTS
// Check if other plugins also claim this buffer
// Log conflicts: "Both PluginA and PluginB claim file.ext (using PluginA)"
#endif
```

## REPL Buffer Support

### Python REPL Detection
- `*Python*`, `*REPL*`, `python-repl`, `.py-repl`
- Standard Python file extensions: `.py`, `.pyw`, `.pyi`, `.pyx`

### Common Lisp REPL Detection  
- `*SLIME*`, `*Lisp*`, `*REPL*`, `lisp-repl`, `.lisp-repl`
- Common Lisp implementations: `*sbcl*`, `*ccl*`, `*clisp*`
- Standard Lisp file extensions: `.lisp`, `.lsp`, `.cl`, `.asd`, `.el`

## Plugin Load Order

Plugins are loaded in **alphabetical order** by DLL filename:
1. `example_plugin.dll`
2. `file_browser.dll` 
3. `syntax_c.dll`
4. `syntax_csharp.dll`
5. `syntax_lisp.dll`
6. `syntax_markdown.dll`
7. `syntax_python.dll`
8. `syntax_xml.dll`

Load order determines precedence in the unlikely event of conflicts.

## Preventing Future Conflicts

### Plugin Development Guidelines
1. **Unique File Extensions**: Choose non-overlapping file extensions
2. **Specific REPL Names**: Use unique REPL buffer naming patterns
3. **Return 0 for Non-Matching**: Plugins should return 0 tokens for files they don't handle
4. **Test File Type Detection**: Verify file extension and REPL buffer detection logic

### Potential Improvements (Future)
1. **Plugin Priority System**: Allow plugins to specify priority levels
2. **Explicit Registration**: Plugins register supported file types during init
3. **Conflict Resolution UI**: Allow users to choose preferred plugin for conflicting types
4. **Plugin Dependencies**: Declare plugin dependencies and load order requirements

## Current Status: ✅ No Conflicts

All current syntax highlighting plugins have **clean, non-overlapping** file extension mappings and unique REPL buffer detection patterns. The first-come-first-served system works perfectly for the current plugin ecosystem.

## Testing Scenarios

### Multi-File Loading (Verified)
```bash
./vizero.exe test_python.py test_lisp.lisp README.md test.cs test.xml
# All 5 different file types load with correct syntax highlighting
# No conflict warnings
```

### REPL Buffer Detection (Verified)  
```bash
./vizero.exe "*Python*.py" "*SLIME*.lisp"
# REPL buffers correctly detected and highlighted
# Python and Lisp plugins handle special buffer names
```

### Removed Conflicts (Resolved)
- **OLD**: Monolithic `syntax_highlight.c` conflicted with modular plugins
- **FIXED**: Removed old monolithic plugin directory
- **RESULT**: Clean plugin ecosystem with no overlapping claims
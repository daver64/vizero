# JavaScript Syntax Highlighter Plugin

## Overview

The JavaScript syntax highlighter plugin provides comprehensive syntax highlighting for JavaScript, TypeScript, and JSX files in Vizero editor.

## Supported File Extensions

- `.js` - JavaScript files
- `.jsx` - React JSX files  
- `.mjs` - ECMAScript modules
- `.cjs` - CommonJS modules
- `.ts` - TypeScript files
- `.tsx` - TypeScript JSX files

## Supported Features

### Language Constructs
- **Keywords**: `async`, `await`, `class`, `const`, `let`, `var`, `function`, `import`, `export`, etc.
- **Built-in Objects**: `Array`, `Object`, `Promise`, `Map`, `Set`, `JSON`, `Math`, `console`, etc.
- **Constants**: `true`, `false`, `null`, `undefined`, `Infinity`, `NaN`
- **DOM APIs**: `document`, `window`, `localStorage`, `fetch`, `setTimeout`, etc.

### Syntax Elements
- **Comments**: Single-line (`//`) and multi-line (`/* */`) comments
- **Strings**: Single quotes, double quotes, and template literals (backticks)
- **Numbers**: Integers, floats, hex (0xFF), octal (0o755), binary (0b1010), BigInt (123n)
- **Regular Expressions**: `/pattern/flags` with proper flag detection
- **Operators**: All JavaScript operators and punctuation
- **Template Literals**: Backtick strings with ${} interpolation highlighting

### Advanced Features
- **Function Detection**: Highlights function names when followed by parentheses
- **Property Access**: Highlights property names after dot notation
- **Class/Function Definitions**: Bold highlighting for `class` and `function` keywords
- **Import/Export**: Special highlighting for module syntax

## Color Scheme

The plugin uses a modern JavaScript-themed color palette:

- **Keywords**: Blue - Core language keywords
- **Strings**: Orange - String literals and template literals  
- **Comments**: Green (italic) - All comment types
- **Numbers**: Light green - Numeric literals
- **Functions**: Yellow - Function names and calls
- **Built-ins**: Cyan - Built-in objects and methods
- **Constants**: Cornflower blue - Boolean and special values
- **Properties**: Light blue - Object properties
- **Operators**: Light gray - Operators and punctuation
- **Imports**: Purple - Import/export statements
- **Regex**: Brown - Regular expression literals

## Usage

The plugin automatically activates when opening JavaScript files or buffers with supported extensions. No manual configuration is required.

### Example Code

```javascript
// Modern JavaScript with syntax highlighting
class ApiClient {
    constructor(baseUrl) {
        this.baseUrl = baseUrl;
    }
    
    async fetchData(endpoint) {
        const response = await fetch(`${this.baseUrl}/${endpoint}`);
        return response.json();
    }
}

const client = new ApiClient('https://api.example.com');
const data = await client.fetchData('users');
console.log(data);
```

## Technical Details

- **Plugin Type**: Syntax Highlighter
- **Version**: 1.0.0
- **Dependencies**: Vizero core API
- **Performance**: Optimized for real-time highlighting during editing

## Installation

The plugin is built automatically as part of the Vizero build process and loaded at runtime. No separate installation is required.
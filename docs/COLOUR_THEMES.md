# Vizero Colour Themes

Vizero now includes a comprehensive collection of colour themes inspired by popular editors and IDEs. This document describes the available themes and how to use them.

## Available Themes

### Dark Themes

#### Default
- **Description**: Original Vizero dark theme
- **Background**: Dark blue-gray
- **Best for**: General programming, low-light environments
- **Command**: `:colourscheme Default`

#### Monokai 
- **Description**: Popular theme inspired by Sublime Text
- **Background**: Dark charcoal
- **Colours**: Pink keywords, yellow strings, purple numbers
- **Best for**: Modern development, syntax highlighting
- **Command**: `:colourscheme Monokai`

#### Solarized Dark
- **Description**: Ethan Schoonover's scientifically designed colour scheme
- **Background**: Dark blue-green
- **Colours**: Carefully balanced hues for reduced eye strain
- **Best for**: Long coding sessions, colour accuracy
- **Command**: `:colourscheme "Solarized Dark"`

#### GVim Desert
- **Description**: Popular desert colour scheme from GVim
- **Background**: Dark gray
- **Colours**: Warm earth tones (oranges, browns, yellows)
- **Best for**: Retro feel, warm colour preference
- **Command**: `:colourscheme "GVim Desert"`

#### GVim Evening
- **Description**: Evening colour scheme from GVim
- **Background**: Dark blue
- **Colours**: Cool evening tones (blues, cyans, yellows)
- **Best for**: Night coding, cool colour preference
- **Command**: `:colourscheme "GVim Evening"`

#### Gruvbox Dark
- **Description**: Retro groove colour scheme
- **Background**: Dark gray with warm undertones
- **Colours**: Retro-inspired palette with high contrast
- **Best for**: Vintage aesthetics, warm retro feel
- **Command**: `:colourscheme "Gruvbox Dark"`

### Light Themes

#### MSVC Light
- **Description**: Clean light theme inspired by Visual Studio
- **Background**: Pure white
- **Colours**: Blue keywords, red strings, green comments
- **Best for**: Daytime coding, professional environments
- **Command**: `:colourscheme "MSVC Light"`

#### MSVC Blue
- **Description**: Subtle blue-tinted theme inspired by Visual Studio
- **Background**: Very light blue
- **Colours**: Similar to MSVC Light with cooler undertones
- **Best for**: Reduced eye strain in bright environments
- **Command**: `:colourscheme "MSVC Blue"`

#### GVim Default
- **Description**: Classic GVim default colour scheme
- **Background**: White
- **Colours**: Traditional vim colours (purple, magenta, blue)
- **Best for**: Vim users, traditional editing
- **Command**: `:colourscheme "GVim Default"`

#### GitHub Light
- **Description**: Light theme inspired by GitHub's interface
- **Background**: White
- **Colours**: Modern web-inspired palette
- **Best for**: Web development, modern workflows
- **Command**: `:colourscheme "GitHub Light"`

## Usage Instructions

### Switching Themes
Use the `:colourscheme` command (or `:colorscheme` for US spelling) followed by the theme name:

```
:colourscheme Monokai
:colourscheme "MSVC Light"
:colourscheme "GVim Desert"
```

**Note**: Theme names with spaces must be quoted.

### Listing Available Themes
To see all available themes with descriptions:
```
:colourscheme
```

### Persistent Theme Settings
Once you set a theme, it's automatically saved to your settings file and will be restored when you restart Vizero.

## Testing Themes

Use the provided `test_themes.c` file to see how different syntax elements appear in each theme:

```bash
.\vizero test_themes.c
```

The test file includes:
- Keywords and control structures
- String literals and escape sequences  
- Numbers in various formats (decimal, hex, octal)
- Comments (single-line and multi-line)
- Preprocessor directives
- Function definitions and calls
- Operators and punctuation
- Type definitions and constants

## Theme Development

The colour theme system is implemented in `src/core/colour_theme.cpp`. Each theme defines colours for:

### UI Elements
- Background and foreground
- Cursor and selection
- Line numbers
- Status bar
- Window borders

### Syntax Elements  
- Keywords
- Strings and literals
- Comments
- Numbers
- Operators
- Functions and types
- Variables and constants
- Error highlighting

## Popular Theme Combinations

### For C/C++ Development
- **Professional**: MSVC Light or MSVC Blue
- **Modern**: Monokai or GitHub Light  
- **Classic**: GVim Default or GVim Desert
- **Eye-friendly**: Solarized Dark or Gruvbox Dark

### For Web Development
- GitHub Light (matches GitHub interface)
- MSVC Light (clean and modern)
- Monokai (popular with web developers)

### For Long Coding Sessions
- Solarized Dark (scientifically designed for eye comfort)
- Gruvbox Dark (warm, high contrast)
- GVim Evening (cool, calming colours)

### For Bright Environments
- MSVC Light (high contrast)
- MSVC Blue (reduced glare)
- GitHub Light (modern and clean)

## Customization

Future versions may support:
- Custom theme loading from files
- Theme editor interface
- Per-language theme overrides
- Dynamic theme switching based on time of day

The theme system is designed to be easily extensible - new themes can be added to the `vizero_theme_manager_load_builtin_themes` function in `colour_theme.cpp`.
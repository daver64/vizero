# Claude LLM Plugin for Vizero

This plugin integrates Claude Haiku with Vizero for AI-powered code assistance, providing interactive chat and code analysis capabilities directly within the editor.

## Quick Setup

1. **Get Claude API Key**
   - Sign up at https://console.anthropic.com/
   - Create an API key in your dashboard

2. **Create API Key File**
   - Create a file named `claude-key.txt` in your Vizero directory (same directory as vizero.exe)
   - Paste your API key into this file (just the key, nothing else)
   - Save the file

3. **Test the Integration**
   ```
   :claude-chat Hello, can you help me with C programming?
   ```

## Current Commands

### `:claude-chat <prompt>`
Interactive AI chat for general coding assistance and programming conversations.

**Examples:**
```
:claude-chat How do I implement a linked list in C?
:claude-chat What are the best practices for memory management?
:claude-chat Explain the difference between stack and heap allocation
```

### `:claude-ask <prompt>`
Ask specific questions about your current code or programming concepts.

**Examples:**
```
:claude-ask What could be causing this segmentation fault?
:claude-ask How can I optimize this algorithm?
:claude-ask Is this code following good C++ practices?
```

## Model Information

**Current Model**: Claude 3 Haiku (claude-3-haiku-20240307)
- **Speed**: Very fast responses
- **Cost**: Most economical option (~$1-3/month for typical usage)
- **Capability**: Excellent for code assistance, debugging, and programming help

## Cost Estimation

For typical coding usage (100 requests/day):
- **Claude 3 Haiku**: ~$1-3/month
- **Much more cost-effective** than subscription services for occasional use!

## Features

### AI-Powered Development
- **Code Explanation**: Get clear explanations of complex code
- **Debugging Assistance**: Help identifying and fixing bugs
- **Best Practice Guidance**: Learn proper coding techniques
- **Algorithm Help**: Assistance with data structures and algorithms
- **Language Support**: Knowledgeable in C, C++, Python, JavaScript, and more

### Integration Features
- **Popup Responses**: AI responses appear in scrollable popup windows
- **Keyboard Navigation**: Use Up/Down arrows to scroll through responses
- **Easy Dismissal**: Press Esc to close AI response popups
- **Secure API Handling**: Direct HTTPS communication with Anthropic servers

## Security & Privacy

### Data Handling
- **API Key Storage**: Stored locally in `claude-key.txt` file only
- **No Data Caching**: No conversation history is saved locally
- **Direct Communication**: API calls go directly to Anthropic servers via HTTPS
- **Session Only**: AI interactions exist only during current editor session

### File Security
- Keep your `claude-key.txt` file secure and do not commit it to version control
- Add `claude-key.txt` to your `.gitignore` file
- The API key file should contain only the key with no extra whitespace

## Troubleshooting

### Common Issues

1. **"API key not found"**
   - Ensure `claude-key.txt` exists in the same directory as `vizero.exe`
   - Check that the file contains only your API key with no extra spaces or newlines

2. **"Request failed"** 
   - Verify your API key is valid and has sufficient credits
   - Check your internet connection
   - Ensure Anthropic's API is accessible from your network

3. **"No response from AI"**
   - Check that your prompt is not empty
   - Verify your API key has sufficient credits in your Anthropic account
   - Try a simpler prompt to test connectivity

### Getting Help

If you continue to experience issues:
1. Check the Anthropic Console for API usage and credit balance
2. Verify network connectivity to api.anthropic.com
3. Test with a simple prompt like `:claude-chat Hello`

## Example Workflow

```
# Open your code file
:e myprogram.c

# Get AI help with your code
:claude-ask How can I improve this function's performance?

# Have a programming conversation
:claude-chat What are the best practices for error handling in C?

# Ask specific debugging questions
:claude-ask Why might this code cause a memory leak?
```

## Future Enhancements

- **Code Context Awareness**: Send current buffer content with prompts
- **Code Generation**: Direct code insertion from AI responses
- **Project Analysis**: Analyze entire project structure
- **Custom Prompts**: Saved prompt templates for common tasks
- **Multiple Model Support**: Switch between different Claude models
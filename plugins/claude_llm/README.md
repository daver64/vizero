# Claude LLM Plugin for Vizero

This plugin integrates Claude Sonnet 3.5 with Vizero for AI-powered code assistance.

## Quick Setup (Recommended)

1. **Get Claude API Key**
   - Sign up at https://console.anthropic.com/
   - Create an API key in your dashboard

2. **Create API Key File**
   - Create a file named `claude_api_key.txt` in your Vizero directory
   - Paste your API key into this file (just the key, nothing else)
   - Save the file

3. **Test Connection**
   ```
   :llm-test
   ```

## Alternative Setup Methods

### Environment Variable
```cmd
set ANTHROPIC_API_KEY=your_api_key_here
```

### Configuration Files
- **claude_api_key.txt** - Place in Vizero directory (recommended)
- **ANTHROPIC_API_KEY** - Environment variable
- **GITHUB_TOKEN** - For GitHub API compatibility  
- **OPENAI_API_KEY** - For OpenAI API compatibility

## Usage

### Test Connection
```
:llm-test
```

### Ask Questions (Future Enhancement)
```
:llm explain this function
:llm how do I optimize this code?
:llm write unit tests for this
```

*Note: General LLM commands are planned for future versions. Currently only `:llm-test` is available.*

## Configuration

### Environment Variables
- `ANTHROPIC_API_KEY` - Your Claude API key
- `LLM_ENDPOINT` - API endpoint (default: https://api.anthropic.com/v1/messages)
- `LLM_MODEL` - Model name (default: claude-3-5-sonnet-20241022)

### Models Available
- `claude-3-5-sonnet-20241022` - Latest Sonnet (recommended)
- `claude-3-opus-20240229` - Most capable (more expensive)
- `claude-3-haiku-20240307` - Fastest and cheapest

## Cost Estimation

For typical coding usage (100 requests/day):
- **Claude 3.5 Sonnet**: ~$3-8/month
- **Claude 3 Haiku**: ~$1-3/month
- **Claude 3 Opus**: ~$10-25/month

Much more cost-effective than subscription services for occasional use!

## Commands

- `:llm token <api-key>` - Set API token
- `:llm test` - Test connection
- `:llm <prompt>` - Send prompt to Claude

## Troubleshooting

1. **"Request failed"** - Check your API key and internet connection
2. **"Failed to parse JSON"** - Check if Claude API is accessible
3. **No response** - Verify your API key has sufficient credits

## Example Usage

```vim
" In Vizero, select some code and run:
:llm explain this code and suggest improvements

" Ask for help with specific tasks:
:llm write a function to parse JSON safely

" Get code reviews:
:llm review this function for bugs and style issues
```
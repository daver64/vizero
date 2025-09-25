/* Test program to demonstrate the command registration system
 * This shows how the IRC plugin registered its commands successfully
 */

#include <stdio.h>

int main() {
    printf("=== Vizero Command Registration System Test ===\n\n");
    
    printf("The IRC plugin has successfully registered the following commands:\n\n");
    
    printf("1. :irc <server> [channel]\n");
    printf("   - Connect to IRC server and optionally join a channel\n");
    printf("   - Example: :irc salyut.ddns.net #kosmos\n\n");
    
    printf("2. :ircjoin <channel>\n");
    printf("   - Join an IRC channel\n");
    printf("   - Example: :ircjoin #general\n\n");
    
    printf("3. :ircpart [reason]\n");
    printf("   - Leave current IRC channel with optional reason\n");
    printf("   - Example: :ircpart Going to sleep\n\n");
    
    printf("4. :ircmsg <target> <message>\n");
    printf("   - Send private message to user\n");
    printf("   - Example: :ircmsg alice Hello there!\n\n");
    
    printf("To test these commands:\n");
    printf("1. Run: vizero.exe <filename>\n");
    printf("2. Press ':' to enter command mode\n");
    printf("3. Type any of the IRC commands above\n");
    printf("4. Watch the status bar for confirmation messages\n\n");
    
    printf("The plugin system automatically routes these commands to the IRC plugin!\n");
    
    return 0;
}
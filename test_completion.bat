@echo off
echo Starting vizero with completion test...
echo Press Ctrl+Space in the editor, then wait 2 seconds for polling to complete
echo.
timeout /t 15 /nobreak > nul
echo.
echo Vizero should have closed by now. Check the output above for completion polling debug messages.
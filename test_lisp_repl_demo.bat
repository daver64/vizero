REM Lisp REPL Plugin Phase 1 - Test Script
REM Demonstrates SBCL integration with Vizero

@echo off
echo Testing Lisp REPL Plugin Phase 1
echo ================================
echo.

echo 1. Checking SBCL installation...
sbcl\sbcl --version
if %errorlevel% neq 0 (
    echo ERROR: SBCL not found in sbcl\ directory
    echo Please ensure SBCL is installed in the sbcl subdirectory
    pause
    exit /b 1
)
echo.

echo 2. Starting Vizero with test file...
echo Opening test_lisp_repl.lisp for demonstration
echo.
echo Available Lisp REPL commands:
echo   :lisp-connect    - Connect to SBCL REPL
echo   :lisp-status     - Show connection status  
echo   :lisp-eval (expr) - Evaluate Lisp expression
echo   :lisp-package [pkg] - Change/show current package
echo   :lisp-disconnect - Disconnect from SBCL
echo.
echo TIP: Use Ctrl+C to exit Vizero when done testing
echo.
pause

.\vizero.exe test_lisp_repl.lisp
# SLIME Disconnection Test

## Test Procedure

1. **Start Swank Server**:
   ```lisp
   sbcl
   (ql:quickload :swank)
   (swank:create-server :port 4005 :dont-close t)
   ```

2. **Connect via Vizero**:
   ```
   :lisp-slime-connect localhost 4005
   ```

3. **Test Basic Functionality**:
   ```lisp
   (* 3 4)
   (+ 10 5)
   ```

4. **Test Disconnection**:
   ```lisp
   (quit)
   ```

## Expected Behavior

- After `(quit)`, the REPL buffer should display:
  ```
  *** SLIME connection lost - REPL closed ***
  ```
- The buffer should then automatically close
- No hanging or crash should occur
- You should return to your previous editing session

## Fallback Test

If the automatic detection doesn't work, you can manually kill the SBCL process:
- On Windows: Ctrl+C in the SBCL terminal
- The SLIME connection should be detected as lost on the next evaluation attempt

## Verification

The feature is working correctly if:
- [x] Connection loss is detected immediately when SBCL quits
- [x] Disconnect message is displayed in the buffer
- [x] Buffer closes automatically without user intervention
- [x] No crashes or hanging state occurs
- [x] Logging shows "SLIME connection closed" messages
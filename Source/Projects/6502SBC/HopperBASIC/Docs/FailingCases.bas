! Clear Screen to make results capture easier
CLS
NEW
MEM

! Test global BIT declarations and basic operations
FUNC TestGlobalBits()
    PRINT "=== Global BIT Tests ==="
    PRINT "GB1="; GB1; " (expect TRUE)"
    PRINT "GB2="; GB2; " (expect FALSE)"
    GB1 = FALSE: GB2 = TRUE
    PRINT "After assignment: GB1="; GB1; " GB2="; GB2
    PRINT "Use VARS to verify global state"
ENDFUNC

BEGIN
    PRINT "=== BIT Type Test Suite ==="
    TestGlobalBits(): PRINT
END

! Global variable declarations for testing
! Initialized global (resets on each RUN)
BIT GB1 = TRUE  

! Uninitialized global (persists across RUNs)
BIT GB2

! CONST for immutability testing
CONST BIT CB = TRUE

! VAR for runtime typing
VAR VB

! Examine Globals
VARS

RUN

! Look for memory leaks
! NEW
! MEM

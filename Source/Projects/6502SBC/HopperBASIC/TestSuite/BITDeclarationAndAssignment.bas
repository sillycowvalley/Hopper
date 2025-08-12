! Clear Screen to make results capture easier
CLS

! BIT Type Test Suite for Hopper BASIC

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

! Test CONST BIT immutability
FUNC TestConstBits()
    PRINT "=== CONST BIT Tests ==="
    PRINT "CB="; CB; " (expect TRUE)"
    PRINT "Attempting CB=FALSE (should error)..."
    CB = FALSE
    PRINT "CB="; CB; " (should still be TRUE)"
ENDFUNC

! Test VAR with BIT values (runtime typing)
FUNC TestVarBits()
    PRINT "=== VAR BIT Tests ==="
    VB = TRUE
    PRINT "VB="; VB; " (expect TRUE)"
    VB = FALSE  
    PRINT "VB="; VB; " (expect FALSE)"
    PRINT "Check VARS for VAR(BIT) type"
ENDFUNC

! Test local BIT variables and scope
FUNC TestLocalBits()
    BIT LB1 = TRUE
    BIT LB2
    PRINT "=== Local BIT Tests ==="
    PRINT "LB1="; LB1; " LB2="; LB2; " (expect TRUE FALSE)"
    LB1 = FALSE: LB2 = TRUE
    PRINT "After assignment: LB1="; LB1; " LB2="; LB2
ENDFUNC

! Test scope shadowing behavior
FUNC TestShadowing()
    BIT GB1 = FALSE
    PRINT "=== Shadowing Tests ==="
    PRINT "Local GB1="; GB1; " (expect FALSE - shadows global)"
    PRINT "Global should remain unchanged"
ENDFUNC

! Test type safety and invalid operations
FUNC TestTypeSafety()
    PRINT "=== Type Safety Tests ==="
    PRINT "Testing invalid assignment GB1=123..."
    GB1 = 123
    PRINT "GB1="; GB1; " (should show error above)"
ENDFUNC

! Test BIT in expressions and logic
FUNC TestExpressions()
    PRINT "=== Expression Tests ==="
    PRINT "TRUE AND FALSE="; TRUE AND FALSE
    PRINT "TRUE OR FALSE="; TRUE OR FALSE  
    PRINT "NOT TRUE="; NOT TRUE
    PRINT "GB1 AND GB2="; GB1 AND GB2
ENDFUNC

! Test re-initialization behavior
FUNC TestReinit()
    PRINT "=== Re-init Test ==="
    PRINT "Before: GB1="; GB1; " GU="; GU
    GB1 = FALSE: GU = TRUE
    PRINT "Modified: GB1="; GB1; " GU="; GU
    PRINT "Run this test twice with RUN to see difference"
ENDFUNC

BEGIN
    PRINT "=== BIT Type Test Suite ==="
    TestGlobalBits(): PRINT
    TestLocalBits(): PRINT  
    TestExpressions(): PRINT
    TestReinit(): PRINT
    PRINT "=== Error Tests ==="
    TestConstBits(): PRINT
    TestTypeSafety(): PRINT
    TestVarBits(): PRINT
    TestShadowing()
    PRINT "=== Use VARS and MEM to verify ==="
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

RUN

! Look for memory leaks
NEW
MEM

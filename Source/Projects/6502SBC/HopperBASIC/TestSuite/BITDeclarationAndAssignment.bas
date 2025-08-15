! Clear Screen to make results capture easier
CLS

! BIT Type Test Suite for Hopper BASIC

NEW
MEM

! Global variable declarations for testing
! Initialized global (resets on each RUN)
BIT GB1 = TRUE  

! Uninitialized global (persists across RUNs)
BIT GB2

! CONST for immutability testing
CONST BIT CB = TRUE

! VAR for runtime typing
VAR VB


! Test global BIT declarations and basic operations
BEGIN
    PRINT "=== Global BIT Tests ==="
    PRINT "GB1="; GB1; " (expect TRUE)"
    PRINT "GB2="; GB2; " (expect FALSE)"
    GB1 = FALSE: GB2 = TRUE
    PRINT "After assignment: GB1="; GB1; " GB2="; GB2
    PRINT "Use VARS to verify global state"
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test CONST BIT immutability
BEGIN
    PRINT "=== CONST BIT Tests ==="
    PRINT "CB="; CB; " (expect TRUE)"
    PRINT "Attempting CB=FALSE (should error)..."
    CB = FALSE
    PRINT "CB="; CB; " (should still be TRUE)"
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test VAR with BIT values (runtime typing)
BEGIN
    PRINT "=== VAR BIT Tests ==="
    VB = TRUE
    PRINT "VB="; VB; " (expect TRUE)"
    VB = FALSE  
    PRINT "VB="; VB; " (expect FALSE)"
    PRINT "Check VARS for VAR(BIT) type"
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test local BIT variables and scope
BEGIN
    BIT LB1 = TRUE
    BIT LB2
    PRINT "=== Local BIT Tests ==="
    PRINT "LB1="; LB1; " LB2="; LB2; " (expect TRUE FALSE)"
    LB1 = FALSE: LB2 = TRUE
    PRINT "After assignment: LB1="; LB1; " LB2="; LB2
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test scope shadowing behavior
BEGIN
    BIT GB1 = FALSE
    PRINT "=== Shadowing Tests ==="
    PRINT "Local GB1="; GB1; " (expect FALSE - shadows global)"
    PRINT "Global should remain unchanged"
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test type safety and invalid operations
BEGIN
    PRINT "=== Type Safety Tests ==="
    PRINT "Testing invalid assignment GB1=123..."
    GB1 = 123
    PRINT "GB1="; GB1; " (should show error above)"
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test BIT in expressions and logic
BEGIN
    PRINT "=== Expression Tests ==="
    PRINT "TRUE AND FALSE="; TRUE AND FALSE
    PRINT "TRUE OR FALSE="; TRUE OR FALSE  
    PRINT "NOT TRUE="; NOT TRUE
    PRINT "GB1 AND GB2="; GB1 AND GB2
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Test re-initialization behavior
BEGIN
    PRINT "=== Re-init Test ==="
    PRINT "Before: GB1="; GB1; " GB2="; GB2
    GB1 = FALSE: GB2 = TRUE
    PRINT "Modified: GB1="; GB1; " GB2="; GB2
    PRINT "Run this test twice with RUN to see difference"
END
RUN
PRINT "=== Use VARS to verify post-run values==="
VARS


! Look for memory leaks
NEW
MEM

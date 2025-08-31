CLS
! BIT Type Test Suite for Hopper BASIC v3 - Simplified Type System
! Tests VAR with BIT values, CONST BIT, and BIT arrays

NEW
MEM

! Global variable declarations for testing
VAR GB1 = TRUE    ! VAR with BIT literal (type inference)
VAR GB2 = FALSE   ! VAR with BIT literal 
CONST CB = TRUE   ! CONST with BIT literal (type inference)

! Uninitialized VAR (becomes LONG by default)
VAR VB            

! Test global BIT declarations and basic operations
BEGIN
    PRINT "=== Global BIT Tests ==="
    PRINT "GB1="; GB1; " ! expect TRUE"
    PRINT "GB2="; GB2; " ! expect FALSE"
    GB1 = FALSE
    GB2 = TRUE
    PRINT "After assignment: GB1="; GB1; " GB2="; GB2
END
RUN
VARS

! Test CONST BIT immutability
BEGIN
    PRINT "=== CONST BIT Tests ==="
    PRINT "CB="; CB; " ! expect TRUE"
    PRINT "Attempting CB=FALSE (should error)..."
    CB = FALSE
    PRINT "CB="; CB; " ! should still be TRUE"
END
RUN
VARS

! Test VAR with BIT values (runtime typing)
BEGIN
    PRINT "=== VAR BIT Tests ==="
    VB = TRUE
    PRINT "VB="; VB; " ! expect TRUE"
    VB = FALSE  
    PRINT "VB="; VB; " ! expect FALSE"
    PRINT "VB should show as VAR(BIT) in VARS"
END
RUN
VARS

! Test local BIT variables and scope
BEGIN
    VAR LB1 = TRUE
    VAR LB2 = FALSE
    PRINT "=== Local BIT Tests ==="
    PRINT "LB1="; LB1; " LB2="; LB2; " ! expect TRUE FALSE"
    LB1 = FALSE
    LB2 = TRUE
    PRINT "After assignment: LB1="; LB1; " LB2="; LB2
END
RUN
VARS

! Test scope shadowing behavior
BEGIN
    VAR GB1 = FALSE
    PRINT "=== Shadowing Tests ==="
    PRINT "Local GB1="; GB1; " ! expect FALSE (shadows global)"
    PRINT "Global should remain unchanged"
END
RUN
VARS

! Test type safety and invalid operations
BEGIN
    PRINT "=== Type Safety Tests ==="
    PRINT "Testing invalid assignment GB1=123..."
    GB1 = 123
    PRINT "GB1="; GB1; " ! should show error above"
END
RUN
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
VARS

! Test BIT arrays (explicit typing for memory efficiency)
NEW
BIT flags[5]
BEGIN
    PRINT "=== BIT Array Tests ==="
    flags[0] = TRUE
    flags[1] = FALSE
    flags[2] = TRUE
    PRINT "flags[0]="; flags[0]; " ! expect TRUE"
    PRINT "flags[1]="; flags[1]; " ! expect FALSE"
    PRINT "flags[2]="; flags[2]; " ! expect TRUE"
    PRINT "flags[0] AND flags[2]="; flags[0] AND flags[2]
    PRINT "flags[0] OR flags[1]="; flags[0] OR flags[1]
END
RUN

! Test re-initialization behavior
BEGIN
    PRINT "=== Re-init Test ==="
    PRINT "Before: GB1="; GB1; " GB2="; GB2
    GB1 = FALSE
    GB2 = TRUE
    PRINT "Modified: GB1="; GB1; " GB2="; GB2
    PRINT "Run this test twice with RUN to see reset behavior"
END
RUN
VARS

! Test VAR type evolution with BIT
BEGIN
    PRINT "=== VAR Type Evolution ==="
    VAR flex = 42
    PRINT "flex="; flex; " ! LONG value"
    flex = TRUE
    PRINT "flex="; flex; " ! now BIT"
    flex = "STRING"
    PRINT "flex="; flex; " ! now STRING"
    flex = FALSE
    PRINT "flex="; flex; " ! back to BIT"
END
RUN
VARS

NEW
MEM
PRINT "BIT tests complete - check memory vs baseline"

CLS
! LONG Arithmetic Test Suite for Hopper BASIC
! Tests pure LONG-to-LONG operations using VAR declarations
! Uses simplified type system with type inference
! Following progressive isolation methodology

NEW
MEM

! ===== TEST 1: VAR Creation with LONG Type Inference =====
NEW
FUNC TestLongVars()
    VAR l1 = 10000
    VAR l2 = 25000
    VAR l3 = -15000
    VAR l4 = -30000
    PRINT "VAR(LONG) Variables:"
    PRINT l1; " ! expect 10000"
    PRINT l2; " ! expect 25000"
    PRINT l3; " ! expect -15000"
    PRINT l4; " ! expect -30000"
ENDFUNC
BEGIN
    TestLongVars()
END
RUN

! ===== TEST 2: LONG Addition - Positive + Positive =====
NEW
FUNC TestLongAddPos()
    VAR l1 = 10000
    VAR l2 = 20000
    VAR l3 = 5000
    VAR l4 = 15000
    PRINT "LONG Add (+,+):"
    PRINT l1 + l2; " ! expect 30000"
    PRINT l3 + l4; " ! expect 20000"
    PRINT l1 + l3; " ! expect 15000"
ENDFUNC
BEGIN
    TestLongAddPos()
END
RUN

! ===== TEST 3: LONG Addition - Negative + Negative =====
NEW
FUNC TestLongAddNeg()
    VAR l1 = -5000
    VAR l2 = -10000
    VAR l3 = -2000
    VAR l4 = -8000
    PRINT "LONG Add (-,-):"
    PRINT l1 + l2; " ! expect -15000"
    PRINT l3 + l4; " ! expect -10000"
    PRINT l1 + l3; " ! expect -7000"
ENDFUNC
BEGIN
    TestLongAddNeg()
END
RUN

! ===== TEST 4: LONG Addition - Mixed Signs =====
NEW
FUNC TestLongAddMixed()
    VAR l1 = 15000
    VAR l2 = -10000
    VAR l3 = -20000
    VAR l4 = 8000
    PRINT "LONG Add Mixed:"
    PRINT l1 + l2; " ! expect 5000"
    PRINT l3 + l4; " ! expect -12000"
    PRINT l1 + l3; " ! expect -5000"
ENDFUNC
BEGIN
    TestLongAddMixed()
END
RUN

! ===== TEST 5: LONG Subtraction - All Sign Combinations =====
NEW
FUNC TestLongSub()
    VAR l1 = 25000
    VAR l2 = 10000
    VAR l3 = -8000
    VAR l4 = -15000
    PRINT "LONG Subtract:"
    PRINT l1 - l2; " ! expect 15000"
    PRINT l1 - l3; " ! expect 33000"
    PRINT l3 - l1; " ! expect -33000"
    PRINT l3 - l4; " ! expect 7000"
ENDFUNC
BEGIN
    TestLongSub()
END
RUN

! ===== TEST 6: LONG Multiplication - Sign Combinations =====
NEW
FUNC TestLongMul()
    VAR l1 = 100
    VAR l2 = 200
    VAR l3 = -150
    VAR l4 = -80
    PRINT "LONG Multiply:"
    PRINT l1 * l2; " ! expect 20000"
    PRINT l1 * l3; " ! expect -15000"
    PRINT l3 * l2; " ! expect -30000"
    PRINT l3 * l4; " ! expect 12000"
ENDFUNC
BEGIN
    TestLongMul()
END
RUN

! ===== TEST 7: LONG Division - Sign Combinations =====
NEW
FUNC TestLongDiv()
    VAR l1 = 24000
    VAR l2 = 12
    VAR l3 = -18000
    VAR l4 = -9
    PRINT "LONG Divide:"
    PRINT l1 / l2; " ! expect 2000"
    PRINT l1 / l4; " ! expect -2667" 
    PRINT l3 / l2; " ! expect -1500"
    PRINT l3 / l4; " ! expect 2000"
ENDFUNC
BEGIN
    TestLongDiv()
END
RUN

! ===== TEST 8: LONG Modulo - Sign Combinations =====
NEW
FUNC TestLongMod()
    VAR l1 = 23000
    VAR l2 = 7
    VAR l3 = -23000
    VAR l4 = -7
    PRINT "LONG Modulo:"
    PRINT l1 MOD l2; " ! expect 5"
    PRINT l1 MOD l4; " ! expect 5"
    PRINT l3 MOD l2; " ! expect -5"
    PRINT l3 MOD l4; " ! expect -5"
ENDFUNC
BEGIN
    TestLongMod()
END
RUN

! ===== TEST 9: LONG Equality Comparisons =====
NEW
FUNC TestLongEqual()
    VAR l1 = 15000
    VAR l2 = 15000
    VAR l3 = -12000
    VAR l4 = -12000
    VAR l5 = 20000
    PRINT "LONG Equality:"
    PRINT l1 = l2; " ! expect TRUE"
    PRINT l3 = l4; " ! expect TRUE"
    PRINT l1 = l3; " ! expect FALSE"
    PRINT l1 <> l5; " ! expect TRUE"
    PRINT l3 <> l4; " ! expect FALSE"
ENDFUNC
BEGIN
    TestLongEqual()
END
RUN

! ===== TEST 10: LONG Ordering - Positive Numbers =====
NEW
FUNC TestLongOrderPos()
    VAR l1 = 5000
    VAR l2 = 10000
    VAR l3 = 15000
    VAR l4 = 20000
    PRINT "LONG Order (+):"
    PRINT l1 < l2; " ! expect TRUE"
    PRINT l2 > l1; " ! expect TRUE"
    PRINT l3 < l4; " ! expect TRUE"
    PRINT l4 > l3; " ! expect TRUE"
    PRINT l1 <= l1; " ! expect TRUE"
    PRINT l2 >= l2; " ! expect TRUE"
ENDFUNC
BEGIN
    TestLongOrderPos()
END
RUN

! ===== TEST 11: LONG Ordering - Negative Numbers =====
NEW
FUNC TestLongOrderNeg()
    VAR l1 = -20000
    VAR l2 = -10000
    VAR l3 = -15000
    VAR l4 = -5000
    PRINT "LONG Order (-):"
    PRINT l1 < l2; " ! expect TRUE"
    PRINT l2 > l1; " ! expect TRUE"
    PRINT l3 < l4; " ! expect TRUE"
    PRINT l4 > l3; " ! expect TRUE"
    PRINT l1 <= l1; " ! expect TRUE"
    PRINT l3 >= l3; " ! expect TRUE"
ENDFUNC
BEGIN
    TestLongOrderNeg()
END
RUN

! ===== TEST 12: LONG Mixed Sign Ordering =====
NEW
FUNC TestLongOrderMixed()
    VAR l1 = -15000
    VAR l2 = 0
    VAR l3 = 15000
    VAR l4 = -25000
    VAR l5 = 25000
    PRINT "LONG Mixed Order:"
    PRINT l1 < l2; " ! expect TRUE"
    PRINT l2 < l3; " ! expect TRUE"
    PRINT l1 < l3; " ! expect TRUE"
    PRINT l4 < l5; " ! expect TRUE"
    PRINT l3 > l1; " ! expect TRUE"
    PRINT l5 > l4; " ! expect TRUE"
ENDFUNC
BEGIN
    TestLongOrderMixed()
END
RUN

! ===== TEST 13: LONG in BYTE Range (0-255) =====
NEW
FUNC TestLongByteRange()
    VAR l1 = 0
    VAR l2 = 255
    VAR l3 = 128
    VAR l4 = 200
    PRINT "LONG BYTE Range:"
    PRINT l1; " ! expect 0"
    PRINT l2; " ! expect 255"
    PRINT l3; " ! expect 128"
    PRINT l1 + l2; " ! expect 255"
    PRINT l3 * 2; " ! expect 256"
    PRINT l4 - l3; " ! expect 72"
ENDFUNC
BEGIN
    TestLongByteRange()
END
RUN

! ===== TEST 14: LONG in WORD Range (256-65535) =====
NEW
FUNC TestLongWordRange()
    VAR l1 = 1000
    VAR l2 = 32000
    VAR l3 = 50000
    VAR l4 = 2000
    PRINT "LONG WORD Range:"
    PRINT l1; " ! expect 1000"
    PRINT l2; " ! expect 32000"
    PRINT l3; " ! expect 50000"
    PRINT l1 + l4; " ! expect 3000"
    PRINT l2 + l3; " ! expect 82000"
    PRINT l3 - l2; " ! expect 18000"
ENDFUNC
BEGIN
    TestLongWordRange()
END
RUN

! ===== TEST 15: LONG in INT Range (-32768 to 32767) =====
NEW
FUNC TestLongIntRange()
    VAR l1 = 30000
    VAR l2 = -30000
    VAR l3 = 32000
    VAR l4 = -32000
    PRINT "LONG INT Range:"
    PRINT l1; " ! expect 30000"
    PRINT l2; " ! expect -30000"
    PRINT l3; " ! expect 32000"
    PRINT l4; " ! expect -32000"
    PRINT l1 + l2; " ! expect 0"
    PRINT l3 + l4; " ! expect 0"
ENDFUNC
BEGIN
    TestLongIntRange()
END
RUN

! ===== TEST 16: LONG Beyond INT Range =====
NEW
FUNC TestLongBeyondInt()
    VAR l1 = 40000
    VAR l2 = -40000
    VAR l3 = 50000
    VAR l4 = -35000
    PRINT "LONG Beyond INT:"
    PRINT l1; " ! expect 40000"
    PRINT l2; " ! expect -40000"
    PRINT l3; " ! expect 50000"
    PRINT l4; " ! expect -35000"
    PRINT l1 + l3; " ! expect 90000"
    PRINT l2 + l4; " ! expect -75000"
ENDFUNC
BEGIN
    TestLongBeyondInt()
END
RUN

! ===== TEST 17: LONG Complex Expressions =====
NEW
FUNC TestLongComplex()
    VAR l1 = 1000
    VAR l2 = 2000
    VAR l3 = 500
    VAR l4 = 3000
    PRINT "LONG Complex Expr:"
    PRINT l1 + l2 * l3; " ! expect 1001000"
    PRINT l4 - l2 / l3; " ! expect 2996"  
    PRINT l1 * l2 + l3; " ! expect 2000500"
    PRINT l4 + l1 - l2; " ! expect 2000"
ENDFUNC
BEGIN
    TestLongComplex()
END
RUN

! ===== TEST 18: LONG Assignment and Modification =====
NEW
FUNC TestLongAssign()
    VAR l1 = 5000
    VAR l2 = 8000
    PRINT "LONG Assignment:"
    PRINT l1; " ! expect 5000"
    l1 = l2
    PRINT l1; " ! expect 8000"
    l1 = l1 + l2
    PRINT l1; " ! expect 16000"
    l2 = l1 - l2
    PRINT l2; " ! expect 8000"
ENDFUNC
BEGIN
    TestLongAssign()
END
RUN

! ===== TEST 19: VAR Type Inference Test =====
NEW
FUNC TestVarInference()
    VAR v1 = 12000    ! Inferred as LONG
    VAR v2 = -8000    ! Inferred as LONG
    VAR v3 = 0        ! Inferred as LONG
    PRINT "VAR Type Inference:"
    PRINT v1; " ! expect 12000"
    PRINT v2; " ! expect -8000" 
    PRINT v3; " ! expect 0"
    VAR sum = v1 + v2 + v3
    PRINT sum; " ! expect 4000"
ENDFUNC
BEGIN
    TestVarInference()
END
RUN

! ===== TEST 20: LONG Edge Values =====
NEW
FUNC TestLongEdges()
    VAR l1 = 1
    VAR l2 = -1
    VAR l3 = 0
    VAR l4 = 32767
    VAR l5 = -32768
    VAR l6 = 65535
    PRINT "LONG Edge Values:"
    PRINT l1; " ! expect 1"
    PRINT l2; " ! expect -1"
    PRINT l3; " ! expect 0"
    PRINT l4; " ! expect 32767"
    PRINT l5; " ! expect -32768"
    PRINT l6; " ! expect 65535"
ENDFUNC
BEGIN
    TestLongEdges()
END
RUN

! ===== TEST 21: LONG Large Values =====
NEW
FUNC TestLongLarge()
    VAR l1 = 100000
    VAR l2 = 500000
    VAR l3 = 1000000
    VAR l4 = -1000000
    PRINT "LONG Large Values:"
    PRINT l1; " ! expect 100000"
    PRINT l2; " ! expect 500000"
    PRINT l3; " ! expect 1000000"
    PRINT l4; " ! expect -1000000"
ENDFUNC
BEGIN
    TestLongLarge()
END
RUN

! ===== TEST 22: LONG Overflow Detection =====
NEW
FUNC TestLongOverflow()
    VAR big1 = 1000000
    VAR big2 = 2000
    VAR big3 = 1500000  
    VAR big4 = 1600
    PRINT "LONG Overflow Tests:"
    PRINT "1000000 * 2000:"
    VAR result1 = big1 * big2
    PRINT result1; " ! should be 2000000000 (if no overflow)"
    
    PRINT "1500000 * 1600:"
    VAR result2 = big3 * big4  
    PRINT result2; " ! should be 2400000000 (if no overflow)"
ENDFUNC
BEGIN
    TestLongOverflow()
END
RUN

! ===== TEST 23: Memory Check =====
NEW
MEM
PRINT "LONG arithmetic test suite complete"
PRINT "All tests use VAR with LONG type inference"

CLS
! IF Statement Test Suite for Hopper BASIC
! Tests all multiline IF variants and edge cases using simplified type system
! All numeric variables use VAR with type inference
! Following progressive isolation methodology

NEW
MEM

! ===== TEST 1: Basic IF/THEN/ENDIF =====
NEW
FUNC TestBasicIf()
    VAR x = 5
    VAR result = FALSE
    IF x = 5 THEN
        result = TRUE
        PRINT "Inside IF block"
    ENDIF
    PRINT "result="; result; " ! expect TRUE"
    PRINT "Basic IF test complete"
ENDFUNC
BEGIN
    TestBasicIf()
END
RUN

! ===== TEST 2: IF/THEN/ELSE/ENDIF =====
NEW
FUNC TestIfElse()
    VAR x = 3
    VAR msg = ""
    IF x > 5 THEN
        msg = "GREATER"
        PRINT "In THEN branch"
    ELSE
        msg = "SMALLER"
        PRINT "In ELSE branch"
    ENDIF
    PRINT "msg="; msg; " ! expect SMALLER"
ENDFUNC
BEGIN
    TestIfElse()
END
RUN

! ===== TEST 3: Multiple Statements in THEN Block =====
NEW
FUNC TestMultipleThen()
    VAR count = 0
    VAR flag = FALSE
    IF TRUE THEN
        count = count + 1
        flag = TRUE
        count = count * 2
        PRINT "count="; count; " ! expect 2"
    ENDIF
    PRINT "flag="; flag; " ! expect TRUE"
ENDFUNC
BEGIN
    TestMultipleThen()
END
RUN

! ===== TEST 4: Multiple Statements in ELSE Block =====
NEW
FUNC TestMultipleElse()
    VAR total = 10
    VAR status = "INIT"
    IF FALSE THEN
        total = 999
        status = "WRONG"
    ELSE
        total = total * 3
        status = "CORRECT"
        total = total + 5
    ENDIF
    PRINT "total="; total; " ! expect 35"
    PRINT "status="; status; " ! expect CORRECT"
ENDFUNC
BEGIN
    TestMultipleElse()
END
RUN

! ===== TEST 5: Multiple Statements in Both Blocks =====
NEW
FUNC TestBothBlocks()
    VAR x = 7
    VAR result1 = 0
    VAR result2 = 0
    IF x > 5 THEN
        result1 = x * 2
        result2 = result1 + 10
        PRINT "THEN: r1="; result1; " r2="; result2
    ELSE
        result1 = x / 2
        result2 = result1 - 5
        PRINT "ELSE: r1="; result1; " r2="; result2
    ENDIF
    PRINT "Final: "; result1; " "; result2; " ! expect 14 24"
ENDFUNC
BEGIN
    TestBothBlocks()
END
RUN

! ===== TEST 6: Nested IF Statements (Simple) =====
NEW
FUNC TestNestedSimple()
    VAR x = 8
    VAR y = 3
    VAR result = "NONE"
    IF x > 5 THEN
        IF y < 5 THEN
            result = "BOTH_TRUE"
        ELSE
            result = "X_ONLY"
        ENDIF
    ELSE
        result = "X_FALSE"
    ENDIF
    PRINT "result="; result; " ! expect BOTH_TRUE"
ENDFUNC
BEGIN
    TestNestedSimple()
END
RUN

! ===== TEST 7: Nested IF in ELSE Branch =====
NEW
FUNC TestNestedInElse()
    VAR score = 45
    VAR grade = ""
    IF score >= 90 THEN
        grade = "A"
    ELSE
        IF score >= 70 THEN
            grade = "B"
        ELSE
            IF score >= 50 THEN
                grade = "C"
            ELSE
                grade = "F"
            ENDIF
        ENDIF
    ENDIF
    PRINT "grade="; grade; " ! expect F"
ENDFUNC
BEGIN
    TestNestedInElse()
END
RUN

! ===== TEST 8: Complex Boolean Expressions =====
NEW
FUNC TestComplexBoolean()
    VAR a = 5
    VAR b = 10
    VAR c = TRUE
    VAR result = "DEFAULT"
    IF a < b AND c = TRUE THEN
        result = "CONDITION1"
        IF a + b > 12 THEN
            result = "CONDITION2"
        ENDIF
    ELSE
        result = "FAILED"
    ENDIF
    PRINT "result="; result; " ! expect CONDITION2"
ENDFUNC
BEGIN
    TestComplexBoolean()
END
RUN

! ===== TEST 9: IF with Comparison Operations =====
NEW
FUNC TestComparisons()
    VAR w1 = 100
    VAR w2 = 200
    VAR count = 0
    IF w1 < w2 THEN
        count = count + 1
    ENDIF
    IF w1 <> w2 THEN
        count = count + 1
    ENDIF
    IF w2 >= w1 THEN
        count = count + 1
    ENDIF
    PRINT "count="; count; " ! expect 3"
ENDFUNC
BEGIN
    TestComparisons()
END
RUN

! ===== TEST 10: IF with CHAR Comparisons =====
NEW
FUNC TestCharIf()
    VAR c1 = 'A'
    VAR c2 = 'Z'
    VAR result = "DEFAULT"
    IF c1 = 'A' THEN
        result = "MATCH_A"
        IF c2 > c1 THEN
            result = "Z_GREATER"
        ENDIF
    ENDIF
    PRINT "result="; result; " ! expect Z_GREATER"
ENDFUNC
BEGIN
    TestCharIf()
END
RUN

! ===== TEST 11: IF with String Comparisons =====
NEW
FUNC TestStringIf()
    VAR s1 = "HELLO"
    VAR s2 = "WORLD"
    VAR matches = 0
    IF s1 = "HELLO" THEN
        matches = matches + 1
    ENDIF
    IF s2 <> s1 THEN
        matches = matches + 1
    ENDIF
    IF s1 <> "" THEN
        matches = matches + 1
    ENDIF
    PRINT "matches="; matches; " ! expect 3"
ENDFUNC
BEGIN
    TestStringIf()
END
RUN

! ===== TEST 12: IF with Function Calls =====
NEW
FUNC IsEven(n)
    RETURN n MOD 2 = 0
ENDFUNC
FUNC TestFunctionIf()
    VAR num = 8
    VAR parity = "UNKNOWN"
    IF IsEven(num) THEN
        parity = "EVEN"
        PRINT "Number is even"
    ELSE
        parity = "ODD"
        PRINT "Number is odd"
    ENDIF
    PRINT "parity="; parity; " ! expect EVEN"
ENDFUNC
BEGIN
    TestFunctionIf()
END
RUN

! ===== TEST 13: IF with Built-in Functions =====
NEW
FUNC TestBuiltinIf()
    VAR text = "BASIC"
    VAR first = 'X'
    VAR length = 0
    IF LEN(text) > 3 THEN
        first = text[0]
        length = LEN(text)
        PRINT "Text is long enough"
    ENDIF
    PRINT "first="; first; " ! expect B"
    PRINT "length="; length; " ! expect 5"
ENDFUNC
BEGIN
    TestBuiltinIf()
END
RUN

! ===== TEST 14: IF with VAR Type Evolution =====
NEW
FUNC TestVarIf()
    VAR v = 42
    VAR result = "INIT"
    IF v = 42 THEN
        result = "INT_MATCH"
        v = "STRING_VAL"
        IF v = "STRING_VAL" THEN
            result = "STRING_MATCH"
        ENDIF
    ENDIF
    PRINT "result="; result; " ! expect STRING_MATCH"
ENDFUNC
BEGIN
    TestVarIf()
END
RUN

! ===== TEST 15: Deep Nesting Test (No Concatenation) =====
NEW
FUNC TestDeepNesting()
    VAR x = 3
    VAR path = ""
    IF x > 0 THEN
        path = "A"
        IF x > 1 THEN
            path = "AB"
            IF x > 2 THEN
                path = "ABC"
                IF x > 3 THEN
                    path = "ABCD"
                ELSE
                    path = "ABCE"
                ENDIF
            ENDIF
        ENDIF
    ENDIF
    PRINT "path="; path; " ! expect ABCE"
ENDFUNC
BEGIN
    TestDeepNesting()
END
RUN

! ===== TEST 16: IF with Local vs Global Variables =====
NEW
VAR globalCounter = 0
FUNC TestScopeIf()
    VAR localCounter = 5
    IF localCounter > globalCounter THEN
        globalCounter = 10
        localCounter = 20
        PRINT "Modified both counters"
    ENDIF
    PRINT "local="; localCounter; " ! expect 20"
ENDFUNC
BEGIN
    TestScopeIf()
    PRINT "global="; globalCounter; " ! expect 10"
END
RUN

! ===== TEST 17: IF with RETURN Statements =====
NEW
FUNC EarlyReturn(flag)
    IF flag THEN
        PRINT "Returning early"
        RETURN 999
    ENDIF
    PRINT "Normal path"
    RETURN 111
ENDFUNC
FUNC TestReturnIf()
    VAR result1 = EarlyReturn(TRUE)
    VAR result2 = EarlyReturn(FALSE)
    PRINT "r1="; result1; " r2="; result2; " ! expect 999 111"
ENDFUNC
BEGIN
    TestReturnIf()
END
RUN

! ===== TEST 18: IF with PRINT Variations =====
NEW
FUNC TestPrintIf()
    VAR verbose = TRUE
    VAR value = 42
    IF verbose THEN
        PRINT "Verbose mode: value ="; value
        PRINT "Additional info: "; value * 2
        PRINT
        PRINT "End of verbose block"
    ELSE
        PRINT value
    ENDIF
    PRINT "Test complete"
ENDFUNC
BEGIN
    TestPrintIf()
END
RUN

! ===== TEST 19: Edge Case - Empty THEN/ELSE Blocks =====
NEW
FUNC TestEmptyBlocks()
    VAR flag = TRUE
    VAR result = "BEFORE"
    IF flag THEN
        ! Empty THEN block
    ELSE
        result = "ELSE_EXECUTED"
    ENDIF
    IF NOT flag THEN
        result = "WRONG"
    ELSE
        ! Empty ELSE block  
    ENDIF
    result = "AFTER"
    PRINT "result="; result; " ! expect AFTER"
ENDFUNC
BEGIN
    TestEmptyBlocks()
END
RUN

! ===== Memory Leak Check =====
NEW
MEM
PRINT "IF statement tests complete - check for memory leaks"

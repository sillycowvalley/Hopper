CLS
! IF Statement Test Suite for Hopper BASIC
! Tests all multiline IF variants and edge cases
! Following progressive isolation methodology
! Each test is under 400 characters to stay within 512-byte limit

NEW
MEM

! ===== TEST 1: Basic IF/THEN/ENDIF (Isolated) =====
NEW
FUNC TestBasicIf()
    INT x = 5
    BIT result = FALSE
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

! ===== TEST 2: IF/THEN/ELSE/ENDIF (Isolated) =====
NEW
FUNC TestIfElse()
    INT x = 3
    STRING msg = ""
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
    INT count = 0
    BIT flag = FALSE
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
    INT total = 10
    STRING status = "INIT"
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
    INT x = 7
    INT result1 = 0
    INT result2 = 0
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
    INT x = 8
    INT y = 3
    STRING result = "NONE"
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
    INT score = 45
    STRING grade = ""
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
    INT a = 5
    INT b = 10
    BIT c = TRUE
    STRING result = "DEFAULT"
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
    WORD w1 = 100
    WORD w2 = 200
    INT count = 0
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
    CHAR c1 = 'A'
    CHAR c2 = 'Z'
    STRING result = "DEFAULT"
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
    STRING s1 = "HELLO"
    STRING s2 = "WORLD"
    INT matches = 0
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
    INT num = 8
    STRING parity = "UNKNOWN"
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
    STRING text = "BASIC"
    CHAR first = 'X'
    INT length = 0
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
    STRING result = "INIT"
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

! ===== TEST 15: Deep Nesting Test =====
NEW
FUNC TestDeepNesting()
    INT x = 3
    STRING path = ""
    IF x > 0 THEN
        path = "A"
        IF x > 1 THEN
            path = path + "B"
            IF x > 2 THEN
                path = path + "C"
                IF x > 3 THEN
                    path = path + "D"
                ELSE
                    path = path + "E"
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
INT global_counter = 0
FUNC TestScopeIf()
    INT local_counter = 5
    IF local_counter > global_counter THEN
        global_counter = 10
        local_counter = 20
        PRINT "Modified both counters"
    ENDIF
    PRINT "local="; local_counter; " ! expect 20"
ENDFUNC
BEGIN
    TestScopeIf()
    PRINT "global="; global_counter; " ! expect 10"
END
RUN

! ===== TEST 17: IF with Colon-Separated Statements =====
NEW
FUNC TestColonStatements()
    INT x = 7
    INT a = 0 : INT b = 0
    IF x > 5 THEN a = 1 : b = 2 : PRINT "Colon THEN"
    ELSE a = 3 : b = 4 : PRINT "Colon ELSE"
    ENDIF
    PRINT "a="; a; " b="; b; " ! expect 1 2"
ENDFUNC
BEGIN
    TestColonStatements()
END
RUN

! ===== TEST 18: IF with RETURN Statements =====
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
    INT result1 = EarlyReturn(TRUE)
    INT result2 = EarlyReturn(FALSE)
    PRINT "r1="; result1; " r2="; result2; " ! expect 999 111"
ENDFUNC
BEGIN
    TestReturnIf()
END
RUN

! ===== TEST 19: IF with PRINT Variations =====
NEW
FUNC TestPrintIf()
    BIT verbose = TRUE
    INT value = 42
    IF verbose THEN
        PRINT "Verbose mode: value ="; value
        PRINT "Additional info: "; value * 2
        PRINT ! Empty line
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

! ===== TEST 20: Edge Case - Empty THEN/ELSE Blocks =====
NEW
FUNC TestEmptyBlocks()
    BIT flag = TRUE
    STRING result = "BEFORE"
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

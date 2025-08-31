NEW
! Small focused tests for local variables - run individually

! Test 1: Basic LONG local
FUNC TestLong()
    VAR num = 42
    PRINT "num="; num
    num = 999
    PRINT "changed="; num
ENDFUNC

! Test 2: Basic CHAR local  
FUNC TestChar()
    VAR ch = 'X'
    PRINT "ch="; ch
    ch = 'Y'
    PRINT "changed="; ch
ENDFUNC

! Test 3: Basic BIT local
FUNC TestBit()
    VAR flag = TRUE
    PRINT "flag="; flag
    flag = FALSE
    PRINT "changed="; flag
ENDFUNC

! Test 4: Basic STRING local
FUNC TestString()
    VAR text = "LOCAL"
    PRINT "text="; text
    text = "CHANGED"
    PRINT "changed="; text
ENDFUNC

! Test 5: Uninitialized local
FUNC TestUninit()
    VAR empty
    PRINT "empty="; empty
    empty = -555
    PRINT "set="; empty
ENDFUNC

! Run tests individually:
BEGIN
    PRINT "1. LONG local test:"
    TestLong()
END
RUN

BEGIN
    PRINT "2. CHAR local test:"  
    TestChar()
END
RUN

BEGIN
    PRINT "3. BIT local test:"
    TestBit()
END
RUN

BEGIN
    PRINT "4. STRING local test:"
    TestString()
END  
RUN

BEGIN
    PRINT "5. Uninitialized test:"
    TestUninit()
END
RUN

NEW
! Test local reset behavior
FUNC TestReset()
    VAR counter = 0
    PRINT "counter="; counter
    counter = counter + 1
    PRINT "incremented="; counter
ENDFUNC

BEGIN
    PRINT "6. Reset test (call twice):"
    TestReset()
    TestReset()
END
RUN

NEW  
! Test parameters
FUNC TestParam(value)
    PRINT "param="; value
    value = 777
    PRINT "modified="; value
ENDFUNC

VAR original = 123

BEGIN
    PRINT "7. Parameter test:"
    TestParam(original)
    PRINT "original still="; original
END
RUN

NEW
! Test multiple parameters and return
FUNC TestParams(a, b)
    PRINT "a="; a; " b="; b
    RETURN a + b
ENDFUNC

BEGIN
    PRINT "8. Multiple params:"
    VAR sum = TestParams(10, 20) 
    PRINT "returned="; sum
END
RUN

NEW
! Test VAR type changes
FUNC TestVarType()
    VAR flex = 42
    PRINT "start="; flex
    flex = "STRING"  
    PRINT "string="; flex
    flex = TRUE
    PRINT "bit="; flex
ENDFUNC

BEGIN
    PRINT "9. VAR type changes:"
    TestVarType()
END
RUN

NEW
! Test simple recursion
FUNC TestRecur(n)
    PRINT "depth="; n
    IF n > 0 THEN
        RETURN TestRecur(n-1) + 1
    ENDIF
    RETURN 0
ENDFUNC

BEGIN
    PRINT "10. Simple recursion:"
    VAR result = TestRecur(2)
    PRINT "result="; result
END
RUN

NEW
! Test array parameters
BIT flags[3]
CHAR letters[3]  
INT numbers[3]

FUNC ModifyArrays(f, l, n)
    f[0] = TRUE
    l[0] = 'A'
    n[0] = 100
    PRINT "Arrays modified"
ENDFUNC

BEGIN
    PRINT "11. Array parameters:"
    PRINT "Before: flags[0]="; flags[0]
    PRINT "Before: letters[0]="; letters[0] 
    PRINT "Before: numbers[0]="; numbers[0]
    
    ModifyArrays(flags, letters, numbers)
    
    PRINT "After: flags[0]="; flags[0]
    PRINT "After: letters[0]="; letters[0]
    PRINT "After: numbers[0]="; numbers[0]
END
RUN

NEW
! Test scope isolation
VAR global = 999

FUNC TestScope()
    VAR global = 123
    PRINT "local global="; global
ENDFUNC

BEGIN
    PRINT "12. Scope isolation:"
    PRINT "before="; global
    TestScope() 
    PRINT "after="; global
END
RUN

NEW
! Test many small locals
FUNC ManyLocals()
    VAR a = 1
    VAR b = 2
    VAR c = 3
    VAR d = 4
    VAR e = 5
    PRINT "sum="; a + b + c + d + e
ENDFUNC

BEGIN
    PRINT "13. Many locals:"
    ManyLocals()
END
RUN

MEM
NEW

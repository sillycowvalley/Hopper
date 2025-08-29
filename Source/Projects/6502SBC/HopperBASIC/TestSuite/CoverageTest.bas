! Hopper BASIC Complete Coverage Test
! Exercises every language feature for profiling

CLS
NEW
MEM

! Variable declarations - all types
VAR n = 42, ch = 'A', b = TRUE, s = "test"
VAR n2 = -1000000, ch2 = 'z', b2 = FALSE, s2 = ""
CONST K1 = 100
CONST K2 = 50
CONST K3 = 'Q'
CONST K4 = TRUE
CONST K5 = "const"

! Array declarations - all types
BIT flags[5]
CHAR buf[10]
BYTE bytes[8]
WORD words[4]
INT ints[6]

! Test all operators
FUNC TestOps()
    VAR a = 10, b = 3, r
    ! Arithmetic
    r = a + b
    r = a - b
    r = a * b
    r = a / b
    r = a MOD b
    r = a / 10
    r = a / 100
    r = a / 50
    r = a / 16
    r = a / 8
    r = a / 4
    r = a / 2
    r = a / 1
    r = a * 0
    r = a * 1
    r = a * 2
    r = a * 4
    r = a * 10
    r = a * 16
    
    r = -a
    r = -b
    ! Bitwise
    r = a & b
    r = a | b
    r = 15 & 7
    r = 8 | 4
    ! Comparison with LONG
    VAR t
    t = a = b
    t = a <> b
    t = a < b
    t = a > b
    t = a <= b
    t = a >= b
    t = 5 = 5
    t = 10 <> 20
    ! Comparison with CHAR
    VAR c1 = 'A', c2 = 'B'
    t = c1 = c2
    t = c1 <> c2
    t = c1 < c2
    t = c1 > c2
    t = c1 <= c2
    t = c1 >= c2
    ! Comparison with BIT
    VAR p = TRUE, q = FALSE
    t = p = q
    t = p <> q
    t = TRUE = TRUE
    t = FALSE <> TRUE
    ! Comparison with STRING
    VAR s1 = "abc", s2 = "xyz"
    t = s1 = s2
    t = s1 <> s2
    t = "same" = "same"
    ! Logical
    t = p AND q
    t = p OR q
    t = NOT p
    t = NOT q
    t = TRUE AND FALSE
    t = TRUE OR FALSE
    t = NOT TRUE
    t = NOT FALSE
    ! Complex expressions
    r = (a + b) * 2
    r = ((a - b) * (a + b)) / 2
    t = (a > 5) AND (b < 10)
    t = NOT (p OR q)
ENDFUNC
TestOps()
FORGET TestOps

! Test all control flow
FUNC TestFlow()
    VAR x = 5
    VAR start = 2, finish = 8
    VAR w = 5
    VAR d = 3
    ! Simple IF
    IF x > 0 THEN
        x = x + 1
    ENDIF
    ! IF with ELSE
    IF x < 0 THEN
        x = 0
    ELSE
        x = x - 1
    ENDIF
    ! Nested IF
    IF x > 0 THEN
        IF x < 10 THEN
            x = x * 2
        ELSE
            x = x / 2
        ENDIF
    ENDIF
    ! Empty blocks
    IF FALSE THEN
    ENDIF
    IF TRUE THEN
    ELSE
    ENDIF
    ! FOR loops forward
    FOR i = 1 TO 10
        x = x + 1
    NEXT i
    FOR j = 0 TO 5
        x = x - 1
    NEXT j
    ! FOR loops backward
    FOR k = 10 TO 1 STEP -1
        x = x + 1
    NEXT k
    FOR m = 100 TO 50 STEP -5
        x = x + 1
    NEXT m
    ! FOR with expressions
    
    FOR n = start TO finish
        x = x + 1
    NEXT n
    
    FOR n = -1 TO 1
        x = x + 1
    NEXT n
    
    
    ! WHILE loops
    
    WHILE w > 0
        w = w - 1
    WEND
    WHILE FALSE
        x = 999
    WEND
    ! DO UNTIL loops
    
    DO
        d = d - 1
    UNTIL d = 0
    DO
        x = x + 1
    UNTIL TRUE
ENDFUNC
TestFlow()
FORGET TestFlow

! Test all array operations
FUNC TestArrays()
    ! BIT array
    flags[0] = TRUE
    flags[1] = FALSE
    flags[2] = TRUE
    VAR f0 = flags[0]
    VAR f1 = flags[1]
    ! CHAR array
    buf[0] = 'H'
    buf[1] = 'i'
    buf[9] = '!'
    VAR c0 = buf[0]
    VAR c9 = buf[9]
    ! BYTE array
    bytes[0] = 0
    bytes[1] = 128
    bytes[2] = 255
    VAR b0 = bytes[0]
    VAR b2 = bytes[2]
    ! WORD array
    words[0] = 0
    words[1] = 32768
    words[2] = 65535
    VAR w0 = words[0]
    VAR w2 = words[2]
    ! INT array
    ints[0] = -32768
    ints[1] = 0
    ints[2] = 32767
    VAR i0 = ints[0]
    VAR i2 = ints[2]
    ! Array with variables as index
    VAR idx = 3
    bytes[idx] = 100
    VAR bx = bytes[idx]
    ! LEN function on arrays
    VAR lf = LEN(flags)
    VAR lb = LEN(buf)
    VAR lby = LEN(bytes)
    VAR lw = LEN(words)
    VAR li = LEN(ints)
ENDFUNC
TestArrays()
FORGET TestArrays

! Test string operations
FUNC TestStrings()
    VAR str1 = "Hello"
    VAR str2 = "World"
    VAR str3 = ""
    VAR str4 = "A"
    ! String indexing
    VAR c0 = str1[0]
    VAR c1 = str1[1]
    VAR c4 = str1[4]
    ! String comparison
    VAR eq1 = str1 = str2
    VAR ne1 = str1 <> str2
    VAR eq2 = str1 = "Hello"
    VAR ne2 = str1 <> "Goodbye"
    ! LEN on strings
    VAR l1 = LEN(str1)
    VAR l2 = LEN(str2)
    VAR l3 = LEN(str3)
    VAR l4 = LEN(str4)
    VAR l5 = LEN("literal")
ENDFUNC
TestStrings()
FORGET TestStrings

! Test all built-in functions
FUNC TestBuiltins1()
    VAR t = MILLIS()
    t = SECONDS()
    DELAY(50)
    ! ASC function
    VAR a1 = ASC('A')
    VAR a2 = ASC('Z')
    VAR a3 = ASC('0')
    VAR a4 = ASC(' ')
    VAR cx = 'x'
    VAR a5 = ASC(cx)
    ! CHR function
    VAR c1 = CHR(65)
    VAR c2 = CHR(90)
    VAR c3 = CHR(48)
    VAR c4 = CHR(32)
    VAR nx = 120
    VAR c5 = CHR(nx)
    ! ABS function
    VAR ab1 = ABS(10)
    VAR ab2 = ABS(-10)
    VAR ab3 = ABS(0)
    VAR ab4 = ABS(-999)
    VAR negv = -42
    VAR ab5 = ABS(negv)
ENDFUNC
TestBuiltins1()
FORGET TestBuiltins1
FUNC TestBuiltins2()
    ! RND function
    VAR r1 = RND(10)
    VAR r2 = RND(100)
    VAR r3 = RND(1000)
    VAR r4 = RND(1)
    VAR maxr = 50
    VAR r5 = RND(maxr)
    ! Memory functions
    VAR p1 = PEEK(0x00E0)
    VAR p2 = PEEK(0x00E1)
    POKE(0x00E0, 42)
    POKE(0x00E1, 255)
    POKE(0x00E2, 0)
    VAR p3 = PEEK(0x00E0)
    ! I/O functions
    PINMODE(0, 1)
    WRITE(1, TRUE)
    WRITE(0, FALSE)
    VAR rd1 = READ(0)
    VAR rd2 = READ(1)
    
    ! I2C
    VAR success = I2CFIND(0x50)
    I2CBEGIN(0x50)
    I2CPUT(0)
    I2CEND()
    I2CGET(0x50, 128)
    I2CNEXT()
ENDFUNC
TestBuiltins2()
FORGET TestBuiltins2

! Test all PRINT variants
FUNC TestPrint()
    PRINT
    PRINT "string literal"
    PRINT 42
    PRINT -100
    PRINT TRUE
    PRINT FALSE
    PRINT 'X'
    ! Comma separator
    PRINT 1, 2, 3
    PRINT "a", "b", "c"
    ! Semicolon separator
    PRINT 1; 2; 3
    PRINT "a"; "b"; "c"
    ! Mixed
    PRINT "Value:", 42, "OK"
    PRINT "X="; 10; " Y="; 20
    ! Trailing comma
    PRINT "No newline",
    PRINT "continues"
    ! Trailing semicolon
    PRINT "No space";
    PRINT "tight"
    ! Variables
    VAR pv = 999
    VAR ps = "test"
    PRINT pv
    PRINT ps
    PRINT pv, ps
    PRINT "pv="; pv; " ps="; ps
ENDFUNC

! Test INPUT
FUNC TestInput()
    PRINT "Press Enter:";
    VAR inp = INPUT()
    PRINT "Got:"; inp
ENDFUNC

! Function with parameters
FUNC FuncParam(x)
    VAR local = x * 2
    VAR local2 = x + 10
    RETURN local + local2
ENDFUNC

! Function with multiple parameters
FUNC FuncMulti(a, b, c)
    VAR sum = a + b + c
    RETURN sum
ENDFUNC

! Function with array parameter
FUNC FuncArray(arr)
    arr[0] = 999
    arr[1] = 888
    VAR v = arr[2]
    RETURN v
ENDFUNC

! Recursive function
FUNC Factorial(n)
    VAR prev
    IF n <= 1 THEN
        RETURN 1
    ENDIF
    prev = Factorial(n - 1)
    RETURN n * prev
ENDFUNC

! Nested function calls
FUNC Inner(x)
    RETURN x + 1
ENDFUNC

FUNC Middle(x)
    VAR r = Inner(x)
    RETURN r * 2
ENDFUNC

FUNC Outer(x)
    VAR r = Middle(x)
    RETURN r - 5
ENDFUNC

! Test comments
FUNC TestComments()
    REM This is a REM comment
    REM Another REM comment
    ! This is a bang comment
    ! Another bang comment
    VAR x = 1
    VAR y = 2
    REM Mixed comment types
    ! Both work
ENDFUNC

! Empty function
FUNC Empty()
ENDFUNC

! Function to be forgotten
FUNC ToForget()
    PRINT "Will be forgotten"
ENDFUNC

! Test FUNCS before FORGET
FUNCS

! Test LIST
LIST
LIST ToForget


! Main program
FUNC Main()
    PRINT "=== COVERAGE TEST START ==="
    
    ! Test function calls
    VAR r1 = FuncParam(10)
    PRINT "FuncParam:"; r1
    
    VAR r2 = FuncMulti(10, 20, 30)
    PRINT "FuncMulti:"; r2
    
    FuncArray(ints)
    PRINT "After FuncArray:"; ints[0]; ints[1]
    
    VAR r3 = Factorial(5)
    PRINT "Factorial:"; r3
    
    VAR r4 = Outer(7)
    PRINT "Nested:"; r4
    
    ! Test constants
    PRINT "Constants:"; K1; K2; K3; K4; K5
    
    ! Test variables
    PRINT "Variables:"; n; ch; b; s
    
    PRINT "=== COVERAGE TEST END ==="
ENDFUNC

MEM
VARS
FUNCS
VAR temp = 999
VAR after = 111


TestPrint()
FORGET TestPrint
TestInput()
FORGET TestInput
TestComments()
FORGET TestComments
Empty()
FORGET Empty

! Test FORGET
FORGET ToForget

! Test FUNCS after FORGET
FUNCS


! Run everything
Main()
NEW

PRINT
PRINT
PRINT



! Errors


VAR G = 10
G = G + 1
G = G / 0
H = 0
BEGIN
    FOR I = -1 TO 1
    NEXT I
    VAR A
END
RUN
BYTE B[10]
B[11] = 0
B[-1] = 0
PRINT "A" + 10




! Test file operations
FUNC MyFunc(A,B)
    PRINT A, B
ENDFUNC
BEGIN
    MyFunc("Hello", "World")
END

FORMAT
Y
SAVE coverage

! errors
SAVE
SAVE coverage coverage
SAVE "coverage"
DIR
LOAD coverage
! load and run
coverage
DEL coverage

! Final cleanup
CLEAR
NEW

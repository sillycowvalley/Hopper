CLS
! ================================================================
! CORRECTED TRAILING COMMENTS TEST SUITE
! Properly separates REPL-level vs function/program contexts
! Tests trailing ! comments in appropriate contexts only
! ================================================================

NEW

! ===== SECTION 1: REPL-LEVEL SINGLE STATEMENTS =====
! Only test statements that work directly at REPL prompt

! 1.1 System commands
LIST ! comment
CLEAR ! comment  
NEW ! comment
MEM ! comment
VARS ! comment
FUNCS ! comment
DIR ! comment

! 1.2 System commands with identifiers (not quoted strings)
VAR dummyFile = 0
SAVE dummyFile ! comment
LOAD dummyFile ! comment  
DEL dummyFile ! comment
FORGET dummyFile ! comment

! 1.3 Variable declarations  
VAR testVar1 ! comment
VAR testVar2 = 42 ! comment
VAR testVar3 = 'A' ! comment
VAR testVar4 = TRUE ! comment
VAR testVar5 = "TEXT" ! comment

! 1.4 Multiple variable declarations
VAR a, b, c ! comment
VAR x = 1, y = 2, z = 3 ! comment

! 1.5 Constant declarations
CONST testConst = 100 ! comment
CONST testChar = 'Z' ! comment

! 1.6 Array declarations
BIT testBits[5] ! comment
CHAR testChars[10] ! comment
INT testInts[8] ! comment

! 1.7 Variable assignments
testVar1 = 999 ! comment
testBits[0] = TRUE ! comment
testChars[0] = 'X' ! comment

! 1.8 PRINT operations (all variants)
PRINT "Hello" ! comment
PRINT 42 ! comment
PRINT testVar1 ! comment
PRINT testVar1 + 100 ! comment
PRINT "A", "B", "C" ! comment
PRINT "X"; "Y"; "Z" ! comment
PRINT 1, 2, 3 ! comment
PRINT ! comment (empty PRINT)
PRINT testVar1, ! comment (trailing comma)
PRINT testVar1; ! comment (trailing semicolon)

! 1.9 Built-in function calls
PRINT ASC('A') ! comment
PRINT CHR(65) ! comment
PRINT LEN("HELLO") ! comment
VAR timeVal = MILLIS() ! comment

! 1.10 Expression contexts
VAR expr1 = (10 + 20) * 3 ! comment on complex expression
VAR expr2 = testVar1 MOD 100 ! comment on modulo
VAR expr3 = testBits[0] AND TRUE ! comment on boolean expression
VAR comp1 = testVar1 > 100 ! comment on comparison
VAR comp2 = testChars[0] = 'X' ! comment on char comparison

! 1.11 Multi-statement lines with colons
VAR x = 1 : VAR y = 2 : PRINT x + y ! comment after colons
x = 10 : y = 20 : PRINT x * y ! comment after assignments
VAR temp = 50 : PRINT temp : temp = temp * 2 ! comment after mixed

! ===== SECTION 2: FUNCTION CONTEXT (Multiline Capture) =====
! Test comments in function definitions

FUNC TestCommentsInFunc(param1, param2) ! comment on FUNC line
    VAR local1 = 100 ! comment on local declaration
    VAR local2 ! comment on uninitialized local
    local1 = param1 + param2 ! comment on assignment
    PRINT "In function: ", local1 ! comment on PRINT
    
    ! Test control flow with comments inside function
    IF local1 > 50 THEN ! comment on IF line
        PRINT "Large value" ! comment in THEN block
    ELSE ! comment on ELSE line
        PRINT "Small value" ! comment in ELSE block
    ENDIF ! comment on ENDIF
    
    ! Test loops with comments inside function
    FOR i = 1 TO 3 ! comment on FOR line
        PRINT "Loop ", i ! comment in loop body
    NEXT i ! comment on NEXT
    
    VAR counter = 0 ! comment
    WHILE counter < 2 ! comment on WHILE line
        counter = counter + 1 ! comment in WHILE body
        PRINT "While ", counter ! comment
    WEND ! comment on WEND
    
    DO ! comment on DO line
        counter = counter - 1 ! comment in DO body
        PRINT "Do ", counter ! comment
    UNTIL counter = 0 ! comment on UNTIL line
    
    RETURN local1 ! comment on RETURN
ENDFUNC ! comment on ENDFUNC

! ===== SECTION 3: MAIN PROGRAM CONTEXT (Multiline Capture) =====
! Test comments in BEGIN/END blocks

BEGIN ! comment on BEGIN line
    VAR mainVar = 777 ! comment in main program
    PRINT "Main program running" ! comment in main
    
    ! Test function call with comment in main
    VAR result = TestCommentsInFunc(10, 20) ! comment on function call
    PRINT "Function returned: ", result ! comment after call
    
    ! Test control flow in main program
    IF result > 100 THEN ! comment on IF in main
        PRINT "Large result" ! comment in main IF block
    ENDIF ! comment on ENDIF in main
    
    FOR i = 1 TO 2 ! comment on FOR in main
        PRINT "Main loop ", i ! comment in main loop
    NEXT i ! comment on NEXT in main
    
END ! comment on END line

! ===== SUMMARY =====
! This test focuses on:
! 1. REPL-level single statements (should all work)
! 2. Function context multiline statements (should all work)  
! 3. Main program context multiline statements (should all work)
!
! Expected: All trailing comments should work perfectly now
! Any failures indicate specific keyword parsing issues
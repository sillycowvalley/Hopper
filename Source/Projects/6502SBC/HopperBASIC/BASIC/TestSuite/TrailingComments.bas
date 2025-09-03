CLS
! ================================================================
! COMPREHENSIVE TRAILING COMMENTS TEST SUITE
! Tests trailing ! comments in all contexts:
! - REPL commands (system, variables, operations)
! - Function contexts (parameters, locals, body statements)
! - Multi-statement lines with colons
! ================================================================

NEW

! ===== SECTION 1: REPL SYSTEM COMMANDS =====
! Test all system commands with trailing comments

! 1.1 Basic system commands
LIST ! comment
CLEAR ! comment
NEW ! comment
MEM ! comment
VARS ! comment
FUNCS ! comment

! 1.2 System commands with parameters
FORGET nonexistent ! comment
SAVE "TESTFILE" ! comment
LOAD "TESTFILE" ! comment
DIR ! comment
DEL "TESTFILE" ! comment

! ===== SECTION 2: REPL VARIABLE OPERATIONS =====
! Test variable declarations and assignments

! 2.1 Variable declarations
VAR testVar1 ! comment
VAR testVar2 = 42 ! comment
VAR testVar3 = 'A' ! comment
VAR testVar4 = TRUE ! comment
VAR testVar5 = "TEXT" ! comment

! 2.2 Multiple variable declarations
VAR a, b, c ! comment
VAR x = 1, y = 2, z = 3 ! comment

! 2.3 Constant declarations
CONST testConst = 100 ! comment
CONST testChar = 'Z' ! comment

! 2.4 Array declarations
BIT testBits[5] ! comment
CHAR testChars[10] ! comment
INT testInts[8] ! comment

! 2.5 Variable assignments
testVar1 = 999 ! comment
testBits[0] = TRUE ! comment
testChars[0] = 'X' ! comment

! ===== SECTION 3: REPL PRINT OPERATIONS =====
! Test all PRINT variants

! 3.1 PRINT with expressions
PRINT "Hello" ! comment
PRINT 42 ! comment
PRINT testVar1 ! comment
PRINT testVar1 + 100 ! comment

! 3.2 PRINT with separators
PRINT "A", "B", "C" ! comment
PRINT "X"; "Y"; "Z" ! comment
PRINT 1, 2, 3 ! comment

! 3.3 PRINT edge cases
PRINT ! comment (empty PRINT)
PRINT testVar1, ! comment (trailing comma)
PRINT testVar1; ! comment (trailing semicolon)

! ===== SECTION 4: FUNCTION CONTEXTS =====
! Test comments in function definitions and calls

! 4.1 Function declaration with comments
FUNC TestFunc(param1, param2) ! comment on function line
    VAR local1 = 100 ! comment on local declaration
    VAR local2 ! comment on uninitialized local
    local1 = param1 + param2 ! comment on assignment
    PRINT "In function: ", local1 ! comment on PRINT
    RETURN local1 ! comment on RETURN
ENDFUNC ! comment on ENDFUNC

! 4.2 Function calls with comments
VAR result = TestFunc(10, 20) ! comment on function call
PRINT result ! comment after function call

! ===== SECTION 5: CONTROL FLOW WITH COMMENTS =====
! Test comments in control structures

! 5.1 IF statements
IF testVar1 > 500 THEN ! comment on IF line
    PRINT "Greater" ! comment in THEN block
ELSE ! comment on ELSE line
    PRINT "Lesser" ! comment in ELSE block
ENDIF ! comment on ENDIF

! 5.2 FOR loops
FOR i = 1 TO 3 ! comment on FOR line
    PRINT i ! comment in loop body
NEXT i ! comment on NEXT

! 5.3 WHILE loops
VAR counter = 0 ! comment
WHILE counter < 3 ! comment on WHILE line
    counter = counter + 1 ! comment in WHILE body
WEND ! comment on WEND

! 5.4 DO/UNTIL loops
DO ! comment on DO line
    counter = counter - 1 ! comment in DO body
UNTIL counter = 0 ! comment on UNTIL line

! ===== SECTION 6: MULTI-STATEMENT LINES (COLONS) =====
! Test comments with colon-separated statements

! 6.1 Basic colon statements
VAR x = 1 : VAR y = 2 : PRINT x + y ! comment after colons
x = 10 : y = 20 : PRINT x * y ! comment after assignments

! 6.2 Mixed statement types with colons
VAR temp = 50 : PRINT temp : temp = temp * 2 ! comment after mixed

! 6.3 Function calls with colons
VAR r1 = TestFunc(1, 2) : VAR r2 = TestFunc(3, 4) : PRINT r1, r2 ! comment

! 6.4 Control flow with colons (may trigger Issue #3)
IF TRUE THEN PRINT "A" : PRINT "B" ENDIF ! comment

! ===== SECTION 7: EXPRESSION CONTEXTS =====
! Test comments in complex expressions

! 7.1 Arithmetic expressions
VAR expr1 = (10 + 20) * 3 ! comment on complex expression
VAR expr2 = testVar1 MOD 100 ! comment on modulo
VAR expr3 = testBits[0] AND TRUE ! comment on boolean expression

! 7.2 Comparison expressions
VAR comp1 = testVar1 > 100 ! comment on comparison
VAR comp2 = testChars[0] = 'X' ! comment on char comparison

! 7.3 Function call expressions
VAR funcResult = TestFunc(expr1, expr2) ! comment on nested call

! ===== SECTION 8: BUILT-IN FUNCTION CALLS =====
! Test comments with built-in functions

PRINT ASC('A') ! comment on ASC
PRINT CHR(65) ! comment on CHR
PRINT LEN("HELLO") ! comment on LEN
VAR timeVal = MILLIS() ! comment on MILLIS

! ===== SECTION 9: BEGIN/END MAIN PROGRAM =====
! Test comments in main program context

BEGIN ! comment on BEGIN line
    VAR mainVar = 777 ! comment in main program
    PRINT "Main program running" ! comment in main
    FOR i = 1 TO 2 ! comment on FOR in main
        PRINT "Loop ", i ! comment in main loop
    NEXT i ! comment on NEXT in main
END ! comment on END line

! ===== SECTION 10: EDGE CASES =====
! Test unusual comment scenarios

! 10.1 Multiple consecutive comments
testVar1 = 42 ! first comment
! Second comment line
testVar1 = testVar1 + 1 ! third comment

! 10.2 Comments with special characters
testVar1 = 100 ! comment with "quotes" and 'apostrophes'
testVar2 = 200 ! comment with numbers 123 and symbols @#$%

! 10.3 Very long comments
testVar3 = 300 ! This is a very long comment that tests whether the tokenizer can handle extended comment text without issues or buffer overflows

! ===== SUMMARY INSTRUCTIONS =====
! Run this test suite and check for:
! 1. Any ?SYNTAX ERROR messages
! 2. Commands that fail to execute
! 3. Comments that don't appear in LIST output
! 4. Any crashes or hangs
!
! Expected results:
! - Most trailing comments should work
! - Some system commands may still fail
! - Function contexts should work
! - Colon statements may have issues

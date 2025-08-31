! Storage Module Error Tests - Expected Behavior Specification
! Each test shows the EXPECTED error message
! Copy and paste each section into REPL after FORMAT command

! Initial setup - clean slate
FORMAT
Y

! ==============================================================================
! Setup: Create valid test files for later tests
! ==============================================================================
NEW
BEGIN
    PRINT "Test Program"
END
SAVE TESTPROG
! Expected: OK

NEW
BEGIN
    PRINT "Another Test"
END
SAVE VALIDFILE
! Expected: OK

! ==============================================================================
! Test 1: SAVE inside function
! ==============================================================================
NEW
FUNC TestSave()
    SAVE MYFILE
ENDFUNC
TestSave()
! Expected: ILLEGAL IN FUNCTION

! ==============================================================================
! Test 2: SAVE without filename
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE
! Expected: FILENAME EXPECTED

! ==============================================================================
! Test 3: SAVE with numeric filename
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE 123
! Expected: ILLEGAL FILENAME

! ==============================================================================
! Test 4: SAVE with keyword as filename
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE FOR
! Expected: ILLEGAL FILENAME

NEW
BEGIN
    PRINT "Test"
END
SAVE IF
! Expected: ILLEGAL FILENAME

! ==============================================================================
! Test 5: SAVE with special characters in filename
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE TEST*FILE
! Expected: ILLEGAL FILENAME

NEW
BEGIN
    PRINT "Test"
END
SAVE TEST/FILE
! Expected: ILLEGAL FILENAME

! ==============================================================================
! Test 6: SAVE with multiple arguments
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE TEST1 TEST2
! Expected: SYNTAX ERROR

! ==============================================================================
! Test 7: SAVE with filename > 13 characters
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
! This is 14 characters
SAVE FOURTEENCHARS1
! Expected: FILENAME TOO LONG

NEW
BEGIN
    PRINT "Test"
END
! This is 20 characters
SAVE TWENTYCHARACTERNAMES
! Expected: FILENAME TOO LONG

! ==============================================================================
! Test 8: SAVE with exactly 13 characters (should work)
! ==============================================================================
NEW
BEGIN
    PRINT "Test 13"
END
SAVE THIRTEENCHAR1
! Expected: OK

! ==============================================================================
! Test 9: SAVE with quoted string instead of identifier
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE "TEST"
! Expected: IDENTIFIER EXPECTED

! ==============================================================================
! Test 10: LOAD inside function
! ==============================================================================
NEW
FUNC TestLoad()
    LOAD TESTPROG
ENDFUNC
TestLoad()
! Expected: ILLEGAL IN FUNCTION

! ==============================================================================
! Test 11: LOAD without filename
! ==============================================================================
NEW
LOAD
! Expected: FILENAME EXPECTED

! ==============================================================================
! Test 12: LOAD with numeric filename
! ==============================================================================
NEW
LOAD 456
! Expected: ILLEGAL FILENAME

! ==============================================================================
! Test 13: LOAD with multiple arguments
! ==============================================================================
NEW
LOAD FILE1 FILE2
! Expected: SYNTAX ERROR

! ==============================================================================
! Test 14: LOAD non-existent file
! ==============================================================================
NEW
LOAD DOESNOTEXIST999
! Expected: FILE NOT FOUND

! ==============================================================================
! Test 15: LOAD with quoted string
! ==============================================================================
NEW
LOAD "TEST"
! Expected: IDENTIFIER EXPECTED

! ==============================================================================
! Test 16: DEL without filename
! ==============================================================================
NEW
DEL
! Expected: FILENAME EXPECTED

! ==============================================================================
! Test 17: DEL with numeric filename
! ==============================================================================
NEW
DEL 789
! Expected: ILLEGAL FILENAME

! ==============================================================================
! Test 18: DEL with multiple arguments
! ==============================================================================
NEW
DEL FILE1 FILE2
! Expected: SYNTAX ERROR

! ==============================================================================
! Test 19: DEL non-existent file
! ==============================================================================
NEW
DEL NOFILE
! Expected: FILE NOT FOUND

! ==============================================================================
! Test 20: DEL file that exists, then DEL again
! ==============================================================================
NEW
DEL VALIDFILE
! Expected: OK

DEL VALIDFILE
! Expected: FILE NOT FOUND

! ==============================================================================
! Test 21: DEL with quoted string
! ==============================================================================
NEW
DEL "TEST"
! Expected: IDENTIFIER EXPECTED

! ==============================================================================
! Test 22: DEL with special characters
! ==============================================================================
NEW
DEL TEST*FILE
! Expected: ILLEGAL FILENAME

NEW
DEL TEST/FILE
! Expected: ILLEGAL FILENAME

! ==============================================================================
! Test 23: Test case sensitivity (should be case-insensitive)
! ==============================================================================
NEW
BEGIN
    PRINT "Case Test"
END
SAVE TestFile
! Expected: OK

NEW
LOAD TESTFILE
! Expected: OK (loads same file - case insensitive)

NEW
LOAD testfile
! Expected: OK (loads same file - case insensitive)

DEL testFILE
! Expected: OK (deletes same file - case insensitive)

! ==============================================================================
! Test 24: Empty program save (should work)
! ==============================================================================
NEW
SAVE EMPTYFILE
! Expected: OK

! ==============================================================================
! Working examples for reference
! ==============================================================================
NEW
BEGIN
    PRINT "Demo"
END
SAVE DEMO
! Expected: OK

NEW
LOAD DEMO
LIST
! Expected: Shows the loaded program

DIR
! Expected: Lists all saved files

DEL DEMO
! Expected: OK

! ==============================================================================
! Cleanup
! ==============================================================================
DEL TESTPROG
! Expected: OK

DEL THIRTEENCHAR1
! Expected: OK

DEL EMPTYFILE
! Expected: OK

DEL TESTFILE
! Expected: FILE NOT FOUND (already deleted in test 23)

! ==============================================================================
! Summary of Expected Error Messages:
! ==============================================================================
! FILENAME EXPECTED - When filename is missing
! ILLEGAL FILENAME - When filename contains numbers, keywords, or special chars
! IDENTIFIER EXPECTED - When quoted string used instead of identifier  
! FILENAME TOO LONG - When filename exceeds 13 characters
! FILE NOT FOUND - When trying to load/delete non-existent file
! ILLEGAL IN FUNCTION - When SAVE/LOAD used inside function definition
! SYNTAX ERROR - When multiple arguments provided
! OK - When operation succeeds

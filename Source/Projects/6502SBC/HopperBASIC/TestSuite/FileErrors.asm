! Storage Module Error Tests with Expected Errors
! Each test shows the EXPECTED error message based on our recommendations
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
! Expected: OK (success)

NEW
BEGIN
    PRINT "Another Test"
END
SAVE VALIDFILE
! Expected: OK (success)

! ==============================================================================
! Test 1: SAVE inside function
! Expected: ILLEGAL IN FUNCTION
! ==============================================================================
NEW
FUNC TestSave()
    SAVE MYFILE
ENDFUNC
TestSave()
! Expected: ILLEGAL IN FUNCTION
! Current: SYNTAX ERROR (0xB734)

! ==============================================================================
! Test 2: SAVE without filename
! Expected: FILENAME EXPECTED
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE
! Expected: FILENAME EXPECTED
! Current: ILLEGAL IDENTIFIER (0xE3F5)

! ==============================================================================
! Test 3: SAVE with numeric filename
! Expected: ILLEGAL FILENAME
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE 123
! Expected: ILLEGAL FILENAME
! Current: ILLEGAL IDENTIFIER (0xE3F5)

! ==============================================================================
! Test 4: SAVE with keyword as filename
! Expected: ILLEGAL FILENAME
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE FOR
! Expected: ILLEGAL FILENAME
! Current: ILLEGAL IDENTIFIER (0xE3F5)

NEW
BEGIN
    PRINT "Test"
END
SAVE IF
! Expected: ILLEGAL FILENAME
! Current: ILLEGAL IDENTIFIER (0xE3F5)

! ==============================================================================
! Test 5: SAVE with special characters in filename
! Expected: ILLEGAL FILENAME
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE TEST*FILE
! Expected: ILLEGAL FILENAME
! Current: SYNTAX ERROR (0xE5AE)

NEW
BEGIN
    PRINT "Test"
END
SAVE TEST/FILE
! Expected: ILLEGAL FILENAME
! Current: SYNTAX ERROR (0xE5AE)

! ==============================================================================
! Test 6: SAVE with multiple arguments
! Expected: SYNTAX ERROR
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE TEST1 TEST2
! Expected: SYNTAX ERROR (this is appropriate)
! Current: SYNTAX ERROR (0xE5AE)

! ==============================================================================
! Test 7: SAVE with filename > 13 characters
! Expected: FILENAME TOO LONG
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
! This is 14 characters
SAVE FOURTEENCHARS1
! Expected: FILENAME TOO LONG
! Current: INVALID FILENAME (0x84E0)

NEW
BEGIN
    PRINT "Test"
END
! This is 20 characters
SAVE TWENTYCHARACTERNAMES
! Expected: FILENAME TOO LONG
! Current: INVALID FILENAME (0x84E0)

! ==============================================================================
! Test 8: SAVE with exactly 13 characters (should work)
! ==============================================================================
NEW
BEGIN
    PRINT "Test 13"
END
SAVE THIRTEENCHAR1
! Expected: OK (success)
! Current: OK

! ==============================================================================
! Test 9: SAVE with quoted string instead of identifier
! Expected: IDENTIFIER EXPECTED
! ==============================================================================
NEW
BEGIN
    PRINT "Test"
END
SAVE "TEST"
! Expected: IDENTIFIER EXPECTED
! Current: ILLEGAL IDENTIFIER (0xE3F5)

! ==============================================================================
! Test 10: LOAD inside function
! Expected: ILLEGAL IN FUNCTION
! ==============================================================================
NEW
FUNC TestLoad()
    LOAD TESTPROG
ENDFUNC
TestLoad()
! Expected: ILLEGAL IN FUNCTION
! Current: SYNTAX ERROR (0xB734)

! ==============================================================================
! Test 11: LOAD without filename
! Expected: FILENAME EXPECTED
! ==============================================================================
NEW
LOAD
! Expected: FILENAME EXPECTED
! Current: ILLEGAL IDENTIFIER (0xE41B)

! ==============================================================================
! Test 12: LOAD with numeric filename
! Expected: ILLEGAL FILENAME
! ==============================================================================
NEW
LOAD 456
! Expected: ILLEGAL FILENAME
! Current: ILLEGAL IDENTIFIER (0xE41B)

! ==============================================================================
! Test 13: LOAD with multiple arguments
! Expected: SYNTAX ERROR
! ==============================================================================
NEW
LOAD FILE1 FILE2
! Expected: SYNTAX ERROR (this is appropriate)
! Current: SYNTAX ERROR (0xE5AE)

! ==============================================================================
! Test 14: LOAD non-existent file
! Expected: FILE NOT FOUND
! ==============================================================================
NEW
LOAD DOESNOTEXIST999
! Expected: FILE NOT FOUND
! Current: FILE NOT FOUND (0x8618) - Perfect!

! ==============================================================================
! Test 15: LOAD with quoted string
! Expected: IDENTIFIER EXPECTED
! ==============================================================================
NEW
LOAD "TEST"
! Expected: IDENTIFIER EXPECTED
! Current: ILLEGAL IDENTIFIER (0xE41B)

! ==============================================================================
! Test 16: DEL without filename
! Expected: FILENAME EXPECTED
! ==============================================================================
NEW
DEL
! Expected: FILENAME EXPECTED
! Current: ILLEGAL IDENTIFIER (0xE452)

! ==============================================================================
! Test 17: DEL with numeric filename
! Expected: ILLEGAL FILENAME
! ==============================================================================
NEW
DEL 789
! Expected: ILLEGAL FILENAME
! Current: ILLEGAL IDENTIFIER (0xE452)

! ==============================================================================
! Test 18: DEL with multiple arguments
! Expected: SYNTAX ERROR
! ==============================================================================
NEW
DEL FILE1 FILE2
! Expected: SYNTAX ERROR (this is appropriate)
! Current: SYNTAX ERROR (0xE5AE)

! ==============================================================================
! Test 19: DEL non-existent file
! Expected: FILE NOT FOUND
! ==============================================================================
NEW
DEL NOFILE
! Expected: FILE NOT FOUND
! Current: Should show FILE NOT FOUND

! ==============================================================================
! Test 20: DEL file that exists, then DEL again
! Expected: First OK, then FILE NOT FOUND
! ==============================================================================
NEW
DEL VALIDFILE
! Expected: OK (success)
! Current: OK

DEL VALIDFILE
! Expected: FILE NOT FOUND
! Current: FILE NOT FOUND (0x85C5) - Perfect!

! ==============================================================================
! Test 21: DEL with quoted string
! Expected: IDENTIFIER EXPECTED
! ==============================================================================
NEW
DEL "TEST"
! Expected: IDENTIFIER EXPECTED
! Current: ILLEGAL IDENTIFIER (0xE452)

! ==============================================================================
! Test 22: DEL with special characters
! Expected: ILLEGAL FILENAME
! ==============================================================================
NEW
DEL TEST*FILE
! Expected: ILLEGAL FILENAME
! Current: SYNTAX ERROR (0xE5AE)

NEW
DEL TEST/FILE
! Expected: ILLEGAL FILENAME
! Current: SYNTAX ERROR (0xE5AE)

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
! Expected: OK (case insensitive)
! Current: OK

NEW
LOAD testfile
! Expected: OK (case insensitive)
! Current: OK

DEL testFILE
! Expected: OK (case insensitive)

! ==============================================================================
! Test 24: Empty program save (should work)
! ==============================================================================
NEW
SAVE EMPTYFILE
! Expected: OK (empty programs can be saved)
! Current: OK

! ==============================================================================
! Working examples for reference
! ==============================================================================
NEW
BEGIN
    PRINT "Demo"
END
SAVE DEMO

NEW
LOAD DEMO
LIST
! Should show the program

DIR
! Should list all saved files

DEL DEMO
! Should succeed

! ==============================================================================
! Cleanup
! ==============================================================================
DEL TESTPROG
DEL THIRTEENCHAR1
DEL EMPTYFILE
DEL TESTFILE

! ==============================================================================
! Summary of Expected Error Improvements:
! ==============================================================================
! 1. ILLEGAL IDENTIFIER → FILENAME EXPECTED (when missing filename)
! 2. ILLEGAL IDENTIFIER → ILLEGAL FILENAME (when using number/keyword)
! 3. ILLEGAL IDENTIFIER → IDENTIFIER EXPECTED (when using quoted string)
! 4. INVALID FILENAME → FILENAME TOO LONG (when > 13 chars)
! 5. INVALID FILENAME → FILE NOT FOUND (for DEL of non-existent)
! 6. SYNTAX ERROR → ILLEGAL IN FUNCTION (for SAVE/LOAD in FUNC)
! 7. SYNTAX ERROR → ILLEGAL FILENAME (for special chars)
! 8. Keep SYNTAX ERROR for multiple arguments
! 9. Keep FILE NOT FOUND where it's already correct


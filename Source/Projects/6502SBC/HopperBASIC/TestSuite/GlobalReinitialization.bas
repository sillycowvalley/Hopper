NEW
! ================================================================
! GLOBAL VARIABLE RE-INITIALIZATION TEST SUITE
!
! Tests that global variables reset to their original declared
! values when RUN is executed, regardless of modifications made
! between declaration and RUN.
!
! Key behaviors tested:
! - VAR declarations with type inference reset to original values
! - Uninitialized variables reset to type defaults
! - CONST values remain unchanged (immutable)
! - VAR type changes reset to original type
! - Arrays reset to default values
! - Type compatibility enforcement
! ================================================================

! Test 1: LONG variable re-initialization
VAR longVar = 42
VAR longVar2

! Modify before RUN to test reset behavior
longVar = 999
longVar2 = -5555

BEGIN
    PRINT "1. LONG test:"
    PRINT "longVar="; longVar; " expect 42"
    PRINT "longVar2="; longVar2; " expect 0"
END
RUN

NEW
! Test 2: CHAR variable re-initialization  
VAR charVar = 'A'
VAR charVar2

! Modify before RUN
charVar = 'Z'
charVar2 = 'Q'

BEGIN
    PRINT "2. CHAR test:"
    PRINT "charVar="; charVar; " expect A"
    PRINT "charVar2="; charVar2; " expect NUL"
END
RUN

NEW
! Test 3: BIT variable re-initialization (CORRECTED)
VAR bitVar = TRUE
VAR bitVar2 = FALSE  ! Must initialize to get BIT type

! Modify before RUN
bitVar = FALSE
bitVar2 = TRUE

BEGIN
    PRINT "3. BIT test:"
    PRINT "bitVar="; bitVar; " expect TRUE"
    PRINT "bitVar2="; bitVar2; " expect FALSE"
END
RUN

NEW
! Test 4: STRING variable re-initialization (CORRECTED)
VAR stringVar = "HELLO"
VAR stringVar2 = ""  ! Must initialize to get STRING type

! Modify before RUN
stringVar = "CHANGED"
stringVar2 = "MODIFIED"

BEGIN
    PRINT "4. STRING test:"
    PRINT "stringVar="; stringVar; " expect HELLO"
    PRINT "stringVar2="; stringVar2; " expect empty"
END
RUN

NEW
! Additional Test: Uninitialized VAR defaults (LONG)
! Defaults to LONG type, value 0
VAR uninitLong

! Modify before RUN
uninitLong = 12345

BEGIN
    PRINT "Uninitialized VAR test:"
    PRINT "uninitLong="; uninitLong; " expect 0 (LONG default)"
END
RUN

NEW
! Test 5: CONST immutability (should never change)
CONST longConst = 100
CONST charConst = 'K'
CONST bitConst = TRUE
CONST stringConst = "CONSTANT"

BEGIN
    PRINT "5. CONST test:"
    PRINT "longConst="; longConst
    PRINT "charConst="; charConst
    PRINT "bitConst="; bitConst
    PRINT "stringConst="; stringConst
END
RUN

NEW
! Test 6: VAR type changes reset to original type
VAR flexVar = 42

! Change type before RUN
flexVar = "STRING NOW"

BEGIN
    PRINT "6. VAR type change:"
    PRINT "flexVar="; flexVar; " expect 42"
END
RUN

NEW
! Test 7: BIT array re-initialization
BIT bitArray[3]

! Modify before RUN
bitArray[0] = TRUE
bitArray[1] = TRUE

BEGIN
    PRINT "7. BIT array:"
    PRINT "bitArray[0]="; bitArray[0]; " expect FALSE"
    PRINT "bitArray[1]="; bitArray[1]; " expect FALSE"
END
RUN

NEW
! Test 8: CHAR array re-initialization
CHAR charArray[3]

! Modify before RUN
charArray[0] = 'A'
charArray[1] = 'B'

BEGIN
    PRINT "8. CHAR array:"
    PRINT "charArray[0]="; charArray[0]; " expect NUL"
    PRINT "charArray[1]="; charArray[1]; " expect NUL"
END
RUN



NEW
! Test 9: INT array re-initialization
INT intArray[3]

! Modify before RUN
intArray[0] = 1000
intArray[1] = -500

BEGIN
    PRINT "9. INT array:"
    PRINT "intArray[0]="; intArray[0]; " expect 0"
    PRINT "intArray[1]="; intArray[1]; " expect 0"
END
RUN

NEW
! Test 10: Type compatibility verification
VAR longNum = 1000000
VAR character = 'Z'
VAR boolean = FALSE
VAR text = "TEST"

CONST maxValue = 2147483647
CONST letter = 'A'
CONST flag = TRUE
CONST message = "HELLO"

! Modify before RUN
longNum = -999999
character = 'Q'
boolean = TRUE
text = "MODIFIED"

BEGIN
    PRINT "10. Type compatibility:"
    PRINT "longNum="; longNum; " expect 1000000"
    PRINT "character="; character; " expect Z"
    PRINT "boolean="; boolean; " expect FALSE"
    PRINT "text="; text; " expect TEST"
    
    ! Test assignments work
    longNum = maxValue
    character = letter
    boolean = flag
    text = message
    PRINT "Assignments successful"
END
RUN

NEW
! Test 11: Array operations with LONG indices
BIT flags[5]
CHAR letters[3]
INT numbers[4]

BEGIN
    PRINT "11. Array operations:"
    VAR index = 2
    flags[index] = TRUE
    letters[index] = 'M'
    numbers[index] = 999
    
    PRINT "flags[2]="; flags[2]
    PRINT "letters[2]="; letters[2]
    PRINT "numbers[2]="; numbers[2]
END
RUN

NEW
! Test 12: LONG arithmetic (32-bit)
VAR bigNum = 1000000

BEGIN
    PRINT "12. Large arithmetic:"
    bigNum = bigNum * 2000
    PRINT "1000000 * 2000 ="; bigNum
END
RUN

NEW
! Test 13: Complex type interactions
VAR longVar = 42
VAR charVar = 'A'
VAR bitVar = TRUE
VAR stringVar = "TEST"

! Modify all before RUN
longVar = 999
charVar = 'Z'
bitVar = FALSE
stringVar = "CHANGED"

BEGIN
    PRINT "13. All types reset:"
    PRINT "LONG:"; longVar; " CHAR:"; charVar
    PRINT "BIT:"; bitVar; " STRING:"; stringVar
    
    ! Modify inside BEGIN/END
    longVar = 777
    charVar = 'X'
    bitVar = FALSE
    stringVar = "FINAL"
    PRINT "Modified successfully"
END
RUN

! Final verification
VARS
NEW
MEM



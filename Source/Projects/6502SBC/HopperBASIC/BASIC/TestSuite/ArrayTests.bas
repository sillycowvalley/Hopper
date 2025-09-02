CLS
NEW
MEM

! Global arrays of all 5 types
BIT globBitArray[10]
CHAR globCharArray[10] 
BYTE globByteArray[10]
WORD globWordArray[10]
INT globIntArray[10]

! Global indices
VAR globIndex = 2
VAR globIndex2 = 5
VAR globValue = 42

! Test arrays to pass as parameters
BIT testBitArray[10]
CHAR testCharArray[10]
BYTE testByteArray[10] 
WORD testWordArray[10]
INT testIntArray[10]



FUNC TestBitArrayOpcodes(localBitArray)
    VAR localIndex = 3
    VAR localValue = TRUE
    
    ! SETITEM variants
    globBitArray[globIndex] = TRUE      ! SETITEMGG
    globBitArray[localIndex] = FALSE    ! SETITEMGL
    localBitArray[globIndex] = TRUE     ! SETITEMLG
    localBitArray[localIndex] = FALSE   ! SETITEMLL
    globBitArray[globIndex + 1] = TRUE  ! SETITEM (generic)
    
    ! GETITEM variants
    localValue = globBitArray[globIndex2]         ! GETITEMGG
    localValue = globBitArray[localIndex]         ! GETITEMGL
    localValue = localBitArray[globIndex2]        ! GETITEMLG
    localValue = localBitArray[localIndex]        ! GETITEMLL
    localValue = globBitArray[globIndex + 1]      ! GETITEM (generic)
    
    PRINT "BIT: glob[2]="; globBitArray[2]; " local[3]="; localBitArray[3]
ENDFUNC

FUNC TestCharArrayOpcodes(localCharArray)
    VAR localIndex = 3
    VAR localValue = 'A'
    
    ! SETITEM variants  
    globCharArray[globIndex] = 'X'       ! SETITEMGG
    globCharArray[localIndex] = 'Y'      ! SETITEMGL
    localCharArray[globIndex] = 'Z'      ! SETITEMLG
    localCharArray[localIndex] = 'W'     ! SETITEMLL
    globCharArray[globIndex + 1] = 'Q'   ! SETITEM (generic)
    
    ! GETITEM variants
    localValue = globCharArray[globIndex2]        ! GETITEMGG
    localValue = globCharArray[localIndex]        ! GETITEMGL
    localValue = localCharArray[globIndex2]       ! GETITEMLG
    localValue = localCharArray[localIndex]       ! GETITEMLL
    localValue = globCharArray[globIndex + 1]     ! GETITEM (generic)
    
    PRINT "CHAR: glob[2]="; globCharArray[2]; " local[3]="; localCharArray[3]
ENDFUNC

FUNC TestByteArrayOpcodes(localByteArray)
    VAR localIndex = 3
    VAR localValue = 100
    
    ! SETITEM variants
    globByteArray[globIndex] = 200       ! SETITEMGG
    globByteArray[localIndex] = 150      ! SETITEMGL
    localByteArray[globIndex] = 175      ! SETITEMLG
    localByteArray[localIndex] = 125     ! SETITEMLL
    globByteArray[globIndex + 1] = 255   ! SETITEM (generic)
    
    ! GETITEM variants
    localValue = globByteArray[globIndex2]        ! GETITEMGG
    localValue = globByteArray[localIndex]        ! GETITEMGL
    localValue = localByteArray[globIndex2]       ! GETITEMLG
    localValue = localByteArray[localIndex]       ! GETITEMLL
    localValue = globByteArray[globIndex + 1]     ! GETITEM (generic)
    
    PRINT "BYTE: glob[2]="; globByteArray[2]; " local[3]="; localByteArray[3]
ENDFUNC

FUNC TestWordArrayOpcodes(localWordArray)
    VAR localIndex = 3
    VAR localValue = 1000
    
    ! SETITEM variants
    globWordArray[globIndex] = 2000      ! SETITEMGG
    globWordArray[localIndex] = 3000     ! SETITEMGL
    localWordArray[globIndex] = 4000     ! SETITEMLG
    localWordArray[localIndex] = 5000    ! SETITEMLL
    globWordArray[globIndex + 1] = 6000  ! SETITEM (generic)
    
    ! GETITEM variants
    localValue = globWordArray[globIndex2]        ! GETITEMGG
    localValue = globWordArray[localIndex]        ! GETITEMGL
    localValue = localWordArray[globIndex2]       ! GETITEMLG
    localValue = localWordArray[localIndex]       ! GETITEMLL
    localValue = globWordArray[globIndex + 1]     ! GETITEM (generic)
    
    PRINT "WORD: glob[2]="; globWordArray[2]; " local[3]="; localWordArray[3]
ENDFUNC

FUNC TestIntArrayOpcodes(localIntArray)
    VAR localIndex = 3
    VAR localValue = -100
    
    ! SETITEM variants
    globIntArray[globIndex] = -200       ! SETITEMGG
    globIntArray[localIndex] = 300       ! SETITEMGL
    localIntArray[globIndex] = -400      ! SETITEMLG
    localIntArray[localIndex] = 500      ! SETITEMLL
    globIntArray[globIndex + 1] = -600   ! SETITEM (generic)
    
    ! GETITEM variants
    localValue = globIntArray[globIndex2]         ! GETITEMGG
    localValue = globIntArray[localIndex]         ! GETITEMGL
    localValue = localIntArray[globIndex2]        ! GETITEMLG
    localValue = localIntArray[localIndex]        ! GETITEMLL
    localValue = globIntArray[globIndex + 1]      ! GETITEM (generic)
    
    PRINT "INT: glob[2]="; globIntArray[2]; " local[3]="; localIntArray[3]
ENDFUNC

BEGIN
    ! Initialize all global arrays
    FOR i = 0 TO 9
        globBitArray[i] = FALSE
        globCharArray[i] = 'A'
        globByteArray[i] = i * 10
        globWordArray[i] = i * 100
        globIntArray[i] = i - 5
    NEXT i
    
    ! Initialize test arrays 
    FOR i = 0 TO 9
        testBitArray[i] = TRUE
        testCharArray[i] = 'a'
        testByteArray[i] = i * 20
        testWordArray[i] = i * 200
        testIntArray[i] = i * (-10)
    NEXT i
    
    PRINT "=== COMPREHENSIVE ARRAY OPCODE TESTS ==="
    PRINT
    
    ! Run tests for each array type
    TestBitArrayOpcodes(testBitArray)
    TestCharArrayOpcodes(testCharArray)
    TestByteArrayOpcodes(testByteArray)
    TestWordArrayOpcodes(testWordArray)
    TestIntArrayOpcodes(testIntArray)
    
    PRINT
    PRINT "All array types tested with all opcode variants!"
END
RUN

NEW
MEM

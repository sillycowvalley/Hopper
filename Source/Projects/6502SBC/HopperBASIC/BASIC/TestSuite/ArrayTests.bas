CLS
NEW
MEM

! Global array and indices
INT globArray[10]
VAR globIndex = 2
VAR globIndex2 = 5
VAR globValue = 42
INT testArray[10]

FUNC TestArrayOpcodes(localArray)
    ! Local variables (function arguments create locals)
    VAR localIndex = 3
    VAR localValue = 99
    
    ! === SETITEM TESTS ===
    
    ! SETITEMGG: Global array, Global index  
    globArray[globIndex] = 10
    
    ! SETITEMGL: Global array, Local index
    globArray[localIndex] = 20
    
    ! SETITEMLG: Local array, Global index
    localArray[globIndex] = 30
    
    ! SETITEMLL: Local array, Local index
    localArray[localIndex] = 40
    
    ! Generic SETITEM: Expression-based (forces stack operands)
    globArray[globIndex + 1] = 50
    localArray[localIndex + 1] = 60
    
    ! === GETITEM TESTS ===
    
    ! GETITEMGG: Global array, Global index
    globValue = globArray[globIndex2]
    
    ! GETITEMGL: Global array, Local index  
    localValue = globArray[localIndex]
    
    ! GETITEMLG: Local array, Global index
    globValue = localArray[globIndex2]
    
    ! GETITEMLL: Local array, Local index
    localValue = localArray[localIndex]
    
    ! Generic GETITEM: Expression-based (forces stack operands)
    globValue = globArray[globIndex + globIndex2]
    localValue = localArray[localIndex - 1]
    
    ! Print results to verify
    PRINT "Global array[2] ="; globArray[2]
    PRINT "Local array[3] ="; localArray[3]
    PRINT "Global value ="; globValue
    PRINT "Local value ="; localValue
ENDFUNC

BEGIN
    FOR i = 0 TO 9
        globArray[i] = i
    NEXT i
    FOR i = 0 TO 9
        testArray[i] = i * 10
    NEXT i

    ! Run the test
    TestArrayOpcodes(testArray)
END
RUN

NEW
MEM


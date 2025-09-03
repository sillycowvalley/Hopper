NEW

CONST InWritePtr = 0x0A   ! Serial buffer write position
CONST InReadPtr  = 0x0B   ! Serial buffer read position
CONST InBuffer   = 0x0200 ! Serial input buffer

! WordStar key test results
BIT tested[26]     ! Track which keys we've tested
VAR testCount

! Sequence handling state  
VAR waitingFor     ! 0=none, 17=Ctrl-Q, 11=Ctrl-K
VAR sequenceKeys   ! Count of valid sequences found

! Check if key available
FUNC KeyReady()
    RETURN PEEK(InReadPtr) <> PEEK(InWritePtr)
ENDFUNC

! Get raw key from buffer
FUNC GetKey()
    VAR iptr, key
    WHILE NOT KeyReady()
        DELAY(5)
    WEND
    iptr = PEEK(InReadPtr)
    key = PEEK(InBuffer + iptr)
    POKE(InReadPtr, (iptr + 1) & 0xFF)
    RETURN key
ENDFUNC

! Show the WordStar menu
FUNC ShowMenu()
    PRINT
    PRINT "=== WORDSTAR KEY TESTER ==="
    PRINT
    PRINT "CURSOR MOVEMENT (The Diamond):"
    PRINT "  Ctrl-E  Up one line"
    PRINT "  Ctrl-S  Left one character"  
    PRINT "  Ctrl-D  Right one character"
    PRINT "  Ctrl-X  Down one line"
    PRINT
    PRINT "WORD MOVEMENT:"
    PRINT "  Ctrl-A  Left one word"
    PRINT "  Ctrl-F  Right one word"
    PRINT
    PRINT "LINE OPERATIONS:"
    PRINT "  Ctrl-QS Beginning of line"
    PRINT "  Ctrl-QD End of line"
    PRINT
    PRINT "EDITING:"
    PRINT "  Ctrl-G  Delete character"
    PRINT "  Ctrl-H  Backspace"
    PRINT "  Ctrl-Y  Delete line"
    PRINT
    PRINT "FILE OPERATIONS:"
    PRINT "  Ctrl-KS Save file"
    PRINT "  Ctrl-KQ Quit without saving"
    PRINT "  Ctrl-KX Save and quit"
    PRINT
    PRINT "TOGGLES:"
    PRINT "  Ctrl-V  Insert/Overstrike"
    PRINT
    PRINT "Press ESC to exit, any WordStar key to test"
    PRINT "Regular keys will be ignored..."
    PRINT
ENDFUNC

! Get key name for display
FUNC GetKeyName(ctrlKey)
    VAR ch
    ch = ctrlKey + ASC('A') - 1
    RETURN CHR(ch)
ENDFUNC

! Check if this is a WordStar key we care about
FUNC IsWordStarKey(key)
    ! Check single Ctrl keys: A,D,E,F,G,H,S,V,X,Y
    IF key = 1 THEN RETURN TRUE ENDIF   ! Ctrl-A
    IF key = 4 THEN RETURN TRUE ENDIF   ! Ctrl-D  
    IF key = 5 THEN RETURN TRUE ENDIF   ! Ctrl-E
    IF key = 6 THEN RETURN TRUE ENDIF   ! Ctrl-F
    IF key = 7 THEN RETURN TRUE ENDIF   ! Ctrl-G
    IF key = 8 THEN RETURN TRUE ENDIF   ! Ctrl-H
    IF key = 19 THEN RETURN TRUE ENDIF  ! Ctrl-S
    IF key = 22 THEN RETURN TRUE ENDIF  ! Ctrl-V
    IF key = 24 THEN RETURN TRUE ENDIF  ! Ctrl-X
    IF key = 25 THEN RETURN TRUE ENDIF  ! Ctrl-Y
    
    ! Note: Ctrl-Q and Ctrl-K sequences need special handling
    IF key = 17 THEN RETURN TRUE ENDIF  ! Ctrl-Q (start of sequence)
    IF key = 11 THEN RETURN TRUE ENDIF  ! Ctrl-K (start of sequence)
    
    RETURN FALSE
ENDFUNC

! Mark a key as tested and count it
FUNC MarkTested(key)
    VAR index
    index = key - 1  ! Convert to 0-based index
    IF (index >= 0) AND (index < 26) THEN
        IF NOT tested[index] THEN
            tested[index] = TRUE
            testCount = testCount + 1
        ENDIF
    ENDIF
ENDFUNC

! Show test results
FUNC ShowResults()
    VAR ch
    PRINT
    PRINT "=== TEST RESULTS ==="
    PRINT "Keys tested: ", testCount, "/15"
    PRINT
    PRINT "WORKING:"
    
    ! Check each WordStar key
    IF tested[0] THEN PRINT "  Ctrl-A (Left word)" ENDIF      ! A=1
    IF tested[3] THEN PRINT "  Ctrl-D (Right char)" ENDIF     ! D=4
    IF tested[4] THEN PRINT "  Ctrl-E (Up line)" ENDIF        ! E=5
    IF tested[5] THEN PRINT "  Ctrl-F (Right word)" ENDIF     ! F=6
    IF tested[6] THEN PRINT "  Ctrl-G (Delete char)" ENDIF    ! G=7
    IF tested[7] THEN PRINT "  Ctrl-H (Backspace)" ENDIF      ! H=8
    IF tested[10] THEN PRINT "  Ctrl-K (File prefix)" ENDIF   ! K=11
    IF tested[16] THEN PRINT "  Ctrl-Q (Quick prefix)" ENDIF  ! Q=17
    IF tested[18] THEN PRINT "  Ctrl-S (Left char)" ENDIF     ! S=19
    IF tested[21] THEN PRINT "  Ctrl-V (Insert toggle)" ENDIF ! V=22
    IF tested[23] THEN PRINT "  Ctrl-X (Down line)" ENDIF     ! X=24
    IF tested[24] THEN PRINT "  Ctrl-Y (Delete line)" ENDIF   ! Y=25
    
    IF testCount < 15 THEN
        PRINT
        PRINT "Some keys not tested yet."
        PRINT "Note: Ctrl-Q and Ctrl-K are prefixes"
        PRINT "for sequences like Ctrl-QS, Ctrl-KS"
    ENDIF
ENDFUNC

! Main test loop
BEGIN
    VAR key, running = TRUE
    
    ! Clear test results
    FOR i = 0 TO 25
        tested[i] = FALSE
    NEXT i
    testCount = 0
    waitingFor = 0
    sequenceKeys = 0
    
    ShowMenu()
    
    PRINT "Listening for keys..."
    PRINT
    
    WHILE running
        key = GetKey()
        
        ! ESC to exit
        IF key = 27 THEN
            running = FALSE
        ELSE
            ! Check if it's a WordStar key
            IF IsWordStarKey(key) THEN
                PRINT "DETECTED: Ctrl-"; GetKeyName(key); " (ASCII "; key; ")"
                MarkTested(key)
                
                ! Special handling for sequences
                IF key = 17 THEN  ! Ctrl-Q
                    PRINT "  Waiting for second key (S or D)..."
                ENDIF
                IF key = 11 THEN  ! Ctrl-K  
                    PRINT "  Waiting for second key (S, Q, or X)..."
                ENDIF
            ELSE
                ! Show non-WordStar keys but don't count them
                IF key >= 32 AND key <= 126 THEN
                    ! Printable character - ignore silently
                ELSE
                    PRINT "Non-WordStar key: ASCII "; key
                ENDIF
            ENDIF
        ENDIF
    WEND
    
    ShowResults()
    PRINT
    PRINT "Test complete. Press any key..."
    key = GetKey()
END
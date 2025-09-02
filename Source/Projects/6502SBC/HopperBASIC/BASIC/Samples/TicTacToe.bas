NEW
CLS
! Tic Tac Toe for Hopper BASIC (no INPUT)
! Uses direct keyboard reading from serial buffer

CONST InWritePtr = 0x0A   ! Serial buffer write position
CONST InReadPtr  = 0x0B   ! Serial buffer read position
CONST InBuffer   = 0x0200 ! actual Serial input buffer

! Board represented as CHAR array
! 0=empty, 1=X, 2=O
CHAR board[9]
VAR gameOver
VAR winner
VAR moves

! Check if key available
FUNC KeyReady()
    RETURN PEEK(InReadPtr) <> PEEK(InWritePtr)
ENDFUNC

! Get a key from buffer
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

! Clear the board
FUNC ClearBoard()
    FOR i = 0 TO 8
        board[i] = CHR(0)
    NEXT i
ENDFUNC

! Draw the board
FUNC DrawBoard()
    VAR ch
    PRINT
    PRINT "     1   2   3"
    PRINT "   +---+---+---+"
    
    FOR row = 0 TO 2
        PRINT " "; CHR(ASC('A') + row); " ";
        FOR col = 0 TO 2
            PRINT "|";
            ch = board[row * 3 + col]
            IF ch = 0 THEN
                PRINT "   ";
            ELSE
                IF ch = 1 THEN
                    PRINT " X ";
                ELSE
                    PRINT " O ";
                ENDIF
            ENDIF
        NEXT col
        PRINT "|"
        PRINT "   +---+---+---+"
    NEXT row
    PRINT
ENDFUNC

! Check for winner (returns 0=none, 1=X, 2=O)
FUNC CheckWinner()
    VAR p
    
    ! Check rows
    FOR row = 0 TO 2
        p = board[row * 3]
        IF p <> 0 THEN
            IF (board[row * 3 + 1] = p) AND (board[row * 3 + 2] = p) THEN
                RETURN p
            ENDIF
        ENDIF
    NEXT row
    
    ! Check columns
    FOR col = 0 TO 2
        p = board[col]
        IF p <> 0 THEN
            IF (board[3 + col] = p) AND (board[6 + col] = p) THEN
                RETURN p
            ENDIF
        ENDIF
    NEXT col
    
    ! Check diagonals
    p = board[4]
    IF p <> 0 THEN
        IF (board[0] = p) AND (board[8] = p) THEN
            RETURN p
        ENDIF
        IF (board[2] = p) AND (board[6] = p) THEN
            RETURN p
        ENDIF
    ENDIF
    
    RETURN 0
ENDFUNC

! Get player move
FUNC GetPlayerMove()
    VAR row, col, pos, key
    VAR valid = FALSE
    
    WHILE NOT valid
        PRINT "Enter move (A1-C3): ";
        
        ! Get row (A-C)
        key = GetKey()
        IF (key >= ASC('a')) AND (key <= ASC('c')) THEN
            key = key - 32  ! Convert to uppercase
        ENDIF
        PRINT CHR(key);
        
        IF (key >= ASC('A')) AND (key <= ASC('C')) THEN
            row = key - ASC('A')
            
            ! Get column (1-3)
            key = GetKey()
            PRINT CHR(key)
            
            IF (key >= ASC('1')) AND (key <= ASC('3')) THEN
                col = key - ASC('1')
                pos = row * 3 + col
                
                IF board[pos] = 0 THEN
                    valid = TRUE
                ELSE
                    PRINT "That position is taken!"
                ENDIF
            ELSE
                PRINT "Invalid column! Use 1-3"
            ENDIF
        ELSE
            PRINT
            PRINT "Invalid row! Use A-C"
        ENDIF
    WEND
    
    RETURN pos
ENDFUNC

! Simple computer move - just take first available
FUNC GetComputerMove()
    ! Try center first
    IF board[4] = 0 THEN
        RETURN 4
    ENDIF
    
    ! Try corners
    IF board[0] = 0 THEN RETURN 0 ENDIF
    IF board[2] = 0 THEN RETURN 2 ENDIF
    IF board[6] = 0 THEN RETURN 6 ENDIF
    IF board[8] = 0 THEN RETURN 8 ENDIF
    
    ! Take any available
    FOR i = 0 TO 8
        IF board[i] = 0 THEN
            RETURN i
        ENDIF
    NEXT i
    
    RETURN -1
ENDFUNC

! Main game
BEGIN
    VAR playAgain = TRUE
    VAR key, pos
    
    PRINT "TIC TAC TOE"
    PRINT "==========="
    PRINT
    PRINT "You are X, Computer is O"
    PRINT
    
    WHILE playAgain
        ClearBoard()
        gameOver = FALSE
        winner = 0
        moves = 0
        
        DrawBoard()
        
        WHILE NOT gameOver
            ! Player move
            pos = GetPlayerMove()
            board[pos] = 1  ! X
            moves = moves + 1
            DrawBoard()
            
            ! Check for win or draw
            winner = CheckWinner()
            IF winner <> 0 THEN
                gameOver = TRUE
            ELSE
                IF moves = 9 THEN
                    gameOver = TRUE
                ELSE
                    ! Computer move
                    PRINT "Computer is thinking..."
                    DELAY(500)
                    pos = GetComputerMove()
                    IF pos >= 0 THEN
                        board[pos] = 2  ! O
                        moves = moves + 1
                        DrawBoard()
                        
                        winner = CheckWinner()
                        IF winner <> 0 THEN
                            gameOver = TRUE
                        ELSE
                            IF moves = 9 THEN
                                gameOver = TRUE
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        WEND
        
        ! Game over
        IF winner = 1 THEN
            PRINT "You win!"
        ELSE
            IF winner = 2 THEN
                PRINT "Computer wins!"
            ELSE
                PRINT "It's a draw!"
            ENDIF
        ENDIF
        
        PRINT
        PRINT "Play again? (Y/N): ";
        key = GetKey()
        PRINT CHR(key)
        
        IF (key = ASC('n')) OR (key = ASC('N')) THEN
            playAgain = FALSE
        ENDIF
        PRINT
    WEND
    
    PRINT "Thanks for playing!"
END

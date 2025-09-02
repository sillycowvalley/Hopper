! Tic Tac Toe for Hopper BASIC

CONST InWritePtr = 0x0A   ! Serial buffer write position
CONST InReadPtr  = 0x0B   ! Serial buffer read position
CONST InBuffer   = 0x0200 ! actual Serial input buffer

! Board represented as BYTE array
! 0=empty, 1=X, 2=O
BYTE board[9]
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
        board[i] = 0
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

! Check if either player can still win
FUNC CanEitherPlayerWin()
    ! Check all 8 winning lines: 3 rows, 3 columns, 2 diagonals
    
    ! Check rows
    FOR row = 0 TO 2
        IF CanPlayerWinLine(row * 3, row * 3 + 1, row * 3 + 2) THEN
            RETURN TRUE
        ENDIF
    NEXT row
    
    ! Check columns  
    FOR col = 0 TO 2
        IF CanPlayerWinLine(col, col + 3, col + 6) THEN
            RETURN TRUE
        ENDIF
    NEXT col
    
    ! Check diagonals
    IF CanPlayerWinLine(0, 4, 8) THEN RETURN TRUE ENDIF  ! Top-left to bottom-right
    IF CanPlayerWinLine(2, 4, 6) THEN RETURN TRUE ENDIF  ! Top-right to bottom-left
    
    RETURN FALSE  ! No winning moves possible for either player
ENDFUNC

! Check if either player can win on a specific line of 3 positions
FUNC CanPlayerWinLine(pos1, pos2, pos3)
    VAR xCount = 0
    VAR oCount = 0
    
    ! Count X's and O's in this line
    IF board[pos1] = 1 THEN xCount = xCount + 1 ENDIF
    IF board[pos1] = 2 THEN oCount = oCount + 1 ENDIF
    
    IF board[pos2] = 1 THEN xCount = xCount + 1 ENDIF
    IF board[pos2] = 2 THEN oCount = oCount + 1 ENDIF
    
    IF board[pos3] = 1 THEN xCount = xCount + 1 ENDIF
    IF board[pos3] = 2 THEN oCount = oCount + 1 ENDIF
    
    ! A line is winnable by X if it has no O's (only X's and empty spaces)
    ! A line is winnable by O if it has no X's (only O's and empty spaces)
    RETURN (xCount > 0 AND oCount = 0) OR (oCount > 0 AND xCount = 0)
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

! Smarter computer move
FUNC GetComputerMove()
    VAR pos
    
    ! 1. Check if computer can win
    pos = FindWinningMove(2)  ! 2 = O (computer)
    IF pos >= 0 THEN
        RETURN pos
    ENDIF
    
    ! 2. Check if player is about to win and block
    pos = FindWinningMove(1)  ! 1 = X (player)
    IF pos >= 0 THEN
        RETURN pos
    ENDIF
    
    ! 3. Try center
    IF board[4] = 0 THEN
        RETURN 4
    ENDIF
    
    ! 4. Try corners
    IF board[0] = 0 THEN RETURN 0 ENDIF
    IF board[2] = 0 THEN RETURN 2 ENDIF
    IF board[6] = 0 THEN RETURN 6 ENDIF
    IF board[8] = 0 THEN RETURN 8 ENDIF
    
    ! 5. Try edges
    IF board[1] = 0 THEN RETURN 1 ENDIF
    IF board[3] = 0 THEN RETURN 3 ENDIF
    IF board[5] = 0 THEN RETURN 5 ENDIF
    IF board[7] = 0 THEN RETURN 7 ENDIF
    
    RETURN -1
ENDFUNC

! Find a winning move for the given player
FUNC FindWinningMove(player)
    VAR i
    
    ! Try each empty position
    FOR i = 0 TO 8
        IF board[i] = 0 THEN
            ! Temporarily place piece
            board[i] = player
            
            ! Check if this creates a win
            IF CheckWinner() = player THEN
                ! Undo the move and return this position
                board[i] = 0
                RETURN i
            ENDIF
            
            ! Undo the temporary move
            board[i] = 0
        ENDIF
    NEXT i
    
    RETURN -1  ! No winning move found
ENDFUNC

! Play one turn (player move + check + computer move + check)
FUNC PlayPlayerFirst()
    VAR pos
    ! Player move
    pos = GetPlayerMove()
    board[pos] = 1  ! X
    moves = moves + 1
    DrawBoard()
    
    ! Check for win
    winner = CheckWinner()
    IF winner <> 0 THEN
        gameOver = TRUE
        RETURN
    ENDIF
    
    ! Check for early draw
    IF NOT CanEitherPlayerWin() THEN
        gameOver = TRUE
        RETURN
    ENDIF
    
    IF moves = 9 THEN
        gameOver = TRUE
        RETURN
    ENDIF
    
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
            ! Check for early draw after computer move too
            IF NOT CanEitherPlayerWin() THEN
                gameOver = TRUE
            ELSE
                IF moves = 9 THEN
                    gameOver = TRUE
                ENDIF
            ENDIF
        ENDIF
    ENDIF
ENDFUNC

! Play one turn (computer move + check + player move + check)
FUNC PlayComputerFirst()
    VAR pos
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
            RETURN
        ENDIF
        
        ! Check for early draw
        IF NOT CanEitherPlayerWin() THEN
            gameOver = TRUE
            RETURN
        ENDIF
        
        IF moves = 9 THEN
            gameOver = TRUE
            RETURN
        ENDIF
        
        ! Player move
        pos = GetPlayerMove()
        board[pos] = 1  ! X
        moves = moves + 1
        DrawBoard()
        
        winner = CheckWinner()
        IF winner <> 0 THEN
            gameOver = TRUE
        ELSE
            ! Check for early draw after player move too
            IF NOT CanEitherPlayerWin() THEN
                gameOver = TRUE
            ELSE
                IF moves = 9 THEN
                    gameOver = TRUE
                ENDIF
            ENDIF
        ENDIF
    ENDIF
ENDFUNC

! Main game
BEGIN
    VAR playAgain = TRUE
    VAR key
    VAR playerStarts
    VAR firstGame = TRUE
    
    PRINT "TIC TAC TOE"
    PRINT "==========="
    PRINT
    
    WHILE playAgain
        ! Determine who starts
        IF firstGame THEN
            playerStarts = RND(2) = 1
            firstGame = FALSE
        ELSE
            playerStarts = NOT playerStarts
        ENDIF
        
        IF playerStarts THEN
            PRINT "You are X and go first, Computer is O"
        ELSE
            PRINT "Computer is O and goes first, You are X"
        ENDIF
        PRINT
        
        ClearBoard()
        gameOver = FALSE
        winner = 0
        moves = 0
        DrawBoard()
        
        ! Play the game
        WHILE NOT gameOver
            IF playerStarts THEN
                PlayPlayerFirst()
            ELSE
                PlayComputerFirst()
            ENDIF
        WEND
        
        ! Show result
        IF winner = 1 THEN
            PRINT "You win!"
        ELSE
            IF winner = 2 THEN
                PRINT "Computer wins!"
            ELSE
                IF moves = 9 THEN
                    PRINT "It's a draw!"
                ELSE
                    PRINT "Draw detected early! No winning moves left."
                ENDIF
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
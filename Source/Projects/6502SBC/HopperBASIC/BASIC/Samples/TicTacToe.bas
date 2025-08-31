NEW

BYTE board[9]
VAR turn
VAR moves

FUNC Init()
    VAR i
    FOR i = 0 TO 8
        board[i] = ASC(' ')
    NEXT i
    turn = ASC('X')
    moves = 0
ENDFUNC

FUNC Show()
    PRINT
    PRINT " "; CHR(board[0]); " | "; CHR(board[1]); " | "; CHR(board[2])
    PRINT "-----------"
    PRINT " "; CHR(board[3]); " | "; CHR(board[4]); " | "; CHR(board[5])
    PRINT "-----------"
    PRINT " "; CHR(board[6]); " | "; CHR(board[7]); " | "; CHR(board[8])
    PRINT
    PRINT "Use 1-9 to play:"
    PRINT " 1 | 2 | 3"
    PRINT " 4 | 5 | 6"
    PRINT " 7 | 8 | 9"
    PRINT
ENDFUNC

FUNC Win()
    VAR b0 = board[0]
    VAR b1 = board[1]
    VAR b2 = board[2]
    VAR b3 = board[3]
    VAR b4 = board[4]
    VAR b5 = board[5]
    VAR b6 = board[6]
    VAR b7 = board[7]
    VAR b8 = board[8]
    VAR s = ASC(' ')
    
    ! Rows
    IF b0 = b1 AND b1 = b2 AND b0 <> s THEN
        RETURN b0
    ENDIF
    IF b3 = b4 AND b4 = b5 AND b3 <> s THEN
        RETURN b3
    ENDIF
    IF b6 = b7 AND b7 = b8 AND b6 <> s THEN
        RETURN b6
    ENDIF
    
    ! Cols
    IF b0 = b3 AND b3 = b6 AND b0 <> s THEN
        RETURN b0
    ENDIF
    IF b1 = b4 AND b4 = b7 AND b1 <> s THEN
        RETURN b1
    ENDIF
    IF b2 = b5 AND b5 = b8 AND b2 <> s THEN
        RETURN b2
    ENDIF
    
    ! Diags
    IF b0 = b4 AND b4 = b8 AND b0 <> s THEN
        RETURN b0
    ENDIF
    IF b2 = b4 AND b4 = b6 AND b2 <> s THEN
        RETURN b2
    ENDIF
    
    RETURN 0
ENDFUNC

FUNC Play()
    VAR pos
    VAR key
    VAR w
    
    Init()
    
    WHILE moves < 9
        Show()
        PRINT "Player "; CHR(turn); " move: ";
        key = INPUT()
        
        IF key >= 1 AND key <= 9 THEN
            pos = key - 1
            IF board[pos] = ASC(' ') THEN
                board[pos] = turn
                moves = moves + 1
                
                w = Win()
                IF w <> 0 THEN
                    Show()
                    PRINT "Player "; CHR(w); " wins!"
                    RETURN
                ENDIF
                
                IF turn = ASC('X') THEN
                    turn = ASC('O')
                ELSE
                    turn = ASC('X')
                ENDIF
            ELSE
                PRINT "Position taken!"
            ENDIF
        ELSE
            PRINT "Invalid move!"
        ENDIF
    WEND
    
    Show()
    PRINT "It's a draw!"
ENDFUNC

FUNC Comp()
    VAR i
    VAR o = ASC('O')
    VAR x = ASC('X')
    VAR s = ASC(' ')
    
    ! Try to win
    FOR i = 0 TO 8
        IF board[i] = s THEN
            board[i] = o
            IF Win() = o THEN
                RETURN
            ENDIF
            board[i] = s
        ENDIF
    NEXT i
    
    ! Block player
    FOR i = 0 TO 8
        IF board[i] = s THEN
            board[i] = x
            IF Win() = x THEN
                board[i] = o
                RETURN
            ENDIF
            board[i] = s
        ENDIF
    NEXT i
    
    ! Take center
    IF board[4] = s THEN
        board[4] = o
        RETURN
    ENDIF
    
    ! Take corner
    IF board[0] = s THEN
        board[0] = o
        RETURN
    ENDIF
    IF board[2] = s THEN
        board[2] = o
        RETURN
    ENDIF
    IF board[6] = s THEN
        board[6] = o
        RETURN
    ENDIF
    IF board[8] = s THEN
        board[8] = o
        RETURN
    ENDIF
    
    ! Take any
    FOR i = 0 TO 8
        IF board[i] = s THEN
            board[i] = o
            RETURN
        ENDIF
    NEXT i
ENDFUNC

FUNC Play2()
    VAR pos
    VAR key
    VAR w
    
    Init()
    turn = ASC('X')
    
    WHILE moves < 9
        Show()
        
        IF turn = ASC('X') THEN
            PRINT "Your move: ";
            key = INPUT()
            
            IF key >= 1 AND key <= 9 THEN
                pos = key - 1
                IF board[pos] = ASC(' ') THEN
                    board[pos] = turn
                    moves = moves + 1
                    turn = ASC('O')
                ELSE
                    PRINT "Position taken!"
                ENDIF
            ELSE
                PRINT "Invalid move!"
            ENDIF
        ELSE
            PRINT "Computer thinking..."
            Comp()
            moves = moves + 1
            turn = ASC('X')
        ENDIF
        
        w = Win()
        IF w <> 0 THEN
            Show()
            IF w = ASC('X') THEN
                PRINT "You win!"
            ELSE
                PRINT "Computer wins!"
            ENDIF
            RETURN
        ENDIF
    WEND
    
    Show()
    PRINT "It's a draw!"
ENDFUNC

BEGIN
    PRINT "TIC TAC TOE"
    PRINT "==========="
    PRINT "1) Human vs Human"
    PRINT "2) Human vs Computer"
    PRINT "Choose: ";
    VAR mode = INPUT()
    
    IF mode = 1 THEN
        Play()
    ELSE
        Play2()
    ENDIF
END

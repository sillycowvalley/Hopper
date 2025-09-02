BYTE board[9]

FUNC DRAWBOARD()
     VAR CH
     PRINT
     PRINT "     1   2   3"
     PRINT "   +---+---+---+"
     FOR ROW = 0 TO 2
         PRINT " "; CHR(ASC('A') + ROW); " ";
         FOR COL = 0 TO 2
             PRINT "|";
             CH = BOARD[ROW * 3 + COL]
         NEXT COL
     NEXT ROW
ENDFUNC

DRAWBOARD()

FUNC GetByte(bufpos)
    VAR b = BOARD[bufpos]
    bufpos = bufpos + 1
    RETURN b
ENDFUNC

print "=", GetByte(0), "="


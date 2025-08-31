NEW

FUNC HexB(b)
    VAR h = b / 16
    VAR l = b & 15
    VAR c
    
    IF h < 10 THEN
        c = h + ASC('0')
    ELSE
        c = h - 10 + ASC('A')
    ENDIF
    PRINT CHR(c);
    
    IF l < 10 THEN
        c = l + ASC('0')
    ELSE
        c = l - 10 + ASC('A')
    ENDIF
    PRINT CHR(c);
ENDFUNC

FUNC HexW(w)
    HexB(w / 256)
    HexB(w & 255)
ENDFUNC

FUNC PStr(a)
    VAR c
    VAR o = 11
    
    DO
        c = PEEK(a + o)
        IF c <> 0 THEN
            PRINT CHR(c);
        ENDIF
        o = o + 1
    UNTIL c = 0
ENDFUNC

FUNC DmpF(a)
    VAR nL = PEEK(a + 0)
    VAR nH = PEEK(a + 1)
    VAR n = nL + (nH * 256)
    
    VAR t = PEEK(a + 2)
    
    VAR tkL = PEEK(a + 3)
    VAR tkH = PEEK(a + 4)
    VAR tk = tkL + (tkH * 256)
    
    VAR arL = PEEK(a + 5)
    VAR arH = PEEK(a + 6)
    VAR ar = arL + (arH * 256)
    
    VAR opL = PEEK(a + 9)
    VAR opH = PEEK(a + 10)
    VAR op = opL + (opH * 256)
    
    PRINT "  ";
    HexW(a)
    PRINT " ";
    PStr(a)
    PRINT " [";
    HexB(t)
    PRINT " tk:";
    HexW(tk)
    PRINT " ar:";
    HexW(ar)
    PRINT " op:";
    HexW(op)
    PRINT "]"
    
    RETURN n
ENDFUNC

FUNC StrEq(a1, s2)
    VAR o = 11
    VAR i = 0
    VAR c1, c2
    VAR sz = LEN(s2)
    
    DO
        c1 = PEEK(a1 + o)
        IF i >= sz THEN
            c2 = 0
        ELSE
            c2 = ASC(s2[i])
        ENDIF
        
        IF c1 <> c2 THEN
            RETURN FALSE
        ENDIF
        
        IF c1 = 0 THEN
            RETURN TRUE
        ENDIF
        
        o = o + 1
        i = i + 1
    UNTIL FALSE
ENDFUNC

FUNC DASM(name)
    VAR hL = PEEK(0x3E)
    VAR hH = PEEK(0x3F)
    VAR c = hL + (hH * 256)
    VAR cnt = 0
    VAR all = (name = "*")
    VAR nL, nH
    
    IF c = 0 THEN
        PRINT "None"
        RETURN
    ENDIF
    
    IF all THEN
        PRINT "Functions @ ";
        HexW(c)
        PRINT ":"
    ENDIF
    
    WHILE c <> 0
        IF all THEN
            cnt = cnt + 1
            c = DmpF(c)
            IF cnt > 30 THEN
                PRINT "..."
                RETURN
            ENDIF
        ELSE
            IF StrEq(c, name) THEN
                DmpF(c)
                RETURN
            ENDIF
            nL = PEEK(c + 0)
            nH = PEEK(c + 1)
            c = nL + (nH * 256)
        ENDIF
    WEND
    
    IF all THEN
        PRINT cnt; " total"
    ELSE
        PRINT "Not found: "; name
    ENDIF
ENDFUNC



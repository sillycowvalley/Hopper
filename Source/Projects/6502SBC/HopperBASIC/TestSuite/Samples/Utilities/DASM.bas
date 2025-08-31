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

FUNC PrOp00(op)
    VAR n = "?"
    
    IF op = 0x00 THEN n = "INVALID" ENDIF
    IF op = 0x01 THEN n = "ADD" ENDIF
    IF op = 0x02 THEN n = "SUB" ENDIF
    IF op = 0x03 THEN n = "MUL" ENDIF
    IF op = 0x04 THEN n = "DIV" ENDIF
    IF op = 0x05 THEN n = "MOD" ENDIF
    IF op = 0x06 THEN n = "NEG" ENDIF
    IF op = 0x07 THEN n = "BITWISE_AND" ENDIF
    IF op = 0x08 THEN n = "BITWISE_OR" ENDIF
    IF op = 0x09 THEN n = "BITWISE_NOT" ENDIF
    IF op = 0x0A THEN n = "LOGICAL_AND" ENDIF
    IF op = 0x0B THEN n = "LOGICAL_OR" ENDIF
    IF op = 0x0C THEN n = "LOGICAL_NOT" ENDIF
    IF op = 0x0D THEN n = "EQ" ENDIF
    IF op = 0x0E THEN n = "NE" ENDIF
    IF op = 0x0F THEN n = "LT" ENDIF
    IF op = 0x10 THEN n = "GT" ENDIF
    IF op = 0x11 THEN n = "LE" ENDIF
    IF op = 0x12 THEN n = "GE" ENDIF
    IF op = 0x13 THEN n = "DUP" ENDIF
    IF op = 0x14 THEN n = "NOP" ENDIF
    IF op = 0x15 THEN n = "PUSH0" ENDIF
    IF op = 0x16 THEN n = "PUSH1" ENDIF
    IF op = 0x17 THEN n = "PUSHVOID" ENDIF
    IF op = 0x18 THEN n = "PUSHLONG0" ENDIF
    IF op = 0x19 THEN n = "HALT" ENDIF
    IF op = 0x1A THEN n = "ENTER" ENDIF
    IF op = 0x1B THEN n = "PUSHEMPTYVAR" ENDIF
    IF op = 0x1C THEN n = "GETITEM" ENDIF
    IF op = 0x1D THEN n = "SETITEM" ENDIF
    
    PRINT n;
ENDFUNC

FUNC PrOp40(op)
    VAR n = "?"
    
    IF op = 0x40 THEN n = "PUSHBIT" ENDIF
    IF op = 0x41 THEN n = "PUSHBYTE" ENDIF
    IF op = 0x42 THEN n = "PUSHCHAR" ENDIF
    IF op = 0x43 THEN n = "PUSHLOCAL" ENDIF
    IF op = 0x44 THEN n = "POPLOCAL" ENDIF
    IF op = 0x48 THEN n = "SYSCALL" ENDIF
    IF op = 0x49 THEN n = "RETURN" ENDIF
    IF op = 0x4A THEN n = "RETURNVAL" ENDIF
    IF op = 0x4B THEN n = "PUSHGLOBAL" ENDIF
    IF op = 0x4C THEN n = "POPGLOBAL" ENDIF
    IF op = 0x4D THEN n = "INCGLOBAL" ENDIF
    IF op = 0x4E THEN n = "INCLOCAL" ENDIF
    IF op = 0x4F THEN n = "DECSP" ENDIF
    
    PRINT n;
ENDFUNC

FUNC PrOp80(op)
    VAR n = "?"
    
    IF op = 0x80 THEN n = "PUSHINT" ENDIF
    IF op = 0x81 THEN n = "PUSHWORD" ENDIF
    IF op = 0x82 THEN n = "PUSHCSTRING" ENDIF
    IF op = 0x83 THEN n = "CALL" ENDIF
    IF op = 0x84 THEN n = "CALLF" ENDIF
    IF op = 0x87 THEN n = "JUMPW" ENDIF
    IF op = 0x88 THEN n = "JUMPZW" ENDIF
    IF op = 0x8A THEN n = "ADDLOCALS" ENDIF
    IF op = 0x8B THEN n = "ADDGLOBALS" ENDIF
    IF op = 0x8C THEN n = "GETITEMGG" ENDIF
    IF op = 0x8D THEN n = "GETITEMGL" ENDIF
    IF op = 0x8E THEN n = "GETITEMLG" ENDIF
    IF op = 0x8F THEN n = "GETITEMLL" ENDIF
    IF op = 0x90 THEN n = "SETITEMGG" ENDIF
    IF op = 0x91 THEN n = "SETITEMGL" ENDIF
    IF op = 0x92 THEN n = "SETITEMLG" ENDIF
    IF op = 0x93 THEN n = "SETITEMLL" ENDIF
    IF op = 0x94 THEN n = "PUSHLONG" ENDIF
    
    PRINT n;
ENDFUNC

FUNC PrOpC0(op)
    VAR n = "?"
    
    IF op = 0xC0 THEN n = "FORCHK" ENDIF
    IF op = 0xC1 THEN n = "FORIT" ENDIF
    IF op = 0xC2 THEN n = "FORITF" ENDIF
    
    PRINT n;
ENDFUNC

FUNC PrOpc(addr, opc)
    PRINT "    ";
    HexW(addr)
    PRINT ": ";
    HexB(opc)
ENDFUNC

FUNC PrOpr(cnt, op, pc)
    VAR op1, op2, op3
    VAR opc = PEEK(op + pc - 1)
    
    IF cnt = 0 THEN
        PRINT "       ";
        RETURN pc
    ENDIF
    
    op1 = PEEK(op + pc)
    pc = pc + 1
    PRINT " ";
    HexB(op1)
    
    IF cnt = 1 THEN
        PRINT "    ";
        RETURN pc
    ENDIF
    
    op2 = PEEK(op + pc)
    pc = pc + 1
    PRINT " ";
    HexB(op2)
    
    IF cnt = 2 THEN
        PRINT " ";
        RETURN pc
    ENDIF
    
    op3 = PEEK(op + pc)
    pc = pc + 1
    PRINT " ";
    HexB(op3)
    
    RETURN pc
ENDFUNC

FUNC DecOpr(opc, op, pc, cnt)
    VAR op1, op2
    VAR val
    VAR fL, fH, f
    
    IF cnt = 0 THEN
        RETURN
    ENDIF
    
    op1 = PEEK(op + pc)
    
    IF cnt = 1 THEN
        IF opc = 0x43 THEN
            ! PUSHLOCAL
            PRINT "  [BP";
            IF op1 > 127 THEN
                PRINT "-";
                HexB(256 - op1)
            ELSE
                PRINT "+";
                HexB(op1)
            ENDIF
            PRINT "]";
        ENDIF
        
        IF opc = 0x41 THEN
            ! PUSHBYTE
            PRINT "  (";
            PRINT op1;
            PRINT ")";
        ENDIF
        RETURN
    ENDIF
    
    op2 = PEEK(op + pc + 1)
    val = op1 + (op2 * 256)
    
    IF opc = 0x80 THEN
        ! PUSHINT
        PRINT "  (";
        PRINT val;
        PRINT ")";
    ENDIF
    
    IF opc = 0x84 THEN
        ! CALLF
        fL = PEEK(0x3E)
        fH = PEEK(0x3F)
        f = fL + (fH * 256)
        
        WHILE f <> 0
            IF f = val THEN
                PRINT "  (";
                PStr(f)
                PRINT ")";
                RETURN
            ENDIF
            fL = PEEK(f)
            fH = PEEK(f + 1)
            f = fL + (fH * 256)
        WEND
    ENDIF
ENDFUNC

FUNC DmpOp(a)
    VAR opL = PEEK(a + 9)
    VAR opH = PEEK(a + 10)
    VAR op = opL + (opH * 256)
    VAR pc = 0
    VAR opc, cnt
    VAR addr
    VAR oldpc
    
    IF op = 0 THEN 
        PRINT "    Not compiled"
        RETURN 
    ENDIF
    
    PRINT "    OpCode @ ";
    HexW(op)
    PRINT ":"
    
    WHILE pc < 100
        addr = op + pc
        opc = PEEK(addr)
        
        IF opc = 0 THEN
            PrOpc(addr, opc)
            PRINT "       INVALID!"
            RETURN
        ENDIF
        
        PrOpc(addr, opc)
        pc = pc + 1
        oldpc = pc
        
        cnt = opc / 64
        pc = PrOpr(cnt, op, pc)
        
        PRINT " ";
        IF cnt = 0 THEN
            PrOp00(opc)
        ENDIF
        IF cnt = 1 THEN
            PrOp40(opc)
        ENDIF
        IF cnt = 2 THEN
            PrOp80(opc)
        ENDIF
        IF cnt = 3 THEN
            PrOpC0(opc)
        ENDIF
        
        DecOpr(opc, op, oldpc, cnt)
        PRINT
        
        IF opc = 0x19 THEN
            RETURN
        ENDIF
        IF opc = 0x49 THEN
            RETURN
        ENDIF
        IF opc = 0x4A THEN
            RETURN
        ENDIF
    WEND
    
    PRINT "    ... truncated"
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
    
    IF op <> 0 THEN
        DmpOp(a)
    ENDIF
    
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

FUNC DSM(name)
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



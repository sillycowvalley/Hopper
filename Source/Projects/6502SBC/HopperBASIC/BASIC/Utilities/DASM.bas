NEW

CONST NAMELEN = 12
CHAR Op00Names[0]  ! Opcodes 0x00-0x1D (30 entries)
CHAR Op40Names[0]  ! Opcodes 0x40-0x4F (16 entries)
CHAR Op80Names[0]  ! Opcodes 0x80-0x94 (21 entries)
CHAR OpC0Names[0]  ! Opcodes 0xC0-0xC2 (3 entries)
VAR OpcodesLoaded = FALSE

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

FUNC LoadOpcodes()
    VAR count
    
    IF OpcodesLoaded THEN
        RETURN TRUE
    ENDIF
    
    ! Load the 4 opcode tables
    count = IMPORT(Op00Names, "OPCODES00")
    IF count = 0 THEN
        PRINT "Error: OPCODES00 file not found!"
        RETURN FALSE
    ENDIF
    
    count = IMPORT(Op40Names, "OPCODES40")
    IF count = 0 THEN
        PRINT "Error: OPCODES40 file not found!"
        RETURN FALSE
    ENDIF
    
    count = IMPORT(Op80Names, "OPCODES80")
    IF count = 0 THEN
        PRINT "Error: OPCODES80 file not found!"
        RETURN FALSE
    ENDIF
    
    count = IMPORT(OpC0Names, "OPCODESC0")
    IF count = 0 THEN
        PRINT "Error: OPCODESC0 file not found!"
        RETURN FALSE
    ENDIF
    
    OpcodesLoaded = TRUE
    RETURN TRUE
ENDFUNC

FUNC PrOpName(opc)
    VAR idx
    VAR c
    VAR printed = FALSE
    
    ! Select the right array based on opcode range
    IF opc <= 0x1D THEN
        idx = opc * NAMELEN
        FOR i = 0 TO NAMELEN - 1
            c = Op00Names[idx + i]
            IF c <> ' ' THEN
                PRINT c;
                printed = TRUE
            ELSE
                IF printed THEN
                    RETURN
                ENDIF
            ENDIF
        NEXT i
        IF NOT printed THEN
            PRINT "?";
        ENDIF
        RETURN
    ENDIF
    
    IF (opc >= 0x40) AND (opc <= 0x4F) THEN
        idx = (opc - 0x40) * NAMELEN
        FOR i = 0 TO NAMELEN - 1
            c = Op40Names[idx + i]
            IF c <> ' ' THEN
                PRINT c;
                printed = TRUE
            ELSE
                IF printed THEN
                    RETURN
                ENDIF
            ENDIF
        NEXT i
        IF NOT printed THEN
            PRINT "?";
        ENDIF
        RETURN
    ENDIF
    
    IF (opc >= 0x80) AND (opc <= 0x94) THEN
        idx = (opc - 0x80) * NAMELEN
        FOR i = 0 TO NAMELEN - 1
            c = Op80Names[idx + i]
            IF c <> ' ' THEN
                PRINT c;
                printed = TRUE
            ELSE
                IF printed THEN
                    RETURN
                ENDIF
            ENDIF
        NEXT i
        IF NOT printed THEN
            PRINT "?";
        ENDIF
        RETURN
    ENDIF
    
    IF (opc >= 0xC0) AND (opc <= 0xC2) THEN
        idx = (opc - 0xC0) * NAMELEN
        FOR i = 0 TO NAMELEN - 1
            c = OpC0Names[idx + i]
            IF c <> ' ' THEN
                PRINT c;
                printed = TRUE
            ELSE
                IF printed THEN
                    RETURN
                ENDIF
            ENDIF
        NEXT i
        IF NOT printed THEN
            PRINT "?"; 
        ENDIF
        RETURN
    ENDIF
    
    ! Unknown opcode range
    PRINT "?";
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
        PrOpName(opc)
        
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
            c2 = ASC(s2[i])  ! Need ASC here because PEEK returns LONG
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
    VAR hL, hH, c
    VAR cnt = 0
    VAR all = (name = "*")
    VAR nL, nH
    
    ! Load opcode tables if not already loaded
    IF NOT LoadOpcodes() THEN
        RETURN
    ENDIF
    
    hL = PEEK(0x3E)
    hH = PEEK(0x3F)
    c = hL + (hH * 256)
    
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

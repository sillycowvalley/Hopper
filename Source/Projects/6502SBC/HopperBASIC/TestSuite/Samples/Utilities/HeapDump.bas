FUNC HexNibble(n)
    IF n < 10 THEN
        RETURN CHR(n + 48)
    ELSE
        RETURN CHR(n + 55)
    ENDIF
ENDFUNC

FUNC HexByte(b)
    PRINT HexNibble(b / 16);
    PRINT HexNibble(b MOD 16);
ENDFUNC

FUNC PrintBlockData(addr, size)
    VAR b
    VAR i
    VAR maxBytes = size - 2
    IF maxBytes > 8 THEN
        maxBytes = 8
    ENDIF
    
    FOR i = 0 TO 7
        IF i < maxBytes THEN
            b = PEEK(addr + 2 + i)
            HexByte(b)
            PRINT " ";
        ELSE
            PRINT "   ";
        ENDIF
    NEXT i
    
    PRINT " ";
    FOR i = 0 TO 7
        IF i < maxBytes THEN
            b = PEEK(addr + 2 + i)
            IF (b >= 32) AND (b <= 126) THEN
                PRINT CHR(b);
            ELSE
                PRINT ".";
            ENDIF
        ENDIF
    NEXT i
ENDFUNC

FUNC HeapDump()
    VAR heapPage = PEEK(0x08)
    VAR heapPages = PEEK(0x09)
    VAR addr = heapPage * 256
    VAR heapEnd = addr + (heapPages * 256)
    VAR size
    VAR sizeL
    VAR sizeH
    VAR blockType
    VAR blockCount = 0
    
    PRINT ""
    PRINT "== HEAP DUMP =="
    PRINT "Heap: ";
    HexByte(addr / 256)
    HexByte(addr MOD 256)
    PRINT " Size: ";
    HexByte(heapPages)
    PRINT " pages"
    
    WHILE blockCount < 20
        sizeL = PEEK(addr)
        sizeH = PEEK(addr + 1)
        size = sizeL + (sizeH * 256)
        
        IF size = 0 THEN
            PRINT "END"
            RETURN
        ENDIF
        
        blockType = size & 1
        size = size & 0xFFFE
        
        IF size < 2 THEN
            PRINT "CORRUPTED?"
            RETURN
        ENDIF
        
        HexByte(addr / 256)
        HexByte(addr MOD 256)
        PRINT " ";
        HexByte(size / 256)
        HexByte(size MOD 256)
        PRINT " ";
        
        IF blockType = 1 THEN
            PRINT "FREE ";
        ELSE
            PRINT "USED ";
        ENDIF
        
        PrintBlockData(addr, size)
        PRINT ""
        
        addr = addr + size
        blockCount = blockCount + 1
        
        IF addr >= heapEnd THEN
            PRINT "..."
            RETURN
        ENDIF
    WEND
    
    PRINT "..."
ENDFUNC


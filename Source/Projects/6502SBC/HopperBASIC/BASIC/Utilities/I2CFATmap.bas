CONST EEPROM = 80       ! I2C address (0x50)
VAR free
VAR used
VAR reserved
VAR chains
BYTE buffer[256]

FUNC Hex(val)
    VAR h = val / 16
    VAR l = val & 15
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

FUNC DumpBuffer()
    VAR idx
    PRINT "Buffer contents (256 bytes):"
    FOR row = 0 TO 15
        ! Print address
        Hex(row * 16)
        PRINT ": ";
        
        ! Print 16 hex bytes
        FOR col = 0 TO 15
            idx = (row * 16) + col
            Hex(buffer[idx])
            PRINT " ";
        NEXT col
        PRINT
    NEXT row
ENDFUNC

FUNC ReadSector(sector)
    VAR addrHi = sector  
    VAR addrLo = 0       
    VAR result
    VAR bytes
    
    ! First half (128 bytes)
    I2CBEGIN(EEPROM)
    I2CPUT(addrHi)
    I2CPUT(addrLo)
    result = I2CEND()
    
    DELAY(5)
    bytes = I2CGET(EEPROM, 128)
    FOR i = 0 TO 127
        buffer[i] = I2CNEXT()
    NEXT i
    
    ! Second half (128 bytes at offset 128)
    addrLo = 128
    
    I2CBEGIN(EEPROM)
    I2CPUT(addrHi)
    I2CPUT(addrLo)
    result = I2CEND()
    
    DELAY(5)
    bytes = I2CGET(EEPROM, 128)
    FOR i = 0 TO 127
        buffer[i + 128] = I2CNEXT()
    NEXT i
ENDFUNC

FUNC PrintFATMap()
    PRINT "FAT Sector Map (256 sectors):"
    PRINT "  Legend: . = Free, F = FAT, D = Directory, E = End-of-chain, * = Chain link"
    PRINT
    PRINT "     0123456789ABCDEF"
    PRINT "     ----------------"
    
    VAR idx
    VAR val
    
    FOR row = 0 TO 15
        ! Print row header
        Hex(row * 16)
        PRINT " : ";
        
        ! Print 16 sectors
        FOR col = 0 TO 15
            idx = (row * 16) + col
            val = buffer[idx]
            
            IF val = 0 THEN
                PRINT ".";    ! Free
                free = free + 1
            ELSE
                IF val = 1 THEN
                    IF idx < 2 THEN
                        if IDX = 0 THEN
                            PRINT "F";    ! FAT
                        ELSE
                            PRINT "D";    ! Directory
                        ENDIF
                        reserved = reserved + 1
                    ELSE
                        PRINT "E";    ! End of chain
                        chains = chains + 1
                    ENDIF
                ELSE
                    PRINT "*";    ! Part of chain
                    used = used + 1
                ENDIF
            ENDIF
        NEXT col
        
        ! Print sector numbers at end of row
        PRINT "  [";
        Hex(row * 16)
        PRINT "-";
        Hex((row * 16) + 15)
        PRINT "]"
    NEXT row
ENDFUNC

FUNC PrintChainDetails()
    PRINT
    PRINT "File Chains:"
    PRINT "------------"
    VAR found
    VAR start
    VAR curr
    
    ! Find all end-of-chain markers and trace back
    VAR filenum = 0
    FOR i = 2 TO 255
        IF buffer[i] = 1 THEN
            ! Found end of chain, trace it back
            filenum = filenum + 1
            PRINT "File "; filenum; ": ";
            
            ! Find which sector(s) point to this one
            found = 0
            start = i
            
            ! First, find the start of this chain
            WHILE found = 0
                found = 1
                FOR j = 2 TO 255
                    IF buffer[j] = start THEN
                        start = j
                        found = 0
                        j = 256  ! Exit FOR loop
                    ENDIF
                NEXT j
            WEND
            
            ! Now print the chain from start to end
            curr = start
            PRINT "0x";
            Hex(curr)
            
            WHILE buffer[curr] <> 1
                PRINT " -> 0x";
                curr = buffer[curr]
                Hex(curr)
            WEND
            
            PRINT " [end]"
        ENDIF
    NEXT i
ENDFUNC

FUNC PrintStatistics()
    PRINT
    PRINT "FAT Statistics:"
    PRINT "---------------"
    PRINT "Reserved sectors:     "; reserved; " (FAT & Directory)"
    PRINT "File end sectors:     "; chains
    PRINT "Chain link sectors:   "; used
    PRINT "Free sectors:         "; free
    PRINT "------------------------"
    VAR total = reserved + chains + used + free
    PRINT "Total sectors:        "; total
    PRINT
    VAR usedtotal = reserved + chains + used
    VAR percent = (usedtotal * 100) / 256
    PRINT "Space used:  "; usedtotal; " sectors ("; percent; "%)"
    PRINT "Space free:  "; free; " sectors ("; (free * 256); " bytes)"
ENDFUNC

BEGIN
    PRINT "EEPROM FAT Map"
    PRINT "=============="
    PRINT
    
    ReadSector(0)  ! Read FAT sector
    
    PrintFATMap()
    PrintChainDetails()
    PrintStatistics()
END


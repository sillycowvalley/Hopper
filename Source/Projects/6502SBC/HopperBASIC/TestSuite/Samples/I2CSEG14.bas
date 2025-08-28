NEW 

CONST HT16K33           = 0x70         ! Default I2C address (0x70)
CONST HT16K33OSCON      = 0x21         !  Turn on oscillator
CONST HT16K33DISPLAYON  = 0x81         ! Display on, no blink
CONST HT16K33BRIGHTNESS = (0xE0 + 15)  ! Max brightness (0xE0 + 15)
CONST HT16K33DISPLAYOFF = 128          ! Display off

BYTE CharSet[128]
BYTE CharSetHi[128]

FUNC BuildExtras()
    CharSet[ASC(' ')] = 0x00
    CharSet[ASC('-')] = 0xC0
    CharSet[ASC('=')] = 0xC8
    CharSet[ASC('!')] = 0x06
    CharSet[ASC('?')] = 0x83
    CharSetHi[ASC('?')] = 0x10
    CharSet[ASC('@')] = 0xBB
    CharSetHi[ASC('@')] = 0x02
    CharSet[ASC('#')] = 0xCE
    CharSetHi[ASC('#')] = 0x12
    CharSet[ASC('*')] = 0xC0
    CharSetHi[ASC('*')] = 0x3F
ENDFUNC

FUNC BuildLetters()
    CharSet[ASC('A')] = 0xF7
    CharSet[ASC('B')] = 0x8F
    CharSet[ASC('C')] = 0x39
    CharSet[ASC('D')] = 0x0F
    CharSet[ASC('E')] = 0xF9
    CharSet[ASC('F')] = 0x71
    CharSet[ASC('G')] = 0xBD
    CharSet[ASC('H')] = 0xF6
    CharSet[ASC('I')] = 0x09
    CharSet[ASC('J')] = 0x1E
    CharSet[ASC('K')] = 0x70
    CharSet[ASC('L')] = 0x38
    CharSet[ASC('M')] = 0x36
    CharSet[ASC('N')] = 0x36
    CharSet[ASC('O')] = 0x3F
    CharSet[ASC('P')] = 0xF3
    CharSet[ASC('Q')] = 0x3F
    CharSet[ASC('R')] = 0xF3
    CharSet[ASC('S')] = 0xED
    CharSet[ASC('T')] = 0x01
    CharSet[ASC('U')] = 0x3E
    CharSet[ASC('V')] = 0x30
    CharSet[ASC('W')] = 0x36
    CharSet[ASC('X')] = 0x00
    CharSet[ASC('Y')] = 0x00
    CharSet[ASC('Z')] = 0x09
ENDFUNC
FUNC BuildLettersHi()
    CharSetHi[ASC('B')] = 0x12
    CharSetHi[ASC('D')] = 0x12
    CharSetHi[ASC('I')] = 0x12
    CharSetHi[ASC('K')] = 0x24
    CharSetHi[ASC('M')] = 0x05
    CharSetHi[ASC('N')] = 0x21
    CharSetHi[ASC('Q')] = 0x20
    CharSetHi[ASC('R')] = 0x20
    CharSetHi[ASC('S')] = 0x20
    CharSetHi[ASC('T')] = 0x12
    CharSetHi[ASC('V')] = 0x0C
    CharSetHi[ASC('W')] = 0x28
    CharSetHi[ASC('X')] = 0x2D
    CharSetHi[ASC('Y')] = 0x15
    CharSetHi[ASC('Z')] = 0x0C
ENDFUNC

FUNC BuildDigits()
    CharSet[ASC('0')] = 0x3F
    CharSet[ASC('1')] = 0x06
    CharSet[ASC('2')] = 0xDB
    CharSet[ASC('3')] = 0x8F
    CharSet[ASC('4')] = 0xE6
    CharSet[ASC('5')] = 0xED
    CharSet[ASC('6')] = 0xFD
    CharSet[ASC('7')] = 0x07
    CharSet[ASC('8')] = 0xFF
    CharSet[ASC('9')] = 0xEF
ENDFUNC


FUNC BuildSet()
    BuildDigits()
    BuildLetters()
    BuildLettersHi()
    BuildExtras()
    BuildExtras()
ENDFUNC

FUNC TEST()
    BuildSet()
    FOR I = 0 TO 127
        PRINT CharSet[I],
    NEXT I
    PRINT
    FOR I = 0 TO 127
        PRINT CharSetHi[I],
    NEXT I
    PRINT
ENDFUNC

FUNC WriteCharacter(C)
    VAR ascii = ASC(C)
    VAR lsb = CharSet[ascii]
    VAR msb = CharSetHi[ascii]
    I2CPUT(lsb)
    I2CPUT(msb)
ENDFUNC

FUNC SHOW(message)
    VAR C
    IF NOT I2CFIND(HT16K33) THEN
        PRINT "HT16K33 not found at address ", HT16K33
        RETURN
    ENDIF
    PRINT "HT16K33 found!"
    
    ! Turn on oscillator
    I2CBEGIN(HT16K33)
    I2CPUT(HT16K33OSCON)
    IF NOT I2CEND() THEN
        ! PRINT "Failed to start oscillator"
        ! RETURN
    ENDIF
    
    I2CBEGIN(HT16K33)
    I2CPUT(HT16K33DISPLAYON)
    IF NOT I2CEND() THEN  
        ! PRINT "Failed to turn on display"
        ! RETURN
    ENDIF
    
    I2CBEGIN(HT16K33)
    I2CPUT(HT16K33BRIGHTNESS)
    IF NOT I2CEND() THEN
        ! PRINT "Failed to set brightness"
        ! RETURN
    ENDIF
    
    I2CBEGIN(HT16K33)
    I2CPUT(0)           ! Start at display RAM address 0
    
    FOR I = 0 TO LEN(message) - 1
        C = message[I]
        PRINT C,
        WriteCharacter(C)
    NEXT I
    PRINT
    
    IF I2CEND() THEN
        PRINT "Display updated: HELO"
    ELSE
        ! PRINT "Failed to write to display"
    ENDIF
ENDFUNC

FUNC HIDE()
    I2CBEGIN(HT16K33)
    I2CPUT(HT16K33DISPLAYOFF)
    ! I2CEND()
ENDFUNC

BEGIN
    BuildSet()
    Show("HELO")
    DELAY(3000)
    Hide()
END



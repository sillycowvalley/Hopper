CLS
! Built-in Function Test Suite for Hopper BASIC v3
! Tests all system functions, parameter validation, and hex literals
! Uses safe memory area 0x0B40+ for PEEK/POKE tests

NEW
MEM

! ===== TEST 1: ABS() Function (Unimplemented) =====
NEW
FUNC TestAbs()
    VAR pos = 42
    VAR neg = -42
    VAR zero = 0
    PRINT "ABS() Tests:"
    PRINT "ABS(42)="; ABS(pos); " ! expect 42 or error"
    PRINT "ABS(-42)="; ABS(neg); " ! expect 42 or error"
    PRINT "ABS(0)="; ABS(zero); " ! expect 0 or error"
ENDFUNC
BEGIN
    TestAbs()
END
RUN

! ===== TEST 3: ASC() Function =====
NEW
FUNC TestAsc()
    VAR c1 = 'A'
    VAR c2 = 'Z'
    VAR c3 = '0'
    VAR c4 = ' '
    PRINT "ASC() Tests:"
    PRINT "ASC('A')="; ASC(c1); " ! expect 65"
    PRINT "ASC('Z')="; ASC(c2); " ! expect 90"
    PRINT "ASC('0')="; ASC(c3); " ! expect 48"
    PRINT "ASC(' ')="; ASC(c4); " ! expect 32"
ENDFUNC
BEGIN
    TestAsc()
END
RUN

! ===== TEST 4: CHR() Function =====
NEW
FUNC TestChr()
    VAR val1 = 65
    VAR val2 = 90
    VAR val3 = 48
    VAR val4 = 32
    PRINT "CHR() Tests:"
    PRINT "CHR(65)="; CHR(val1); " ! expect A"
    PRINT "CHR(90)="; CHR(val2); " ! expect Z"
    PRINT "CHR(48)="; CHR(val3); " ! expect 0"
    PRINT "CHR(32)="; CHR(val4); " ! expect space"
ENDFUNC
BEGIN
    TestChr()
END
RUN

! ===== TEST 5: CHR() Range Validation =====
NEW
FUNC TestChrRange()
    VAR valid = 255
    VAR invalid = 256
    VAR negative = -1
    PRINT "CHR() Range Tests:"
    PRINT "CHR(255)="; CHR(valid); " ! expect char or success"
    PRINT "CHR(256)="; CHR(invalid); " ! expect OVERFLOW error"
    PRINT "CHR(-1)="; CHR(negative); " ! expect OVERFLOW error"
ENDFUNC
BEGIN
    TestChrRange()
END
RUN

! ===== TEST 6: LEN() with Strings =====
NEW
FUNC TestLenString()
    VAR s1 = "HELLO"
    VAR s2 = ""
    VAR s3 = "A"
    VAR s4 = "This is a longer test string"
    PRINT "LEN() String Tests:"
    PRINT "LEN('HELLO')="; LEN(s1); " ! expect 5"
    PRINT "LEN('')="; LEN(s2); " ! expect 0"
    PRINT "LEN('A')="; LEN(s3); " ! expect 1"
    PRINT "LEN(long)="; LEN(s4); " ! expect 28"
ENDFUNC
BEGIN
    TestLenString()
END
RUN

! ===== TEST 7: LEN() with Arrays =====
NEW
BIT flags[10]
CHAR letters[5]
INT numbers[20]
FUNC TestLenArray()
    PRINT "LEN() Array Tests:"
    PRINT "LEN(BIT[10])="; LEN(flags); " ! expect 10"
    PRINT "LEN(CHAR[5])="; LEN(letters); " ! expect 5"
    PRINT "LEN(INT[20])="; LEN(numbers); " ! expect 20"
ENDFUNC
BEGIN
    TestLenArray()
END
RUN

! ===== TEST 8: PEEK() Function with Hex =====
NEW
FUNC TestPeek()
    VAR addr1 = 0x0B40
    VAR addr2 = 0x0B50
    VAR addr3 = 0x0B7F
    PRINT "PEEK() Tests (hex addresses):"
    PRINT "PEEK(0x0B40)="; PEEK(addr1); " ! expect 0-255"
    PRINT "PEEK(0x0B50)="; PEEK(addr2); " ! expect 0-255"
    PRINT "PEEK(0x0B7F)="; PEEK(addr3); " ! expect 0-255"
ENDFUNC
BEGIN
    TestPeek()
END
RUN

! ===== TEST 9: POKE() Function with Hex =====
NEW
FUNC TestPoke()
    VAR addr = 0x0B40
    VAR val1 = 0x42
    VAR val2 = 0xFF
    VAR val3 = 0x00
    PRINT "POKE() Tests (hex values):"
    POKE(addr, val1)
    PRINT "POKE(0x0B40, 0x42) PEEK="; PEEK(addr); " ! expect 66"
    POKE(addr, val2)
    PRINT "POKE(0x0B40, 0xFF) PEEK="; PEEK(addr); " ! expect 255"
    POKE(addr, val3)
    PRINT "POKE(0x0B40, 0x00) PEEK="; PEEK(addr); " ! expect 0"
ENDFUNC
BEGIN
    TestPoke()
END
RUN

! ===== TEST 10: POKE() Range Validation =====
NEW
FUNC TestPokeRange()
    VAR addr = 0x0B40
    VAR validVal = 255
    VAR invalidVal = 256
    VAR negativeVal = -1
    PRINT "POKE() Range Tests:"
    POKE(addr, validVal)
    PRINT "POKE(addr, 255) - success"
    POKE(addr, invalidVal)
    PRINT "POKE(addr, 256) ! expect OVERFLOW error"
    POKE(addr, negativeVal)
    PRINT "POKE(addr, -1) ! expect OVERFLOW error"
ENDFUNC
BEGIN
    TestPokeRange()
END
RUN

! ===== TEST 11: MILLIS() and SECONDS() =====
NEW
FUNC TestTimers()
    VAR start = MILLIS()
    VAR startSec = SECONDS()
    PRINT "Timer Tests:"
    PRINT "MILLIS()="; start; " ! expect positive number"
    PRINT "SECONDS()="; startSec; " ! expect positive number"
    DELAY(100)
    VAR end = MILLIS()
    PRINT "After DELAY(100): "; end; " ! expect >"; start
ENDFUNC
BEGIN
    TestTimers()
END
RUN

! ===== TEST 12: DELAY() Function =====
NEW
FUNC TestDelay()
    VAR before = MILLIS()
    PRINT "DELAY() Tests:"
    PRINT "Before DELAY(500): "; before
    DELAY(500)
    VAR after = MILLIS()
    PRINT "After DELAY(500): "; after
    VAR elapsed = after - before
    PRINT "Elapsed: "; elapsed; " ! expect ~500ms"
ENDFUNC
BEGIN
    TestDelay()
END
RUN

! ===== TEST 13: Hardware I/O Range Validation =====
NEW
FUNC TestHardwareRange()
    PRINT "Hardware I/O Range Tests:"
    PRINT "PINMODE(0, 1) - valid pin/mode"
    PINMODE(0, 1)
    PRINT "READ(15) - max valid pin"
    PRINT READ(15)
    PRINT "WRITE(0, TRUE) - valid"
    WRITE(0, TRUE)
    PRINT "Testing invalid ranges..."
    PINMODE(16, 1)
    PRINT "PINMODE(16, 1) ! expect error (pin > 15)"
ENDFUNC
BEGIN
    TestHardwareRange()
END
RUN

! ===== TEST 14: Hardware I/O Mode Validation =====
NEW
FUNC TestHardwareModes()
    PRINT "Hardware Mode Tests:"
    PINMODE(5, 0)
    PRINT "PINMODE(5, 0) - input mode"
    PINMODE(5, 1)
    PRINT "PINMODE(5, 1) - output mode"
    PINMODE(5, 2)
    PRINT "PINMODE(5, 2) ! expect error (invalid mode)"
ENDFUNC
BEGIN
    TestHardwareModes()
END
RUN

! ===== TEST 15: Mixed Hex and Decimal =====
NEW
FUNC TestHexFormat()
    VAR dec = 255
    VAR hex = 0xFF
    VAR addr = 0x0B60
    PRINT "Hex Format Tests:"
    PRINT "dec="; dec; " hex="; hex; " ! both expect 255"
    PRINT "Equal? "; dec = hex; " ! expect TRUE"
    POKE(addr, 0xAA)
    PRINT "PEEK(0x0B60)="; PEEK(addr); " ! expect 170"
ENDFUNC
BEGIN
    TestHexFormat()
END
RUN

! ===== TEST 16: Function Parameter Types =====
NEW
FUNC TestParamTypes()
    VAR longVal = 100
    VAR charVal = 'A'
    VAR bitVal = TRUE
    PRINT "Parameter Type Tests:"
    PRINT "LEN with VAR(STRING):"
    VAR text = "TEST"
    PRINT "LEN="; LEN(text); " ! expect 4"
    PRINT "ASC with VAR(CHAR):"
    PRINT "ASC="; ASC(charVal); " ! expect 65"
    PRINT "CHR with VAR(LONG):"
    PRINT "CHR="; CHR(longVal); " ! expect d"
ENDFUNC
BEGIN
    TestParamTypes()
END
RUN

NEW
MEM
PRINT "Built-in function tests complete"

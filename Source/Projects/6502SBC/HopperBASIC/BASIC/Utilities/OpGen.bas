NEW

CONST NAMELEN = 12

! Arrays for each opcode range
CHAR Op00Names[30 * NAMELEN]  ! Opcodes 0x00-0x1D
CHAR Op40Names[16 * NAMELEN]  ! Opcodes 0x40-0x4F
CHAR Op80Names[21 * NAMELEN]  ! Opcodes 0x80-0x94
CHAR OpC0Names[3 * NAMELEN]   ! Opcodes 0xC0-0xC2

FUNC PadName(arr, idx, name)
    VAR pos = idx * NAMELEN
    VAR i = 0
    VAR c
    VAR nlen = LEN(name)
    
    ! Copy the name
    WHILE i < nlen
        IF i < NAMELEN THEN
            arr[pos + i] = name[i]
        ENDIF
        i = i + 1
    WEND
    
    ! Pad with spaces
    WHILE i < NAMELEN
        arr[pos + i] = ' '
        i = i + 1
    WEND
ENDFUNC

FUNC Build00()
    ! Initialize all to "?" first
    FOR i = 0 TO 29
        PadName(Op00Names, i, "?")
    NEXT i
    
    ! Fill in known opcodes 0x00-0x1D
    PadName(Op00Names, 0x00, "INVALID")
    PadName(Op00Names, 0x01, "ADD")
    PadName(Op00Names, 0x02, "SUB")
    PadName(Op00Names, 0x03, "MUL")
    PadName(Op00Names, 0x04, "DIV")
    PadName(Op00Names, 0x05, "MOD")
    PadName(Op00Names, 0x06, "NEG")
    PadName(Op00Names, 0x07, "BITWISE_AND")
    PadName(Op00Names, 0x08, "BITWISE_OR")
    PadName(Op00Names, 0x09, "BITWISE_NOT")
    PadName(Op00Names, 0x0A, "LOGICAL_AND")
    PadName(Op00Names, 0x0B, "LOGICAL_OR")
    PadName(Op00Names, 0x0C, "LOGICAL_NOT")
    PadName(Op00Names, 0x0D, "EQ")
    PadName(Op00Names, 0x0E, "NE")
    PadName(Op00Names, 0x0F, "LT")
    PadName(Op00Names, 0x10, "GT")
    PadName(Op00Names, 0x11, "LE")
    PadName(Op00Names, 0x12, "GE")
    PadName(Op00Names, 0x13, "DUP")
    PadName(Op00Names, 0x14, "NOP")
    PadName(Op00Names, 0x15, "PUSH0")
    PadName(Op00Names, 0x16, "PUSH1")
    PadName(Op00Names, 0x17, "PUSHVOID")
    PadName(Op00Names, 0x18, "PUSHLONG0")
    PadName(Op00Names, 0x19, "HALT")
    PadName(Op00Names, 0x1A, "ENTER")
    PadName(Op00Names, 0x1B, "PUSHEMPTYVAR")
    PadName(Op00Names, 0x1C, "GETITEM")
    PadName(Op00Names, 0x1D, "SETITEM")
    
    EXPORT(Op00Names, "OPCODES00")
    PRINT "Created OPCODES00 (", 30 * NAMELEN, " bytes)"
ENDFUNC

FUNC Build40()
    ! Initialize all to "?" first
    FOR i = 0 TO 15
        PadName(Op40Names, i, "?")
    NEXT i
    
    ! Fill in known opcodes 0x40-0x4F
    PadName(Op40Names, 0x00, "PUSHBIT")     ! 0x40
    PadName(Op40Names, 0x01, "PUSHBYTE")    ! 0x41
    PadName(Op40Names, 0x02, "PUSHCHAR")    ! 0x42
    PadName(Op40Names, 0x03, "PUSHLOCAL")   ! 0x43
    PadName(Op40Names, 0x04, "POPLOCAL")    ! 0x44
    ! 0x45-0x47 are unknown
    PadName(Op40Names, 0x08, "SYSCALL")     ! 0x48
    PadName(Op40Names, 0x09, "RETURN")      ! 0x49
    PadName(Op40Names, 0x0A, "RETURNVAL")   ! 0x4A
    PadName(Op40Names, 0x0B, "PUSHGLOBAL")  ! 0x4B
    PadName(Op40Names, 0x0C, "POPGLOBAL")   ! 0x4C
    PadName(Op40Names, 0x0D, "INCGLOBAL")   ! 0x4D
    PadName(Op40Names, 0x0E, "INCLOCAL")    ! 0x4E
    PadName(Op40Names, 0x0F, "DECSP")       ! 0x4F
    
    EXPORT(Op40Names, "OPCODES40")
    PRINT "Created OPCODES40 (", 16 * NAMELEN, " bytes)"
ENDFUNC

FUNC Build80()
    ! Initialize all to "?" first
    FOR i = 0 TO 20
        PadName(Op80Names, i, "?")
    NEXT i
    
    ! Fill in known opcodes 0x80-0x94
    PadName(Op80Names, 0x00, "PUSHINT")     ! 0x80
    PadName(Op80Names, 0x01, "PUSHWORD")    ! 0x81
    PadName(Op80Names, 0x02, "PUSHCSTRING") ! 0x82
    PadName(Op80Names, 0x03, "CALL")        ! 0x83
    PadName(Op80Names, 0x04, "CALLF")       ! 0x84
    ! 0x85-0x86 are unknown
    PadName(Op80Names, 0x07, "JUMPW")       ! 0x87
    PadName(Op80Names, 0x08, "JUMPZW")      ! 0x88
    ! 0x89 is unknown
    PadName(Op80Names, 0x0A, "ADDLOCALS")   ! 0x8A
    PadName(Op80Names, 0x0B, "ADDGLOBALS")  ! 0x8B
    PadName(Op80Names, 0x0C, "GETITEMGG")   ! 0x8C
    PadName(Op80Names, 0x0D, "GETITEMGL")   ! 0x8D
    PadName(Op80Names, 0x0E, "GETITEMLG")   ! 0x8E
    PadName(Op80Names, 0x0F, "GETITEMLL")   ! 0x8F
    PadName(Op80Names, 0x10, "SETITEMGG")   ! 0x90
    PadName(Op80Names, 0x11, "SETITEMGL")   ! 0x91
    PadName(Op80Names, 0x12, "SETITEMLG")   ! 0x92
    PadName(Op80Names, 0x13, "SETITEMLL")   ! 0x93
    PadName(Op80Names, 0x14, "PUSHLONG")    ! 0x94
    
    EXPORT(Op80Names, "OPCODES80")
    PRINT "Created OPCODES80 (", 21 * NAMELEN, " bytes)"
ENDFUNC

FUNC BuildC0()
    ! Initialize all to "?" first
    FOR i = 0 TO 2
        PadName(OpC0Names, i, "?")
    NEXT i
    
    ! Fill in known opcodes 0xC0-0xC2
    PadName(OpC0Names, 0x00, "FORCHK")      ! 0xC0
    PadName(OpC0Names, 0x01, "FORIT")       ! 0xC1
    PadName(OpC0Names, 0x02, "FORITF")      ! 0xC2
    
    EXPORT(OpC0Names, "OPCODESC0")
    PRINT "Created OPCODESC0 (", 3 * NAMELEN, " bytes)"
ENDFUNC

BEGIN
    PRINT "HopperBASIC Opcode Table Generator"
    PRINT "==================================="
    PRINT
    
    Build00()
    Build40()
    Build80()
    BuildC0()
    
    PRINT
    PRINT "Total size: ", (30 + 16 + 21 + 3) * NAMELEN, " bytes"
    PRINT
    PRINT "Opcode tables successfully created!"
    PRINT "You can now use DASM to disassemble functions."
END

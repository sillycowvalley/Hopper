BYTE bytes[8]
VAR ig
VAR bg
FUNC TestArraysOpt(bytesl)
    VAR il
    VAR bl
    bytes[il] = 0
    bytes[ig] = 0
    bytes[il] = bl
    bytes[ig] = bl
    bytes[il] = bg
    bytes[ig] = bg
    bl = bytes[il]
    bl = bytes[ig]
    bg = bytes[il]
    bg = bytes[ig]
    bytesl[il] = 0
    bytesl[ig] = 0
    bytesl[il] = bl
    bytesl[ig] = bl
    bytesl[il] = bg
    bytesl[ig] = bg
    bl = bytesl[il]
    bl = bytesl[ig]
    bg = bytesl[il]
    bg = bytesl[ig]
ENDFUNC
BEGIN
TestArraysOpt(bytes)
END
RUN

DASM

FUNC TESTARRAYSOPT(BYTESL)

   [1552][1582]
    1772: 1A           ENTER
    1773: 1C           PUSHEMPTYVAR
    1774: 1C           PUSHEMPTYVAR
    
    1775: 81 00 00     PUSHWORD     0000 (0)
    1778: 18           PUSHLONG0
    1779: 91 00 00     SETITEMGL    0000  00 00
    
    177C: 81 00 00     PUSHWORD     0000 (0)
    177F: 18           PUSHLONG0
    1780: 90 00 01     SETITEMGG    0100  00 01
    
    1783: 43 01        PUSHLOCAL    01 [BP+1]
    1785: 91 00 00     SETITEMGL    0000  00 00
    
    1788: 43 01        PUSHLOCAL    01 [BP+1]
    178A: 90 00 01     SETITEMGG    0100  00 01
    
    178D: 4B 02        PUSHGLOBAL   02 ()
    178F: 91 00 00     SETITEMGL    0000  00 00
    
    1792: 4B 02        PUSHGLOBAL   02 ()
    1794: 90 00 01     SETITEMGG    0100  00 01
    
    1797: 8D 00 00     GETITEMGL    0000  00 00
    179A: 44 01        POPLOCAL     01 [BP+1]
    179C: 8C 00 01     GETITEMGG    0100  00 01
    179F: 44 01        POPLOCAL     01 [BP+1]
    17A1: 8D 00 00     GETITEMGL    0000  00 00
    17A4: 4C 02        POPGLOBAL    02 ()
    17A6: 8C 00 01     GETITEMGG    0100  00 01
    17A9: 4C 02        POPGLOBAL    02 ()
    17AB: 81 00 00     PUSHWORD     0000 (0)
    17AE: 18           PUSHLONG0
    17AF: 93 FF 00     SETITEMLL    00FF  FF 00
    17B2: 81 00 00     PUSHWORD     0000 (0)
    17B5: 18           PUSHLONG0
    17B6: 92 FF 01     SETITEMLG    01FF  FF 01
    17B9: 43 01        PUSHLOCAL    01 [BP+1]
    17BB: 93 FF 00     SETITEMLL    00FF  FF 00
    17BE: 43 01        PUSHLOCAL    01 [BP+1]
    17C0: 92 FF 01     SETITEMLG    01FF  FF 01
    17C3: 4B 02        PUSHGLOBAL   02 ()
    17C5: 93 FF 00     SETITEMLL    00FF  FF 00
    17C8: 4B 02        PUSHGLOBAL   02 ()
    17CA: 92 FF 01     SETITEMLG    01FF  FF 01
    17CD: 8F FF 00     GETITEMLL    00FF  FF 00
    17D0: 44 01        POPLOCAL     01 [BP+1]
    17D2: 8E FF 01     GETITEMLG    01FF  FF 01
    17D5: 44 01        POPLOCAL     01 [BP+1]
    17D7: 8F FF 00     GETITEMLL    00FF  FF 00
    17DA: 4C 02        POPGLOBAL    02 ()
    17DC: 8E FF 01     GETITEMLG    01FF  FF 01
    17DF: 4C 02        POPGLOBAL    02 ()
    17E1: 49 01        RETURN       01
    17E3: 19           HALT
ENDFUNC
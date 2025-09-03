NEW
CLS

! ============================================
! Hopper BASIC Text Editor v1.0
! Modular framework with VT100 terminal
! ============================================

! ============================================
! GLOBAL CONSTANTS
! ============================================

! Editor screen dimensions (internal coordinates)
CONST edcW = 60
CONST edcH = 22
CONST edcTotalH = 24

! Terminal positioning (VT100 coordinates - 1-based)
CONST tmcFrameX = 10
CONST tmcFrameY = 10
CONST tmcTextX = 11
CONST tmcTextY = 11

! Line buffer configuration
CONST bufcRecSz = 64
CONST bufcMaxLines = 150
CONST bufcContentMax = 60
CONST bufcTotalSz = 9600
CONST bufcTestLines = 50

! Screen buffer configuration  
CONST scrTotalSz = 1440

! Key codes (avoid ASCII conflicts)
CONST kbcUp = 128
CONST kbcDown = 129
CONST kbcLeft = 130
CONST kbcRight = 131
CONST kbcEnter = 13
CONST kbcEsc = 27

! Terminal control
CONST tmcEsc = 0x1B

! Serial buffer constants
CONST kbcWrPtr = 0x0A
CONST kbcRdPtr = 0x0B  
CONST kbcBuf = 0x0200

! ============================================
! GLOBAL STORAGE ARRAYS
! ============================================

BYTE lineBuf[bufcTotalSz]
BYTE scrBuf[scrTotalSz]

! ============================================
! GLOBAL STATE VARIABLES  
! ============================================

VAR bufFree = 0
VAR bufFirst = 0
VAR bufCnt = 0
VAR viewPtr = 0
VAR viewTop = 0
VAR curLn = 0
VAR curCol = 0
VAR kbEsc = 0
VAR scrDirty = 0

! ============================================
! TERMINAL MODULE (tm)
! ============================================

! Initialize terminal for editor use
FUNC tmInit()
    tmClr()
    tmHide()
    tmFrame()
    RETURN TRUE
ENDFUNC

! Clear entire terminal screen
FUNC tmClr()
    PRINT CHR(tmcEsc); "[2J";
    PRINT CHR(tmcEsc); "[H";
ENDFUNC

! Position cursor (1-based terminal coordinates)
FUNC tmGoto(x, y)
    PRINT CHR(tmcEsc); "["; y; ";"; x; "H";
ENDFUNC

! Hide cursor
FUNC tmHide()
    PRINT CHR(tmcEsc); "[?25l";
ENDFUNC

! Show cursor  
FUNC tmShow()
    PRINT CHR(tmcEsc); "[?25h";
ENDFUNC

! Draw frame around text area
FUNC tmFrame()
    VAR x, y
    
    tmGoto(tmcFrameX, tmcFrameY)
    PRINT "+";
    FOR x = 1 TO edcW
        PRINT "-";
    NEXT x
    PRINT "+";
    
    FOR y = 1 TO edcTotalH
        tmGoto(tmcFrameX, tmcFrameY + y)
        PRINT "|";
        tmGoto(tmcFrameX + edcW + 1, tmcFrameY + y)
        PRINT "|";
    NEXT y
    
    tmGoto(tmcFrameX, tmcFrameY + edcTotalH + 1)
    PRINT "+";
    FOR x = 1 TO edcW
        PRINT "-";
    NEXT x
    PRINT "+";
ENDFUNC

! ============================================
! KEYBOARD MODULE (kb)
! ============================================

! Initialize keyboard
FUNC kbInit()
    kbEsc = 0
ENDFUNC

! Check if key available
FUNC kbRdy()
    RETURN PEEK(kbcRdPtr) <> PEEK(kbcWrPtr)
ENDFUNC

! Get raw character from buffer
FUNC kbRaw()
    VAR ptr, key
    WHILE NOT kbRdy()
        DELAY(5)
    WEND
    ptr = PEEK(kbcRdPtr)
    key = PEEK(kbcBuf + ptr)
    POKE(kbcRdPtr, (ptr + 1) & 0xFF)
    RETURN key
ENDFUNC

! Get processed key (handles VT100 escape sequences)
FUNC kbGet()
    VAR k
    
    k = kbRaw()
    
    IF kbEsc = 0 THEN
        IF k = tmcEsc THEN
            kbEsc = 1
            RETURN kbGet()
        ELSE
            RETURN k
        ENDIF
    ELSE
        IF kbEsc = 1 THEN
            IF k = 0x5B THEN
                kbEsc = 2
                RETURN kbGet()
            ELSE
                kbEsc = 0
                RETURN tmcEsc
            ENDIF
        ELSE
            IF kbEsc = 2 THEN
                kbEsc = 0
                IF k = 0x41 THEN RETURN kbcUp ENDIF
                IF k = 0x42 THEN RETURN kbcDown ENDIF
                IF k = 0x43 THEN RETURN kbcRight ENDIF
                IF k = 0x44 THEN RETURN kbcLeft ENDIF
                RETURN k
            ENDIF
        ENDIF
    ENDIF
    
    RETURN k
ENDFUNC

! ============================================
! BUFFER MODULE (buf)
! ============================================

! Initialize buffer system with test data
FUNC bufInit()
    VAR ptr
    VAR nxt
    
    bufFree = 0
    FOR i = 0 TO bufcMaxLines - 2
        ptr = i * bufcRecSz
        nxt = (i + 1) * bufcRecSz
        POKE(ptr, nxt & 0xFF)
        POKE(ptr + 1, nxt / 256)
    NEXT i
    
    ptr = (bufcMaxLines - 1) * bufcRecSz
    POKE(ptr, 0)
    POKE(ptr + 1, 0)
    
    bufFirst = 0
    bufCnt = 0
    
    bufTest()
ENDFUNC

! Allocate a line record from free list
FUNC bufAlloc()
    VAR ptr, nxt
    
    IF bufFree = 0 THEN
        RETURN 0
    ENDIF
    
    ptr = bufFree
    nxt = PEEK(ptr) + PEEK(ptr + 1) * 256
    bufFree = nxt
    
    POKE(ptr, 0)
    POKE(ptr + 1, 0)
    POKE(ptr + 2 + bufcContentMax, 0)
    
    RETURN ptr
ENDFUNC

! Get pointer to line record by line number
FUNC bufGetPtr(ln)
    VAR ptr, cnt
    
    ptr = bufFirst
    cnt = 0
    
    WHILE ptr <> 0 AND cnt < ln
        ptr = PEEK(ptr) + PEEK(ptr + 1) * 256
        cnt = cnt + 1
    WEND
    
    RETURN ptr
ENDFUNC

! Get pointer to following line record in chain
FUNC bufNxt(ptr)
    IF ptr = 0 THEN
        RETURN 0
    ENDIF
    RETURN PEEK(ptr) + PEEK(ptr + 1) * 256
ENDFUNC

! Add line to end of document
FUNC bufAdd(txt)
    VAR n, ptr, last, i, sz
    
    n = bufAlloc()
    IF n = 0 THEN
        RETURN FALSE
    ENDIF
    
    sz = LEN(txt)
    IF sz > bufcContentMax THEN
        sz = bufcContentMax
    ENDIF
    
    FOR i = 0 TO sz - 1
        POKE(n + 2 + i, ASC(txt[i]))
    NEXT i
    POKE(n + 2 + sz, 0)
    
    IF bufFirst = 0 THEN
        bufFirst = n
    ELSE
        ptr = bufFirst
        WHILE TRUE
            last = PEEK(ptr) + PEEK(ptr + 1) * 256
            IF last = 0 THEN
                POKE(ptr, n & 0xFF)
                POKE(ptr + 1, n / 256)
                bufCnt = bufCnt + 1
                RETURN TRUE
            ENDIF
            ptr = last
        WEND
    ENDIF
    
    bufCnt = bufCnt + 1
    RETURN TRUE
ENDFUNC

! Create test data - only as many lines as will fit in buffer
FUNC bufTest()
    VAR i, maxTest
    
    maxTest = bufcTestLines
    IF maxTest > bufcMaxLines THEN
        maxTest = bufcMaxLines
    ENDIF
    
    FOR i = 1 TO maxTest
        IF i MOD 10 = 0 THEN
            bufAdd("")
        ELSE
            bufAdd("Test line content goes here for testing purposes")
        ENDIF
    NEXT i
    
    viewPtr = bufFirst
    viewTop = 0
ENDFUNC

! ============================================
! CURSOR MODULE (cur)  
! ============================================

! Initialize cursor
FUNC curInit()
    curLn = 0
    curCol = 0
ENDFUNC

! Move cursor up
FUNC curUp()
    IF curLn > 0 THEN
        curLn = curLn - 1
        RETURN TRUE
    ENDIF
    RETURN FALSE
ENDFUNC

! Move cursor down  
FUNC curDown()
    IF curLn < bufCnt - 1 THEN
        curLn = curLn + 1
        RETURN TRUE
    ENDIF
    RETURN FALSE
ENDFUNC

! ============================================
! SCREEN MODULE (scr)
! ============================================

! Initialize screen buffer
FUNC scrInit()
    VAR i
    
    FOR i = 0 TO scrTotalSz - 1
        scrBuf[i] = 32 | 0x80
    NEXT i
    
    scrDirty = TRUE
ENDFUNC

! Force full redraw
FUNC scrRedo()
    VAR i
    
    FOR i = 0 TO scrTotalSz - 1
        scrBuf[i] = scrBuf[i] | 0x80
    NEXT i
    
    scrUpd()
ENDFUNC

! Update screen (render dirty characters only)
FUNC scrUpd()
    VAR row, col, pos, ptr, ln, ch, nw, old
    
    ptr = viewPtr
    ln = 0
    
    WHILE ln < edcH AND ptr <> 0
        FOR col = 0 TO edcW - 1
            pos = ln * edcW + col
            
            IF col < bufcContentMax THEN
                ch = PEEK(ptr + 2 + col)
                IF ch = 0 THEN
                    nw = 32
                ELSE
                    nw = ch
                ENDIF
            ELSE
                nw = 32
            ENDIF
            
            old = scrBuf[pos]
            IF nw <> (old & 0x7F) THEN
                scrBuf[pos] = nw | 0x80
            ENDIF
        NEXT col
        
        ptr = bufNxt(ptr)
        ln = ln + 1
    WEND
    
    WHILE ln < edcH
        FOR col = 0 TO edcW - 1
            pos = ln * edcW + col
            old = scrBuf[pos]
            IF 32 <> (old & 0x7F) THEN
                scrBuf[pos] = 32 | 0x80
            ENDIF
        NEXT col
        ln = ln + 1
    WEND
    
    scrRender()
ENDFUNC

! Render only dirty screen positions
FUNC scrRender()
    VAR row, col, pos, ch, x, y
    
    FOR row = 0 TO edcH - 1
        FOR col = 0 TO edcW - 1
            pos = row * edcW + col
            
            IF (scrBuf[pos] & 0x80) <> 0 THEN
                x = tmcTextX + col
                y = tmcTextY + row
                tmGoto(x, y)
                
                ch = scrBuf[pos] & 0x7F
                PRINT CHR(ch);
                
                scrBuf[pos] = ch
            ENDIF
        NEXT col
    NEXT row
ENDFUNC

! ============================================
! EDITOR MODULE (ed)
! ============================================

! Initialize entire editor system
FUNC edInit()
    bufInit()
    tmInit() 
    kbInit()
    curInit()
    scrInit()
    scrRedo()
ENDFUNC

! Main editor loop
FUNC edRun()
    VAR key, moved
    
    WHILE TRUE
        key = kbGet()
        
        IF key = kbcEsc THEN
            RETURN
        ENDIF
        
        moved = FALSE
        
        IF key = kbcUp THEN
            IF curUp() THEN
                moved = TRUE
            ENDIF
        ELSE
            IF key = kbcDown THEN
                IF curDown() THEN
                    moved = TRUE
                ENDIF
            ENDIF
        ENDIF
        
        IF moved THEN
            edView()
            scrUpd()
        ENDIF
    WEND
ENDFUNC

! Adjust view to keep cursor visible
FUNC edView()
    VAR newTop
    
    IF curLn < viewTop THEN
        viewTop = curLn
        viewPtr = bufGetPtr(curLn)
        RETURN
    ENDIF
    
    IF curLn >= viewTop + edcH THEN
        newTop = curLn - edcH + 1
        IF newTop <> viewTop THEN
            viewTop = newTop
            viewPtr = bufGetPtr(newTop)
        ENDIF
    ENDIF
ENDFUNC

! Clean shutdown
FUNC edShut()
    tmShow()
    tmClr()
ENDFUNC

! ============================================
! MAIN PROGRAM
! ============================================

BEGIN
    PRINT "Hopper BASIC Text Editor v1.0"
    PRINT "Initializing..."
    
    edInit()
    
    tmGoto(tmcTextX, tmcTextY + edcH + 1)
    PRINT "Use arrow keys to navigate. ESC to exit.";
    
    edRun()
    edShut()
    
    PRINT "Editor terminated."
END

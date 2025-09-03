Given the constraints of HopperBASIC (limited memory, token buffer limits, 6502 target), here's the simplest full text editor design:

## Core Design

**Single-buffer approach** with a character array as the main text storage, using VT100 terminal control for display. Keep it minimal but functional.

## Key Components

### 1. Text Buffer
```basic
CHAR buffer[1000]     ! Main text storage
VAR bufferLen = 0     ! Current content length
VAR cursorPos = 0     ! Cursor position in buffer
VAR screenTop = 0     ! First line shown on screen
```

### 2. Screen Management
```basic
CONST SCREENWIDTH = 80
CONST SCREENHEIGHT = 20   ! Leave room for status line

FUNC GotoXY(x, y)
    PRINT CHR(27); "["; y; ";"; x; "H";
ENDFUNC

FUNC ClearScreen()
    PRINT CHR(27); "[2J";
    PRINT CHR(27); "[H";
ENDFUNC
```

### 3. Core Functions
- **LoadFile(filename)** - Use IMPORT to load character data
- **SaveFile(filename)** - Use EXPORT to save character data  
- **RefreshScreen()** - Redraw visible portion
- **InsertChar(ch)** - Insert at cursor, shift text right
- **DeleteChar()** - Delete at cursor, shift text left
- **HandleArrows()** - Move cursor with bounds checking

### 4. Main Editor Loop
```basic
BEGIN
    VAR key
    VAR filename = "EDIT"
    
    ClearScreen()
    LoadFile(filename)
    
    DO
        RefreshScreen()
        key = GetKey()
        
        ! Handle different key types
        IF key = 27 THEN          ! ESC to quit
            ! Exit
        ELSE IF key = 8 THEN      ! Backspace
            DeletePrevChar()
        ELSE IF key >= 32 THEN    ! Printable character
            InsertChar(key)
        ENDIF
        ! Arrow keys handled in GetKey() state machine
        
    UNTIL key = 27
    
    SaveFile(filename)
END
```

## Simplifications

1. **Fixed filename** - Start with one hardcoded file to avoid filename input complexity
2. **No line wrapping** - Each line ends at screen width or explicit newline
3. **Limited file size** - 1000 characters max (fits comfortably in memory)
4. **Character-based** - No word processing features, just raw text
5. **No undo** - Keeps state management simple
6. **Status line** - Just show cursor position and file info

## Memory Efficiency Tricks

1. **Reuse existing code** - Adapt the VT100 and keyboard handling from Keys.bas/Dot.bas
2. **Split into multiple files** - Keep each function under ~50 lines to avoid token buffer issues
3. **Minimal screen redraws** - Only refresh changed portions when possible

Would you like me to implement this step-by-step, starting with the basic buffer management and keyboard handling?
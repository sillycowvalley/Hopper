# WordStar-Style Text Editor for HopperBASIC
## Architecture Specification v1.0

## Unified Storage Operations

### Line Allocation Example
```basic
! Document uses lines: DOC[0]=slot 5, DOC[1]=slot 2, DOC[2]=slot 8
! Clipboard empty: NC=0  
! Free slots: FREE[0]=0, FREE[1]=1, FREE[2]=3, FREE[3]=4...

! Copy line 1 to clipboard:
s=AL()              ! Get slot (returns 0, decrements NF)  
! Copy text from LD[DOC[1]*LL] to LD[s*LL]
CLIP[0]=s:NC=1      ! Clipboard now has 1 line

! Insert new line at position 1:
s=AL()              ! Get slot (returns 1)
FOR i=NL TO 1 STEP -1:DOC[i+1]=DOC[i]:NEXT i ! Shift refs
DOC[1]=s:NL=NL+1    ! Insert new ref, increment count
```

### Undo Operation Example
```basic
! Before edit - save current line to undo:
FUNC SaveUndo()
  FOR i=0 TO NU-1:FL(UNDO[i]):NEXT i ! Free old undo
  s=AL():UNDO[0]=s:NU=1:UL=L:UC=C    ! Save line ref & cursor
  ! Copy current line text to undo slot
ENDFUNC

! Restore undo:
FUNC RestoreUndo()
  IF NU>0 THEN ! Copy back, free current, restore cursor
    s=DOC[UL]:DOC[UL]=UNDO[0]:FL(s):L=UL:C=UC:NU=0
  ENDIF
ENDFUNC
```

---

## Project Overview

### Goal
Create a text editor for HopperBASIC using classic WordStar control key commands, optimized for 6502 systems with limited memory and processing power.

### Design Constraints
- **Target Platform**: 6502 systems running HopperBASIC v3.3
- **Memory Budget**: ~10K total (8K text + 2K overhead)
- **Screen Size**: 80x24 terminal (22 lines text + 2 status)
- **Storage**: EEPROM via HopperBASIC file system
- **Input**: Direct serial buffer reading (no INPUT statement)

### Design Principles
- **Simplicity First**: Working editor before optimization
- **Incremental Development**: Modular design enabling phased implementation
- **Performance Awareness**: Minimize screen redraws and memory moves
- **Classic Feel**: Authentic WordStar command experience

---

## Data Structures

### Core Storage System (Three-Array Architecture)

#### 1. Physical Line Storage
```basic
CONST MAX_LINES = 100
CONST LINE_LENGTH = 80

! Fixed-size line slots (8000 bytes total)
CHAR lineData[MAX_LINES * LINE_LENGTH]
```

#### 2. Logical Document Order  
```basic
! Document line order (references to lineData slots)
WORD lineList[MAX_LINES]
VAR numLines = 0
```

#### 3. Free Slot Management
```basic
! Available line slots for allocation
WORD freeLines[MAX_LINES]  
VAR numFree = MAX_LINES
```

### Editor State Variables
```basic
! Cursor position
VAR currentLine = 0      ! Document line (0-based)
VAR currentCol = 0       ! Column within line (0-based)

! Display state  
VAR topLine = 0          ! First visible line on screen
VAR leftCol = 0          ! Horizontal scroll offset

! Editor modes
BIT insertMode = TRUE    ! Insert vs overwrite
BIT fileModified = FALSE ! Unsaved changes flag
BIT quitPending = FALSE  ! Exit requested

! File management
CHAR fileName[13]        ! Current file name
VAR fileLength = 0       ! Total characters in file
```

### Screen Layout Constants
```basic
CONST SCREEN_WIDTH = 80
CONST SCREEN_HEIGHT = 24
CONST TEXT_LINES = 22      ! Lines 1-22 for text
CONST STATUS_LINE = 23     ! Line 23 for status
CONST MESSAGE_LINE = 24    ! Line 24 for messages/commands
```

---

## Module Architecture

### 1. LineManager Module

**Purpose**: Manage the three-array line storage system

**Key Functions**:
```basic
FUNC InitLineManager()           ! Initialize arrays and free list
FUNC AllocateLine()             ! Get free slot, returns slot index
FUNC FreeLine(slot)             ! Return slot to free list
FUNC InsertTextLine(pos)        ! Insert new line at document position  
FUNC DeleteTextLine(pos)        ! Remove line from document
FUNC GetLineText(lineNum)       ! Get pointer to line text
FUNC SetLineText(lineNum, text) ! Replace line content
FUNC GetLineLength(lineNum)     ! Get line length (to null terminator)
```

**Memory Layout**:
```
lineData:   [Slot0: "Hello\0......."][Slot1: "World\0......."][Slot2: ...]
lineList:   [0][1][2]...  ! Document uses slots 0,1,2 in order
freeLines:  [3][4][5]...  ! Slots 3+ available for allocation
```

### 2. Cursor Module

**Purpose**: Track cursor position and handle movement commands

**Key Functions**:
```basic
FUNC MoveCursorUp()         ! ^E - Move up one line
FUNC MoveCursorDown()       ! ^X - Move down one line  
FUNC MoveCursorLeft()       ! ^S - Move left one character
FUNC MoveCursorRight()      ! ^D - Move right one character
FUNC MoveWordLeft()         ! ^A - Move left one word
FUNC MoveWordRight()        ! ^F - Move right one word
FUNC MoveStartOfLine()      ! ^QS - Move to line start
FUNC MoveEndOfLine()        ! ^QD - Move to line end
FUNC MoveTopOfFile()        ! ^QR - Move to first line
FUNC MoveEndOfFile()        ! ^QC - Move to last line
FUNC MovePage(direction)    ! ^R/^C - Page up/down
FUNC EnsureCursorVisible()  ! Adjust topLine if needed
```

**Coordinate Systems**:
- **Document coordinates**: (currentLine, currentCol) in logical text
- **Screen coordinates**: (screenRow, screenCol) for display
- **Buffer coordinates**: Absolute offset in lineData for direct access

### 3. Display Module  

**Purpose**: Handle all screen output and VT100 control

**Key Functions**:
```basic
FUNC InitDisplay()          ! Clear screen, hide cursor
FUNC RefreshScreen()        ! Full screen redraw
FUNC RefreshTextArea()      ! Redraw lines 1-22 only
FUNC UpdateStatusLine()     ! Show file, position, mode
FUNC ShowMessage(msg)       ! Display message on line 24
FUNC ClearMessage()         ! Clear message line
FUNC PositionCursor()       ! Move cursor to current position
FUNC ScrollUp()             ! Scroll display up one line
FUNC ScrollDown()           ! Scroll display down one line
```

**VT100 Helper Functions**:
```basic
FUNC SendEsc()              ! Output ESC character
FUNC GotoXY(x, y)          ! Position cursor  
FUNC ClearScreen()          ! Clear entire screen
FUNC ClearLine()           ! Clear current line
```

### 4. Input Module

**Purpose**: Handle keyboard input and command interpretation

**Key Functions**:
```basic
FUNC GetKey()               ! Get single key (building on Keys.bas)
FUNC IsControlKey(key)      ! Test if key < 32
FUNC HandleControlKey(key)  ! Process WordStar commands
FUNC HandlePrintableKey(key) ! Insert/overwrite character
FUNC ProcessInput()         ! Main input processing loop
```

**Command State Machine**:
```basic
VAR commandPrefix = 0       ! 0=none, 11=^K, 17=^Q
VAR lastKey = 0            ! For multi-key sequences
```

### 5. EditOperations Module

**Purpose**: Core text editing operations

**Key Functions**:
```basic
FUNC InsertChar(ch)         ! Insert character at cursor
FUNC DeleteChar()           ! Delete character at cursor (DEL)
FUNC Backspace()           ! Delete character before cursor
FUNC InsertNewLine()        ! Break line at cursor
FUNC JoinLines()           ! Join current line with next
FUNC DeleteLine()          ! ^Y - Delete entire line
FUNC OverwriteChar(ch)     ! Overwrite mode character placement
```

### 6. Storage Module

**Purpose**: File save/load operations

**Key Functions**:
```basic
FUNC NewFile()             ! ^KN - Clear document
FUNC SaveFile()            ! ^KS - Save current file  
FUNC SaveAndExit()         ! ^KD - Save and quit
FUNC QuitNoSave()          ! ^KQ - Quit without saving
FUNC LoadFile(name)        ! ^KR - Read file
FUNC PromptFileName()      ! Get file name from user
```

**File Format**: Plain text with Unix line endings (\n)

---

## Command Set Specification

### Navigation Commands (No Prefix)
| Key | Command | Action |
|-----|---------|--------|
| `^E` | Up | Move cursor up one line |
| `^X` | Down | Move cursor down one line |
| `^S` | Left | Move cursor left one character |
| `^D` | Right | Move cursor right one character |
| `^A` | Word Left | Move cursor left one word |
| `^F` | Word Right | Move cursor right one word |
| `^R` | Page Up | Move up one screen |
| `^C` | Page Down | Move down one screen |

### Quick Commands (^Q Prefix)
| Key | Command | Action |
|-----|---------|--------|
| `^QS` | Start of Line | Move to beginning of current line |
| `^QD` | End of Line | Move to end of current line |
| `^QR` | Top of File | Move to first line of document |
| `^QC` | End of File | Move to last line of document |

### File Commands (^K Prefix)
| Key | Command | Action |
|-----|---------|--------|
| `^KS` | Save | Save file (prompt for name if new) |
| `^KD` | Save and Exit | Save file and quit editor |
| `^KQ` | Quit | Quit without saving (confirm if modified) |
| `^KR` | Read File | Load file (prompt for name) |
| `^KN` | New File | Clear document and start fresh |

### Edit Commands (No Prefix)
| Key | Command | Action |
|-----|---------|--------|
| `^G` | Delete | Delete character at cursor |
| `^H` | Backspace | Delete character before cursor |
| `^Y` | Delete Line | Delete entire current line |
| `^N` | Insert Line | Insert new line at cursor |
| `ESC` | Exit | Exit editor (same as ^KQ) |
| Printable | Insert/Overwrite | Add character to text |

---

## Screen Layout

### Text Area (Lines 1-22)
```
Line 1:  [First line of document text]
Line 2:  [Second line of document text]  
...
Line 22: [22nd visible line]
```

### Status Line (Line 23)
```
Format: "FILE: filename.ext  LINE: 45/123  COL: 12  [INS/OVR]  [MODIFIED]"
Example: "FILE: MYPROG.BAS   LINE: 12/45   COL: 8   INS        *"
```

### Message Line (Line 24)  
```
Used for:
- Command prompts: "Save as: _"
- Error messages: "File not found!"
- Confirmations: "Quit without saving? (Y/N)"
- Command feedback: "File saved."
```

---

## Implementation Phases

### Phase 1: Core Editing
**Goal**: Basic text entry and cursor movement

**Deliverables**:
- Three-array line storage system
- Cursor movement (^E/^X/^S/^D navigation diamond)
- Character insert/delete
- Basic screen display
- Simple status line

**Success Criteria**: Can type text, move cursor, and see changes on screen

### Phase 2: File Operations
**Goal**: Persistent text storage

**Deliverables**:
- Save file (^KS) with filename prompt
- Load file (^KR) with filename prompt  
- New file (^KN) command
- Save and exit (^KD)
- Quit without save (^KQ) with confirmation
- File modification tracking

**Success Criteria**: Can create, edit, save, and reload text files

### Phase 3: Advanced Navigation
**Goal**: Complete WordStar navigation

**Deliverables**:
- Word movement (^A/^F)
- Line operations (^QS/^QD start/end of line)
- File operations (^QR/^QC top/end of file)  
- Page scrolling (^R/^C)
- Delete line (^Y) and insert line (^N)

**Success Criteria**: Full navigation command set working

### Future Phases (v2+)
- Screen refresh optimization
- Block operations (^K block commands)  
- Search and replace
- Undo functionality
- Horizontal scrolling for long lines

---

## Technical Specifications

### Memory Allocation
```
Text Storage:     8,000 bytes (100 lines × 80 chars)
Line List:          200 bytes (100 × 2 bytes)  
Free List:          200 bytes (100 × 2 bytes)
State Variables:     50 bytes (cursor, flags, etc.)
Screen Buffer:      TBD (Phase 2 optimization)
Total:           ~8,450 bytes
```

### Performance Targets
- **Cursor Movement**: Immediate response (<50ms)
- **Character Insert**: <100ms for typical line
- **Line Operations**: <200ms for insert/delete line
- **Screen Refresh**: <500ms for full redraw
- **File Save/Load**: Reasonable for files up to ~5K

### Input Processing Pipeline
```
Raw Key → Control Detection → Command Prefix → Command Execute → Screen Update
```

### File Format
- **Plain Text**: Standard ASCII text files
- **Line Endings**: Unix format (\n) for simplicity
- **Encoding**: 7-bit ASCII (compatible with terminal)
- **No Metadata**: Pure text content only

### Error Handling
- **Buffer Full**: Warn at 90% capacity, block at 100%
- **File Errors**: Clear error messages on status line
- **Invalid Commands**: Brief feedback, continue editing
- **Memory Issues**: Graceful degradation where possible

---

## Command Reference

### Control Key Legend
```
^E = Ctrl+E (ASCII 5)     ^K = Ctrl+K (ASCII 11)
^X = Ctrl+X (ASCII 24)    ^Q = Ctrl+Q (ASCII 17)
^S = Ctrl+S (ASCII 19)    ^Y = Ctrl+Y (ASCII 25)
^D = Ctrl+D (ASCII 4)     ^N = Ctrl+N (ASCII 14)
```

### Multi-Key Command Processing
```
^K followed by S = Save File
^K followed by D = Save and Exit
^Q followed by S = Start of Line
^Q followed by R = Top of File
```

### Status Line Indicators
```
INS     - Insert mode active
OVR     - Overwrite mode active  
*       - File has unsaved changes
MOD     - Alternative modified indicator
```

---

## Function Interface Specifications

### LineManager Module
```basic
! Initialization
FUNC InitLineManager()
    ! Initialize freeLines with [0,1,2...99]
    ! Set numLines = 0, numFree = MAX_LINES
    ! Clear lineList

! Line allocation
FUNC AllocateLine() ! Returns WORD slot index or -1 if full
FUNC FreeLine(slot) ! Add slot back to free list

! Document operations  
FUNC InsertTextLine(pos)     ! Insert new line at document line pos
FUNC DeleteTextLine(pos)     ! Delete document line pos
FUNC GetLineSlot(lineNum)    ! Get slot index for document line
FUNC GetLineText(lineNum)    ! Get CHAR* to line text
FUNC GetLineLength(lineNum)  ! Get line length
FUNC SetLineText(lineNum, text) ! Replace line content
FUNC GetDocumentLines()      ! Return numLines
```

### Cursor Module
```basic
! Position management
FUNC GetCursorLine()         ! Return currentLine  
FUNC GetCursorCol()          ! Return currentCol
FUNC SetCursorPos(line, col) ! Set cursor position with bounds check
FUNC GetScreenRow()          ! Convert to screen coordinates
FUNC GetScreenCol()          ! Convert to screen coordinates

! Movement operations
FUNC MoveCursorUp()          ! Move up, handle scrolling
FUNC MoveCursorDown()        ! Move down, handle scrolling  
FUNC MoveCursorLeft()        ! Move left, handle line wrap
FUNC MoveCursorRight()       ! Move right, handle line wrap
FUNC MoveWordLeft()          ! Skip to previous word
FUNC MoveWordRight()         ! Skip to next word
FUNC EnsureVisible()         ! Adjust topLine if cursor off-screen
```

### Display Module
```basic
! Screen control
FUNC InitDisplay()           ! Clear screen, set up display
FUNC RefreshScreen()         ! Full screen redraw
FUNC RefreshTextArea()       ! Redraw lines 1-22
FUNC RefreshLine(lineNum)    ! Redraw single line
FUNC UpdateStatusLine()      ! Update line 23 with file info
FUNC ShowMessage(msg)        ! Display message on line 24
FUNC ClearMessage()          ! Clear line 24

! VT100 primitives  
FUNC SendEsc()               ! Send ESC character
FUNC GotoXY(x, y)           ! Position cursor
FUNC ClearToEOL()           ! Clear from cursor to end of line
FUNC ShowCursor()           ! Make cursor visible
FUNC HideCursor()           ! Hide cursor
```

### Input Module  
```basic
! Key input (based on Keys.bas)
FUNC KeyReady()             ! Check if key available
FUNC GetRawKey()            ! Get key from serial buffer
FUNC GetKey()               ! Get key with escape sequence processing

! Command processing
FUNC ProcessInput()         ! Main input processing loop
FUNC HandleControlKey(key)  ! Process control characters
FUNC HandlePrefixCommand(prefix, key) ! Handle ^K, ^Q commands
FUNC IsWordChar(ch)         ! Test if character is part of word
```

### EditOperations Module
```basic
! Character operations
FUNC InsertChar(ch)         ! Insert character at cursor
FUNC OverwriteChar(ch)      ! Overwrite character at cursor
FUNC DeleteChar()           ! Delete character at cursor
FUNC Backspace()            ! Delete character before cursor

! Line operations
FUNC InsertNewLine()        ! Break line at cursor position
FUNC JoinWithNextLine()     ! Join current line with next
FUNC DeleteCurrentLine()    ! Delete entire current line
FUNC InsertBlankLine()      ! Insert empty line at cursor

! Utility operations
FUNC GetCurrentChar()       ! Get character at cursor
FUNC GetCurrentLineText()   ! Get text of current line
FUNC SetCurrentLineText(text) ! Replace current line text
```

### Storage Module
```basic
! File operations
FUNC NewFile()              ! Clear document, reset state
FUNC SaveFile()             ! Save with current filename
FUNC SaveAsFile(name)       ! Save with new filename
FUNC LoadFile(name)         ! Load file into document
FUNC FileExists(name)       ! Check if file exists

! File conversion
FUNC DocumentToText(buffer) ! Convert lines to flat text
FUNC TextToDocument(buffer) ! Parse text into lines

! User interaction
FUNC PromptFileName()       ! Get filename from user
FUNC ConfirmQuit()          ! Confirm quit without save
```

---

## State Machine Design

### Command Prefix States
```
STATE_NORMAL = 0        ! Normal editing mode
STATE_K_PREFIX = 1      ! After ^K, waiting for file command  
STATE_Q_PREFIX = 2      ! After ^Q, waiting for quick command
```

### Input Processing Flow
```
1. GetKey() → Raw keystroke
2. If control character → HandleControlKey()
3. If prefix active → HandlePrefixCommand()  
4. If printable → InsertChar() or OverwriteChar()
5. UpdateDisplay() → Refresh changed areas
6. Return to step 1
```

---

## File Format Specification

### Save Format
- **Content**: Plain ASCII text
- **Line Endings**: Unix format (\n) 
- **Encoding**: 7-bit ASCII only
- **Structure**: Flat text file, no metadata

### Load Process  
```
1. Read entire file as string
2. Split on \n characters
3. Allocate line slots for each line
4. Copy line content to lineData slots
5. Build lineList with allocated slots
6. Set cursor to start of file
```

### Save Process
```  
1. Walk lineList in document order
2. Concatenate line content with \n separators
3. Write flat text to EEPROM file
4. Clear modified flag
```

---

## Error Conditions and Handling

### Memory Limits
- **Line Buffer Full**: No free line slots available
- **Line Too Long**: Input exceeds 79 characters
- **Document Too Large**: Approaching memory limits

### File System Errors
- **File Not Found**: During load operation
- **Storage Full**: EEPROM capacity exceeded  
- **Invalid Filename**: Illegal characters or length

### Recovery Strategies
- **Graceful Degradation**: Continue operating with limitations
- **Clear Error Messages**: Brief, actionable feedback
- **State Recovery**: Return to known good state after errors

---

## Development Milestones

### Milestone 1: Basic Editor (1-2 weeks)
- [ ] LineManager with three arrays working
- [ ] Cursor movement (^E/^X/^S/^D diamond)
- [ ] Character insert/delete
- [ ] Basic screen display
- [ ] Exit with ESC

### Milestone 2: File Operations (1 week)  
- [ ] Save file (^KS) with filename prompt
- [ ] Load file (^KR) 
- [ ] New file (^KN)
- [ ] Save and exit (^KD)
- [ ] Quit without save (^KQ) with confirmation

### Milestone 3: Advanced Navigation (1 week)
- [ ] Word movement (^A/^F)
- [ ] Quick commands (^QS/^QD/^QR/^QC)
- [ ] Page scrolling (^R/^C)
- [ ] Line operations (^Y delete, ^N insert)
- [ ] Insert/overwrite modes

### Success Metrics
- **Functionality**: Can edit, save, and reload text files
- **Usability**: Familiar WordStar key commands work correctly
- **Performance**: Responsive editing on target hardware
- **Reliability**: Handles errors gracefully without crashes

---

## Notes and Considerations

### WordStar Authenticity vs. Simplification
- **Keep**: Core navigation diamond, ^K file commands, ^Q quick commands
- **Simplify**: Block operations deferred to v3, complex formatting removed
- **Modernize**: Use standard file extensions, Unix line endings

### Memory Optimization Opportunities (Future)
- **Line Length Optimization**: Variable-length lines with length prefix
- **Text Compression**: Simple compression for stored files
- **Screen Caching**: Remember displayed content to minimize redraws

### Platform Integration
- **EEPROM Files**: Integrate with existing HopperBASIC file system
- **Terminal Compatibility**: Support standard VT100 terminals
- **Hardware Abstraction**: Use HopperBASIC's serial I/O functions

---

*WordStar-Style Editor for HopperBASIC v1.0 Architecture*  
*Target Implementation: 2025*
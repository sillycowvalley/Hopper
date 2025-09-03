# WordStar-Style Text Editor for HopperBASIC
## Architecture Specification v1.0 (Compact Function Names)

## Unified Storage Operations

### Line Allocation Example
```basic
! Document uses lines: DOC[0]=slot 5, DOC[1]=slot 2, DOC[2]=slot 8
! Clipboard empty: NC=0  
! Free slots: FREE[0]=0, FREE[1]=1, FREE[2]=3, FREE[3]=4...

! Copy line 1 to clipboard:
s=aL()              ! Get slot (returns 0, decrements NF)  
! Copy text from LD[DOC[1]*LL] to LD[s*LL]
CLIP[0]=s:NC=1      ! Clipboard now has 1 line

! Insert new line at position 1:
s=aL()              ! Get slot (returns 1)
FOR i=NL TO 1 STEP -1:DOC[i+1]=DOC[i]:NEXT i ! Shift refs
DOC[1]=s:NL=NL+1    ! Insert new ref, increment count
```

### Undo Operation Example
```basic
! Before edit - save current line to undo:
FUNC sU()
  FOR i=0 TO NU-1:fL(UNDO[i]):NEXT i ! Free old undo
  s=aL():UNDO[0]=s:NU=1:UL=L:UC=C    ! Save line ref & cursor
  ! Copy current line text to undo slot
ENDFUNC

! Restore undo:
FUNC rU()
  IF NU>0 THEN ! Copy back, free current, restore cursor
    s=DOC[UL]:DOC[UL]=UNDO[0]:fL(s):L=UL:C=UC:NU=0
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
CHAR LD[MAX_LINES * LINE_LENGTH]  ! lineData -> LD
```

#### 2. Logical Document Order  
```basic
! Document line order (references to LD slots)
WORD DOC[MAX_LINES]  ! lineList -> DOC
VAR NL = 0           ! numLines -> NL
```

#### 3. Free Slot Management
```basic
! Available line slots for allocation
WORD FREE[MAX_LINES] ! freeLines -> FREE
VAR NF = MAX_LINES   ! numFree -> NF
```

### Editor State Variables
```basic
! Cursor position
VAR L = 0            ! currentLine -> L
VAR C = 0            ! currentCol -> C

! Display state  
VAR TL = 0           ! topLine -> TL
VAR LC = 0           ! leftCol -> LC

! Editor modes
BIT IM = TRUE        ! insertMode -> IM
BIT MOD = FALSE      ! fileModified -> MOD
BIT QP = FALSE       ! quitPending -> QP

! File management
CHAR FN[13]          ! fileName -> FN
VAR FL = 0           ! fileLength -> FL
```

### Screen Layout Constants
```basic
CONST SW = 80        ! SCREEN_WIDTH
CONST SH = 24        ! SCREEN_HEIGHT
CONST TXT = 22       ! TEXT_LINES (lines 1-22 for text)
CONST STAT = 23      ! STATUS_LINE (line 23)
CONST MSG = 24       ! MESSAGE_LINE (line 24)
```

---

## Module Architecture

### 1. LineManager Module

**Purpose**: Manage the three-array line storage system

**Key Functions**:
```basic
FUNC iLM()           ! InitLineManager - Initialize arrays and free list
FUNC aL()            ! AllocateLine - Get free slot, returns slot index
FUNC fL(slot)        ! FreeLine - Return slot to free list
FUNC iTL(pos)        ! InsertTextLine - Insert new line at document position  
FUNC dTL(pos)        ! DeleteTextLine - Remove line from document
FUNC gLT(ln)         ! GetLineText - Get pointer to line text
FUNC sLT(ln, txt)    ! SetLineText - Replace line content
FUNC gLL(ln)         ! GetLineLength - Get line length (to null terminator)
```

**Memory Layout**:
```
LD:   [Slot0: "Hello\0......."][Slot1: "World\0......."][Slot2: ...]
DOC:  [0][1][2]...  ! Document uses slots 0,1,2 in order
FREE: [3][4][5]...  ! Slots 3+ available for allocation
```

### 2. Cursor Module

**Purpose**: Track cursor position and handle movement commands

**Key Functions**:
```basic
FUNC mCU()           ! MoveCursorUp - ^E Move up one line
FUNC mCD()           ! MoveCursorDown - ^X Move down one line  
FUNC mCL()           ! MoveCursorLeft - ^S Move left one character
FUNC mCR()           ! MoveCursorRight - ^D Move right one character
FUNC mWL()           ! MoveWordLeft - ^A Move left one word
FUNC mWR()           ! MoveWordRight - ^F Move right one word
FUNC mSL()           ! MoveStartOfLine - ^QS Move to line start
FUNC mEL()           ! MoveEndOfLine - ^QD Move to line end
FUNC mTF()           ! MoveTopOfFile - ^QR Move to first line
FUNC mEF()           ! MoveEndOfFile - ^QC Move to last line
FUNC mPg(dir)        ! MovePage - ^R/^C Page up/down
FUNC eCV()           ! EnsureCursorVisible - Adjust TL if needed
```

**Coordinate Systems**:
- **Document coordinates**: (L, C) in logical text
- **Screen coordinates**: (screenRow, screenCol) for display
- **Buffer coordinates**: Absolute offset in LD for direct access

### 3. Display Module  

**Purpose**: Handle all screen output and VT100 control

**Key Functions**:
```basic
FUNC iD()            ! InitDisplay - Clear screen, hide cursor
FUNC rS()            ! RefreshScreen - Full screen redraw
FUNC rTA()           ! RefreshTextArea - Redraw lines 1-22 only
FUNC uSL()           ! UpdateStatusLine - Show file, position, mode
FUNC sM(msg)         ! ShowMessage - Display message on line 24
FUNC cM()            ! ClearMessage - Clear message line
FUNC pC()            ! PositionCursor - Move cursor to current position
FUNC sU()            ! ScrollUp - Scroll display up one line
FUNC sD()            ! ScrollDown - Scroll display down one line
```

**VT100 Helper Functions**:
```basic
FUNC sE()            ! SendEsc - Output ESC character
FUNC gXY(x, y)       ! GotoXY - Position cursor  
FUNC cS()            ! ClearScreen - Clear entire screen
FUNC cL()            ! ClearLine - Clear current line
```

### 4. Input Module

**Purpose**: Handle keyboard input and command interpretation

**Key Functions**:
```basic
FUNC gK()            ! GetKey - Get single key (building on Keys.bas)
FUNC iCK(key)        ! IsControlKey - Test if key < 32
FUNC hCK(key)        ! HandleControlKey - Process WordStar commands
FUNC hPK(key)        ! HandlePrintableKey - Insert/overwrite character
FUNC pI()            ! ProcessInput - Main input processing loop
```

**Command State Machine**:
```basic
VAR CP = 0           ! commandPrefix -> CP (0=none, 11=^K, 17=^Q)
VAR LK = 0           ! lastKey -> LK (for multi-key sequences)
```

### 5. EditOperations Module

**Purpose**: Core text editing operations

**Key Functions**:
```basic
FUNC iC(ch)          ! InsertChar - Insert character at cursor
FUNC dC()            ! DeleteChar - Delete character at cursor (DEL)
FUNC bS()            ! Backspace - Delete character before cursor
FUNC iNL()           ! InsertNewLine - Break line at cursor
FUNC jL()            ! JoinLines - Join current line with next
FUNC dL()            ! DeleteLine - ^Y Delete entire line
FUNC oC(ch)          ! OverwriteChar - Overwrite mode character placement
```

### 6. Storage Module

**Purpose**: File save/load operations

**Key Functions**:
```basic
FUNC nF()            ! NewFile - ^KN Clear document
FUNC sF()            ! SaveFile - ^KS Save current file  
FUNC sE()            ! SaveAndExit - ^KD Save and quit
FUNC qNS()           ! QuitNoSave - ^KQ Quit without saving
FUNC lF(name)        ! LoadFile - ^KR Read file
FUNC pFN()           ! PromptFileName - Get file name from user
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

## Function Interface Specifications

### LineManager Module
```basic
! Initialization
FUNC iLM()           ! InitLineManager
    ! Initialize FREE with [0,1,2...99]
    ! Set NL = 0, NF = MAX_LINES
    ! Clear DOC

! Line allocation
FUNC aL()            ! AllocateLine - Returns WORD slot index or -1 if full
FUNC fL(slot)        ! FreeLine - Add slot back to free list

! Document operations  
FUNC iTL(pos)        ! InsertTextLine - Insert new line at document line pos
FUNC dTL(pos)        ! DeleteTextLine - Delete document line pos
FUNC gLS(ln)         ! GetLineSlot - Get slot index for document line
FUNC gLT(ln)         ! GetLineText - Get CHAR* to line text
FUNC gLL(ln)         ! GetLineLength - Get line length
FUNC sLT(ln, txt)    ! SetLineText - Replace line content
FUNC gDL()           ! GetDocumentLines - Return NL
```

### Cursor Module
```basic
! Position management
FUNC gCL()           ! GetCursorLine - Return L
FUNC gCC()           ! GetCursorCol - Return C
FUNC sCP(ln, cl)     ! SetCursorPos - Set cursor position with bounds check
FUNC gSR()           ! GetScreenRow - Convert to screen coordinates
FUNC gSC()           ! GetScreenCol - Convert to screen coordinates

! Movement operations
FUNC mCU()           ! MoveCursorUp - Move up, handle scrolling
FUNC mCD()           ! MoveCursorDown - Move down, handle scrolling  
FUNC mCL()           ! MoveCursorLeft - Move left, handle line wrap
FUNC mCR()           ! MoveCursorRight - Move right, handle line wrap
FUNC mWL()           ! MoveWordLeft - Skip to previous word
FUNC mWR()           ! MoveWordRight - Skip to next word
FUNC eV()            ! EnsureVisible - Adjust TL if cursor off-screen
```

### Display Module
```basic
! Screen control
FUNC iD()            ! InitDisplay - Clear screen, set up display
FUNC rS()            ! RefreshScreen - Full screen redraw
FUNC rTA()           ! RefreshTextArea - Redraw lines 1-22
FUNC rL(ln)          ! RefreshLine - Redraw single line
FUNC uSL()           ! UpdateStatusLine - Update line 23 with file info
FUNC sM(msg)         ! ShowMessage - Display message on line 24
FUNC cM()            ! ClearMessage - Clear line 24

! VT100 primitives  
FUNC sE()            ! SendEsc - Send ESC character
FUNC gXY(x, y)       ! GotoXY - Position cursor
FUNC cEL()           ! ClearToEOL - Clear from cursor to end of line
FUNC sC()            ! ShowCursor - Make cursor visible
FUNC hC()            ! HideCursor - Hide cursor
```

### Input Module  
```basic
! Key input (based on Keys.bas)
FUNC kR()            ! KeyReady - Check if key available
FUNC gRK()           ! GetRawKey - Get key from serial buffer
FUNC gK()            ! GetKey - Get key with escape sequence processing

! Command processing
FUNC pI()            ! ProcessInput - Main input processing loop
FUNC hCK(key)        ! HandleControlKey - Process control characters
FUNC hPC(pfx, key)   ! HandlePrefixCommand - Handle ^K, ^Q commands
FUNC iWC(ch)         ! IsWordChar - Test if character is part of word
```

### EditOperations Module
```basic
! Character operations
FUNC iC(ch)          ! InsertChar - Insert character at cursor
FUNC oC(ch)          ! OverwriteChar - Overwrite character at cursor
FUNC dC()            ! DeleteChar - Delete character at cursor
FUNC bS()            ! Backspace - Delete character before cursor

! Line operations
FUNC iNL()           ! InsertNewLine - Break line at cursor position
FUNC jNL()           ! JoinWithNextLine - Join current line with next
FUNC dCL()           ! DeleteCurrentLine - Delete entire current line
FUNC iBL()           ! InsertBlankLine - Insert empty line at cursor

! Utility operations
FUNC gCC()           ! GetCurrentChar - Get character at cursor
FUNC gCLT()          ! GetCurrentLineText - Get text of current line
FUNC sCLT(txt)       ! SetCurrentLineText - Replace current line text
```

### Storage Module
```basic
! File operations
FUNC nF()            ! NewFile - Clear document, reset state
FUNC sF()            ! SaveFile - Save with current filename
FUNC sAF(name)       ! SaveAsFile - Save with new filename
FUNC lF(name)        ! LoadFile - Load file into document
FUNC fE(name)        ! FileExists - Check if file exists

! File conversion
FUNC dTT(buf)        ! DocumentToText - Convert lines to flat text
FUNC tTD(buf)        ! TextToDocument - Parse text into lines

! User interaction
FUNC pFN()           ! PromptFileName - Get filename from user
FUNC cQ()            ! ConfirmQuit - Confirm quit without save
```

---

## State Machine Design

### Command Prefix States
```
ST_NORM = 0          ! STATE_NORMAL - Normal editing mode
ST_K = 1             ! STATE_K_PREFIX - After ^K, waiting for file command  
ST_Q = 2             ! STATE_Q_PREFIX - After ^Q, waiting for quick command
```

### Input Processing Flow
```
1. gK() → Raw keystroke
2. If control character → hCK()
3. If prefix active → hPC()  
4. If printable → iC() or oC()
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
4. Copy line content to LD slots
5. Build DOC with allocated slots
6. Set cursor to start of file
```

### Save Process
```  
1. Walk DOC in document order
2. Concatenate line content with \n separators
3. Write flat text to EEPROM file
4. Clear MOD flag
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

## Compact Function Reference

### LineManager (LM)
```basic
iLM()        ! Init line manager
aL()         ! Allocate line slot
fL(s)        ! Free line slot  
iTL(p)       ! Insert text line
dTL(p)       ! Delete text line
gLT(n)       ! Get line text pointer
sLT(n,t)     ! Set line text
gLL(n)       ! Get line length
gLS(n)       ! Get line slot
gDL()        ! Get document lines count
```

### Cursor (C)
```basic
mCU/mCD()    ! Move cursor up/down
mCL/mCR()    ! Move cursor left/right  
mWL/mWR()    ! Move word left/right
mSL/mEL()    ! Move start/end line
mTF/mEF()    ! Move top/end file
mPg(d)       ! Move page up/down
eV()         ! Ensure cursor visible
gCL/gCC()    ! Get cursor line/col
sCP(l,c)     ! Set cursor position
gSR/gSC()    ! Get screen row/col
```

### Display (D)  
```basic
iD()         ! Init display
rS/rTA()     ! Refresh screen/text area
rL(n)        ! Refresh line
uSL()        ! Update status line
sM(m)/cM()   ! Show/clear message
pC()         ! Position cursor
sU/sD()      ! Scroll up/down
sE()         ! Send escape
gXY(x,y)     ! Goto XY
cS/cL()      ! Clear screen/line
sC/hC()      ! Show/hide cursor
```

### Input (I)
```basic
kR()         ! Key ready
gRK/gK()     ! Get raw key/processed key
pI()         ! Process input loop
hCK(k)       ! Handle control key
hPC(p,k)     ! Handle prefix command  
iWC(c)       ! Is word character
iCK(k)       ! Is control key
hPK(k)       ! Handle printable key
```

### EditOperations (E)
```basic
iC(c)        ! Insert character
oC(c)        ! Overwrite character
dC()         ! Delete character
bS()         ! Backspace
iNL()        ! Insert new line
jNL()        ! Join next line
dCL()        ! Delete current line
iBL()        ! Insert blank line
gCC()        ! Get current character
gCLT()       ! Get current line text
sCLT(t)      ! Set current line text
```

### Storage (S)
```basic
nF()         ! New file
sF()         ! Save file
sAF(n)       ! Save as file
lF(n)        ! Load file  
fE(n)        ! File exists
dTT(b)       ! Document to text
tTD(b)       ! Text to document
pFN()        ! Prompt filename
cQ()         ! Confirm quit
```

---

## Notes and Considerations

### WordStar Authenticity vs. Simplification
- **Keep**: Core navigation diamond, ^K file commands, ^Q quick commands
- **Simplify**: Insert mode only (no overwrite toggle), no horizontal scroll
- **Defer**: Block operations to v2, complex formatting removed
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

*WordStar-Style Editor for HopperBASIC v1.0 Architecture (Compact)*  
*Target Implementation: 2025*
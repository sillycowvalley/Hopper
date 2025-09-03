# Hopper BASIC Text Editor - Project Specification

## Overview

A lightweight, responsive text editor written in Hopper BASIC for 6502 systems, targeting VT100-compatible terminals. Designed for minimal memory usage with maximum responsiveness through efficient data structures and rendering optimizations.

## Design Goals

- **Responsive scrolling** - Test navigation performance with arrow keys
- **Memory efficient** - ~11KB total footprint  
- **Modular architecture** - Clean separation of concerns
- **VT100 compatible** - Standard terminal escape sequences
- **60x24 display** - 60 columns, 22 text rows + 2 UI rows

## Memory Architecture

### Line Buffer Design

**Fixed-length records**: 64 bytes per line
```
Byte 0-1:   Next pointer (WORD) - 0x0000 = end of chain
Byte 2-61:  Text content (60 characters maximum)
Byte 62:    Null terminator (0x00)
Byte 63:    Spare byte (future: flags, syntax state, etc.)
```

**Storage allocation**:
- **150 line records** total available
- **Linked list structure** for O(1) insertion/deletion
- **Free list management** for unused records
- **Total size**: 150 × 64 = 9,600 bytes

### Screen Buffer Design

**ASCII + Dirty Bit encoding**: Each screen position uses 1 byte
```
Bit 7:      Dirty flag (1 = needs redraw, 0 = clean)
Bits 0-6:   ASCII character (0-127)
```

**Buffer dimensions**:
- **60 columns × 24 rows** = 1,440 bytes total
- **Rows 0-21**: Text display area  
- **Rows 22-23**: Status/UI area
- **No separate dirty tracking array needed**

### Total Memory Usage: ~11KB

## Module Architecture

### 1. Terminal Module (`tm`)
**Purpose**: Low-level VT100 terminal control

**Interface**:
- `tmInit()` - Initialize terminal state
- `tmClear()` - Clear entire screen
- `tmGoto(x, y)` - Position cursor (1-based coordinates)
- `tmHideCursor()` / `tmShowCursor()` - Cursor visibility

**Constants**:
- `tmcEscape = 0x1B` - ESC character
- `tmcWidth = 60`, `tmcHeight = 24` - Screen dimensions

### 2. Keyboard Module (`kb`)
**Purpose**: Input handling and key code translation

**Interface**:
- `kbInit()` - Initialize keyboard state
- `kbReady()` - Check if key available
- `kbGetKey()` - Get processed key (handles escape sequences)
- `kbGetRaw()` - Get unprocessed character

**Key codes** (using constants to avoid ASCII conflicts):
- `kbcUp = 128`, `kbcDown = 129`, `kbcLeft = 130`, `kbcRight = 131`
- `kbcEnter = 13`, `kbcEscape = 27`

**State management**:
- Escape sequence parsing state machine
- Serial buffer pointer management

### 3. Buffer Module (`buf`)
**Purpose**: Text storage and line management

**Interface**:
- `bufInit()` - Initialize with test data
- `bufGetLinePtr(lineNum)` - Get pointer to Nth line record
- `bufGetLineText(ptr)` - Extract text from line record  
- `bufWalkNext(ptr)` - Get next line in chain
- `bufGetLineCount()` - Total lines in use

**Internal management**:
- Linked list of active lines
- Free list of available records
- Line numbering (logical vs physical separation)

### 4. Cursor Module (`cur`)
**Purpose**: Cursor position and movement validation

**Interface**:
- `curInit()` - Set initial position
- `curMoveTo(line, col)` - Absolute positioning with bounds checking
- `curMoveUp()`, `curMoveDown()`, `curMoveLeft()`, `curMoveRight()` - Relative movement
- `curGetLine()`, `curGetCol()` - Current position accessors

**State**:
- `curLine`, `curCol` - current logical position
- Bounds validation against buffer content

### 5. Screen Module (`scr`)
**Purpose**: Double buffering, dirty tracking, render coordination

**Interface**:
- `scrInit()` - Initialize screen buffer  
- `scrSetViewStart(linePtr)` - Set top visible line
- `scrMarkDirty()` - Force full redraw
- `scrUpdate()` - Incremental render (only dirty bytes)
- `scrRedraw()` - Force complete redraw

**Rendering strategy**:
- Compare buffer content vs current screen buffer
- Set dirty bits for changed characters
- Output only dirty characters with cursor positioning
- Clear dirty bits after successful output

### 6. Editor Module (`ed`)
**Purpose**: Main control loop and command coordination

**Interface**:
- `edInit()` - Initialize entire system
- `edRun()` - Main event loop
- `edHandleKey(key)` - Process user input
- `edShutdown()` - Clean exit

**Responsibilities**:
- Coordinate between all other modules
- Handle scrolling decisions
- Manage view window positioning
- Process quit commands

## Key Algorithms

### Efficient Scrolling

**Single line scroll** (up/down arrows):
1. **Buffer module**: Walk to new start line (linked list traversal)
2. **Screen module**: 
   - Most screen positions unchanged → remain clean
   - Only new visible line → gets dirty bits set
   - Incremental render outputs ~60 characters maximum

**Page scroll** (future):
- Similar principle, but 22 lines of changes instead of 1

### Dirty Bit Management

**Character comparison** during screen update:
```basic
! newChar = character from line buffer
! oldByte = screenBuf[position]
! IF newChar <> (oldByte & 0x7F) THEN
!     screenBuf[position] = newChar | 0x80  ! Set dirty bit
! ENDIF
```

**Render pass**:
```basic
! FOR each screen position
!     IF (screenBuf[pos] & 0x80) <> 0 THEN  ! Check dirty bit
!         ! Output this character with cursor positioning
!         screenBuf[pos] = screenBuf[pos] & 0x7F  ! Clear dirty bit
!     ENDIF
```

### Test Data Population

**Initial content**: 50+ lines of varied test data
- Lines of different lengths (test horizontal scrolling readiness)
- Some empty lines (test sparse content)
- Numbered lines for easy position verification
- Mix of content types (simulate real editing scenarios)

## Performance Targets

### Responsiveness Goals
- **Single line scroll**: <100ms perceived delay
- **Navigation**: Immediate cursor response
- **Screen updates**: Minimal flicker or artifacts

### Memory Efficiency
- **~75% memory usage**: 150 line capacity, ~100 lines of test data
- **Linked list overhead**: 2 bytes per line (acceptable)
- **Screen buffer**: 1,440 bytes (reasonable for 6502)

## Implementation Strategy

### Phase 1: Framework
1. **Global arrays and constants** - All storage allocation
2. **Terminal module** - Basic VT100 output
3. **Keyboard module** - Arrow key detection
4. **Simple test**: Clear screen, show cursor, respond to arrows

### Phase 2: Text Display  
1. **Buffer module** - Line storage, test data population
2. **Screen module** - Basic rendering without dirty tracking
3. **Display test**: Show text, scroll with arrows

### Phase 3: Optimization
1. **Implement dirty bit tracking** - Minimize terminal output
2. **Cursor module** - Proper bounds checking and movement
3. **Editor module** - Main loop coordination
4. **Performance testing**: Measure scroll responsiveness

### Phase 4: Polish
1. **Status line** - Current position display
2. **Error handling** - Graceful degradation
3. **Clean exit** - Restore terminal state

## Technical Constraints

### Hopper BASIC Limitations
- **Arrays must be global** - All line and screen buffers at file scope
- **No underscores** - Use camelCase module prefixes
- **Function size limits** - Keep modules under ~50 lines each
- **All locals declared first** - Before any control flow
- **No reserved word conflicts** - Careful identifier selection

### VT100 Terminal Requirements
- **ESC sequences**: Cursor positioning, screen clearing
- **Input handling**: Raw mode for arrow keys
- **Coordinate system**: 1-based (terminal) vs 0-based (internal)

## Data Flow

```
User Input → Keyboard → Editor → Cursor → Buffer
                              ↓
Terminal ← Screen ← [comparison] ← Buffer
```

**Key insight**: Buffer content drives screen rendering, cursor position drives view window, minimal data copying throughout the pipeline.

## Success Criteria

1. **Populate 100+ lines** of test content successfully
2. **Arrow navigation** works smoothly in both directions  
3. **Screen updates** show only changed content
4. **Memory usage** stays within target bounds
5. **No visible flicker** during scroll operations
6. **Consistent response time** regardless of content position

## Future Extensibility

The modular design enables future enhancements:
- **Variable length lines** (replace buf module)
- **Horizontal scrolling** (extend scr module) 
- **Syntax highlighting** (use spare byte in line records)
- **Multiple buffers** (extend buf module)
- **Search/replace** (add modules)

## Risk Mitigation

**Potential issues**:
- **Token buffer limits**: Keep functions small and focused
- **Memory fragmentation**: Fixed allocation avoids this
- **Performance degradation**: Dirty bit tracking minimizes output
- **Terminal compatibility**: Stick to basic VT100 subset

The linked list + dirty bit combination should give us very responsive scrolling even on a 1MHz 6502, since we're doing minimal work per scroll operation and only outputting characters that actually changed.
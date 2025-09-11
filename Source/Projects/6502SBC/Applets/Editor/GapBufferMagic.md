## How the GapBuffer Works in the editor

### The Core Concept

A gap buffer is a dynamic data structure that makes text editing efficient by maintaining a "gap" (empty space) at the cursor position.
This allows insertions and deletions at the cursor to be O(1) operations instead of requiring expensive array shifts.

### Basic Structure

```
                    Buffer in Memory
    ┌────────────────────────────────────────────┐
    │ Text Before │    GAP (empty)    │Text After│
    └────────────────────────────────────────────┘
                  ↑                    ↑
              gbGapStart            gbGapEnd
```

The buffer consists of three logical parts:
- **Text before the gap**: From buffer start to `gbGapStart`
- **The gap itself**: Empty space from `gbGapStart` to `gbGapEnd`  
- **Text after the gap**: From `gbGapEnd` to buffer end

### Key Variables in the editor

```
gbBuffer      → Points to start of allocated memory
gbBufferSize  → Total allocated size
gbGapStart    → Where the gap begins (cursor position)
gbGapEnd      → Where the gap ends
GapValue      → Public parameter for position operations
```

### How Text Operations Work

#### 1. **Inserting a Character**

When you type 'A' at the cursor:

```
Before:  "Hello|___World"  (gap at position 5)
         └─────┘   └─────┘
         gbGapStart=5  gbGapEnd=8

Action:  InsertChar('A')
         - Write 'A' at gbGapStart
         - Increment gbGapStart

After:   "HelloA|__World"
         └──────┘  └─────┘
         gbGapStart=6  gbGapEnd=8
```

#### 2. **Backspace**

```
Before:  "Hello|___World"
         gbGapStart=5  gbGapEnd=8

Action:  Backspace()
         - Decrement gbGapStart

After:   "Hell|____World"
         gbGapStart=4  gbGapEnd=8
```

#### 3. **Delete**

```
Before:  "Hello|___World"
         gbGapStart=5  gbGapEnd=8

Action:  Delete()
         - Increment gbGapEnd

After:   "Hello|____orld"
         gbGapStart=5  gbGapEnd=9
```

### Moving the Cursor (Gap Movement)

When you move the cursor, the gap must follow. This is the most expensive operation:

```
Moving from position 5 to position 11:

Before:  "Hello|___World"
         gbGapStart=5  gbGapEnd=8

Action:  MoveGapTo(11)
         - Copy "World" backwards to fill old gap
         - Create new gap at position 11

Step 1:  "HelloWorld|___"  (moved text, gap at end)
         gbGapStart=10  gbGapEnd=13
```

The `MoveGapTo` method uses efficient block memory copies to minimize overhead.

### Growing the Buffer

When the gap becomes full (gbGapStart == gbGapEnd):

```
Before:  "HelloWorld|"  (no gap left!)
         gbGapStart=10  gbGapEnd=10

Action:  growBuffer()
         - Allocate new buffer (2x size)
         - Copy text before gap
         - Create new larger gap
         - Copy text after gap

After:   "HelloWorld|__________"  (new gap created)
         gbGapStart=10  gbGapEnd=20
```

## The Undo/Redo Mechanism

This implementation uses a particularly elegant approach to undo/redo that takes advantage of the gap buffer structure itself.

### The Magic: Gap Swapping

Instead of storing copies of deleted/inserted text, the undo system stores **alternate gap positions**:

```
Undo State Variables:
- undoOp        → Type of last operation (Inserted/Deleted/None)
- undoGapStart  → Previous gap start position
- undoGapEnd    → Previous gap end position
```

### How Undo Works

#### Case 1: Undoing an Insert

```
Original: "Hello|___World"
          gbGapStart=5, gbGapEnd=8

Type:     "ABC"

After:    "HelloABC|World"  
          gbGapStart=8, gbGapEnd=8
          undoOp=Inserted
          undoGapStart=5, undoGapEnd=8  (saved old gap)

Undo:     ToggleUndo()
          - Swap current gap with undo gap
          - Text "ABC" is now in the gap (invisible!)
          
Result:   "Hello|___World"
          gbGapStart=5, gbGapEnd=8
          undoOp=Deleted (flipped!)
          undoGapStart=8, undoGapEnd=8  (ready for redo)
```

#### Case 2: Undoing a Delete

```
Original: "HelloABC|World"
          gbGapStart=8, gbGapEnd=8

Delete:   Backspace 3 times

After:    "Hello|___World"
          gbGapStart=5, gbGapEnd=8
          undoOp=Deleted
          undoGapStart=8, undoGapEnd=8  (saved old gap)

Undo:     ToggleUndo()
          - Swap gaps again
          - "ABC" reappears (was in the gap)

Result:   "HelloABC|World"
          gbGapStart=8, gbGapEnd=8
          undoOp=Inserted (flipped!)
```

### The Advantages of This Design

1. **No Extra Storage**: Deleted text isn't copied elsewhere - it's just hidden in the gap!
2. **Instant Undo/Redo**: Just swap two pairs of pointers (O(1) operation)
3. **Toggle Design**: The same `ToggleUndo()` function handles both undo AND redo
4. **Natural Integration**: The gap buffer already has the infrastructure needed: low code footprint

### Visual Representation of Undo/Redo Cycle

```
State 1: "Hello|World"
         Current: gbGapStart=5, gbGapEnd=5
         Undo:    (empty)

Type 'X': "HelloX|World"
         Current: gbGapStart=6, gbGapEnd=6
         Undo:    undoGapStart=5, undoGapEnd=5
         undoOp=Inserted

Undo:    "Hello|World"   (X is now in gap at position 5-6)
         Current: gbGapStart=5, gbGapEnd=6
         Undo:    undoGapStart=6, undoGapEnd=6
         undoOp=Deleted (toggled!)

Redo:    "HelloX|World"  (X restored from gap)
         Current: gbGapStart=6, gbGapEnd=6
         Undo:    undoGapStart=5, undoGapEnd=6
         undoOp=Inserted (toggled back!)
```

### Limitations

This elegant design has one limitation: it only supports single-level undo/redo.
However, for a lightweight 6502 editor, this is an acceptable trade-off between functionality and memory usage.

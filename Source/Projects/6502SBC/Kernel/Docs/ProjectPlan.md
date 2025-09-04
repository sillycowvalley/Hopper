# Hopper 6502 API Reference & Testing Project Plan

## Module Public APIs

### Time Module
Provides timing and delay functionality using hardware timer interrupts.

#### Public Methods
- **Delay()** - Delays execution for the number of milliseconds specified in TOP (16-bit value)
- **Millis()** - Returns current millisecond counter in TOP (32-bit value from TICK0-3)
- **Seconds()** - Returns current time in seconds (Millis/1000) in TOP (32-bit value)

---

### Heap/Memory Module  
Dynamic memory management with best-fit allocation strategy.

#### Public Methods
- **Initialize()** - Initializes heap, probes RAM size, sets up free list
  - Discovers RAM size: 16K/32K/48K/56K
  - Sets HEAPSTART and HEAPSIZE in zero page
  - Creates initial free list block
  
- **Allocate()** - Allocates memory block
  - Input: ACC = requested size (16-bit)
  - Output: IDX = allocated address (0x0000 if failed)
  - Uses best-fit algorithm, 8-byte alignment
  
- **Free()** - Frees memory block
  - Input: IDX = address to free
  - Output: C set on success
  - Coalesces adjacent free blocks
  
- **Available()** - Returns total free memory
  - Output: ACC = total free bytes (16-bit)
  - Walks free list summing all free blocks

---

### Long Module
32-bit signed integer arithmetic and comparisons.

#### Arithmetic Operations
- **Add()** - Adds TOP to NEXT, result in NEXT (32-bit)
- **Sub()** - Subtracts TOP from NEXT, result in NEXT (32-bit)  
- **Mul()** - Multiplies NEXT by TOP, result in NEXT (32-bit)
- **Div()** - Divides NEXT by TOP, quotient in NEXT (32-bit)
- **Mod()** - Divides NEXT by TOP, remainder in NEXT (32-bit)

#### Comparison Operations
- **LT()** - Tests if NEXT < TOP, pushes bool result
- **GT()** - Tests if NEXT > TOP, pushes bool result
- **EQ()** - Tests if NEXT == TOP, pushes bool result
- **NE()** - Tests if NEXT != TOP, pushes bool result
- **LE()** - Tests if NEXT <= TOP, pushes bool result
- **GE()** - Tests if NEXT >= TOP, pushes bool result

#### Stack Operations
- **PushNext()** - Pushes NEXT onto value stack
- **PopTop()** - Pops top of stack into TOP
- **PopNext()** - Pops top of stack into NEXT

---

### Float Module
IEEE 754 single-precision floating-point arithmetic.

#### Public Methods
- **Add()** - Adds TOP to NEXT, result in NEXT (float)
- **Sub()** - Subtracts TOP from NEXT, result in NEXT (float)
- **Mul()** - Multiplies NEXT by TOP, result in NEXT (float)
- **Div()** - Divides NEXT by TOP, result in NEXT (float)
- **ToLong()** - Converts float in NEXT to 32-bit integer

#### Internal Format
- 32-bit IEEE 754 format
- Sign bit, 8-bit exponent, 23-bit mantissa
- Stored in NEXT3-0 or TOP3-0

---

### File Module
FAT-based filesystem for EEPROM storage with 256-byte sectors.

#### File Operations
- **Initialize()** - Formats filesystem, creates empty FAT and directory
  - Clears FAT and directory sectors
  - Marks system sectors as reserved
  
- **Exists()** - Checks if file exists
  - Input: STR = filename pointer
  - Output: C set if exists, NC if not found
  
- **Delete()** - Deletes file from filesystem
  - Input: STR = filename pointer
  - Frees all sectors in FAT chain
  - Compacts directory entries
  
- **Dir()** - Lists all files in directory
  - Prints file listing to serial
  - Shows file count and total bytes

#### Save Operations
- **StartSave()** - Opens file for writing
  - Input: STR = filename pointer
  - Allocates first sector, initializes state
  
- **AppendStream()** - Writes data to open file
  - Input: SectorSource = data pointer
  - Input: TransferLength = byte count
  - Allocates sectors as needed
  
- **EndSave()** - Closes file and updates metadata
  - Updates directory with final file length
  - Writes FAT and directory to EEPROM

#### Load Operations  
- **StartLoad()** - Opens file for reading
  - Input: STR = filename pointer
  - Initializes for NextStream() calls
  
- **NextStream()** - Reads next chunk from file
  - Output: Data in FileDataBuffer
  - Output: TransferLength = bytes read (max 256)
  - Returns NC at end of file

#### Simplified High-Level Operations
- **Save()** - Complete file save (combines Start/Append/End)
- **Load()** - Complete file load (combines Start/Next operations)

---

## Testing Project Plan

### Phase 1: Component Unit Tests

#### 1.1 Time Module Tests
**Goal:** Verify timer accuracy and delay functions
- Test Millis() counter increment
- Test Delay() with various durations (1ms, 10ms, 100ms, 1000ms)  
- Test Seconds() conversion accuracy
- Verify interrupt handling doesn't corrupt state

#### 1.2 Heap Module Tests  
**Goal:** Validate memory allocation and deallocation
- Test Initialize() RAM size detection
- Test Allocate() with various sizes
- Test Free() and verify coalescing
- Test Available() accuracy
- Stress test with fragmentation patterns
- Test edge cases (0 size, max size, out of memory)

#### 1.3 Long Module Tests
**Goal:** Verify 32-bit arithmetic correctness
- Test Add/Sub with positive, negative, overflow cases
- Test Mul with various operands including edge cases
- Test Div/Mod including division by zero handling
- Test all comparison operations (LT, GT, EQ, NE, LE, GE)
- Test sign handling across all operations

#### 1.4 Float Module Tests
**Goal:** Validate IEEE 754 operations
- Test Add/Sub with normalized and denormalized numbers
- Test Mul/Div with overflow/underflow cases
- Test ToLong() conversion with rounding
- Test special values (zero, infinity, NaN)
- Compare results with known IEEE 754 implementations

#### 1.5 File Module Tests
**Goal:** Verify filesystem integrity
- Test Initialize() creates valid FAT/directory
- Test Save/Load round-trip for various file sizes
- Test Delete and directory compaction
- Test filesystem full conditions
- Test filename validation (1-13 chars, alphanumeric)
- Test multi-file operations

---

### Phase 2: System API Definition

#### 2.1 Core System Services
Define standard interfaces for:
- **Memory Management** - Allocate, Free, Query
- **Process Control** - Load, Execute, Terminate
- **I/O Services** - Console, Serial, Storage
- **Timer Services** - Delays, Scheduling
- **Error Handling** - Standard error codes

#### 2.2 API Documentation
Create comprehensive documentation for:
- Function signatures and parameters
- Return values and error conditions
- Zero page usage and register preservation
- Example code snippets

---

### Phase 3: Command Console Implementation

#### 3.1 Basic Console Features
- **Command Parser** - Parse input, tokenize commands
- **Built-in Commands**:
  - `DIR` - List files
  - `LOAD filename` - Load and execute program
  - `DELETE filename` - Delete file
  - `MEM` - Show memory statistics
  - `TIME` - Show system uptime
  - `HELP` - List available commands

#### 3.2 Advanced Features
- Command history (up/down arrows)
- Tab completion for filenames
- Batch file execution
- Environment variables
- Error messages and return codes

---

## Testing Infrastructure

### Test Framework Components
1. **Test Runner** - Executes test suites, reports results
2. **Assertion Library** - Compare expected vs actual values
3. **Debug Output** - Hex dumps, register dumps, trace logs
4. **Coverage Tracking** - Track which code paths tested

### Test Output Format
```
[PASS] Time.Delay: 100ms delay accurate within 1%
[FAIL] Long.Div: Division by zero not handled
       Expected: Error code 0x01
       Actual: System hang
[PASS] Heap.Allocate: 256 byte allocation successful
```

---

## Development Timeline

### Week 1-2: Component Testing
- Implement test framework
- Write and run unit tests for all modules
- Fix discovered bugs

### Week 3: System API
- Define system call interface
- Document API specifications  
- Implement system call dispatcher

### Week 4-5: Console Implementation
- Build command parser
- Implement basic commands
- Add file operations

### Week 6: Integration & Polish
- System integration testing
- Performance optimization
- Documentation completion

---

## Success Criteria

1. **All unit tests pass** - 100% of defined tests
2. **Memory stability** - No leaks after 1000 allocate/free cycles
3. **File integrity** - 100 save/load cycles without corruption
4. **Console usability** - All commands work reliably
5. **Documentation** - Complete API reference available
6. **Performance** - Operations complete within defined time limits

---

## Notes

- All modules use Hopper Assembly language for 6502
- Zero page usage is carefully managed to avoid conflicts
- Register preservation rules must be strictly followed
- Error handling uses consistent error codes across modules
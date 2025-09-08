unit Definitions
{
    #define HOPPER_BIOS_APPLET
    
    // =====================================================
    // ZERO PAGE ALLOCATION MAP - HOPPER BIOS & APPLICATIONS
    // =====================================================
    //
    // SYSTEM CORE (0x00-0x29) - DO NOT USE IN APPLICATIONS
    // -------------------------------------------------------
    // 0x00-0x01: FLAGS, LastError
    // 0x02-0x05: Heap management (FREELIST, HEAPSTART, HEAPSIZE)
    // 0x06-0x09: Serial/I2C buffers
    // 0x0A-0x0D: File system (OutB, InB, LastAck, TEMP)
    // 0x0E-0x0F: DEBUG ONLY - STR2 (conditional compilation)
    // 0x10-0x1F: Core registers (ACC, TOP, NEXT, IDX, IDY, STR)
    // 0x20-0x23: System vectors (JumpTable, BIOSDISPATCH)
    // 0x24-0x27: Timer ticks (TICK0-3)
    // 0x28-0x29: Emulator support (EmulatorPCL/H)
    //
    // SHARED WORKSPACE (0x30-0x41) - LEAF FUNCTIONS ONLY
    // -------------------------------------------------------
    // 0x30-0x41: M0-M17 (18 bytes)
    // Used by (mutually exclusive):
    //   - Memory.Allocate/Free (M0-M17)
    //   - Time.Delay (M0-M3 as TARGET0-3)
    //   - Time.Seconds (M0-M7 as RESULT0-7)
    //   - Long math operations (M0-M7 as RESULT)
    //   - Debug functions (M0-M15 as DB0-DB15)
    //   - Float operations (M0-M13)
    //   - GapBuffer calculations (M0-M7 as temp workspace)
    //   - View rendering (M8-M15 - safe since doesn't call Memory/Time)
    //   - ScreenBuffer.Update (M0-M9 - leaf function)
    //   - Debug.DumpMemory (M0 as dumpCount)
    //
    // FILE SYSTEM WORKSPACE (0x42-0x57)
    // -------------------------------------------------------
    // 0x42-0x51: FS0-FS15 (File unit workspace)
    // 0x52-0x57: File operation parameters
    //
    
    
    
    // =====================================================
    // HARDWARE I/O (0xEC-0xFF) - PLATFORM DEPENDENT
    // -------------------------------------------------------
    // 0xEC-0xED: ACIA (status/control, data)
    // 0xEE-0xEF: Reserved for hardware expansion
    // 0xF0-0xFF: VIA 65C22 registers
    //   - PORTB/A, DDRB/A, timers, shift register
    //   - ACR, PCR, IFR, IER, ORA
    
    
    // =====================================================
    // APPLICATION ZERO PAGE (0x58-0xEB)
    // Available for application use - 148 bytes total
    // =====================================================
    //
    // APPLICATION MODULE ALLOCATIONS (16-byte aligned):
    // -------------------------------------------------------
    // 0x58-0x5F: Reserved (8 bytes)
    //   - Available for small system extensions
    //
    // 0x60-0x6F: Debug & Screen (shared 16-byte block)
    //   0x60-0x64: Debug (5 bytes)
    //     - debugRow, debugEnabled, debugEntries
    //   0x65-0x66: Screen (2 bytes)
    //     - screenStrL, screenStrH (temporary string pointer)
    //   [0x67-0x6F available for Debug/Screen expansion]
    //
    // 0x70-0x7A: ScreenBuffer (11 bytes)
    //   - CursorCol, CursorRow, Foreground, Background
    //   - Attributes, sbWidth, sbHeight
    //   - sbBuffer (2 bytes), sbSuspendCount, sbCursorVisible
    //   - [0x7B-0x7F available for ScreenBuffer expansion]
    //
    // 0x80: Keyboard (1 byte)
    //   - kbEscState
    //   - [0x81-0x8F available for Keyboard expansion]
    //
    // 0x90-0x9E: View (15 bytes)
    //   - vwLogicalCursor (2 bytes), vwTopLine (2 bytes)
    //   - vwCurrentLine (2 bytes), vwCurrentCol
    //   - vwScreenRows, vwScreenCols, vwModified, vwDirty
    //   - vwLineStarts (2 bytes), vwLineCount (2 bytes)
    //   - [0x9F available for View expansion]
    //
    // 0xA0-0xA7: GapBuffer (8 bytes)
    //   - gbBuffer (2 bytes), gbGapStart (2 bytes)
    //   - gbGapEnd (2 bytes), gbBufferSize (2 bytes)
    //   - [0xA8-0xAF available for GapBuffer expansion]
    //
    // 0xB0-0xB6: Commands (7 bytes)
    //   - cmdExitFlag, cmdSaveNeeded
    //   - cmdFileSize (2 bytes), cmdReadPos (2 bytes)
    //   - cmdChar
    //   - [0xB7-0xBF available for Commands expansion]
    //
    // 0xC0-0xEB: Available (44 bytes)
    //   - Free for additional application modules
    //
    // =====================================================
    
    
    
    
    
    uses "../../Kernel/Definitions/BIOSInterface"
    uses "../../Kernel/Definitions/ZeroPage"
    uses "../../Kernel/Definitions/Limits"
    uses "../../Kernel/Definitions/MemoryMap"
}

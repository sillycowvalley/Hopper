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
    // APPLICATION MODULE ALLOCATIONS:
    // -------------------------------------------------------
    // 0x58-0x5F: Available (8 bytes)
    //   - Free for application use
    //
    // 0x60-0x66: Debug & Screen (7 bytes used)
    //   0x60-0x64: Debug (5 bytes)
    //     - debugRow, debugEnabled, debugEntries
    //     - debugByte, debugNibble
    //   0x65-0x66: Screen (2 bytes)
    //     - screenStrL, screenStrH (temporary string pointer)
    // 0x67-0x6F: Available (9 bytes)
    //   - Free for application expansion
    //
    // 0x70-0x7A: ScreenBuffer (11 bytes)
    //   - CursorCol, CursorRow, Foreground, Background
    //   - Attributes, sbWidth, sbHeight
    //   - sbBuffer (2 bytes), sbSuspendCount, sbCursorVisible
    // 0x7B-0x7E: Available (4 bytes)
    //   - Free for application use
    //
    // 0x7F: Keyboard (1 byte)
    //   - kbEscState (escape sequence state machine)
    //
    // 0x80-0x8B: GapBuffer (12 bytes)
    //   - gbBuffer (2 bytes), gbGapStart (2 bytes)
    //   - gbGapEnd (2 bytes), gbBufferSize (2 bytes)
    //   - GapValue (2 bytes), gbTempSize (2 bytes)
    // 0x8C-0x8F: Available (4 bytes)
    //   - Free for GapBuffer expansion
    //
    // 0x90-0x9A: View (11 bytes)
    //   - vwScreenCols, vwScreenRows, vwCurrentRow
    //   - vwTopLine (2 bytes), vwLineCount (2 bytes)
    //   - vwTemp (2 bytes), vwPos (2 bytes)
    //
    // 0x9B-0xEB: Available (81 bytes)
    //   - Free for additional application modules
    //
    // =====================================================
    // SUMMARY:
    // Used: 42 bytes (Debug:5, Screen:2, ScreenBuffer:11,
    //                 Keyboard:1, GapBuffer:12, View:11)
    // Available: 106 bytes (in various blocks)
    // =====================================================
    
    
    uses "../../Kernel/Definitions/BIOSInterface"
    uses "../../Kernel/Definitions/ZeroPage"
    uses "../../Kernel/Definitions/Limits"
    uses "../../Kernel/Definitions/MemoryMap"
}

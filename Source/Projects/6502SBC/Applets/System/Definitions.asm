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
    //
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
    // 0x60-0x65: Debug (6 bytes)
    //   - debugRow, debugCol (0x60-0x61)
    //   - debugEnabled, debugEntries (0x62-0x63)
    //   - dumpCount (0x64-0x65, uint)
    //
    // 0x66: Available (1 byte)
    //   - Free for application use
    //
    // 0x67-0x6C: Screen (6 bytes)
    //   - screenStrL, screenStrH (0x67-0x68, temporary string pointer)
    //   - currentAttributes, workingAttributes (0x69-0x6A)
    //   - workingStatus, workingColour (0x6B-0x6C)
    //
    // 0x6D-0x6F: Available (3 bytes)
    //   - Free for application expansion
    //
    // 0x70-0x7E: ScreenBuffer (15 bytes)
    //   - CursorCol, CursorRow, Foreground, Background (0x70-0x73)
    //   - Attributes, sbWidth, sbHeight (0x74-0x76)
    //   - sbBuffer (0x77-0x78, uint)
    //   - sbSuspendCount, sbCursorVisible (0x79-0x7A)
    //   - sbLastOffsetCol, sbLastOffsetRow (0x7B-0x7C)
    //   - sbOffsetL, sbOffsetH (0x7D-0x7E)
    //
    // 0x7F: Keyboard (1 byte)
    //   - kbEscState (escape sequence state machine)
    //
    // 0x80-0x8F: GapBuffer (16 bytes)
    //   - gbBuffer (0x80-0x81, uint), gbGapStart (0x82-0x83, uint)
    //   - gbGapEnd (0x84-0x85, uint), gbBufferSize (0x86-0x87, uint)
    //   - GapValue (0x88-0x89, uint), gbTempSize (0x8A-0x8B, uint)
    //   - gbGapSizeL/H (0x8C-0x8D), FastLengthL/H (0x8E-0x8F)
    //
    // 0x90-0x9F: View (16 bytes)
    //   - vwScreenCols, vwScreenRows, vwCurrentRow, vwCurrentCol (0x90-0x93)
    //   - vwTopLine (0x94-0x95, uint) [Note: was incorrectly shown as 0x95-0x96]
    //   - vwTopLineL, vwTopLineH (0x95-0x96)
    //   - vwLineCount (0x97-0x98, uint) [Note: was incorrectly shown as 0x98-0x99]
    //   - vwLineCountL, vwLineCountH (0x98-0x99)
    //   - vwPosL/H (0x9A-0x9B), vwLeafTempL/H (0x9C-0x9D)
    //   - vwSkipCountL/H (0x9E-0x9F)
    //
    // 0xA0-0xAF: Available (16 bytes)
    //   - Free for application use
    //
    // 0xB0-0xB5: Prompt (6 bytes)
    //   - promptBuffer (0xB0-0xB1, uint)
    //   - promptLength (0xB2)
    //   - promptMaxLen (0xB3)
    //   - promptStartCol (0xB4)
    //   - promptLastChar (0xB5)
    //
    // 0xB6-0xCA: Edit (21 bytes, +4 in DEBUG mode)
    //   - EditorFlags (0xB6)
    //   - currentFilename (0xB7-0xB8, uint)
    //   - BlockStart (0xB9-0xBA, uint)
    //   - BlockEnd (0xBB-0xBC, uint)
    //   - clipBoard (0xBD-0xBE, uint)
    //   - currentPos (0xBF-0xC0, uint)
    //   - targetPos (0xC1-0xC2, uint)
    //   - editCount (0xC3-0xC4, uint)
    //   - editStore (0xC5-0xC6, uint)
    //   - DEBUG: crPos (0xC7-0xC8, uint), crCol (0xC9) [conditional]
    //
    // 0xCB-0xEB: Available (33 bytes, 29 if Edit DEBUG mode)
    //   - Free for additional application modules
    //
    // =====================================================
    // SUMMARY:
    // Used: 115 bytes (Debug:6, Screen:6, ScreenBuffer:15,
    //                  Keyboard:1, GapBuffer:16, View:16, 
    //                  Prompt:6, Edit:21 [+4 in DEBUG])
    // Available: 33 bytes (in various blocks)
    // =====================================================
    
    
    uses "../../Kernel/Definitions/BIOSInterface"
    uses "../../Kernel/Definitions/ZeroPage"
    uses "../../Kernel/Definitions/Limits"
    uses "../../Kernel/Definitions/MemoryMap"
}

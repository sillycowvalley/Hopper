unit Tools
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // No accidental side effects or register corruption
 
    // Print BasicType enum value as readable string
    // Input: A = BasicType enum value
    // Output: Type name printed to serial
    // Preserves: Everything
    PrintType()
    
    // Print null-terminated string to serial output
    // Input: ZP.ACC = pointer to null-terminated string
    // Output: String printed to serial
    PrintStringACC()

    // Write '\n' preserving carry flag
    // Output: '\n' printed to serial
    // Preserves: Everything
    NL()
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOP = 16-bit number to print (0-65535)
    //        ZP.TOPT = type (for signed/unsigned determination)
    // Output: Decimal number printed to serial
    PrintDecimalWord()
    
    // Powers of 10 table for PrintDecimalWord (little-endian format)
    const byte[] PrDec16Tens = { 
        0x01, 0x00,  // 1 (little-endian)
        0x0A, 0x00,  // 10
        0x64, 0x00,  // 100  
        0xE8, 0x03,  // 1000
        0x10, 0x27   // 10000
    };
    
    // Copy bytes from source to destination
    // Input: ZP.FSOURCEADDRESS = source pointer
    //        ZP.FDESTINATIONADDRESS = destination pointer  
    //        ZP.FLENGTH = number of bytes to copy (16-bit)
    // Output: Data copied from source to destination
    CopyBytes()
    
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    StringLength()
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    StringCompare()

#ifdef DEBUG    
    
    // Dump key zero page variables for debugging
    // Input: None
    // Output: Variables printed to serial
    // Preserves: Everything
    DumpVariables()

    // Dump the value stack for debugging
    // Input: None
    // Output: Stack contents printed to serial
    // Preserves: Everything
    DumpStack()
    
    // Lightweight heap summary for use during iteration
    // Input: None
    // Output: List head pointers printed to serial
    // Preserves: Everything
    DumpHeapSummary()

    // Debug output for iteration state
    // Input: None
    // Output: Current IDX pointer printed to serial
    // Preserves: Everything
    DumpIterationState()
    
    // Dump heap with state preservation for debugging
    // Input: None
    // Output: Heap contents printed to serial
    // Modifies: ZP.M* scratch space (internal to heap analysis operations)
    DumpHeap()

    // Dump a 256-byte page in hex+ASCII format for debugging
    // Input: A = page number (high byte of address)
    // Output: Page contents printed to serial
    // Modifies: ZP.M0, ZP.M1 (internal operations)
    DumpPage()

    // Dump the BASIC input and tokenizer buffers for debugging
    // Input: None
    // Output: Buffer contents printed to serial
    DumpBasicBuffers()
    
    // Output hex byte preserving carry flag  
    // Input: A = byte to output as hex
    // Output: Hex byte printed to serial
    // Preserves: Everything
    HOut()
    
    // Output IDX register as "IDX:hhll "
    // Input: None (uses ZP.IDX)
    // Output: IDX value printed to serial
    // Preserves: Everything
    XOut()
    
    // Output IDY register as "IDY:hhll "
    // Input: None (uses ZP.IDY)
    // Output: IDY value printed to serial
    // Preserves: Everything
    YOut()
    
    // Output ZP.SymbolIteratorFilter as "I:ll "
    // Input: None (uses ZP.SymbolIteratorFilter)
    // Output: ZP.SymbolIteratorFilter value printed to serial
    // Preserves: Everything
    IOut()
    
    // Output ACC register as "ACC:hhll "
    // Input: None (uses ZP.ACC)
    // Output: ACC value printed to serial
    // Preserves: Everything
    AOut()
    
    // Output ACCT register as "ACCT:ll "
    // Input: None (uses ZP.ACCT)
    // Output: ACCT value printed to serial
    // Preserves: Everything
    ATOut()
    
    // Output NEXT register as "NEXT:type-hhll "
    // Input: None (uses ZP.NEXT)
    // Output: NEXT value printed to serial
    // Preserves: Everything
    NOut()
    
    // Output TOP register as "TOP:type-hhll "
    // Input: None (uses ZP.TOP)
    // Output: TOP value printed to serial
    // Preserves: Everything
    TOut()
    
    // Output ACCL register as "ACCL:ll "
    // Input: None (uses ZP.ACCL)
    // Output: ACCL value printed to serial
    // Preserves: Everything
    ALOut()

#endif

    // Write character preserving carry flag
    // Input: A = character to output
    // Output: Character printed to serial
    // Preserves: Everything
    COut()
}
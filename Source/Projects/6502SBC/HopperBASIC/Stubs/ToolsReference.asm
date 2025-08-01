unit Tools
{
    // API Reference - Complete Implementations
    // 
    // All methods listed in this file are fully implemented and thoroughly tested.
    // The actual source code has been omitted from this reference to reduce the 
    // volume of code in the project knowledge base while maintaining API visibility.
    //
    // These are working implementations, not placeholder stubs - the term "stubs" 
    // refers only to the abbreviated documentation format, not the implementation status.
    //
    // For debugging or modification needs, refer to the actual source files which
    // contain the complete, tested implementations with full error handling and
    // proper register preservation as documented in the API contracts below.

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
    // Preserves: Everything
    PrintStringACC()
    
    // Print null-terminated string to serial output
    // Input: ZP.TOP = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringTOP()
    
    // Print null-terminated string to serial output
    // Input: ZP.IDY = pointer to null-terminated string
    // Output: String printed to serial
    // Preserves: Everything
    PrintStringIDY()

    // Debug utility: Write '\n' preserving carry flag
    // Output: '\n' printed to serial
    // Preserves: Everything
    NL()
    
    // Print variable value with proper type formatting
    // Input: ZP.TOP = value, ZP.TOPT = type
    // Output: Value printed to serial (TRUE/FALSE for BIT, numeric for others)
    // Preserves: Everything
    PrintVariableValue()
    
    // Print 16-bit decimal number with no leading zeros
    // Input: ZP.TOP = 16-bit number to print (0-65535)
    //        ZP.TOPT = type (for signed/unsigned determination)
    // Output: Decimal number printed to serial
    // Preserves: Everything
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
    // Preserves: Everything
    CopyBytes()
    
    // Get string length
    // Input: X = string pointer low byte, Y = string pointer high byte
    // Output: A = string length (not including null terminator)
    // Preserves: Everything
    StringLength()
    
    // Compare two strings
    // Input: ZP.TOP = first string pointer, ZP.NEXT = second string pointer
    // Output: C set if strings match, NC if different
    // Preserves: Everything
    StringCompare()

    // Debug dump: key zero page variables for debugging
    // Input: None
    // Output: Variables printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    DumpVariables()

    // Debug dump: value stack contents for debugging
    // Input: None
    // Output: Stack contents printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    DumpStack()
    
    // Debug dump: lightweight heap summary for use during iteration
    // Input: None
    // Output: List head pointers printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    DumpHeapSummary()

    // Debug dump: iteration state for debugging
    // Input: None
    // Output: Current IDX pointer printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    DumpIterationState()
    
    // Debug dump: heap with state preservation for debugging
    // Input: None
    // Output: Heap contents printed to serial
    // Modifies: ZP.M* scratch space (internal to heap analysis operations)
    // Preserves: Everything else (saves/restores flags with PHP/PLP)
    DumpHeap()

    // Debug dump: 256-byte page in hex+ASCII format for debugging
    // Input: A = page number (high byte of address)
    // Output: Page contents printed to serial
    // Modifies: ZP.M0, ZP.M1 (internal operations)
    // Preserves: Everything else (saves/restores flags with PHP/PLP)
    DumpPage()

    // Debug dump: BASIC input and tokenizer buffers for debugging
    // Input: None
    // Output: Buffer contents printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    DumpBasicBuffers()
    
    // Debug output: hex byte preserving carry flag  
    // Input: A = byte to output as hex
    // Output: Hex byte printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    HOut()
    
    // Debug output: IDX register as "IDX:hhll "
    // Input: None (uses ZP.IDX)
    // Output: IDX value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    XOut()
    
    // Debug output: IDY register as "IDY:hhll "
    // Input: None (uses ZP.IDY)
    // Output: IDY value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    YOut()
    
    // Debug output: ZP.SymbolIteratorFilter as "I:ll "
    // Input: None (uses ZP.SymbolIteratorFilter)
    // Output: ZP.SymbolIteratorFilter value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    IOut()
    
    // Debug output: ACC register as "ACC:hhll "
    // Input: None (uses ZP.ACC)
    // Output: ACC value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    AOut()
    
    // Debug output: ACCT register as "ACCT:ll "
    // Input: None (uses ZP.ACCT)
    // Output: ACCT value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    ATOut()
    
    // Debug output: NEXT register as "NEXT:type-hhll "
    // Input: None (uses ZP.NEXT)
    // Output: NEXT value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    NOut()
    
    // Debug output: TOP register as "TOP:type-hhll "
    // Input: None (uses ZP.TOP)
    // Output: TOP value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    TOut()
    
    // Debug output: ACCL register as "ACCL:ll "
    // Input: None (uses ZP.ACCL)
    // Output: ACCL value printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    ALOut()

    // Debug utility: Write character preserving carry flag
    // Input: A = character to output
    // Output: Character printed to serial
    // Preserves: Everything (saves/restores flags with PHP/PLP)
    COut()
}
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

unit ComparisonInstructions
{
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // Comparison operations pop two operands and push BIT result
    // Uses ZP.ACC for internal comparison calculations
    // Error handling via ZP.LastError for type mismatches
    
    // Equality comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    Equal()
    
    // Not-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    NotEqual()
    
    // Less-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left < right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LessThan()
    
    // Greater-than comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left > right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    GreaterThan()
    
    // Less-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left <= right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    LessEqual()
    
    // Greater-than-or-equal comparison operation (pops two operands, pushes BIT result)
    // Input: Stack contains two operands (right operand on top)
    // Output: BIT value (0 or 1) pushed to stack representing (left >= right)
    // Modifies: Stack interface (ZP.TOP, ZP.NEXT, ZP.TOPT, ZP.NEXTT, ZP.SP, stack memory)
    //          Error state (ZP.LastError if type mismatch occurs)
    GreaterEqual()
}
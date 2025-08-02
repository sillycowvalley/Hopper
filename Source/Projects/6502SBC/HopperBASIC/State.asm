unit State
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Error"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // System state management for robust orchestration method coordination
    
    flags SystemState 
    {
        Failure = 0,        // Zero for easy testing (CMP -> Z flag)
        Success = 1,        // Normal completion
        Exiting = 2,        // User exit request (BYE, Ctrl+C)
        Return  = 3         // exit from a compiled function
    }
    
    // Set system state
    // Input: A = SystemState value
    // Output: ZP.SystemState updated, A preserved
    // Modifies: ZP.SystemState only
    SetState()
    {
        STA ZP.SystemState
    }
    
    // Get current system state  
    // Output: A = current SystemState
    // Modifies: A only
    GetState()
    {
        LDA ZP.SystemState
    }
    
    // Output: C implies failure (as in, yes condition = C no condition is NC)
    // Preserves: A register, modifies flags only
    IsFailure()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Output: C implies success (as in, yes condition = C no condition is NC)
    // Preserves: A register, modifies flags only
    IsSuccess()
    {
        PHA  
        LDA ZP.SystemState
        CMP #SystemState.Success
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Output: C implies success (as in, yes condition = C, no condition is NC)
    // Preserves: A register, modifies flags only
    IsExiting()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Exiting
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Output: C implies success (as in, yes condition = C, no condition is NC)
    // Preserves: A register, modifies flags only
    IsReturn()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Return
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Check if current state indicates continuation is possible
    // Output: C set if can continue (Success), NC if should stop (Failure or Exiting)
    // Preserves: A register
    CanContinue()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Success
        if (Z)
        {
            SEC  // Success = can continue
        }
        else
        {
            CLC  // Failure or Exiting = cannot continue
        }
        PLA
    }
    
    // Convenience methods for common state changes
    // Input: None
    // Output: ZP.SystemState updated
    // Preserves: A register
    // Modifies: ZP.SystemState only
    SetSuccess()
    {
        PHA
        LDA #SystemState.Success
        STA ZP.SystemState
        PLA
    }
    
    SetFailure()
    {
        PHA
        LDA #SystemState.Failure  
        STA ZP.SystemState
        PLA
    }
    
    SetExiting()
    {
        PHA
        LDA #SystemState.Exiting
        STA ZP.SystemState
        PLA
    }
    SetReturn()
    {
        PHA
        LDA #SystemState.Return
        STA ZP.SystemState
        PLA
    }
    
    
    
    // Check if current state indicates an error condition
    // Output: C set if error (Failure), NC if not error (Success or Exiting)
    // Preserves: A register
    IsError()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z)
        {
            SEC  // Failure = is error = C
        }
        else
        {
            CLC // no error = NC
        }
        PLA
    }
}
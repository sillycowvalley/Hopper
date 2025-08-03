unit State
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Error"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // System state management for robust orchestration method coordination

    // State display strings
    const string stateSuccess = "SUCCESS";
    const string stateFailure = "FAILURE"; 
    const string stateExiting = "EXITING";
    const string stateReturn = "RETURN";
    const string stateUnknown = "UNKNOWN";

    
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
    
    // Output: C implies failure (as in, yes condition = C, no condition is NC)
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
    
    // Check if current state indicates continuation is possible
    // Output: C set if can continue (Success|Exiting|Return), NC if should stop (Failure)
    // Preserves: A register
    CanContinue()
    {
        PHA
        LDA ZP.SystemState
        CMP #SystemState.Failure
        if (Z)
        {
            CLC  // Failure
        }
        else
        {
            SEC  // Success, Exiting or Return -> can continue
        }
        PLA
    }
    
    // Output: C implies Exiting (as in, yes condition = C, no condition is NC)
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
    
    // Output: C implies Success (as in, yes condition = C, no condition is NC)
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
    
    // Output: C implies Return (as in, yes condition = C, no condition is NC)
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
    

    // Print current system state as human-readable string
    // Input: None
    // Output: Current SystemState printed to serial
    // Preserves: A, X, Y registers
    PrintState()
    {
        PHP
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        LDA ZP.SystemState
        switch (A)
        {
            case SystemState.Success:
            {
                LDA #(stateSuccess % 256)
                STA ZP.ACCL
                LDA #(stateSuccess / 256)
                STA ZP.ACCH
            }
            case SystemState.Failure:
            {
                LDA #(stateFailure % 256)
                STA ZP.ACCL
                LDA #(stateFailure / 256)
                STA ZP.ACCH
            }
            case SystemState.Exiting:
            {
                LDA #(stateExiting % 256)
                STA ZP.ACCL
                LDA #(stateExiting / 256)
                STA ZP.ACCH
            }
            case SystemState.Return:
            {
                LDA #(stateReturn % 256)
                STA ZP.ACCL
                LDA #(stateReturn / 256)
                STA ZP.ACCH
            }
            default:
            {
                LDA #(stateUnknown % 256)
                STA ZP.ACCL
                LDA #(stateUnknown / 256)
                STA ZP.ACCH
            }
        }
        
        Tools.PrintStringACC();
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
        PLP
    }
    
}

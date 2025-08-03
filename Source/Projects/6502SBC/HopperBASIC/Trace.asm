unit Trace
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Tools"
    uses "Debug"
    

#if defined(DEBUG) || defined(TRACE)
    // Shared utilities (needed by Debug unit too)
    // Print current indentation level using spaces
    // Input: ZP.TraceIndent = current depth
    // Output: Appropriate number of spaces printed
    // Preserves: All registers and flags
    PrintIndent()
    {
        PHA
        PHX
        
        LDX ZP.TraceIndent
        loop
        {
            CPX #0
            if (Z) { break; }
            
            Debug.Space(); Debug.Space();
            
            DEX
        }
        
        PLX
        PLA
    }
#endif

#ifdef TRACE
    const string convergenceMarker = " <- CONVERGENCE";
    const string endBrace = "} // ";

    // Initialize trace system
    // Input: None
    // Output: ZP.TraceIndent initialized to 0
    // Preserves: Everything
    Initialize()
    {
        STZ ZP.TraceIndent
    }

    // Method entry tracing
    // Input: ZP.TraceMessageL/H = method name string pointer
    // Output: Method name + '{' printed with indentation, indent increased
    // Preserves: All registers and flags
    MethodEntry()
    {
        PHP
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // capture current stack pointer
        //TSX
        //PHX
        
        Debug.ValidateHeap();
        
        PrintIndent();
        
        // Print method name from ZP.TraceMessage
        LDA ZP.TraceMessageL STA ZP.ACCL LDA ZP.TraceMessageH STA ZP.ACCH Tools.PrintStringACC(); Debug.Space(); LDA #'{' Debug.COut(); 
        
        /*
        LDA #' ' Debug.COut();
        LDA #'S' Debug.COut(); 
        PLA Debug.HOut();  // Print current stack pointer
        LDA #' ' Debug.COut();
        */
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
        if (NZ)
        {
            LDA #'!' Debug.COut(); 
            PLP
            if (NC)
            {
                LDA #'N' Debug.COut(); LDA #'C' Debug.COut(); 
                CLC
            }
            PHP

            Error.IsFatal();
            if (C)
            {
                LDA # 0x06 Debug.Crash(); // fail on CheckError in MethodEntry()
            }
        }
        
        Debug.NL();
        
        // Increase indentation
        INC ZP.TraceIndent
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
        PLP
    }
    
    // Method exit tracing  
    // Input: ZP.TraceMessageL/H = method name string pointer
    // Output: Indent decreased, method name + '}' printed with indentation
    // Preserves: All registers and flags
    MethodExit()
    {
        PHP
        PHA
        PHX
        PHY
        
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // capture current stack pointer
        //TSX
        //PHX
        
        Debug.ValidateHeap();
        
        // Decrease indentation first
        DEC ZP.TraceIndent
        
        PrintIndent();
        
        LDA #(endBrace % 256) STA ZP.ACCL LDA #(endBrace / 256) STA ZP.ACCH Tools.PrintStringACC(); // ' } // '
        
        // Print method name from ZP.TraceMessage
        LDA ZP.TraceMessageL STA ZP.ACCL LDA ZP.TraceMessageH STA ZP.ACCH Tools.PrintStringACC(); Debug.Space(); 
        
        /*
        LDA #' ' Debug.COut();
        LDA #'S' Debug.COut();
        PLA Debug.HOut();  // Print current stack pointer
        LDA #' ' Debug.COut();
        */
        
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
        if (NZ)
        {
            LDA #'!' Debug.COut(); 
            PLP
            if (NC)
            {
                LDA #'N' Debug.COut(); LDA #'C' Debug.COut(); 
                CLC
            }
            PHP
            
            Error.IsFatal();
            if (C)
            {
                LDA # 0x05 Debug.Crash(); // fail on CheckError in MethodExit()
            }
        }
        
        Debug.NL();
        
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
        PLP
    }
    
    // Mark convergence points where different execution paths merge
    // Input: None
    // Output: " <- CONVERGENCE" marker printed at current indent level
    // Preserves: All registers and flags
    MarkConvergence()
    {
        PHA
        
        LDA #(convergenceMarker % 256) STA ZP.ACCL LDA #(convergenceMarker / 256) STA ZP.ACCH Tools.PrintStringACC();
        
        PLA
    }
#else
    // No-op initialization for production builds
    Initialize() { }
#endif
}

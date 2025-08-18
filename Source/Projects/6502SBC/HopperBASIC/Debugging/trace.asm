unit Trace // Trace.asm
{
    // #define TRACESP (6502 SP for stack balance diagnosis)
    
    IsTracing()
    {
#if defined(TRACE) || defined(TRACEEXE)
        if (BBS2, ZP.FLAGS)
        {
            SEC  // Bit 2 is set - tracing enabled
        }
        else
        {
            CLC  // Bit 2 is clear - tracing disabled
        }
#else
        CLC
#endif
    }

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
        IsTracing();
        if (NC)
        {
            PLP
            return;
        }
        PHA
        PHX
        PHY
        
#ifdef TRACESP        
        // capture current stack pointer
        TSX
        PHX
#endif
        
        PrintIndent();
        
        // Print method name from ZP.TraceMessage
        LDA ZP.TraceMessageL STA ZP.STRL LDA ZP.TraceMessageH STA ZP.STRH Tools.PrintStringSTR(); Debug.Space(); LDA #'{' Debug.COut(); 
        
        Debug.ValidateHeap();
      
#ifdef TRACESP  
        LDA #' ' Debug.COut();
        LDA #'S' Debug.COut(); 
        PLA Debug.HOut();  // Print current stack pointer
        LDA #' ' Debug.COut();
#endif
        
        States.IsSuccess();
        if (NC)
        {
            LDA #' ' Debug.COut(); States.PrintState();
        }
        
        LDA ZP.LastError
        if (NZ)
        {
            LDA #' ' Debug.COut(); 
            LDA #'(' Debug.COut(); 
            LDA ZP.LastErrorL Error.PrintError();
            LDA #')' Debug.COut(); LDA #' ' Debug.COut(); 
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
                LDA # 0x05 Debug.Crash(); // fail on CheckError in MethodEntry()
            }
        }
        
        Debug.NL();
        
        // Increase indentation
        INC ZP.TraceIndent
        
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
        IsTracing();
        if (NC)
        {
            PLP
            return;
        }
        
        PHA
        PHX
        PHY
        
#ifdef TRACESP
        // capture current stack pointer
        TSX
        PHX
#endif
        
        // Decrease indentation first
        DEC ZP.TraceIndent

        Debug.ValidateHeap();

        PrintIndent();
        
        LDA #(endBrace % 256) STA ZP.STRL LDA #(endBrace / 256) STA ZP.STRH Tools.PrintStringSTR(); // ' } // '
        
        // Print method name from ZP.TraceMessage
        LDA ZP.TraceMessageL STA ZP.STRL LDA ZP.TraceMessageH STA ZP.STRH Tools.PrintStringSTR(); Debug.Space(); 
        
#ifdef TRACESP
        LDA #' ' Debug.COut();
        LDA #'S' Debug.COut();
        PLA Debug.HOut();  // Print current stack pointer
        LDA #' ' Debug.COut();
#endif

        States.IsSuccess();
        if (NC)
        {
            LDA #' ' Debug.COut(); States.PrintState();
        }
        
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
        if (NZ)
        {
            LDA #' ' Debug.COut(); 
            LDA #'(' Debug.COut(); 
            LDA ZP.LastErrorL STA ZP.STRL LDA ZP.LastErrorH STA ZP.STRH Tools.PrintStringSTR();
            LDA #')' Debug.COut(); LDA #' ' Debug.COut(); 
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
                LDA # 0x06 Debug.Crash(); // fail on CheckError in MethodExit()
            }
        }
        
        
        
        Debug.NL();
        
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
        
        LDA #(convergenceMarker % 256) STA ZP.STRL LDA #(convergenceMarker / 256) STA ZP.STRH Tools.PrintStringSTR();
        
        PLA
    }
#else
    // No-op initialization for production builds
    Initialize() { }
#endif
}

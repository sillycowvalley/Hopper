program CC
{
    #define CPU_65C02S
    //#define DEBUG
    #define PEEPHOLE
    
    uses "../System/Definitions"
    uses "../System/Args"
    uses "../System/Shared"
    uses "../System/Print"
    uses "../System/Long"
    uses "../System/File"
    
    uses "Errors"
    uses "Tokens"
    uses "Lexer"
    uses "AST"
    uses "Parser"
    
    uses "VCode"
    uses "Gen6502"
    uses "Library"
    uses "CodeGen"
    
    
    
    const string messageCompiling = "Compiling ";
    
//#if defined(DEBUG)    
    const string msgStart = "Memory: ";
    const string msgCompiled = "Memory after compile: ";
    const string msgCleaned = "Memory after cleanup: ";
//#endif    
    
    const byte ccSlots = 0x60;
    const byte sourceName  = ccSlots+0;
    const byte sourceNameL = ccSlots+0;
    const byte sourceNameH = ccSlots+1;
    const uint outputName  = ccSlots+2;
    const byte outputNameL = ccSlots+2;
    const byte outputNameH = ccSlots+3;
    
    // Create output filename: "HELLO" -> "HELLOX"
    makeOutputName()
    {
        // Allocate buffer for output name (14 bytes max + ".EXE" + null)
        LDA #20
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        if (NC)
        {
            Errors.OutOfMemory();
            return;
        }
        
        LDA ZP.IDXL
        STA outputNameL
        LDA ZP.IDXH
        STA outputNameH
        
        // Copy source name
        LDY #0
        loop
        {
            LDA [sourceName], Y
            if (Z) { break; }  // Found null
            CMP #'.'
            if (Z) { break; }  // Found "." (like ".C")
            
            STA [ZP.IDX], Y
            INY
            CPY #13  // Max filename length
            if (Z)
            {
                LDA # Error.FilenameTooLong
                Errors.Show();
                CLC
                return;
            }
        }
        
        // Append ".EXE"
        LDA #'.'
        STA [ZP.IDX], Y
        INY
        LDA #'E'
        STA [ZP.IDX], Y
        INY
        LDA #'X'
        STA [ZP.IDX], Y
        INY
        LDA #'E'
        STA [ZP.IDX], Y
        INY
        
        // Null terminate
        LDA #0
        STA [ZP.IDX], Y
        
        LDA outputNameL
        STA STRL
        LDA outputNameH
        STA STRH
        
        SEC
    }
    
    // Get filename argument from command line
    // Input:  None (reads from command line buffer at Address.LineBuffer)
    // Output: C set if filename found, clear if none
    //         ZP.STR = pointer to filename (null-terminated, uppercase)
    //         A = filename length
    // Note:   Returns first argument after program name
    //         BIOS has already uppercased the input
    GetFilename()
    {
        PHY
        PHX
        
        Args.GetArgument(); // filename length -> A, X != 0 means "." seen
        
        // append ".C" if there is no "."
        CPX #0
        if (Z)
        {
            // getArgument gives us a STR that is pointing into Address.LineBuffer so we can extend the filename safely
            LDY #0
            loop
            {
                LDA [ZP.STR], Y
                if (Z) { break; } 
                INY
            }
            LDA #'.'
            STA [ZP.STR], Y
            INY
            LDA #'C'
            STA [ZP.STR], Y
            INY
            LDA #0
            STA [ZP.STR], Y
            TYA // length -> A
        }
        PLX
        PLY
    }

    
    Hopper()
    {
//#if defined(DEBUG) 
        LDA #(msgStart % 256)
        STA ZP.STRL
        LDA #(msgStart / 256)
        STA ZP.STRH
        Print.String();
        Memory.Available();  // Returns in ZP.ACC
        Shared.MoveAccToTop();
        Long.Print();
        Print.NewLine();
//#endif        
        Args.HasFilename();
        if (NC)
        {
            LDA # Error.SourceNotFound
            Errors.Show();
            return;
        }
        GetFilename(); // appends .C if missing
        LDA ZP.STRL
        STA sourceNameL
        LDA ZP.STRH
        STA sourceNameH
        
        LDA # FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA # Error.SourceLoadingError
            Errors.Show();
            return;     
        }
        
        LDA # (messageCompiling % 256)
        STA ZP.STRL
        LDA # (messageCompiling / 256)
        STA ZP.STRH
        Print.String();
        
        LDA sourceNameL
        STA ZP.STRL
        LDA sourceNameH
        STA ZP.STRH
        Print.String();
        Print.NewLine();
        
        STZ outputNameL
        STZ outputNameH
        
        AST.Initialize();
        if (NC)
        {
            Errors.OutOfMemory();
            return;
        }

        Lexer.Initialize();
        if (NC)
        {
            AST.Dispose();
            Errors.OutOfMemory();
            return;
        }
        CodeGen.Initialize();
        if (NC)
        {
            AST.Dispose();
            Lexer.Dispose();
            Errors.OutOfMemory();
            return;
        }
        
        // Parse the program
        Parser.Parse();

#if defined(DEBUG)                
if (NC)
{
    LDA #'a' Print.Char();
    CLC
}        
        

        PHP
        LDA AST.astRootL
        ORA AST.astRootH
        if (NZ)  // Have an AST?
        {
            AST.PrintTree(); 
        }
        PLP
#endif        
        if (C)
        {
            CodeGen.Compile();
            if (C)
            {
                // Create output filename by adding ".EXE"
                makeOutputName(); // -> STR
                if (C)
                {
                    Gen6502.Save();
                }
            }
        
//#if defined(DEBUG) 
            LDA #(msgCompiled % 256)
            STA ZP.STRL
            LDA #(msgCompiled / 256)
            STA ZP.STRH
            Print.String();
            Memory.Available();
            Shared.MoveAccToTop();
            Long.Print();
            Print.NewLine();
//#endif      
        }  
        
        LDA outputNameL
        ORA outputNameH
        if (NZ)
        {
            LDA outputNameL
            STA ZP.IDXL
            LDA outputNameH
            STA ZP.IDXH
            Memory.Free();
        }
        
        CodeGen.Dispose();
        Lexer.Dispose();
        AST.Dispose();
        
//#if defined(DEBUG) 
        LDA #(msgCleaned % 256)
        STA ZP.STRL
        LDA #(msgCleaned / 256)
        STA ZP.STRH
        Print.String();
        Memory.Available();
        Shared.MoveAccToTop();
        Long.Print();
        Print.NewLine();
//#endif        
    }
}

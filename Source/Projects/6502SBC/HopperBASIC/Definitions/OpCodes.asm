unit OpCodes 
{
   // OpCode definitions for HopperBASIC JIT compiler
   // 
   // Encoding scheme uses 6+2 bits:
   // - Bits 7-6: Operand count (00=none, 01=one byte, 10=two bytes, 11=reserved)
   // - Bits 5-0: OpCode value (0-63 per group)
   //
   // This design enables:
   // - Fast dispatch: Single byte fetch reveals operand count
   // - Compact encoding: Common operations use single byte
   // - Future expansion: 64 opcodes reserved for complex instructions
   // - Resolve-and-replace: Unresolved opcodes patch to fast versions after first execution
   
   enum OpCode
   {
       // === INVALID OPCODE ===
       INVALID      = 0x00,  // Invalid opcode - triggers error
       
       // === OPCODES WITH NO OPERANDS (0x01-0x3F) ===
       // Bits 7-6: 00 (no operands)
       // Bits 5-0: OpCode (1-63 available, 0x00 reserved for INVALID)
       
       // Arithmetic operations
       ADD          = 0x01,  // Pop two values, push sum
       SUB          = 0x02,  // Pop two values, push difference
       MUL          = 0x03,  // Pop two values, push product
       DIV          = 0x04,  // Pop two values, push quotient
       MOD          = 0x05,  // Pop two values, push remainder
       NEG          = 0x06,  // Pop value, push negation
       
       // Bitwise operations
       BITWISE_AND  = 0x07,  // Pop two values, push bitwise AND
       BITWISE_OR   = 0x08,  // Pop two values, push bitwise OR
       
       // Logical operations (BIT type only)
       LOGICAL_AND  = 0x09,  // Pop two BIT values, push logical AND
       LOGICAL_OR   = 0x0A,  // Pop two BIT values, push logical OR
       LOGICAL_NOT  = 0x0B,  // Pop BIT value, push logical NOT
       
       // Comparison operations (return BIT type)
       EQ           = 0x0C,  // Pop two values, push equality result (BIT)
       NE           = 0x0D,  // Pop two values, push inequality result (BIT)
       LT           = 0x0E,  // Pop two values, push less than result (BIT)
       GT           = 0x0F,  // Pop two values, push greater than result (BIT)
       LE           = 0x10,  // Pop two values, push less or equal result (BIT)
       GE           = 0x11,  // Pop two values, push greater or equal result (BIT)
       
       // Stack manipulation
       DECSP        = 0x14,  // Decrement stack pointer (discard top value)
       DUP          = 0x15,  // Duplicate top stack value
       
       // Utility and common literals
       NOP          = 0x16,  // No operation (useful for code generation/optimization)
       PUSH0        = 0x17,  // Push INT 0 (very common literal, no operand)
       PUSH1        = 0x18,  // Push INT 1 (very common literal, no operand)
       PUSHVOID     = 0x19,  // Push VOID 0 (very common literal, no operand)
       
       HALT         = 0x1A,  // end of REPL opcode stream
       
       // Function frame management
       ENTER        = 0x1B,  // Enter function frame - push BP, SP->BP
       
       CLEARSCREEN  = 0x1C,
       PUSHEMPTYVAR = 0x1D,  // create a stack slot with 0 value and type VAR|INT
       
       // === OPCODES WITH ONE BYTE OPERAND (0x40-0x7F) ===
       // Bits 7-6: 01 (one byte operand)
       // Bits 5-0: OpCode (0-63 available)
       
       // Literal pushes (small values)
       PUSHBIT      = 0x40,  // Push BIT immediate [value] (0 or 1)
       PUSHBYTE     = 0x41,  // Push BYTE immediate [value] (0-255)
       
       // Local variable operations (stack frame relative)
       PUSHLOCAL    = 0x42,  // Push local variable [signed_offset]
       POPLOCAL     = 0x43,  // Pop to local variable [signed_offset]
       
       // Control flow (short jumps)
       JUMPB        = 0x44,  // Unconditional jump [signed_delta]
       JUMPZB       = 0x45,  // Jump if zero [signed_delta]
       JUMPNZB      = 0x46,  // Jump if non-zero [signed_delta]
       
       // System calls
       SYSCALL      = 0x47,  // System call [function_id]
       
       // Control flow
       RETURN       = 0x49,  // Return from function (no return value)
       RETURNVAL    = 0x4A,  // Return from function (pop return value from stack)
       
       // Global variable operations (unresolved → resolved)
       PUSHGLOBAL   = 0x4B,  // Push global [value] (resolved at compile time to index in Variables)
       POPGLOBAL    = 0x4C,  // Pop to global [value]  (resolved at compile time to index in Variables)
       
       
       // === OPCODES WITH TWO BYTE OPERANDS (0x80-0xBF) ===
       // Bits 7-6: 10 (two byte operands)
       // Bits 5-0: OpCode (0-63 available)
       
       // Literal pushes (16-bit values)
       PUSHINT      = 0x80,  // Push INT immediate [lsb] [msb]
       PUSHWORD     = 0x81,  // Push WORD immediate [lsb] [msb]
       PUSHCSTRING  = 0x82,  // Push CONSTSTRING pointer [addr_lsb] [addr_msb]
       
       // Function calls (unresolved → resolved)
       CALL         = 0x83,  // Call function by name [name_offset_lsb] [name_offset_msb]
       CALLF        = 0x84,  // Call function fast [node_lsb] [node_msb] (resolved on first call)
       
       // Control flow (long jumps)
       JUMPW        = 0x87,  // Unconditional jump [lsb] [msb]
       JUMPZW       = 0x88,  // Jump if zero [lsb] [msb]
       JUMPNZW      = 0x89,  // Jump if non-zero [lsb] [msb]
       

        // === THREE-OPERAND OPCODES (0xC0-0xFF) ===
        // Bits 7-6: 11 (three operand bytes)
        // All opcodes in this range have exactly 3 operand bytes

        FORCHK  = 0xC0,  // FOR initial check [iterator_offset] [forward_offset_lsb] [forward_offset_msb]
                         // Compares iterator with limit, jumps forward if out of range
                        
        FORIT   = 0xC1,  // FOR iterate [iterator_offset] [backward_offset_lsb] [backward_offset_msb]  
                         // Increments iterator by step, checks limit, jumps back if continuing
                        
        FORITF  = 0xC2,  // FOR iterate [iterator_offset] [backward_offset_lsb] [backward_offset_msb]  
                         // Built for speed, increments iterator by +1, FROM and TO are +ve and FROM < TO, checks limit (no FORCHK), jumps back if continuing
       
   }
   
   
   // String constants for all opcodes
   const string opcodeUNDEFINED = "UNDEFINED";
   const string opcodeINVALID = "INVALID";
   const string opcodeADD = "ADD";
   const string opcodeSUB = "SUB";
   const string opcodeMUL = "MUL";
   const string opcodeDIV = "DIV";
   const string opcodeMOD = "MOD";
   const string opcodeNEG = "NEG";
   const string opcodeBITWISE_AND = "BITWISE_AND";
   const string opcodeBITWISE_OR = "BITWISE_OR";
   const string opcodeLOGICAL_AND = "LOGICAL_AND";
   const string opcodeLOGICAL_OR = "LOGICAL_OR";
   const string opcodeLOGICAL_NOT = "LOGICAL_NOT";
   const string opcodeEQ = "EQ";
   const string opcodeNE = "NE";
   const string opcodeLT = "LT";
   const string opcodeGT = "GT";
   const string opcodeLE = "LE";
   const string opcodeGE = "GE";
   const string opcodeDECSP = "DECSP";
   const string opcodeDUP = "DUP";
   const string opcodeNOP = "NOP";
   const string opcodePUSH0 = "PUSH0";
   const string opcodePUSH1 = "PUSH1";
   const string opcodePUSHVOID = "PUSHVOID";
   const string opcodeHALT = "HALT";
   const string opcodeENTER = "ENTER";
   const string opcodePUSHBIT = "PUSHBIT";
   const string opcodePUSHBYTE = "PUSHBYTE";
   const string opcodePUSHLOCAL = "PUSHLOCAL";
   const string opcodePOPLOCAL = "POPLOCAL";
   const string opcodeJUMPB = "JUMPB";
   const string opcodeJUMPZB = "JUMPZB";
   const string opcodeJUMPNZB = "JUMPNZB";
   const string opcodeSYSCALL = "SYSCALL";
   const string opcodeRETURN = "RETURN";
   const string opcodeRETURNVAL = "RETURNVAL";
   const string opcodePUSHINT = "PUSHINT";
   const string opcodePUSHWORD = "PUSHWORD";
   const string opcodePUSHCSTRING = "PUSHCSTRING";
   const string opcodeCALL = "CALL";
   const string opcodeCALLF = "CALLF";
   const string opcodePUSHGLOBAL = "PUSHGLOBAL";
   const string opcodePOPGLOBAL = "POPGLOBAL";
   const string opcodeJUMPW = "JUMPW";
   const string opcodeJUMPZW = "JUMPZW";
   const string opcodeJUMPNZW = "JUMPNZW";
   const string opcodeFORCHK = "FORCHK";
   const string opcodeFORIT  = "FORIT";
   const string opcodeFORITF  = "FORITF";
   const string opcodePUSHEMPTYVAR  = "PUSHEMPTYVAR";
   const string opcodeCLEARSCREEN  = "CLEARSCREEN";
   
#if defined(DEBUG) || defined(TRACEEXE)
   // Input: opcode in X
   // Output: string pointer in ZP.STR
   ToString()
   {
        PHA
        switch (X)
        {
            case OpCode.PUSHEMPTYVAR:
            {
                LDA #(opcodePUSHEMPTYVAR % 256)
                STA ZP.STRL
                LDA #(opcodePUSHEMPTYVAR / 256)
                STA ZP.STRH
            }
            case OpCode.CLEARSCREEN:
            {
                LDA #(opcodeCLEARSCREEN % 256)
                STA ZP.STRL
                LDA #(opcodeCLEARSCREEN / 256)
                STA ZP.STRH
            }
            case OpCode.INVALID:
            {
                LDA #(opcodeINVALID % 256)
                STA ZP.STRL
                LDA #(opcodeINVALID / 256)
                STA ZP.STRH
            }
            case OpCode.ADD:
            {
                LDA #(opcodeADD % 256)
                STA ZP.STRL
                LDA #(opcodeADD / 256)
                STA ZP.STRH
            }
            case OpCode.SUB:
            {
                LDA #(opcodeSUB % 256)
                STA ZP.STRL
                LDA #(opcodeSUB / 256)
                STA ZP.STRH
            }
            case OpCode.MUL:
            {
                LDA #(opcodeMUL % 256)
                STA ZP.STRL
                LDA #(opcodeMUL / 256)
                STA ZP.STRH
            }
            case OpCode.DIV:
            {
                LDA #(opcodeDIV % 256)
                STA ZP.STRL
                LDA #(opcodeDIV / 256)
                STA ZP.STRH
            }
            case OpCode.MOD:
            {
                LDA #(opcodeMOD % 256)
                STA ZP.STRL
                LDA #(opcodeMOD / 256)
                STA ZP.STRH
            }
            case OpCode.NEG:
            {
                LDA #(opcodeNEG % 256)
                STA ZP.STRL
                LDA #(opcodeNEG / 256)
                STA ZP.STRH
            }
            case OpCode.BITWISE_AND:
            {
                LDA #(opcodeBITWISE_AND % 256)
                STA ZP.STRL
                LDA #(opcodeBITWISE_AND / 256)
                STA ZP.STRH
            }
            case OpCode.BITWISE_OR:
            {
                LDA #(opcodeBITWISE_OR % 256)
                STA ZP.STRL
                LDA #(opcodeBITWISE_OR / 256)
                STA ZP.STRH
            }
            case OpCode.LOGICAL_AND:
            {
                LDA #(opcodeLOGICAL_AND % 256)
                STA ZP.STRL
                LDA #(opcodeLOGICAL_AND / 256)
                STA ZP.STRH
            }
            case OpCode.LOGICAL_OR:
            {
                LDA #(opcodeLOGICAL_OR % 256)
                STA ZP.STRL
                LDA #(opcodeLOGICAL_OR / 256)
                STA ZP.STRH
            }
            case OpCode.LOGICAL_NOT:
            {
                LDA #(opcodeLOGICAL_NOT % 256)
                STA ZP.STRL
                LDA #(opcodeLOGICAL_NOT / 256)
                STA ZP.STRH
            }
            case OpCode.EQ:
            {
                LDA #(opcodeEQ % 256)
                STA ZP.STRL
                LDA #(opcodeEQ / 256)
                STA ZP.STRH
            }
            case OpCode.NE:
            {
                LDA #(opcodeNE % 256)
                STA ZP.STRL
                LDA #(opcodeNE / 256)
                STA ZP.STRH
            }
            case OpCode.LT:
            {
                LDA #(opcodeLT % 256)
                STA ZP.STRL
                LDA #(opcodeLT / 256)
                STA ZP.STRH
            }
            case OpCode.GT:
            {
                LDA #(opcodeGT % 256)
                STA ZP.STRL
                LDA #(opcodeGT / 256)
                STA ZP.STRH
            }
            case OpCode.LE:
            {
                LDA #(opcodeLE % 256)
                STA ZP.STRL
                LDA #(opcodeLE / 256)
                STA ZP.STRH
            }
            case OpCode.GE:
            {
                LDA #(opcodeGE % 256)
                STA ZP.STRL
                LDA #(opcodeGE / 256)
                STA ZP.STRH
            }
            case OpCode.DECSP:
            {
                LDA #(opcodeDECSP % 256)
                STA ZP.STRL
                LDA #(opcodeDECSP / 256)
                STA ZP.STRH
            }
            case OpCode.DUP:
            {
                LDA #(opcodeDUP % 256)
                STA ZP.STRL
                LDA #(opcodeDUP / 256)
                STA ZP.STRH
            }
            case OpCode.NOP:
            {
                LDA #(opcodeNOP % 256)
                STA ZP.STRL
                LDA #(opcodeNOP / 256)
                STA ZP.STRH
            }
            case OpCode.PUSH0:
            {
                LDA #(opcodePUSH0 % 256)
                STA ZP.STRL
                LDA #(opcodePUSH0 / 256)
                STA ZP.STRH
            }
            case OpCode.PUSH1:
            {
                LDA #(opcodePUSH1 % 256)
                STA ZP.STRL
                LDA #(opcodePUSH1 / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHVOID:
            {
                LDA #(opcodePUSHVOID % 256)
                STA ZP.STRL
                LDA #(opcodePUSHVOID / 256)
                STA ZP.STRH
            }
            case OpCode.HALT:
            {
                LDA #(opcodeHALT % 256)
                STA ZP.STRL
                LDA #(opcodeHALT / 256)
                STA ZP.STRH
            }
            case OpCode.ENTER:
            {
                LDA #(opcodeENTER % 256)
                STA ZP.STRL
                LDA #(opcodeENTER / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHBIT:
            {
                LDA #(opcodePUSHBIT % 256)
                STA ZP.STRL
                LDA #(opcodePUSHBIT / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHBYTE:
            {
                LDA #(opcodePUSHBYTE % 256)
                STA ZP.STRL
                LDA #(opcodePUSHBYTE / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHLOCAL:
            {
                LDA #(opcodePUSHLOCAL % 256)
                STA ZP.STRL
                LDA #(opcodePUSHLOCAL / 256)
                STA ZP.STRH
            }
            case OpCode.POPLOCAL:
            {
                LDA #(opcodePOPLOCAL % 256)
                STA ZP.STRL
                LDA #(opcodePOPLOCAL / 256)
                STA ZP.STRH
            }
            case OpCode.JUMPB:
            {
                LDA #(opcodeJUMPB % 256)
                STA ZP.STRL
                LDA #(opcodeJUMPB / 256)
                STA ZP.STRH
            }
            case OpCode.JUMPZB:
            {
                LDA #(opcodeJUMPZB % 256)
                STA ZP.STRL
                LDA #(opcodeJUMPZB / 256)
                STA ZP.STRH
            }
            case OpCode.JUMPNZB:
            {
                LDA #(opcodeJUMPNZB % 256)
                STA ZP.STRL
                LDA #(opcodeJUMPNZB / 256)
                STA ZP.STRH
            }
            case OpCode.SYSCALL:
            {
                LDA #(opcodeSYSCALL % 256)
                STA ZP.STRL
                LDA #(opcodeSYSCALL / 256)
                STA ZP.STRH
            }
            case OpCode.RETURN:
            {
                LDA #(opcodeRETURN % 256)
                STA ZP.STRL
                LDA #(opcodeRETURN / 256)
                STA ZP.STRH
            }
            case OpCode.RETURNVAL:
            {
                LDA #(opcodeRETURNVAL % 256)
                STA ZP.STRL
                LDA #(opcodeRETURNVAL / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHINT:
            {
                LDA #(opcodePUSHINT % 256)
                STA ZP.STRL
                LDA #(opcodePUSHINT / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHWORD:
            {
                LDA #(opcodePUSHWORD % 256)
                STA ZP.STRL
                LDA #(opcodePUSHWORD / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHCSTRING:
            {
                LDA #(opcodePUSHCSTRING % 256)
                STA ZP.STRL
                LDA #(opcodePUSHCSTRING / 256)
                STA ZP.STRH
            }
            case OpCode.CALL:
            {
                LDA #(opcodeCALL % 256)
                STA ZP.STRL
                LDA #(opcodeCALL / 256)
                STA ZP.STRH
            }
            case OpCode.CALLF:
            {
                LDA #(opcodeCALLF % 256)
                STA ZP.STRL
                LDA #(opcodeCALLF / 256)
                STA ZP.STRH
            }
            case OpCode.PUSHGLOBAL:
            {
                LDA #(opcodePUSHGLOBAL % 256)
                STA ZP.STRL
                LDA #(opcodePUSHGLOBAL / 256)
                STA ZP.STRH
            }
            case OpCode.POPGLOBAL:
            {
                LDA #(opcodePOPGLOBAL % 256)
                STA ZP.STRL
                LDA #(opcodePOPGLOBAL / 256)
                STA ZP.STRH
            }
            case OpCode.JUMPW:
            {
                LDA #(opcodeJUMPW % 256)
                STA ZP.STRL
                LDA #(opcodeJUMPW / 256)
                STA ZP.STRH
            }
            case OpCode.JUMPZW:
            {
                LDA #(opcodeJUMPZW % 256)
                STA ZP.STRL
                LDA #(opcodeJUMPZW / 256)
                STA ZP.STRH
            }
            case OpCode.JUMPNZW:
            {
                LDA #(opcodeJUMPNZW % 256)
                STA ZP.STRL
                LDA #(opcodeJUMPNZW / 256)
                STA ZP.STRH
            }
            case OpCode.FORCHK:
            {
                LDA #(opcodeFORCHK % 256)
                STA ZP.STRL
                LDA #(opcodeFORCHK / 256)
                STA ZP.STRH
            }
            case OpCode.FORIT:
            {
                LDA #(opcodeFORIT % 256)
                STA ZP.STRL
                LDA #(opcodeFORIT / 256)
                STA ZP.STRH
            }
            case OpCode.FORITF:
            {
                LDA #(opcodeFORITF % 256)
                STA ZP.STRL
                LDA #(opcodeFORITF / 256)
                STA ZP.STRH
            }
            default:
            {
                LDA #(opcodeUNDEFINED % 256)
                STA ZP.STRL
                LDA #(opcodeUNDEFINED / 256)
                STA ZP.STRH
            }
        } // switch
        PLA
   }
#endif

}

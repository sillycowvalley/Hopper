unit Instructions
{
    uses "/Source/Runtime/HopperVM"
    
    uses "OpCodes"
    
    delegate bool InstructionDelegate();
    
    PopulateJumpTable(uint jumpTable)
    {
        InstructionDelegate
        instructionDelegate = Instructions.Undefined;
//#ifdef CHECKED
        for (uint opCode = 0; opCode < 256; opCode++)
        {
            WriteToJumpTable(jumpTable, byte(opCode), instructionDelegate);
        }
//#endif
        instructionDelegate = Instructions.Die;
        WriteToJumpTable(jumpTable, byte(OpCode.DIE), instructionDelegate);
        
        instructionDelegate = Instructions.Add;
        WriteToJumpTable(jumpTable, byte(OpCode.ADD), instructionDelegate);
        instructionDelegate = Instructions.Sub;
        WriteToJumpTable(jumpTable, byte(OpCode.SUB), instructionDelegate);
        instructionDelegate = Instructions.Div;
        WriteToJumpTable(jumpTable, byte(OpCode.DIV), instructionDelegate);
        instructionDelegate = Instructions.Mul;
        WriteToJumpTable(jumpTable, byte(OpCode.MUL), instructionDelegate);
        instructionDelegate = Instructions.Mod;
        WriteToJumpTable(jumpTable, byte(OpCode.MOD), instructionDelegate);
        
        instructionDelegate = Instructions.EQ;
        WriteToJumpTable(jumpTable, byte(OpCode.EQ), instructionDelegate);
        instructionDelegate = Instructions.NE;
        WriteToJumpTable(jumpTable, byte(OpCode.NE), instructionDelegate);
        instructionDelegate = Instructions.GT;
        WriteToJumpTable(jumpTable, byte(OpCode.GT), instructionDelegate);
        instructionDelegate = Instructions.LT;
        WriteToJumpTable(jumpTable, byte(OpCode.LT), instructionDelegate);
        instructionDelegate = Instructions.GE;
        WriteToJumpTable(jumpTable, byte(OpCode.GE), instructionDelegate);
        instructionDelegate = Instructions.LE;
        WriteToJumpTable(jumpTable, byte(OpCode.LE), instructionDelegate);
        
        instructionDelegate = Instructions.BoolOr;
        WriteToJumpTable(jumpTable, byte(OpCode.BOOLOR), instructionDelegate);
        instructionDelegate = Instructions.BoolAnd;
        WriteToJumpTable(jumpTable, byte(OpCode.BOOLAND), instructionDelegate);
        instructionDelegate = Instructions.BitOr;
        WriteToJumpTable(jumpTable, byte(OpCode.BITOR), instructionDelegate);
        instructionDelegate = Instructions.BitAnd;
        WriteToJumpTable(jumpTable, byte(OpCode.BITAND), instructionDelegate);
        instructionDelegate = Instructions.BitShl;
        WriteToJumpTable(jumpTable, byte(OpCode.BITSHL), instructionDelegate);
        instructionDelegate = Instructions.BitShr;
        WriteToJumpTable(jumpTable, byte(OpCode.BITSHR), instructionDelegate);
        
        instructionDelegate = Instructions.AddI;
        WriteToJumpTable(jumpTable, byte(OpCode.ADDI), instructionDelegate);
        instructionDelegate = Instructions.SubI;
        WriteToJumpTable(jumpTable, byte(OpCode.SUBI), instructionDelegate);
        instructionDelegate = Instructions.DivI;
        WriteToJumpTable(jumpTable, byte(OpCode.DIVI), instructionDelegate);
        instructionDelegate = Instructions.MulI;
        WriteToJumpTable(jumpTable, byte(OpCode.MULI), instructionDelegate);
        instructionDelegate = Instructions.ModI;
        WriteToJumpTable(jumpTable, byte(OpCode.MODI), instructionDelegate);
        instructionDelegate = Instructions.GTI;
        WriteToJumpTable(jumpTable, byte(OpCode.GTI), instructionDelegate);
        instructionDelegate = Instructions.LTI;
        WriteToJumpTable(jumpTable, byte(OpCode.LTI), instructionDelegate);
        instructionDelegate = Instructions.GEI;
        WriteToJumpTable(jumpTable, byte(OpCode.GEI), instructionDelegate);
        instructionDelegate = Instructions.LEI;
        WriteToJumpTable(jumpTable, byte(OpCode.LEI), instructionDelegate);
        
        instructionDelegate = Instructions.PushIB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIB), instructionDelegate);
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHDB), instructionDelegate);
        instructionDelegate = Instructions.PushIBB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIBB), instructionDelegate);
        
        instructionDelegate = Instructions.PopLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCALB), instructionDelegate);
        instructionDelegate = Instructions.PushLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCALB), instructionDelegate);
        instructionDelegate = Instructions.PopRelB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPRELB), instructionDelegate);
        instructionDelegate = Instructions.PushRelB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHRELB), instructionDelegate);
        instructionDelegate = Instructions.PopGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPGLOBALB), instructionDelegate);
        instructionDelegate = Instructions.PushGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHGLOBALB), instructionDelegate);
        instructionDelegate = Instructions.PushStackAddrB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHSTACKADDRB), instructionDelegate);
        instructionDelegate = Instructions.CallB;
        WriteToJumpTable(jumpTable, byte(OpCode.CALLB), instructionDelegate);
        instructionDelegate = Instructions.JZB;
        WriteToJumpTable(jumpTable, byte(OpCode.JZB), instructionDelegate);
        instructionDelegate = Instructions.JNZB;
        WriteToJumpTable(jumpTable, byte(OpCode.JNZB), instructionDelegate);
        instructionDelegate = Instructions.JB;
        WriteToJumpTable(jumpTable, byte(OpCode.JB), instructionDelegate);
        instructionDelegate = Instructions.Ret0;
        WriteToJumpTable(jumpTable, byte(OpCode.RET0), instructionDelegate);
        instructionDelegate = Instructions.PushI0;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHI0), instructionDelegate);
        instructionDelegate = Instructions.PushI1;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHI1), instructionDelegate);
        
        instructionDelegate = Instructions.PopLocalB00;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCALB00), instructionDelegate);
        instructionDelegate = Instructions.PopLocalB01;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCALB01), instructionDelegate);
        instructionDelegate = Instructions.PushLocalB00;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCALB00), instructionDelegate);
        instructionDelegate = Instructions.PushLocalB01;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCALB01), instructionDelegate);
        
        instructionDelegate = Instructions.SysCall0;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL0), instructionDelegate);
        instructionDelegate = Instructions.SysCall1;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL1), instructionDelegate);
        instructionDelegate = Instructions.SysCall00;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL00), instructionDelegate);
        instructionDelegate = Instructions.SysCall01;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL01), instructionDelegate);
        instructionDelegate = Instructions.SysCall10;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL10), instructionDelegate);
        instructionDelegate = Instructions.SysCallB0;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALLB0), instructionDelegate);
        instructionDelegate = Instructions.SysCallB1;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALLB1), instructionDelegate);
        
        
        instructionDelegate = Instructions.PushGlobalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHGLOBALBB), instructionDelegate);
     
        
        instructionDelegate = Instructions.PushLocalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCALBB), instructionDelegate);
        instructionDelegate = Instructions.PopCopyLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPCOPYLOCALB), instructionDelegate);
        instructionDelegate = Instructions.PopCopyRelB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPCOPYRELB), instructionDelegate);
        instructionDelegate = Instructions.PopCopyGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPCOPYGLOBALB), instructionDelegate);
        instructionDelegate = Instructions.PopCopyLocalB00;
        WriteToJumpTable(jumpTable, byte(OpCode.POPCOPYLOCALB00), instructionDelegate);
        instructionDelegate = Instructions.PopCopyLocalB01;
        WriteToJumpTable(jumpTable, byte(OpCode.POPCOPYLOCALB01), instructionDelegate);
        
        instructionDelegate = Instructions.EnterB;
        WriteToJumpTable(jumpTable, byte(OpCode.ENTERB), instructionDelegate);
     
        
        instructionDelegate = Instructions.JIXB;
        WriteToJumpTable(jumpTable, byte(OpCode.JIXB), instructionDelegate);
        
        instructionDelegate = Instructions.PushILE;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHILE), instructionDelegate);
        instructionDelegate = Instructions.PushILT;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHILT), instructionDelegate);
        instructionDelegate = Instructions.PushIBLE;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIBLE), instructionDelegate);
        instructionDelegate = Instructions.PushILEI;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHILEI), instructionDelegate);
        instructionDelegate = Instructions.PushIBEQ;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIBEQ), instructionDelegate);
        
        instructionDelegate = Instructions.AddB;
        WriteToJumpTable(jumpTable, byte(OpCode.ADDB), instructionDelegate);
        instructionDelegate = Instructions.SubB;
        WriteToJumpTable(jumpTable, byte(OpCode.SUBB), instructionDelegate);
        
        instructionDelegate = Instructions.RetB;
        WriteToJumpTable(jumpTable, byte(OpCode.RETB), instructionDelegate);
        instructionDelegate = Instructions.RetResB;
        WriteToJumpTable(jumpTable, byte(OpCode.RETRESB), instructionDelegate);
        instructionDelegate = Instructions.RetFast;
        WriteToJumpTable(jumpTable, byte(OpCode.RETFAST), instructionDelegate);
        
        instructionDelegate = Instructions.PopLocal;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCAL), instructionDelegate);
        instructionDelegate = Instructions.PushLocal;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCAL), instructionDelegate);
        instructionDelegate = Instructions.PopRel;
        WriteToJumpTable(jumpTable, byte(OpCode.POPREL), instructionDelegate);
        instructionDelegate = Instructions.PushRel;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHREL), instructionDelegate);
        instructionDelegate = Instructions.PopGlobal;
        WriteToJumpTable(jumpTable, byte(OpCode.POPGLOBAL), instructionDelegate);
        instructionDelegate = Instructions.PushGlobal;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHGLOBAL), instructionDelegate);
        instructionDelegate = Instructions.PushStackAddr;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHSTACKADDR), instructionDelegate);
        
        instructionDelegate = Instructions.Dup;
        WriteToJumpTable(jumpTable, byte(OpCode.DUP), instructionDelegate);
        instructionDelegate = Instructions.DecSP;
        WriteToJumpTable(jumpTable, byte(OpCode.DECSP), instructionDelegate);
        
        instructionDelegate = Instructions.Ret;
        WriteToJumpTable(jumpTable, byte(OpCode.RET), instructionDelegate);
        instructionDelegate = Instructions.RetRes;
        WriteToJumpTable(jumpTable, byte(OpCode.RETRES), instructionDelegate);
        instructionDelegate = Instructions.TestBPB;
        WriteToJumpTable(jumpTable, byte(OpCode.TESTBPB), instructionDelegate);
        
        instructionDelegate = Instructions.Exit;
        WriteToJumpTable(jumpTable, byte(OpCode.EXIT), instructionDelegate);
        
        
        instructionDelegate = Instructions.JZ;
        WriteToJumpTable(jumpTable, byte(OpCode.JZ), instructionDelegate);
        instructionDelegate = Instructions.JNZ;
        WriteToJumpTable(jumpTable, byte(OpCode.JNZ), instructionDelegate);
        instructionDelegate = Instructions.J;
        WriteToJumpTable(jumpTable, byte(OpCode.JW), instructionDelegate);
        
        instructionDelegate = Instructions.PushIW;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHI), instructionDelegate);
        
        instructionDelegate = Instructions.BoolNot;
        WriteToJumpTable(jumpTable, byte(OpCode.BOOLNOT), instructionDelegate);
        instructionDelegate = Instructions.BitNot;
        WriteToJumpTable(jumpTable, byte(OpCode.BITNOT), instructionDelegate);
        
        instructionDelegate = Instructions.Swap;
        WriteToJumpTable(jumpTable, byte(OpCode.SWAP), instructionDelegate);
        instructionDelegate = Instructions.PushIM1;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIM1), instructionDelegate);
        instructionDelegate = Instructions.PushGP;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHGP), instructionDelegate);
        
        instructionDelegate = Instructions.CNP;
        WriteToJumpTable(jumpTable, byte(OpCode.COPYNEXTPOP), instructionDelegate);
        instructionDelegate = Instructions.Enter;
        WriteToJumpTable(jumpTable, byte(OpCode.ENTER), instructionDelegate);
     
        instructionDelegate = Instructions.NOP;
        WriteToJumpTable(jumpTable, byte(OpCode.NOP), instructionDelegate);
        instructionDelegate = Instructions.Cast;
        WriteToJumpTable(jumpTable, byte(OpCode.CAST), instructionDelegate);
        
        instructionDelegate = Instructions.PushIW;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHD), instructionDelegate);
        
        instructionDelegate = Instructions.BitXor;
        WriteToJumpTable(jumpTable, byte(OpCode.BITXOR), instructionDelegate);
        
        instructionDelegate = Instructions.JREL;
        WriteToJumpTable(jumpTable, byte(OpCode.JREL), instructionDelegate);
        instructionDelegate = Instructions.JIX;
        WriteToJumpTable(jumpTable, byte(OpCode.JIX), instructionDelegate);
        
        instructionDelegate = Instructions.Call;
        WriteToJumpTable(jumpTable, byte(OpCode.CALL), instructionDelegate);
        instructionDelegate = Instructions.CallI;
        WriteToJumpTable(jumpTable, byte(OpCode.CALLI), instructionDelegate);
        instructionDelegate = Instructions.CallRel;
        WriteToJumpTable(jumpTable, byte(OpCode.CALLREL), instructionDelegate);
        
        instructionDelegate = Instructions.SysCall;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL), instructionDelegate);
        instructionDelegate = Instructions.LibCall0;
        WriteToJumpTable(jumpTable, byte(OpCode.LIBCALL0), instructionDelegate);
        instructionDelegate = Instructions.LibCall1;
        WriteToJumpTable(jumpTable, byte(OpCode.LIBCALL1), instructionDelegate);
        instructionDelegate = Instructions.LibCall;
        WriteToJumpTable(jumpTable, byte(OpCode.LIBCALL), instructionDelegate);
        
        instructionDelegate = Instructions.IncLocalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCLOCALBB), instructionDelegate);
        instructionDelegate = Instructions.IncLocalIBB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCLOCALIBB), instructionDelegate);
        instructionDelegate = Instructions.IncGlobalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCGLOBALBB), instructionDelegate);
        
        instructionDelegate = Instructions.IncLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCLOCALB), instructionDelegate);
        instructionDelegate = Instructions.DecLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.DECLOCALB), instructionDelegate);
        instructionDelegate = Instructions.IncGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCGLOBALB), instructionDelegate);
        instructionDelegate = Instructions.DecGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.DECGLOBALB), instructionDelegate);
        
        instructionDelegate = Instructions.IncLocalIB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCLOCALIB), instructionDelegate);
        instructionDelegate = Instructions.DecLocalIB;
        WriteToJumpTable(jumpTable, byte(OpCode.DECLOCALIB), instructionDelegate);
        instructionDelegate = Instructions.IncGlobalIB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCGLOBALIB), instructionDelegate);
        instructionDelegate = Instructions.DecGlobalIB;
        WriteToJumpTable(jumpTable, byte(OpCode.DECGLOBALIB), instructionDelegate);
        
    }
    
    bool Add()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next + top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        
        next = next + top;
        
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif
        return true;
    }
    bool Sub()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next - top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        
        next = next - top;
        
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif
        return true;
    }
    bool Mul()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next * top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        
        next = next * top;
        
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif
        return true;
    }
    bool Div()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        if (top == 0)
        {
            Error = 0x04; // division by zero attempted
            return false;
        }
        else
        {
            Push(next / top, Type.UInt); 
        }
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        
        if (top == 0)
        {
            Error = 0x04; // division by zero attempted
            return false;
        }
        next = next / top;
        
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif
        return true;
    }
    bool Mod()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        if (top == 0)
        {
            Error = 0x04; // division by zero attempted
            return false;
        }
        else
        {
            Push(next % top, Type.UInt); 
        }
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        
        if (top == 0)
        {
            Error = 0x04; // division by zero attempted
            return false;
        }
        next = next % top;
        
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif
        return true;
    }
    
    bool AddI()
    {
#ifdef CHECKED                
        Type ttype;
        int top = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        PushI(next + top); 
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        nexti = nexti + topi;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(nexti & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(nexti >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif
        return true;
    }
    bool MulI()
    {
#ifdef CHECKED                
        Type ttype;
        int top = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        PushI(next * top); 
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        nexti = nexti * topi;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(nexti & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(nexti >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif
        return true;
    }
    bool SubI()
    {
#ifdef CHECKED                
        Type ttype;
        int top = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        PushI(next - top); 
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        nexti = nexti - topi;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(nexti & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(nexti >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif
        return true;
    }
    bool DivI()
    {
#ifdef CHECKED                
        Type ttype;
        int top = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        if (top == 0)
        {
            Error = 0x04; // division by zero attempted
            return false;
        }
        else
        {
            PushI(next / top); 
        }
#else
        int top = PopI(); PushI(PopI() / top);
#endif
        return true;
    }
    bool ModI()
    {
#ifdef CHECKED                
        Type ttype;
        int top = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        if (top == 0)
        {
            Error = 0x04; // division by zero attempted
            return false;
        }
        else
        {
            PushI(next % top); 
        }
#else
        int top = PopI(); PushI(PopI() % top);
#endif
        return true;
    }
    
    bool PushI0()
    {
#ifdef CHECKED
        Push(0, Type.Byte);
#else
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, 0);
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0);
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Byte));
        HopperVM.sp++;
#endif   
        return true;
    }
    bool PushI1()
    {
#ifdef CHECKED
        Push(1, Type.Byte);
#else
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, 1);
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0);
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Byte));
        HopperVM.sp++;
#endif   
        return true;
    }

    bool PushIM1()
    {
#ifdef CHECKED
        PushI(-1);
#else
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, 0xFF);
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0xFF);
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Int));
        HopperVM.sp++;
#endif           
        return true;
    }
    
    bool PushIB()
    {
#ifdef CHECKED
        Push(ReadByteOperand(), Type.Byte);
#else
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, ReadProgramByte(HopperVM.pc));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0);
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Byte));
        HopperVM.sp++;
        HopperVM.pc++;
#endif
        return true;
    }
    bool PushIBB()
    {
#ifdef CHECKED
        Push(ReadByteOperand(), Type.Byte);
        Push(ReadByteOperand(), Type.Byte);
#else
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, ReadProgramByte(HopperVM.pc));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0);
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Byte));
        HopperVM.sp++;
        HopperVM.pc++;
        
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, ReadProgramByte(HopperVM.pc));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0);
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Byte));
        HopperVM.sp++;
        HopperVM.pc++;
#endif
        return true;
    }

    bool PushGP()
    {
        Push(GP, Type.UInt);
        return true;
    }
    bool PushIW()
    {
        Push(ReadWordOperand(), Type.UInt);
        return true;
    }
    bool CallI()
    {
#ifdef CHECKED
        uint methodAddress = ReadWordOperand();
        PushCS(PC);
        PC = methodAddress;
#else
        uint methodAddress = ReadProgramByte(HopperVM.pc) + (ReadProgramByte(HopperVM.pc+1) << 8); 
        uint pc2 = HopperVM.pc+2;
        WriteByte(HopperVM.callStackLSBPage + HopperVM.csp, byte(pc2 & 0xFF));
        WriteByte(HopperVM.callStackMSBPage + HopperVM.csp, byte(pc2 >> 8));
        HopperVM.csp++;
        HopperVM.pc = methodAddress;
#endif
        return true;
    }
    
    bool PushLocalBB()
    {
        bool res = PushLocalB();
        return PushLocalB();
    }
    bool PushGlobalBB()
    {
        bool res = PushGlobalB();
        return PushGlobalB();
    }
    
    bool PushLocalB()
    {
#ifdef CHECKED        
        int offset = ReadByteOffsetOperand();
        Type htype;
        uint value = HopperVM.Get(byte(int(BP) + offset), ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
#else
        int offset = int(ReadProgramByte(HopperVM.pc)); 
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        }
        HopperVM.pc++;
        
        byte address = byte(offset + HopperVM.bp);
        uint value = ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8);
        byte htype = ReadByte(HopperVM.typeStackPage + address);
        
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, htype);
        HopperVM.sp++;
        
        if (htype >= 0x0D)
        {
            value++;
            WriteByte(value, ReadByte(value)+1);
        }
#endif
        return true;
    }
    
    bool PushLocalB00()
    {
#ifdef CHECKED
        Type htype;
        uint value = HopperVM.Get(BP, ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
#else
        byte address = byte(HopperVM.bp);
        uint value = ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8);
        byte htype = ReadByte(HopperVM.typeStackPage + address);
        
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, htype);
        HopperVM.sp++;
        
        if (htype >= 0x0D)
        {
            value++;
            WriteByte(value, ReadByte(value)+1);
        }
#endif
        return true;
    }
    bool PushLocalB01()
    {
#ifdef CHECKED
        Type htype;
        uint value = HopperVM.Get(BP+1, ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
#else
        byte address = byte(HopperVM.bp + 1);
        uint value = ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8);
        byte htype = ReadByte(HopperVM.typeStackPage + address);
        
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, htype);
        HopperVM.sp++;
        
        if (htype >= 0x0D)
        {
            value++;
            WriteByte(value, ReadByte(value)+1);
        }
#endif
        return true;
    }

    bool PushLocal()
    {
        int offset = ReadWordOffsetOperand();
        Type htype;
        uint value = HopperVM.Get(byte(int(BP) + offset), ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    
    bool Enter()
    {
#ifdef CHECKED
        PushCS(BP);
        BP = SP;
#else
        WriteByte(HopperVM.callStackLSBPage + HopperVM.csp, HopperVM.bp);
        WriteByte(HopperVM.callStackMSBPage + HopperVM.csp, 0);
        HopperVM.csp++;
        HopperVM.bp = HopperVM.sp;
#endif
        return true;
    }
    
    bool EnterB()
    {
#ifdef CHECKED
        PushCS(BP);
        BP = SP;
        uint zeros = ReadByteOperand();
        for (uint i = 0; i < zeros; i++)
        {
            Push(0, Type.Byte);
        }
#else
        WriteByte(HopperVM.callStackLSBPage + HopperVM.csp, HopperVM.bp);
        WriteByte(HopperVM.callStackMSBPage + HopperVM.csp, 0);
        HopperVM.csp++;
        HopperVM.bp = HopperVM.sp;
        
        byte zeros = ReadProgramByte(HopperVM.pc); 
        HopperVM.pc++;
        for (uint i = 0; i < zeros; i++)
        {
            WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, 0);
            WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, 0);
            WriteByte(HopperVM.typeStackPage + HopperVM.sp, byte(Type.Byte));
            HopperVM.sp++;
        }
#endif
        return true;
    }
    
    bool JZB()
    {
#ifdef CHECKED  
        int offset = ReadByteOffsetOperand();              
        Type htype;
        uint choice = Pop(ref htype);
        if (IsReferenceType(htype)) // != 0 is not the same as UInt
        {
            ErrorDump(40);
            Error = 0x0B;
            return false;
        }
        
        if (choice == 0)
        {
            PC = uint(int(PC-2) + offset);
        }
#else
        HopperVM.sp--;
        if ((ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)) == 0)
        {
            int offset = int(ReadProgramByte(HopperVM.pc)); 
            HopperVM.pc++;
            if (offset > 127)
            {
                offset = offset - 256; // 0xFF -> -1
            }
            HopperVM.pc = uint(offset + int(HopperVM.pc - 2));
        }
        else
        {
            HopperVM.pc++;
        }
#endif   
        return true;
    }
    bool Exit()
    {
        return HopperVM.ExitInline();
    }
    
    bool RetResB()
    {
#ifdef CHECKED        
        Type rtype;
        uint value = Pop(ref rtype);
        uint popBytes = ReadByteOperand();
        while (popBytes != 0)
        {
            Type htype;
            uint address = Pop(ref htype);
            if (IsReferenceType(htype))
            {
                GC.Release(address);
            }
            popBytes--;
        }
        Push(value, rtype);
        BP = byte(PopCS());
        if (CSP == CSPStart)
        {
            PC = 0; // exit program
        }
        else
        {
            PC = PopCS();
        }
        return PC != 0;
#else
        HopperVM.sp--;
        uint value  = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        byte rtype  = ReadByte(HopperVM.typeStackPage + HopperVM.sp);
        
        byte popBytes = ReadProgramByte(HopperVM.pc); 
        HopperVM.pc++;
        
        while (popBytes != 0)
        {
            HopperVM.sp--;
            uint address  = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
            byte htype  = ReadByte(HopperVM.typeStackPage + HopperVM.sp);
            if (htype >= 0x0D)
            {
                GC.Release(address);
            }
            popBytes--;
        }
        
        WriteByte(HopperVM.valueStackLSBPage + HopperVM.sp, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + HopperVM.sp, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + HopperVM.sp, rtype);
        HopperVM.sp++;
        
        HopperVM.csp--;
        HopperVM.bp = ReadByte(HopperVM.callStackLSBPage + HopperVM.csp);        
        if (HopperVM.csp == HopperVM.cspStart)
        {
            HopperVM.pc = 0; // exit program
        }
        else
        {
            HopperVM.csp--;
            HopperVM.pc= ReadByte(HopperVM.callStackLSBPage + HopperVM.csp) + (ReadByte(HopperVM.callStackMSBPage + HopperVM.csp) << 8);
        }
        return HopperVM.pc != 0;
#endif        
    }
    
    bool Ret0()
    {
#ifdef CHECKED
        BP = byte(PopCS());
        if (CSP == CSPStart)
        {
            PC = 0; // exit program
        }
        else
        {
            PC = PopCS();
        }
        return PC != 0;
#else
        HopperVM.csp--;
        HopperVM.bp = ReadByte(HopperVM.callStackLSBPage + HopperVM.csp);        
        if (HopperVM.csp == HopperVM.cspStart)
        {
            HopperVM.pc = 0; // exit program
        }
        else
        {
            HopperVM.csp--;
            HopperVM.pc= ReadByte(HopperVM.callStackLSBPage + HopperVM.csp) + (ReadByte(HopperVM.callStackMSBPage + HopperVM.csp) << 8);
        }
        return HopperVM.pc != 0;
#endif
    }
    
    bool RetRes()
    {
        Type rtype;
        uint value = Pop(ref rtype);
        uint popBytes = ReadWordOperand();
        while (popBytes != 0)
        {
            Type htype;
            uint address = Pop(ref htype);
            if (IsReferenceType(htype))
            {
                GC.Release(address);
            }
            popBytes--;
        }
        Push(value, rtype);
        BP = byte(PopCS());
        if (CSP == CSPStart)
        {
            PC = 0; // exit program
        }
        else
        {
            PC = PopCS();
        }
        return PC != 0;
    }
    bool NOP()
    {
        return true;
    }
    bool Cast()
    {
#ifdef CHECKED
        HopperVM.Put(SP-1, HopperVM.Get(SP-1), Type(ReadByteOperand()));
#else
        WriteByte(HopperVM.typeStackPage + HopperVM.sp-1, ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
#endif
        return true;
    }
    bool BoolNot()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        AssertBool(ttype, top);
        Push((top == 0) ? 1 : 0, Type.Bool);
#else
        uint sp1 = HopperVM.sp-1;
        uint top  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (top == 0) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool)); 
#endif
        return true;
    }
    bool BitNot()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        AssertUInt(ttype, top);
        Push(~top, Type.UInt); 
#else
        uint sp1 = HopperVM.sp-1;
        uint top  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        top = ~top;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(top & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(top >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif              
        return true;
    }
    bool BoolAnd()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertBool(ttype, top);
        AssertBool(ntype, next);
        // be aware of short circuit boolean evaluation here - need both Pops..
        Push(((next != 0) && (top != 0)) ? 1 : 0, Type.Bool); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, ((next != 0) && (top != 0)) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool)); 
#endif
        return true;
    }
    bool BoolOr()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);

        AssertBool(ttype, top);
        AssertBool(ntype, next);
        // be aware of short circuit boolean evaluation here - need both Pops..
        Push(((next != 0) || (top != 0)) ? 1 : 0, Type.Bool); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, ((next != 0) || (top != 0)) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));  
#endif              
        return true;
    }
    bool BitAnd()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next & top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        next = next & top;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif              
        return true;
    }
    bool BitOr()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next | top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        next = next | top;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif              
        return true;
    }
    bool BitXor()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push((next | top) & (~(next & top)), Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        next = (next | top) & (~(next & top));
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif              
        return true;
    }
    bool BitShl()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next << top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        next = next << top;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt));
#endif              
        return true;
    }
    bool BitShr()
    {
#ifdef CHECKED
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next >> top, Type.UInt); 
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        next = next >> top;
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(next & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(next >> 8));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif              
        return true;
    }
    
    
    bool PopLocalB00()
    {
        if (HopperVM.CNP) 
        { 
            HopperVM.CNP = false;
            return PopCopyLocalB00();
        }
        else
        {
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadByte(uint(int(TypeStackLSB) + int(HopperVM.BP))));
            uint value;
            if (IsReferenceType(htype))
            {
                value = HopperVM.Get(HopperVM.BP);
                GC.Release(value);
            }
            value = Pop(ref htype);
            HopperVM.Put(HopperVM.BP, value, htype);
        }
        return true;
    }
    bool PopLocalB01()
    {
        if (HopperVM.CNP) 
        { 
            HopperVM.CNP = false;
            return PopCopyLocalB01();
        }
        else
        {
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadByte(uint(int(TypeStackLSB) + int(HopperVM.BP) + 1)));
            uint value;
            if (IsReferenceType(htype))
            {
                value = HopperVM.Get(HopperVM.BP+1);
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            HopperVM.Put(HopperVM.BP+1, value, htype);
        }
        return true;
    }
    bool PopLocalB()
    {
        int offset;
        if (HopperVM.CNP) 
        { 
            HopperVM.CNP = false;
            return PopCopyLocalB();
        }
        else
        {
            offset     = ReadByteOffsetOperand();
        
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadByte(uint(int(TypeStackLSB) + int(HopperVM.BP) + offset)));
            uint value;
            if (IsReferenceType(htype))
            {
                value = HopperVM.Get(byte(int(HopperVM.BP) + offset));
                GC.Release(value);
            }
            value = Pop(ref htype);
            HopperVM.Put(byte(int(HopperVM.BP) + offset), value, htype);
        }
        return true;
    }

    bool PopLocal()
    {
        int offset;
        if (HopperVM.CNP) 
        { 
            HopperVM.CNP = false;
            return PopCopyLocal();
        }
        else
        {
            offset     = ReadWordOffsetOperand();
        
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadByte(uint(int(TypeStackLSB) + int(HopperVM.BP) + offset)));
            uint value;
            if (IsReferenceType(htype))
            {
                value = HopperVM.Get(byte(int(HopperVM.BP) + offset));
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            HopperVM.Put(byte(int(HopperVM.BP) + offset), value, htype);
        }
        return true;
    }
    
    bool PopCopyLocalB00()
    {
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        byte localAddress = HopperVM.BP;
        uint oldvalue = HopperVM.Get(localAddress, ref htype);
        if (IsReferenceType(htype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref htype);
        if (value == oldvalue)
        {
            // overwriting self - no more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(localAddress, newvalue, htype); 
        }
        return true;
    }
    bool PopCopyLocalB01()
    {
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        byte localAddress = byte(HopperVM.BP + 1);
        uint oldvalue = HopperVM.Get(localAddress, ref htype);
        if (IsReferenceType(htype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref htype);
        if (value == oldvalue)
        {
            // overwriting self - no more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(localAddress, newvalue, htype); 
        }
        return true;
    }
    
    bool PopCopyLocal()
    {
        int offset     = ReadWordOffsetOperand();
        
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        byte localAddress = byte(int(BP) + offset);
        uint oldvalue = HopperVM.Get(localAddress, ref htype);
        if (IsReferenceType(htype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref htype);
        if (value == oldvalue)
        {
            // overwriting self - no more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(localAddress, newvalue, htype); 
        }
        return true;
    }
    
    bool PopCopyLocalB()
    {
        int offset     = ReadByteOffsetOperand();
        
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        byte localAddress = byte(int(BP) + offset);
        uint oldvalue = HopperVM.Get(localAddress, ref htype);
        if (IsReferenceType(htype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref htype);
        if (value == oldvalue)
        {
            // overwriting self - no more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(localAddress, newvalue, htype); 
        }
        return true;
    }
    
    bool JB()
    {
        PC = uint(ReadByteOffsetOperand() + int(PC-2));
        return true;
    }

    bool J()
    {
        PC = uint(ReadWordOffsetOperand() + int(PC-3));
        return true;
    }
    bool JREL()
    {
        uint address = Pop();
        PC = address;
        return true;
    }
    
    bool JIXB()
    {
        uint switchCase = Pop();

        byte minRange = ReadByteOperand();
        byte maxRange = ReadByteOperand();
        
        byte lsb = ReadByteOperand();
        byte msb = ReadByteOperand();
        
        int jumpBackOffset = int(lsb + (msb << 8));
        
        uint tpc = PC;
        
        PC = uint(int(PC) - jumpBackOffset - 5);
        
        uint tableSize = uint(maxRange) - uint(minRange) + 1;
        
        uint offset = 0;
        if ((switchCase >= minRange) && (switchCase <= maxRange))
        {
            // in the table
            uint index = tpc + switchCase - minRange;
            offset = ReadProgramByte(index);
        }
        
        if (offset == 0)
        {
            // default
            PC = tpc + tableSize;
        }
        else
        {
            PC = PC + offset;
        }
        return true;
    }

    bool JIX()
    {
        uint switchCase = Pop();

        byte minRange = ReadByteOperand();
        byte maxRange = ReadByteOperand();
        
        byte lsb = ReadByteOperand();
        byte msb = ReadByteOperand();
        
        int jumpBackOffset = int(lsb + (msb << 8));
        
        uint tpc = PC;
        
        PC = uint(int(PC) - jumpBackOffset - 5);
        
        uint tableSize = (uint(maxRange) - uint(minRange) + 1) << 1;
        
        uint offset = 0;
        if ((switchCase >= minRange) && (switchCase <= maxRange))
        {
            // in the table
            uint index = tpc + (switchCase - minRange)*2;
            offset = ReadProgramByte(index) + (ReadProgramByte(index+1) << 8);
        }
        
        if (offset == 0)
        {
            // default
            PC = tpc + tableSize;
        }
        else
        {
            PC = PC + offset;
        }
        return true;
    }
    bool CNP()
    {
        HopperVM.CNP = true;
        return true;
    }
    bool EQ()
    {
#ifdef CHECKED
        Push((Pop() == Pop()) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (next == top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));        
#endif   
        return true;
    }
    bool NE()
    {
#ifdef CHECKED
        Push((Pop() != Pop()) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (next == top) ? 0 : 1); // reversed
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));        
#endif           
        return true;
    }
    bool LT()
    {
#ifdef CHECKED
        Type ttype;
        uint top  = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push((next < top) ? 1 : 0, Type.Bool);
#else              
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (next < top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool)); 
#endif
        return true;
    }
    bool GT()
    {
#ifdef CHECKED
        Type ttype;
        uint top  = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push((next > top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (next > top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif        
        return true;        
    }
    bool LE()
    {
#ifdef CHECKED
        Type ttype;
        uint top  = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push((next <= top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (next <= top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif 
        return true;               
    }
    bool GE()
    {
#ifdef CHECKED
        Type ttype;
        uint top  = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push((next >= top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        uint top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        uint sp1 = HopperVM.sp-1;
        uint next  = ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (next >= top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif      
        return true;          
    }
    bool LTI()
    {
#ifdef CHECKED
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        Push((next < top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (nexti < topi) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif   
        return true;             
    }
    bool GTI()
    {
#ifdef CHECKED
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        Push((next > top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (nexti > topi) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif     
        return true;           
    }
    bool LEI()
    {
#ifdef CHECKED
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        Push((next <= top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (nexti <= topi) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif 
        return true;               
    }
    bool GEI()
    {
#ifdef CHECKED
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        Push((next >= top) ? 1 : 0, Type.Bool);
#else
        HopperVM.sp--;
        int topi   = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8)); 
        uint sp1 = HopperVM.sp-1;
        int nexti  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, (nexti >= topi) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.Bool));
#endif   
        return true;             
    }
    
    bool JNZB()
    {
#ifdef CHECKED
        int offset = ReadByteOffsetOperand();
        Type htype;
        uint choice = Pop(ref htype);
        if (IsReferenceType(htype)) // != 0 is not the same as UInt
        {
            ErrorDump(38);
            Error = 0x0B;
            return false;
        }
        if (choice != 0)
        {
            HopperVM.pc = uint(int(HopperVM.pc-2) + offset);
        }
#else
        if (Pop() != 0)
        {
            HopperVM.pc = uint(ReadByteOffsetOperand() + int(HopperVM.pc-2));
        }
        else
        {
            HopperVM.pc++;
        }
#endif   
        return true;
    }

    bool JZ()
    {
#ifdef CHECKED
        int offset = ReadWordOffsetOperand();
        Type htype;
        uint choice = Pop(ref htype);
        if (IsReferenceType(htype)) // != 0 is not the same as UInt
        {
            ErrorDump(39);
            Error = 0x0B;
            return false;
        }
        
        if (choice == 0)
        {
            HopperVM.pc = uint(int(HopperVM.pc-3) + offset);
        }
#else
        if (Pop() == 0)
        {
            HopperVM.pc = uint(ReadWordOffsetOperand() + int(HopperVM.pc-3));
        }
        else
        {
            HopperVM.pc = HopperVM.pc + 2;
        }
#endif   
        return true;
    }
    bool JNZ()
    {
#ifdef CHECKED
        int offset = ReadWordOffsetOperand();
        Type htype;
        uint choice = Pop(ref htype);
        if (IsReferenceType(htype)) // != 0 is not the same as UInt
        {
            ErrorDump(41);
            Error = 0x0B;
            return false;
        }
        
        if (choice != 0)
        {
            PC = uint(int(PC-3) + offset);
        }
#else
        if (Pop() != 0)
        {
            PC = uint(ReadWordOffsetOperand() + int(PC-3));
        }
        else
        {
            PC = PC + 2;
        }
#endif   
        return true;
    }
    bool LibCall0()
    {
#ifdef INCLUDE_LIBRARY
        byte iLibCall = ReadByteOperand();  
        return ExecuteLibCall(iLibCall, 0);
#else
        Error = 0x0A;
        return false;
#endif
    }
    bool LibCall1()
    {
#ifdef INCLUDE_LIBRARY            
        Type htype;
        byte iLibCall = ReadByteOperand();  
        return ExecuteLibCall(iLibCall, 1);
#else
        Error = 0x0A;
        return false;
#endif
    }
    bool LibCall()
    {
#ifdef INCLUDE_LIBRARY            
        Type htype;
        uint iOverload = Pop(ref htype);
        byte iLibCall = ReadByteOperand();  
        return ExecuteLibCall(iLibCall, iOverload);
#else
        Error = 0x0A;
        return false;
#endif
    }

    bool SysCall0()
    {
        byte iSysCall = ReadByteOperand();  
        return ExecuteSysCall(iSysCall, 0);
    }
    bool SysCall1()
    {
        byte iSysCall = ReadByteOperand();  
        return ExecuteSysCall(iSysCall, 1);
    }
    bool SysCall00()
    {
        byte iSysCall = ReadByteOperand();  
        bool doNext0 = ExecuteSysCall(iSysCall, 0);
        iSysCall = ReadByteOperand();  
        bool doNext1 = ExecuteSysCall(iSysCall, 0);
        return doNext0 && doNext1;
    }
    bool SysCall01()
    {
        byte iSysCall = ReadByteOperand();  
        bool doNext0 = ExecuteSysCall(iSysCall, 0);
        iSysCall = ReadByteOperand();  
        bool doNext1 = ExecuteSysCall(iSysCall, 1);
        return doNext0 && doNext1;
    }
    bool SysCall10()
    {
        byte iSysCall = ReadByteOperand();  
        bool doNext0 = ExecuteSysCall(iSysCall, 1);
        iSysCall = ReadByteOperand();  
        bool doNext1 = ExecuteSysCall(iSysCall, 0);
        return doNext0 && doNext1;
    }
    bool SysCallB0()
    {
        Push(ReadByteOperand(), Type.Byte);
        byte iSysCall = ReadByteOperand();  
        return ExecuteSysCall(iSysCall, 0);
    }
    bool SysCallB1()
    {
        Push(ReadByteOperand(), Type.Byte);
        byte iSysCall = ReadByteOperand();  
        return ExecuteSysCall(iSysCall, 1);
    }

    bool SysCall()
    {
        Type htype;
        uint iOverload = Pop(ref htype);
        byte iSysCall  = ReadByteOperand();  
        return ExecuteSysCall(iSysCall, iOverload);
    }

    bool PushILT()
    {
#ifdef CHECKED
        uint top = ReadWordOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ntype, next);
        Push((next < top) ? 1 : 0, Type.Bool);
#else
        uint top = ReadProgramByte(HopperVM.pc) + (ReadProgramByte(HopperVM.pc+1) << 8); 
        HopperVM.pc = HopperVM.pc + 2;
        uint sp2 = HopperVM.sp - 1;
        uint next = ReadByte(HopperVM.valueStackLSBPage + sp2) + (ReadByte(HopperVM.valueStackMSBPage + sp2) << 8);
        WriteByte(HopperVM.valueStackLSBPage + sp2, (next < top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp2, 0);
        WriteByte(HopperVM.typeStackPage + sp2, byte(Type.Bool));
#endif  
        return true;
    }
    bool PushILE()
    {
#ifdef CHECKED
        uint top = ReadWordOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
        //AssertUInt(ntype, next);
        Push((next <= top) ? 1 : 0, Type.Bool);
#else
        uint top = ReadProgramByte(HopperVM.pc) + (ReadProgramByte(HopperVM.pc+1) << 8); 
        HopperVM.pc = HopperVM.pc + 2;
        uint sp2 = HopperVM.sp - 1;
        uint next = ReadByte(HopperVM.valueStackLSBPage + sp2) + (ReadByte(HopperVM.valueStackMSBPage + sp2) << 8);
        WriteByte(HopperVM.valueStackLSBPage + sp2, (next <= top) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp2, 0);
        WriteByte(HopperVM.typeStackPage + sp2, byte(Type.Bool));
#endif  
        return true;
    }
    bool PushILEI()
    {
#ifdef CHECKED
        Push(ReadWordOperand(), Type.UInt);    
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        AssertInt(ttype);
        AssertInt(ntype);
        Push((next <= top) ? 1 : 0, Type.Bool);
#else
        Push(ReadWordOperand(), Type.UInt);    
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
        Push((next <= top) ? 1 : 0, Type.Bool);
#endif  
        return true;
    }
    
    bool PushIBLE()
    {
#ifdef CHECKED
        uint top = ReadByteOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ntype, next);
        Push((next <= top) ? 1 : 0, Type.Bool);
#else
        uint sp2 = HopperVM.sp - 1;
        uint next = ReadByte(HopperVM.valueStackLSBPage + sp2) + (ReadByte(HopperVM.valueStackMSBPage + sp2) << 8);
        WriteByte(HopperVM.valueStackLSBPage + sp2, (next <= ReadProgramByte(HopperVM.pc)) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp2, 0);
        WriteByte(HopperVM.typeStackPage + sp2, byte(Type.Bool));
        HopperVM.pc++;
#endif  
        return true;
    }
    bool PushIBEQ()
    {
#ifdef CHECKED
        uint top = ReadByteOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
        Push((next == top) ? 1 : 0, Type.Bool);
#else
        uint sp2 = HopperVM.sp - 1;
        uint next = ReadByte(HopperVM.valueStackLSBPage + sp2) + (ReadByte(HopperVM.valueStackMSBPage + sp2) << 8);
        WriteByte(HopperVM.valueStackLSBPage + sp2, (next == ReadProgramByte(HopperVM.pc)) ? 1 : 0);
        WriteByte(HopperVM.valueStackMSBPage + sp2, 0);
        WriteByte(HopperVM.typeStackPage + sp2, byte(Type.Bool));
        HopperVM.pc++;
#endif
        return true;
    }
    bool AddB()
    {
#ifdef CHECKED
        uint top = ReadByteOperand();
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ntype, next);
        Push(next + top, Type.UInt);
#else
        uint sp2 = HopperVM.sp - 1;
        uint lsb = HopperVM.valueStackLSBPage + sp2;
        uint msb = HopperVM.valueStackMSBPage + sp2;
        uint value = (ReadByte(lsb) + (ReadByte(msb) << 8)) + ReadProgramByte(HopperVM.pc);        
        WriteByte(lsb, byte(value & 0xFF));
        WriteByte(msb, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + sp2, byte(Type.UInt));
        HopperVM.pc++;
#endif  
        return true;
    }
    bool SubB()
    {
#ifdef CHECKED
        uint top = ReadByteOperand();
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ntype, next);
        Push(next - top, Type.UInt);
#else
        uint sp2 = HopperVM.sp - 1;
        uint lsb = HopperVM.valueStackLSBPage + sp2;
        uint msb = HopperVM.valueStackMSBPage + sp2;
        uint value = (ReadByte(lsb) + (ReadByte(msb) << 8)) - ReadProgramByte(HopperVM.pc);        
        WriteByte(lsb, byte(value & 0xFF));
        WriteByte(msb, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + sp2, byte(Type.UInt));
        HopperVM.pc++;
#endif  
        return true;
    }
    
    bool PushRelB()
    {
        int  offset = ReadByteOffsetOperand();
        byte referenceAddress = byte(int(BP) + offset);
        Type rtype;
        byte localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
#ifdef CHECKED
        AssertReference(rtype, localAddress);
#endif
        uint value = HopperVM.Get(localAddress, ref rtype);
        Push(value, rtype);
        if (IsReferenceType(rtype))
        {
            GC.AddReference(value);
        }
        return true;
    }

    bool PushRel()
    {
        int  offset = ReadWordOffsetOperand();
        byte referenceAddress = byte(int(BP) + offset);
        Type rtype;
        byte localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
#ifdef CHECKED
        AssertReference(rtype, localAddress);
#endif
        uint value = HopperVM.Get(localAddress, ref rtype);
        Push(value, rtype);
        if (IsReferenceType(rtype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    bool PushStackAddrB()
    {
        int  offset = ReadByteOffsetOperand();
#ifdef CHECKED
        if ((int(BP) + offset > 255) || (int(BP) + offset < 0))
        {
            ErrorDump(258); Error = 0x0B;
        }
#endif
        byte address = byte(int(BP) + offset);
        Push(address, Type.Reference);
        return true;
    }
    bool PushStackAddr()
    {
        int  offset = ReadWordOffsetOperand();
#ifdef CHECKED
        if ((int(HopperVM.BP) + offset > 255) || (int(HopperVM.BP) + offset < 0))
        {
            ErrorDump(259); Error = 0x0B;
        }
#endif
        byte address = byte(int(HopperVM.BP) + offset);
        Push(address, Type.Reference);
        return true;
    }
    bool Swap()
    {
        Type ttype;
        Type ntype;
        uint topValue  = HopperVM.Get(HopperVM.SP - 1, ref ttype);
        uint nextValue = HopperVM.Get(HopperVM.SP - 2, ref ntype);
        HopperVM.Put(byte(HopperVM.SP - 1), nextValue, ntype);
        HopperVM.Put(byte(HopperVM.SP - 2), topValue, ttype);
        return true;
    }
    bool Dup()
    {
        // operand is offset 0..255 into stack where 0=[top], 1=[next], etc
        byte  offset  = ReadByteOperand();
        byte address = byte(SP - 1 - offset);
        Type htype;
        uint value = HopperVM.Get(address, ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    bool TestBPB()
    {
        byte  operand  = ReadByteOperand();
        uint bpExpected = uint(HopperVM.SP - operand);
        if (bpExpected != HopperVM.BP)
        {
            Error = 0x0B;
            return false;
        }
        return true;
    }
    
    bool CallB()
    {
        uint methodIndex = ReadByteOperand();
        PushCS(HopperVM.PC);
        HopperVM.PC = LookupMethod(methodIndex);
        return true;
    }

    bool Call()
    {
        uint methodIndex = ReadWordOperand();
        PushCS(HopperVM.PC);
        uint methodAddress = LookupMethod(methodIndex);
        WriteProgramByte(HopperVM.PC-3, byte(OpCode.CALLI));
        WriteProgramWord(HopperVM.PC-2, methodAddress);
        PC = methodAddress;
        return true;
    }
    bool CallRel()
    {
#ifdef CHECKED
        Type rtype;
        uint methodIndex = Pop(ref rtype);
        AssertUInt(rtype, methodIndex);
        if (methodIndex == 0)
        {
            Error = 0x0F; // invalid or uninitialized delegate
            return false;
        }
#else
        uint methodIndex = Pop();                
#endif          
        PushCS(HopperVM.PC);
        HopperVM.PC = LookupMethod(methodIndex);
        return true;
    }
    bool DecSP()
    {
        uint popBytes = ReadByteOperand();
        while (popBytes != 0)
        {
            Type htype;
            uint address = Pop(ref htype);
            if (IsReferenceType(htype))
            {
                GC.Release(address);
            }
            popBytes--;
        }
        return true;
    }
    
    bool RetFast()
    {
        HopperVM.PC = PopCS();
        return true;
    }
    bool RetB()
    {
        uint popBytes = ReadByteOperand();
        while (popBytes != 0)
        {
            Type htype;
            uint address = Pop(ref htype);
            if (IsReferenceType(htype))
            {
                GC.Release(address);
            }
            popBytes--;
        }
        HopperVM.BP = byte(PopCS());
        if (CSP == CSPStart)
        {
            HopperVM.PC = 0; // exit program
        }
        else
        {
            HopperVM.PC = PopCS();
        }
        return HopperVM.PC != 0;
    }
    
    bool Ret()
    {
        uint popBytes = ReadWordOperand();
        while (popBytes != 0)
        {
            Type htype;
            uint address = Pop(ref htype);
            if (IsReferenceType(htype))
            {
                GC.Release(address);
            }
            popBytes--;
        }
        HopperVM.BP = byte(PopCS());
        if (HopperVM.CSP == HopperVM.CSPStart)
        {
            PC = 0; // exit program
        }
        else
        {
            PC = PopCS();
        }
        return PC != 0;
    }
    
    bool PushGlobalB()
    {
        byte address     = byte(ReadByteOperand() + HopperVM.GP);
        Type htype;
        uint value = HopperVM.Get(address, ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }

    bool PushGlobal()
    {
        byte address     = byte(ReadWordOperand() + HopperVM.GP);
        Type htype;
        uint value = HopperVM.Get(address, ref htype);
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    
    bool PopGlobalB()
    {
        if (HopperVM.CNP)
        { 
            HopperVM.CNP=false; 
            return PopCopyGlobalB(); 
        }
        else
        {
            byte address     = byte(ReadByteOperand() + HopperVM.GP);
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadByte(TypeStackLSB + address));
            uint value;
            if (IsReferenceType(htype))
            {
                value = HopperVM.Get(address);
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            HopperVM.Put(address, value, htype);
        }
        return true;
    }

    bool PopGlobal()
    {
        if (HopperVM.CNP)
        { 
            HopperVM.CNP=false; 
            return PopCopyGlobal(); 
        }
        else
        {
            byte address     = byte(ReadWordOperand() + HopperVM.GP);
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadByte(TypeStackLSB + address));
            uint value;
            if (IsReferenceType(htype))
            {
                value = HopperVM.Get(address);
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            HopperVM.Put(address, value, htype);
        }
        return true;
    }
    
    bool PopRelB()
    {
        if (HopperVM.CNP)
        {
            HopperVM.CNP = false;
            return PopCopyRelB();
        }
        else
        {        
            int offset = ReadByteOffsetOperand();
            byte referenceAddress = byte(int(HopperVM.BP) + offset);
            Type rtype;
            byte localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
            uint existing = HopperVM.Get(localAddress, ref rtype);
            if (IsReferenceType(rtype))
            {
                GC.Release(existing);
            }
            Type vtype;
            uint value = Pop(ref vtype);
            Put(localAddress, value, vtype);
        }
        return true;
    }

    bool PopRel()
    {
        if (HopperVM.CNP)
        {
            HopperVM.CNP = false;
            return PopCopyRel();
        }
        else
        {        
            int offset = ReadWordOffsetOperand();
            byte referenceAddress = byte(int(HopperVM.BP) + offset);
            Type rtype;
            byte localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
            uint existing = HopperVM.Get(localAddress, ref rtype);
            if (IsReferenceType(rtype))
            {
                GC.Release(existing);
            }
            Type vtype;
            uint value = Pop(ref vtype);
            Put(localAddress, value, vtype);
        }
        return true;
    }
    
    bool PopCopyGlobalB()
    {
        byte address     = byte(ReadByteOperand() + HopperVM.GP);
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        uint oldvalue = HopperVM.Get(address, ref htype);
        if (IsReferenceType(htype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref htype);
        if (value == oldvalue)
        {
            // overwriting self - no more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(address, newvalue, htype); 
        }
        return true;
    }

    bool PopCopyGlobal()
    {
        byte address     = byte(ReadWordOperand() + HopperVM.GP);
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        uint oldvalue = HopperVM.Get(address, ref htype);
        if (IsReferenceType(htype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref htype);
        if (value == oldvalue)
        {
            // overwriting self - no more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(address, newvalue, htype); 
        }
        return true;
    }
    
    bool PopCopyRelB()
    {
        int  offset = ReadByteOffsetOperand();
        byte referenceAddress = byte(int(BP) + offset);
        Type rtype;
        byte localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
#ifdef CHECKED
        AssertReference(rtype, localAddress);
#endif
        // this is the slot we are about to overwrite: decrease reference count if reference type
        uint oldvalue = HopperVM.Get(localAddress, ref rtype);
        if (IsReferenceType(rtype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref rtype);
        if (value == oldvalue)
        {
            // nothing more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(localAddress, newvalue, rtype);
        }
        return true;
    }

    bool PopCopyRel()
    {
        int  offset = ReadWordOffsetOperand();
        byte referenceAddress = byte(int(BP) + offset);
        Type rtype;
        byte localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
#ifdef CHECKED
        AssertReference(rtype, localAddress);
#endif
        // this is the slot we are about to overwrite: decrease reference count if reference type
        uint oldvalue = HopperVM.Get(localAddress, ref rtype);
        if (IsReferenceType(rtype))
        {
            GC.Release(oldvalue);
        }
        uint value = Pop(ref rtype);
        if (value == oldvalue)
        {
            // nothing more to do
        }
        else
        {
            // clone self, release the original
            uint newvalue = GC.Clone(value);
            GC.Release(value);
            Put(localAddress, newvalue, rtype);
        }
        return true;
    }
    bool Die()
    {
        Type atype;
        uint err = Pop(ref atype);
#ifdef CHECKED
        AssertByte(atype, err);
#endif          
        ErrorDump(94);
        Error = byte(err);
        return false;
    }
    bool Undefined()
    {
        Runtime.Out4Hex(HopperVM.PC);
        Serial.WriteChar(':');
        Serial.WriteChar('O');
        Runtime.Out2Hex(byte(CurrentOpCode));
        Serial.WriteChar(' ');
        ErrorDump(93);
        Error = 0x0A; // not implemented
        return false;
    }
    
    bool IncGlobalBB()
    {
        byte address0     = byte(ReadByteOperand() + HopperVM.gp);
        byte address1     = byte(ReadByteOperand() + HopperVM.gp);
        Type type0;
        uint value = HopperVM.Get(address0, ref type0);
        Type type1;   
        Put(address0, value + HopperVM.Get(address1, ref type1), type0);
        return true;
    }
    
    bool IncLocalB()
    {
#ifdef CHECKED        
        int offset     = ReadByteOffsetOperand();
        
        // INCLOCALB is an optimization of "i = i + 1":
        // If it were done using ADDI or ADD, then the result pushed on the stack
        // would be tInt or tUInt, even if i was a tByte.
        // POPLOCALB would then supply the type for the resulting expression.
        //
        // So, we need to choose between tUInt and tInt for the "pop" if it was tByte .. I choose tUInt
        // (we need to avoid munting the type if it is currently a -ve tInt)
        
        Type itype;
        byte address = byte(int(HopperVM.bp) + offset);
        uint value = HopperVM.Get(address, ref itype);
        if (itype == Type.Byte)
        {
            itype = Type.UInt;
        }
        Put(address, value+1, itype);
#else
        int offset = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        }
        
        byte address = byte(int(HopperVM.bp) + offset);
        byte itype   = ReadByte(HopperVM.typeStackPage + address);
        if (itype == byte(Type.Byte))
        {
            WriteByte(HopperVM.typeStackPage + address, byte(Type.UInt));
        }
         uint value   = ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8) + 1;
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
        
#endif
        return true;
    }
      
    bool IncGlobalB()
    {
#ifdef CHECKED        
        byte address     = byte(ReadByteOperand() + GP);
        // INCGLOBALB is an optimization of "i = i + 1":
        // If it were done using ADDI or ADD, then the result pushed on the stack
        // would be tInt or tUInt, even if i was a tByte.
        // POPGLOBALB would then supply the type for the resulting expression.
        //
        // So, we need to choose between tUInt and tInt for the "pop" if it was tByte .. I choose tUInt
        // (we need to avoid munting the type if it is currently a -ve tInt)
        Type itype;
        uint value = HopperVM.Get(address, ref itype);
        if (itype == Type.Byte)
        {
            itype = Type.UInt;
        }
        Put(address, value+1, itype);
#else
        byte address     = byte(ReadProgramByte(HopperVM.pc) + HopperVM.gp);
        HopperVM.pc++;
        
        byte itype   = ReadByte(HopperVM.typeStackPage + address);
        if (itype == byte(Type.Byte))
        {
            WriteByte(HopperVM.typeStackPage + address, byte(Type.UInt));
        }
        uint value   = ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8) + 1;
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
#endif        
        return true;
    }
    
    bool DecLocalB()
    {
#ifdef CHECKED
        int offset     = ReadByteOffsetOperand();
        Type itype;
        byte address = byte(int(BP) + offset);
        uint value = HopperVM.Get(address, ref itype);
        Put(address, value-1, itype);
#else
        int offset = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        }
        
        byte address = byte(int(HopperVM.bp) + offset);
        uint value   = (ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8)) - 1;
        
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
#endif
        return true;
    }
    bool DecGlobalB()
    {
#ifdef CHECKED        
        byte address     = byte(ReadByteOperand() + GP);
        Type itype;
        uint value = HopperVM.Get(address, ref itype);
        if (itype == Type.Byte)
        {
            itype = Type.UInt;
        }
        Put(address, value-1, itype);
#else
        byte address     = byte(ReadProgramByte(HopperVM.pc) + HopperVM.gp);
        HopperVM.pc++;
        
        byte itype   = ReadByte(HopperVM.typeStackPage + address);
        if (itype == byte(Type.Byte))
        {
            WriteByte(HopperVM.typeStackPage + address, byte(Type.UInt));
        }
        
        uint value   = (ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8)) - 1;
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
#endif  
        return true;      
    }
      
    bool DecLocalIB()
    {
#ifdef CHECKED
        int offset     = ReadByteOffsetOperand();
        byte address = byte(int(BP) + offset);
        int value = HopperVM.GetI(address);
        PutI(address, value-1);
#else
        int offset = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        }     
        
        byte address = byte(int(HopperVM.bp) + offset);
        int value  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8))-1;
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
#endif
        return true;
    }
    
    bool IncLocalIB()
    {
#ifdef CHECKED
        int offset     = ReadByteOffsetOperand();
        byte address = byte(int(BP) + offset);
        int value = HopperVM.GetI(address);
        PutI(address, value+1);
#else
        int offset = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset > 127)
        {
            offset = offset - 256; // 0xFF -> -1
        } 
        
        byte address = byte(int(HopperVM.bp) + offset);
        int value  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8)) + 1;
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));        
#endif        
        return true;
    }
    
    bool IncGlobalIB()
    {
#ifdef CHECKED
        byte address     = (ReadByteOperand() + HopperVM.GP);
        int value = HopperVM.GetI(address);
        PutI(address, value+1);
#else
        byte address     = (ReadProgramByte(HopperVM.pc) + HopperVM.gp);
        HopperVM.pc++;
        
        int value  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8)) + 1; 
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
#endif
        return true;
    }
    bool DecGlobalIB()
    {
#ifdef CHECKED
        byte address     = byte(ReadByteOperand() + GP);
        int value = HopperVM.GetI(address);
        PutI(address, value-1);
#else
        byte address     = (ReadProgramByte(HopperVM.pc) + HopperVM.gp);
        HopperVM.pc++;
        
        int value  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8)) - 1; 
        WriteByte(HopperVM.valueStackLSBPage + address, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address, byte(value >> 8));
#endif
        return true;
    }
    
    bool IncLocalIBB()
    {
#ifdef CHECKED
        int offset0    = ReadByteOffsetOperand();
        int offset1    = ReadByteOffsetOperand();
        byte address0 = byte(int(BP) + offset0);
        byte address1 = byte(int(BP) + offset1);
        Type htype;
        HopperVM.Put(address0, 
                     IntToUInt(External.UIntToInt(HopperVM.Get(address0, ref htype)) + 
                               External.UIntToInt(HopperVM.Get(address1))
                              )
                     , htype
                     );
#else
        int offset0 = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset0 > 127)
        {
            offset0 = offset0 - 256; // 0xFF -> -1
        } 
        int offset1 = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset1 > 127)
        {
            offset1 = offset1 - 256; // 0xFF -> -1
        }
        byte address0 = byte(int(HopperVM.bp) + offset0);
        byte address1 = byte(int(HopperVM.bp) + offset1);
                     
        byte htype = ReadByte(HopperVM.typeStackPage + address0);
        int value0 = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address0) + (ReadByte(HopperVM.valueStackMSBPage + address0) << 8));
        int value1 = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address1) + (ReadByte(HopperVM.valueStackMSBPage + address1) << 8));
        
        value0 = value0 + value1;
        
        WriteByte(HopperVM.valueStackLSBPage + address0, byte(value0 & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address0, byte(value0 >> 8));
        WriteByte(HopperVM.typeStackPage + address0, htype);
        
#endif                     
        return true;
    }
    bool IncLocalBB()
    {
#ifdef CHECKED
        int offset0    = ReadByteOffsetOperand();
        int offset1    = ReadByteOffsetOperand();
        byte address0 = byte(int(BP) + offset0);
        byte address1 = byte(int(BP) + offset1);
        Type htype;
        HopperVM.Put(address0, HopperVM.Get(address0, ref htype) + HopperVM.Get(address1), htype);
#else
        int offset0 = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset0 > 127)
        {
            offset0 = offset0 - 256; // 0xFF -> -1
        } 
        int offset1 = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset1 > 127)
        {
            offset1 = offset1 - 256; // 0xFF -> -1
        }
        byte address0 = byte(int(HopperVM.bp) + offset0);
        byte address1 = byte(int(HopperVM.bp) + offset1);
        
        byte htype = ReadByte(HopperVM.typeStackPage + address0);
        uint value0 = ReadByte(HopperVM.valueStackLSBPage + address0) + (ReadByte(HopperVM.valueStackMSBPage + address0) << 8);
        uint value1 = ReadByte(HopperVM.valueStackLSBPage + address1) + (ReadByte(HopperVM.valueStackMSBPage + address1) << 8);
        
        value0 = value0 + value1;
        
        WriteByte(HopperVM.valueStackLSBPage + address0, byte(value0 & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address0, byte(value0 >> 8));
        WriteByte(HopperVM.typeStackPage + address0, htype);
        
#endif        
        return true;
    }   
    
    
}

unit Instructions
{
    uses "/Source/Runtime/HopperVM"
    
    uses "OpCodes"
    
    delegate bool InstructionDelegate();
    
    PopulateJumpTable(uint jumpTable)
    {
        External.SetJumpTableAddress(jumpTable);
        
        InstructionDelegate
        instructionDelegate = Instructions.Undefined;
//#ifdef CHECKED
        byte opCode;
        loop
        {
            WriteToJumpTable(OpCode(opCode), instructionDelegate);
            if (opCode == 0xFF) { break; }
            opCode++;
        }
//#endif
        WriteToJumpTable(OpCode.DIE, Instructions.Die);
        
        WriteToJumpTable(OpCode.PUSHR0, Instructions.PushR0);
        WriteToJumpTable(OpCode.POPR0, Instructions.PopR0);
        
        WriteToJumpTable(OpCode.ADD, Instructions.Add);
        WriteToJumpTable(OpCode.SUB, Instructions.Sub);
        WriteToJumpTable(OpCode.DIV, Instructions.Div);
        WriteToJumpTable(OpCode.MUL, Instructions.Mul);
        WriteToJumpTable(OpCode.MOD, Instructions.Mod);
        
        WriteToJumpTable(OpCode.EQ, Instructions.EQ);
        WriteToJumpTable(OpCode.NE, Instructions.NE);
        WriteToJumpTable(OpCode.GT, Instructions.GT);
        WriteToJumpTable(OpCode.LT, Instructions.LT);
        WriteToJumpTable(OpCode.GE, Instructions.GE);
        WriteToJumpTable(OpCode.LE, Instructions.LE);
        
        WriteToJumpTable(OpCode.BOOLOR, Instructions.BoolOr);
        WriteToJumpTable(OpCode.BOOLAND, Instructions.BoolAnd);
        WriteToJumpTable(OpCode.BITOR, Instructions.BitOr);
        WriteToJumpTable(OpCode.BITAND, Instructions.BitAnd);
        WriteToJumpTable(OpCode.BITSHL, Instructions.BitShl);
        WriteToJumpTable(OpCode.BITSHR, Instructions.BitShr);
        WriteToJumpTable(OpCode.ADDI, Instructions.AddI);
        WriteToJumpTable(OpCode.SUBI, Instructions.SubI);
        WriteToJumpTable(OpCode.DIVI, Instructions.DivI);
        WriteToJumpTable(OpCode.MULI, Instructions.MulI);
        WriteToJumpTable(OpCode.MODI, Instructions.ModI);
        WriteToJumpTable(OpCode.GTI, Instructions.GTI);
        WriteToJumpTable(OpCode.LTI, Instructions.LTI);
        WriteToJumpTable(OpCode.GEI, Instructions.GEI);
        WriteToJumpTable(OpCode.LEI, Instructions.LEI);
        WriteToJumpTable(OpCode.PUSHIB, Instructions.PushIB);
        WriteToJumpTable(OpCode.PUSHDB, Instructions.PushIB);
        WriteToJumpTable(OpCode.PUSHIBB, Instructions.PushIBB);
        WriteToJumpTable(OpCode.POPLOCALB, Instructions.PopLocalB);
        WriteToJumpTable(OpCode.PUSHLOCALB, Instructions.PushLocalB);
        WriteToJumpTable(OpCode.POPRELB, Instructions.PopRelB);
        WriteToJumpTable(OpCode.PUSHRELB, Instructions.PushRelB);
        WriteToJumpTable(OpCode.POPGLOBALB, Instructions.PopGlobalB);
        WriteToJumpTable(OpCode.PUSHGLOBALB, Instructions.PushGlobalB);
        WriteToJumpTable(OpCode.PUSHSTACKADDRB, Instructions.PushStackAddrB);
        WriteToJumpTable(OpCode.CALLB, Instructions.CallB);
        WriteToJumpTable(OpCode.JZB, Instructions.JZB);
        WriteToJumpTable(OpCode.JNZB, Instructions.JNZB);
        WriteToJumpTable(OpCode.JB, Instructions.JB);
        WriteToJumpTable(OpCode.RET0, Instructions.Ret0);
        WriteToJumpTable(OpCode.PUSHI0, Instructions.PushI0);
        WriteToJumpTable(OpCode.PUSHI1, Instructions.PushI1);
        WriteToJumpTable(OpCode.POPLOCALB00, Instructions.PopLocalB00);
        WriteToJumpTable(OpCode.POPLOCALB01, Instructions.PopLocalB01);
        WriteToJumpTable(OpCode.PUSHLOCALB00, Instructions.PushLocalB00);
        WriteToJumpTable(OpCode.PUSHLOCALB01, Instructions.PushLocalB01);
        WriteToJumpTable(OpCode.SYSCALL0, Instructions.SysCall0);
        WriteToJumpTable(OpCode.SYSCALL1, Instructions.SysCall1);
        WriteToJumpTable(OpCode.SYSCALL00, Instructions.SysCall00);
        WriteToJumpTable(OpCode.SYSCALL01, Instructions.SysCall01);
        WriteToJumpTable(OpCode.SYSCALL10, Instructions.SysCall10);
        WriteToJumpTable(OpCode.SYSCALLB0, Instructions.SysCallB0);
        WriteToJumpTable(OpCode.SYSCALLB1, Instructions.SysCallB1);
        WriteToJumpTable(OpCode.PUSHGLOBALBB, Instructions.PushGlobalBB);
        WriteToJumpTable(OpCode.PUSHLOCALBB, Instructions.PushLocalBB);
        WriteToJumpTable(OpCode.POPCOPYLOCALB, Instructions.PopCopyLocalB);
        WriteToJumpTable(OpCode.POPCOPYRELB, Instructions.PopCopyRelB);
        WriteToJumpTable(OpCode.POPCOPYGLOBALB, Instructions.PopCopyGlobalB);
        WriteToJumpTable(OpCode.POPCOPYLOCALB00, Instructions.PopCopyLocalB00);
        WriteToJumpTable(OpCode.POPCOPYLOCALB01, Instructions.PopCopyLocalB01);
        WriteToJumpTable(OpCode.ENTER, Instructions.Enter);
        WriteToJumpTable(OpCode.ENTERB, Instructions.EnterB);
#ifndef NO_JIX_INSTRUCTIONS
        WriteToJumpTable(OpCode.JIXB, Instructions.JIXB);
#endif
        WriteToJumpTable(OpCode.PUSHILE, Instructions.PushILE);
        WriteToJumpTable(OpCode.PUSHILT, Instructions.PushILT);
        WriteToJumpTable(OpCode.PUSHIBLE, Instructions.PushIBLE);
        WriteToJumpTable(OpCode.PUSHILEI, Instructions.PushILEI);
        WriteToJumpTable(OpCode.PUSHIBEQ, Instructions.PushIBEQ);
        WriteToJumpTable(OpCode.ADDB, Instructions.AddB);
        WriteToJumpTable(OpCode.SUBB, Instructions.SubB);
        WriteToJumpTable(OpCode.RETB, Instructions.RetB);
        WriteToJumpTable(OpCode.RETRESB, Instructions.RetResB);
        WriteToJumpTable(OpCode.RETFAST, Instructions.RetFast);
        WriteToJumpTable(OpCode.POPLOCAL, Instructions.PopLocal);
        WriteToJumpTable(OpCode.PUSHLOCAL, Instructions.PushLocal);
        WriteToJumpTable(OpCode.POPREL, Instructions.PopRel);
        WriteToJumpTable(OpCode.PUSHREL, Instructions.PushRel);
        WriteToJumpTable(OpCode.POPGLOBAL, Instructions.PopGlobal);
        WriteToJumpTable(OpCode.PUSHGLOBAL, Instructions.PushGlobal);
        WriteToJumpTable(OpCode.PUSHSTACKADDR, Instructions.PushStackAddr);
        WriteToJumpTable(OpCode.DUP, Instructions.Dup);
        WriteToJumpTable(OpCode.DUP0, Instructions.Dup0);
        WriteToJumpTable(OpCode.DECSP, Instructions.DecSP);
        WriteToJumpTable(OpCode.RET, Instructions.Ret);
        WriteToJumpTable(OpCode.RETRES, Instructions.RetRes);
        WriteToJumpTable(OpCode.TESTBPB, Instructions.TestBPB);
#ifndef CPU_Z80
        WriteToJumpTable(OpCode.EXIT, Instructions.Exit);
#endif        
        WriteToJumpTable(OpCode.JZ, Instructions.JZ);
        WriteToJumpTable(OpCode.JNZ, Instructions.JNZ);
        WriteToJumpTable(OpCode.JW, Instructions.J);
        WriteToJumpTable(OpCode.PUSHI, Instructions.PushIW);
        WriteToJumpTable(OpCode.PUSHD, Instructions.PushIW);
        WriteToJumpTable(OpCode.BOOLNOT, Instructions.BoolNot);
        WriteToJumpTable(OpCode.BITNOT, Instructions.BitNot);
        WriteToJumpTable(OpCode.BITSHL8, Instructions.BitShl8);
        WriteToJumpTable(OpCode.BITSHR8, Instructions.BitShr8);
        WriteToJumpTable(OpCode.BITANDFF, Instructions.BitAndFF);
        WriteToJumpTable(OpCode.SWAP, Instructions.Swap);
        WriteToJumpTable(OpCode.PUSHIM1, Instructions.PushIM1);
        WriteToJumpTable(OpCode.PUSHGP, Instructions.PushGP);
        WriteToJumpTable(OpCode.COPYNEXTPOP, Instructions.CNP);
        WriteToJumpTable(OpCode.NOP, Instructions.NOP);
        WriteToJumpTable(OpCode.CAST, Instructions.Cast);
        WriteToJumpTable(OpCode.BITXOR, Instructions.BitXor);
        WriteToJumpTable(OpCode.JREL, Instructions.JREL);
#ifndef NO_JIX_INSTRUCTIONS
        WriteToJumpTable(OpCode.JIX, Instructions.JIX);
#endif
        WriteToJumpTable(OpCode.CALL, Instructions.Call);
        WriteToJumpTable(OpCode.CALLI, Instructions.CallI);
        WriteToJumpTable(OpCode.CALLREL, Instructions.CallRel);
        WriteToJumpTable(OpCode.SYSCALL, Instructions.SysCall);
        WriteToJumpTable(OpCode.LIBCALL0, Instructions.LibCall0);
        WriteToJumpTable(OpCode.LIBCALL1, Instructions.LibCall1);
        WriteToJumpTable(OpCode.LIBCALL, Instructions.LibCall);
        WriteToJumpTable(OpCode.INCLOCALBB, Instructions.IncLocalBB);
        WriteToJumpTable(OpCode.INCLOCALIBB, Instructions.IncLocalIBB);
        WriteToJumpTable(OpCode.INCGLOBALBB, Instructions.IncGlobalBB);
        WriteToJumpTable(OpCode.INCLOCALB, Instructions.IncLocalB);
        WriteToJumpTable(OpCode.DECLOCALB, Instructions.DecLocalB);
        WriteToJumpTable(OpCode.INCGLOBALB, Instructions.IncGlobalB);
        WriteToJumpTable(OpCode.DECGLOBALB, Instructions.DecGlobalB);
        WriteToJumpTable(OpCode.INCLOCALIB, Instructions.IncLocalIB);
        WriteToJumpTable(OpCode.DECLOCALIB, Instructions.DecLocalIB);
        WriteToJumpTable(OpCode.INCGLOBALIB, Instructions.IncGlobalIB);
        WriteToJumpTable(OpCode.DECGLOBALIB, Instructions.DecGlobalIB);

        
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
        Type ntype;
        uint top;
        top = Pop(ref ttype);
        uint next = Pop(ref ntype);
        AssertUInt(ttype, top);
        AssertUInt(ntype, next);
        Push(next * top, Type.UInt); 
#else
        uint top;
        uint next;
        uint sp1;
        HopperVM.sp--;
        top   = ReadByte(HopperVM.valueStackLSBPage + HopperVM.sp) + (ReadByte(HopperVM.valueStackMSBPage + HopperVM.sp) << 8); 
        sp1   = HopperVM.sp-1;
        next  = (ReadByte(HopperVM.valueStackLSBPage + sp1) + (ReadByte(HopperVM.valueStackMSBPage + sp1) << 8)) * top; 
        
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
    bool PushR0()
    {
        Push(R0, R0Type);
        return true;
    }
    bool PopR0()
    {
        Type rtype;
        R0 = Pop(ref rtype);
        R0Type = rtype;
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
    
    bool BitAndFF()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        AssertUInt(ttype, top);
        Push(top & 0xFF, Type.UInt); 
#else
        uint sp1 = HopperVM.sp-1;
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0); // clear th MSB
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif              
        return true;
    }
    
    bool BitShl8()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        AssertUInt(ttype, top);
        Push((top & 0xFF) << 8, Type.UInt); 
#else
        uint sp1 = HopperVM.sp-1;
        uint lsb  = ReadByte(HopperVM.valueStackLSBPage + sp1); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, 0);
        WriteByte(HopperVM.valueStackMSBPage + sp1, byte(lsb));
        WriteByte(HopperVM.typeStackPage + sp1, byte(Type.UInt)); 
#endif              
        return true;
    }
    bool BitShr8()
    {
#ifdef CHECKED                
        Type ttype;
        uint top = Pop(ref ttype);
        AssertUInt(ttype, top);
        Push(top >> 8, Type.UInt); 
#else
        uint sp1 = HopperVM.sp-1;
        uint msb  = ReadByte(HopperVM.valueStackMSBPage + sp1); 
        WriteByte(HopperVM.valueStackLSBPage + sp1, byte(msb));
        WriteByte(HopperVM.valueStackMSBPage + sp1, 0);
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
    bool Dup0()
    {
        // push [top]
        byte address = byte(SP - 1);
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
        int  offset;
        byte referenceAddress;
        Type rtype;
        byte localAddress;
        uint oldvalue;
        uint value;
        uint newvalue;
        
        offset = ReadWordOffsetOperand();
        referenceAddress = byte(int(BP) + offset);
        localAddress = byte(HopperVM.Get(referenceAddress, ref rtype));
        
#ifdef CHECKED
        AssertReference(rtype, localAddress);
#endif
        // this is the slot we are about to overwrite: decrease reference count if reference type
        oldvalue = HopperVM.Get(localAddress, ref rtype);
        if (IsReferenceType(rtype))
        {
            GC.Release(oldvalue);
        }
        value = Pop(ref rtype);
        if (value == oldvalue)
        {
            // nothing more to do
        }
        else
        {
            // clone self, release the original
            newvalue = GC.Clone(value);
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
        byte address0;
        byte address1;
        Type type0;
        Type type1;   
        
        address0     = byte(ReadByteOperand() + HopperVM.gp);
        address1     = byte(ReadByteOperand() + HopperVM.gp);
        Put(address0, HopperVM.Get(address0, ref type0) + HopperVM.Get(address1, ref type1), type0);
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
        byte address;
        int value;
        
        address     = (ReadProgramByte(HopperVM.pc) + HopperVM.gp);
        HopperVM.pc++;
        
        value  = External.UIntToInt(ReadByte(HopperVM.valueStackLSBPage + address) + (ReadByte(HopperVM.valueStackMSBPage + address) << 8)) - 1; 
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
        int offset0;
        int offset1;
        byte address0;
        byte address1;
        byte htype;
        uint value;
        
        offset0 = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset0 > 127)
        {
            offset0 = offset0 - 256; // 0xFF -> -1
        } 
        offset1 = int(ReadProgramByte(HopperVM.pc)); 
        HopperVM.pc++;
        if (offset1 > 127)
        {
            offset1 = offset1 - 256; // 0xFF -> -1
        }
        address0 = byte(int(HopperVM.bp) + offset0);
        address1 = byte(int(HopperVM.bp) + offset1);
        
        htype = ReadByte(HopperVM.typeStackPage + address0);
        value  = ReadByte(HopperVM.valueStackLSBPage + address0) + (ReadByte(HopperVM.valueStackMSBPage + address0) << 8)
               + ReadByte(HopperVM.valueStackLSBPage + address1) + (ReadByte(HopperVM.valueStackMSBPage + address1) << 8);
        
        WriteByte(HopperVM.valueStackLSBPage + address0, byte(value & 0xFF));
        WriteByte(HopperVM.valueStackMSBPage + address0, byte(value >> 8));
        WriteByte(HopperVM.typeStackPage + address0, htype);
        
#endif        
        return true;
    }   
    
    
}

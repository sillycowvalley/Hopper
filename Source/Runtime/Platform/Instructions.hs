unit Instructions
{
    uses "/Source/Runtime/HopperVM"
    uses "/Source/Runtime/Platform/OpCodes"
    
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
        instructionDelegate = Instructions.InlinedAdd;
        WriteToJumpTable(jumpTable, byte(OpCode.ADD), instructionDelegate);
        
        instructionDelegate = Instructions.InlinedSub;
        WriteToJumpTable(jumpTable, byte(OpCode.SUB), instructionDelegate);
        instructionDelegate = Instructions.InlinedDiv;
        WriteToJumpTable(jumpTable, byte(OpCode.DIV), instructionDelegate);
        instructionDelegate = Instructions.InlinedMul;
        WriteToJumpTable(jumpTable, byte(OpCode.MUL), instructionDelegate);
        instructionDelegate = Instructions.InlinedMod;
        WriteToJumpTable(jumpTable, byte(OpCode.MOD), instructionDelegate);
        
        instructionDelegate = Instructions.InlinedEQ;
        WriteToJumpTable(jumpTable, byte(OpCode.EQ), instructionDelegate);
        instructionDelegate = Instructions.InlinedNE;
        WriteToJumpTable(jumpTable, byte(OpCode.NE), instructionDelegate);
        instructionDelegate = Instructions.InlinedGT;
        WriteToJumpTable(jumpTable, byte(OpCode.GT), instructionDelegate);
        instructionDelegate = Instructions.InlinedLT;
        WriteToJumpTable(jumpTable, byte(OpCode.LT), instructionDelegate);
        instructionDelegate = Instructions.InlinedGE;
        WriteToJumpTable(jumpTable, byte(OpCode.GE), instructionDelegate);
        instructionDelegate = Instructions.InlinedLE;
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
        
        instructionDelegate = Instructions.InlinedAddI;
        WriteToJumpTable(jumpTable, byte(OpCode.ADDI), instructionDelegate);
        instructionDelegate = Instructions.InlinedSubI;
        WriteToJumpTable(jumpTable, byte(OpCode.SUBI), instructionDelegate);
        instructionDelegate = Instructions.InlinedDivI;
        WriteToJumpTable(jumpTable, byte(OpCode.DIVI), instructionDelegate);
        instructionDelegate = Instructions.InlinedMulI;
        WriteToJumpTable(jumpTable, byte(OpCode.MULI), instructionDelegate);
        instructionDelegate = Instructions.InlinedModI;
        WriteToJumpTable(jumpTable, byte(OpCode.MODI), instructionDelegate);
        instructionDelegate = Instructions.InlinedGTI;
        WriteToJumpTable(jumpTable, byte(OpCode.GTI), instructionDelegate);
        instructionDelegate = Instructions.InlinedLTI;
        WriteToJumpTable(jumpTable, byte(OpCode.LTI), instructionDelegate);
        instructionDelegate = Instructions.InlinedGEI;
        WriteToJumpTable(jumpTable, byte(OpCode.GEI), instructionDelegate);
        instructionDelegate = Instructions.InlinedLEI;
        WriteToJumpTable(jumpTable, byte(OpCode.LEI), instructionDelegate);
        
        instructionDelegate = Instructions.InlinedPushIB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIB), instructionDelegate);
        instructionDelegate = Instructions.PopLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCALB), instructionDelegate);
        instructionDelegate = Instructions.InlinedPushLocalB;
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
        instructionDelegate = Instructions.IncLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCLOCALB), instructionDelegate);
        instructionDelegate = Instructions.DecLocalB;
        WriteToJumpTable(jumpTable, byte(OpCode.DECLOCALB), instructionDelegate);
        instructionDelegate = Instructions.CallB;
        WriteToJumpTable(jumpTable, byte(OpCode.CALLB), instructionDelegate);
        instructionDelegate = Instructions.InlinedJZB;
        WriteToJumpTable(jumpTable, byte(OpCode.JZB), instructionDelegate);
        instructionDelegate = Instructions.JNZB;
        WriteToJumpTable(jumpTable, byte(OpCode.JNZB), instructionDelegate);
        instructionDelegate = Instructions.JB;
        WriteToJumpTable(jumpTable, byte(OpCode.JB), instructionDelegate);
        instructionDelegate = Instructions.IncLocalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCLOCALBB), instructionDelegate);
        instructionDelegate = Instructions.Ret0;
        WriteToJumpTable(jumpTable, byte(OpCode.RET0), instructionDelegate);
        instructionDelegate = Instructions.InlinedPushI0;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHI0), instructionDelegate);
        instructionDelegate = Instructions.InlinedPushI1;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHI1), instructionDelegate);
        
        instructionDelegate = Instructions.PopLocalB00;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCALB00), instructionDelegate);
        instructionDelegate = Instructions.PopLocalB02;
        WriteToJumpTable(jumpTable, byte(OpCode.POPLOCALB02), instructionDelegate);
        instructionDelegate = Instructions.InlinedPushLocalB00;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCALB00), instructionDelegate);
        instructionDelegate = Instructions.InlinedPushLocalB02;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHLOCALB02), instructionDelegate);
        
        instructionDelegate = Instructions.SysCall0;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL0), instructionDelegate);
        instructionDelegate = Instructions.SysCall1;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL1), instructionDelegate);
        
        instructionDelegate = Instructions.PushGlobalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHGLOBALBB), instructionDelegate);
     
        instructionDelegate = Instructions.IncGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCGLOBALB), instructionDelegate);
        instructionDelegate = Instructions.DecGlobalB;
        WriteToJumpTable(jumpTable, byte(OpCode.DECGLOBALB), instructionDelegate);
        
        instructionDelegate = Instructions.IncGlobalBB;
        WriteToJumpTable(jumpTable, byte(OpCode.INCGLOBALBB), instructionDelegate);
     
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
        instructionDelegate = Instructions.PopCopyLocalB02;
        WriteToJumpTable(jumpTable, byte(OpCode.POPCOPYLOCALB02), instructionDelegate);
        
        instructionDelegate = Instructions.EnterB;
        WriteToJumpTable(jumpTable, byte(OpCode.ENTERB), instructionDelegate);
     
        instructionDelegate = Instructions.InlinedPushIB;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHDB), instructionDelegate);
        
        instructionDelegate = Instructions.JIXB;
        WriteToJumpTable(jumpTable, byte(OpCode.JIXB), instructionDelegate);
        
        instructionDelegate = Instructions.PushILE;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHILE), instructionDelegate);
        instructionDelegate = Instructions.PushILT;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHILT), instructionDelegate);
        instructionDelegate = Instructions.InlinedPushIBLE;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIBLE), instructionDelegate);
        instructionDelegate = Instructions.PushILEI;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHILEI), instructionDelegate);
        instructionDelegate = Instructions.PushIBEQ;
        WriteToJumpTable(jumpTable, byte(OpCode.PUSHIBEQ), instructionDelegate);
        
        instructionDelegate = Instructions.InlinedAddB;
        WriteToJumpTable(jumpTable, byte(OpCode.ADDB), instructionDelegate);
        instructionDelegate = Instructions.InlinedSubB;
        WriteToJumpTable(jumpTable, byte(OpCode.SUBB), instructionDelegate);
        
        instructionDelegate = Instructions.RetB;
        WriteToJumpTable(jumpTable, byte(OpCode.RETB), instructionDelegate);
        instructionDelegate = Instructions.InlinedRetResB;
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
        instructionDelegate = Instructions.InlinedEnter;
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
        instructionDelegate = Instructions.InlinedCallI;
        WriteToJumpTable(jumpTable, byte(OpCode.CALLI), instructionDelegate);
        instructionDelegate = Instructions.CallRel;
        WriteToJumpTable(jumpTable, byte(OpCode.CALLREL), instructionDelegate);
        
        instructionDelegate = Instructions.SysCall;
        WriteToJumpTable(jumpTable, byte(OpCode.SYSCALL), instructionDelegate);
        instructionDelegate = Instructions.LibCall;
        WriteToJumpTable(jumpTable, byte(OpCode.LIBCALL), instructionDelegate);
        
    }
    
    bool InlinedAdd()
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
        Push(Pop() + Pop(), Type.UInt); 
#endif
        return true;
    }
    bool InlinedSub()
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
        uint top = Pop(); Push(Pop() - top, Type.UInt);
#endif
        return true;
    }
    bool InlinedMul()
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
        Push(Pop() * Pop(), Type.UInt); 
#endif
        return true;
    }
    bool InlinedDiv()
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
        uint top = Pop(); Push(Pop() / top, Type.UInt);
#endif
        return true;
    }
    bool InlinedMod()
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
        uint top = Pop(); Push(Pop() % top, Type.UInt);
#endif
        return true;
    }
    
    bool InlinedAddI()
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
        PushI(PopI() + PopI()); 
#endif
        return true;
    }
    bool InlinedMulI()
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
        PushI(PopI() * PopI()); 
#endif
        return true;
    }
    bool InlinedSubI()
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
        int top = PopI(); PushI(PopI() - top); 
#endif
        return true;
    }
    bool InlinedDivI()
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
    bool InlinedModI()
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
    
    bool InlinedPushI0()
    {
        Push(0, Type.Byte);
        return true;
    }
    bool InlinedPushI1()
    {
        Push(1, Type.Byte);
        return true;
    }

    bool PushIM1()
    {
        PushI(-1);
        return true;
    }
    
    bool InlinedPushIB()
    {
        Push(ReadByteOperand(), Type.Byte);
        return true;
    }

    bool PushGP()
    {
        Push(0, Type.UInt);
        return true;
    }
    bool PushIW()
    {
        Push(ReadWordOperand(), Type.UInt);
        return true;
    }
    bool InlinedCallI()
    {
        uint methodAddress = ReadWordOperand();
        PushCS(PC);
        PC = methodAddress;
        return true;
    }
    
    bool PushLocalBB()
    {
        bool res = InlinedPushLocalB();
        return InlinedPushLocalB();
    }
    bool PushGlobalBB()
    {
        bool res = PushGlobalB();
        return PushGlobalB();
    }
    
    bool InlinedPushLocalB()
    {
        int offset = ReadByteOffsetOperand();
        uint value =      ReadWord(uint(int(ValueStack) + int(BP) + offset));
        Type htype = Type(ReadWord(uint(int(TypeStack)  + int(BP) + offset)));
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }

    bool PushLocal()
    {
        int offset = ReadWordOffsetOperand();
        uint value =      ReadWord(uint(int(ValueStack) + int(BP) + offset));
        Type htype = Type(ReadWord(uint(int(TypeStack)  + int(BP) + offset)));
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    
    bool InlinedPushLocalB00()
    {
        uint value = ReadWord(uint(int(ValueStack)     + int(BP)));
        Type htype = Type(ReadWord(uint(int(TypeStack) + int(BP))));
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    bool InlinedPushLocalB02()
    {
        uint value = ReadWord(uint(int(ValueStack)     + int(BP) + 2));
        Type htype = Type(ReadWord(uint(int(TypeStack) + int(BP) + 2)));
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }

    bool InlinedEnter()
    {
        PushCS(BP);
        BP = SP;
        return true;
    }
    bool InlinedJZB()
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
        if (Pop() == 0)
        {
            PC = uint(ReadByteOffsetOperand() + int(PC-2));
        }
        else
        {
            PC = PC + 1;
        }
#endif   
        return true;
    }
    bool Exit()
    {
        return HopperVM.ExitInline();
    }
    
    bool InlinedRetResB()
    {
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
            popBytes = popBytes - 2;
        }
        Push(value, rtype);
        BP = PopCS();
        if (CSP == 0)
        {
            PC = 0; // exit program
            return false;
        }
        else
        {
            PC = PopCS();
        }
        return true;
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
            popBytes = popBytes - 2;
        }
        Push(value, rtype);
        BP = PopCS();
        if (CSP == 0)
        {
            PC = 0; // exit program
            return false;
        }
        else
        {
            PC = PopCS();
        }
        return true;
    }
    bool NOP()
    {
        return true;
    }
    bool Cast()
    {
        HopperVM.Put(SP-2, HopperVM.Get(SP-2), Type(ReadByteOperand()));
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
        Push((Pop() == 0) ? 1 : 0, Type.Bool);
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
        Push(~(Pop()), Type.UInt); 
#endif              
        return true;
    }
    bool BoolAnd()
    {
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        AssertBool(ttype, top);
        AssertBool(ntype, next);
#endif
        // be aware of short circuit boolean evaluation here - need both Pops..
        Push(((next != 0) && (top != 0)) ? 1 : 0, Type.Bool); 
        return true;
    }
    bool BoolOr()
    {
        Type ttype;
        uint top = Pop(ref ttype);
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        AssertBool(ttype, top);
        AssertBool(ntype, next);
#endif              
        // be aware of short circuit boolean evaluation here - need both Pops..
        Push(((next != 0) || (top != 0)) ? 1 : 0, Type.Bool); 
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
        Push(Pop() & Pop(), Type.UInt); 
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
        Push(Pop() | Pop(), Type.UInt); 
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
#else
        uint top = Pop();
        uint next = Pop();
#endif              
        Push((next | top) & (~(next & top)), Type.UInt); 
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
#else
        uint top = Pop();
        uint next = Pop();
#endif              
        Push(next << top, Type.UInt); 
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
#else
        uint top = Pop();
        uint next = Pop();
#endif              
        Push(next >> top, Type.UInt); 
        return true;
    }
    
    bool IncLocalB()
    {
        int offset     = ReadByteOffsetOperand();
        
        // INCLOCALB is an optimization of "i = i + 1":
        // If it were done using ADDI or ADD, then the result pushed on the stack
        // would be tInt or tUInt, even if i was a tByte.
        // POPLOCALB would then supply the type for the resulting expression.
        //
        // So, we need to choose between tUInt and tInt for the "pop" if it was tByte .. I choose tUInt
        // (we need to avoid munting the type if it is currently a -ve tInt)
        
        Type itype;
        uint address = uint(int(BP) + offset);
        uint value = HopperVM.Get(address, ref itype);
        if (itype == Type.Byte)
        {
            itype = Type.UInt;
        }
        Put(address, value+1, itype);
        return true;
    }
    bool IncGlobalB()
    {
        uint address     = ReadByteOperand();
        
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
        return true;
    }
    bool IncGlobalBB()
    {
        uint address0     = ReadByteOperand();
        uint address1     = ReadByteOperand();
     
        Type type0;
        uint value = HopperVM.Get(address0, ref type0);
        
        Type type1;   
        Put(address0, value + HopperVM.Get(address1, ref type1), type0);
        return true;
    }
    bool DecGlobalB()
    {
        uint address     = ReadByteOperand();
        
        Type itype;
        uint value = HopperVM.Get(address, ref itype);
        if (itype == Type.Byte)
        {
            itype = Type.UInt;
        }
        Put(address, value-1, itype);
        return true;
    }
    bool PopLocalB00()
    {
        if (CNP) 
        { 
            CNP = false;
            return PopCopyLocalB00();
        }
        else
        {
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadWord(uint(int(TypeStack) + int(BP))));
            uint value;
            if (IsReferenceType(htype))
            {
                value = ReadWord(uint(int(ValueStack)     + int(BP)));
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            WriteWord(uint(int(ValueStack) + int(BP)), value);
            WriteWord(uint(int(TypeStack)  + int(BP)), uint(htype));
        }
        return true;
    }
    bool PopLocalB02()
    {
        if (CNP) 
        { 
            CNP = false;
            return PopCopyLocalB02();
        }
        else
        {
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadWord(uint(int(TypeStack) + int(BP) + 2)));
            uint value;
            if (IsReferenceType(htype))
            {
                value = ReadWord(uint(int(ValueStack)     + int(BP) + 2));
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            WriteWord(uint(int(ValueStack) + int(BP) + 2), value);
            WriteWord(uint(int(TypeStack)  + int(BP) + 2), uint(htype));
        }
        return true;
    }
    bool PopLocalB()
    {
        int offset;
        if (CNP) 
        { 
            CNP = false;
            return PopCopyLocalB();
        }
        else
        {
            int offset     = ReadByteOffsetOperand();
        
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadWord(uint(int(TypeStack) + int(BP) + offset)));
            uint value;
            if (IsReferenceType(htype))
            {
                value = ReadWord(uint(int(ValueStack)     + int(BP) + offset));
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            WriteWord(uint(int(ValueStack) + int(BP) + offset), value);
            WriteWord(uint(int(TypeStack)  + int(BP) + offset), uint(htype));
        }
        return true;
    }

    bool PopLocal()
    {
        int offset;
        if (CNP) 
        { 
            CNP = false;
            return PopCopyLocal();
        }
        else
        {
            int offset     = ReadWordOffsetOperand();
        
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadWord(uint(int(TypeStack) + int(BP) + offset)));
            uint value;
            if (IsReferenceType(htype))
            {
                value = ReadWord(uint(int(ValueStack)     + int(BP) + offset));
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            WriteWord(uint(int(ValueStack) + int(BP) + offset), value);
            WriteWord(uint(int(TypeStack)  + int(BP) + offset), uint(htype));
        }
        return true;
    }
    
    bool PopCopyLocalB00()
    {
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        uint localAddress = uint(int(BP));
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
    bool PopCopyLocalB02()
    {
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        uint localAddress = uint(int(BP) + 2);
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
        uint localAddress = uint(int(BP) + offset);
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
        uint localAddress = uint(int(BP) + offset);
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
    bool DecLocalB()
    {
        int offset     = ReadByteOffsetOperand();
        Type itype;
        uint address = uint(int(BP) + offset);
        uint value = HopperVM.Get(address, ref itype);
        Put(address, value-1, itype);
        return true;
    }
    bool IncLocalBB()
    {
        int offset0    = ReadByteOffsetOperand();
        int offset1    = ReadByteOffsetOperand();
        uint address0 = uint(int(ValueStack) + int(BP) + offset0);
        uint address1 = uint(int(ValueStack) + int(BP) + offset1);
        WriteWord(address0, ReadWord(address0) + ReadWord(address1));
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
            offset = ReadCodeByte(index);
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
            offset = ReadCodeByte(index) + (ReadCodeByte(index+1) << 8);
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
    bool InlinedEQ()
    {
        Push((Pop() == Pop()) ? 1 : 0, Type.Bool);
        return true;
    }
    bool InlinedNE()
    {
        Push((Pop() != Pop()) ? 1 : 0, Type.Bool);
        return true;
    }
    bool InlinedLT()
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
        Push((Pop() > Pop()) ? 1 : 0, Type.Bool);
#endif
        return true;
    }
    bool InlinedGT()
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
        Push((Pop() < Pop()) ? 1 : 0, Type.Bool);
#endif        
        return true;        
    }
    bool InlinedLE()
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
        Push((Pop() >= Pop()) ? 1 : 0, Type.Bool);
#endif 
        return true;               
    }
    bool InlinedGE()
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
        Push((Pop() <= Pop()) ? 1 : 0, Type.Bool);
#endif      
        return true;          
    }
    bool InlinedLTI()
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
        Push((PopI() > PopI()) ? 1 : 0, Type.Bool);
#endif   
        return true;             
    }
    bool InlinedGTI()
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
        Push((PopI() < PopI()) ? 1 : 0, Type.Bool);
#endif     
        return true;           
    }
    bool InlinedLEI()
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
        Push((PopI() >= PopI()) ? 1 : 0, Type.Bool);
#endif 
        return true;               
    }
    bool InlinedGEI()
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
        Push((PopI() <= PopI()) ? 1 : 0, Type.Bool);
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
            PC = uint(int(PC-2) + offset);
        }
#else
        if (Pop() != 0)
        {
            PC = uint(ReadByteOffsetOperand() + int(PC-2));
        }
        else
        {
            PC = PC + 1;
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
            PC = uint(int(PC-3) + offset);
        }
#else
        if (Pop() == 0)
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
    bool LibCall()
    {
        byte iLibCall = ReadByteOperand();  
        return ExecuteLibCall(iLibCall);
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

    bool SysCall()
    {
        Type htype;
        uint iOverload = Pop(ref htype);
        byte iSysCall  = ReadByteOperand();  
        return ExecuteSysCall(iSysCall, iOverload);
    }

    bool PushILT()
    {
        uint top = ReadWordOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        AssertUInt(ntype, next);
#endif  
        Push((next < top) ? 1 : 0, Type.Bool);
        return true;
    }
    bool PushILE()
    {
        uint top = ReadWordOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        AssertUInt(ntype, next);
#endif  
        Push((next <= top) ? 1 : 0, Type.Bool);
        return true;
    }
    bool PushILEI()
    {
        Push(ReadWordOperand(), Type.UInt);    
        Type ttype;
        int top  = PopI(ref ttype);
        Type ntype;
        int next = PopI(ref ntype);
#ifdef CHECKED
        AssertInt(ttype);
        AssertInt(ntype);
#endif  
        Push((next <= top) ? 1 : 0, Type.Bool);
        return true;
    }
    
    bool InlinedPushIBLE()
    {
        uint top = ReadByteOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        AssertUInt(ntype, next);
#endif  
        Push((next <= top) ? 1 : 0, Type.Bool);
        return true;
    }
    bool PushIBEQ()
    {
        uint top = ReadByteOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        // AssertUInt(ntype, next); - there is no EQI opcode
#endif  
        Push((next == top) ? 1 : 0, Type.Bool);
        return true;
    }
    bool InlinedAddB()
    {
        uint top = ReadByteOperand();    
        Type ntype;
        uint next = Pop(ref ntype);
#ifdef CHECKED
        AssertUInt(ntype, next);
#endif  
        Push(next + top, Type.UInt);
        return true;
    }
    bool InlinedSubB()
    {
        uint top = ReadCodeByte(PC); PC = PC + 1;
#ifdef CHECKED
        Type ntype;
        uint next = Pop(ref ntype);
        AssertUInt(ntype, next);
        Push(next - top, Type.UInt);
#else
        uint sp2 = SP - 2;
        WriteWord(ValueStack + sp2, ReadWord(ValueStack + sp2) - top);
        WriteWord(TypeStack  + sp2, byte(Type.UInt));
#endif  
        return true;
    }
    
    bool PushRelB()
    {
        int  offset = ReadByteOffsetOperand();
        uint referenceAddress = uint(int(BP) + offset);
        Type rtype;
        uint localAddress = HopperVM.Get(referenceAddress, ref rtype);
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
        uint referenceAddress = uint(int(BP) + offset);
        Type rtype;
        uint localAddress = HopperVM.Get(referenceAddress, ref rtype);
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
        uint address = uint(int(BP) + offset);
        Push(address, Type.Reference);
        return true;
    }
    bool PushStackAddr()
    {
        int  offset = ReadWordOffsetOperand();
        uint address = uint(int(BP) + offset);
        Push(address, Type.Reference);
        return true;
    }
    bool Swap()
    {
        uint topValue  = ReadWord(ValueStack + SP - 2);
        uint nextValue = ReadWord(ValueStack + SP - 4);
        WriteWord(ValueStack + SP - 2, nextValue);
        WriteWord(ValueStack + SP - 4, topValue);
        uint topType  = ReadWord(TypeStack + SP - 2);
        uint nextType = ReadWord(TypeStack + SP - 4);
        WriteWord(TypeStack + SP - 2, nextType);
        WriteWord(TypeStack + SP - 4, topType);
        return true;
    }
    bool Dup()
    {
        // operand is offset 0..255 into stack where 0=[top], 1=[next], etc
        byte  offset  = ReadByteOperand();
        uint address = SP - 2 - offset;
        uint value = ReadWord(ValueStack     + address);
        Type htype = Type(ReadWord(TypeStack + address));
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
        uint bpExpected = uint(SP - operand);
        if (bpExpected != BP)
        {
            Error = 0x0B;
            return false;
        }
        return true;
    }
    
    bool CallB()
    {
        uint methodIndex = ReadByteOperand();
        PushCS(PC);
        PC = LookupMethod(methodIndex);
        return true;
    }

    bool Call()
    {
        uint methodIndex = ReadWordOperand();
        PushCS(PC);
        uint methodAddress = LookupMethod(methodIndex);
        WriteCodeByte(PC-3, byte(OpCode.CALLI));
        WriteCodeWord(PC-2, methodAddress);
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
        PushCS(PC);
        PC = LookupMethod(methodIndex);
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
            popBytes = popBytes - 2;
        }
        return true;
    }
    
    bool RetFast()
    {
        PC = PopCS();
        return true;
    }
    bool Ret0()
    {
        BP = PopCS();
        if (CSP == 0)
        {
            PC = 0; // exit program
            return false;
        }
        else
        {
            PC = PopCS();
        }
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
            popBytes = popBytes - 2;
        }
        BP = PopCS();
        if (CSP == 0)
        {
            PC = 0; // exit program
            return false;
        }
        else
        {
            PC = PopCS();
        }
        return true;
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
            popBytes = popBytes - 2;
        }
        BP = PopCS();
        if (CSP == 0)
        {
            PC = 0; // exit program
            return false;
        }
        else
        {
            PC = PopCS();
        }
        return true;
    }
    
    bool EnterB()
    {
        PushCS(BP);
        BP = SP;
        uint zeros = ReadByteOperand();
        for (uint i = 0; i < zeros; i++)
        {
            Push(0, Type.Byte);
        }
        return true;
    }
    bool PushGlobalB()
    {
        byte offset     = ReadByteOperand();
        uint value = ReadWord(ValueStack + offset);
        Type htype = Type(ReadWord(TypeStack + offset));
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }

    bool PushGlobal()
    {
        uint offset     = ReadWordOperand();
        uint value = ReadWord(ValueStack + offset);
        Type htype = Type(ReadWord(TypeStack + offset));
        Push(value, htype);
        if (IsReferenceType(htype))
        {
            GC.AddReference(value);
        }
        return true;
    }
    
    bool PopGlobalB()
    {
        if (CNP)
        { 
            CNP=false; 
            return PopCopyGlobalB(); 
        }
        else
        {
            byte offset     = ReadByteOperand();
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadWord(TypeStack  + offset));
            uint value;
            if (IsReferenceType(htype))
            {
                value = ReadWord(ValueStack  + offset);
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            WriteWord(ValueStack + offset, value);
            WriteWord(TypeStack  + offset, uint(htype));
        }
        return true;
    }

    bool PopGlobal()
    {
        if (CNP)
        { 
            CNP=false; 
            return PopCopyGlobal(); 
        }
        else
        {
            uint offset     = ReadWordOperand();
            // this is the slot we are about to overwrite: decrease reference count if reference type
            Type htype = Type(ReadWord(TypeStack  + offset));
            uint value;
            if (IsReferenceType(htype))
            {
                value = ReadWord(ValueStack  + offset);
                GC.Release(value);
            }
            
            value = Pop(ref htype);
            WriteWord(ValueStack + offset, value);
            WriteWord(TypeStack  + offset, uint(htype));
        }
        return true;
    }
    
    bool PopRelB()
    {
        if (CNP)
        {
            CNP = false;
            return PopCopyRelB();
        }
        else
        {        
            int offset = ReadByteOffsetOperand();
            uint referenceAddress = uint(int(BP) + offset);
            Type rtype;
            uint localAddress = HopperVM.Get(referenceAddress, ref rtype);
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
        if (CNP)
        {
            CNP = false;
            return PopCopyRel();
        }
        else
        {        
            int offset = ReadWordOffsetOperand();
            uint referenceAddress = uint(int(BP) + offset);
            Type rtype;
            uint localAddress = HopperVM.Get(referenceAddress, ref rtype);
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
        byte offset     = ReadByteOperand();
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        uint oldvalue = HopperVM.Get(offset, ref htype);
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
            Put(offset, newvalue, htype); 
        }
        return true;
    }

    bool PopCopyGlobal()
    {
        uint offset     = ReadWordOperand();
        // this is the slot we are about to overwrite: decrease reference count if reference type
        Type htype;
        uint oldvalue = HopperVM.Get(offset, ref htype);
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
            Put(offset, newvalue, htype); 
        }
        return true;
    }
    
    bool PopCopyRelB()
    {
        int  offset = ReadByteOffsetOperand();
        uint referenceAddress = uint(int(BP) + offset);
        Type rtype;
        uint localAddress = HopperVM.Get(referenceAddress, ref rtype);
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
        uint referenceAddress = uint(int(BP) + offset);
        Type rtype;
        uint localAddress = HopperVM.Get(referenceAddress, ref rtype);
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
    bool Undefined()
    {
        Runtime.Out4Hex(PC);
        Serial.WriteChar(':');
        Serial.WriteChar('O');
        Runtime.Out2Hex(byte(CurrentOpCode));
        Serial.WriteChar(' ');
        ErrorDump(93);
        Error = 0x0A; // not implemented
        return false;
    }
}

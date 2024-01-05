#include "Common.h"

typedef Bool (*OpCodeMethod)();

OpCodeMethod opCodeJumps[256];

bool undefined()
{
    printf("\nundefined opcode at 0x%04X", GetPC()-1);
    SetError(0x0A, (14));
    return false;
}
bool nop()
{
    return true;
}



bool pushI()
{
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    *top = VMReadWordOperand();
    dataMemoryBlock[typeStack + sp] = Type::eUInt;
    sp += 2;
    return true;
}
bool pushIB()
{
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    *top = codeMemoryBlock[pc]; pc++;
    dataMemoryBlock[typeStack + sp] = Type::eByte;
    sp += 2;
    return true;
}
bool pushI0()
{
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    *top = 0;
    dataMemoryBlock[typeStack + sp] = Type::eByte;
    sp += 2;
    return true;
}
bool pushI1()
{
    Long * top  = (Long*)&dataMemoryBlock[valueStack + (sp << 1)];
    *top = 1;
    dataMemoryBlock[typeStack + sp] = Type::eByte;
    sp += 2;
    return true;
}
bool pushIM1()
{
    VMPush(0xFFFF, Type::eInt);
    return true;
}
Bool pushStackAddr()
{
    Int offset = VMReadWordOffsetOperand();
    UInt address = UInt(Int(GetBP()) + offset);
    VMPush(address, Type::eReference);
    return true;
}
Bool pushStackAddrB()
{
    Int offset = VMReadByteOffsetOperand();
    UInt address = UInt(Int(GetBP()) + offset);
    VMPush(address, Type::eReference);
    return true;
}
Bool pushRel()
{
    Int offset = VMReadWordOffsetOperand();
    UInt referenceAddress = UInt(Int(GetBP()) + offset);
    Type rtype = (Type)0;
    UInt localAddress = VMGet(referenceAddress, rtype);
    UInt value        = VMGet(localAddress, rtype);
    VMPush(value, rtype);
    if (IsReferenceType(rtype))
    {
        GC_AddReference(value);
    }
    return true;
}
Bool pushRelB()
{
    Int offset = VMReadByteOffsetOperand();
    UInt referenceAddress = UInt(Int(GetBP()) + offset);
    Type rtype = (Type)0;
    UInt localAddress = VMGet(referenceAddress, rtype);
    UInt value        = VMGet(localAddress, rtype);
    VMPush(value, rtype);
    if (IsReferenceType(rtype))
    {
        GC_AddReference(value);
    }
    return true;
}
bool pushLocalShared(Int operand)
{
    const UInt offset = (UInt)(operand + bp);
    const Type htype = (Type)dataMemoryBlock[typeStack + offset];
    if (IsReferenceType(htype))
    {
        const UInt value = Memory_ReadWord(valueStack + (offset << 1));
        VMPush(value, htype);
        GC_AddReference(value);
    }
    else
    {
        memcpy(&dataMemoryBlock[valueStack + (sp << 1)], &dataMemoryBlock[valueStack + (offset << 1)], 4);
        dataMemoryBlock[typeStack + sp] = htype;
        sp += 2;
    }
    return true;
}

bool pushLocal()
{
    return pushLocalShared(VMReadWordOffsetOperand());
}
bool pushLocalB()
{
    Int offset = (Int)(codeMemoryBlock[pc]); pc++; if (offset > 0x7F) { offset = offset - 0x0100; }
    return pushLocalShared(offset);
}
bool pushLocalBB()
{
    pushLocalShared(VMReadByteOffsetOperand());
    return pushLocalShared(VMReadByteOffsetOperand());
}
bool pushLocalB00()
{
    return pushLocalShared(0);
}
bool pushLocalB02()
{
    return pushLocalShared(2);
}
bool popCopyLocalShared(Int operand)
{
    Int offset = operand;
    Type htype = (Type)0;
    UInt localAddress = UInt(Int(GetBP()) + offset);
    UInt oldvalue = VMGet(localAddress, htype);
    if (IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt32 value = VMPop32(htype);
    if ((htype == Type::eLong) || (htype == Type::eFloat))
    {
        VMPut32(localAddress, value, htype);
        value = VMGet32(localAddress, htype);
    }
    else if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        VMPut(localAddress, newvalue, htype);
    }
    return true;
}
bool popCopyLocal()
{
    return popCopyLocalShared(VMReadWordOffsetOperand());
}
bool popCopyLocalB()
{
    return popCopyLocalShared(VMReadByteOffsetOperand());
}
bool popCopyLocalB00()
{
    return popCopyLocalShared(0);
}
bool popCopyLocalB02()
{
    return popCopyLocalShared(2);
}
bool popLocalShared(Int operand)
{
    if (GetCNP())
    {
        SetCNP(false);
        return popCopyLocalShared(operand);
    }
    Int offset = operand + GetBP();
    UInt stackAddress = GetValueStack();
    Type htype = (Type)Memory_ReadWord(UInt(GetTypeStack() + offset));
    if ((htype == Type::eLong) || (htype == Type::eFloat))
    {
        UInt32 value = VMPop32(htype);
        Memory_WriteWord(UInt(stackAddress + (offset * 2)), value & 0xFFFF);
        Memory_WriteWord(UInt(stackAddress + (offset * 2) + 2), value >> 16);
    }
    else
    {
        UInt value = 0;
        if (IsReferenceType(htype))
        {
            value = Memory_ReadWord(UInt(stackAddress + (offset * 2)));
            GC_Release(value);
        }
        value = VMPop(htype);
        Memory_WriteWord(UInt(stackAddress + (offset * 2)), value);
    }
    Memory_WriteWord(UInt(GetTypeStack() + offset), UInt(htype));
    return true;
}
bool popLocal()
{
    return popLocalShared(VMReadWordOffsetOperand());
}
bool popLocalB()
{
    return popLocalShared(VMReadByteOffsetOperand());
}
bool popLocalB00()
{
    return popLocalShared(0);
}
bool popLocalB02()
{
    return popLocalShared(2);
}


bool popCopyRelShared(Int offset)
{
    UInt referenceAddress = UInt(Int(GetBP()) + offset);
    Type rtype = (Type)0;
    UInt localAddress = VMGet(referenceAddress, rtype);
    UInt oldvalue = VMGet(localAddress, rtype);
    if (IsReferenceType(rtype))
    {
        GC_Release(oldvalue);
    }
    UInt32 value = VMPop32(rtype);
    if ((rtype == Type::eLong) || (rtype == Type::eFloat))
    {
        VMPut32(localAddress, value, rtype);
    }
    else if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        VMPut(localAddress, newvalue, rtype);
    }
    return true;
}
bool popCopyRel()
{
    return popCopyRelShared(VMReadWordOffsetOperand());
}
bool popCopyRelB()
{
    return popCopyRelShared(VMReadByteOffsetOperand());
}
bool popRelShared(Int offset)
{
    if (GetCNP())
    {
        SetCNP(false);
        return popCopyRelShared(offset);
    }
    UInt referenceAddress = UInt(Int(GetBP()) + offset);
    Type rtype = (Type)0;
    UInt localAddress = VMGet(referenceAddress, rtype);
    UInt existing = VMGet(localAddress, rtype);
    if (IsReferenceType(rtype))
    {
        GC_Release(existing);
    }
    Type vtype = (Type)0;
    UInt32 value = VMPop32(vtype);
    VMPut32(localAddress, value, vtype);
    return true;
}
bool popRel()
{
    return popRelShared(VMReadWordOffsetOperand());
}
bool popRelB()
{
    return popRelShared(VMReadByteOffsetOperand());
}

bool pushGlobalShared(UInt offset)
{
    UInt stackAddress = GetValueStack();
    Type htype = (Type)Memory_ReadWord(GetTypeStack() + offset);
    if ((htype == Type::eLong) || (htype == Type::eFloat))
    {
        UInt32 value = Memory_ReadWord(stackAddress + (offset * 2)) + (Memory_ReadWord(stackAddress + (offset * 2) + 2) << 16);
        VMPush32(value, htype);
    }
    else
    {
        UInt value = Memory_ReadWord(stackAddress + (offset * 2));
        VMPush(value, htype);
        if (IsReferenceType(htype))
        {
            GC_AddReference(value);
        }
    }
    return true;
}
bool pushGlobal()
{
    return pushGlobalShared(VMReadWordOperand());
}
bool pushGlobalB()
{
    return pushGlobalShared(VMReadByteOperand());
}

bool copyNextPop()
{
    SetCNP(true);
    return true;
}

bool enter()
{
    UInt * slot = (UInt*)&dataMemoryBlock[callStack + csp];
    *slot = bp;
    csp += 2;
    
    bp = sp;
    return true;
}
bool enterB()
{
    VMPushCS(bp);
    bp = sp;
    UInt zeros = codeMemoryBlock[pc]; pc++;
    for (UInt i = 0x00; i < zeros; i++)
    {
        VMPush(0, Type::eByte);
    }
    return true;
}

Bool call()
{
    UInt methodIndex = VMReadWordOperand();
    UInt pc = GetPC();
    VMPushCS(pc);
    UInt methodAddress = VMLookupMethod(methodIndex);
    
    Memory_WriteCodeByte(pc - 0x03, Byte(OpCode::eCALLI));
    Memory_WriteCodeByte(pc - 0x02, methodAddress & 0xFF);
    Memory_WriteCodeByte(pc - 0x01, methodAddress >> 8);
    SetPC(methodAddress);
    return true;
}

Bool callRel()
{
    UInt methodIndex = VMPop();
    UInt methodAddress = VMLookupMethod(methodIndex);
    VMPushCS(pc);
    pc = methodAddress;
    return true;
}

Bool callI()
{
    UInt * slot = (UInt*)&dataMemoryBlock[callStack + csp];
    *slot = pc+2;
    csp += 2;
    
    pc = codeMemoryBlock[pc] + (codeMemoryBlock[pc+1] << 8);
    return true;
}

Bool retShared(UInt popBytes)
{
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = VMPop(htype);
        if (IsReferenceType(htype))
        {
            GC_Release(address);
        }
        popBytes = popBytes - 0x02;
    }
    
    csp -= 2;
    UInt * slot = (UInt*)&dataMemoryBlock[callStack + csp];
    bp = *slot;
    
    if (csp == 0)
    {
        pc = 0;
        return false;
    }
    else
    {
        csp -= 2;
        slot = (UInt*)&dataMemoryBlock[callStack + csp];
        pc = *slot;
    }
    return true;
}
Bool ret()
{
    return retShared(VMReadWordOperand());
}
Bool retB()
{
    return retShared(VMReadByteOperand());
}
Bool ret0()
{
    return retShared(0);
}
Bool retresShared(UInt popBytes)
{
    sp -= 2;
    Byte * top = (Byte*)&dataMemoryBlock[valueStack + (sp << 1)];
    Type rtype = (Type)dataMemoryBlock[typeStack + sp];
    while (popBytes != 0)
    {
        if (IsReferenceType(dataMemoryBlock[typeStack + sp - 2]))
        {
            UInt address = VMPop();
            GC_Release(address);
        }
        else
        {
            sp -= 2;
        }
        popBytes -= 2;
    }
    
    memcpy(&dataMemoryBlock[valueStack + (sp << 1)], top, 4);
    dataMemoryBlock[typeStack + sp] = rtype;
    sp += 2;
    
    csp -= 2;
    UInt * slot = (UInt*)&dataMemoryBlock[callStack + csp];
    bp = *slot;
    
    if (csp == 0)
    {
        pc = 0;
        return false;
    }
    else
    {
        csp -= 2;
        UInt * slot = (UInt*)&dataMemoryBlock[callStack + csp];
        pc = *slot;
    }
    return true;
}
Bool retres()
{
    return retresShared(VMReadWordOperand());
}
Bool retresB()
{
    return retresShared(VMReadByteOperand());
}

Bool decSP()
{
    UInt popBytes = codeMemoryBlock[pc]; pc++;
    while (popBytes != 0)
    {
        if (IsReferenceType(dataMemoryBlock[typeStack + sp - 2]))
        {
            UInt address = VMPop();
            GC_Release(address);
        }
        else
        {
            sp -= 2;
        }
        popBytes -= 2;
    }
    return true;
}

Bool swap()
{
    Type ttype;
    Type ntype;
    UInt32 top  = VMGet32(sp-2, ttype);
    UInt32 next = VMGet32(sp-4, ntype);
    //printf("\nswap: 0x%08X:0x%02X 0x%08X:0x%02X", next, ntype, top, ttype);
    
    VMPut32(sp-4, top,  ttype);
    VMPut32(sp-2, next, ntype);
    
    //top  = VMGet32(GetSP()-2, ttype);
    //next = VMGet32(GetSP()-4, ntype);
    //printf("\nswap: 0x%08X:0x%02X 0x%08X:0x%02X", next, ntype, top, ttype);
    
    return true;
}
Bool cast()
{
    // assumptions:
    // - msb of type  is zero
    Memory_WriteByte(typeStack  + sp - 2, (Type)codeMemoryBlock[pc]); pc++;
    return true;
}


bool jz()
{
    if (VMPop() == 0x00)
    {
        SetPC((UInt)(VMReadWordOffsetOperand() + (Int)(GetPC() - 0x03)));
    }
    else
    {
        pc += 2;
    }
    return true;
}
bool jnz()
{
    if (VMPop() != 0x00)
    {
        SetPC((UInt)(VMReadWordOffsetOperand() + (Int)(GetPC() - 0x03)));
    }
    else
    {
        pc += 2;
    }
    return true;
}
bool jw()
{
    SetPC((UInt)(VMReadWordOffsetOperand() + (Int)(GetPC() - 0x03)));
    return true;
}
bool jzb()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    if (*top == 0)
    {
        SetPC((UInt)(VMReadByteOffsetOperand() + (Int)(GetPC() - 0x02)));
    }
    else
    {
        pc++;
    }
    return true;
}
bool jnzb()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    if (*top != 0)
    {
        SetPC((UInt)(VMReadByteOffsetOperand() + (Int)(GetPC() - 0x02)));
    }
    else
    {
        pc++;
    }
    return true;
}
bool jb()
{
    SetPC((UInt)(VMReadByteOffsetOperand() + (Int)(GetPC() - 0x02)));
    return true;
}

bool eq()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next == *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool ne()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next != *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool lt()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next < *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool le()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next <= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool gt()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next > *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool ge()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next >= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}

bool lti()
{
    sp -= 2;
    Int * top  = (Int*)&dataMemoryBlock[valueStack + (sp << 1)];
    Int * next = (Int*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next < *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool lei()
{
    sp -= 2;
    Int * top  = (Int*)&dataMemoryBlock[valueStack + (sp << 1)];
    Int * next = (Int*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next <= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool gti()
{
    sp -= 2;
    Int * top  = (Int*)&dataMemoryBlock[valueStack + (sp << 1)];
    Int * next = (Int*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next > *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool gei()
{
    sp -= 2;
    Int * top  = (Int*)&dataMemoryBlock[valueStack + (sp << 1)];
    Int * next = (Int*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next >= *top) ? 1 : 0;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}


bool add()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = *next + *top;
    dataMemoryBlock[typeStack + sp-2] = Type::eUInt;
    return true;
}
bool sub()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = *next - *top;
    dataMemoryBlock[typeStack + sp-2] = Type::eUInt;
    return true;
}
bool mul()
{
    sp -= 2;
    UInt * top  = (UInt*)&dataMemoryBlock[valueStack + (sp << 1)];
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = *next * *top;
    dataMemoryBlock[typeStack + sp-2] = Type::eUInt;
    return true;
}
bool div()
{
    UInt top  = VMPop();
    if (top == 0)
    {
        SetError(0x04, (16));
        return false;
    }
    UInt address = valueStack + ((sp-2) << 1);
    Memory_WriteWord(address, Memory_ReadWord(address) / top);
    Memory_WriteByte(typeStack  + sp - 2, Type::eUInt);
    return true;
}
bool mod()
{
    UInt top  = VMPop();
    if (top == 0)
    {
        SetError(0x04, (17));
        return false;
    }
    UInt address = valueStack + ((sp-2) << 1);
    Memory_WriteWord(address, Memory_ReadWord(address) % top);
    Memory_WriteByte(typeStack  + sp - 2, Type::eUInt);
    return true;
}
bool addB()
{
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next += codeMemoryBlock[pc]; pc++;
    dataMemoryBlock[typeStack + sp-2] = Type::eUInt;
    return true;
}
bool subB()
{
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next -= codeMemoryBlock[pc]; pc++;
    dataMemoryBlock[typeStack + sp-2] = Type::eUInt;
    return true;
}
bool incLocalB()
{
    Int offset = VMReadByteOffsetOperand();
    Type itype = (Type)0;
    UInt address = UInt(Int(GetBP()) + offset);
    UInt value = VMGet(address, itype);
    if (itype == Type::eByte)
    {
        itype = Type::eUInt;
    }
    VMPut(address, value + 0x01, itype);
    return true;
}
bool decLocalB()
{
    Int offset = VMReadByteOffsetOperand();
    Type itype = (Type)0;
    UInt address = UInt(Int(GetBP()) + offset);
    UInt value = VMGet(address, itype);
    VMPut(address, value - 0x01, itype);
    return true;
}

bool pushIBLE()
{
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next <= codeMemoryBlock[pc]) ? 1 : 0; pc++;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return true;
}
bool pushIBEQ()
{
    UInt * next = (UInt*)&dataMemoryBlock[valueStack + ((sp-2) << 1)];
    *next = (*next == codeMemoryBlock[pc]) ? 1 : 0; pc++;
    dataMemoryBlock[typeStack + sp-2] = Type::eBool;
    return eq();
}
bool pushILE()
{
    pushI();
    return le();
}
bool pushILT()
{
    pushI();
    return lt();
}
bool pushILEI()
{
    pushI();
    return lei();
}

void OpCodes_PopulateJumpTable()
{
    
    for (UInt i = 0; i < 256; i++)
    {
        opCodeJumps[i] = undefined;
    }
    opCodeJumps[OpCode::eNOP]            = nop;
    
    opCodeJumps[OpCode::ePUSHI0]         = pushI0;
    opCodeJumps[OpCode::ePUSHI1]         = pushI1;
    opCodeJumps[OpCode::ePUSHIM1]        = pushIM1;
    opCodeJumps[OpCode::ePUSHIB]         = pushIB;
    opCodeJumps[OpCode::ePUSHI]          = pushI;
    opCodeJumps[OpCode::ePUSHD]          = pushI;
    opCodeJumps[OpCode::ePUSHLOCAL]      = pushLocal;
    opCodeJumps[OpCode::ePUSHLOCALB]     = pushLocalB;
    opCodeJumps[OpCode::ePUSHLOCALBB]    = pushLocalBB;
    opCodeJumps[OpCode::ePUSHLOCALB00]   = pushLocalB00;
    opCodeJumps[OpCode::ePUSHLOCALB02]   = pushLocalB02;
    opCodeJumps[OpCode::ePUSHGLOBAL]     = pushGlobal;
    opCodeJumps[OpCode::ePUSHGLOBALB]    = pushGlobalB;
    opCodeJumps[OpCode::ePUSHSTACKADDR]  = pushStackAddr;
    opCodeJumps[OpCode::ePUSHSTACKADDRB] = pushStackAddrB;
    opCodeJumps[OpCode::ePUSHREL]        = pushRel;
    opCodeJumps[OpCode::ePUSHRELB]       = pushRelB;
    
    opCodeJumps[OpCode::ePOPLOCAL]        = popLocal;
    opCodeJumps[OpCode::ePOPLOCALB]       = popLocalB;
    opCodeJumps[OpCode::ePOPLOCALB00]     = popLocalB00;
    opCodeJumps[OpCode::ePOPLOCALB02]     = popLocalB02;
    opCodeJumps[OpCode::ePOPREL]          = popRel;
    opCodeJumps[OpCode::ePOPRELB]         = popRelB;
    
    opCodeJumps[OpCode::ePOPCOPYLOCALB]   = popCopyLocalB;
    opCodeJumps[OpCode::ePOPCOPYLOCALB00] = popCopyLocalB00;
    opCodeJumps[OpCode::ePOPCOPYLOCALB02] = popCopyLocalB02;
    opCodeJumps[OpCode::ePOPCOPYRELB]     = popCopyRelB;
    
    
    opCodeJumps[OpCode::eCOPYNEXTPOP] = copyNextPop;
    
    opCodeJumps[OpCode::eENTER]    = enter;
    opCodeJumps[OpCode::eENTERB]   = enterB;
    opCodeJumps[OpCode::eDECSP]    = decSP;
    opCodeJumps[OpCode::eSWAP]     = swap;
    opCodeJumps[OpCode::eCAST]     = cast;
    
    opCodeJumps[OpCode::eCALL]     = call;
    opCodeJumps[OpCode::eCALLI]    = callI;
    opCodeJumps[OpCode::eCALLREL]  = callRel;
    opCodeJumps[OpCode::eLIBCALL]  = LibCall;
    opCodeJumps[OpCode::eSYSCALL]  = SysCall;
    opCodeJumps[OpCode::eSYSCALL0] = SysCall0;
    opCodeJumps[OpCode::eSYSCALL1] = SysCall1;
    
    opCodeJumps[OpCode::eRET]      = ret;
    opCodeJumps[OpCode::eRET0]     = ret0;
    opCodeJumps[OpCode::eRETB]     = retB;
    opCodeJumps[OpCode::eRETRES]   = retres;
    opCodeJumps[OpCode::eRETRESB]  = retresB;
    
    opCodeJumps[OpCode::eJZ]   = jz;
    opCodeJumps[OpCode::eJNZ]  = jnz;
    opCodeJumps[OpCode::eJW]   = jw;
    opCodeJumps[OpCode::eJZB]  = jzb;
    opCodeJumps[OpCode::eJNZB] = jnzb;
    opCodeJumps[OpCode::eJB]   = jb;
    
    opCodeJumps[OpCode::eEQ]   = eq;
    opCodeJumps[OpCode::eNE]   = ne;
    opCodeJumps[OpCode::eLT]   = lt;
    opCodeJumps[OpCode::eGT]   = gt;
    opCodeJumps[OpCode::eLE]   = le;
    opCodeJumps[OpCode::eGE]   = ge;
    opCodeJumps[OpCode::eLTI]  = lti;
    opCodeJumps[OpCode::eGTI]  = gti;
    opCodeJumps[OpCode::eLEI]  = lei;
    opCodeJumps[OpCode::eGEI]  = gei;
    
    opCodeJumps[OpCode::eADD]  = add;
    opCodeJumps[OpCode::eSUB]  = sub;
    opCodeJumps[OpCode::eMUL]  = mul;
    opCodeJumps[OpCode::eDIV]  = div;
    opCodeJumps[OpCode::eMOD]  = mod;
    
    opCodeJumps[OpCode::ePUSHILE]  = pushILE;
    opCodeJumps[OpCode::ePUSHILT]  = pushILT;
    opCodeJumps[OpCode::ePUSHILEI] = pushILEI;
    opCodeJumps[OpCode::ePUSHIBLE] = pushIBLE;
    opCodeJumps[OpCode::ePUSHIBEQ] = pushIBEQ;
 
    opCodeJumps[OpCode::eADDB]  = addB;
    opCodeJumps[OpCode::eSUBB]  = subB;
    
    opCodeJumps[OpCode::eINCLOCALB]  = incLocalB;
    opCodeJumps[OpCode::eDECLOCALB]  = decLocalB;
    
    // TODO
}

Bool OpCodeCall(OpCode opCode)
{
    //printf("\nOpCode: 0x%04X 0x%02X", GetPC()-1, opCode);
    return opCodeJumps[opCode]();
}
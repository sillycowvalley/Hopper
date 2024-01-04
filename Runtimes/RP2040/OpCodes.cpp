#include "Common.h"

typedef Bool (*OpCodeMethod)();

OpCodeMethod opCodeJumps[256];

bool undefined()
{
    printf("\nundefined opcode at 0x%04X", GetPC()-1);
    SetError(0x0A, 10);
    return false;
}
bool nop()
{
    return true;
}



bool pushI()
{
    VMPush(VMReadWordOperand(), Type::eUInt);
    return true;
}
bool pushLocal()
{
    Int offset = VMReadWordOffsetOperand() + GetBP();
    UInt stackAddress = GetValueStack();
    Type htype = Type(Memory_ReadWord(UInt(GetTypeStack() + offset)));
    if ((htype == Type::eLong) || (htype == Type::eFloat))
    {
        UInt32 value = Memory_ReadWord(UInt(stackAddress + (offset * 2))) + Memory_ReadWord(UInt(stackAddress + (offset * 2) + 2)) << 16;
        VMPush(value, htype);
    }
    else
    {
        UInt value = Memory_ReadWord(UInt(stackAddress + (offset * 2)));
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
    UInt offset = VMReadWordOperand();
    UInt stackAddress = GetValueStack();
    Type htype = Type(Memory_ReadWord(GetTypeStack() + offset));
    if ((htype == Type::eLong) || (htype == Type::eFloat))
    {
        UInt32 value = Memory_ReadWord(stackAddress + (offset * 2)) + Memory_ReadWord(stackAddress + (offset * 2) + 2) << 16;
        VMPush(value, htype);
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

bool enter()
{
    VMPushCS(GetBP());
    SetBP(GetSP());
    
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

Bool callI()
{
    UInt methodAddress = VMReadWordOperand();
    VMPushCS(GetPC());
    SetPC(methodAddress);
    return true;
}

Bool ret()
{
    UInt popBytes = VMReadWordOperand();
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
    SetBP(VMPopCS());
    if (GetCSP() == 0x00)
    {
        SetPC(0x00);
        return false;
    }
    else
    {
        SetPC(VMPopCS());
    }
    return true;
}

bool libCall()
{
    Byte iLibCall = VMReadByteOperand();
    return LibCall(iLibCall);
}
bool sysCall()
{
    Byte iOverload = VMPop();
    Byte iSysCall  = VMReadByteOperand();
    return SysCall(iSysCall, iOverload);
}

bool jz()
{
    if (VMPop() == 0x00)
    {
        SetPC((UInt)(VMReadWordOffsetOperand() + (Int)(GetPC() - 0x03)));
    }
    else
    {
        SetPC(GetPC() + 0x02);
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
        SetPC(GetPC() + 0x02);
    }
    return true;
}
bool jw()
{
    SetPC((UInt)(VMReadWordOffsetOperand() + (Int)(GetPC() - 0x03)));
    return true;
}
void OpCodes_PopulateJumpTable()
{
    
    for (UInt i = 0; i < 256; i++)
    {
        opCodeJumps[i] = undefined;
    }
    opCodeJumps[OpCode::eNOP]   = nop;
    
    opCodeJumps[OpCode::ePUSHI]      = pushI;
    opCodeJumps[OpCode::ePUSHD]      = pushI;
    opCodeJumps[OpCode::ePUSHLOCAL]  = pushLocal;
    opCodeJumps[OpCode::ePUSHGLOBAL] = pushGlobal;
    
    opCodeJumps[OpCode::eENTER] = enter;
    
    opCodeJumps[OpCode::eCALL]  = call;
    opCodeJumps[OpCode::eCALLI] = callI;
    opCodeJumps[OpCode::eLIBCALL] = libCall;
    opCodeJumps[OpCode::eSYSCALL] = sysCall;
    
    opCodeJumps[OpCode::eRET] = ret;
    
    opCodeJumps[OpCode::eJZ] = jz;
    opCodeJumps[OpCode::eJNZ] = jnz;
    opCodeJumps[OpCode::eJW] = jw;
    
    // TODO
}

Bool OpCodeCall(OpCode opCode)
{
    return opCodeJumps[opCode]();
}
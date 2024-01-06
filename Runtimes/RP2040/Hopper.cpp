

#include "Common.h"

const UInt LED_PIN = 25;

const Char enter  = (Char)0x0D;
const Char escape = (Char)0x1B;
const Char slash  = (Char)0x5C;

const UInt stackSize     = 512; // size of value stack in byte (each stack slot is 2 bytes)
const UInt callStackSize = 512; // size of callstack in bytes (4 bytes per call)
    
const UInt dataMemoryStart = 0x0000; // data memory magically exists from 0x0000 to 0xFFFF
const UInt codeMemoryStart = 0x0000; // code memory magically exists from 0x0000 to 0xFFFF

Byte * dataMemoryBlock = nullptr;
Byte * codeMemoryBlock = nullptr;
Bool codeLoaded;
Byte lastError;
UInt errorContext;

UInt binaryAddress;
UInt programSize;
UInt constAddress;
UInt methodTable;

UInt valueStack; // 4 byte slots (sp << 1)

UInt typeStack;  // 2 byte slots (but we only use the LSB, just to be able to use the same 'sp' as valueStack)
UInt callStack;  // 2 byte slots (either return address PC or BP for stack from)

UInt dataMemory; // start of free memory (changes if a new program is loaded)

UInt breakpoints[16];
Bool breakpointExists;

UInt currentDirectory;

UInt pc;
UInt sp;
UInt bp;
UInt csp;
Bool cnp;

OpCode opCode;


void SetError(Byte error, UInt context)
{
    lastError = error;
    errorContext = context;
    printf("\nError: 0x%02X at %d", error, context);
    putchar(slash);
    putchar(slash);
    putchar(slash);
}

std::queue<Char> charQueue;

Char GetChar()
{
    Char ch;
    if (!charQueue.empty())
    {
        ch = charQueue.front();
        charQueue.pop();
    }
    else
    {
        ch = getchar();
    }
    return ch;
}
Bool IsAvailable()
{
    return !charQueue.empty() || tud_cdc_available();
}

Bool IsBreak()
{
    while (tud_cdc_available())
    {
        Char ch = getchar();
        if (ch == Char(0x03))
        {
            return true;
        }
        charQueue.push(ch);
    }
    return false;
}

void Memory_WriteCodeByte(UInt address, Byte value)
{
    codeMemoryBlock[address] = value;
}

UInt VMGetCS(UInt stackOffset)
{
    return Memory_ReadWord(callStack + stackOffset);
}

UInt32 VMGetVS(UInt stackOffset)
{
    return Memory_ReadWord(valueStack + (stackOffset << 1)) + (Memory_ReadWord(valueStack + (stackOffset << 1) + 2) << 16);
}

Type VMGetTS(UInt stackOffset)
{
    return Type(Memory_ReadWord(typeStack + stackOffset));
}

void VMPushCS(UInt word)
{
#ifdef CHECKED
    if (csp == callStackSize)
    {
        SetError(0x08, (1)); // call stack overflow
    }
#endif
    Memory_WriteWord(callStack + csp,     word);
    csp += 2;
}
UInt VMPopCS()
{
#ifdef CHECKED
    if (csp == 0)
    {
        SetError(0x08, (2)); // call stack overflow
    }
#endif
    csp -= 2;
    return Memory_ReadWord(callStack + csp);
}

void VMPush(UInt word, Type type)
{
#ifdef CHECKED
    if (sp == stackSize)
    {
        SetError(0x07, (3)); // argument stack overflow
    }
#endif
    Memory_WriteWord(valueStack + (sp << 1),     word); // lsw
    Memory_WriteWord(valueStack + (sp << 1) + 2, 0);    // msw
    Memory_WriteWord(typeStack  + sp, type);
    sp += 2;
}
void VMPush32(UInt32 dword, Type type)
{
    #ifdef CHECKED
    if (sp == stackSize)
    {
        SetError(0x07, (4)); // argument stack overflow
    }
#endif
    Memory_WriteWord(valueStack + (sp << 1),     dword & 0xFFFF); // lsw
    Memory_WriteWord(valueStack + (sp << 1) + 2, dword >> 16);    // msw
    Memory_WriteWord(typeStack  + sp, type);
    sp += 2;
}

UInt VMPop()
{
#ifdef CHECKED
    if (sp == 0)
    {
        SetError(0x07, (5)); // argument stack overflow
    }
#endif
    sp -= 2;
    UInt value = Memory_ReadWord(valueStack + (sp << 1)); // lsw
    return value;
}

UInt VMPop(Type & type)
{
#ifdef CHECKED
    if (sp == 0)
    {
        SetError(0x07, (6)); // argument stack overflow
    }
#endif
    sp -= 2;
    UInt value = Memory_ReadWord(valueStack + (sp << 1)); // lsw
    type = (Type)(Memory_ReadWord(typeStack  + sp));
    return value;
}
UInt VMGet(UInt address, Type & type)
{
    UInt value = Memory_ReadWord(valueStack + (address << 1)); // lsw
    type = Type(Memory_ReadWord(typeStack + address));
    return value;
}
UInt32 VMGet32(UInt address, Type & type)
{
    UInt32 value = Memory_ReadWord(valueStack + (address << 1)) + (Memory_ReadWord(valueStack + (address << 1) + 2) << 16); 
    type = Type(Memory_ReadWord(typeStack + address));
    return value;
}
void VMPut  (UInt address, UInt value, Type type)
{
    Memory_WriteWord(valueStack + (address << 1),    value); // lsw
    Memory_WriteWord(valueStack + (address << 1) +2, 0);     // msw
    Memory_WriteWord(typeStack + address, type);
}
void VMPut32(UInt address, UInt32 value, Type type)
{
    Memory_WriteWord(valueStack + (address << 1),    value & 0xFFFF); // lsw
    Memory_WriteWord(valueStack + (address << 1) +2, value >> 16);     // msw
    Memory_WriteWord(typeStack + address, type);
}

Float VMPopFloat()
{
    Type type;
    UInt32 ui = VMPop32(type);
    Float * f = (Float*)&ui;
    return *f;
}
void  VMPushFloat(Float f)
{
    UInt32 * ui = (UInt32*)&f;
    VMPush32(*ui, Type::eFloat);
}
Int VMPopInt()
{
    UInt ui = VMPop();
    Int * i = (Int*)&ui;
    return *i;
}
void  VMPushInt(Int i)
{
    UInt * ui = (UInt*)&i;
    VMPush(*ui, Type::eInt);
}
    



UInt32 VMPop32(Type & type)
{
#ifdef CHECKED
    if (sp == 0)
    {
        SetError(0x07, (7)); // argument stack overflow
    }
#endif
    sp -= 2;
    UInt32 value = Memory_ReadWord(valueStack + (sp << 1)) + (Memory_ReadWord(valueStack + (sp << 1) + 2 ) << 16);
    type = (Type)(Memory_ReadWord(typeStack  + sp));
    return value;
}

UInt VMLookupMethod(UInt methodIndex)
{
    methodIndex = (methodIndex & 0x3FFF);
    UInt address = methodTable;
    for (;;)
    {
        UInt entry = Memory_ReadCodeWord(address);
        if (entry == methodIndex)
        {
            address = Memory_ReadCodeWord(address + 0x02);
            break;
        }
        address = address + 0x04;
    }
    return address;
}

    

void ClearBreakpoints(Bool includingZero)
{
    for (Byte i=1; i < 16; i++)
    {
        breakpoints[i] = 0;
    }
    if (includingZero)
    {
        breakpoints[0] = 0;
        breakpointExists = false;
    }
    else
    {
        breakpointExists = breakpoints[0] != 0; // single step breakpoint set?
    }
}

UInt GetBreakpoint(Byte n)
{
    return breakpoints[n];
}
    
void SetBreakpoint(Byte n, uint address)
{
    breakpoints[n] = address;
    if (address != 0)
    {
        breakpointExists = true;
    }
    else
    {
        breakpointExists = false;
        for (Byte i=0; i < 16; i++)
        {
            if (breakpoints[i]!= 0)
            {
                breakpointExists = true;
                break;
            }
        }
    }
}

Byte FromHex(Char ch)
{
    switch (ch)
    {
        case '0': { return 0x00; }
        case '1': { return 0x01; }
        case '2': { return 0x02; }
        case '3': { return 0x03; }
        case '4': { return 0x04; }
        case '5': { return 0x05; }
        case '6': { return 0x06; }
        case '7': { return 0x07; }
        case '8': { return 0x08; }
        case '9': { return 0x09; }
        case 'a': case 'A': { return 0x0A; }
        case 'b': case 'B': { return 0x0B; }
        case 'c': case 'C': { return 0x0C; }
        case 'd': case 'D': { return 0x0D; }
        case 'e': case 'E': { return 0x0E; }
        case 'f': case 'F': { return 0x0F; }
    }
    return 0;
}


Bool TryReadSerialByte(Byte & data)
{
    char c0 = GetChar();
    char c1 = GetChar();
    Byte msn = FromHex(c0);
    Byte lsn = FromHex(c1);
    data =  (msn << 4) + lsn;
    return true;
}
    
Bool SerialLoadIHex(UInt & loadedAddress, UInt & codeLength)
{
    Bool success = true;
    loadedAddress = codeMemoryStart;
    
    UInt codeLimit = 0xFFFF; // 64K - 1 (so that it fits in 16 bits)
    
    codeLength = 0;
    for (;;)
    {
        char colon = GetChar();
        if (colon != ':') { success = false; break; }
        
        Byte byteCount;
        if (!TryReadSerialByte(byteCount)) { success = false; break; }
        
        Byte lsb;
        Byte msb;
        if (!TryReadSerialByte(msb)) { success = false; break; }
        if (!TryReadSerialByte(lsb)) { success = false; break; }
        UInt recordAddress = lsb + (msb << 8);
        
        Byte recordType;
        if (!TryReadSerialByte(recordType)) { success = false; break; }
        
        switch (recordType)
        {
            case 0x00: // data
            {
                for (UInt c=0; c < byteCount; c++)
                {
                    Byte dataByte;
                    if (!TryReadSerialByte(dataByte))             { success = false; break; }
                    if (codeMemoryStart + recordAddress >= codeLimit) { success = false; break; }
                    Memory_WriteCodeByte(codeMemoryStart + recordAddress, dataByte);
                    codeLength++;
                    recordAddress++;
                }
                Byte checkSum;
                if (!TryReadSerialByte(checkSum)) { success = false; break; }
                
                Char eol = GetChar();
                if ((eol != char(0x0D)) && (eol != char(0x0A))) // either will do
                { 
                    success = false; break;
                }
                continue; // next record
            }
            case 0x01:
            {
                Byte checkSum;
                if (!TryReadSerialByte(checkSum)) { success = false; break; }
                break; // EOF
            }
            default:
            {
                success = false; break;
            }
        }
        break;
    } // loop
    return success;
}

void DumpPage(Byte iPage, Bool includeAddresses)
{
    UInt rowAddress = (iPage << 0x08);;
    for (Byte row = 0x00; row < 0x10; row++)
    {
        putchar(Char(13));
        if (includeAddresses)
        {
            printf("%04X ", rowAddress);
            rowAddress = rowAddress + 0x10;
        }
        if (iPage == 0x00)
        {
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte data = 0;
                Byte address = col + (row << 0x04);
                switch (address)
                {
                    case 0xB1:
                    {
                        data = Byte(pc >> 0x08);
                        break;
                    }
                    case 0xB0:
                    {
                        data = Byte(pc & 0xFF);
                        break;
                    }
                    case 0xB3:
                    {
                        data = Byte((sp + 0x0600) >> 0x08);
                        break;
                    }
                    case 0xB2:
                    {
                        data = Byte((sp + 0x0600) & 0xFF);
                        break;
                    }
                    case 0xB5:
                    {
                        data = Byte(((sp / 0x02) + 0x0500) >> 0x08);
                        break;
                    }
                    case 0xB4:
                    {
                        data = Byte(((sp / 0x02) + 0x0500) & 0xFF);
                        break;
                    }
                    case 0xB7:
                    {
                        data = Byte((bp + 0x0600) >> 0x08);
                        break;
                    }
                    case 0xB6:
                    {
                        data = Byte((bp + 0x0600) & 0xFF);
                        break;
                    }
                    case 0xB9:
                    {
                        data = Byte((csp + 0x0400) >> 0x08);
                        break;
                    }
                    case 0xB8:
                    {
                        data = Byte((csp + 0x0400) & 0xFF);
                        break;
                    }
                    case 0xBB:
                    {
                        data = Byte(HopperFlags::eMCUPlatform | HopperFlags::eLongValues);
                        if (breakpointExists)
                        {
                            data = data | Byte(HopperFlags::eBreakpointsSet);
                        }
                        break;
                    }
                    case 0xE9:
                    {
                        data = Byte(Memory_FreeList_Get() >> 0x08);
                        break;
                    }
                    case 0xE8:
                    {
                        data = Byte(Memory_FreeList_Get() & 0xFF);
                        break;
                    }
                    case 0xEB:
                    {
                        data = Byte(Memory_HeapSize_Get() >> 0x08);
                        break;
                    }
                    case 0xEA:
                    {
                        data = Byte(Memory_HeapStart_Get() >> 0x08);
                        break;
                    }
                    case 0xCA:
                    {
                        data = 0x00;
                        break;
                    }
                    default:
                    {
                        if ((address >= 0x50) && (address <= 0x5F))
                        {
                            data = Byte(GetBreakpoint(address - 0x50) & 0xFF);
                        }
                        else if ((address >= 0x60) && (address <= 0x6F))
                        {
                            data = Byte(GetBreakpoint(address - 0x60) >> 0x08);
                        }
                        else
                        {
                            data = 0x00;
                        }
                        break;
                    }
                } // switch
                if (includeAddresses)
                {
                    putchar(' ');
                    if (col == 0x08)
                    {
                        putchar(' ');
                    }
                }
                printf("%02X", data);
            }
        }
        else if (iPage == 0x04)
        {
            for (Byte col = 0x00; col < 0x08; col++)
            {
                UInt address = col * 0x02 + (row << 0x04);
                UInt stackData = VMGetCS(address);
                if (includeAddresses)
                {
                    putchar(' ');
                    if (col == 0x04)
                    {
                        putchar(' ');
                    }
                }
                printf("%02X", Byte(stackData & 0xFF));
                if (includeAddresses)
                {
                    putchar(' ');
                }
                printf("%02X", Byte(stackData >> 0x08));
            }
        }
        else if (iPage == 0x05)
        {
            for (Byte col = 0x00; col < 0x10; col++)
            {
                UInt address = col + (row << 0x04);
                address = address * 0x02;
                Type htype = VMGetTS(address);
                if (includeAddresses)
                {
                    putchar(' ');
                    if (col == 0x08)
                    {
                        putchar(' ');
                    }
                }
                printf("%02X", Byte(htype));
            }
        }
        else if ((iPage >= 0x06) && (iPage <= 0x09))
        {
            uint delta = (iPage - 0x06) * 0x0100;
            for (Byte col = 0; col < 16; col++)
            {
                UInt address = col + (row << 4);
                address += delta;
                
                Byte data = Memory_ReadByte(valueStack + address);
                if (includeAddresses)
                {
                    putchar(' ');
                    if (col == 8)
                    {
                        putchar(' ');
                    }
                }
                printf("%02X", data);
            }
        }
        else if (iPage > 0x09)
        {
            for (Byte col = 0x00; col < 0x10; col++)
            {
                UInt address = col + (row << 0x04);
                address = address + (0x0100 * iPage);
                if (includeAddresses)
                {
                    putchar(' ');
                    if (col == 0x08)
                    {
                        putchar(' ');
                    }
                }
                printf("%02X", Memory_ReadByte(address));
            }
        }
        else
        {
            for (Byte col = 0x00; col < 0x10; col++)
            {
                if (includeAddresses)
                {
                    putchar(' ');
                    if (col == 0x08)
                    {
                        putchar(' ');
                    }
                }
                printf("00");
            }
        }
    }
    putchar(Char(13));
}

void Platform_Release()
{
    free(dataMemoryBlock);
    dataMemoryBlock = nullptr;
    free(codeMemoryBlock);
    codeMemoryBlock = nullptr;
}


Bool Platform_Initialize()
{
    for (;;)
    {
        lastError = 0;
        errorContext = 0;
        if (nullptr != dataMemoryBlock)
        {
            Platform_Release();
        }
        codeMemoryBlock = (unsigned char*)malloc(0x10000); // 64K
        if (nullptr == codeMemoryBlock) { break; }
        memset(codeMemoryBlock, 0, 0x10000);
        
        dataMemoryBlock = (unsigned char*)malloc(0x10000); // 64K
        if (nullptr == dataMemoryBlock) { break; }
        memset(dataMemoryBlock, 0, 0x10000);
        
        return true;
    }
    return false;
}


void WaitForEnter()
{
    for(;;)
    {
        Char ch = GetChar();
        if (ch == enter)
        {
            break;
        }       
    } // loop
    putchar(slash); // '\' response : acknowledge <enter> received
}

void VMInitialize(UInt loadedAddress, UInt loadedSize)
{
    binaryAddress      = loadedAddress;
    programSize        = loadedSize;
    constAddress       = Memory_ReadCodeWord(binaryAddress + 0x0002);
    methodTable        = binaryAddress + 0x0006;
}

void DataMemoryReset()
{
    UInt nextAddress   = dataMemoryStart;
    callStack          = nextAddress;
    nextAddress        = nextAddress + callStackSize;
    
    valueStack         = nextAddress;
    nextAddress        = nextAddress + (stackSize << 1);
    
    typeStack          = nextAddress;
    nextAddress        = nextAddress + stackSize;
    
    OpCodes_PopulateJumpTable();
    SysCalls_PopulateJumpTable();
    LibCalls_PopulateJumpTable();
    
    dataMemory         = nextAddress;
    
    if (dataMemory < 0x0A00)
    {
        dataMemory = 0x0A00; // after our 'fake' stack pages
    }
    Memory_Initialize(dataMemory, 0x10000 - dataMemory);
    ClearBreakpoints(true);
    currentDirectory = HRString_New();
    Memory_Set(typeStack,  0, stackSize);
    Memory_Set(valueStack, 0, (stackSize << 1));
}

void VMRelease()
{
    if (currentDirectory != 0)
    {
        GC_Release(currentDirectory);
        currentDirectory = 0;
    }
}

void VMRestart()
{
    DataMemoryReset();
    // TODO DiskSetup();
    
    sp = 0;
    bp = 0;
    csp = 0;
    lastError = 0;
    cnp = false;
    pc = Memory_ReadCodeWord(binaryAddress + 0x0004);  
}


Bool ExecuteOpCode()
{
    Bool doNext = false;
    // TODO : External_ServiceInterrupts();
    opCode = OpCode(Memory_ReadCodeByte(pc));
    pc++;
    
    doNext = OpCodeCall(opCode);
    return doNext;
}

void Execute()
{
    for (;;)
    {
        ExecuteOpCode();
        if (lastError != 0x00)
        {
            break;
        }
        if (pc == 0x00)
        {
            VMRestart();
            break;
        }
        if (IsBreak())
        {
            printf("\nBREAK\n");
            break;;
        }
        if (breakpointExists)
        {
            for (Byte iBreak = 0; iBreak < 16; iBreak++)
            {
                if (GetBreakpoint(iBreak) == pc)
                {
                    if (iBreak == 0)
                    {
                        SetBreakpoint(0, 0);
                    }
                    return;
                }
            }
        }
    }
}

void ExecuteStepTo()
{
    ExecuteOpCode();
    if (lastError != 0x00)
    {
    }
    else if (pc == 0x00)
    {
        VMRestart();
    }
}

void ExecuteWarp()
{
    UInt watchDog = 2500;
    for (;;)
    {
        watchDog--;
        if (watchDog == 0)
        {
            watchDog = 2500;
            if (IsBreak())
            {
                printf("\nBREAK\n");
                break;
            }
        }
        
        // TODO if (Library_ISRExists_Get())
        //{
        //    External_ServiceInterrupts();
        //}
        
        OpCode opCode = (OpCode)Memory_ReadCodeByte(pc);
        pc++;
        
        if (OpCodeCall(opCode))
        {
            continue;
        }
        if (lastError != 0)
        {
            break;
        }
        if (pc == 0)
        {
            VMRestart();
            break;
        }
        if (IsBreak())
        {
            printf("\nBREAK\n");
            break;
        }
    }
}

void VMDumpStack(UInt limit)
{
    putchar(enter);
    limit = limit * 2;
    for (UInt s = 0; s < sp; s = s + 2)
    {
        if (sp - s > limit) { continue; }
        if (s == bp)
        {
            printf("BP");
        }
        else
        {
            printf("  ");
        }
        printf("  ");
        UInt * pvalue   = (UInt*)&dataMemoryBlock[valueStack + (s << 1)];
        UInt address    = valueStack + (s << 1);
        Byte htype      = Byte(Memory_ReadWord(typeStack + s));
        printf("%04X %04X %04X:%02X", s, address, *pvalue, htype);
        if (IsReferenceType(Type(htype)))
        {
            Byte count  = Byte(Memory_ReadByte(*pvalue + 1));
            printf(" %02X ", count);
            GC_Dump(*pvalue);
        }
        else
        {
            switch ((Type)htype)
            {
                case Type::eByte:
                case Type::eUInt:
                {
                    printf(" %d", *pvalue);
                    break;
                }
                case Type::eBool:
                {
                    printf(" %c", (*pvalue != 0) ? 't' : 'f');
                    break;
                }
                case Type::eChar:
                {
                    printf(" '%c'", (Char)*pvalue);
                    break;
                }
                case Type::eInt:
                {
                    Int * iv = (Int *)pvalue;
                    printf(" %d", *iv);
                    break;
                }
                case Type::eLong:
                {
                    Long * pl = (Long*)pvalue;
                    printf(" %ld ", *pl);
                    break;
                }
                case Type::eFloat:
                {
                    Float * pf = (Float*)pvalue;
                    printf(" %g ", *pf);
                    break;
                }
                default:
                {
                    break;
                }
            } 
        }
        putchar(enter);
    }
}

Bool VMIsOnFreeList(UInt pCandidate)
{
    UInt pCurrent = Memory_FreeList_Get();
    for (;;)
    {
        if (0 == pCurrent)
        {
            break;
        }
        if (pCurrent == pCandidate)
        {
            return true;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
    }
    return false;
}

void VMDumpHeap(bool display, UInt accountedFor)
{
    bool verboseDisplay = false;
    if (display)
    {
        verboseDisplay = true;
    }
    if (verboseDisplay)
    {
        putchar(enter);
        for (UInt s = 0; s < sp; s = s + 2)
        {
            UInt * pvalue = (UInt*)&dataMemoryBlock[valueStack + (s << 1)];
            //UInt address  = valueStack + (s << 1);
            Byte htype    = Byte(Memory_ReadWord(typeStack + s));
            if (IsReferenceType(htype))
            {
                Byte count  = Byte(Memory_ReadByte(*pvalue + 1));
                printf("%04X:%02X %02X ", *pvalue, htype, count);
                GC_Dump(*pvalue, 0);
                putchar(enter);
            }
        }
    }
    
    if (display)
    {
        putchar(enter);
        printf("PC:%04X", pc);
        putchar(enter);
        printf("F:");
    }
    
    UInt pCurrent = Memory_FreeList_Get();
    UInt freeSize = 0;
    UInt allocatedSize = 0;
    for(;;)
    {
        if (0 == pCurrent)
        {
            break;
        }
        UInt size  = Memory_ReadWord(pCurrent);    
        UInt pNext = Memory_ReadWord(pCurrent+2);    
        UInt pPrev = Memory_ReadWord(pCurrent+4);   
        if (verboseDisplay) // free list items
        { 
            putchar(enter);
            printf("%04X %04X %04X> <%04X", pCurrent, size, pNext, pPrev);
        }
        pCurrent = pNext;
        freeSize = freeSize + size;
    }
    if (display)
    {
        putchar(enter);
        
        UInt h = Memory_HeapStart_Get()+Memory_HeapSize_Get();
        printf("H:%04X-%04X=%04X", h, Memory_HeapStart_Get(), Memory_HeapSize_Get());
    }
    pCurrent      = Memory_HeapStart_Get();
    UInt pLimit   = Memory_HeapStart_Get() + Memory_HeapSize_Get();
    UInt count    = 0;
    for(;;)
    {
        count ++;
        if (pCurrent >= pLimit)
        {
            break;
        }
        UInt size  = Memory_ReadWord(pCurrent);    
        if (count > 50)
        {
            break;
        }
            
        if (!VMIsOnFreeList(pCurrent))
        {
            if (verboseDisplay) // heap items
            {
                putchar(enter);
                Byte tp = Memory_ReadByte(pCurrent+2);
                Byte rf = Memory_ReadByte(pCurrent+3);
                printf("%04X %04X %02X %02X", pCurrent, size, tp, rf);
            }
            allocatedSize = allocatedSize + size;
        }
        else if (verboseDisplay)
        {
            // free list items
            putchar(enter);
            printf("%04X %04X", pCurrent, size);
        }
        if (size == 0)
        {
            break;
        }
        pCurrent = pCurrent + size; // this is why we limit ourselves to 0xFF00 (not 0x10000, actual 64K)
    }
    bool reportAndStop = (Memory_HeapSize_Get() != (allocatedSize + freeSize));
    if (!reportAndStop && (accountedFor > 0))
    {
        reportAndStop = (accountedFor != allocatedSize);    
    }
    if (reportAndStop)
    {
        if (!display)
        {
            VMDumpHeap(true, accountedFor);
            SetError(0x0B, 25);
        }
        else
        {
            putchar(enter);
            UInt fl = Memory_HeapSize_Get() - (allocatedSize + freeSize);
            printf("A%04X:%04X F%04X %04X", allocatedSize, accountedFor, freeSize, fl);
        }
    }
}


void HopperEntryPoint()
{
    codeLoaded = false;
    VMRestart();
    UInt loadedAddress = 0;
    UInt codeLength = 0;
    
    if (loadAuto)
    {
        /* TODO
        if (Runtime_LoadAuto_R(loadedAddress, codeLength))
        {
            VMInitialize(loadedAddress, codeLength);
            VMRestart();
            codeLoaded = true;
            VMExecuteWarp();
        }
        */
    }
    putchar(slash); // ready
    
    for(;;) // loop
    {
        Char ch = 0;
        if (IsAvailable())
        {
            ch = GetChar();
        }
        if (ch == escape) // <esc> from Debugger
        {
            putchar(slash); // '\' response -> ready for command
            ch = GetChar();  // single letter command
            switch (ch)
            {
                case 'F': // fast memory page dump
                {
                    Byte msn = FromHex(GetChar());
                    Byte lsn = FromHex(GetChar());
                    WaitForEnter();
                    
                    Byte iPage = (msn << 4) + lsn;
                    DumpPage(iPage, false);
                    putchar(slash); // confirm data
                    break;
                }
                case 'M': // memory page dump
                {
                    Byte msn = FromHex(GetChar());
                    Byte lsn = FromHex(GetChar());
                    WaitForEnter();
                    
                    Byte iPage = (msn << 4) + lsn;
                    DumpPage(iPage, true);
                    putchar(slash); // confirm data
                    break;
                }
                case 'B':
                {
                    char arg = GetChar();
                    if (arg == 'X')
                    {
                        ClearBreakpoints(false);
                    }
                    else
                    {
                        Byte n  = FromHex(arg);
                        Byte a3 = FromHex(GetChar());
                        Byte a2 = FromHex(GetChar());
                        Byte a1 = FromHex(GetChar());
                        Byte a0 = FromHex(GetChar());
                        UInt address = (a3 << 12) + (a2 << 8) + (a1 << 4) + a0;
                        SetBreakpoint(n, address);   
                    }
                    WaitForEnter();
                    break;
                }
                case 'P': // get PC
                {
                    WaitForEnter();
                    
                    putchar(enter);
                    printf("%04X", pc);
                    putchar(slash); // confirm data
                    break;
                }
                case 'R': // get Registers
                {
                    WaitForEnter();
                    
                    putchar(enter);
                    putchar('P');
                    putchar('C');
                    putchar('=');
                    printf("%04X", pc);
                    putchar(' ');
                    
                    putchar('C');
                    putchar('S');
                    putchar('P');
                    putchar('=');
                    printf("%04X", csp);
                    putchar(' ');
                    
                    putchar('S');
                    putchar('P');
                    putchar('=');
                    printf("%04X", sp + 0x0600);
                    putchar(' ');
                    
                    putchar('T');
                    putchar('S');
                    putchar('P');
                    putchar('=');
                    printf("%04X", sp + 0x0500);
                    putchar(' ');
                    
                    putchar('B');
                    putchar('P');
                    putchar('=');
                    printf("%04X", bp + 0x0600);
                    
                    putchar(slash); // confirm data
                    break;
                }
                /*
                case 'T':
                    {
                        WaitForEnter();
                        
                        // read name characters till 0x0D
                        uint destinationName = HRString.New();
                        for(;;)
                        {
                            char ch = GetChar();
                            if (ch == enter)
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationName, ch);
                        }
                        putchar(enter);
                        putchar(slash);
                        
                        // read path characters till 0x0D
                        uint destinationFolder = HRString.New();
                        for(;;)
                        {
                            char ch = GetChar();
                            if (ch == enter)
                            {
                                break;
                            }
                            HRString.BuildChar(ref destinationFolder, ch);
                        }
                        putchar(enter);
                        putchar(slash);
                        
                        HRDirectory.Create(destinationFolder);
                        
                        char h3 = GetChar();
                        char h2 = GetChar();
                        char h1 = GetChar();
                        char h0 = GetChar();
                        
                        uint size = (FromHex(h3) << 12) + (FromHex(h2) << 8) + (FromHex(h1) << 4) + FromHex(h0);
                        
                        putchar(enter);
                        putchar(slash);
                        
                        uint fh = HRFile.Create(destinationName);
                        
                        // read file hex nibbles
                        while (size != 0)
                        {
                            char n1 = GetChar();
                            char n0 = GetChar();
                            byte b = (FromHex(n1) << 4) + FromHex(n0);
                            HRFile.Append(fh, b);
                            size--;
                        }
                        HRFile.Flush(fh);
                        putchar(enter);
                        putchar(slash);
                        break;
                    }
                    */
                case 'L':
                    {
                        WaitForEnter();
                        
                        loadedAddress = 0;
                        
                        codeLoaded = SerialLoadIHex(loadedAddress, codeLength);
                        putchar(enter);
                        if (codeLoaded)
                        {
                            VMInitialize(loadedAddress, codeLength);
                            VMRestart();
                            
                            // codeLength
                            printf("\n codeLength=0x%04X  pc=0x%04X", codeLength, pc);
                            putchar(' ');
                        }
                        
                        // '*' success
                        for(;;)
                        {
                            ch = GetChar();
                            if ((ch == '!') || (ch == '*'))
                            {
                                break;
                            }
                        }
                        putchar(enter);
                        putchar(codeLoaded ? '*' : '!');
                        
                        putchar(slash); // confirm the data
                        
                        if (codeLoaded)
                        {
                            // TODO FlashProgram(loadedAddress, codeLength);
                        }
                        break;
                    } // L
                default:
                    {
                        if (codeLoaded)
                        {
                            switch (ch)
                            {
                                case 'O': // Step Over | <F10>
                                {
                                    WaitForEnter();
                                    OpCode opCode = OpCode(Memory_ReadCodeByte(pc));
                                    if ((opCode == OpCode::eCALL) || (opCode == OpCode::eCALLI))
                                    {
                                        
                                        // set breakpoint[0] to PC+3
                                        SetBreakpoint(0, pc+3);
                                        Execute();
                                    }
                                    else if (opCode == OpCode::eCALLB)
                                    {
                                        // set breakpoint[0] to PC+2
                                        SetBreakpoint(0, pc+2);
                                        Execute();
                                    }
                                    else
                                    {
                                        // use single step (set bit in HopperFlags)
                                        ExecuteStepTo();
                                    }
                                    putchar(slash); // confirm handing back control
                                    break;
                                }
                                case 'I': // Step Into | <F11>
                                {
                                    WaitForEnter();
                                    ExecuteStepTo();
                                    putchar(slash); // confirm handing back control
                                    break;
                                }
                                case 'D': // Debug
                                {
                                    WaitForEnter();
                                    Execute();
                                    putchar(slash); // confirm handing back control
                                    break;
                                }
                                case 'X': // Execute
                                {
                                    WaitForEnter();
                                    ExecuteWarp();
                                    putchar(slash); // confirm handing back control
                                    break;
                                }
                                case 'W': // Warm restart
                                {
                                    WaitForEnter();
                                    VMRestart();
                                    break;
                                }
                                
                                case 'V':
                                {
                                    WaitForEnter();
                                    VMDumpStack(20);
                                    putchar(slash); // confirm data
                                    break;
                                }
                                
                                case 'H':
                                {
                                    WaitForEnter();
                                    VMDumpHeap(true, 0);
                                    putchar(slash); // confirm data
                                    break;
                                }
                                
                            } // switch
                        } // loaded
                    } // default
            } // switch
        } // <esc> from Debugger
    } // loop
    VMRelease();
}


int main() 
{
    stdio_init_all();
    
    while (!tud_cdc_connected())
    {
        sleep_ms(100);
    }
    
    if (Platform_Initialize())
    {
        // flicker LED_BUILTIN to show that initialization completed
        gpio_init(LED_PIN);
        gpio_set_dir(LED_PIN, GPIO_OUT);
        for (int i = 0; i < 5; i++)
        {
            gpio_put(LED_PIN, 1);
            sleep_ms(50);
            gpio_put(LED_PIN, 0);
            sleep_ms(50);
        }
        
        HopperEntryPoint();
        Platform_Release();
    }
}


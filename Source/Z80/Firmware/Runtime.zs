program Runtime
{
    embed "/Bin/Sieve.hexe2"
    //embed "/Bin/Test.hexe"
    //embed "/Bin/Hello.hexe"
    //embed "/Bin/Bench.hexe"

    uses "/Source/Z80/Firmware/Memory"
    uses "/Source/Z80/Firmware/Utilities"
    uses "/Source/Z80/Firmware/LCDDriver"
    
    const byte ADD = 0x00;
    const byte SUB = 0x01;
    const byte DIV = 0x02;
    const byte MUL = 0x03;
    const byte MOD = 0x04;
    
    const byte ADDI = 0x05;
    const byte SUBI = 0x06;
    const byte DIVI = 0x07;
    const byte MULI = 0x08;
    const byte MODI = 0x09;
    
    const byte EQ  = 0x0A;
    const byte NE  = 0x0B;
    const byte GT  = 0x0C;
    const byte LT  = 0x0D;
    const byte GE  = 0x0E;
    const byte LE  = 0x0F;
    const byte GTI = 0x10;
    const byte LTI = 0x11;
    const byte GEI = 0x12;
    const byte LEI = 0x13;
    
    const byte PUSHI   = 0x1B;
    const byte PUSHI0  = 0x1C;
    const byte PUSHI1  = 0x1D;
    const byte PUSHIM1 = 0x1E;
    
    const byte POPLOCAL  = 0x1F;
    const byte PUSHLOCAL = 0x20;
    const byte POPREL    = 0x21;
    const byte PUSHREL   = 0x22;
    
    const byte PUSHSP     = 0x23;

    const byte POPGLOBAL  = 0x24;
    const byte PUSHGLOBAL = 0x25;

    const byte DUP        = 0x26;
    const byte SWAP       = 0x27;

    const byte SYSCALL0   = 0x28;
    const byte SYSCALL1   = 0x29;
    
    const byte COPYNEXTPOP = 0x2E;
    
    const byte DECSP      = 0x2F;
    const byte INCSP      = 0x30;

    const byte JZ         = 0x31;
    const byte JNZ        = 0x32;
    const byte J          = 0x33;
    const byte CALL       = 0x34;
    const byte ENTER      = 0x35;
    const byte RET        = 0x36;
    const byte RET0       = 0x37;
    const byte DELEGATE   = 0x38;
    const byte CALLREL    = 0x39;
    
    // Code Density
    const byte INCLOCAL        = 0x3A;
    const byte PUSHILT         = 0x3B;
    const byte PUSHLOCALPUSHI0 = 0x3C;
    const byte PUSHLOCALPUSHI1 = 0x3D;
    const byte PUSHLOCALX2     = 0x3E;
    const byte PUSHLOCALX2M    = 0x3F;
    const byte PUSHLOCALX2P    = 0x40;
    

    const byte DIE             = 0x41;
    
    const byte RegisterObjectSlot = 0;
    const byte RegisterObjectSlotRET = 1;
    
    const byte StringNewFromConstant = 3;
    const byte StringNewFromChar = 4;
    const byte StringNew = 5;
    const byte StringAppend = 6;
    const byte StringInsertChar = 7;
    const byte StringCompare = 8;
    const byte StringLengthGet = 9;
    
    const byte ArrayNew = 14;
    const byte ArrayCountGet = 15;
    const byte ArrayGetItem = 16;
    const byte ArraySetItem = 17;
    
    const byte ScreenPrint = 42;
    const byte ScreenPrintLn = 43;
    const byte ScreenClear = 44;

    uint hbp;
    uint hcsp;
    
    uint stackBase;
    uint callStack;
    uint screenBuffer;
    uint screenSize;
    byte screenRow;
    byte screenCol;
    
    uint GetStackSlot(uint address)
    {
        return memoryword[address];
    }
    SetStackSlot(uint address, uint value)
    {
        memoryword[address]   value;
    }
    StackPush(uint value)
    {
        memoryword[iy] = value;
        iy++;iy++;
    }
    uint StackPop()
    {
        iy--;iy--;
        return memoryword[iy];
    }
    
    uint MakeACopy(uint original)
    {
        // TODO: make this work for types other than strings
        uint length = StringLength(original);
        uint copy = MemoryAllocate(length+1);
        MemoryMove(copy, original, length);
        memory[copy+length] = 0;
        return copy;
    }
    
    ArrayNew()
    {
        // TODO : object manager!
        byte vtype = byte(StackPop()); // TODO : single byte cells for now : use type
        uint size = StackPop();
        uint arrayPtr = MemoryAllocate(size); 
        StackPush(arrayPtr);
    }
    ArrayGetItem()
    {
        uint index = StackPop();
        uint arrayPtr = StackPop();
        byte db = memory[arrayPtr + index];
        uint value = db;
        StackPush(value);
    }
    ArraySetItem()
    {
        uint value = StackPop();
        byte db = byte(value & 0xFF);
        uint index = StackPop();
        uint arrayPtr = StackPop();
        memory[arrayPtr+index]   = db;
    }
    
    StringNewFromConstant()
    {
        uint location = StackPop();
        location = location + 1;
        StackPush(location); 
      		// TODO: yeah, memory manager is going to have fun free-ing this!
   	}
    uint StringLength(uint str)
    {
        uint length = 0;
        loop
        {
            byte db = memory[str];
            if (0 == db)
            {
                break;
            }
            str++;
            length++;
        }
        return length;
    }
    StringAppend(uint overload)
    {
        uint str2;
        byte c;
        uint appendLength = 1;
        if (overload != 1)
        {
            str2 = StackPop();
            appendLength = StringLength(str2);
        }
        else
        {
            c = byte(StackPop());
        }
        uint str = StackPop();
        uint length = StringLength(str);
        uint nstr = MemoryAllocate(length+appendLength+1);
        MemoryMove(nstr, str, length);
        if (appendLength == 1)
        {
            memory[nstr+length] = c;
        }
        else
        {
            MemoryMove(nstr+length, str2, appendLength);
        }
        memory[nstr+length+appendLength] = 0;
        StackPush(nstr);
    }
    StringInsertChar()
    {
        byte db = byte(StackPop());
        uint index = StackPop();
        uint str = StackPop();
        uint length = StringLength(str);
        uint nstr = MemoryAllocate(length+2);
        uint i = 0;
        loop
        {
            if (i == length+2)
            {
                break;
            }
            if (i == index)
            {
                memory[nstr+i] = db;
            }
            else
            {
                memory[nstr+i] = memory[str];
                str++;
            }
            i++;
        }
        memory[nstr+length+1] = 0;
        StackPush(nstr);
    }
    StringNew()
    {
        uint nstr = MemoryAllocate(1);
        memory[nstr] = 0;
        StackPush(nstr);
    }
    
    ScreenInitialize()
    {
        screenSize = 0;
        uint rows = lcdRows;
        loop
        {
            screenSize = screenSize + lcdColumns;
            rows--;
            if (0 == rows)
            {
                break;
            }
        }
        screenBuffer = MemoryAllocate(screenSize);
        ScreenClear();
    }
    ScreenClear()
    {
        LCDClear();
        MemorySet(screenBuffer, 32, screenSize);
        screenRow = 0;
        screenCol = 0;
    }
    ScreenScrollUp()
    {
        LCDSetCursorPosition(0, 0);
        uint destination  = screenBuffer;
        uint src   = screenBuffer + lcdColumns;
        uint total = screenSize - lcdColumns;
        loop
        {
            byte db = memory[src];
            memory[destination] = db;
            destination++;
            src++;
            LCDCharacter(char(db));
            total--;
            if (0 == total)
            {
                break;
            }
        }
        total = lcdColumns;
        loop
        {
            memory[destination] = 32;
            destination++;
            LCDCharacter(' ');
            total--;
            if (0 == total)
            {
                break;
            }
        }
        screenCol = 0;
        screenRow = lcdRows-1;
    }
    //Redraw()
    //{
    //    LCDSetCursorPosition(0, 0);
    //    uint source = screenBuffer;
    //    uint total  = screenSize;
    //    loop
    //    {
    //        byte db = memory[source];
    //        source++;
    //        LCDCharacter(char(db));
    //        total--;
    //        if (0 == total)
    //        {
    //            break;
    //        }
    //    }
    //}
    
    ScreenPrint(uint overload)
    {
        // Print(string s,   uint foreColour, uint backColour) system;
        uint backColour = StackPop();
        uint foreColour = StackPop();
        uint chr;
        uint str;
        bool single;
        if (0 == overload)
        {
            single = true;
            chr = StackPop();
        }
        else
        {
            str = StackPop();
        }
        loop
        {
            byte db;
            if (single)
            {
                db = byte(chr);
            }
            else
            {
                db = memory[str];
            }
            if (0 == db)
            {
                break;
            }
            if (screenRow == lcdRows)
            {
                // we're about to draw below the screen so scroll up
                ScreenScrollUp();
            }
            uint bufferPosition = screenCol;
            uint row = screenRow;
            loop
            {
                if (0 == row)
                {
                    break;
                }
                bufferPosition = bufferPosition + lcdColumns;
                row--;
            }
            memory[screenBuffer+bufferPosition] = db;
            LCDCharacterAt(char(db), screenCol, screenRow);
            screenCol = screenCol + 1;
            if (screenCol == lcdColumns)
            {
                screenCol = 0;
                screenRow = screenRow+1;
            }
            if (single)
            {
                break;
            }
            str++;
        }
    }
    
    ScreenPrintLn()
    {
        // PrintLn() system;
        screenCol = 0;
        screenRow = screenRow+1;
    }
    
    // ##### SysCalls End #####
    
    PrintStep(byte opCode, uint operand, uint opPC)
    {
        PrintAt(0,  3, 'S', 'P', ' ', ' ', ' ', 'T', 'o', 'p', ' ', char(0));
        PrintAt(10, 3, 'N', 'e', 'x', 't', char(0), char(0), char(0), char(0), char(0), char(0));
        PrintAt(15, 3, 'N', 'x', 't', 'N', char(0), char(0), char(0), char(0), char(0), char(0));
        
        PrintHexAt(0,  1, opPC);
        PrintHexAt(12, 1, operand);
        PrintHexAt(0,  2, iy-stackBase);
        PrintHexAt(0,  3, hbp-stackBase);
        
        if (iy-stackBase > 4)
        {
            uint top = GetStackSlot(iy-2);
            PrintHexAt(5,  2, top);
            uint next = GetStackSlot(iy-4);
            PrintHexAt(10,   2, next);
            uint nextnext = GetStackSlot(iy-6);
            PrintHexAt(15,   2, nextnext);
        }
        else if (iy-stackBase > 2)
        {
            uint top = GetStackSlot(iy-2);
            PrintHexAt(5,  2, top);
            uint next = GetStackSlot(iy-4);
            PrintHexAt(10,   2, next);
            PrintAt(15, 2, ' ', ' ', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (iy-stackBase > 0)
        {
            uint top = GetStackSlot(iy-2);
            PrintHexAt(5,  2, top);
            PrintAt(10, 2, ' ', ' ', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
            PrintAt(15, 2, ' ', ' ', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        if (opCode == ADD)
        {
            PrintAt(6, 1, 'A', 'D', 'D', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == SUB)
        {
            PrintAt(6, 1, 'S', 'U', 'B', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == DIV)
        {
            PrintAt(6, 1, 'D', 'I', 'V', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == MUL)
        {
            PrintAt(6, 1, 'M', 'U', 'L', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == MOD)
        {
            PrintAt(6, 1, 'M', 'O', 'D', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == EQ)
        {
            PrintAt(6, 1, 'E', 'Q', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == NE)
        {
            PrintAt(6, 1, 'N', 'E', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == GT)
        {
            PrintAt(6, 1, 'G', 'T', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == LT)
        {
            PrintAt(6, 1, 'L', 'T', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == GE)
        {
            PrintAt(6, 1, 'G', 'E', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == LE)
        {
            PrintAt(6, 1, 'L', 'E', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == PUSHI)
        {
            PrintAt(6, 1, 'P', 'S', 'H', 'I', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == PUSHI0) 
        {
            PrintAt(6, 1, 'P', 'S', 'H', '0', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == PUSHI1) 
        {
            PrintAt(6, 1, 'P', 'S', 'H', '1', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == POPLOCAL) 
        {
            PrintAt(6, 1, 'P', 'O', 'P', 'L', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == PUSHLOCAL) 
        {
            PrintAt(6, 1, 'P', 'S', 'H', 'L', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == POPGLOBAL) 
        {
            PrintAt(6, 1, 'P', 'O', 'P', 'G', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == PUSHGLOBAL) 
        {
            PrintAt(6, 1, 'P', 'S', 'H', 'G', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == DUP) 
        {
            PrintAt(6, 1, 'D', 'U', 'P', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == SYSCALL0) 
        {
            PrintAt(6, 1, 'S', 'Y', 'S', '0', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == SYSCALL1) 
        {
            PrintAt(6, 1, 'S', 'Y', 'S', '1', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == COPYNEXTPOP) 
        {
            PrintAt(6, 1, 'C', 'P', 'N', 'P', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == DECSP) 
        {
            PrintAt(6, 1, 'D', 'C', 'S', 'P', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == JZ) 
        {
            PrintAt(6, 1, 'J', 'Z', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == JNZ) 
        {
            PrintAt(6, 1, 'J', 'N', 'Z', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == J) 
        {
            PrintAt(6, 1, 'J', ' ', ' ', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == CALL) 
        {
            PrintAt(6, 1, 'C', 'A', 'L', 'L', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == ENTER) 
        {
            PrintAt(6, 1, 'E', 'N', 'T', 'E', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == RET) 
        {
            PrintAt(6, 1, 'R', 'E', 'T', ' ', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else if (opCode == RET0) 
        {
            PrintAt(6, 1, 'R', 'E', 'T', '0', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        else
        {
            PrintHexAt(6,  1, opCode);
            PrintAt(0, 0, 'U', 'n', 'k', 'n', 'o', 'w', 'n', ' ', 'O', 'p');
        }
        ButtonWait(0b00000001); // single step
    }
    
    PushBoolAsUInt(bool result)
    {
        if (result)
        {
            StackPush(1);
        }
        else
        {
            StackPush(0);
        }
    }
    
    {
        bool stepping = false;
        
        LCDInitialize();
        
        LCDClear();
        PrintAt(0, 0, 'L', 'o', 'a', 'd', 'i', 'n', 'g', '.', '.', char(0));
        
        // takes 2 seconds to zero memory so put it after the first message is displayed
        MemoryInitialize(); 
        ScreenInitialize();
        
        PrintAt(0, 0, 'L', 'o', 'a', 'd', 'e', 'd', ' ', ' ', ' ', char(0));
        LCDSetCursorPosition(0, 0); 
        
        uint dataSize = ((memory[4] << 8) + memory[3]);
        //PrintHexAt(0,  1, dataSize);
        
        stackBase = MemoryAllocate(256);    // 128 slots
        callStack = MemoryAllocate(128);    //  64 slots
        
        pc = dataSize + 5;
        iy = stackBase;
        hbp = stackBase;
        hcsp = 0;
        
        byte opCode;
        uint localoperand;
        uint top;
        uint value;
        uint next;
        uint overload;
        uint address;
        byte msb;
        byte lsb;
        
        // fake return address for "Main"
        memoryword[callStack + hcsp] = 0;
        hcsp++; hcsp++;
        
        uint signature = memoryword[3];
        if (signature == 0xFEED)
        {
            PrintAt(0, 0, 'B', 'a', 'd', ' ', 'H', 'e', 'x', 'e', char(0), char(0));
            PrintAt(0, 1, ' ', 'u', 's', 'e', ' ', '-', 's', char(0), char(0), char(0));
            loop
            {
                // endless loop
            }
        }
        
        bool copyNextPop = false;
        loop
        {
            //if (pc-3 == 0xFFFF)
            //{
            //    stepping = true;
            //}
            
            
            //if (stepping)
            //{
            //    opCode  = memory[pc];
            //    localoperand = memoryword[pc+1];
            //    PrintStep(opCode, localoperand, pc);
            //}
            switch (memory[pc])
            {
                case PUSHI:
                {
                    memoryword[iy] = memoryword[pc+1]; // PUSHI
                    iy++;iy++;
                }
                case PUSHILT:
                {
                    if (memoryword[iy-2] < memoryword[pc+1]) // PUSHILT
                    {
                        memoryword[iy-2] = 1;
                    }
                    else
                    {
                        memoryword[iy-2] = 0;
                    }
                }
                case JZ:
                {
                    iy--;iy--; // JZ
                    if (0 == memoryword[iy]) 
                    {
                        pc = memoryword[pc+1] + pc; 
                        continue; // avoid pc += 3 below
                    }
                }
                case JNZ:
                {
                    iy--;iy--; // JNZ
                    if (0 != memoryword[iy]) 
                    {
                        pc = memoryword[pc+1] + pc; 
                        continue; // avoid pc += 3 below
                    }
                }
                case J:
                {
                    pc = memoryword[pc+1] + pc; // J
                    continue; // avoid pc += 3 below
                }
                case CALL:
                {
                    memoryword[callStack + hcsp] = pc+3; // CALL
                    hcsp++; hcsp++;
                    pc = memoryword[pc+1] + pc; 
                    continue; // avoid pc += 3 below
                }
                case RET:
                case RET0:
                {
                    //if (opCode == RET0)
                    //{
                    //    operand = 0; // should already be true
                    //}
                    
                    // clear the locals and arguments off the stack (return value is already dealt with if needed)
                    iy = iy - memoryword[pc+1];
                    
                    hcsp--; hcsp--;
                    hbp = memoryword[callStack + hcsp]; // POP BP
                    if (0 != hcsp)
                    {
                        hcsp--; hcsp--;
                        pc = memoryword[callStack + hcsp]; // POP PC
                    }
                    
                    // ObjectManager -> Remove Slots : TODO
                    
                    if (0 == hcsp)
                    {
                        break; // exiting "main"
                    }
                    continue; // avoid pc += 3 below
                }
                case ENTER:
                {
                    memoryword[callStack + hcsp] = hbp; // ENTER
                    hcsp++; hcsp++;
                    hbp = iy;
                }
                case PUSHI0:
                {
                    memoryword[iy] = 0; // PUSHI0
                    iy++;iy++;
                }
                case PUSHI1:
                {
                    memoryword[iy] = 1; // PUSHI1
                    iy++;iy++;
                }
                
                case ADD:
                {
                    iy--;iy--; // ADD
                    memoryword[iy-2] = memoryword[iy-2] + memoryword[iy];
                }
                case SUB:
                {
                    top = StackPop();
                    next = StackPop();
                    StackPush(next - top);
                }
                case DIV:
                {
                    top = StackPop();
                    next = StackPop();
                    StackPush(next / top);
                }
                case MUL:
                {
                    top = StackPop();
                    next = StackPop();
                    StackPush(next * top);
                }
                case MOD:
                {
                    top = StackPop();
                    next = StackPop();
                    StackPush(next % top);
                }
                case EQ:
                {
                    top = StackPop();
                    next = StackPop();
                    PushBoolAsUInt(next == top);
                }
                case NE:
                {
                    top = StackPop();
                    next = StackPop();
                    PushBoolAsUInt(next != top);
                }
                case GT:
                {
                    top = StackPop();
                    next = StackPop();
                    PushBoolAsUInt(next > top);
                }
                case LT:
                {
                    top = StackPop();
                    next = StackPop();
                    PushBoolAsUInt(next < top);
                }
                case GE:
                {
                    top = StackPop();
                    next = StackPop();
                    PushBoolAsUInt(next >= top);
                }
                case LE:
                {
                    iy--;iy--; // LE
                    if (memoryword[iy-2] <= memoryword[iy])
                    {
                        memoryword[iy-2] = 1;
                    }
                    else
                    {
                        memoryword[iy-2] = 0;
                    }
                }
                
                case POPLOCAL:
                {
                    iy--;iy--;// POPLOCAL
                    if (copyNextPop)
                    {
                        top = MakeACopy(top);
                        copyNextPop = false;
                        memoryword[memoryword[pc+1] + hbp] = memoryword[iy];
                    }
                    else
                    {
                        memoryword[memoryword[pc+1] + hbp] = memoryword[iy];
                    }
                }
                case INCLOCAL:
                {
                    memoryword[memoryword[pc+1] + hbp]++; // INCLOCAL
                }
                case PUSHLOCAL:
                {
                    memoryword[iy] = memoryword[memoryword[pc+1] + hbp]; // PUSHLOCAL
                    iy++;iy++;
                }
                case PUSHLOCALX2P:
                {
                    localoperand = memoryword[pc+1] + hbp; // PUSHLOCALX2P
                    memoryword[iy]   = memoryword[localoperand]; 
                    memoryword[iy+2] = memoryword[localoperand + 2];
                    iy++;iy++;iy++;iy++;
                }
                case PUSHLOCALX2M:
                {
                    localoperand = memoryword[pc+1] + hbp; // PUSHLOCALX2M
                    memoryword[iy]   = memoryword[localoperand]; 
                    memoryword[iy+2] = memoryword[localoperand - 2];
                    iy++;iy++;iy++;iy++;
                }
                case PUSHLOCALX2:
                {
                    value = memoryword[memoryword[pc+1] + hbp];  // PUSHLOCALX2
                    memoryword[iy]   = value;
                    memoryword[iy+2] = value;
                    iy++;iy++;iy++;iy++;
                }
                case PUSHLOCALPUSHI0:
                {
                    memoryword[iy]   = memoryword[memoryword[pc+1] + hbp]; // PUSHLOCALPUSHI0
                    memoryword[iy+2] = 0;
                    iy++;iy++; iy++;iy++;
                }
                case PUSHLOCALPUSHI1:
                { 
                    memoryword[iy]   = memoryword[memoryword[pc+1] + hbp]; // PUSHLOCALPUSHI1
                    memoryword[iy+2] = 1;
                    iy++;iy++;iy++;iy++;
                }
                
                case POPGLOBAL:
                {
                    iy--;iy--; // POPGLOBAL
                    if (copyNextPop)
                    {
                        memoryword[stackBase + memoryword[pc+1]] = MakeACopy(memoryword[iy]);
                        copyNextPop = false;
                    }
                    else
                    {
                        memoryword[stackBase + memoryword[pc+1]] = memoryword[iy];
                    }
                }
                case PUSHGLOBAL:
                {
                    memoryword[iy] = memoryword[stackBase + memoryword[pc+1]];     // PUSHGLOBAL
                    iy++;iy++;
                }
                case DUP:
                {
                    top = GetStackSlot(iy - memoryword[pc+1]); // DUP
                    StackPush(top);
                }
                case COPYNEXTPOP:
                {
                    copyNextPop = true;
                }
                case DECSP:
                {
                    iy = iy - memoryword[pc+1]; // DECSP
                }
                
                
                case SYSCALL0:
                case SYSCALL1:
                {
                    overload = 0;
                    opCode  = memory[pc];
                    localoperand = memoryword[pc+1];
                    if (opCode == SYSCALL1)
                    {
                        overload = 1;
                    }
                    if (localoperand == RegisterObjectSlotRET)
                    {
                        // TODO
                    }
                    else if (localoperand == StringNewFromConstant)
                    {
                        StringNewFromConstant();
                    }
                    else if (localoperand == StringNew)
                    {
                        StringNew();
                    }
                    else if (localoperand == StringAppend)
                    {
                        StringAppend(overload);
                    }
                    else if (localoperand == StringInsertChar)
                    {
                        StringInsertChar();
                    }
                    else if (localoperand == StringLengthGet)
                    {
                        StackPush(StringLength(StackPop()));
                    }
                    else if (localoperand == ArrayNew)
                    {
                        ArrayNew();
                    }
                    else if (localoperand == ArrayGetItem)
                    {
                        ArrayGetItem();
                    }
                    else if (localoperand == ArraySetItem)
                    {
                        ArraySetItem();
                    }
                    else if (localoperand == ScreenPrint)
                    {
                        ScreenPrint(overload);
                    }
                    else if (localoperand == ScreenPrintLn)
                    {
                        ScreenPrintLn();
                    }
                    else if (localoperand == ScreenClear)
                    {
                        ScreenClear();
                    }
                    else
                    {
                        PrintAt(0, 0, 'U', 'n', 'k', 'n', 'o', 'w', 'n', 'S', 'y', 's');
                        PrintHexAt(6,  1, localoperand);
                    }
                }
                default:
                {
                    opCode  = memory[pc];
                    localoperand = memoryword[pc+1];
                    PrintAt(0,  3, 'S', 'P', ' ', ' ', ' ', 'T', 'o', 'p', ' ', char(0));
                    PrintAt(10, 3, 'N', 'e', 'x', 't', char(0), char(0), char(0), char(0), char(0), char(0));
                    PrintAt(15, 3, 'N', 'x', 't', 'N', char(0), char(0), char(0), char(0), char(0), char(0));
                    PrintStep(opCode, localoperand, pc);
                }
            } // switch
            pc++;
            pc++;
            pc++;
        }

        MemoryFree(screenBuffer);
        MemoryFree(stackBase);
        MemoryFree(callStack);
        PrintAt(14, 3, 'H', 'a', 'l', 't', 'e', 'd',  char(0),  char(0),  char(0), char(0));
    }
}

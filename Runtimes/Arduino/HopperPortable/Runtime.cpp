#include "Platform.h"
#include "HopperFile.h"
#include "HopperWiFi.h"
#include "HopperTimer.h"
#include "HopperScreen.h"



Bool Runtime_loaded = false;
UInt Runtime_currentCRC = 0;
Byte Minimal_error = 0;
UInt Memory_heapStart = 0x8000;
UInt Memory_heapSize = 0x4000;
UInt Memory_freeList = 0;
UInt HRArray_setSlots = 0;
UInt HRArray_clearSlots = 0;
Bool External_interruptsDisabled = false;
Bool Library_isrExists = false;
UInt HopperVM_binaryAddress = 0;
UInt HopperVM_programSize = 0;
UInt HopperVM_binaryVersion = 0;
UInt HopperVM_constAddress = 0;
UInt HopperVM_methodTable = 0;
UInt HopperVM_programOffset = 0;
UInt HopperVM_keyboardBuffer = 0;
UInt HopperVM_valueStackLSBPage = 0;
UInt HopperVM_valueStackMSBPage = 0;
UInt HopperVM_typeStackPage = 0;
UInt HopperVM_callStackLSBPage = 0;
UInt HopperVM_callStackMSBPage = 0;
UInt HopperVM_dataMemory = 0;
UInt HopperVM_breakpoints = 0;
Bool HopperVM_breakpointExists = false;
UInt HopperVM_currentDirectory = 0;
UInt HopperVM_currentArguments = 0;
UInt HopperVM_pc = 0;
Byte HopperVM_gp = 0;
Byte HopperVM_sp = 0;
Byte HopperVM_bp = 0;
Byte HopperVM_csp = 0;
Byte HopperVM_cspStart = 0;
Bool HopperVM_cnp = false;
Bool HopperVM_inDebugger = false;
OpCode HopperVM_opCode = (OpCode)0;
UInt HopperVM_jumpTable = 0;
UInt HopperVM_pcStore = 0;
Bool IO_echoToLCD = false;
UInt IO_keyboardBufferBase = 0;
UInt IO_keyboardInPointer = 0;
UInt IO_keyboardOutPointer = 0;
UInt Screen_defaultForeColour = Colour_MatrixGreen_Get();
UInt Screen_defaultBackColour = Colour_Black_Get();
    
void HopperEntryPoint()
{
    Runtime_loaded = false;
    HopperVM_Restart();
    Bool refresh = true;
    if (External_LoadAuto_Get())
    {
        UInt loadedAddress = 0;
        UInt codeLength = 0;
        UInt startAddress = 0;
        UInt autoPath = HopperVM_GetAppName(false);
        if (Runtime_LoadHexe_R(autoPath, startAddress, loadedAddress, codeLength, true))
        {
            HopperVM_Initialize(loadedAddress, codeLength);
            HopperVM_Restart();
            Runtime_loaded = true;
            if (HopperVM_InlinedExecuteWarp(false))
            {
                HopperVM_Restart();
            }
        }
        GC_Release(autoPath);
    }
    Runtime_SerialWriteChar(Char(92));
    for (;;)
    {
        Char ch = 0;
        if (Serial_IsAvailable_Get())
        {
            ch = Runtime_SerialReadChar();
        }
        if (ch == Char(0x03))
        {
            Runtime_SerialWriteChar(Char(92));
        }
        else if (ch == Char(27))
        {
            Runtime_SerialWriteChar(Char(92));
            ch = Runtime_SerialReadChar();
            switch (ch)
            {
            case 'F':
            {
                Byte msn = Runtime_FromHex(Runtime_SerialReadChar());
                Byte lsn = Runtime_FromHex(Runtime_SerialReadChar());
                Runtime_WaitForEnter();
                Byte iPage = (msn << 0x04) + lsn;
                Runtime_DumpPage(iPage);
                Runtime_SerialWriteChar(Char(92));
                break;
            }
            case 'B':
            {
                Char arg = Runtime_SerialReadChar();
                if (arg == 'X')
                {
                    HopperVM_ClearBreakpoints(false);
                }
                else
                {
                    Byte n = Runtime_FromHex(arg);
                    Byte a3 = Runtime_FromHex(Runtime_SerialReadChar());
                    Byte a2 = Runtime_FromHex(Runtime_SerialReadChar());
                    Byte a1 = Runtime_FromHex(Runtime_SerialReadChar());
                    Byte a0 = Runtime_FromHex(Runtime_SerialReadChar());
                    UInt address = (a3 << 0x0C) + (a2 << 0x08) + (a1 << 0x04) + a0;
                    HopperVM_SetBreakpoint(n, address);
                }
                Runtime_WaitForEnter();
                break;
            }
            case 'P':
            {
                Runtime_WaitForEnter();
                UInt serialBuffer = HRString_New();
                HRString_BuildChar_R(serialBuffer, Char(13));
                Runtime_out4Hex_R(serialBuffer, HopperVM_PC_Get());
                HRString_BuildChar_R(serialBuffer, Char(92));
                External_SerialWriteString(serialBuffer);
                GC_Release(serialBuffer);
                break;
            }
            case 'K':
            {
                Runtime_WaitForEnter();
                UInt serialBuffer = HRString_New();
                HRString_BuildChar_R(serialBuffer, Char(13));
                Runtime_out4Hex_R(serialBuffer, Runtime_currentCRC);
                HRString_BuildChar_R(serialBuffer, Char(92));
                External_SerialWriteString(serialBuffer);
                GC_Release(serialBuffer);
                break;
            }
            case 'E':
            {
                Runtime_WaitForEnter();
                External_MCUReboot(true);
                break;
            }
            case 'T':
            {
                Runtime_WaitForEnter();
                UInt destinationName = HRString_New();
                for (;;)
                {
                    Char rc = Runtime_SerialReadChar();
                    if (rc == Char(13))
                    {
                        break;;
                    }
                    HRString_BuildChar_R(destinationName, rc);
                }
                Runtime_SerialWriteChar(Char(13));
                Runtime_SerialWriteChar(Char(92));
                UInt destinationFolder = HRString_New();
                for (;;)
                {
                    Char rc = Runtime_SerialReadChar();
                    if (rc == Char(13))
                    {
                        break;;
                    }
                    HRString_BuildChar_R(destinationFolder, rc);
                }
                Runtime_SerialWriteChar(Char(13));
                Runtime_SerialWriteChar(Char(92));
                HRDirectory_Create(destinationFolder);
                Char h3 = Runtime_SerialReadChar();
                Char h2 = Runtime_SerialReadChar();
                Char h1 = Runtime_SerialReadChar();
                Char h0 = Runtime_SerialReadChar();
                UInt size = (Runtime_FromHex(h3) << 0x0C) + (Runtime_FromHex(h2) << 0x08) + (Runtime_FromHex(h1) << 0x04) + Runtime_FromHex(h0);
                Runtime_SerialWriteChar(Char(13));
                Runtime_SerialWriteChar(Char(92));
                UInt fh = HRFile_Create(destinationName);
                while (size != 0x00)
                {
                    Char n1 = Runtime_SerialReadChar();
                    Char n0 = Runtime_SerialReadChar();
                    Byte b = (Runtime_FromHex(n1) << 0x04) + Runtime_FromHex(n0);
                    HRFile_Append(fh, b);
                    
                    size--;
                }
                HRFile_Flush(fh);
                Runtime_SerialWriteChar(Char(13));
                Runtime_SerialWriteChar(Char(92));
                break;
            }
            case 'L':
            {
                Runtime_WaitForEnter();
                UInt loadedAddress = 0x00;
                UInt codeLength = 0;
                Runtime_loaded = Runtime_SerialLoadIHex_R(loadedAddress, codeLength);
                Runtime_SerialWriteChar(Char(13));
                if (Runtime_loaded)
                {
                    HopperVM_Initialize(loadedAddress, codeLength);
                    HopperVM_Restart();
                    UInt serialBuffer = HRString_New();
                    Runtime_out4Hex_R(serialBuffer, codeLength);
                    HRString_BuildChar_R(serialBuffer, ' ');
                    Runtime_out4Hex_R(serialBuffer, Memory_HeapStart_Get());
                    HRString_BuildChar_R(serialBuffer, ' ');
                    Runtime_out4Hex_R(serialBuffer, Memory_HeapSize_Get());
                    HRString_BuildChar_R(serialBuffer, ' ');
                    External_SerialWriteString(serialBuffer);
                    GC_Release(serialBuffer);
                }
                for (;;)
                {
                    ch = Runtime_SerialReadChar();
                    if ((ch == '!') || (ch == '*'))
                    {
                        break;;
                    }
                }
                Runtime_SerialWriteChar(Char(13));
                Runtime_SerialWriteChar((Runtime_loaded) ? ('*') : ('!'));
                Runtime_SerialWriteChar(Char(92));
                if (Runtime_loaded)
                {
                    HopperVM_FlashProgram(loadedAddress, codeLength, Runtime_currentCRC);
                }
                break;
            }
            default:
            {
                if (Runtime_loaded)
                {
                    switch (ch)
                    {
                    case 'O':
                    {
                        Runtime_WaitForEnter();
                        Bool restart = false;
                        UInt pc = HopperVM_PC_Get();
                        OpCode opCode = OpCode(Memory_ReadProgramByte(pc));
                        if ((opCode == OpCode::eCALL) || (opCode == OpCode::eCALLI))
                        {
                            HopperVM_SetBreakpoint(0x00, pc + 0x03);
                            restart = HopperVM_Execute();
                        }
                        else if (opCode == OpCode::eCALLB)
                        {
                            HopperVM_SetBreakpoint(0x00, pc + 0x02);
                            restart = HopperVM_Execute();
                        }
                        else
                        {
                            restart = HopperVM_ExecuteStepTo();
                        }
                        if (restart)
                        {
                            HopperVM_Restart();
                        }
                        Runtime_SerialWriteChar(Char(92));
                        break;
                    }
                    case 'I':
                    {
                        Runtime_WaitForEnter();
                        if (HopperVM_ExecuteStepTo())
                        {
                            HopperVM_Restart();
                        }
                        Runtime_SerialWriteChar(Char(92));
                        break;
                    }
                    case 'D':
                    {
                        Runtime_WaitForEnter();
                        if (HopperVM_Execute())
                        {
                            HopperVM_Restart();
                        }
                        Runtime_SerialWriteChar(Char(92));
                        break;
                    }
                    case 'X':
                    {
                        Runtime_WaitForEnter();
                        if (HopperVM_InlinedExecuteWarp(false))
                        {
                            HopperVM_Restart();
                        }
                        Runtime_SerialWriteChar(Char(92));
                        break;
                    }
                    case 'W':
                    {
                        Runtime_WaitForEnter();
                        HopperVM_Restart();
                        break;
                    }
                    } // switch
                }
                break;
            }
            } // switch
        }
    }
    HopperVM_Release();
}

Bool Runtime_LoadHexe_R(UInt path, UInt startAddress, UInt & loadedAddress, UInt & codeLength, Bool doCRC)
{
    Bool success = false;
    loadedAddress = startAddress;
    UInt address = 0x00;
    if (HRFile_Exists(path))
    {
        success = External_ReadAllCodeBytes_R(path, loadedAddress, codeLength);
        if (doCRC)
        {
            UInt crcpath = HopperVM_GetAppName(true);
            UInt crcFile = HRFile_Open(path);
            Byte crc0 = HRFile_Read(crcFile);
            Byte crc1 = HRFile_Read(crcFile);
            Runtime_currentCRC = crc0 + (crc1 << 0x08);
            GC_Release(crcFile);
            GC_Release(crcpath);
        }
    }
    return success;
}

void Runtime_SerialWriteChar(Char c)
{
    Serial_WriteChar(c);
}

Char Runtime_SerialReadChar()
{
    Char c = Serial_ReadChar();
    return c;
}

Byte Runtime_FromHex(Char ch)
{
    switch (ch)
    {
    case '0':
    {
        return 0x00;
        break;
    }
    case '1':
    {
        return 0x01;
        break;
    }
    case '2':
    {
        return 0x02;
        break;
    }
    case '3':
    {
        return 0x03;
        break;
    }
    case '4':
    {
        return 0x04;
        break;
    }
    case '5':
    {
        return 0x05;
        break;
    }
    case '6':
    {
        return 0x06;
        break;
    }
    case '7':
    {
        return 0x07;
        break;
    }
    case '8':
    {
        return 0x08;
        break;
    }
    case '9':
    {
        return 0x09;
        break;
    }
    case 'a':
    case 'A':
    {
        return 0x0A;
        break;
    }
    case 'b':
    case 'B':
    {
        return 0x0B;
        break;
    }
    case 'c':
    case 'C':
    {
        return 0x0C;
        break;
    }
    case 'd':
    case 'D':
    {
        return 0x0D;
        break;
    }
    case 'e':
    case 'E':
    {
        return 0x0E;
        break;
    }
    case 'f':
    case 'F':
    {
        return 0x0F;
        break;
    }
    } // switch
    return 0x00;
}

void Runtime_WaitForEnter()
{
    for (;;)
    {
        Char ch = Runtime_SerialReadChar();
        if (ch == Char(13))
        {
            break;;
        }
    }
    Runtime_SerialWriteChar(Char(92));
}

void Runtime_DumpPage(Byte iPage)
{
    UInt pageBuffer = HRString_New();
    UInt rowAddress = (iPage << 0x08);;
    for (Byte row = 0x00; row < 0x10; row++)
    {
        if (iPage == 0x00)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte data = 0;
                Byte address = col + (row << 0x04);
                switch (address)
                {
                case 0:
                {
                    data = Byte(HopperVM_PC_Get() & 0xFF);
                    break;
                }
                case 1:
                {
                    data = Byte(HopperVM_PC_Get() >> 0x08);
                    break;
                }
                case 13:
                case 14:
                {
                    data = 0x00;
                    break;
                }
                case 3:
                {
                    data = HopperVM_SP_Get();
                    break;
                }
                case 4:
                {
                    data = HopperVM_BP_Get();
                    break;
                }
                case 5:
                {
                    data = HopperVM_CSP_Get();
                    break;
                }
                case 15:
                {
                    data = (HopperVM_CNP_Get()) ? (0x01) : (0x00);
                    break;
                }
                case 2:
                {
                    HopperFlags flgs = HopperFlags::eMCUPlatform;
                    if (HopperVM_BreakpointExists_Get())
                    {
                        flgs = (HopperFlags)(flgs | HopperFlags::eBreakpointsSet);
                    }
                    if (Runtime_loaded)
                    {
                        flgs = (HopperFlags)(flgs | HopperFlags::eProgramLoaded);
                    }
                    data = Byte(flgs);
                    break;
                }
                case 6:
                {
                    data = Byte(Memory_FreeList_Get() & 0xFF);
                    break;
                }
                case 7:
                {
                    data = Byte(Memory_FreeList_Get() >> 0x08);
                    break;
                }
                case 8:
                {
                    data = Byte(Memory_HeapStart_Get() >> 0x08);
                    break;
                }
                case 9:
                {
                    data = Byte(Memory_HeapSize_Get() >> 0x08);
                    break;
                }
                default:
                {
                    if ((address >= 32) && (address <= 32 + 0x0F))
                    {
                        data = Byte(HopperVM_GetBreakpoint(address - 32) & 0xFF);
                    }
                    else if ((address >= 48) && (address <= 48 + 0x0F))
                    {
                        data = Byte(HopperVM_GetBreakpoint(address - 48) >> 0x08);
                    }
                    else
                    {
                        data = 0x00;
                    }
                    break;
                }
                } // switch
                Runtime_out2HexOrDot_R(pageBuffer, data);
            }
        }
        else if (iPage == 0x03)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte address = col + (row << 0x04);
                UInt stackData = HopperVM_GetCS(address);
                Runtime_out2HexOrDot_R(pageBuffer, Byte(stackData & 0xFF));
            }
        }
        else if (iPage == 0x04)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte address = col + (row << 0x04);
                UInt stackData = HopperVM_GetCS(address);
                Runtime_out2HexOrDot_R(pageBuffer, Byte(stackData >> 0x08));
            }
        }
        else if (iPage == 0x05)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte address = col + (row << 0x04);
                Type htype = (Type)0;
                UInt stackData = HopperVM_Get_R(address, htype);
                Runtime_out2HexOrDot_R(pageBuffer, Byte(htype));
            }
        }
        else if (iPage == 0x06)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte address = col + (row << 0x04);
                UInt stackData = HopperVM_Get(address);
                Runtime_out2HexOrDot_R(pageBuffer, Byte(stackData & 0xFF));
            }
        }
        else if (iPage == 0x07)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Byte address = col + (row << 0x04);
                UInt stackData = HopperVM_Get(address);
                Runtime_out2HexOrDot_R(pageBuffer, Byte(stackData >> 0x08));
            }
        }
        else if (iPage > 0x07)
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                UInt address = col + (row << 0x04);
                address = address + (iPage << 0x08);
                Runtime_out2HexOrDot_R(pageBuffer, Memory_ReadByte(address));
            }
        }
        else
        {;
            for (Byte col = 0x00; col < 0x10; col++)
            {
                Runtime_out2HexOrDot_R(pageBuffer, 0x00);
            }
        }
    }
    Int length = Int(HRString_GetLength(pageBuffer));
    Int iLast = -0x01;
    Int i = length - 0x01;
    for (;;)
    {
        Char ch = HRString_GetChar(pageBuffer, UInt(i));
        if ((ch != '0') && (ch != '.'))
        {
            iLast = i;
            break;;
        }
        if (i == 0x00)
        {
            break;;
        }
        
        i--;
    }
    if (iLast != length - 0x01)
    {
        
        iLast++;
        HRString_SetChar(pageBuffer, UInt(iLast), '+');
        HRString_SetLength(pageBuffer, UInt(iLast + 0x01));
    }
    External_SerialWriteString(pageBuffer);
    GC_Release(pageBuffer);
}

void Runtime_out4Hex_R(UInt & pageBuffer, UInt value)
{
    Byte b = Byte(value >> 0x0C);
    HRString_BuildChar_R(pageBuffer, HRByte_ToHex(b));
    b = Byte((value >> 0x08) & 0x0F);
    HRString_BuildChar_R(pageBuffer, HRByte_ToHex(b));
    b = Byte((value >> 0x04) & 0x0F);
    HRString_BuildChar_R(pageBuffer, HRByte_ToHex(b));
    b = Byte(value & 0x0F);
    HRString_BuildChar_R(pageBuffer, HRByte_ToHex(b));
}

Bool Runtime_SerialLoadIHex_R(UInt & loadedAddress, UInt & codeLength)
{
    Bool success = true;
    loadedAddress = 0;
    Runtime_currentCRC = 0x00;
    Byte crc0 = 0;
    Byte crc1 = 0;
    if (!Runtime_TryReadSerialByte_R(crc0))
    {
        success = false;
    }
    if (success)
    {
        if (!Runtime_TryReadSerialByte_R(crc1))
        {
            success = false;
        }
    }
    if (success)
    {
        Char eol = Runtime_SerialReadChar();
        if ((eol != Char(0x0D)) && (eol != Char(0x0A)))
        {
            success = false;
        }
        else
        {
            Runtime_currentCRC = crc0 + (crc1 << 0x08);
        }
    }
    UInt codeLimit = External_GetSegmentPages() << 0x08;
    codeLength = 0x00;
    for (;;)
    {
        if (!success)
        {
            break;;
        }
        Char colon = Runtime_SerialReadChar();
        if (colon != ':')
        {
            success = false;
            break;;
        }
        Byte byteCount = 0;
        if (!Runtime_TryReadSerialByte_R(byteCount))
        {
            success = false;
            break;;
        }
        Byte lsb = 0;
        Byte msb = 0;
        if (!Runtime_TryReadSerialByte_R(msb))
        {
            success = false;
            break;;
        }
        if (!Runtime_TryReadSerialByte_R(lsb))
        {
            success = false;
            break;;
        }
        UInt recordAddress = lsb + (msb << 0x08);
        Byte recordType = 0;
        if (!Runtime_TryReadSerialByte_R(recordType))
        {
            success = false;
            break;;
        }
        switch (recordType)
        {
        case 0x00:
        {;
            for (UInt c = 0x00; c < byteCount; c++)
            {
                Byte dataByte = 0;
                if (!Runtime_TryReadSerialByte_R(dataByte))
                {
                    success = false;
                    break;;
                }
                if (0 + recordAddress >= codeLimit)
                {
                    success = false;
                    break;;
                }
                Memory_WriteCodeByte(0 + recordAddress, dataByte);
                
                codeLength++;
                
                recordAddress++;
            }
            Byte checkSum = 0;
            if (!Runtime_TryReadSerialByte_R(checkSum))
            {
                success = false;
                break;;
            }
            Char eol = Runtime_SerialReadChar();
            if ((eol != Char(0x0D)) && (eol != Char(0x0A)))
            {
                success = false;
                break;;
            }
            continue;;
            break;
        }
        case 0x01:
        {
            Byte checkSum = 0;
            if (!Runtime_TryReadSerialByte_R(checkSum))
            {
                success = false;
                break;;
            }
            break;;
            break;
        }
        default:
        {
            success = false;
            break;;
            break;
        }
        } // switch
        break;;
    }
    return success;
}

void Runtime_out2HexOrDot_R(UInt & pageBuffer, Byte value)
{
    if (value == 0x00)
    {
        HRString_BuildChar_R(pageBuffer, '.');
    }
    else
    {
        Byte b = Byte((value >> 0x04) & 0x0F);
        HRString_BuildChar_R(pageBuffer, HRByte_ToHex(b));
        b = Byte(value & 0x0F);
        HRString_BuildChar_R(pageBuffer, HRByte_ToHex(b));
    }
}

Bool Runtime_TryReadSerialByte_R(Byte & data)
{
    Char c0 = Runtime_SerialReadChar();
    Char c1 = Runtime_SerialReadChar();
    Byte msn = Runtime_FromHex(c0);
    Byte lsn = Runtime_FromHex(c1);
    data = (msn << 0x04) + lsn;
    return true;
}

UInt Colour_MatrixGreen_Get()
{
    return 0x07F7;
}

UInt Colour_Black_Get()
{
    return 0x00;
}

void HopperVM_Restart()
{
    External_MCUClockSpeedSet(0x85);
    HopperVM_DataMemoryReset();
    HopperVM_DiskSetup();
    External_TimerInitialize();
    HopperVM_sp = 0x00;
    HopperVM_gp = 0x00;
    HopperVM_bp = 0x00;
    HopperVM_csp = 0x00;
    HopperVM_cspStart = HopperVM_csp;
    Minimal_Error_Set(0x00);
    HopperVM_cnp = false;
    UInt version = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x00);
    UInt entryPoint = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x04);
    if (version > 0x00)
    {
        HopperVM_pc = 0x00;
        HopperVM_programOffset = entryPoint;
    }
    else
    {
        HopperVM_pc = entryPoint;
        HopperVM_programOffset = 0x00;
    }
    External_SetProgramOffset(HopperVM_programOffset);
}

UInt HopperVM_GetAppName(Bool crc)
{
    UInt path = HRString_New();
    HRString_BuildChar_R(path, Char('/'));
    HRString_BuildChar_R(path, Char('B'));
    HRString_BuildChar_R(path, Char('i'));
    HRString_BuildChar_R(path, Char('n'));
    HRString_BuildChar_R(path, Char('/'));
    HRString_BuildChar_R(path, Char('A'));
    HRString_BuildChar_R(path, Char('u'));
    HRString_BuildChar_R(path, Char('t'));
    HRString_BuildChar_R(path, Char('o'));
    HRString_BuildChar_R(path, Char('.'));
    if (crc)
    {
        HRString_BuildChar_R(path, Char('c'));
        HRString_BuildChar_R(path, Char('r'));
        HRString_BuildChar_R(path, Char('c'));
    }
    else
    {
        HRString_BuildChar_R(path, Char('h'));
        HRString_BuildChar_R(path, Char('e'));
        HRString_BuildChar_R(path, Char('x'));
        HRString_BuildChar_R(path, Char('e'));
    }
    return path;
}

void HopperVM_Initialize(UInt loadedAddress, UInt loadedSize)
{
    HopperVM_binaryAddress = loadedAddress;
    HopperVM_programSize = loadedSize;
    HopperVM_binaryVersion = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x00);
    HopperVM_constAddress = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x02);
    HopperVM_methodTable = HopperVM_binaryAddress + 0x06;
}

void HopperVM_ClearBreakpoints(Bool includingZero)
{;
    for (Byte i = 0x02; i < 0x20; i = i + 0x02)
    {
        Memory_WriteWord(HopperVM_breakpoints + i, 0x00);
    }
    if (includingZero)
    {
        Memory_WriteWord(HopperVM_breakpoints, 0x00);
        HopperVM_breakpointExists = false;
    }
    else
    {
        HopperVM_breakpointExists = Memory_ReadWord(HopperVM_breakpoints) != 0x00;
    }
}

void HopperVM_SetBreakpoint(Byte n, UInt address)
{
    Memory_WriteWord(HopperVM_breakpoints + n * 0x02, address);
    if (address != 0x00)
    {
        HopperVM_breakpointExists = true;
    }
    else
    {
        HopperVM_breakpointExists = false;;
        for (Byte i = 0x00; i < 0x20; i = i + 0x02)
        {
            if (Memory_ReadWord(HopperVM_breakpoints + i) != 0x00)
            {
                HopperVM_breakpointExists = true;
                break;;
            }
        }
    }
}

UInt HopperVM_PC_Get()
{
    return HopperVM_pc;
}

void HopperVM_FlashProgram(UInt codeLocation, UInt codeLength, UInt crc)
{
    UInt path = HopperVM_GetAppName(false);
    UInt appFile = HRFile_CreateFromCode(path, codeLocation, codeLength);
    HRFile_Flush(appFile);
    GC_Release(appFile);
    GC_Release(path);
    path = HopperVM_GetAppName(true);
    UInt crcFile = HRFile_Create(path);
    HRFile_Append(crcFile, Byte(crc & 0xFF));
    HRFile_Append(crcFile, Byte(crc >> 0x08));
    HRFile_Flush(crcFile);
    GC_Release(crcFile);
    GC_Release(path);
}

Bool HopperVM_Execute()
{
    Bool restart = false;
    HopperVM_inDebugger = true;
    for (;;)
    {
        Bool doNext = HopperVM_ExecuteOpCode();
        if (Minimal_Error_Get() != 0x00)
        {
            HopperVM_WriteERROR();
            break;;
        }
        if (HopperVM_pc == 0x00)
        {
            restart = true;
            break;;
        }
        if (IO_IsBreak())
        {
            HopperVM_WriteBREAK();
            break;;
        }
        if (HopperVM_BreakpointExists_Get())
        {
            Byte iBreak = 0;
            Bool atBreakpoint = false;;
            for (iBreak = 0x00; iBreak < 0x10; iBreak++)
            {
                if (HopperVM_GetBreakpoint(iBreak) == HopperVM_pc)
                {
                    if (iBreak == 0x00)
                    {
                        HopperVM_SetBreakpoint(0x00, 0x00);
                    }
                    HopperVM_inDebugger = false;
                    return restart;
                }
            }
        }
        External_WatchDog();
    }
    HopperVM_inDebugger = false;
    return restart;
}

Bool HopperVM_ExecuteStepTo()
{
    Bool restart = false;
    HopperVM_inDebugger = true;
    Bool doNext = HopperVM_ExecuteOpCode();
    if (Minimal_Error_Get() != 0x00)
    {
        HopperVM_WriteERROR();
    }
    else if (HopperVM_pc == 0x00)
    {
        restart = true;
    }
    HopperVM_inDebugger = false;
    return restart;
}

void HopperVM_Release()
{
    HRArray_Release();
    External_WebServerRelease();
    Memory_Free(HopperVM_breakpoints);
    HopperVM_breakpoints = 0x00;
    if (HopperVM_currentDirectory != 0x00)
    {
        GC_Release(HopperVM_currentDirectory);
        HopperVM_currentDirectory = 0x00;
    }
    if (HopperVM_currentArguments != 0x00)
    {
        GC_Release(HopperVM_currentArguments);
        HopperVM_currentArguments = 0x00;
    }
}

Byte HopperVM_SP_Get()
{
    return HopperVM_sp;
}

Byte HopperVM_BP_Get()
{
    return HopperVM_bp;
}

Byte HopperVM_CSP_Get()
{
    return HopperVM_csp;
}

Bool HopperVM_CNP_Get()
{
    return HopperVM_cnp;
}

Bool HopperVM_BreakpointExists_Get()
{
    return HopperVM_breakpointExists;
}

UInt HopperVM_GetBreakpoint(Byte n)
{
    return Memory_ReadWord(HopperVM_breakpoints + n * 0x02);
}

UInt HopperVM_GetCS(Byte address)
{
    return Memory_ReadByte(HopperVM_callStackLSBPage + address) + (Memory_ReadByte(HopperVM_callStackMSBPage + address) << 0x08);
}

UInt HopperVM_Get_R(Byte address, Type & htype)
{
    htype = Type(Memory_ReadByte(HopperVM_typeStackPage + address));
    return Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08);
}

UInt HopperVM_Get(Byte address)
{
    return Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08);
}

void HopperVM_DataMemoryReset()
{
    HRArray_Release();
    External_WebServerRelease();
    UInt nextAddress = 0;
    HopperVM_callStackLSBPage = nextAddress;
    nextAddress = nextAddress + 0x0100;
    HopperVM_callStackMSBPage = nextAddress;
    nextAddress = nextAddress + 0x0100;
    HopperVM_typeStackPage = nextAddress;
    nextAddress = nextAddress + 0x0100;
    HopperVM_valueStackLSBPage = nextAddress;
    nextAddress = nextAddress + 0x0100;
    HopperVM_valueStackMSBPage = nextAddress;
    nextAddress = nextAddress + 0x0100;
    HopperVM_jumpTable = nextAddress;
    nextAddress = nextAddress + 512;
    HopperVM_keyboardBuffer = nextAddress;
    nextAddress = nextAddress + 256;
    HopperVM_dataMemory = nextAddress;
    if (HopperVM_dataMemory < 0x0800)
    {
        HopperVM_dataMemory = 0x0800;
    }
    UInt i = 0x00;
    for (;;)
    {
        Memory_WriteWord(i, 0x00);
        i = i + 0x02;
        if (i == HopperVM_dataMemory)
        {
            break;;
        }
    }
    IO_AssignKeyboardBuffer(HopperVM_keyboardBuffer);
    Instructions_PopulateJumpTable(HopperVM_jumpTable);
    Memory_Initialize(HopperVM_dataMemory, (External_GetSegmentPages() << 0x08) - HopperVM_dataMemory);
    HopperVM_breakpoints = Memory_Allocate(0x20);
    HopperVM_ClearBreakpoints(true);
    HRArray_Initialize();
    HopperVM_currentDirectory = 0x00;
    HopperVM_currentArguments = 0x00;
}

void HopperVM_DiskSetup()
{
    UInt path = HRString_New();
    HRString_BuildChar_R(path, Char('/'));
    HRString_BuildChar_R(path, Char('B'));
    HRString_BuildChar_R(path, Char('i'));
    HRString_BuildChar_R(path, Char('n'));
    if (!HRDirectory_Exists(path))
    {
        HRDirectory_Create(path);
    }
    HRString_BuildClear_R(path);
    HRString_BuildChar_R(path, Char('/'));
    HRString_BuildChar_R(path, Char('T'));
    HRString_BuildChar_R(path, Char('e'));
    HRString_BuildChar_R(path, Char('m'));
    HRString_BuildChar_R(path, Char('p'));
    if (!HRDirectory_Exists(path))
    {
        HRDirectory_Create(path);
    }
    GC_Release(path);
}

Bool HopperVM_ExecuteOpCode()
{
    External_ServiceInterrupts();
    HopperVM_opCode = OpCode(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    return External_FunctionCall(HopperVM_jumpTable, Byte(HopperVM_opCode));
}

void HopperVM_WriteERROR()
{
    IO_WriteLn();
    IO_WriteHex(HopperVM_PC_Get());
    IO_Write(' ');
    IO_Write('E');
    IO_Write('r');
    IO_Write('r');
    IO_Write('o');
    IO_Write('r');
    IO_Write(':');
    Byte berror = Minimal_Error_Get();
    IO_WriteHex(berror);
    IO_WriteLn();
}

void HopperVM_WriteBREAK()
{
    IO_WriteLn();
    IO_Write('B');
    IO_Write('R');
    IO_Write('E');
    IO_Write('A');
    IO_Write('K');
    IO_WriteLn();
}

void GC_Release(UInt address)
{
    Byte referenceCount = Memory_ReadByte(address + 0x01);
    
    referenceCount--;
    Memory_WriteByte(address + 0x01, referenceCount);
    Type htype = Type(Memory_ReadByte(address));
    if (referenceCount == 0x00)
    {
        switch (htype)
        {
        case Type::eArray:
        case Type::eLong:
        case Type::eFloat:
        case Type::eString:
        {
            Memory_Free(address);
            break;
        }
        case Type::eDirectory:
        {
            HRDirectory_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eFile:
        {
            HRFile_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eList:
        {
            HRList_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eDictionary:
        {
            HRDictionary_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::ePair:
        {
            HRPair_Clear(address);
            Memory_Free(address);
            break;
        }
        case Type::eVariant:
        {
            HRVariant_Clear(address);
            Memory_Free(address);
            break;
        }
        default:
        {
            break;
        }
        } // switch
    }
}

UInt HRString_New()
{
    UInt address = HRString_new(0x00);
    Memory_WriteWord(address + 2, 0x00);
    return address;
}

void HRString_BuildChar_R(UInt & _this, Char ch)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length = HRString_GetLength(_this);
    if (capacity < length + 0x01)
    {
        UInt copy = HRString_clone(_this, 0x01);
        GC_Release(_this);
        _this = copy;
    }
    Memory_WriteByte(_this + 4 + length, Byte(ch));
    Memory_WriteWord(_this + 2, length + 0x01);
}

UInt HRString_GetLength(UInt _this)
{
    return Memory_ReadWord(_this + 2);
}

Char HRString_GetChar(UInt _this, UInt index)
{
    UInt length = HRString_GetLength(_this);
    if (index >= length)
    {
        Minimal_Error_Set(0x05);
        return Char(0x00);
    }
    return Char(Memory_ReadByte(_this + 4 + index));
}

void HRString_SetChar(UInt _this, UInt index, Char ch)
{
    UInt length = HRString_GetLength(_this);
    if (index >= length)
    {
        Minimal_Error_Set(0x05);
    }
    else
    {
        Memory_WriteByte(_this + 4 + index, Byte(ch));
    }
}

void HRString_SetLength(UInt _this, UInt length)
{
    if (length > Memory_ReadWord(_this + 2))
    {
        Minimal_Error_Set(0x05);
    }
    else
    {
        Memory_WriteWord(_this + 2, length);
    }
}

void HRString_BuildClear_R(UInt & _this)
{
    Memory_WriteWord(_this + 2, 0x00);
}

UInt HRString_new(UInt size)
{
    UInt blockSize = 0x06 + size;
    blockSize = (blockSize + 0x0F) & 0xFFF0;
    return GC_New(blockSize - 0x04, Type::eString);
}

UInt HRString_getCapacity(UInt _this)
{
    return Memory_ReadWord(_this - 0x02) - 0x06;
}

UInt HRString_clone(UInt original, UInt extra)
{
    UInt length = Memory_ReadWord(original + 2);
    UInt address = HRString_new(length + extra);
    Memory_WriteWord(address + 2, length);;
    for (UInt i = 0x00; i < length; i++)
    {
        Memory_WriteByte(address + 4 + i, Memory_ReadByte(original + 4 + i));
    }
    return address;
}

void HRDirectory_Create(UInt hrpath)
{
    if (!HRDirectory_Exists(hrpath))
    {
        External_DirectoryCreate(hrpath);
    }
}

Bool HRDirectory_Exists(UInt hrpath)
{
    return External_DirectoryExists(hrpath);
}

void HRDirectory_Clear(UInt _this)
{
    Memory_WriteByte(_this + 2, 0x00);
    GC_Release(Memory_ReadWord(_this + 3));
}

UInt HRFile_Create(UInt hrpath)
{
    if (HRFile_Exists(hrpath))
    {
        HRFile_Delete(hrpath);
    }
    UInt address = HRFile_New();
    Memory_WriteByte(address + 2, 0x01);
    Memory_WriteByte(address + 4, 0x01);
    GC_Release(Memory_ReadWord(address + 6));
    Memory_WriteWord(address + 6, HRString_Clone(hrpath));
    return address;
}

void HRFile_Append(UInt _this, Byte b)
{
    if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 4) != 0x00) && (Memory_ReadByte(_this + 5) == 0x00))
    {
        UInt buffer = Memory_ReadWord(_this + 12);
        HRString_BuildChar_R(buffer, Char(b));
        UInt length = HRString_GetLength(buffer);
        if (length >= 0x0100)
        {
            External_FileWriteAllBytes(Memory_ReadWord(_this + 6), buffer, true);
            HRString_BuildClear_R(buffer);
        }
        Memory_WriteWord(_this + 12, buffer);
    }
    else
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
}

void HRFile_Flush(UInt _this)
{
    if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 4) != 0x00))
    {
        if (Memory_ReadByte(_this + 5) == 0x00)
        {
            UInt content = Memory_ReadWord(_this + 12);
            External_FileWriteAllBytes(Memory_ReadWord(_this + 6), content, true);
            HRString_BuildClear_R(content);
        }
        else
        {
            UInt posLSW = Memory_ReadWord(_this + 8);
            External_FileWriteAllCodeBytes(Memory_ReadWord(_this + 6), Memory_ReadWord(_this + 12), posLSW);
        }
    }
    else
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
}

Bool HRFile_Exists(UInt str)
{
    return External_FileExists(str);
}

UInt HRFile_Open(UInt hrpath)
{
    UInt address = HRFile_New();
    if (HRFile_Exists(hrpath))
    {
        Memory_WriteByte(address + 2, 0x01);
        Memory_WriteByte(address + 3, 0x01);
        GC_Release(Memory_ReadWord(address + 6));
        Memory_WriteWord(address + 6, HRString_Clone(hrpath));
        Memory_WriteWord(address + 8, 0x00);
        Memory_WriteWord(address + 8 + 0x02, 0x00);
        UInt hrsize = HRFile_GetSize(hrpath);
        Memory_WriteByte(address + 14 + 0x00, HRLong_GetByte(hrsize, 0x00));
        Memory_WriteByte(address + 14 + 0x01, HRLong_GetByte(hrsize, 0x01));
        Memory_WriteByte(address + 14 + 0x02, HRLong_GetByte(hrsize, 0x02));
        Memory_WriteByte(address + 14 + 0x03, HRLong_GetByte(hrsize, 0x03));
        GC_Release(hrsize);
    }
    return address;
}

Byte HRFile_Read(UInt _this)
{
    Byte b = 0;
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00))
        {
            UInt posLSW = Memory_ReadWord(_this + 8);
            UInt posMSW = Memory_ReadWord(_this + 8 + 0x02);
            UInt sizeLSW = Memory_ReadWord(_this + 14);
            UInt sizeMSW = Memory_ReadWord(_this + 14 + 0x02);
            if (HRFile_lt32(posLSW, posMSW, sizeLSW, sizeMSW))
            {
                UInt hrpos = HRLong_FromBytes(Memory_ReadByte(_this + 8 + 0x00), Memory_ReadByte(_this + 8 + 0x01), Memory_ReadByte(_this + 8 + 0x02), Memory_ReadByte(_this + 8 + 0x03));
                if (External_TryFileReadByte_R(Memory_ReadWord(_this + 6), hrpos, b))
                {
                    GC_Release(hrpos);
                    if (posLSW == 0xFFFF)
                    {
                        posLSW = 0x00;
                        
                        posMSW++;
                    }
                    else
                    {
                        
                        posLSW++;
                    }
                    Memory_WriteWord(_this + 8, posLSW);
                    Memory_WriteWord(_this + 8 + 0x02, posMSW);
                    break;;
                }
                GC_Release(hrpos);
            }
        }
        Memory_WriteByte(_this + 2, 0x00);
        break;;
    }
    return b;
}

UInt HRFile_CreateFromCode(UInt hrpath, UInt codeStart, UInt codeLength)
{
    if (HRFile_Exists(hrpath))
    {
        HRFile_Delete(hrpath);
    }
    UInt address = HRFile_New();
    Memory_WriteByte(address + 2, 0x01);
    Memory_WriteByte(address + 4, 0x01);
    Memory_WriteByte(address + 5, 0x01);
    GC_Release(Memory_ReadWord(address + 6));
    Memory_WriteWord(address + 6, HRString_Clone(hrpath));
    GC_Release(Memory_ReadWord(address + 12));
    Memory_WriteWord(address + 8, codeLength);
    Memory_WriteWord(address + 8 + 0x02, 0x00);
    Memory_WriteWord(address + 12, codeStart);
    return address;
}

void HRFile_Clear(UInt _this)
{
    Memory_WriteByte(_this + 2, 0x00);
    Memory_WriteByte(_this + 3, 0x00);
    Memory_WriteByte(_this + 4, 0x00);
    GC_Release(Memory_ReadWord(_this + 6));
    Memory_WriteWord(_this + 6, 0x00);
    Memory_WriteWord(_this + 8, 0x00);
    Memory_WriteWord(_this + 8 + 0x02, 0x00);
    if (!HRFile_IsCode(_this))
    {
        GC_Release(Memory_ReadWord(_this + 12));
    }
    Memory_WriteWord(_this + 12, 0x00);
    Memory_WriteWord(_this + 14, 0x00);
    Memory_WriteWord(_this + 14 + 0x02, 0x00);
}

void HRFile_Delete(UInt path)
{
    External_FileDelete(path);
}

UInt HRFile_New()
{
    UInt address = GC_New(0x10, Type::eFile);
    Memory_WriteByte(address + 2, 0x00);
    Memory_WriteByte(address + 3, 0x00);
    Memory_WriteByte(address + 4, 0x00);
    Memory_WriteByte(address + 5, 0x00);
    Memory_WriteWord(address + 6, HRString_New());
    Memory_WriteWord(address + 8, 0x00);
    Memory_WriteWord(address + 8 + 0x02, 0x00);
    Memory_WriteWord(address + 12, HRString_New());
    Memory_WriteWord(address + 14, 0x00);
    Memory_WriteWord(address + 14 + 0x02, 0x00);
    return address;
}

UInt HRFile_GetSize(UInt path)
{
    return External_FileGetSize(path);
}

Bool HRFile_lt32(UInt nextLSW, UInt nextMSW, UInt topLSW, UInt topMSW)
{
    if (nextMSW < topMSW)
    {
        return true;
    }
    if (nextMSW == topMSW)
    {
        return nextLSW < topLSW;
    }
    return false;
}

Bool HRFile_IsCode(UInt _this)
{
    return (Memory_ReadByte(_this + 5) != 0x00);
}

UInt Memory_HeapStart_Get()
{
    return Memory_heapStart;
}

UInt Memory_HeapSize_Get()
{
    return Memory_heapSize;
}

UInt Memory_FreeList_Get()
{
    return Memory_freeList;
}

void Memory_Free(UInt address)
{
    for (;;)
    {
        if (0x00 == address)
        {
            Runtime_ErrorDump(0x01);
            Minimal_Error_Set(0x0B);
            break;;
        }
        UInt blockAddress = address - 0x02;
        UInt size = Memory_ReadWord(blockAddress);
        UInt current = Memory_freeList;
        UInt previous = 0x00;
        for (;;)
        {
            if (0x00 == current)
            {
                break;;
            }
            if (current > address)
            {
                break;;
            }
            previous = current;
            UInt currentNext = Memory_ReadWord(current + 0x02);
            current = currentNext;
        }
        UInt currentPrev = previous;
        UInt currentSize = 0x00;
        UInt currentNext = 0x00;
        if (0x00 != current)
        {
            currentSize = Memory_ReadWord(current);
            currentNext = Memory_ReadWord(current + 0x02);
        }
        UInt freeSlot = address - 0x02;
        if (0x00 == currentPrev)
        {
            Memory_WriteWord(freeSlot + 0x02, current);
            Memory_WriteWord(freeSlot + 0x04, 0x00);
            Memory_WriteWord(current + 0x04, freeSlot);
            UInt gapFront = Memory_freeList - (freeSlot + size);
            if (0x00 == gapFront)
            {
                UInt nextSize = Memory_ReadWord(Memory_freeList);
                UInt nextNext = Memory_ReadWord(Memory_freeList + 0x02);
                Memory_WriteWord(freeSlot, size + nextSize);
                Memory_WriteWord(freeSlot + 0x02, nextNext);
                if (0x00 != nextNext)
                {
                    Memory_WriteWord(nextNext + 0x04, freeSlot);
                }
            }
            Memory_freeList = freeSlot;
        }
        else if (0x00 == current)
        {
            Memory_WriteWord(currentPrev + 0x02, freeSlot);
            Memory_WriteWord(freeSlot + 0x04, currentPrev);
            Memory_WriteWord(freeSlot + 0x02, 0x00);
            UInt prevSize = Memory_ReadWord(currentPrev);
            UInt gapBack = freeSlot - (currentPrev + prevSize);
            if (0x00 == gapBack)
            {
                Memory_WriteWord(currentPrev, prevSize + size);
                Memory_WriteWord(currentPrev + 0x02, 0x00);
            }
        }
        else
        {
            Memory_WriteWord(currentPrev + 0x02, freeSlot);
            Memory_WriteWord(freeSlot + 0x04, currentPrev);
            Memory_WriteWord(freeSlot + 0x02, current);
            Memory_WriteWord(current + 0x04, freeSlot);
            UInt prevSize = Memory_ReadWord(currentPrev);
            UInt gapBack = freeSlot - (currentPrev + prevSize);
            if (0x00 == gapBack)
            {
                Memory_WriteWord(currentPrev, prevSize + size);
                Memory_WriteWord(currentPrev + 0x02, current);
                Memory_WriteWord(current + 0x04, currentPrev);
                freeSlot = currentPrev;
                size = prevSize + size;
            }
            UInt gapNext = current - (freeSlot + size);
            if (0x00 == gapNext)
            {
                Memory_WriteWord(freeSlot, size + currentSize);
                Memory_WriteWord(freeSlot + 0x02, currentNext);
                if (0x00 != currentNext)
                {
                    Memory_WriteWord(currentNext + 0x04, freeSlot);
                }
            }
        }
        break;;
    }
}

void Memory_Initialize(UInt start, UInt size)
{
    Memory_heapStart = start;
    Memory_heapSize = size;
    Memory_freeList = Memory_heapStart;
    Memory_Set(Memory_freeList, 0x00, Memory_heapSize);
    Memory_WriteWord(Memory_freeList, Memory_heapSize);
    Memory_WriteWord(Memory_freeList + 0x02, 0x00);
    Memory_WriteWord(Memory_freeList + 0x04, 0x00);
}

UInt Memory_Allocate(UInt size)
{
    UInt address = 0;
    for (;;)
    {
        if (0x00 == size)
        {
            Runtime_ErrorDump(0xA1);
            Minimal_Error_Set(0x0C);
            break;;
        }
        UInt best = 0;
        UInt bestSize = 0;
        UInt bestNext = 0;
        UInt bestPrev = 0;
        UInt current = Memory_freeList;
        
        size++;
        
        size++;
        if (size < 0x06)
        {
            size = 0x06;
        }
        for (;;)
        {
            if (0x00 == current)
            {
                break;;
            }
            UInt currentSize = Memory_ReadWord(current);
            UInt currentNext = Memory_ReadWord(current + 0x02);
            UInt currentPrev = Memory_ReadWord(current + 0x04);
            if ((currentSize >= size) && ((0x00 == bestSize) || (currentSize < bestSize)))
            {
                best = current;
                bestSize = currentSize;
                bestNext = currentNext;
                bestPrev = currentPrev;
            }
            if (bestSize == size)
            {
                break;;
            }
            current = currentNext;
        }
        address = best + 0x02;
        if (bestSize >= size + 0x06)
        {
            Memory_WriteWord(best, size);
            UInt newHole = best + size;
            UInt newHoleSize = bestSize - size;
            Memory_WriteWord(newHole, newHoleSize);
            if (0x00 == bestPrev)
            {
                Memory_freeList = newHole;
                Memory_WriteWord(newHole + 0x02, bestNext);
                Memory_WriteWord(newHole + 0x04, 0x00);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, newHole);
                }
            }
            else
            {
                Memory_WriteWord(newHole + 0x02, bestNext);
                Memory_WriteWord(newHole + 0x04, bestPrev);
                Memory_WriteWord(bestPrev + 0x02, newHole);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, newHole);
                }
            }
        }
        else if (bestSize >= size)
        {
            Memory_WriteWord(best, bestSize);
            if (0x00 == bestPrev)
            {
                Memory_freeList = bestNext;
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(Memory_freeList + 0x04, 0x00);
                }
            }
            else
            {
                Memory_WriteWord(bestPrev + 0x02, bestNext);
                if (0x00 != bestNext)
                {
                    Memory_WriteWord(bestNext + 0x04, bestPrev);
                }
            }
        }
        else
        {
            IO_WriteHex(size);
            Runtime_ErrorDump(0xA2);
            Minimal_Error_Set(0x0C);
            address = 0x00;
        }
        break;;
    }
    return address;
}

void Memory_Set(UInt memory, Byte value, UInt size)
{;
    for (UInt i = 0x00; i < size; i++)
    {
        Memory_WriteByte(memory + i, value);
    }
}

Char HRByte_ToHex(Byte h)
{
    if (h < 0x0A)
    {
        h = h + 0x30;
    }
    else
    {
        h = h + 0x37;
    }
    return Char(h);
}

void Minimal_Error_Set(Byte value)
{
    Minimal_error = value;
    Diagnostics_SetError(Minimal_error);
}

Byte Minimal_Error_Get()
{
    return Minimal_error;
}

Bool IO_IsBreak()
{
    while (Serial_IsAvailable_Get())
    {
        Char ch = Serial_ReadChar();
        if (ch == Char(0x03))
        {
            return true;
        }
        IO_PushKey(ch);
    }
    return false;
}

void IO_AssignKeyboardBuffer(UInt buffer)
{
    IO_keyboardBufferBase = buffer;
}

void IO_WriteLn()
{
    IO_Write(Char(0x0D));
}

void IO_WriteHex(UInt u)
{
    Byte msb = Byte(u >> 0x08);
    IO_WriteHex(msb);
    Byte lsb = Byte(u & 0xFF);
    IO_WriteHex(lsb);
}

void IO_Write(Char c)
{
    Serial_WriteChar(c);
}

void IO_WriteHex(Byte b)
{
    Byte msn = ((b >> 0x04) & 0x0F);
    IO_Write(HRByte_ToHex(msn));
    Byte lsn = b & 0x0F;
    IO_Write(HRByte_ToHex(lsn));
}

void IO_PushKey(Char c)
{
    Byte k = Byte(c);
    Memory_WriteByte(IO_keyboardBufferBase + IO_keyboardInPointer, k);
    if (IO_keyboardInPointer == 0xFF)
    {
        IO_keyboardInPointer = 0x00;
    }
    else
    {
        
        IO_keyboardInPointer++;
    }
}

void HRArray_Release()
{
    if (HRArray_setSlots != 0x00)
    {
        Memory_Free(HRArray_setSlots);
    }
    if (HRArray_clearSlots != 0x00)
    {
        Memory_Free(HRArray_clearSlots);
    }
}

void HRArray_Initialize()
{
    HRArray_setSlots = Memory_Allocate(0x08);
    HRArray_clearSlots = Memory_Allocate(0x08);
    Memory_WriteByte(HRArray_setSlots + 0x00, 0x01);
    Memory_WriteByte(HRArray_setSlots + 0x01, 0x02);
    Memory_WriteByte(HRArray_setSlots + 0x02, 0x04);
    Memory_WriteByte(HRArray_setSlots + 0x03, 0x08);
    Memory_WriteByte(HRArray_setSlots + 0x04, 0x10);
    Memory_WriteByte(HRArray_setSlots + 0x05, 0x20);
    Memory_WriteByte(HRArray_setSlots + 0x06, 0x40);
    Memory_WriteByte(HRArray_setSlots + 0x07, 0x80);
    Memory_WriteByte(HRArray_clearSlots + 0x00, 0xFE);
    Memory_WriteByte(HRArray_clearSlots + 0x01, 0xFD);
    Memory_WriteByte(HRArray_clearSlots + 0x02, 0xFB);
    Memory_WriteByte(HRArray_clearSlots + 0x03, 0xF7);
    Memory_WriteByte(HRArray_clearSlots + 0x04, 0xEF);
    Memory_WriteByte(HRArray_clearSlots + 0x05, 0xDF);
    Memory_WriteByte(HRArray_clearSlots + 0x06, 0xBF);
    Memory_WriteByte(HRArray_clearSlots + 0x07, 0x7F);
}

void Instructions_PopulateJumpTable(UInt jumpTable)
{
    InstructionDelegate instructionDelegate = &Instructions_Undefined;;
    for (UInt opCode = 0x00; opCode < 0x0100; opCode++)
    {
        External_WriteToJumpTable(jumpTable, Byte(opCode), instructionDelegate);
    }
    instructionDelegate = &Instructions_Die;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDIE), instructionDelegate);
    instructionDelegate = &Instructions_Add;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eADD), instructionDelegate);
    instructionDelegate = &Instructions_Sub;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSUB), instructionDelegate);
    instructionDelegate = &Instructions_Div;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDIV), instructionDelegate);
    instructionDelegate = &Instructions_Mul;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMUL), instructionDelegate);
    instructionDelegate = &Instructions_Mod;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMOD), instructionDelegate);
    instructionDelegate = &Instructions_EQ;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eEQ), instructionDelegate);
    instructionDelegate = &Instructions_NE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eNE), instructionDelegate);
    instructionDelegate = &Instructions_GT;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGT), instructionDelegate);
    instructionDelegate = &Instructions_LT;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLT), instructionDelegate);
    instructionDelegate = &Instructions_GE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGE), instructionDelegate);
    instructionDelegate = &Instructions_LE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLE), instructionDelegate);
    instructionDelegate = &Instructions_BoolOr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBOOLOR), instructionDelegate);
    instructionDelegate = &Instructions_BoolAnd;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBOOLAND), instructionDelegate);
    instructionDelegate = &Instructions_BitOr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITOR), instructionDelegate);
    instructionDelegate = &Instructions_BitAnd;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITAND), instructionDelegate);
    instructionDelegate = &Instructions_BitShl;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITSHL), instructionDelegate);
    instructionDelegate = &Instructions_BitShr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITSHR), instructionDelegate);
    instructionDelegate = &Instructions_AddI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eADDI), instructionDelegate);
    instructionDelegate = &Instructions_SubI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSUBI), instructionDelegate);
    instructionDelegate = &Instructions_DivI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDIVI), instructionDelegate);
    instructionDelegate = &Instructions_MulI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMULI), instructionDelegate);
    instructionDelegate = &Instructions_ModI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eMODI), instructionDelegate);
    instructionDelegate = &Instructions_GTI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGTI), instructionDelegate);
    instructionDelegate = &Instructions_LTI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLTI), instructionDelegate);
    instructionDelegate = &Instructions_GEI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eGEI), instructionDelegate);
    instructionDelegate = &Instructions_LEI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLEI), instructionDelegate);
    instructionDelegate = &Instructions_PushIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIB), instructionDelegate);
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHDB), instructionDelegate);
    instructionDelegate = &Instructions_PushIBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIBB), instructionDelegate);
    instructionDelegate = &Instructions_PopLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_PopRelB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPRELB), instructionDelegate);
    instructionDelegate = &Instructions_PushRelB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHRELB), instructionDelegate);
    instructionDelegate = &Instructions_PopGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_PushGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_PushStackAddrB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHSTACKADDRB), instructionDelegate);
    instructionDelegate = &Instructions_CallB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLB), instructionDelegate);
    instructionDelegate = &Instructions_JZB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJZB), instructionDelegate);
    instructionDelegate = &Instructions_JNZB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJNZB), instructionDelegate);
    instructionDelegate = &Instructions_JB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJB), instructionDelegate);
    instructionDelegate = &Instructions_Ret0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRET0), instructionDelegate);
    instructionDelegate = &Instructions_PushI0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHI0), instructionDelegate);
    instructionDelegate = &Instructions_PushI1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHI1), instructionDelegate);
    instructionDelegate = &Instructions_PopLocalB00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCALB00), instructionDelegate);
    instructionDelegate = &Instructions_PopLocalB01;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCALB01), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalB00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALB00), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalB01;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALB01), instructionDelegate);
    instructionDelegate = &Instructions_SysCall0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL0), instructionDelegate);
    instructionDelegate = &Instructions_SysCall1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL1), instructionDelegate);
    instructionDelegate = &Instructions_SysCall00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL00), instructionDelegate);
    instructionDelegate = &Instructions_SysCall01;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL01), instructionDelegate);
    instructionDelegate = &Instructions_SysCall10;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL10), instructionDelegate);
    instructionDelegate = &Instructions_SysCallB0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALLB0), instructionDelegate);
    instructionDelegate = &Instructions_SysCallB1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALLB1), instructionDelegate);
    instructionDelegate = &Instructions_PushGlobalBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHGLOBALBB), instructionDelegate);
    instructionDelegate = &Instructions_PushLocalBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCALBB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyRelB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYRELB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyLocalB00;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYLOCALB00), instructionDelegate);
    instructionDelegate = &Instructions_PopCopyLocalB01;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPCOPYLOCALB01), instructionDelegate);
    instructionDelegate = &Instructions_EnterB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eENTERB), instructionDelegate);
    instructionDelegate = &Instructions_JIXB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJIXB), instructionDelegate);
    instructionDelegate = &Instructions_PushILE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHILE), instructionDelegate);
    instructionDelegate = &Instructions_PushILT;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHILT), instructionDelegate);
    instructionDelegate = &Instructions_PushIBLE;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIBLE), instructionDelegate);
    instructionDelegate = &Instructions_PushILEI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHILEI), instructionDelegate);
    instructionDelegate = &Instructions_PushIBEQ;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIBEQ), instructionDelegate);
    instructionDelegate = &Instructions_AddB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eADDB), instructionDelegate);
    instructionDelegate = &Instructions_SubB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSUBB), instructionDelegate);
    instructionDelegate = &Instructions_RetB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETB), instructionDelegate);
    instructionDelegate = &Instructions_RetResB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETRESB), instructionDelegate);
    instructionDelegate = &Instructions_RetFast;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETFAST), instructionDelegate);
    instructionDelegate = &Instructions_PopLocal;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPLOCAL), instructionDelegate);
    instructionDelegate = &Instructions_PushLocal;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHLOCAL), instructionDelegate);
    instructionDelegate = &Instructions_PopRel;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPREL), instructionDelegate);
    instructionDelegate = &Instructions_PushRel;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHREL), instructionDelegate);
    instructionDelegate = &Instructions_PopGlobal;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePOPGLOBAL), instructionDelegate);
    instructionDelegate = &Instructions_PushGlobal;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHGLOBAL), instructionDelegate);
    instructionDelegate = &Instructions_PushStackAddr;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHSTACKADDR), instructionDelegate);
    instructionDelegate = &Instructions_Dup;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDUP), instructionDelegate);
    instructionDelegate = &Instructions_DecSP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECSP), instructionDelegate);
    instructionDelegate = &Instructions_Ret;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRET), instructionDelegate);
    instructionDelegate = &Instructions_RetRes;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eRETRES), instructionDelegate);
    instructionDelegate = &Instructions_TestBPB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eTESTBPB), instructionDelegate);
    instructionDelegate = &Instructions_Exit;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eEXIT), instructionDelegate);
    instructionDelegate = &Instructions_JZ;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJZ), instructionDelegate);
    instructionDelegate = &Instructions_JNZ;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJNZ), instructionDelegate);
    instructionDelegate = &Instructions_J;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJW), instructionDelegate);
    instructionDelegate = &Instructions_PushIW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHI), instructionDelegate);
    instructionDelegate = &Instructions_BoolNot;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBOOLNOT), instructionDelegate);
    instructionDelegate = &Instructions_BitNot;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITNOT), instructionDelegate);
    instructionDelegate = &Instructions_Swap;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSWAP), instructionDelegate);
    instructionDelegate = &Instructions_PushIM1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHIM1), instructionDelegate);
    instructionDelegate = &Instructions_PushGP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHGP), instructionDelegate);
    instructionDelegate = &Instructions_CNP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCOPYNEXTPOP), instructionDelegate);
    instructionDelegate = &Instructions_Enter;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eENTER), instructionDelegate);
    instructionDelegate = &Instructions_NOP;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eNOP), instructionDelegate);
    instructionDelegate = &Instructions_Cast;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCAST), instructionDelegate);
    instructionDelegate = &Instructions_PushIW;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::ePUSHD), instructionDelegate);
    instructionDelegate = &Instructions_BitXor;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eBITXOR), instructionDelegate);
    instructionDelegate = &Instructions_JREL;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJREL), instructionDelegate);
    instructionDelegate = &Instructions_JIX;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eJIX), instructionDelegate);
    instructionDelegate = &Instructions_Call;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALL), instructionDelegate);
    instructionDelegate = &Instructions_CallI;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLI), instructionDelegate);
    instructionDelegate = &Instructions_CallRel;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eCALLREL), instructionDelegate);
    instructionDelegate = &Instructions_SysCall;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eSYSCALL), instructionDelegate);
    instructionDelegate = &Instructions_LibCall0;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLIBCALL0), instructionDelegate);
    instructionDelegate = &Instructions_LibCall1;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLIBCALL1), instructionDelegate);
    instructionDelegate = &Instructions_LibCall;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eLIBCALL), instructionDelegate);
    instructionDelegate = &Instructions_IncLocalBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCLOCALBB), instructionDelegate);
    instructionDelegate = &Instructions_IncLocalIBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCLOCALIBB), instructionDelegate);
    instructionDelegate = &Instructions_IncGlobalBB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCGLOBALBB), instructionDelegate);
    instructionDelegate = &Instructions_IncLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_DecLocalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECLOCALB), instructionDelegate);
    instructionDelegate = &Instructions_IncGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_DecGlobalB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECGLOBALB), instructionDelegate);
    instructionDelegate = &Instructions_IncLocalIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCLOCALIB), instructionDelegate);
    instructionDelegate = &Instructions_DecLocalIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECLOCALIB), instructionDelegate);
    instructionDelegate = &Instructions_IncGlobalIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eINCGLOBALIB), instructionDelegate);
    instructionDelegate = &Instructions_DecGlobalIB;
    External_WriteToJumpTable(jumpTable, Byte(OpCode::eDECGLOBALIB), instructionDelegate);
}

Bool Instructions_Undefined()
{
    Runtime_Out4Hex(HopperVM_PC_Get());
    Serial_WriteChar(':');
    Serial_WriteChar('O');
    Runtime_Out2Hex(Byte(HopperVM_CurrentOpCode_Get()));
    Serial_WriteChar(' ');
    Runtime_ErrorDump(0x5D);
    Minimal_Error_Set(0x0A);
    return false;
}

Bool Instructions_Die()
{
    Type atype = (Type)0;
    UInt err = HopperVM_Pop_R(atype);
    Runtime_ErrorDump(0x5E);
    Minimal_Error_Set(Byte(err));
    return false;
}

Bool Instructions_Add()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next + top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_Sub()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next - top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_Div()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    if (top == 0x00)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    next = next / top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_Mul()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next * top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_Mod()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    if (top == 0x00)
    {
        Minimal_Error_Set(0x04);
        return false;
    }
    next = next % top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_EQ()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((next == top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_NE()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((next == top)) ? (0x00) : (0x01));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_GT()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((next > top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_LT()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((next < top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_GE()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((next >= top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_LE()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((next <= top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_BoolOr()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, (((next != 0x00) || (top != 0x00))) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_BoolAnd()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, (((next != 0x00) && (top != 0x00))) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_BitOr()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next | top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_BitAnd()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next & top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_BitShl()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next << top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_BitShr()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = next >> top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_AddI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    nexti = nexti + topi;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(nexti & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(nexti >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_SubI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    nexti = nexti - topi;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(nexti & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(nexti >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_DivI()
{
    Int top = HopperVM_PopI();
    HopperVM_PushI(HopperVM_PopI() / top);
    return true;
}

Bool Instructions_MulI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    nexti = nexti * topi;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(nexti & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(nexti >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_ModI()
{
    Int top = HopperVM_PopI();
    HopperVM_PushI(HopperVM_PopI() % top);
    return true;
}

Bool Instructions_GTI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((nexti > topi)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_LTI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((nexti < topi)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_GEI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((nexti >= topi)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_LEI()
{
    
    HopperVM_sp--;
    Int topi = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08));
    UInt sp1 = HopperVM_sp - 0x01;
    Int nexti = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08));
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((nexti <= topi)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_PushIB()
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Memory_ReadProgramByte(HopperVM_pc));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eByte));
    
    HopperVM_sp++;
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_PushIBB()
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Memory_ReadProgramByte(HopperVM_pc));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eByte));
    
    HopperVM_sp++;
    
    HopperVM_pc++;
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Memory_ReadProgramByte(HopperVM_pc));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eByte));
    
    HopperVM_sp++;
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_PopLocalB()
{
    Int offset = 0;
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocalB();
    }
    else
    {
        offset = HopperVM_ReadByteOffsetOperand();
        Type htype = Type(Memory_ReadByte(UInt(Int(HopperVM_TypeStackLSB_Get()) + Int(HopperVM_BP_Get()) + offset)));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = HopperVM_Get(Byte(Int(HopperVM_BP_Get()) + offset));
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        HopperVM_Put(Byte(Int(HopperVM_BP_Get()) + offset), value, htype);
    }
    return true;
}

Bool Instructions_PushLocalB()
{
    Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    
    HopperVM_pc++;
    Byte address = Byte(offset + HopperVM_bp);
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08);
    Byte htype = Memory_ReadByte(HopperVM_typeStackPage + address);
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, htype);
    
    HopperVM_sp++;
    if (htype >= 0x0D)
    {
        
        value++;
        Memory_WriteByte(value, Memory_ReadByte(value) + 0x01);
    }
    return true;
}

Bool Instructions_PopRelB()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyRelB();
    }
    else
    {
        Int offset = HopperVM_ReadByteOffsetOperand();
        Byte referenceAddress = Byte(Int(HopperVM_BP_Get()) + offset);
        Type rtype = (Type)0;
        Byte localAddress = Byte(HopperVM_Get_R(referenceAddress, rtype));
        UInt existing = HopperVM_Get_R(localAddress, rtype);
        if (Types_IsReferenceType(rtype))
        {
            GC_Release(existing);
        }
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        HopperVM_Put(localAddress, value, vtype);
    }
    return true;
}

Bool Instructions_PushRelB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Byte referenceAddress = Byte(Int(HopperVM_BP_Get()) + offset);
    Type rtype = (Type)0;
    Byte localAddress = Byte(HopperVM_Get_R(referenceAddress, rtype));
    UInt value = HopperVM_Get_R(localAddress, rtype);
    HopperVM_Push(value, rtype);
    if (Types_IsReferenceType(rtype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PopGlobalB()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyGlobalB();
    }
    else
    {
        Byte address = Byte(HopperVM_ReadByteOperand() + HopperVM_GP_Get());
        Type htype = Type(Memory_ReadByte(HopperVM_TypeStackLSB_Get() + address));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = HopperVM_Get(address);
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        HopperVM_Put(address, value, htype);
    }
    return true;
}

Bool Instructions_PushGlobalB()
{
    Byte address = Byte(HopperVM_ReadByteOperand() + HopperVM_GP_Get());
    Type htype = (Type)0;
    UInt value = HopperVM_Get_R(address, htype);
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PushStackAddrB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Byte address = Byte(Int(HopperVM_BP_Get()) + offset);
    HopperVM_Push(address, Type::eReference);
    return true;
}

Bool Instructions_CallB()
{
    UInt methodIndex = HopperVM_ReadByteOperand();
    HopperVM_PushCS(HopperVM_PC_Get());
    HopperVM_PC_Set(HopperVM_LookupMethod(methodIndex));
    return true;
}

Bool Instructions_JZB()
{
    
    HopperVM_sp--;
    if ((Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08)) == 0x00)
    {
        Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
        
        HopperVM_pc++;
        if (offset > 0x7F)
        {
            offset = offset - 0x0100;
        }
        HopperVM_pc = UInt(offset + Int(HopperVM_pc - 0x02));
    }
    else
    {
        
        HopperVM_pc++;
    }
    return true;
}

Bool Instructions_JNZB()
{
    if (HopperVM_Pop() != 0x00)
    {
        HopperVM_pc = UInt(HopperVM_ReadByteOffsetOperand() + Int(HopperVM_pc - 0x02));
    }
    else
    {
        
        HopperVM_pc++;
    }
    return true;
}

Bool Instructions_JB()
{
    HopperVM_PC_Set(UInt(HopperVM_ReadByteOffsetOperand() + Int(HopperVM_PC_Get() - 0x02)));
    return true;
}

Bool Instructions_Ret0()
{
    
    HopperVM_csp--;
    HopperVM_bp = Memory_ReadByte(HopperVM_callStackLSBPage + HopperVM_csp);
    if (HopperVM_csp == HopperVM_cspStart)
    {
        HopperVM_pc = 0x00;
    }
    else
    {
        
        HopperVM_csp--;
        HopperVM_pc = Memory_ReadByte(HopperVM_callStackLSBPage + HopperVM_csp) + (Memory_ReadByte(HopperVM_callStackMSBPage + HopperVM_csp) << 0x08);
    }
    return HopperVM_pc != 0x00;
}

Bool Instructions_PushI0()
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, 0x00);
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eByte));
    
    HopperVM_sp++;
    return true;
}

Bool Instructions_PushI1()
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, 0x01);
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eByte));
    
    HopperVM_sp++;
    return true;
}

Bool Instructions_PopLocalB00()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocalB00();
    }
    else
    {
        Type htype = Type(Memory_ReadByte(UInt(Int(HopperVM_TypeStackLSB_Get()) + Int(HopperVM_BP_Get()))));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = HopperVM_Get(HopperVM_BP_Get());
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        HopperVM_Put(HopperVM_BP_Get(), value, htype);
    }
    return true;
}

Bool Instructions_PopLocalB01()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocalB01();
    }
    else
    {
        Type htype = Type(Memory_ReadByte(UInt(Int(HopperVM_TypeStackLSB_Get()) + Int(HopperVM_BP_Get()) + 0x01)));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = HopperVM_Get(HopperVM_BP_Get() + 0x01);
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        HopperVM_Put(HopperVM_BP_Get() + 0x01, value, htype);
    }
    return true;
}

Bool Instructions_PushLocalB00()
{
    Byte address = Byte(HopperVM_bp);
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08);
    Byte htype = Memory_ReadByte(HopperVM_typeStackPage + address);
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, htype);
    
    HopperVM_sp++;
    if (htype >= 0x0D)
    {
        
        value++;
        Memory_WriteByte(value, Memory_ReadByte(value) + 0x01);
    }
    return true;
}

Bool Instructions_PushLocalB01()
{
    Byte address = Byte(HopperVM_bp + 0x01);
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08);
    Byte htype = Memory_ReadByte(HopperVM_typeStackPage + address);
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, htype);
    
    HopperVM_sp++;
    if (htype >= 0x0D)
    {
        
        value++;
        Memory_WriteByte(value, Memory_ReadByte(value) + 0x01);
    }
    return true;
}

Bool Instructions_SysCall0()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, 0x00);
}

Bool Instructions_SysCall1()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, 0x01);
}

Bool Instructions_SysCall00()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    Bool doNext0 = HopperVM_ExecuteSysCall(iSysCall, 0x00);
    iSysCall = HopperVM_ReadByteOperand();
    Bool doNext1 = HopperVM_ExecuteSysCall(iSysCall, 0x00);
    return doNext0 && doNext1;
}

Bool Instructions_SysCall01()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    Bool doNext0 = HopperVM_ExecuteSysCall(iSysCall, 0x00);
    iSysCall = HopperVM_ReadByteOperand();
    Bool doNext1 = HopperVM_ExecuteSysCall(iSysCall, 0x01);
    return doNext0 && doNext1;
}

Bool Instructions_SysCall10()
{
    Byte iSysCall = HopperVM_ReadByteOperand();
    Bool doNext0 = HopperVM_ExecuteSysCall(iSysCall, 0x01);
    iSysCall = HopperVM_ReadByteOperand();
    Bool doNext1 = HopperVM_ExecuteSysCall(iSysCall, 0x00);
    return doNext0 && doNext1;
}

Bool Instructions_SysCallB0()
{
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, 0x00);
}

Bool Instructions_SysCallB1()
{
    HopperVM_Push(HopperVM_ReadByteOperand(), Type::eByte);
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, 0x01);
}

Bool Instructions_PushGlobalBB()
{
    Bool res = Instructions_PushGlobalB();
    return Instructions_PushGlobalB();
}

Bool Instructions_PushLocalBB()
{
    Bool res = Instructions_PushLocalB();
    return Instructions_PushLocalB();
}

Bool Instructions_PopCopyLocalB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Type htype = (Type)0;
    Byte localAddress = Byte(Int(HopperVM_BP_Get()) + offset);
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyRelB()
{
    Int offset = HopperVM_ReadByteOffsetOperand();
    Byte referenceAddress = Byte(Int(HopperVM_BP_Get()) + offset);
    Type rtype = (Type)0;
    Byte localAddress = Byte(HopperVM_Get_R(referenceAddress, rtype));
    UInt oldvalue = HopperVM_Get_R(localAddress, rtype);
    if (Types_IsReferenceType(rtype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(rtype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, rtype);
    }
    return true;
}

Bool Instructions_PopCopyGlobalB()
{
    Byte address = Byte(HopperVM_ReadByteOperand() + HopperVM_GP_Get());
    Type htype = (Type)0;
    UInt oldvalue = HopperVM_Get_R(address, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(address, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyLocalB00()
{
    Type htype = (Type)0;
    Byte localAddress = HopperVM_BP_Get();
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyLocalB01()
{
    Type htype = (Type)0;
    Byte localAddress = Byte(HopperVM_BP_Get() + 0x01);
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_EnterB()
{
    Memory_WriteByte(HopperVM_callStackLSBPage + HopperVM_csp, HopperVM_bp);
    Memory_WriteByte(HopperVM_callStackMSBPage + HopperVM_csp, 0x00);
    
    HopperVM_csp++;
    HopperVM_bp = HopperVM_sp;
    Byte zeros = Memory_ReadProgramByte(HopperVM_pc);
    
    HopperVM_pc++;;
    for (UInt i = 0x00; i < zeros; i++)
    {
        Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, 0x00);
        Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0x00);
        Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eByte));
        
        HopperVM_sp++;
    }
    return true;
}

Bool Instructions_JIXB()
{
    UInt switchCase = HopperVM_Pop();
    Byte minRange = HopperVM_ReadByteOperand();
    Byte maxRange = HopperVM_ReadByteOperand();
    Byte lsb = HopperVM_ReadByteOperand();
    Byte msb = HopperVM_ReadByteOperand();
    Int jumpBackOffset = Int(lsb + (msb << 0x08));
    UInt tpc = HopperVM_PC_Get();
    HopperVM_PC_Set(UInt(Int(HopperVM_PC_Get()) - jumpBackOffset - 0x05));
    UInt tableSize = UInt(maxRange) - UInt(minRange) + 0x01;
    UInt offset = 0x00;
    if ((switchCase >= minRange) && (switchCase <= maxRange))
    {
        UInt index = tpc + switchCase - minRange;
        offset = Memory_ReadProgramByte(index);
    }
    if (offset == 0x00)
    {
        HopperVM_PC_Set(tpc + tableSize);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + offset);
    }
    return true;
}

Bool Instructions_PushILE()
{
    UInt top = Memory_ReadProgramByte(HopperVM_pc) + (Memory_ReadProgramByte(HopperVM_pc + 0x01) << 0x08);
    HopperVM_pc = HopperVM_pc + 0x02;
    UInt sp2 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp2) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp2) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp2, ((next <= top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp2, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp2, Byte(Type::eBool));
    return true;
}

Bool Instructions_PushILT()
{
    UInt top = Memory_ReadProgramByte(HopperVM_pc) + (Memory_ReadProgramByte(HopperVM_pc + 0x01) << 0x08);
    HopperVM_pc = HopperVM_pc + 0x02;
    UInt sp2 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp2) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp2) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp2, ((next < top)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp2, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp2, Byte(Type::eBool));
    return true;
}

Bool Instructions_PushIBLE()
{
    UInt sp2 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp2) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp2) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp2, ((next <= Memory_ReadProgramByte(HopperVM_pc))) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp2, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp2, Byte(Type::eBool));
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_PushILEI()
{
    HopperVM_Push(HopperVM_ReadWordOperand(), Type::eUInt);
    Type ttype = (Type)0;
    Int top = HopperVM_PopI_R(ttype);
    Type ntype = (Type)0;
    Int next = HopperVM_PopI_R(ntype);
    HopperVM_Push(((next <= top)) ? (0x01) : (0x00), Type::eBool);
    return true;
}

Bool Instructions_PushIBEQ()
{
    UInt sp2 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp2) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp2) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp2, ((next == Memory_ReadProgramByte(HopperVM_pc))) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp2, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp2, Byte(Type::eBool));
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_AddB()
{
    UInt sp2 = HopperVM_sp - 0x01;
    UInt lsb = HopperVM_valueStackLSBPage + sp2;
    UInt msb = HopperVM_valueStackMSBPage + sp2;
    UInt value = (Memory_ReadByte(lsb) + (Memory_ReadByte(msb) << 0x08)) + Memory_ReadProgramByte(HopperVM_pc);
    Memory_WriteByte(lsb, Byte(value & 0xFF));
    Memory_WriteByte(msb, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp2, Byte(Type::eUInt));
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_SubB()
{
    UInt sp2 = HopperVM_sp - 0x01;
    UInt lsb = HopperVM_valueStackLSBPage + sp2;
    UInt msb = HopperVM_valueStackMSBPage + sp2;
    UInt value = (Memory_ReadByte(lsb) + (Memory_ReadByte(msb) << 0x08)) - Memory_ReadProgramByte(HopperVM_pc);
    Memory_WriteByte(lsb, Byte(value & 0xFF));
    Memory_WriteByte(msb, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp2, Byte(Type::eUInt));
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_RetB()
{
    UInt popBytes = HopperVM_ReadByteOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        
        popBytes--;
    }
    HopperVM_BP_Set(Byte(HopperVM_PopCS()));
    if (HopperVM_CSP_Get() == HopperVM_CSPStart_Get())
    {
        HopperVM_PC_Set(0x00);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PopCS());
    }
    return HopperVM_PC_Get() != 0x00;
}

Bool Instructions_RetResB()
{
    
    HopperVM_sp--;
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    Byte rtype = Memory_ReadByte(HopperVM_typeStackPage + HopperVM_sp);
    Byte popBytes = Memory_ReadProgramByte(HopperVM_pc);
    
    HopperVM_pc++;
    while (popBytes != 0x00)
    {
        
        HopperVM_sp--;
        UInt address = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
        Byte htype = Memory_ReadByte(HopperVM_typeStackPage + HopperVM_sp);
        if (htype >= 0x0D)
        {
            GC_Release(address);
        }
        
        popBytes--;
    }
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, rtype);
    
    HopperVM_sp++;
    
    HopperVM_csp--;
    HopperVM_bp = Memory_ReadByte(HopperVM_callStackLSBPage + HopperVM_csp);
    if (HopperVM_csp == HopperVM_cspStart)
    {
        HopperVM_pc = 0x00;
    }
    else
    {
        
        HopperVM_csp--;
        HopperVM_pc = Memory_ReadByte(HopperVM_callStackLSBPage + HopperVM_csp) + (Memory_ReadByte(HopperVM_callStackMSBPage + HopperVM_csp) << 0x08);
    }
    return HopperVM_pc != 0x00;
}

Bool Instructions_RetFast()
{
    HopperVM_PC_Set(HopperVM_PopCS());
    return true;
}

Bool Instructions_PopLocal()
{
    Int offset = 0;
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyLocal();
    }
    else
    {
        offset = HopperVM_ReadWordOffsetOperand();
        Type htype = Type(Memory_ReadByte(UInt(Int(HopperVM_TypeStackLSB_Get()) + Int(HopperVM_BP_Get()) + offset)));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = HopperVM_Get(Byte(Int(HopperVM_BP_Get()) + offset));
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        HopperVM_Put(Byte(Int(HopperVM_BP_Get()) + offset), value, htype);
    }
    return true;
}

Bool Instructions_PushLocal()
{
    Int offset = HopperVM_ReadWordOffsetOperand();
    Type htype = (Type)0;
    UInt value = HopperVM_Get_R(Byte(Int(HopperVM_BP_Get()) + offset), htype);
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PopRel()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyRel();
    }
    else
    {
        Int offset = HopperVM_ReadWordOffsetOperand();
        Byte referenceAddress = Byte(Int(HopperVM_BP_Get()) + offset);
        Type rtype = (Type)0;
        Byte localAddress = Byte(HopperVM_Get_R(referenceAddress, rtype));
        UInt existing = HopperVM_Get_R(localAddress, rtype);
        if (Types_IsReferenceType(rtype))
        {
            GC_Release(existing);
        }
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        HopperVM_Put(localAddress, value, vtype);
    }
    return true;
}

Bool Instructions_PushRel()
{
    Int offset = HopperVM_ReadWordOffsetOperand();
    Byte referenceAddress = Byte(Int(HopperVM_BP_Get()) + offset);
    Type rtype = (Type)0;
    Byte localAddress = Byte(HopperVM_Get_R(referenceAddress, rtype));
    UInt value = HopperVM_Get_R(localAddress, rtype);
    HopperVM_Push(value, rtype);
    if (Types_IsReferenceType(rtype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PopGlobal()
{
    if (HopperVM_CNP_Get())
    {
        HopperVM_CNP_Set(false);
        return Instructions_PopCopyGlobal();
    }
    else
    {
        Byte address = Byte(HopperVM_ReadWordOperand() + HopperVM_GP_Get());
        Type htype = Type(Memory_ReadByte(HopperVM_TypeStackLSB_Get() + address));
        UInt value = 0;
        if (Types_IsReferenceType(htype))
        {
            value = HopperVM_Get(address);
            GC_Release(value);
        }
        value = HopperVM_Pop_R(htype);
        HopperVM_Put(address, value, htype);
    }
    return true;
}

Bool Instructions_PushGlobal()
{
    Byte address = Byte(HopperVM_ReadWordOperand() + HopperVM_GP_Get());
    Type htype = (Type)0;
    UInt value = HopperVM_Get_R(address, htype);
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_PushStackAddr()
{
    Int offset = HopperVM_ReadWordOffsetOperand();
    Byte address = Byte(Int(HopperVM_BP_Get()) + offset);
    HopperVM_Push(address, Type::eReference);
    return true;
}

Bool Instructions_Dup()
{
    Byte offset = HopperVM_ReadByteOperand();
    Byte address = Byte(HopperVM_SP_Get() - 0x01 - offset);
    Type htype = (Type)0;
    UInt value = HopperVM_Get_R(address, htype);
    HopperVM_Push(value, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_AddReference(value);
    }
    return true;
}

Bool Instructions_DecSP()
{
    UInt popBytes = HopperVM_ReadByteOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        
        popBytes--;
    }
    return true;
}

Bool Instructions_Ret()
{
    UInt popBytes = HopperVM_ReadWordOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        
        popBytes--;
    }
    HopperVM_BP_Set(Byte(HopperVM_PopCS()));
    if (HopperVM_CSP_Get() == HopperVM_CSPStart_Get())
    {
        HopperVM_PC_Set(0x00);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PopCS());
    }
    return HopperVM_PC_Get() != 0x00;
}

Bool Instructions_RetRes()
{
    Type rtype = (Type)0;
    UInt value = HopperVM_Pop_R(rtype);
    UInt popBytes = HopperVM_ReadWordOperand();
    while (popBytes != 0x00)
    {
        Type htype = (Type)0;
        UInt address = HopperVM_Pop_R(htype);
        if (Types_IsReferenceType(htype))
        {
            GC_Release(address);
        }
        
        popBytes--;
    }
    HopperVM_Push(value, rtype);
    HopperVM_BP_Set(Byte(HopperVM_PopCS()));
    if (HopperVM_CSP_Get() == HopperVM_CSPStart_Get())
    {
        HopperVM_PC_Set(0x00);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PopCS());
    }
    return HopperVM_PC_Get() != 0x00;
}

Bool Instructions_TestBPB()
{
    Byte operand = HopperVM_ReadByteOperand();
    UInt bpExpected = UInt(HopperVM_SP_Get() - operand);
    if (bpExpected != HopperVM_BP_Get())
    {
        Minimal_Error_Set(0x0B);
        return false;
    }
    return true;
}

Bool Instructions_Exit()
{
    return HopperVM_ExitInline();
}

Bool Instructions_JZ()
{
    if (HopperVM_Pop() == 0x00)
    {
        HopperVM_pc = UInt(HopperVM_ReadWordOffsetOperand() + Int(HopperVM_pc - 0x03));
    }
    else
    {
        HopperVM_pc = HopperVM_pc + 0x02;
    }
    return true;
}

Bool Instructions_JNZ()
{
    if (HopperVM_Pop() != 0x00)
    {
        HopperVM_PC_Set(UInt(HopperVM_ReadWordOffsetOperand() + Int(HopperVM_PC_Get() - 0x03)));
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + 0x02);
    }
    return true;
}

Bool Instructions_J()
{
    HopperVM_PC_Set(UInt(HopperVM_ReadWordOffsetOperand() + Int(HopperVM_PC_Get() - 0x03)));
    return true;
}

Bool Instructions_PushIW()
{
    HopperVM_Push(HopperVM_ReadWordOperand(), Type::eUInt);
    return true;
}

Bool Instructions_BoolNot()
{
    UInt sp1 = HopperVM_sp - 0x01;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, ((top == 0x00)) ? (0x01) : (0x00));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, 0x00);
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eBool));
    return true;
}

Bool Instructions_BitNot()
{
    UInt sp1 = HopperVM_sp - 0x01;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    top = ~top;
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(top & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(top >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_Swap()
{
    Type ttype = (Type)0;
    Type ntype = (Type)0;
    UInt topValue = HopperVM_Get_R(HopperVM_SP_Get() - 0x01, ttype);
    UInt nextValue = HopperVM_Get_R(HopperVM_SP_Get() - 0x02, ntype);
    HopperVM_Put(Byte(HopperVM_SP_Get() - 0x01), nextValue, ntype);
    HopperVM_Put(Byte(HopperVM_SP_Get() - 0x02), topValue, ttype);
    return true;
}

Bool Instructions_PushIM1()
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, 0xFF);
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, 0xFF);
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eInt));
    
    HopperVM_sp++;
    return true;
}

Bool Instructions_PushGP()
{
    HopperVM_Push(HopperVM_GP_Get(), Type::eUInt);
    return true;
}

Bool Instructions_CNP()
{
    HopperVM_CNP_Set(true);
    return true;
}

Bool Instructions_Enter()
{
    Memory_WriteByte(HopperVM_callStackLSBPage + HopperVM_csp, HopperVM_bp);
    Memory_WriteByte(HopperVM_callStackMSBPage + HopperVM_csp, 0x00);
    
    HopperVM_csp++;
    HopperVM_bp = HopperVM_sp;
    return true;
}

Bool Instructions_NOP()
{
    return true;
}

Bool Instructions_Cast()
{
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp - 0x01, Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    return true;
}

Bool Instructions_BitXor()
{
    
    HopperVM_sp--;
    UInt top = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    UInt sp1 = HopperVM_sp - 0x01;
    UInt next = Memory_ReadByte(HopperVM_valueStackLSBPage + sp1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + sp1) << 0x08);
    next = (next | top) & (~(next & top));
    Memory_WriteByte(HopperVM_valueStackLSBPage + sp1, Byte(next & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + sp1, Byte(next >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + sp1, Byte(Type::eUInt));
    return true;
}

Bool Instructions_JREL()
{
    UInt address = HopperVM_Pop();
    HopperVM_PC_Set(address);
    return true;
}

Bool Instructions_JIX()
{
    UInt switchCase = HopperVM_Pop();
    Byte minRange = HopperVM_ReadByteOperand();
    Byte maxRange = HopperVM_ReadByteOperand();
    Byte lsb = HopperVM_ReadByteOperand();
    Byte msb = HopperVM_ReadByteOperand();
    Int jumpBackOffset = Int(lsb + (msb << 0x08));
    UInt tpc = HopperVM_PC_Get();
    HopperVM_PC_Set(UInt(Int(HopperVM_PC_Get()) - jumpBackOffset - 0x05));
    UInt tableSize = (UInt(maxRange) - UInt(minRange) + 0x01) << 0x01;
    UInt offset = 0x00;
    if ((switchCase >= minRange) && (switchCase <= maxRange))
    {
        UInt index = tpc + (switchCase - minRange) * 0x02;
        offset = Memory_ReadProgramByte(index) + (Memory_ReadProgramByte(index + 0x01) << 0x08);
    }
    if (offset == 0x00)
    {
        HopperVM_PC_Set(tpc + tableSize);
    }
    else
    {
        HopperVM_PC_Set(HopperVM_PC_Get() + offset);
    }
    return true;
}

Bool Instructions_Call()
{
    UInt methodIndex = HopperVM_ReadWordOperand();
    HopperVM_PushCS(HopperVM_PC_Get());
    UInt methodAddress = HopperVM_LookupMethod(methodIndex);
    Memory_WriteProgramByte(HopperVM_PC_Get() - 0x03, Byte(OpCode::eCALLI));
    Memory_WriteProgramWord(HopperVM_PC_Get() - 0x02, methodAddress);
    HopperVM_PC_Set(methodAddress);
    return true;
}

Bool Instructions_CallI()
{
    UInt methodAddress = Memory_ReadProgramByte(HopperVM_pc) + (Memory_ReadProgramByte(HopperVM_pc + 0x01) << 0x08);
    UInt pc2 = HopperVM_pc + 0x02;
    Memory_WriteByte(HopperVM_callStackLSBPage + HopperVM_csp, Byte(pc2 & 0xFF));
    Memory_WriteByte(HopperVM_callStackMSBPage + HopperVM_csp, Byte(pc2 >> 0x08));
    
    HopperVM_csp++;
    HopperVM_pc = methodAddress;
    return true;
}

Bool Instructions_CallRel()
{
    UInt methodIndex = HopperVM_Pop();
    HopperVM_PushCS(HopperVM_PC_Get());
    HopperVM_PC_Set(HopperVM_LookupMethod(methodIndex));
    return true;
}

Bool Instructions_SysCall()
{
    Type htype = (Type)0;
    UInt iOverload = HopperVM_Pop_R(htype);
    Byte iSysCall = HopperVM_ReadByteOperand();
    return HopperVM_ExecuteSysCall(iSysCall, iOverload);
}

Bool Instructions_LibCall0()
{
    Byte iLibCall = HopperVM_ReadByteOperand();
    return Library_ExecuteLibCall(iLibCall, 0x00);
}

Bool Instructions_LibCall1()
{
    Type htype = (Type)0;
    Byte iLibCall = HopperVM_ReadByteOperand();
    return Library_ExecuteLibCall(iLibCall, 0x01);
}

Bool Instructions_LibCall()
{
    Type htype = (Type)0;
    UInt iOverload = HopperVM_Pop_R(htype);
    Byte iLibCall = HopperVM_ReadByteOperand();
    return Library_ExecuteLibCall(iLibCall, iOverload);
}

Bool Instructions_IncLocalBB()
{
    Int offset0 = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset0 > 0x7F)
    {
        offset0 = offset0 - 0x0100;
    }
    Int offset1 = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset1 > 0x7F)
    {
        offset1 = offset1 - 0x0100;
    }
    Byte address0 = Byte(Int(HopperVM_bp) + offset0);
    Byte address1 = Byte(Int(HopperVM_bp) + offset1);
    Byte htype = Memory_ReadByte(HopperVM_typeStackPage + address0);
    UInt value0 = Memory_ReadByte(HopperVM_valueStackLSBPage + address0) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address0) << 0x08);
    UInt value1 = Memory_ReadByte(HopperVM_valueStackLSBPage + address1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address1) << 0x08);
    value0 = value0 + value1;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address0, Byte(value0 & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address0, Byte(value0 >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + address0, htype);
    return true;
}

Bool Instructions_IncLocalIBB()
{
    Int offset0 = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset0 > 0x7F)
    {
        offset0 = offset0 - 0x0100;
    }
    Int offset1 = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset1 > 0x7F)
    {
        offset1 = offset1 - 0x0100;
    }
    Byte address0 = Byte(Int(HopperVM_bp) + offset0);
    Byte address1 = Byte(Int(HopperVM_bp) + offset1);
    Byte htype = Memory_ReadByte(HopperVM_typeStackPage + address0);
    Int value0 = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + address0) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address0) << 0x08));
    Int value1 = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + address1) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address1) << 0x08));
    value0 = value0 + value1;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address0, Byte(value0 & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address0, Byte(value0 >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + address0, htype);
    return true;
}

Bool Instructions_IncGlobalBB()
{
    Byte address0 = Byte(HopperVM_ReadByteOperand() + HopperVM_gp);
    Byte address1 = Byte(HopperVM_ReadByteOperand() + HopperVM_gp);
    Type type0 = (Type)0;
    UInt value = HopperVM_Get_R(address0, type0);
    Type type1 = (Type)0;
    HopperVM_Put(address0, value + HopperVM_Get_R(address1, type1), type0);
    return true;
}

Bool Instructions_IncLocalB()
{
    Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    Byte address = Byte(Int(HopperVM_bp) + offset);
    Byte itype = Memory_ReadByte(HopperVM_typeStackPage + address);
    if (itype == Byte(Type::eByte))
    {
        Memory_WriteByte(HopperVM_typeStackPage + address, Byte(Type::eUInt));
    }
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08) + 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_DecLocalB()
{
    Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    Byte address = Byte(Int(HopperVM_bp) + offset);
    UInt value = (Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08)) - 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_IncGlobalB()
{
    Byte address = Byte(Memory_ReadProgramByte(HopperVM_pc) + HopperVM_gp);
    
    HopperVM_pc++;
    Byte itype = Memory_ReadByte(HopperVM_typeStackPage + address);
    if (itype == Byte(Type::eByte))
    {
        Memory_WriteByte(HopperVM_typeStackPage + address, Byte(Type::eUInt));
    }
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08) + 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_DecGlobalB()
{
    Byte address = Byte(Memory_ReadProgramByte(HopperVM_pc) + HopperVM_gp);
    
    HopperVM_pc++;
    Byte itype = Memory_ReadByte(HopperVM_typeStackPage + address);
    if (itype == Byte(Type::eByte))
    {
        Memory_WriteByte(HopperVM_typeStackPage + address, Byte(Type::eUInt));
    }
    UInt value = (Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08)) - 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_IncLocalIB()
{
    Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    Byte address = Byte(Int(HopperVM_bp) + offset);
    Int value = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08)) + 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_DecLocalIB()
{
    Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    Byte address = Byte(Int(HopperVM_bp) + offset);
    Int value = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08)) - 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_IncGlobalIB()
{
    Byte address = (Memory_ReadProgramByte(HopperVM_pc) + HopperVM_gp);
    
    HopperVM_pc++;
    Int value = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08)) + 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_DecGlobalIB()
{
    Byte address = (Memory_ReadProgramByte(HopperVM_pc) + HopperVM_gp);
    
    HopperVM_pc++;
    Int value = External_UIntToInt(Memory_ReadByte(HopperVM_valueStackLSBPage + address) + (Memory_ReadByte(HopperVM_valueStackMSBPage + address) << 0x08)) - 0x01;
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    return true;
}

Bool Instructions_PopCopyLocal()
{
    Int offset = HopperVM_ReadWordOffsetOperand();
    Type htype = (Type)0;
    Byte localAddress = Byte(Int(HopperVM_BP_Get()) + offset);
    UInt oldvalue = HopperVM_Get_R(localAddress, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, htype);
    }
    return true;
}

Bool Instructions_PopCopyRel()
{
    Int offset = HopperVM_ReadWordOffsetOperand();
    Byte referenceAddress = Byte(Int(HopperVM_BP_Get()) + offset);
    Type rtype = (Type)0;
    Byte localAddress = Byte(HopperVM_Get_R(referenceAddress, rtype));
    UInt oldvalue = HopperVM_Get_R(localAddress, rtype);
    if (Types_IsReferenceType(rtype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(rtype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(localAddress, newvalue, rtype);
    }
    return true;
}

Bool Instructions_PopCopyGlobal()
{
    Byte address = Byte(HopperVM_ReadWordOperand() + HopperVM_GP_Get());
    Type htype = (Type)0;
    UInt oldvalue = HopperVM_Get_R(address, htype);
    if (Types_IsReferenceType(htype))
    {
        GC_Release(oldvalue);
    }
    UInt value = HopperVM_Pop_R(htype);
    if (value == oldvalue)
    {
    }
    else
    {
        UInt newvalue = GC_Clone(value);
        GC_Release(value);
        HopperVM_Put(address, newvalue, htype);
    }
    return true;
}

void HRList_Clear(UInt _this)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt pFirst = Memory_ReadWord(_this + 5);
    if (pFirst != 0x00)
    {
        HRList_clearAllItems(pFirst, etype);
    }
    Memory_WriteWord(_this + 2, 0x00);
    Memory_WriteWord(_this + 5, 0x00);
    Memory_WriteWord(_this + 7, 0x00);
    Memory_WriteWord(_this + 7 + 0x02, 0x00);
}

void HRList_clearAllItems(UInt pCurrent, Type etype)
{
    UInt pNext = 0;
    for (;;)
    {
        if (pCurrent == 0x00)
        {
            break;;
        }
        pNext = Memory_ReadWord(pCurrent + 2);
        HRList_clearItem(pCurrent, etype);
        pCurrent = pNext;
    }
}

void HRList_clearItem(UInt pCurrent, Type etype)
{
    UInt pData = Memory_ReadWord(pCurrent + 0);
    Memory_Free(pCurrent);
    if (Types_IsReferenceType(etype))
    {
        GC_Release(pData);
    }
}

void HRDictionary_Clear(UInt _this)
{
    UInt iterator = 0;
    Type ktype = (Type)0;
    UInt key = 0;
    Type vtype = (Type)0;
    UInt value = 0;
    while (HRDictionary_next_R(_this, iterator, ktype, key, vtype, value))
    {
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        if (Types_IsReferenceType(vtype))
        {
            GC_Release(value);
        }
    }
    UInt pEntries = Memory_ReadWord(_this + 8);
    if (pEntries != 0x00)
    {
        Memory_Free(pEntries);
    }
    Memory_WriteWord(_this + 4, 0x00);
    Memory_WriteWord(_this + 6, 0x00);
    Memory_WriteWord(_this + 8, 0x00);
}

Bool HRDictionary_next_R(UInt _this, UInt & iterator, Type & ktype, UInt & key, Type & vtype, UInt & value)
{
    UInt count = Memory_ReadWord(_this + 4);
    if (count == 0x00)
    {
        return false;
    }
    if (iterator == 0xFFFF)
    {
        return false;
    }
    Bool success = false;
    ktype = Type(Memory_ReadByte(_this + 2));
    vtype = Type(Memory_ReadByte(_this + 3));
    Bool isValueTable = (ktype != Type::eString);
    UInt pEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    for (;;)
    {
        UInt pEntry = pEntries + 8 * iterator;
        iterator = (iterator + 0x01) % capacity;
        if (isValueTable)
        {
            value = Memory_ReadWord(pEntry + 6);
            Bool isOccupied = Memory_ReadByte(pEntry + 2) != 0x00;
            if (!isOccupied)
            {
            }
            else
            {
                key = Memory_ReadWord(pEntry + 0);
                value = Memory_ReadWord(pEntry + 6);
                if (iterator == 0x00)
                {
                    iterator = 0xFFFF;
                }
                success = true;
                break;;
            }
        }
        else
        {
            key = Memory_ReadWord(pEntry + 0);
            if (key == 0x00)
            {
            }
            else
            {
                value = Memory_ReadWord(pEntry + 6);
                if (iterator == 0x00)
                {
                    iterator = 0xFFFF;
                }
                success = true;
                break;;
            }
        }
        if (iterator == 0x00)
        {
            success = false;
            break;;
        }
    }
    if (success && Types_IsReferenceType(vtype))
    {
        vtype = Type(Memory_ReadByte(value));
    }
    return success;
}

void HRPair_Clear(UInt _this)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Type dvType = Type(Memory_ReadByte(_this + 3));
    UInt key = Memory_ReadWord(_this + 4);
    UInt value = Memory_ReadWord(_this + 6);
    if (Types_IsReferenceType(dkType) && (0x00 != key))
    {
        GC_Release(key);
    }
    if (Types_IsReferenceType(dvType) && (0x00 != value))
    {
        GC_Release(value);
    }
    Memory_WriteWord(_this + 4, 0x00);
    Memory_WriteWord(_this + 6, 0x00);
}

void HRVariant_Clear(UInt _this)
{
    Type vtype = Type(Memory_ReadByte(_this + 2));
    UInt value = Memory_ReadWord(_this + 3);
    if (Types_IsReferenceType(vtype) && (0x00 != value))
    {
        GC_Release(value);
    }
    Memory_WriteWord(_this + 2, 0x00);
    Memory_WriteWord(_this + 3, 0x00);
}

UInt GC_New(UInt size, Type htype)
{
    UInt address = Memory_Allocate(size + 0x02);
    Memory_WriteByte(address, Byte(htype));
    Memory_WriteByte(address + 0x01, 0x01);
    return address;
}

void GC_AddReference(UInt address)
{
    Byte referenceCount = Memory_ReadByte(address + 0x01);
    
    referenceCount++;
    Memory_WriteByte(address + 0x01, referenceCount);
}

UInt GC_Clone(UInt original)
{
    Type htype = Type(Memory_ReadByte(original));
    switch (htype)
    {
    case Type::eLong:
    {
        return HRLong_Clone(original);
        break;
    }
    case Type::eDirectory:
    {
        return HRDirectory_Clone(original);
        break;
    }
    case Type::eFile:
    {
        return HRFile_Clone(original);
        break;
    }
    case Type::eFloat:
    {
        return HRFloat_Clone(original);
        break;
    }
    case Type::eString:
    {
        return HRString_Clone(original);
        break;
    }
    case Type::eList:
    {
        return HRList_Clone(original);
        break;
    }
    case Type::eArray:
    {
        return HRArray_Clone(original);
        break;
    }
    case Type::eDictionary:
    {
        return HRDictionary_Clone(original);
        break;
    }
    case Type::ePair:
    {
        return HRPair_Clone(original);
        break;
    }
    case Type::eVariant:
    {
        return HRVariant_Clone(original);
        break;
    }
    default:
    {
        break;
    }
    } // switch
    return 0x00;
}

UInt HRString_Clone(UInt original)
{
    return HRString_clone(original, 0x00);
}

Byte HRLong_GetByte(UInt ichunk, UInt i)
{
    return Memory_ReadByte(ichunk + 0x02 + i);
}

UInt HRLong_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3)
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteByte(address + 0x02, b0);
    Memory_WriteByte(address + 0x02 + 0x01, b1);
    Memory_WriteByte(address + 0x02 + 0x02, b2);
    Memory_WriteByte(address + 0x02 + 0x03, b3);
    return address;
}

UInt HRLong_Clone(UInt original)
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteWord(address + 0x02, Memory_ReadWord(original + 0x02));
    Memory_WriteWord(address + 0x04, Memory_ReadWord(original + 0x04));
    return address;
}

void Runtime_ErrorDump(UInt number)
{
    IO_Write('D');
    IO_Write('A');
    IO_Write('N');
    IO_Write('G');
    IO_Write('!');
    IO_WriteUInt(number);
}

void Runtime_Out4Hex(UInt value)
{
    Byte b = Byte(value >> 0x0C);
    Runtime_SerialWriteChar(HRByte_ToHex(b));
    b = Byte((value >> 0x08) & 0x0F);
    Runtime_SerialWriteChar(HRByte_ToHex(b));
    b = Byte((value >> 0x04) & 0x0F);
    Runtime_SerialWriteChar(HRByte_ToHex(b));
    b = Byte(value & 0x0F);
    Runtime_SerialWriteChar(HRByte_ToHex(b));
}

void Runtime_Out2Hex(Byte value)
{
    Byte b = Byte((value >> 0x04) & 0x0F);
    Runtime_SerialWriteChar(HRByte_ToHex(b));
    b = Byte(value & 0x0F);
    Runtime_SerialWriteChar(HRByte_ToHex(b));
}

OpCode HopperVM_CurrentOpCode_Get()
{
    return HopperVM_opCode;
}

UInt HopperVM_Pop_R(Type & htype)
{
    
    HopperVM_sp--;
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    htype = Type(Memory_ReadByte(HopperVM_typeStackPage + HopperVM_sp));
    return value;
}

Int HopperVM_PopI()
{
    
    HopperVM_sp--;
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    return External_UIntToInt(value);
}

void HopperVM_PushI(Int ivalue)
{
    UInt value = External_IntToUInt(ivalue);
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(Type::eInt));
    
    HopperVM_sp++;
}

void HopperVM_CNP_Set(Bool value)
{
    HopperVM_cnp = value;
}

Int HopperVM_ReadByteOffsetOperand()
{
    Int offset = Int(Memory_ReadProgramByte(HopperVM_pc));
    
    HopperVM_pc++;
    if (offset > 0x7F)
    {
        offset = offset - 0x0100;
    }
    return offset;
}

UInt HopperVM_TypeStackLSB_Get()
{
    return HopperVM_typeStackPage;
}

void HopperVM_Put(Byte address, UInt value, Type htype)
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + address, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + address, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + address, Byte(htype));
}

void HopperVM_Push(UInt value, Type htype)
{
    Memory_WriteByte(HopperVM_valueStackLSBPage + HopperVM_sp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_valueStackMSBPage + HopperVM_sp, Byte(value >> 0x08));
    Memory_WriteByte(HopperVM_typeStackPage + HopperVM_sp, Byte(htype));
    
    HopperVM_sp++;
}

Byte HopperVM_ReadByteOperand()
{
    Byte operand = Memory_ReadProgramByte(HopperVM_pc);
    
    HopperVM_pc++;
    return operand;
}

Byte HopperVM_GP_Get()
{
    return HopperVM_gp;
}

void HopperVM_PushCS(UInt value)
{
    Memory_WriteByte(HopperVM_callStackLSBPage + HopperVM_csp, Byte(value & 0xFF));
    Memory_WriteByte(HopperVM_callStackMSBPage + HopperVM_csp, Byte(value >> 0x08));
    
    HopperVM_csp++;
}

void HopperVM_PC_Set(UInt value)
{
    HopperVM_pc = value;
}

UInt HopperVM_LookupMethod(UInt methodIndex)
{
    methodIndex = (methodIndex & 0x3FFF);
    UInt address = HopperVM_methodTable;
    for (;;)
    {
        UInt entry = Memory_ReadCodeWord(address);
        if (entry == methodIndex)
        {
            address = Memory_ReadCodeWord(address + 0x02);
            break;;
        }
        address = address + 0x04;
    }
    return address;
}

UInt HopperVM_Pop()
{
    
    HopperVM_sp--;
    return Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
}

Bool HopperVM_ExecuteSysCall(Byte iSysCall, UInt iOverload)
{
    Bool doNext = true;
    switch (SysCalls(iSysCall))
    {
    case SysCalls::eDiagnosticsDie:
    {
        doNext = Instructions_Die();
        break;
    }
    case SysCalls::eRuntimeInline:
    {
        if (!HopperVM_RunInline())
        {
            Runtime_ErrorDump(0xA5);
            Minimal_Error_Set(0x0B);
            doNext = false;
        }
        break;
    }
    case SysCalls::eRuntimeExecute:
    {
        Type ltype = (Type)0;
        UInt args = HopperVM_Pop_R(ltype);
        Type stype = (Type)0;
        UInt path = HopperVM_Pop_R(stype);
        UInt result = HopperVM_RuntimeExecute(path, args);
        GC_Release(args);
        GC_Release(path);
        HopperVM_Push(result, Type::eUInt);
        doNext = false;
        break;
    }
    case SysCalls::eRuntimeUserCodeGet:
    {
        HopperVM_Push(HopperVM_programSize, Type::eUInt);
        break;
    }
    case SysCalls::eRuntimeInDebuggerGet:
    {
        HopperVM_Push((HopperVM_inDebugger) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case SysCalls::eRuntimeDateTimeGet:
    {
        UInt dateTime = HopperVM_RuntimeDateTime();
        HopperVM_Push(dateTime, Type::eString);
        break;
    }
    case SysCalls::eMemoryAvailable:
    {
        UInt size = Memory_Available();
        HopperVM_Push(size, Type::eUInt);
        break;
    }
    case SysCalls::eMemoryMaximum:
    {
        UInt size = Memory_Maximum();
        HopperVM_Push(size, Type::eUInt);
        break;
    }
    case SysCalls::eMemoryAllocate:
    {
        Type atype = (Type)0;
        UInt size = HopperVM_Pop_R(atype);
        UInt address = Memory_Allocate(size);
        HopperVM_Push(address, Type::eUInt);
        break;
    }
    case SysCalls::eMemoryFree:
    {
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        Memory_Free(address);
        break;
    }
    case SysCalls::eMemoryReadBit:
    {
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        address = address + (index >> 0x03);
        Byte mask = (0x01 << (index & 0x07));
        Byte value = Memory_ReadByte(address) & mask;
        HopperVM_Push(((value != 0x00)) ? (0x01) : (0x00), Type::eByte);
        break;
    }
    case SysCalls::eMemoryWriteBit:
    {
        Type btype = (Type)0;
        UInt data = HopperVM_Pop_R(btype);
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        address = address + (index >> 0x03);
        Byte mask = (0x01 << (index & 0x07));
        Byte current = Memory_ReadByte(address);
        if (data == 0x00)
        {
            Memory_WriteByte(address, Byte(current & ~mask));
        }
        else
        {
            Memory_WriteByte(address, Byte(current | mask));
        }
        break;
    }
    case SysCalls::eMemoryReadByte:
    {
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        Byte b = Memory_ReadByte(address);
        HopperVM_Push(b, Type::eByte);
        break;
    }
    case SysCalls::eMemoryWriteByte:
    {
        Type btype = (Type)0;
        UInt b = HopperVM_Pop_R(btype);
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        Memory_WriteByte(address, Byte(b));
        break;
    }
    case SysCalls::eMemoryReadWord:
    {
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        UInt w = Memory_ReadWord(address);
        HopperVM_Push(w, Type::eUInt);
        break;
    }
    case SysCalls::eMemoryWriteWord:
    {
        Type btype = (Type)0;
        UInt w = HopperVM_Pop_R(btype);
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        Memory_WriteWord(address, w);
        break;
    }
    case SysCalls::eSystemArgumentsGet:
    {
        if (0x00 == HopperVM_currentArguments)
        {
            HopperVM_currentArguments = HRList_New(Type::eString);
        }
        HopperVM_Push(GC_Clone(HopperVM_currentArguments), Type::eString);
        break;
    }
    case SysCalls::eSystemCurrentDirectoryGet:
    {
        if (0x00 == HopperVM_currentDirectory)
        {
            HopperVM_currentDirectory = HRString_NewFromConstant1(UInt('/'));
        }
        HopperVM_Push(GC_Clone(HopperVM_currentDirectory), Type::eString);
        break;
    }
    case SysCalls::eSystemCurrentDirectorySet:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        if (0x00 != HopperVM_currentDirectory)
        {
            GC_Release(HopperVM_currentDirectory);
        }
        HopperVM_currentDirectory = GC_Clone(str);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileNew:
    {
        UInt result = HRFile_New();
        HopperVM_Push(result, Type::eFile);
        break;
    }
    case SysCalls::eFileExists:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        Bool result = HRFile_Exists(str);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileIsValid:
    {
        Type stype = (Type)0;
        UInt hrfile = HopperVM_Pop_R(stype);
        Bool result = HRFile_IsValid(hrfile);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(hrfile);
        break;
    }
    case SysCalls::eFileFlush:
    {
        Type stype = (Type)0;
        UInt hrfile = HopperVM_Pop_R(stype);
        HRFile_Flush(hrfile);
        GC_Release(hrfile);
        break;
    }
    case SysCalls::eFileReadLine:
    {
        Type stype = (Type)0;
        UInt hrfile = HopperVM_Pop_R(stype);
        UInt str = HRFile_ReadLine(hrfile);
        GC_Release(hrfile);
        HopperVM_Push(str, Type::eString);
        break;
    }
    case SysCalls::eFileRead:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            UInt b = HRFile_Read(hrfile);
            GC_Release(hrfile);
            HopperVM_Push(b, Type::eByte);
            break;
        }
        case 0x01:
        {
            Type ltype = (Type)0;
            UInt hrlong = HopperVM_Pop_R(ltype);
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            UInt b = HRFile_Read(hrfile, hrlong);
            GC_Release(hrfile);
            GC_Release(hrlong);
            HopperVM_Push(b, Type::eByte);
            break;
        }
        case 0x02:
        {
            Type ltype = (Type)0;
            UInt bufferSize = HopperVM_Pop_R(ltype);
            Type atype = (Type)0;
            UInt hrarray = HopperVM_Pop_R(atype);
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            UInt bytesRead = HRFile_Read(hrfile, hrarray, bufferSize);
            GC_Release(hrfile);
            GC_Release(hrarray);
            HopperVM_Push(bytesRead, Type::eUInt);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eFileAppend:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            UInt top = HopperVM_Pop();
            Type stype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(stype);
            HRFile_Append(hrfile, Byte(top));
            GC_Release(hrfile);
            break;
        }
        case 0x01:
        {
            Type stype = (Type)0;
            UInt str = HopperVM_Pop_R(stype);
            Type ftype = (Type)0;
            UInt hrfile = HopperVM_Pop_R(ftype);
            HRFile_Append(hrfile, str);
            GC_Release(str);
            GC_Release(hrfile);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eFileCreate:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_Create(str);
        HopperVM_Push(result, Type::eFile);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileOpen:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_Open(str);
        HopperVM_Push(result, Type::eFile);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileDelete:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        HRFile_Delete(str);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileGetTimeStamp:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_GetTimeStamp(str);
        HopperVM_Push(result, Type::eLong);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileGetTime:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_GetTime(str);
        HopperVM_Push(result, Type::eString);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileGetDate:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_GetDate(str);
        HopperVM_Push(result, Type::eString);
        GC_Release(str);
        break;
    }
    case SysCalls::eDirectoryGetTime:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetTime(str);
        HopperVM_Push(result, Type::eString);
        GC_Release(str);
        break;
    }
    case SysCalls::eDirectoryGetDate:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetDate(str);
        HopperVM_Push(result, Type::eString);
        GC_Release(str);
        break;
    }
    case SysCalls::eFileGetSize:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRFile_GetSize(str);
        HopperVM_Push(result, Type::eLong);
        GC_Release(str);
        break;
    }
    case SysCalls::eDirectoryNew:
    {
        UInt result = HRDirectory_New();
        HopperVM_Push(result, Type::eDirectory);
        break;
    }
    case SysCalls::eDirectoryExists:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        Bool result = HRDirectory_Exists(str);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(str);
        break;
    }
    case SysCalls::eDirectoryOpen:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_Open(str);
        HopperVM_Push(result, Type::eDirectory);
        GC_Release(str);
        break;
    }
    case SysCalls::eDirectoryIsValid:
    {
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        Bool result = HRDirectory_IsValid(hrdir);
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        GC_Release(hrdir);
        break;
    }
    case SysCalls::eDirectoryGetFileCount:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type stype = (Type)0;
            UInt hrdir = HopperVM_Pop_R(stype);
            UInt result = HRDirectory_GetFileCount(hrdir);
            HopperVM_Push(result, Type::eUInt);
            GC_Release(hrdir);
            break;
        }
        case 0x01:
        {
            Type utype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(utype));
            UInt skipped = HopperVM_Get_R(address, utype);
            Type stype = (Type)0;
            UInt hrdir = HopperVM_Pop_R(stype);
            UInt result = HRDirectory_GetFileCount_R(hrdir, skipped);
            HopperVM_Push(result, Type::eUInt);
            HopperVM_Put(address, skipped, Type::eUInt);
            GC_Release(hrdir);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eDirectoryGetDirectoryCount:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type stype = (Type)0;
            UInt hrdir = HopperVM_Pop_R(stype);
            UInt result = HRDirectory_GetDirectoryCount(hrdir);
            HopperVM_Push(result, Type::eUInt);
            GC_Release(hrdir);
            break;
        }
        case 0x01:
        {
            Type utype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(utype));
            UInt skipped = HopperVM_Get_R(address, utype);
            Type stype = (Type)0;
            UInt hrdir = HopperVM_Pop_R(stype);
            UInt result = HRDirectory_GetDirectoryCount_R(hrdir, skipped);
            HopperVM_Push(result, Type::eUInt);
            HopperVM_Put(address, skipped, Type::eUInt);
            GC_Release(hrdir);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eDirectoryGetFile:
    {
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetFile(hrdir, index);
        HopperVM_Push(result, Type::eString);
        GC_Release(hrdir);
        break;
    }
    case SysCalls::eDirectoryGetDirectory:
    {
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type stype = (Type)0;
        UInt hrdir = HopperVM_Pop_R(stype);
        UInt result = HRDirectory_GetDirectory(hrdir, index);
        HopperVM_Push(result, Type::eString);
        GC_Release(hrdir);
        break;
    }
    case SysCalls::eDirectoryDelete:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        HRDirectory_Delete(str);
        GC_Release(str);
        break;
    }
    case SysCalls::eDirectoryCreate:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        HRDirectory_Create(str);
        GC_Release(str);
        break;
    }
    case SysCalls::eSerialIsAvailableGet:
    {
        Bool avail = Serial_IsAvailable_Get();
        HopperVM_Push(UInt(avail), Type::eBool);
        break;
    }
    case SysCalls::eSerialReadChar:
    {
        Char ch = Runtime_SerialReadChar();
        HopperVM_Push(UInt(ch), Type::eChar);
        break;
    }
    case SysCalls::eSerialWriteChar:
    {
        Type atype = (Type)0;
        UInt ch = HopperVM_Pop_R(atype);
        Runtime_SerialWriteChar(Char(ch));
        break;
    }
    case SysCalls::eSerialWriteString:
    {
        Type stype = (Type)0;
        UInt str = HopperVM_Pop_R(stype);
        External_SerialWriteString(str);
        break;
    }
    case SysCalls::eLongNewFromConstant:
    {
        Type atype = (Type)0;
        UInt location = HopperVM_Pop_R(atype);
        UInt address = HRLong_NewFromConstant(HopperVM_constAddress + location);
        HopperVM_Push(address, Type::eLong);
        break;
    }
    case SysCalls::eFloatNewFromConstant:
    {
        Type atype = (Type)0;
        UInt location = HopperVM_Pop_R(atype);
        UInt address = HRFloat_NewFromConstant(HopperVM_constAddress + location);
        HopperVM_Push(address, Type::eFloat);
        break;
    }
    case SysCalls::eStringNewFromConstant:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type ltype = (Type)0;
            UInt length = HopperVM_Pop_R(ltype);
            Type atype = (Type)0;
            UInt location = HopperVM_Pop_R(atype);
            UInt address = HRString_NewFromConstant0(HopperVM_constAddress + location, length);
            HopperVM_Push(address, Type::eString);
            break;
        }
        case 0x01:
        {
            Type utype = (Type)0;
            UInt doubleChar = HopperVM_Pop_R(utype);
            UInt address = HRString_NewFromConstant1(doubleChar);
            HopperVM_Push(address, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x05);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringPushImmediate:
    {
        UInt address = HRString_New();
        for (;;)
        {
            Type utype = (Type)0;
            UInt content = HopperVM_Pop_R(utype);
            Byte lsb = Byte(content & 0xFF);
            Byte msb = Byte(content >> 0x08);
            if (lsb == 0x00)
            {
                break;;
            }
            HRString_BuildChar_R(address, Char(lsb));
            if (msb == 0x00)
            {
                break;;
            }
            HRString_BuildChar_R(address, Char(msb));
        }
        HopperVM_Push(address, Type::eString);
        break;
    }
    case SysCalls::eStringNew:
    {
        UInt address = HRString_New();
        HopperVM_Push(address, Type::eString);
        break;
    }
    case SysCalls::eStringLengthGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt length = HRString_GetLength(_this);
        GC_Release(_this);
        HopperVM_Push(length, Type::eUInt);
        break;
    }
    case SysCalls::eStringGetChar:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Char ch = HRString_GetChar(_this, index);
        GC_Release(_this);
        HopperVM_Push(UInt(ch), Type::eChar);
        break;
    }
    case SysCalls::eStringInsertChar:
    {
        Type atype = (Type)0;
        UInt ch = HopperVM_Pop_R(atype);
        Type itype = (Type)0;
        UInt index = HopperVM_Pop_R(itype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt result = HRString_InsertChar(_this, index, Char(ch));
        GC_Release(_this);
        HopperVM_Push(result, Type::eString);
        break;
    }
    case SysCalls::eStringToUpper:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_ToUpper(_this);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_ToUpper_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringToLower:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_ToLower(_this);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_ToLower_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringStartsWith:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_StartsWith(_this, Char(with));
            GC_Release(_this);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_StartsWith(_this, with);
            GC_Release(_this);
            GC_Release(with);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0xE1);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringContains:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_Contains(_this, Char(with));
            GC_Release(_this);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0xDF);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringIndexOf:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt index = HopperVM_Get_R(address, htype);
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_IndexOf_R(_this, Char(with), index);
            if (result)
            {
                HopperVM_Put(address, index, Type::eUInt);
            }
            GC_Release(_this);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt index = HopperVM_Get_R(address, htype);
            Type itype = (Type)0;
            UInt searchIndex = HopperVM_Pop_R(itype);
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_IndexOf_R(_this, Char(with), searchIndex, index);
            if (result)
            {
                HopperVM_Put(address, index, Type::eUInt);
            }
            GC_Release(_this);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0xE0);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringEndsWith:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_EndsWith(_this, Char(with));
            GC_Release(_this);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt with = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            Bool result = HRString_EndsWith(_this, with);
            GC_Release(_this);
            GC_Release(with);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringCompare:
    {
        Type atype = (Type)0;
        UInt right = HopperVM_Pop_R(atype);
        Type btype = (Type)0;
        UInt left = HopperVM_Pop_R(btype);
        Int result = HRString_Compare(left, right);
        GC_Release(right);
        GC_Release(left);
        HopperVM_PushI(result);
        break;
    }
    case SysCalls::eStringReplace:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt to = HopperVM_Pop_R(atype);
            Type btype = (Type)0;
            UInt from = HopperVM_Pop_R(btype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Replace(_this, from, to);
            GC_Release(_this);
            GC_Release(to);
            GC_Release(from);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt to = HopperVM_Pop_R(atype);
            Type btype = (Type)0;
            UInt from = HopperVM_Pop_R(btype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Replace(_this, Char(from), Char(to));
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringAppend:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt append = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Append(_this, append);
            GC_Release(_this);
            GC_Release(append);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt append = HopperVM_Pop_R(atype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Append(_this, Char(append));
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringSubstring:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type stype = (Type)0;
            UInt start = HopperVM_Pop_R(stype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Substring(_this, start);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type ltype = (Type)0;
            UInt limit = HopperVM_Pop_R(ltype);
            Type stype = (Type)0;
            UInt start = HopperVM_Pop_R(stype);
            Type ttype = (Type)0;
            UInt _this = HopperVM_Pop_R(ttype);
            UInt result = HRString_Substring(_this, start, limit);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x02:
        {
            Type stype = (Type)0;
            UInt start = HopperVM_Pop_R(stype);
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_Substring_R(str, start);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x12);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringBuild:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt append = HopperVM_Pop_R(atype);
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_BuildString_R(str, append);
            HopperVM_Put(address, str, Type::eString);
            GC_Release(append);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Char ch = Char(HopperVM_Pop_R(htype));
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_BuildChar_R(str, ch);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        case 0x02:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_BuildClear_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x2A);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringBuildFront:
    {
        Type htype = (Type)0;
        Char ch = Char(HopperVM_Pop_R(htype));
        Byte address = Byte(HopperVM_Pop_R(htype));
        UInt str = HopperVM_Get_R(address, htype);
        HRString_BuildFront_R(str, ch);
        HopperVM_Put(address, str, Type::eString);
        break;
    }
    case SysCalls::eStringTrim:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type htype = (Type)0;
            UInt _this = HopperVM_Pop_R(htype);
            UInt result = HRString_Trim(_this);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_TrimRight_R(str);
            HRString_TrimLeft_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x04);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringTrimLeft:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type htype = (Type)0;
            UInt _this = HopperVM_Pop_R(htype);
            UInt result = HRString_TrimLeft(_this);
            GC_Release(_this);
            HopperVM_Push(result, Type::eString);
            break;
        }
        case 0x01:
        {
            Type htype = (Type)0;
            Byte address = Byte(HopperVM_Pop_R(htype));
            UInt str = HopperVM_Get_R(address, htype);
            HRString_TrimLeft_R(str);
            HopperVM_Put(address, str, Type::eString);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x03);
            Minimal_Error_Set(0x0A);
            break;
        }
        } // switch
        break;
    }
    case SysCalls::eStringTrimRight:
    {
        Type htype = (Type)0;
        Byte address = Byte(HopperVM_Pop_R(htype));
        UInt str = HopperVM_Get_R(address, htype);
        HRString_TrimRight_R(str);
        HopperVM_Put(address, str, Type::eString);
        break;
    }
    case SysCalls::eWiFiConnect:
    {
        Type ptype = (Type)0;
        UInt password = HopperVM_Pop_R(ptype);
        Type stype = (Type)0;
        UInt ssid = HopperVM_Pop_R(stype);
        Bool success = External_WiFiConnect(ssid, password);
        GC_Release(ssid);
        GC_Release(password);
        HopperVM_Push((success) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case SysCalls::eWiFiIPGet:
    {
        UInt ip = External_WiFiIP();
        HopperVM_Push(ip, Type::eString);
        break;
    }
    case SysCalls::eWiFiStatusGet:
    {
        UInt status = External_WiFiStatus();
        HopperVM_Push(status, Type::eUInt);
        break;
    }
    case SysCalls::eWiFiDisconnect:
    {
        External_WiFiDisconnect();
        break;
    }
    case SysCalls::eArrayNew:
    {
        Type stype = (Type)0;
        Type htype = Type(HopperVM_Pop_R(stype));
        UInt count = HopperVM_Pop_R(stype);
        UInt address = HRArray_New(htype, count);
        HopperVM_Push(address, Type::eArray);
        break;
    }
    case SysCalls::eArrayNewFromConstant:
    {
        Type stype = (Type)0;
        Type ltype = (Type)0;
        Type htype = Type(HopperVM_Pop_R(stype));
        UInt length = HopperVM_Pop_R(stype);
        UInt location = HopperVM_Pop_R(ltype);
        UInt address = HRArray_NewFromConstant(HopperVM_constAddress + location, htype, length);
        HopperVM_Push(address, Type::eArray);
        break;
    }
    case SysCalls::eArrayGetItem:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type etype = (Type)0;
        UInt item = HRArray_GetItem_R(_this, index, etype);
        GC_Release(_this);
        HopperVM_Push(item, etype);
        break;
    }
    case SysCalls::eArraySetItem:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRArray_SetItem(_this, index, item);
        GC_Release(_this);
        break;
    }
    case SysCalls::eArrayCountGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt length = HRArray_GetCount(_this);
        GC_Release(_this);
        HopperVM_Push(length, Type::eUInt);
        break;
    }
    case SysCalls::eListNew:
    {
        Type stype = (Type)0;
        Type htype = Type(HopperVM_Pop_R(stype));
        UInt address = HRList_New(htype);
        HopperVM_Push(address, Type::eList);
        break;
    }
    case SysCalls::eListCountGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt count = HRList_GetCount(_this);
        GC_Release(_this);
        HopperVM_Push(count, Type::eUInt);
        break;
    }
    case SysCalls::eListAppend:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Append(_this, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        break;
    }
    case SysCalls::eListSetItem:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_SetItem(_this, index, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        break;
    }
    case SysCalls::eListInsert:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Insert(_this, index, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        break;
    }
    case SysCalls::eListGetItem:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type itype = (Type)0;
        UInt item = HRList_GetItem_R(_this, index, itype);
        GC_Release(_this);
        HopperVM_Push(item, itype);
        break;
    }
    case SysCalls::eListGetItemAsVariant:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type itype = (Type)0;
        UInt item = HRList_GetItem_R(_this, index, itype);
        if (!Types_IsReferenceType(itype))
        {
            item = HRVariant_CreateValueVariant(item, itype);
            itype = Type::eVariant;
        }
        GC_Release(_this);
        HopperVM_Push(item, itype);
        break;
    }
    case SysCalls::eListClear:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Clear(_this);
        GC_Release(_this);
        break;
    }
    case SysCalls::eListRemove:
    {
        Type atype = (Type)0;
        UInt index = HopperVM_Pop_R(atype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRList_Remove(_this, index);
        GC_Release(_this);
        break;
    }
    case SysCalls::eListContains:
    {
        Type itype = (Type)0;
        UInt item = HopperVM_Pop_R(itype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Bool contains = HRList_Contains(_this, item, itype);
        if (Types_IsReferenceType(itype))
        {
            GC_Release(item);
        }
        GC_Release(_this);
        HopperVM_Push((contains) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case SysCalls::ePairNew:
    {
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        UInt address = HRPair_New(ktype, key, vtype, value);
        HopperVM_Push(address, Type::ePair);
        break;
    }
    case SysCalls::eVariantBox:
    {
        Type vvtype = (Type)0;
        Type vtype = Type(HopperVM_Pop_R(vvtype));
        Type vttype = (Type)0;
        UInt value = HopperVM_Pop_R(vttype);
        UInt address = HRVariant_New(value, vtype);
        HopperVM_Push(address, Type::eVariant);
        break;
    }
    case SysCalls::eVariantUnBox:
    {
        Type vType = (Type)0;
        UInt _this = HopperVM_Pop_R(vType);
        Type mType = (Type)0;
        UInt member = HRVariant_UnBox_R(_this, mType);
        HopperVM_Push(member, mType);
        GC_Release(_this);
        break;
    }
    case SysCalls::ePairValue:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type vtype = (Type)0;
        UInt value = HRPair_GetValue_R(_this, vtype);
        GC_Release(_this);
        HopperVM_Push(value, vtype);
        break;
    }
    case SysCalls::ePairKey:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type ktype = (Type)0;
        UInt key = HRPair_GetKey_R(_this, ktype);
        GC_Release(_this);
        HopperVM_Push(key, ktype);
        break;
    }
    case SysCalls::eTypesTypeOf:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        if (Types_IsReferenceType(ttype))
        {
            GC_Release(_this);
        }
        HopperVM_Push(Byte(ttype), Type::eType);
        break;
    }
    case SysCalls::eTypesBoxTypeOf:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        if (Types_IsReferenceType(ttype))
        {
            ttype = Type(Memory_ReadByte(_this));
            if (ttype == Type::eVariant)
            {
                ttype = Type(Memory_ReadByte(_this + 0x02));
            }
            GC_Release(_this);
        }
        HopperVM_Push(Byte(ttype), Type::eType);
        break;
    }
    case SysCalls::eTypesVerifyValueTypes:
    {
        Type ttype = (Type)0;
        Type memberType = Type(HopperVM_Pop_R(ttype));
        UInt _this = HopperVM_Pop_R(ttype);
        Bool success = true;
        switch (ttype)
        {
        case Type::eList:
        {
            UInt count = HRList_GetCount(_this);;
            for (UInt i = 0x00; i < count; i++)
            {
                Type itype = (Type)0;
                UInt item = HRList_GetItem_R(_this, i, itype);
                if (Types_IsReferenceType(itype))
                {
                    GC_Release(item);
                }
                if (itype != memberType)
                {
                    success = false;
                    break;;
                }
            }
            break;
        }
        case Type::eDictionary:
        {
            UInt iterator = 0;
            UInt hrpair = 0;
            while (HRDictionary_Next_R(_this, iterator, hrpair))
            {
                Type vtype = HRPair_GetValueType(hrpair);
                GC_Release(hrpair);
                if (vtype != memberType)
                {
                    success = false;
                    break;;
                }
            }
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x0C);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        HopperVM_Push((success) ? (0x01) : (0x00), Type::eBool);
        GC_Release(_this);
        break;
    }
    case SysCalls::eTypesKeyTypeOf:
    {
        Type vtype = (Type)0;
        UInt _this = HopperVM_Pop_R(vtype);
        switch (vtype)
        {
        case Type::eDictionary:
        {
            HopperVM_Push(UInt(HRDictionary_GetKeyType(_this)), Type::eType);
            break;
        }
        case Type::ePair:
        {
            HopperVM_Push(UInt(HRPair_GetKeyType(_this)), Type::eType);
            break;
        }
        default:
        {
            Runtime_ErrorDump(0x0B);
            Minimal_Error_Set(0x0B);
            break;
        }
        } // switch
        if (Types_IsReferenceType(vtype))
        {
            GC_Release(_this);
        }
        break;
    }
    case SysCalls::eTypesValueTypeOf:
    {
        Type vtype = (Type)0;
        UInt _this = HopperVM_Pop_R(vtype);
        switch (vtype)
        {
        case Type::eDictionary:
        {
            HopperVM_Push(UInt(HRDictionary_GetValueType(_this)), Type::eType);
            break;
        }
        case Type::ePair:
        {
            HopperVM_Push(UInt(HRPair_GetValueType(_this)), Type::eType);
            break;
        }
        case Type::eList:
        {
            HopperVM_Push(UInt(HRList_GetValueType(_this)), Type::eType);
            break;
        }
        case Type::eArray:
        {
            HopperVM_Push(UInt(HRArray_GetValueType(_this)), Type::eType);
            break;
        }
        default:
        {
            if (!Types_IsReferenceType(vtype))
            {
                HopperVM_Push(UInt(HRVariant_GetValueType(_this)), Type::eType);
            }
            else
            {
                Runtime_ErrorDump(0x0A);
                Minimal_Error_Set(0x0B);
            }
            break;
        }
        } // switch
        if (Types_IsReferenceType(vtype))
        {
            GC_Release(_this);
        }
        break;
    }
    case SysCalls::eDictionaryNew:
    {
        Type stype = (Type)0;
        Type vtype = Type(HopperVM_Pop_R(stype));
        Type ktype = Type(HopperVM_Pop_R(stype));
        UInt address = HRDictionary_New(ktype, vtype);
        HopperVM_Push(address, Type::eDictionary);
        break;
    }
    case SysCalls::eDictionaryCountGet:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt count = HRDictionary_GetCount(_this);
        GC_Release(_this);
        HopperVM_Push(count, Type::eUInt);
        break;
    }
    case SysCalls::eDictionarySet:
    {
        Type vtype = (Type)0;
        UInt value = HopperVM_Pop_R(vtype);
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRDictionary_Set(_this, key, ktype, value, vtype);
        if (Types_IsReferenceType(ktype))
        {
            GC_Release(key);
        }
        if (Types_IsReferenceType(vtype))
        {
            GC_Release(value);
        }
        GC_Release(_this);
        break;
    }
    case SysCalls::eDictionaryNext:
    {
        Type htype = (Type)0;
        UInt iterator = HopperVM_Pop_R(htype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt hrpair = 0;
        UInt found = (HRDictionary_Next_R(_this, iterator, hrpair)) ? (0x01) : (0x00);
        GC_Release(_this);
        HopperVM_Push(found, Type::eBool);
        HopperVM_Push(hrpair, Type::ePair);
        HopperVM_Push(iterator, Type::eUInt);
        break;
    }
    case SysCalls::eDictionaryContains:
    {
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        UInt found = (HRDictionary_Contains(_this, key)) ? (0x01) : (0x00);
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        GC_Release(_this);
        HopperVM_Push(found, Type::eBool);
        break;
    }
    case SysCalls::eDictionaryGet:
    {
        Type ktype = (Type)0;
        UInt key = HopperVM_Pop_R(ktype);
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        Type vtype = (Type)0;
        UInt result = HRDictionary_Get_R(_this, key, vtype);
        if (ktype == Type::eString)
        {
            GC_Release(key);
        }
        GC_Release(_this);
        HopperVM_Push(result, vtype);
        break;
    }
    case SysCalls::eDictionaryClear:
    {
        Type ttype = (Type)0;
        UInt _this = HopperVM_Pop_R(ttype);
        HRDictionary_Clear(_this);
        GC_Release(_this);
        break;
    }
    case SysCalls::eCharToString:
    {
        Type utype = (Type)0;
        UInt singleChar = HopperVM_Pop_R(utype);
        UInt address = HRString_NewFromConstant1(singleChar);
        HopperVM_Push(address, Type::eString);
        break;
    }
    case SysCalls::eCharToUpper:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_ToUpper(Char(ch))), Type::eChar);
        break;
    }
    case SysCalls::eCharToLower:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_ToLower(Char(ch))), Type::eChar);
        break;
    }
    case SysCalls::eCharIsUpper:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_IsUpper(Char(ch))), Type::eBool);
        break;
    }
    case SysCalls::eCharIsLower:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_IsLower(Char(ch))), Type::eBool);
        break;
    }
    case SysCalls::eCharIsDigit:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_IsDigit(Char(ch))), Type::eBool);
        break;
    }
    case SysCalls::eCharIsLetterOrDigit:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_IsLetterOrDigit(Char(ch))), Type::eBool);
        break;
    }
    case SysCalls::eCharIsHexDigit:
    {
        Type utype = (Type)0;
        UInt ch = HopperVM_Pop_R(utype);
        HopperVM_Push(Byte(HRChar_IsHexDigit(Char(ch))), Type::eBool);
        break;
    }
    case SysCalls::eByteToDigit:
    {
        Type htype = (Type)0;
        UInt b = Byte(HopperVM_Pop_R(htype));
        HopperVM_Push(Byte(HRByte_ToDigit(Byte(b))), Type::eChar);
        break;
    }
    case SysCalls::eByteToHex:
    {
        Type htype = (Type)0;
        UInt b = Byte(HopperVM_Pop_R(htype));
        HopperVM_Push(Byte(HRByte_ToHex(Byte(b))), Type::eChar);
        break;
    }
    case SysCalls::eUIntToLong:
    {
        Type htype = (Type)0;
        UInt value = HopperVM_Pop_R(htype);
        UInt lng = HRUInt_ToLong(value);
        HopperVM_Push(lng, Type::eLong);
        break;
    }
    case SysCalls::eUIntToInt:
    {
        Type htype = (Type)0;
        UInt value = HopperVM_Pop_R(htype);
        HopperVM_PushI(Int(value));
        break;
    }
    case SysCalls::eIntToLong:
    {
        Type htype = (Type)0;
        UInt ichunk = HopperVM_Pop_R(htype);
        UInt lng = HRInt_ToLong(ichunk);
        HopperVM_Push(lng, Type::eLong);
        break;
    }
    case SysCalls::eIntToFloat:
    {
        Type htype = (Type)0;
        Int ichunk = HopperVM_PopI_R(htype);
        UInt f = External_IntToFloat(ichunk);
        HopperVM_Push(f, Type::eFloat);
        break;
    }
    case SysCalls::eUIntToFloat:
    {
        Type htype = (Type)0;
        UInt ichunk = HopperVM_Pop_R(htype);
        UInt f = External_UIntToFloat(ichunk);
        HopperVM_Push(f, Type::eFloat);
        break;
    }
    case SysCalls::eIntToBytes:
    {
        Type htype = (Type)0;
        UInt ichunk = HopperVM_Pop_R(htype);
        UInt lst = HRInt_ToBytes(ichunk);
        HopperVM_Push(lst, Type::eList);
        break;
    }
    case SysCalls::eLongToBytes:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt lst = HRLong_ToBytes(_this);
        HopperVM_Push(lst, Type::eList);
        GC_Release(_this);
        break;
    }
    case SysCalls::eFloatToLong:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt lng = External_FloatToLong(_this);
        HopperVM_Push(lng, Type::eLong);
        GC_Release(_this);
        break;
    }
    case SysCalls::eFloatToUInt:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt ui = External_FloatToUInt(_this);
        HopperVM_Push(ui, Type::eUInt);
        GC_Release(_this);
        break;
    }
    case SysCalls::eLongGetByte:
    {
        UInt index = HopperVM_Pop();
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        Byte b = HRLong_GetByte(_this, index);
        HopperVM_Push(b, Type::eByte);
        GC_Release(_this);
        break;
    }
    case SysCalls::eLongFromBytes:
    {
        Byte b3 = Byte(HopperVM_Pop());
        Byte b2 = Byte(HopperVM_Pop());
        Byte b1 = Byte(HopperVM_Pop());
        Byte b0 = Byte(HopperVM_Pop());
        UInt l = HRLong_FromBytes(b0, b1, b2, b3);
        HopperVM_Push(l, Type::eLong);
        break;
    }
    case SysCalls::eIntGetByte:
    {
        UInt index = HopperVM_Pop();
        Type htype = (Type)0;
        UInt i = HopperVM_Pop();
        Byte b = HRInt_GetByte(i, index);
        HopperVM_Push(b, Type::eByte);
        break;
    }
    case SysCalls::eIntFromBytes:
    {
        Byte b1 = Byte(HopperVM_Pop());
        Byte b0 = Byte(HopperVM_Pop());
        UInt i = HRInt_FromBytes(b0, b1);
        HopperVM_Push(i, Type::eInt);
        break;
    }
    case SysCalls::eFloatToBytes:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt lst = HRFloat_ToBytes(_this);
        HopperVM_Push(lst, Type::eList);
        GC_Release(_this);
        break;
    }
    case SysCalls::eFloatToString:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt str = External_FloatToString(_this);
        HopperVM_Push(str, Type::eString);
        GC_Release(_this);
        break;
    }
    case SysCalls::eLongToString:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt str = External_LongToString(_this);
        HopperVM_Push(str, Type::eString);
        GC_Release(_this);
        break;
    }
    case SysCalls::eFloatGetByte:
    {
        UInt index = HopperVM_Pop();
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        Byte b = HRFloat_GetByte(_this, index);
        HopperVM_Push(b, Type::eByte);
        GC_Release(_this);
        break;
    }
    case SysCalls::eFloatFromBytes:
    {
        Byte b3 = Byte(HopperVM_Pop());
        Byte b2 = Byte(HopperVM_Pop());
        Byte b1 = Byte(HopperVM_Pop());
        Byte b0 = Byte(HopperVM_Pop());
        UInt f = HRFloat_FromBytes(b0, b1, b2, b3);
        HopperVM_Push(f, Type::eFloat);
        break;
    }
    case SysCalls::eLongToUInt:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt ui = HRLong_ToUInt(_this);
        HopperVM_Push(ui, Type::eUInt);
        GC_Release(_this);
        break;
    }
    case SysCalls::eLongToInt:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        Int i = External_LongToInt(_this);
        HopperVM_PushI(i);
        GC_Release(_this);
        break;
    }
    case SysCalls::eLongToFloat:
    {
        Type htype = (Type)0;
        UInt _this = HopperVM_Pop_R(htype);
        UInt f = External_LongToFloat(_this);
        HopperVM_Push(f, Type::eFloat);
        GC_Release(_this);
        break;
    }
    case SysCalls::eLongNegate:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        UInt result = 0;
        Type rtype = Type::eLong;
        result = HRLong_LongNegate(top);
        HopperVM_Push(result, rtype);
        GC_Release(top);
        break;
    }
    case SysCalls::eLongAdd:
    case SysCalls::eLongSub:
    case SysCalls::eLongDiv:
    case SysCalls::eLongMul:
    case SysCalls::eLongMod:
    case SysCalls::eLongEQ:
    case SysCalls::eLongLT:
    case SysCalls::eLongLE:
    case SysCalls::eLongGT:
    case SysCalls::eLongGE:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        Type ntype = (Type)0;
        UInt next = HopperVM_Pop_R(ntype);
        UInt result = 0;
        Type rtype = Type::eLong;
        switch (SysCalls(iSysCall))
        {
        case SysCalls::eLongAdd:
        {
            result = External_LongAdd(next, top);
            break;
        }
        case SysCalls::eLongSub:
        {
            result = External_LongSub(next, top);
            break;
        }
        case SysCalls::eLongDiv:
        {
            result = External_LongDiv(next, top);
            break;
        }
        case SysCalls::eLongMul:
        {
            result = External_LongMul(next, top);
            break;
        }
        case SysCalls::eLongMod:
        {
            result = External_LongMod(next, top);
            break;
        }
        case SysCalls::eLongEQ:
        {
            result = External_LongEQ(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eLongLT:
        {
            result = External_LongLT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eLongLE:
        {
            result = External_LongLE(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eLongGT:
        {
            result = External_LongGT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eLongGE:
        {
            result = External_LongGE(next, top);
            rtype = Type::eBool;
            break;
        }
        } // switch
        HopperVM_Push(result, rtype);
        GC_Release(top);
        GC_Release(next);
        break;
    }
    case SysCalls::eLongAddB:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        Type ntype = (Type)0;
        UInt next = HopperVM_Pop_R(ntype);
        UInt result = 0;
        Type rtype = Type::eLong;
        result = HRLong_LongAddB(next, top);
        HopperVM_Push(result, rtype);
        GC_Release(next);
        break;
    }
    case SysCalls::eLongSubB:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        Type ntype = (Type)0;
        UInt next = HopperVM_Pop_R(ntype);
        UInt result = 0;
        Type rtype = Type::eLong;
        result = HRLong_LongSubB(next, top);
        HopperVM_Push(result, rtype);
        GC_Release(next);
        break;
    }
    case SysCalls::eFloatAdd:
    case SysCalls::eFloatSub:
    case SysCalls::eFloatDiv:
    case SysCalls::eFloatMul:
    case SysCalls::eFloatATan2:
    case SysCalls::eFloatEQ:
    case SysCalls::eFloatLT:
    case SysCalls::eFloatLE:
    case SysCalls::eFloatGT:
    case SysCalls::eFloatGE:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        Type ntype = (Type)0;
        UInt next = HopperVM_Pop_R(ntype);
        UInt result = 0;
        Type rtype = Type::eFloat;
        switch (SysCalls(iSysCall))
        {
        case SysCalls::eFloatAdd:
        {
            result = External_FloatAdd(next, top);
            break;
        }
        case SysCalls::eFloatSub:
        {
            result = External_FloatSub(next, top);
            break;
        }
        case SysCalls::eFloatDiv:
        {
            result = External_FloatDiv(next, top);
            break;
        }
        case SysCalls::eFloatMul:
        {
            result = External_FloatMul(next, top);
            break;
        }
        case SysCalls::eFloatATan2:
        {
            result = External_FloatATan2(next, top);
            break;
        }
        case SysCalls::eFloatEQ:
        {
            result = External_FloatEQ(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eFloatLT:
        {
            result = External_FloatLT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eFloatLE:
        {
            result = External_FloatLE(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eFloatGT:
        {
            result = External_FloatGT(next, top);
            rtype = Type::eBool;
            break;
        }
        case SysCalls::eFloatGE:
        {
            result = External_FloatGE(next, top);
            rtype = Type::eBool;
            break;
        }
        } // switch
        HopperVM_Push(result, rtype);
        GC_Release(top);
        GC_Release(next);
        break;
    }
    case SysCalls::eFloatSin:
    case SysCalls::eFloatCos:
    case SysCalls::eFloatSqrt:
    {
        Type ttype = (Type)0;
        UInt top = HopperVM_Pop_R(ttype);
        UInt result = 0;
        Type rtype = Type::eFloat;
        switch (SysCalls(iSysCall))
        {
        case SysCalls::eFloatSin:
        {
            result = External_FloatSin(top);
            break;
        }
        case SysCalls::eFloatCos:
        {
            result = External_FloatCos(top);
            break;
        }
        case SysCalls::eFloatSqrt:
        {
            result = External_FloatSqrt(top);
            break;
        }
        } // switch
        HopperVM_Push(result, rtype);
        GC_Release(top);
        break;
    }
    case SysCalls::eLongNew:
    {
        UInt address = HRLong_New();
        HopperVM_Push(address, Type::eLong);
        break;
    }
    case SysCalls::eFloatNew:
    {
        UInt address = HRFloat_New();
        HopperVM_Push(address, Type::eFloat);
        break;
    }
    case SysCalls::eTimeMillis:
    {
        UInt address = External_GetMillis();
        HopperVM_Push(address, Type::eLong);
        break;
    }
    case SysCalls::eTimeDelay:
    {
        External_Delay(HopperVM_Pop());
        doNext = false;
        break;
    }
    case SysCalls::eScreenPrint:
    {
        Runtime_ErrorDump(0xCC);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenPrintLn:
    {
        Runtime_ErrorDump(0xCE);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenClear:
    {
        Runtime_ErrorDump(0xD0);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenSetCursor:
    {
        Runtime_ErrorDump(0xD2);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenColumnsGet:
    {
        Runtime_ErrorDump(0xD4);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenRowsGet:
    {
        Runtime_ErrorDump(0xD6);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenCursorXGet:
    {
        Runtime_ErrorDump(0xD8);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenCursorYGet:
    {
        Runtime_ErrorDump(0xDA);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenSuspend:
    {
        Runtime_ErrorDump(0xDC);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eScreenResume:
    {
        Runtime_ErrorDump(0xDE);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardReadKey:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardIsAvailableGet:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardClickXGet:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardClickYGet:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardClickUpGet:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardClickDoubleGet:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    case SysCalls::eKeyboardScrollDeltaGet:
    {
        Runtime_ErrorDump(0xE3);
        Minimal_Error_Set(0x0B);
        break;
    }
    default:
    {
        IO_WriteHex(HopperVM_PC_Get());
        IO_Write(':');
        IO_Write('S');
        IO_WriteHex(iSysCall);
        IO_Write('-');
        IO_WriteHex(iOverload);
        IO_Write(' ');
        Runtime_ErrorDump(0x02);
        Minimal_Error_Set(0x0A);
        break;
    }
    } // switch
    return doNext && (Minimal_Error_Get() == 0x00);
}

UInt HopperVM_ReadWordOperand()
{
    UInt operand = Memory_ReadProgramWord(HopperVM_pc);
    
    HopperVM_pc++;
    
    HopperVM_pc++;
    return operand;
}

Int HopperVM_PopI_R(Type & htype)
{
    
    HopperVM_sp--;
    UInt value = Memory_ReadByte(HopperVM_valueStackLSBPage + HopperVM_sp) + (Memory_ReadByte(HopperVM_valueStackMSBPage + HopperVM_sp) << 0x08);
    htype = Type(Memory_ReadByte(HopperVM_typeStackPage + HopperVM_sp));
    return External_UIntToInt(value);
}

void HopperVM_BP_Set(Byte value)
{
    HopperVM_bp = value;
}

UInt HopperVM_PopCS()
{
    
    HopperVM_csp--;
    return Memory_ReadByte(HopperVM_callStackLSBPage + HopperVM_csp) + (Memory_ReadByte(HopperVM_callStackMSBPage + HopperVM_csp) << 0x08);
}

Byte HopperVM_CSPStart_Get()
{
    return HopperVM_cspStart;
}

Int HopperVM_ReadWordOffsetOperand()
{
    Int offset = External_UIntToInt(Memory_ReadProgramWord(HopperVM_pc));
    
    HopperVM_pc++;
    
    HopperVM_pc++;
    return offset;
}

Bool HopperVM_ExitInline()
{
    HopperVM_pc = HopperVM_pcStore;
    HopperVM_Push(0x00, Type::eUInt);
    return true;
}

Bool HopperVM_RunInline()
{
    Type stype = (Type)0;
    UInt startIndex = HopperVM_Pop_R(stype);
    Type ttype = (Type)0;
    UInt inlineCodeArray = HopperVM_Pop_R(ttype);
    HopperVM_pcStore = HopperVM_pc;
    UInt inlineLocation = HopperVM_binaryAddress + HopperVM_programSize;
    HopperVM_pc = inlineLocation + startIndex - HopperVM_programOffset;
    UInt length = HRArray_GetCount(inlineCodeArray);;
    for (UInt i = 0x00; i < length; i++)
    {
        Type itype = (Type)0;
        Byte c = Byte(HRArray_GetItem_R(inlineCodeArray, i, itype));
        Memory_WriteCodeByte(inlineLocation, c);
        
        inlineLocation++;
    }
    GC_Release(inlineCodeArray);
    return true;
}

UInt HopperVM_RuntimeExecute(UInt hrpath, UInt hrargs)
{
    UInt result = 0;
    UInt previousArguments = HopperVM_currentArguments;
    HopperVM_currentArguments = hrargs;
    UInt pcBefore = HopperVM_pc;
    Byte spBefore = HopperVM_sp;
    Byte bpBefore = HopperVM_bp;
    Byte gpBefore = HopperVM_gp;
    Byte cspBefore = HopperVM_csp;
    Byte cspStartBefore = HopperVM_cspStart;
    UInt binaryAddressBefore = HopperVM_binaryAddress;
    UInt programSizeBefore = HopperVM_programSize;
    UInt constAddressBefore = HopperVM_constAddress;
    UInt methodTableBefore = HopperVM_methodTable;
    UInt programOffsetBefore = HopperVM_programOffset;
    UInt loadedAddress = 0;
    UInt codeLength = 0;
    UInt startAddress = HopperVM_binaryAddress + HopperVM_programSize;
    if (Runtime_LoadHexe_R(hrpath, startAddress, loadedAddress, codeLength, false))
    {
        HopperVM_binaryAddress = loadedAddress;
        HopperVM_programSize = codeLength;
        HopperVM_constAddress = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x02) + startAddress;
        HopperVM_programOffset = Memory_ReadCodeWord(HopperVM_binaryAddress + 0x04) + startAddress;
        External_SetProgramOffset(HopperVM_programOffset);
        HopperVM_methodTable = HopperVM_binaryAddress + 0x06;
        Minimal_Error_Set(0x00);
        HopperVM_cnp = false;
        HopperVM_gp = HopperVM_sp;
        HopperVM_cspStart = HopperVM_csp;
        HopperVM_pc = 0x00;
        Bool restart = HopperVM_InlinedExecuteWarp(false);
    }
    else if (Minimal_Error_Get() == 0x00)
    {
        Minimal_Error_Set(0x0E);
    }
    result = Minimal_Error_Get();
    HopperVM_binaryAddress = binaryAddressBefore;
    HopperVM_programSize = programSizeBefore;
    HopperVM_constAddress = constAddressBefore;
    HopperVM_methodTable = methodTableBefore;
    HopperVM_programOffset = programOffsetBefore;
    External_SetProgramOffset(HopperVM_programOffset);
    HopperVM_pc = pcBefore;
    HopperVM_gp = gpBefore;
    HopperVM_cspStart = cspStartBefore;
    if (result != 0x00)
    {
        HopperVM_csp = cspBefore;
        HopperVM_sp = spBefore;
        HopperVM_bp = bpBefore;
        Minimal_Error_Set(0x00);
    }
    HopperVM_currentArguments = previousArguments;
    return result;
}

UInt HopperVM_RuntimeDateTime()
{
    Runtime_SerialWriteChar(Char(0x07));
    Runtime_SerialWriteChar('D');
    while (!Serial_IsAvailable_Get())
    {
        External_Delay(0x0A);
    }
    UInt hrstring = HRString_New();
    for (;;)
    {
        Char ch = Runtime_SerialReadChar();
        if (ch == Char(0x0D))
        {
            break;;
        }
        HRString_BuildChar_R(hrstring, ch);
    }
    return hrstring;
}

Bool Types_IsReferenceType(Type htype)
{
    return (Byte(htype) >= 0x0D);
}

Bool Library_ExecuteLibCall(Byte iLibCall, UInt iOverload)
{
    Bool doNext = true;
    switch (LibCall(iLibCall))
    {
    case LibCall::eWireBegin:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Bool result = HRWire_Begin(0x00);
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        case 0x01:
        {
            Type ctype = (Type)0;
            UInt controller = HopperVM_Pop_R(ctype);
            Bool result = HRWire_Begin(Byte(controller));
            HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
            break;
        }
        } // switch
        break;
    }
    case LibCall::eWireBeginTx:
    {
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        switch (iOverload)
        {
        case 0x00:
        {
            HRWire_BeginTx(0x00, Byte(address));
            break;
        }
        case 0x01:
        {
            Type ctype = (Type)0;
            UInt controller = HopperVM_Pop_R(ctype);
            HRWire_BeginTx(Byte(controller), Byte(address));
            break;
        }
        } // switch
        break;
    }
    case LibCall::eWireWrite:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Type atype = (Type)0;
            UInt b = HopperVM_Pop_R(atype);
            HRWire_Write(0x00, Byte(b));
            break;
        }
        case 0x01:
        {
            Type atype = (Type)0;
            UInt b = HopperVM_Pop_R(atype);
            Type ctype = (Type)0;
            UInt controller = HopperVM_Pop_R(ctype);
            HRWire_Write(Byte(controller), Byte(b));
            break;
        }
        case 0x02:
        {
            Type ltype = (Type)0;
            UInt length = HopperVM_Pop_R(ltype);
            Type stype = (Type)0;
            UInt startIndex = HopperVM_Pop_R(stype);
            Type atype = (Type)0;
            UInt hrarray = HopperVM_Pop_R(atype);
            Type ctype = (Type)0;
            UInt controller = HopperVM_Pop_R(ctype);
            HRWire_Write(Byte(controller), hrarray, startIndex, length);
            GC_Release(hrarray);
            break;
        }
        } // switch
        break;
    }
    case LibCall::eWireConfigure:
    {
        UInt freqkHz = 0x0190;
        if (iOverload == 0x01)
        {
            Type freqtype = (Type)0;
            freqkHz = HopperVM_Pop_R(freqtype);
        }
        Type cltype = (Type)0;
        UInt sclPin = HopperVM_Pop_R(cltype);
        Type datype = (Type)0;
        UInt sdaPin = HopperVM_Pop_R(datype);
        Type ctype = (Type)0;
        UInt controller = HopperVM_Pop_R(ctype);
        HRWire_Configure(Byte(controller), Byte(sdaPin), Byte(sclPin), freqkHz);
        break;
    }
    case LibCall::eWireRequestFrom:
    {
        Type btype = (Type)0;
        UInt bytes = HopperVM_Pop_R(btype);
        Type atype = (Type)0;
        UInt address = HopperVM_Pop_R(atype);
        Type ctype = (Type)0;
        UInt controller = HopperVM_Pop_R(ctype);
        bytes = HRWire_RequestFrom(Byte(controller), Byte(address), Byte(bytes));
        HopperVM_Push(bytes, Type::eUInt);
        break;
    }
    case LibCall::eWireRead:
    {
        Type ctype = (Type)0;
        UInt controller = HopperVM_Pop_R(ctype);
        Byte data = HRWire_Read(Byte(controller));
        HopperVM_Push(data, Type::eByte);
        break;
    }
    case LibCall::eWireEndTx:
    {
        switch (iOverload)
        {
        case 0x00:
        {
            Byte result = HRWire_EndTx(0x00);
            HopperVM_Push(result, Type::eByte);
            break;
        }
        case 0x01:
        {
            Type ctype = (Type)0;
            UInt controller = HopperVM_Pop_R(ctype);
            Byte result = HRWire_EndTx(Byte(controller));
            HopperVM_Push(result, Type::eByte);
            break;
        }
        } // switch
        break;
    }
    case LibCall::eMCUPinMode:
    {
        Byte mode = Byte(HopperVM_Pop());
        Byte pin = Byte(HopperVM_Pop());
        External_PinMode(pin, mode);
        break;
    }
    case LibCall::eMCUDigitalWrite:
    {
        Byte value = Byte(HopperVM_Pop());
        Byte pin = Byte(HopperVM_Pop());
        External_DigitalWrite(pin, value);
        break;
    }
    case LibCall::eMCUDigitalRead:
    {
        Byte pin = Byte(HopperVM_Pop());
        Byte value = External_DigitalRead(pin);
        HopperVM_Push(value, Type::eByte);
        break;
    }
    case LibCall::eMCUAnalogRead:
    {
        Byte pin = Byte(HopperVM_Pop());
        UInt value = External_AnalogRead(pin);
        HopperVM_Push(value, Type::eUInt);
        break;
    }
    case LibCall::eMCUAnalogWrite:
    {
        UInt value = HopperVM_Pop();
        Byte pin = Byte(HopperVM_Pop());
        External_AnalogWrite(pin, value);
        break;
    }
    case LibCall::eMCUAnalogWriteResolution:
    {
        Byte value = Byte(HopperVM_Pop());
        External_AnalogWriteResolution(value);
        break;
    }
    case LibCall::eMCUAttachToPin:
    {
        Byte state = Byte(HopperVM_Pop());
        PinISRDelegate isrDelegate = PinISRDelegate(HopperVM_Pop());
        Byte pin = Byte(HopperVM_Pop());
        Bool result = External_AttachToPin(pin, isrDelegate, HopperPinStatus(state));
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        Library_isrExists = true;
        break;
    }
    case LibCall::eTimerStart:
    {
        TimerISRDelegate isrDelegate = TimerISRDelegate(HopperVM_Pop());
        Type itype = (Type)0;
        UInt interval = HopperVM_Pop_R(itype);
        if (iOverload == 0x00)
        {
        }
        else
        {
        }
        UInt timerID = 0;
        if (itype == Type::eLong)
        {
            timerID = External_TimerStartLong(interval, isrDelegate);
            GC_Release(interval);
        }
        else
        {
            timerID = External_TimerStart(interval, isrDelegate);
        }
        HopperVM_Push(timerID, Type::eUInt);
        Library_isrExists = true;
        break;
    }
    case LibCall::eTimerAlarm:
    {
        TimerISRDelegate isrDelegate = TimerISRDelegate(HopperVM_Pop());
        Type itype = (Type)0;
        UInt interval = HopperVM_Pop_R(itype);
        if (iOverload == 0x00)
        {
        }
        else
        {
        }
        UInt alarmID = 0;
        if (itype == Type::eLong)
        {
            alarmID = External_TimerAlarmLong(interval, isrDelegate);
            GC_Release(interval);
        }
        else
        {
            alarmID = External_TimerAlarm(interval, isrDelegate);
        }
        HopperVM_Push(alarmID, Type::eUInt);
        Library_isrExists = true;
        break;
    }
    case LibCall::eTimerStop:
    {
        Type itype = (Type)0;
        UInt timerID = HopperVM_Pop_R(itype);
        External_TimerStop(timerID);
        break;
    }
    case LibCall::eTimerCancel:
    {
        Type itype = (Type)0;
        UInt alarmID = HopperVM_Pop_R(itype);
        External_TimerCancel(alarmID);
        break;
    }
    case LibCall::eMCUInterruptsEnabledGet:
    {
        Bool value = External_MCUInterruptsEnabledGet();
        HopperVM_Push((value) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case LibCall::eMCUInterruptsEnabledSet:
    {
        Type ptype = (Type)0;
        UInt value = HopperVM_Pop_R(ptype);
        External_MCUInterruptsEnabledSet(value != 0x00);
        break;
    }
    case LibCall::eMCUClockSpeedGet:
    {
        UInt value = External_MCUClockSpeedGet();
        HopperVM_Push(value, Type::eUInt);
        break;
    }
    case LibCall::eMCUClockSpeedSet:
    {
        Type ptype = (Type)0;
        UInt value = HopperVM_Pop_R(ptype);
        External_MCUClockSpeedSet(value);
        break;
    }
    case LibCall::eMCUReboot:
    {
        Type ptype = (Type)0;
        UInt value = HopperVM_Pop_R(ptype);
        External_MCUReboot(value != 0x00);
        doNext = false;
        break;
    }
    case LibCall::eMCUHeapFree:
    {
        UInt hrlong = External_MCUHeapFree();
        HopperVM_Push(hrlong, Type::eLong);
        break;
    }
    case LibCall::eMCUStackFree:
    {
        UInt hrlong = External_MCUStackFree();
        HopperVM_Push(hrlong, Type::eLong);
        break;
    }
    case LibCall::eSDSPIControllerGet:
    {
        Byte iController = External_SDSPIControllerGet();
        HopperVM_Push(iController, Type::eByte);
        break;
    }
    case LibCall::eSDSPIControllerSet:
    {
        Type ctype = (Type)0;
        UInt spiController = HopperVM_Pop_R(ctype);
        External_SDSPIControllerSet(Byte(spiController));
        break;
    }
    case LibCall::eSDCSPinGet:
    {
        Byte pin = External_SDCSPinGet();
        HopperVM_Push(pin, Type::eByte);
        break;
    }
    case LibCall::eSDCSPinSet:
    {
        Type ctype = (Type)0;
        UInt pin = HopperVM_Pop_R(ctype);
        External_SDCSPinSet(Byte(pin));
        break;
    }
    case LibCall::eSDClkPinGet:
    {
        Byte pin = External_SDClkPinGet();
        HopperVM_Push(pin, Type::eByte);
        break;
    }
    case LibCall::eSDClkPinSet:
    {
        Type ctype = (Type)0;
        UInt pin = HopperVM_Pop_R(ctype);
        External_SDClkPinSet(Byte(pin));
        break;
    }
    case LibCall::eSDTxPinGet:
    {
        Byte pin = External_SDTxPinGet();
        HopperVM_Push(pin, Type::eByte);
        break;
    }
    case LibCall::eSDTxPinSet:
    {
        Type ctype = (Type)0;
        UInt pin = HopperVM_Pop_R(ctype);
        External_SDTxPinSet(Byte(pin));
        break;
    }
    case LibCall::eSDRxPinGet:
    {
        Byte pin = External_SDRxPinGet();
        HopperVM_Push(pin, Type::eByte);
        break;
    }
    case LibCall::eSDRxPinSet:
    {
        Type ctype = (Type)0;
        UInt pin = HopperVM_Pop_R(ctype);
        External_SDRxPinSet(Byte(pin));
        break;
    }
    case LibCall::eSDMount:
    {
        Bool ok = External_SDMount();
        HopperVM_Push((ok) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case LibCall::eSDEject:
    {
        External_SDEject();
        break;
    }
    case LibCall::eSPIBegin:
    {
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        Bool result = HRSPI_Begin(Byte(spiController));
        HopperVM_Push((result) ? (0x01) : (0x00), Type::eBool);
        break;
    }
    case LibCall::eSPIBeginTransaction:
    {
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_BeginTransaction(Byte(spiController));
        break;
    }
    case LibCall::eSPIEndTransaction:
    {
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_EndTransaction(Byte(spiController));
        break;
    }
    case LibCall::eSPISetCSPin:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        Type ctype = (Type)0;
        UInt spiController = HopperVM_Pop_R(ctype);
        HRSPI_SetCSPin(Byte(spiController), Byte(pin));
        break;
    }
    case LibCall::eSPIGetCSPin:
    {
        Type ctype = (Type)0;
        UInt spiController = HopperVM_Pop_R(ctype);
        Byte pin = HRSPI_GetCSPin(Byte(spiController));
        HopperVM_Push(pin, Type::eByte);
        break;
    }
    case LibCall::eSPISetClkPin:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        Type ctype = (Type)0;
        UInt spiController = HopperVM_Pop_R(ctype);
        HRSPI_SetClkPin(Byte(spiController), Byte(pin));
        break;
    }
    case LibCall::eSPISetTxPin:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        Type ctype = (Type)0;
        UInt spiController = HopperVM_Pop_R(ctype);
        HRSPI_SetTxPin(Byte(spiController), Byte(pin));
        break;
    }
    case LibCall::eSPISetRxPin:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        Type ctype = (Type)0;
        UInt spiController = HopperVM_Pop_R(ctype);
        HRSPI_SetRxPin(Byte(spiController), Byte(pin));
        break;
    }
    case LibCall::eSPICSPinGet:
    {
        Byte value = HRSPI_GetCSPin(0x00);
        HopperVM_Push(value, Type::eByte);
        break;
    }
    case LibCall::eSPICSPinSet:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        HRSPI_SetCSPin(0x00, Byte(pin));
        break;
    }
    case LibCall::eSPIClkPinSet:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        HRSPI_SetClkPin(0x00, Byte(pin));
        break;
    }
    case LibCall::eSPITxPinSet:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        HRSPI_SetTxPin(0x00, Byte(pin));
        break;
    }
    case LibCall::eSPIRxPinSet:
    {
        Type ptype = (Type)0;
        UInt pin = HopperVM_Pop_R(ptype);
        HRSPI_SetRxPin(0x00, Byte(pin));
        break;
    }
    case LibCall::eSPIReadByte:
    {
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        Byte data = HRSPI_ReadByte(Byte(spiController));
        HopperVM_Push(data, Type::eByte);
        break;
    }
    case LibCall::eSPIWriteByte:
    {
        Type dtype = (Type)0;
        UInt data = HopperVM_Pop_R(dtype);
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_WriteByte(Byte(spiController), Byte(data));
        break;
    }
    case LibCall::eSPIWriteBytes:
    {
        Type ltype = (Type)0;
        UInt count = HopperVM_Pop_R(ltype);
        Type dtype = (Type)0;
        UInt data = HopperVM_Pop_R(dtype);
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_WriteBytes(Byte(spiController), Byte(data), count);
        break;
    }
    case LibCall::eSPIReadWord:
    {
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        UInt data = HRSPI_ReadWord(Byte(spiController));
        HopperVM_Push(data, Type::eUInt);
        break;
    }
    case LibCall::eSPIWriteWord:
    {
        Type dtype = (Type)0;
        UInt data = HopperVM_Pop_R(dtype);
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_WriteWord(Byte(spiController), data);
        break;
    }
    case LibCall::eSPIWriteWords:
    {
        Type ltype = (Type)0;
        UInt count = HopperVM_Pop_R(ltype);
        Type dtype = (Type)0;
        UInt data = HopperVM_Pop_R(dtype);
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_WriteWords(Byte(spiController), data, count);
        break;
    }
    case LibCall::eSPIReadBuffer:
    {
        Type ltype = (Type)0;
        UInt length = HopperVM_Pop_R(ltype);
        Type stype = (Type)0;
        UInt startIndex = HopperVM_Pop_R(stype);
        Type dtype = (Type)0;
        UInt hrdata = HopperVM_Pop_R(dtype);
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_ReadBuffer(Byte(spiController), hrdata, startIndex, length);
        GC_Release(hrdata);
        break;
    }
    case LibCall::eSPIWriteBuffer:
    {
        Type ltype = (Type)0;
        UInt length = HopperVM_Pop_R(ltype);
        Type stype = (Type)0;
        UInt startIndex = HopperVM_Pop_R(stype);
        Type dtype = (Type)0;
        UInt hrdata = HopperVM_Pop_R(dtype);
        UInt spiController = 0x00;
        if ((iOverload == 0x02) || (iOverload == 0x03))
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_WriteBuffer(Byte(spiController), hrdata, startIndex, length);
        GC_Release(hrdata);
        break;
    }
    case LibCall::eSPISettings:
    {
        Type mtype = (Type)0;
        DataMode dataMode = DataMode(HopperVM_Pop_R(mtype));
        Type otype = (Type)0;
        DataOrder dataOrder = DataOrder(HopperVM_Pop_R(otype));
        Type stype = (Type)0;
        UInt hrspeed = HopperVM_Pop_R(stype);
        UInt spiController = 0x00;
        if (iOverload == 0x01)
        {
            Type ctype = (Type)0;
            spiController = HopperVM_Pop_R(ctype);
        }
        HRSPI_Settings(Byte(spiController), hrspeed, dataOrder, dataMode);
        GC_Release(hrspeed);
        break;
    }
    case LibCall::eNeoPixelBegin:
    {
        Type ptype = (Type)0;
        UInt length = 0x01;
        UInt pixelType = ((0x01 << 0x06) | (0x01 << 0x04) | (0x00 << 0x02) | (0x02));
        if (iOverload == 0x02)
        {
            pixelType = HopperVM_Pop_R(ptype);
        }
        UInt pin = HopperVM_Pop_R(ptype);
        if (iOverload != 0x00)
        {
            length = HopperVM_Pop_R(ptype);
        }
        HRNeoPixel_Begin(length, Byte(pin), pixelType);
        break;
    }
    case LibCall::eNeoPixelBrightnessSet:
    {
        Type ptype = (Type)0;
        UInt brightness = HopperVM_Pop_R(ptype);
        HRNeoPixel_SetBrightness(Byte(brightness));
        break;
    }
    case LibCall::eNeoPixelBrightnessGet:
    {
        Byte brightness = HRNeoPixel_GetBrightness();
        HopperVM_Push(brightness, Type::eByte);
        break;
    }
    case LibCall::eNeoPixelSetColor:
    {
        UInt w = 0x00;
        Type ptype = (Type)0;
        if (iOverload == 0x01)
        {
            w = HopperVM_Pop_R(ptype);
        }
        UInt b = HopperVM_Pop_R(ptype);
        UInt g = HopperVM_Pop_R(ptype);
        UInt r = HopperVM_Pop_R(ptype);
        UInt pixel = HopperVM_Pop_R(ptype);
        HRNeoPixel_SetColor(pixel, Byte(r), Byte(g), Byte(b), Byte(w));
        break;
    }
    case LibCall::eNeoPixelShow:
    {
        HRNeoPixel_Show();
        break;
    }
    case LibCall::eNeoPixelLengthGet:
    {
        UInt length = HRNeoPixel_GetLength();
        HopperVM_Push(length, Type::eUInt);
        break;
    }
    case LibCall::eWebClientGetRequest:
    {
        Type htype = (Type)0;
        Byte address = Byte(HopperVM_Pop_R(htype));
        UInt content = HopperVM_Get_R(address, htype);
        Type utype = (Type)0;
        UInt url = HopperVM_Pop_R(utype);
        Bool success = External_WebClientGetRequest_R(url, content);
        HopperVM_Put(address, content, Type::eString);
        GC_Release(url);
        HopperVM_Push((success) ? (0x01) : (0x00), Type::eBool);
        doNext = false;
        break;
    }
    case LibCall::eWebServerBegin:
    {
        UInt port = 0x50;
        if (iOverload == 0x02)
        {
            Type ptype = (Type)0;
            port = HopperVM_Pop_R(ptype);
        }
        External_WebServerBegin(port);
        doNext = false;
        break;
    }
    case LibCall::eWebServerOn:
    {
        HandlerDelegate handlerDelegate = HandlerDelegate(HopperVM_Pop());
        Type utype = (Type)0;
        UInt url = HopperVM_Pop_R(utype);
        External_WebServerOn(url, handlerDelegate);
        GC_Release(url);
        break;
    }
    case LibCall::eWebServerOnNotFound:
    {
        HandlerDelegate handlerDelegate = HandlerDelegate(HopperVM_Pop());
        External_WebServerOnNotFound(handlerDelegate);
        break;
    }
    case LibCall::eWebServerEvents:
    {
        External_WebServerEvents();
        doNext = false;
        break;
    }
    case LibCall::eWebServerClose:
    {
        External_WebServerClose();
        doNext = false;
        break;
    }
    case LibCall::eWebServerSend:
    {
        Type ctype = (Type)0;
        UInt content = HopperVM_Pop_R(ctype);
        Type ttype = (Type)0;
        UInt headerContent = HopperVM_Pop_R(ttype);
        Type htype = (Type)0;
        UInt httpCode = HopperVM_Pop_R(htype);
        External_WebServerSend(httpCode, headerContent, content);
        GC_Release(headerContent);
        GC_Release(content);
        doNext = false;
        break;
    }
    default:
    {
        IO_WriteHex(HopperVM_PC_Get());
        IO_Write(':');
        IO_Write('L');
        IO_WriteHex(iLibCall);
        IO_Write('-');
        IO_WriteHex(iOverload);
        IO_Write(' ');
        Runtime_ErrorDump(0x84);
        Minimal_Error_Set(0x0A);
        break;
    }
    } // switch
    return doNext && (Minimal_Error_Get() == 0x00);
}

UInt HRDirectory_Clone(UInt original)
{
    UInt address = GC_New(0x03, Type::eDirectory);
    Memory_WriteByte(address + 2, Memory_ReadByte(original + 2));
    Memory_WriteWord(address + 3, HRString_Clone(Memory_ReadWord(original + 3)));
    return address;
}

UInt HRDirectory_GetTime(UInt path)
{
    return External_DirectoryGetTime(path);
}

UInt HRDirectory_GetDate(UInt path)
{
    return External_DirectoryGetDate(path);
}

UInt HRDirectory_New()
{
    UInt address = GC_New(0x03, Type::eDirectory);
    Memory_WriteByte(address + 2, 0x00);
    Memory_WriteWord(address + 3, HRString_New());
    return address;
}

UInt HRDirectory_Open(UInt hrpath)
{
    UInt address = HRDirectory_New();
    Memory_WriteWord(address + 3, HRString_Clone(hrpath));
    if (HRDirectory_Exists(hrpath))
    {
        Memory_WriteByte(address + 2, 0x01);
    }
    return address;
}

Bool HRDirectory_IsValid(UInt _this)
{
    return (Memory_ReadByte(_this + 2) != 0x00);
}

UInt HRDirectory_GetFileCount(UInt hrdir)
{
    UInt skipped = 0;
    return (HRDirectory_IsValid(hrdir)) ? (External_DirectoryGetFileCount_R(Memory_ReadWord(hrdir + 3), skipped)) : (0x00);
}

UInt HRDirectory_GetFileCount_R(UInt hrdir, UInt & skipped)
{
    return (HRDirectory_IsValid(hrdir)) ? (External_DirectoryGetFileCount_R(Memory_ReadWord(hrdir + 3), skipped)) : (0x00);
}

UInt HRDirectory_GetDirectoryCount(UInt hrdir)
{
    UInt skipped = 0;
    return (HRDirectory_IsValid(hrdir)) ? (External_DirectoryGetDirectoryCount_R(Memory_ReadWord(hrdir + 3), skipped)) : (0x00);
}

UInt HRDirectory_GetDirectoryCount_R(UInt hrdir, UInt & skipped)
{
    return (HRDirectory_IsValid(hrdir)) ? (External_DirectoryGetDirectoryCount_R(Memory_ReadWord(hrdir + 3), skipped)) : (0x00);
}

UInt HRDirectory_GetFile(UInt hrdir, UInt index)
{
    if (!HRDirectory_IsValid(hrdir))
    {
        return HRString_New();
    }
    return External_DirectoryGetFile(Memory_ReadWord(hrdir + 3), index);
}

UInt HRDirectory_GetDirectory(UInt hrdir, UInt index)
{
    if (!HRDirectory_IsValid(hrdir))
    {
        return HRString_New();
    }
    return External_DirectoryGetDirectory(Memory_ReadWord(hrdir + 3), index);
}

void HRDirectory_Delete(UInt hrpath)
{
    External_DirectoryDelete(hrpath);
}

UInt HRFile_Clone(UInt original)
{
    UInt address = GC_New(0x10, Type::eFile);
    Memory_WriteByte(address + 2, Memory_ReadByte(original + 2));
    Memory_WriteByte(address + 3, Memory_ReadByte(original + 3));
    Memory_WriteByte(address + 4, Memory_ReadByte(original + 4));
    Memory_WriteByte(address + 5, Memory_ReadByte(original + 5));
    Memory_WriteWord(address + 6, HRString_Clone(Memory_ReadWord(original + 6)));
    Memory_WriteWord(address + 8, Memory_ReadWord(original + 8));
    Memory_WriteWord(address + 8 + 0x02, Memory_ReadWord(original + 8 + 0x02));
    if (HRFile_IsCode(original))
    {
        Memory_WriteWord(address + 12, Memory_ReadWord(original + 12));
    }
    else
    {
        Memory_WriteWord(address + 12, HRString_Clone(Memory_ReadWord(original + 12)));
    }
    Memory_WriteWord(address + 14, Memory_ReadWord(original + 14));
    Memory_WriteWord(address + 14 + 0x02, Memory_ReadWord(original + 14 + 0x02));
    return address;
}

Bool HRFile_IsValid(UInt _this)
{
    return (Memory_ReadByte(_this + 2) != 0x00);
}

UInt HRFile_ReadLine(UInt _this)
{
    return External_ReadLine(_this);
}

Byte HRFile_Read(UInt _this, UInt hrseekpos)
{
    Byte b = 0;
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00))
        {
            UInt seekposLSW = HRLong_GetByte(hrseekpos, 0x00) + (HRLong_GetByte(hrseekpos, 0x01) << 0x08);
            UInt seekposMSW = HRLong_GetByte(hrseekpos, 0x02) + (HRLong_GetByte(hrseekpos, 0x03) << 0x08);
            UInt sizeLSW = Memory_ReadWord(_this + 14);
            UInt sizeMSW = Memory_ReadWord(_this + 14 + 0x02);
            if (HRFile_lt32(seekposLSW, seekposMSW, sizeLSW, sizeMSW))
            {
                if (External_TryFileReadByte_R(Memory_ReadWord(_this + 6), hrseekpos, b))
                {
                    break;;
                }
            }
        }
        Memory_WriteByte(_this + 2, 0x00);
        break;;
    }
    return b;
}

UInt HRFile_Read(UInt _this, UInt hrbuffer, UInt bufferSize)
{
    UInt bytesRead = 0;
    for (;;)
    {
        if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 3) != 0x00))
        {
            UInt posLSW = Memory_ReadWord(_this + 8);
            UInt posMSW = Memory_ReadWord(_this + 8 + 0x02);
            UInt sizeLSW = Memory_ReadWord(_this + 14);
            UInt sizeMSW = Memory_ReadWord(_this + 14 + 0x02);
            if (HRFile_lt32(posLSW, posMSW, sizeLSW, sizeMSW))
            {
                UInt hrpos = HRLong_FromBytes(Memory_ReadByte(_this + 8 + 0x00), Memory_ReadByte(_this + 8 + 0x01), Memory_ReadByte(_this + 8 + 0x02), Memory_ReadByte(_this + 8 + 0x03));
                bytesRead = External_TryFileReadBuffer(Memory_ReadWord(_this + 6), hrpos, hrbuffer, bufferSize);
                if (0x00 != bytesRead)
                {
                    posLSW = HRLong_GetByte(hrpos, 0x00) + (HRLong_GetByte(hrpos, 0x01) << 0x08);
                    posMSW = HRLong_GetByte(hrpos, 0x02) + (HRLong_GetByte(hrpos, 0x03) << 0x08);
                    GC_Release(hrpos);
                    Memory_WriteWord(_this + 8, posLSW);
                    Memory_WriteWord(_this + 8 + 0x02, posMSW);
                    break;;
                }
                GC_Release(hrpos);
            }
        }
        Memory_WriteByte(_this + 2, 0x00);
        break;;
    }
    return bytesRead;
}

void HRFile_Append(UInt _this, UInt hrstr)
{
    if ((Memory_ReadByte(_this + 2) != 0x00) && (Memory_ReadByte(_this + 4) != 0x00) && (Memory_ReadByte(_this + 5) == 0x00))
    {
        UInt buffer = Memory_ReadWord(_this + 12);
        HRString_BuildString_R(buffer, hrstr);
        UInt length = HRString_GetLength(buffer);
        if (length >= 0x0100)
        {
            External_FileWriteAllBytes(Memory_ReadWord(_this + 6), buffer, true);
            HRString_BuildClear_R(buffer);
        }
        Memory_WriteWord(_this + 12, buffer);
    }
    else
    {
        Memory_WriteByte(_this + 2, 0x00);
    }
}

UInt HRFile_GetTimeStamp(UInt path)
{
    return External_FileGetTimeStamp(path);
}

UInt HRFile_GetTime(UInt path)
{
    return External_FileGetTime(path);
}

UInt HRFile_GetDate(UInt path)
{
    return External_FileGetDate(path);
}

UInt HRFloat_Clone(UInt original)
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteWord(address + 0x02, Memory_ReadWord(original + 0x02));
    Memory_WriteWord(address + 0x04, Memory_ReadWord(original + 0x04));
    return address;
}

UInt HRFloat_NewFromConstant(UInt location)
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteWord(address + 0x02, Memory_ReadCodeWord(location));
    Memory_WriteWord(address + 0x04, Memory_ReadCodeWord(location + 0x02));
    return address;
}

UInt HRFloat_ToBytes(UInt ichunk)
{
    UInt lst = HRList_New(Type::eByte);;
    for (Byte i = 0x00; i < 0x04; i++)
    {
        Byte b = Memory_ReadByte(ichunk + 0x02 + i);
        HRList_Append(lst, b, Type::eByte);
    }
    return lst;
}

Byte HRFloat_GetByte(UInt ichunk, UInt i)
{
    return Memory_ReadByte(ichunk + 0x02 + i);
}

UInt HRFloat_FromBytes(Byte b0, Byte b1, Byte b2, Byte b3)
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteByte(address + 0x02, b0);
    Memory_WriteByte(address + 0x03, b1);
    Memory_WriteByte(address + 0x04, b2);
    Memory_WriteByte(address + 0x05, b3);
    return address;
}

UInt HRFloat_New()
{
    UInt address = GC_New(0x04, Type::eFloat);
    Memory_WriteWord(address + 0x02, 0x00);
    Memory_WriteWord(address + 0x04, 0x00);
    return address;
}

UInt HRList_Clone(UInt original)
{
    Type etype = Type(Memory_ReadByte(original + 4));
    UInt clone = HRList_New(etype);
    UInt pCurrentItem = Memory_ReadWord(original + 5);
    for (;;)
    {
        if (pCurrentItem == 0x00)
        {
            break;;
        }
        UInt itemData = Memory_ReadWord(pCurrentItem + 0);
        Type itype = etype;
        if (Types_IsReferenceType(etype))
        {
            itype = Type(Memory_ReadByte(itemData));
        }
        HRList_Append(clone, itemData, itype);
        pCurrentItem = Memory_ReadWord(pCurrentItem + 2);
    }
    return clone;
}

UInt HRList_New(Type htype)
{
    UInt address = GC_New(0x09, Type::eList);
    Memory_WriteWord(address + 2, 0x00);
    Memory_WriteByte(address + 4, Byte(htype));
    Memory_WriteWord(address + 5, 0x00);
    Memory_WriteWord(address + 7, 0x00);
    Memory_WriteWord(address + 7 + 0x02, 0x00);
    return address;
}

UInt HRList_GetCount(UInt _this)
{
    return Memory_ReadWord(_this + 2);
}

void HRList_Append(UInt _this, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt pNewItem = HRList_createItem(item, etype, itype);
    UInt pFirstItem = Memory_ReadWord(_this + 5);
    if (pFirstItem == 0x00)
    {
        Memory_WriteWord(_this + 5, pNewItem);
    }
    else
    {
        UInt pCurrentItem = pFirstItem;
        UInt pRecentItem = Memory_ReadWord(_this + 7);
        if (pRecentItem != 0x00)
        {
            pCurrentItem = pRecentItem;
        }
        for (;;)
        {
            UInt pNextItem = Memory_ReadWord(pCurrentItem + 2);
            if (pNextItem == 0x00)
            {
                break;;
            }
            pCurrentItem = pNextItem;
        }
        Memory_WriteWord(pCurrentItem + 2, pNewItem);
    }
    UInt count = Memory_ReadWord(_this + 2) + 0x01;
    Memory_WriteWord(_this + 2, count);
    Memory_WriteWord(_this + 7, pNewItem);
    Memory_WriteWord(_this + 7 + 0x02, count - 0x01);
}

void HRList_SetItem(UInt _this, UInt index, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt count = Memory_ReadWord(_this + 2);
    UInt pData = item;
    if (Types_IsReferenceType(etype))
    {
        if (Types_IsReferenceType(itype))
        {
            pData = GC_Clone(item);
        }
        else
        {
            pData = HRVariant_CreateValueVariant(item, itype);
        }
    }
    UInt i = 0x00;
    UInt pCurrent = Memory_ReadWord(_this + 5);
    UInt pRecent = Memory_ReadWord(_this + 7);
    if (pRecent != 0x00)
    {
        UInt iRecent = Memory_ReadWord(_this + 7 + 0x02);
        if (iRecent <= index)
        {
            i = iRecent;
            pCurrent = pRecent;
        }
    }
    for (;;)
    {
        if (i == index)
        {
            break;;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
        
        i++;
    }
    UInt oldData = Memory_ReadWord(pCurrent + 0);
    if (Types_IsReferenceType(etype))
    {
        GC_Release(oldData);
    }
    Memory_WriteWord(pCurrent + 0, pData);
    Memory_WriteWord(_this + 7, pCurrent);
    Memory_WriteWord(_this + 7 + 0x02, index);
}

void HRList_Insert(UInt _this, UInt index, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt count = Memory_ReadWord(_this + 2);
    UInt pFirst = Memory_ReadWord(_this + 5);
    if (index >= count)
    {
        HRList_Append(_this, item, itype);
    }
    else if (index == 0x00)
    {
        UInt pItem = HRList_createItem(item, etype, itype);
        Memory_WriteWord(pItem + 2, pFirst);
        Memory_WriteWord(_this + 5, pItem);
        Memory_WriteWord(_this + 2, count + 0x01);
        Memory_WriteWord(_this + 7, 0x00);
        Memory_WriteWord(_this + 7 + 0x02, 0x00);
    }
    else
    {
        UInt pCurrent = pFirst;
        UInt pPrevious = 0x00;
        UInt pRecent = Memory_ReadWord(_this + 7);
        UInt iRecent = Memory_ReadWord(_this + 7 + 0x02);
        count = 0x00;
        if ((iRecent != 0x00) && (index > iRecent))
        {
            pCurrent = pRecent;
            count = iRecent;
        }
        UInt pItem = HRList_createItem(item, etype, itype);
        while (0x00 != pCurrent)
        {
            if (index == count)
            {
                Memory_WriteWord(pItem + 2, pCurrent);
                Memory_WriteWord(pPrevious + 2, pItem);
                Memory_WriteWord(_this + 7, pItem);
                Memory_WriteWord(_this + 7 + 0x02, count);
                Memory_WriteWord(_this + 2, count + 0x01);
                break;;
            }
            pPrevious = pCurrent;
            pCurrent = Memory_ReadWord(pCurrent + 2);
            
            count++;
        }
    }
}

UInt HRList_GetItem_R(UInt _this, UInt index, Type & itype)
{
    itype = Type(Memory_ReadByte(_this + 4));
    UInt count = Memory_ReadWord(_this + 2);
    if (index >= count)
    {
        Minimal_Error_Set(0x01);
        return 0x00;
    }
    UInt i = 0x00;
    UInt pCurrent = Memory_ReadWord(_this + 5);
    UInt pRecent = Memory_ReadWord(_this + 7);
    if (pRecent != 0x00)
    {
        UInt iRecent = Memory_ReadWord(_this + 7 + 0x02);
        if (iRecent <= index)
        {
            i = iRecent;
            pCurrent = pRecent;
        }
    }
    for (;;)
    {
        if (i == index)
        {
            break;;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
        
        i++;
    }
    UInt pData = Memory_ReadWord(pCurrent + 0);
    if (itype == Type::eVariant)
    {
        itype = Type(Memory_ReadByte(pData));
    }
    if (itype == Type::eVariant)
    {
        pData = HRVariant_GetValue_R(pData, itype);
    }
    else if (Types_IsReferenceType(itype))
    {
        itype = Type(Memory_ReadByte(pData));
        pData = GC_Clone(pData);
    }
    Memory_WriteWord(_this + 7, pCurrent);
    Memory_WriteWord(_this + 7 + 0x02, index);
    return pData;
}

void HRList_Remove(UInt _this, UInt index)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt count = Memory_ReadWord(_this + 2);
    UInt pCurrent = Memory_ReadWord(_this + 5);
    if (index == 0x00)
    {
        UInt pNext = Memory_ReadWord(pCurrent + 2);
        HRList_clearItem(pCurrent, etype);
        Memory_WriteWord(_this + 5, pNext);
    }
    else
    {
        UInt pPrevious = pCurrent;
        pCurrent = Memory_ReadWord(pCurrent + 2);
        count = 0x01;
        while (count < index)
        {
            pPrevious = pCurrent;
            pCurrent = Memory_ReadWord(pCurrent + 2);
            
            count++;
        }
        Memory_WriteWord(pPrevious + 2, Memory_ReadWord(pCurrent + 2));
        HRList_clearItem(pCurrent, etype);
    }
    count = Memory_ReadWord(_this + 2) - 0x01;
    Memory_WriteWord(_this + 2, count);
    Memory_WriteWord(_this + 7, 0x00);
    Memory_WriteWord(_this + 7 + 0x02, 0x00);
}

Bool HRList_Contains(UInt _this, UInt item, Type itype)
{
    Type etype = Type(Memory_ReadByte(_this + 4));
    UInt pCurrent = Memory_ReadWord(_this + 5);
    for (;;)
    {
        if (0x00 == pCurrent)
        {
            break;;
        }
        Type dtype = etype;
        UInt pData = Memory_ReadWord(pCurrent + 0);
        if (Types_IsReferenceType(dtype))
        {
            dtype = Type(Memory_ReadByte(pData));
        }
        if (HRVariant_IsEqual(pData, dtype, item, itype))
        {
            return true;
        }
        pCurrent = Memory_ReadWord(pCurrent + 2);
    }
    return false;
}

Type HRList_GetValueType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 4));
}

UInt HRList_createItem(UInt itemData, Type etype, Type itype)
{
    UInt pData = itemData;
    if (Types_IsReferenceType(etype))
    {
        if (Types_IsReferenceType(itype))
        {
            pData = GC_Clone(itemData);
        }
        else
        {
            pData = HRVariant_CreateValueVariant(itemData, itype);
        }
    }
    UInt pitem = Memory_Allocate(0x04);
    Memory_WriteWord(pitem + 0, pData);
    Memory_WriteWord(pitem + 2, 0x00);
    return pitem;
}

UInt HRArray_Clone(UInt original)
{
    UInt count = Memory_ReadWord(original + 2);
    Type etype = Type(Memory_ReadWord(original + 4));
    UInt address = HRArray_New(etype, count);
    UInt elementbytes = 0;
    switch (etype)
    {
    case Type::eBool:
    {
        elementbytes = (count + 0x07) >> 0x03;
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        elementbytes = count;
        break;
    }
    default:
    {
        elementbytes = count * 0x02;
        break;
    }
    } // switch;
    for (UInt i = 0x00; i < elementbytes; i++)
    {
        Memory_WriteByte(address + 5 + i, Memory_ReadByte(original + 5 + i));
    }
    return address;
}

UInt HRArray_New(Type htype, UInt count)
{
    UInt elementbytes = 0;
    switch (htype)
    {
    case Type::eBool:
    {
        elementbytes = (count + 0x07) >> 0x03;
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        elementbytes = count;
        break;
    }
    default:
    {
        elementbytes = count * 0x02;
        break;
    }
    } // switch
    UInt _this = GC_New(0x03 + elementbytes, Type::eArray);
    Memory_WriteWord(_this + 2, count);
    Memory_WriteByte(_this + 4, Byte(htype));
    UInt address = _this + 5;;
    for (UInt i = 0x00; i < elementbytes; i++)
    {
        Memory_WriteByte(address, 0x00);
        
        address++;
    }
    return _this;
}

UInt HRArray_NewFromConstant(UInt location, Type htype, UInt length)
{
    UInt _this = HRArray_New(htype, length);;
    for (UInt i = 0x00; i < length; i++)
    {
        Memory_WriteByte(_this + 5 + i, Memory_ReadCodeByte(location + i));
    }
    return _this;
}

UInt HRArray_GetItem_R(UInt _this, UInt index, Type & etype)
{
    UInt elements = Memory_ReadWord(_this + 2);
    etype = Type(Memory_ReadByte(_this + 4));
    if (index >= elements)
    {
        Minimal_Error_Set(0x02);
        return 0x00;
    }
    UInt address = _this + 5;
    UInt value = 0;
    switch (etype)
    {
    case Type::eBool:
    {
        UInt offset = address + (index >> 0x03);
        Byte slotIndex = Byte(index & 0x07);
        Byte b = Memory_ReadByte(offset);
        Byte mask = Memory_ReadByte(HRArray_setSlots + slotIndex);
        value = b & mask;
        if (value != 0x00)
        {
            value = 0x01;
        }
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        value = Memory_ReadByte(address + index);
        break;
    }
    default:
    {
        UInt offset = address + (index << 0x01);
        Byte msb = Memory_ReadByte(offset);
        Byte lsb = Memory_ReadByte(offset + 0x01);
        value = (msb << 0x08) | lsb;
        break;
    }
    } // switch
    return value;
}

void HRArray_SetItem(UInt _this, UInt index, UInt value)
{
    UInt elements = Memory_ReadWord(_this + 2);
    Type etype = Type(Memory_ReadByte(_this + 4));
    if (index >= elements)
    {
        Minimal_Error_Set(0x02);
        return;
    }
    UInt address = _this + 5;
    switch (etype)
    {
    case Type::eBool:
    {
        UInt offset = address + (index >> 0x03);
        Byte slotIndex = Byte(index & 0x07);
        Byte b = Memory_ReadByte(offset);
        if (value == 0x00)
        {
            Byte mask = Memory_ReadByte(HRArray_clearSlots + slotIndex);
            b = b & mask;
        }
        else
        {
            Byte mask = Memory_ReadByte(HRArray_setSlots + slotIndex);
            b = b | mask;
        }
        Memory_WriteByte(offset, b);
        break;
    }
    case Type::eChar:
    case Type::eByte:
    {
        Memory_WriteByte(address + index, Byte(value & 0xFF));
        break;
    }
    default:
    {
        UInt offset = address + (index << 0x01);
        Memory_WriteByte(offset, Byte(value >> 0x08));
        Memory_WriteByte(offset + 0x01, Byte(value & 0xFF));
        break;
    }
    } // switch
}

UInt HRArray_GetCount(UInt _this)
{
    return Memory_ReadWord(_this + 2);
}

Type HRArray_GetValueType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 4));
}

UInt HRDictionary_Clone(UInt original)
{
    Type dkType = Type(Memory_ReadByte(original + 2));
    Type dvType = Type(Memory_ReadByte(original + 3));
    UInt clone = HRDictionary_New(dkType, dvType);
    UInt iterator = 0;
    Type ktype = (Type)0;
    UInt key = 0;
    Type vtype = (Type)0;
    UInt value = 0;
    while (HRDictionary_next_R(original, iterator, ktype, key, vtype, value))
    {
        HRDictionary_Set(clone, key, ktype, value, vtype);
    }
    return clone;
}

Bool HRDictionary_Next_R(UInt _this, UInt & iterator, UInt & hrpair)
{
    Type ktype = (Type)0;
    UInt key = 0;
    Type vtype = (Type)0;
    UInt value = 0;
    Bool found = HRDictionary_next_R(_this, iterator, ktype, key, vtype, value);
    if (found && (0x00 != value) && (vtype == Type::eVariant))
    {
        value = HRVariant_UnBox_R(value, vtype);
    }
    hrpair = HRPair_New(ktype, key, vtype, value);
    return found;
}

Type HRDictionary_GetKeyType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 2));
}

Type HRDictionary_GetValueType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 3));
}

UInt HRDictionary_New(Type ktype, Type vtype)
{
    UInt address = GC_New(0x08, Type::eDictionary);
    Memory_WriteByte(address + 2, Byte(ktype));
    Memory_WriteByte(address + 3, Byte(vtype));
    Memory_WriteWord(address + 4, 0x00);
    Memory_WriteWord(address + 6, 0x00);
    Memory_WriteWord(address + 8, 0x00);
    return address;
}

UInt HRDictionary_GetCount(UInt _this)
{
    return Memory_ReadWord(_this + 4);
}

void HRDictionary_Set(UInt _this, UInt key, Type ktype, UInt value, Type vtype)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Type dvType = Type(Memory_ReadByte(_this + 3));
    UInt count = Memory_ReadWord(_this + 4);
    UInt capacity = Memory_ReadWord(_this + 6);
    Bool valueKeys = (dkType != Type::eString);
    if (0x04 * (count + 0x01) > capacity * 0x03)
    {
        UInt newCapacity = 0x20;
        if (capacity >= 0x80)
        {
            newCapacity = capacity + 0x20;
        }
        else if (capacity > 0x00)
        {
            newCapacity = capacity * 0x02;
        }
        HRDictionary_adjustCapacity(_this, newCapacity);
    }
    UInt pEntries = Memory_ReadWord(_this + 8);
    capacity = Memory_ReadWord(_this + 6);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = HRDictionary_hashKey16(key);
    }
    UInt pEntry = HRDictionary_findEntry(pEntries, capacity, key, hash, valueKeys);
    UInt ekey = Memory_ReadWord(pEntry + 0);
    UInt evalue = Memory_ReadWord(pEntry + 6);
    Bool isNewKey = (ekey == 0x00);
    Bool existingValue = evalue != 0x00;
    if (valueKeys)
    {
        Bool isOccupied = Memory_ReadByte(pEntry + 2) != 0x00;
        isNewKey = !isOccupied;
        existingValue = existingValue && isOccupied;
    }
    if (isNewKey && (evalue == 0x00))
    {
        Memory_WriteWord(_this + 4, Memory_ReadWord(_this + 4) + 0x01);
    }
    if (valueKeys)
    {
        Memory_WriteWord(pEntry + 0, key);
        Memory_WriteWord(pEntry + 2, 0x01);
    }
    else
    {
        if (0x00 != ekey)
        {
            GC_Release(ekey);
        }
        Memory_WriteWord(pEntry + 0, GC_Clone(key));
        Memory_WriteWord(pEntry + 2, hash);
    }
    if (Types_IsReferenceType(dvType))
    {
        if (existingValue)
        {
            GC_Release(evalue);
        }
        Memory_WriteWord(pEntry + 6, GC_Clone(value));
    }
    else
    {
        Memory_WriteWord(pEntry + 6, value);
    }
}

Bool HRDictionary_Contains(UInt _this, UInt key)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Bool valueKeys = (dkType != Type::eString);
    UInt pEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = HRDictionary_hashKey16(key);
    }
    UInt pEntry = HRDictionary_findEntry(pEntries, capacity, key, hash, valueKeys);
    return HRDictionary_validEntry(pEntry, valueKeys);
}

UInt HRDictionary_Get_R(UInt _this, UInt key, Type & vtype)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Bool valueKeys = (dkType != Type::eString);
    UInt pEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    UInt hash = 0;
    if (!valueKeys)
    {
        hash = HRDictionary_hashKey16(key);
    }
    UInt pEntry = HRDictionary_findEntry(pEntries, capacity, key, hash, valueKeys);
    if (!HRDictionary_validEntry(pEntry, valueKeys))
    {
        Minimal_Error_Set(0x03);
        return 0x00;
    }
    Type dvType = Type(Memory_ReadByte(_this + 3));
    UInt value = Memory_ReadWord(pEntry + 6);
    vtype = dvType;
    if (0x00 != value)
    {
        if (vtype == Type::eVariant)
        {
            vtype = Type(Memory_ReadByte(value));
        }
        if (vtype == Type::eVariant)
        {
            value = HRVariant_GetValue_R(value, vtype);
        }
        else if (Types_IsReferenceType(vtype))
        {
            vtype = Type(Memory_ReadByte(value));
            value = GC_Clone(value);
        }
    }
    return value;
}

void HRDictionary_adjustCapacity(UInt _this, UInt newCapacity)
{
    Type dkType = Type(Memory_ReadByte(_this + 2));
    Bool valueKeys = (dkType != Type::eString);
    UInt sizeRequired = 8 * newCapacity;
    UInt pNewEntries = Memory_Allocate(sizeRequired);
    Memory_Set(pNewEntries, 0x00, sizeRequired);
    UInt pOldEntries = Memory_ReadWord(_this + 8);
    UInt capacity = Memory_ReadWord(_this + 6);
    UInt count = 0x00;;
    for (UInt i = 0x00; i < capacity; i++)
    {
        UInt pOldEntry = pOldEntries + i * 8;
        if (!HRDictionary_validEntry(pOldEntry, valueKeys))
        {
            continue;;
        }
        UInt key = Memory_ReadWord(pOldEntry + 0);
        UInt hash = Memory_ReadWord(pOldEntry + 2);
        UInt pNewEntry = HRDictionary_findEntry(pNewEntries, newCapacity, key, hash, valueKeys);
        Memory_WriteWord(pNewEntry + 0, key);
        Memory_WriteWord(pNewEntry + 2, hash);
        Memory_WriteWord(pNewEntry + 6, Memory_ReadWord(pOldEntry + 6));
        
        count++;
    }
    if (0x00 != pOldEntries)
    {
        Memory_Free(pOldEntries);
    }
    Memory_WriteWord(_this + 4, count);
    Memory_WriteWord(_this + 6, newCapacity);
    Memory_WriteWord(_this + 8, pNewEntries);
}

UInt HRDictionary_hashKey16(UInt key)
{
    UInt length = Memory_ReadWord(key + 0x02);
    UInt hash = 0x9DC5;;
    for (UInt i = 0x00; i < length; i++)
    {
        UInt ch = Memory_ReadByte(key + 0x04 + i);
        hash = hash ^ ch;
        hash = hash * 0x0193;
    }
    return hash;
}

UInt HRDictionary_findEntry(UInt pEntries, UInt capacity, UInt key, UInt hash, Bool valueKeys)
{
    UInt index = 0;
    if (valueKeys)
    {
        index = key % capacity;
    }
    else
    {
        index = hash % capacity;
    }
    UInt tombstone = 0x00;
    for (;;)
    {
        UInt pEntry = pEntries + index * 8;
        UInt ekey = Memory_ReadWord(pEntry + 0);
        if (valueKeys)
        {
            Bool isOccupied = Memory_ReadByte(pEntry + 2) != 0x00;
            if (!isOccupied)
            {
                UInt value = Memory_ReadWord(pEntry + 6);
                if (value == 0x00)
                {
                    return (tombstone != 0x00) ? (tombstone) : (pEntry);
                }
                else
                {
                    if (tombstone == 0x00)
                    {
                        tombstone = pEntry;
                    }
                }
            }
            else if (ekey == key)
            {
                return pEntry;
            }
        }
        else
        {
            if (ekey == 0x00)
            {
                UInt value = Memory_ReadWord(pEntry + 6);
                if (value == 0x00)
                {
                    return (tombstone != 0x00) ? (tombstone) : (pEntry);
                }
                else
                {
                    if (tombstone == 0x00)
                    {
                        tombstone = pEntry;
                    }
                }
            }
            else if (HRString_Compare(ekey, key) == 0x00)
            {
                return pEntry;
            }
        }
        index = (index + 0x01) % capacity;
    }
    return 0x00;
}

Bool HRDictionary_validEntry(UInt pEntry, Bool valueKeys)
{
    if (pEntry == 0x00)
    {
        return false;
    }
    else if (valueKeys)
    {
        return Memory_ReadByte(pEntry + 2) != 0x00;
    }
    else
    {
        return Memory_ReadWord(pEntry + 0) != 0x00;
    }
    return false;
}

UInt HRPair_Clone(UInt original)
{
    Type dkType = Type(Memory_ReadByte(original + 2));
    Type dvType = Type(Memory_ReadByte(original + 3));
    UInt key = Memory_ReadWord(original + 4);
    UInt value = Memory_ReadWord(original + 6);
    return HRPair_New(dkType, key, dvType, value);
}

UInt HRPair_New(Type ktype, UInt key, Type vtype, UInt value)
{
    UInt address = GC_New(0x06, Type::ePair);
    Memory_WriteByte(address + 2, Byte(ktype));
    Memory_WriteByte(address + 3, Byte(vtype));
    if (Types_IsReferenceType(ktype) && (0x00 != key))
    {
        key = GC_Clone(key);
    }
    if (Types_IsReferenceType(vtype) && (0x00 != value))
    {
        value = GC_Clone(value);
    }
    Memory_WriteWord(address + 4, key);
    Memory_WriteWord(address + 6, value);
    return address;
}

UInt HRPair_GetValue_R(UInt _this, Type & vtype)
{
    vtype = Type(Memory_ReadByte(_this + 3));
    UInt value = Memory_ReadWord(_this + 6);
    if (vtype == Type::eVariant)
    {
        value = HRVariant_GetValue_R(value, vtype);
    }
    else if (Types_IsReferenceType(vtype))
    {
        value = GC_Clone(value);
        vtype = Type(Memory_ReadByte(value));
    }
    return value;
}

UInt HRPair_GetKey_R(UInt _this, Type & ktype)
{
    ktype = Type(Memory_ReadByte(_this + 2));
    UInt key = Memory_ReadWord(_this + 4);
    if (Types_IsReferenceType(ktype))
    {
        key = GC_Clone(key);
        ktype = Type(Memory_ReadByte(key));
    }
    return key;
}

Type HRPair_GetValueType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 3));
}

Type HRPair_GetKeyType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 2));
}

UInt HRVariant_Clone(UInt original)
{
    Type vtype = Type(Memory_ReadByte(original + 2));
    UInt value = Memory_ReadWord(original + 3);
    return HRVariant_New(value, vtype);
}

UInt HRVariant_CreateValueVariant(UInt value, Type vtype)
{
    UInt address = GC_New(0x03, Type::eVariant);
    Memory_WriteByte(address + 2, Byte(vtype));
    Memory_WriteWord(address + 3, value);
    return address;
}

UInt HRVariant_New(UInt value, Type vtype)
{
    UInt address = GC_New(0x03, Type::eVariant);
    Memory_WriteByte(address + 2, Byte(vtype));
    if (Types_IsReferenceType(vtype))
    {
        value = GC_Clone(value);
    }
    Memory_WriteWord(address + 3, value);
    return address;
}

UInt HRVariant_UnBox_R(UInt _this, Type & vtype)
{
    vtype = Type(Memory_ReadByte(_this + 2));
    return Memory_ReadWord(_this + 3);
}

Type HRVariant_GetValueType(UInt _this)
{
    return Type(Memory_ReadByte(_this + 2));
}

UInt HRVariant_GetValue_R(UInt _this, Type & vtype)
{
    vtype = Type(Memory_ReadByte(_this + 2));
    UInt value = Memory_ReadWord(_this + 3);
    if (Types_IsReferenceType(vtype))
    {
        value = GC_Clone(value);
        vtype = Type(Memory_ReadByte(value));
    }
    return value;
}

Bool HRVariant_IsEqual(UInt left, Type ltype, UInt right, Type rtype)
{
    Bool lref = Types_IsReferenceType(ltype);
    Bool rref = Types_IsReferenceType(rtype);
    if (lref == rref)
    {
        if (lref)
        {
            if (ltype == rtype)
            {
                switch (ltype)
                {
                case Type::eString:
                {
                    return 0x00 == HRString_Compare(left, right);
                    break;
                }
                case Type::eLong:
                {
                    return 0x00 != External_LongEQ(left, right);
                    break;
                }
                default:
                {
                    Runtime_ErrorDump(0x4B);
                    Minimal_Error_Set(0x0A);
                    break;
                }
                } // switch
            }
        }
        else
        {
            switch (ltype)
            {
            case Type::eBool:
            {
                if (rtype != Type::eBool)
                {
                    return false;
                }
                return left == right;
                break;
            }
            case Type::eChar:
            {
                if (rtype != Type::eChar)
                {
                    return false;
                }
                return left == right;
                break;
            }
            case Type::eByte:
            case Type::eUInt:
            {
                switch (rtype)
                {
                case Type::eByte:
                case Type::eUInt:
                {
                    return left == right;
                    break;
                }
                case Type::eInt:
                {
                    if (0x00 == (0x8000 & right))
                    {
                        return left == right;
                    }
                    break;
                }
                } // switch
                break;
            }
            case Type::eInt:
            {
                switch (rtype)
                {
                case Type::eByte:
                case Type::eUInt:
                {
                    if (0x00 == (0x8000 & left))
                    {
                        return left == right;
                    }
                    break;
                }
                case Type::eInt:
                {
                    return left == right;
                    break;
                }
                } // switch
                break;
            }
            } // switch
        }
    }
    return false;
}

void IO_WriteUInt(UInt _this)
{
    IO_writeDigit(_this);
}

void IO_writeDigit(UInt uthis)
{
    UInt digit = uthis % 0x0A;
    Char c = HRByte_ToDigit(Byte(digit));
    uthis = uthis / 0x0A;
    if (uthis != 0x00)
    {
        IO_writeDigit(uthis);
    }
    IO_Write(c);
}

UInt Memory_Available()
{
    UInt available = 0;
    UInt current = Memory_freeList;
    for (;;)
    {
        if (0x00 == current)
        {
            break;;
        }
        available = available + Memory_ReadWord(current) - 0x02;
        current = Memory_ReadWord(current + 0x02);
    }
    return available;
}

UInt Memory_Maximum()
{
    UInt available = 0;
    UInt current = Memory_freeList;
    for (;;)
    {
        if (0x00 == current)
        {
            break;;
        }
        UInt size = Memory_ReadWord(current + 0x00);
        if (size > available)
        {
            available = size;
        }
        current = Memory_ReadWord(current + 0x02);
    }
    if (available > 0x00)
    {
        
        available--;
        
        available--;
    }
    return available;
}

UInt HRString_NewFromConstant1(UInt doubleChar)
{
    Byte lsb = Byte(doubleChar & 0xFF);
    Byte msb = Byte(doubleChar >> 0x08);
    UInt address = HRString_new(((msb == 0x00)) ? (0x01) : (0x02));
    Memory_WriteWord(address + 2, ((msb == 0x00)) ? (0x01) : (0x02));
    Memory_WriteByte(address + 4, lsb);
    if (msb != 0x00)
    {
        Memory_WriteByte(address + 4 + 0x01, msb);
    }
    return address;
}

UInt HRString_NewFromConstant0(UInt location, UInt length)
{
    UInt address = HRString_new(length);
    Memory_WriteWord(address + 2, length);;
    for (UInt i = 0x00; i < length; i++)
    {
        Memory_WriteByte(address + 4 + i, Memory_ReadCodeByte(location + i));
    }
    return address;
}

UInt HRString_InsertChar(UInt _this, UInt index, Char ch)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_new(length + 0x01);
    UInt j = 0x00;;
    for (UInt i = 0x00; i < length; i++)
    {
        if (i == index)
        {
            Memory_WriteByte(result + 4 + j, Byte(ch));
            
            j++;
        }
        Memory_WriteByte(result + 4 + j, Memory_ReadByte(_this + 4 + i));
        
        j++;
    }
    if ((length == 0x00) || (index >= length))
    {
        Memory_WriteByte(result + 4 + j, Byte(ch));
    }
    Memory_WriteWord(result + 2, length + 0x01);
    return result;
}

UInt HRString_ToUpper(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_ToUpper_R(copy);
    return copy;
}

void HRString_ToUpper_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0x00;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        Memory_WriteByte(_this + 4 + i, Byte(HRChar_ToUpper(ch)));
        
        i++;
    }
}

UInt HRString_ToLower(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_ToLower_R(copy);
    return copy;
}

void HRString_ToLower_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0x00;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        Memory_WriteByte(_this + 4 + i, Byte(HRChar_ToLower(ch)));
        
        i++;
    }
}

Bool HRString_StartsWith(UInt _this, Char with)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0x00)
    {
        return false;
    }
    return (Char(Memory_ReadByte(_this + 4)) == with);
}

Bool HRString_StartsWith(UInt _this, UInt with)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(with);
    if (length0 < length1)
    {
        return false;
    }
    if (length1 == 0x00)
    {
        return true;
    }
    UInt i = 0x00;
    for (;;)
    {
        if (i == length1)
        {
            break;;
        }
        Char w = Char(Memory_ReadByte(with + 4 + i));
        Char t = Char(Memory_ReadByte(_this + 4 + i));
        if (w != t)
        {
            return false;
        }
        
        i++;
    }
    return true;
}

Bool HRString_Contains(UInt _this, Char needle)
{
    UInt i = 0;
    UInt length = HRString_GetLength(_this);
    for (; i < length; i++)
    {
        if (Char(Memory_ReadByte(_this + 4 + i)) == needle)
        {
            return true;
        }
    }
    return false;
}

Bool HRString_IndexOf_R(UInt _this, Char pattern, UInt & index)
{
    UInt length = HRString_GetLength(_this);;
    for (UInt i = 0x00; i < length; i++)
    {
        if (Char(Memory_ReadByte(_this + 4 + i)) == pattern)
        {
            index = i;
            return true;
        }
    }
    return false;
}

Bool HRString_IndexOf_R(UInt _this, Char pattern, UInt searchIndex, UInt & index)
{
    UInt length = HRString_GetLength(_this);
    for (;;)
    {
        if (searchIndex >= length)
        {
            break;;
        }
        if (Char(Memory_ReadByte(_this + 4 + searchIndex)) == pattern)
        {
            index = searchIndex;
            return true;
        }
        
        searchIndex++;
    }
    return false;
}

Bool HRString_EndsWith(UInt _this, Char with)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0x00)
    {
        return false;
    }
    return (Char(Memory_ReadByte(_this + 4 + length - 0x01)) == with);
}

Bool HRString_EndsWith(UInt _this, UInt with)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(with);
    if (length0 < length1)
    {
        return false;
    }
    if (length1 == 0x00)
    {
        return true;
    }
    UInt i = 0x01;
    for (;;)
    {
        Char w = Char(Memory_ReadByte(with + 4 + length1 - i));
        Char t = Char(Memory_ReadByte(_this + 4 + length0 - i));
        if (w != t)
        {
            return false;
        }
        if (i == length1)
        {
            break;;
        }
        
        i++;
    }
    return true;
}

Int HRString_Compare(UInt left, UInt right)
{
    UInt i = 0;
    Int result = 0;
    UInt ll = HRString_GetLength(left);
    UInt rl = HRString_GetLength(right);
    for (;;)
    {
        if (i >= ll)
        {
            break;;
        }
        if (i >= rl)
        {
            break;;
        }
        if (Memory_ReadByte(left + 4 + i) != Memory_ReadByte(right + 4 + i))
        {
            break;;
        }
        
        i++;
    }
    for (;;)
    {
        if ((ll == 0x00) && (rl == 0x00))
        {
            break;;
        }
        if ((i < ll) && (i < rl))
        {
            if (Int(Memory_ReadByte(left + 4 + i)) > Int(Memory_ReadByte(right + 4 + i)))
            {
                result = 0x01;
            }
            else
            {
                result = -0x01;
            }
            break;;
        }
        if (i >= ll)
        {
            if (i >= rl)
            {
                break;;
            }
            result = -0x01;
            break;;
        }
        result = 0x01;
        break;;
    }
    return result;
}

UInt HRString_Replace(UInt _this, UInt pattern, UInt replace)
{
    UInt result = 0;
    UInt patternLength = HRString_GetLength(pattern);
    if (patternLength == 0x00)
    {
        result = HRString_clone(_this, 0x00);
        return result;
    }
    UInt originalLength = HRString_GetLength(_this);
    UInt replaceLength = HRString_GetLength(replace);
    if (replaceLength <= patternLength)
    {
        result = HRString_clone(_this, 0x00);
    }
    else
    {
        result = HRString_clone(_this, (replaceLength - patternLength) * originalLength);
    }
    UInt i = 0;
    UInt j = 0;
    for (;;)
    {
        if (i == originalLength)
        {
            break;;
        }
        Bool match = false;
        if (i + patternLength <= originalLength)
        {
            match = true;;
            for (UInt n = 0x00; n < patternLength; n++)
            {
                if (Memory_ReadByte(_this + 4 + i + n) != Memory_ReadByte(pattern + 4 + n))
                {
                    match = false;
                    break;;
                }
            }
        }
        if (match)
        {
            i = i + patternLength;;
            for (UInt n = 0x00; n < replaceLength; n++)
            {
                Memory_WriteByte(result + 4 + j, Memory_ReadByte(replace + 4 + n));
                
                j++;
            }
        }
        else
        {
            Memory_WriteByte(result + 4 + j, Memory_ReadByte(_this + 4 + i));
            
            j++;
            
            i++;
        }
    }
    Memory_WriteWord(result + 2, j);
    return result;
}

UInt HRString_Replace(UInt _this, Char from, Char to)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_clone(_this, 0x00);;
    for (UInt i = 0x00; i < length; i++)
    {
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        if (ch == from)
        {
            Memory_WriteByte(result + 4 + i, Byte(to));
        }
    }
    return result;
}

UInt HRString_Append(UInt _this, UInt append)
{
    UInt length0 = HRString_GetLength(_this);
    UInt length1 = HRString_GetLength(append);
    UInt result = HRString_new(length0 + length1);
    Memory_WriteWord(result + 2, length0 + length1);;
    for (UInt i = 0x00; i < length0; i++)
    {
        Memory_WriteByte(result + 4 + i, Memory_ReadByte(_this + 4 + i));
    };
    for (UInt i = 0x00; i < length1; i++)
    {
        Memory_WriteByte(result + 4 + i + length0, Memory_ReadByte(append + 4 + i));
    }
    return result;
}

UInt HRString_Append(UInt _this, Char ch)
{
    UInt length = HRString_GetLength(_this);
    UInt result = HRString_clone(_this, 0x01);
    Memory_WriteByte(result + 4 + length, Byte(ch));
    Memory_WriteWord(result + 2, length + 0x01);
    return result;
}

UInt HRString_Substring(UInt _this, UInt start)
{
    UInt limit = HRString_GetLength(_this);
    return HRString_Substring(_this, start, limit);
}

UInt HRString_Substring(UInt _this, UInt start, UInt limit)
{
    UInt length0 = HRString_GetLength(_this);
    if (start >= length0)
    {
        start = length0;
    }
    UInt length1 = length0 - start;
    UInt result = HRString_new(length1);
    UInt newLength = 0x00;;
    for (UInt i = 0x00; i < length1; i++)
    {
        if (newLength == limit)
        {
            break;;
        }
        Memory_WriteByte(result + 4 + i, Memory_ReadByte(_this + 4 + i + start));
        
        newLength++;
    }
    Memory_WriteWord(result + 2, newLength);
    return result;
}

void HRString_Substring_R(UInt & _this, UInt start)
{
    UInt length = HRString_GetLength(_this);
    if (start == 0x00)
    {
        return;
    }
    if (start >= length)
    {
        Memory_WriteWord(_this + 2, 0x00);
        return;
    }
    Memory_WriteWord(_this + 2, length - start);
    UInt i = start;
    UInt j = 0x00;
    for (;;)
    {
        Memory_WriteByte(_this + 4 + j, Memory_ReadByte(_this + 4 + i));
        
        i++;
        
        j++;
        if (i == length)
        {
            break;;
        }
    }
}

void HRString_BuildString_R(UInt & _this, UInt append)
{
    UInt length1 = HRString_GetLength(append);
    if (length1 > 0x00)
    {
        UInt capacity = HRString_getCapacity(_this);
        UInt length0 = HRString_GetLength(_this);
        if (capacity < length0 + length1)
        {
            UInt copy = HRString_clone(_this, length1);
            GC_Release(_this);
            _this = copy;
        };
        for (UInt i = 0x00; i < length1; i++)
        {
            Memory_WriteByte(_this + 4 + length0 + i, Memory_ReadByte(append + 4 + i));
        }
        Memory_WriteWord(_this + 2, length0 + length1);
    }
}

void HRString_BuildFront_R(UInt & _this, Char ch)
{
    UInt capacity = HRString_getCapacity(_this);
    UInt length = HRString_GetLength(_this);
    if (capacity < length + 0x01)
    {
        UInt copy = HRString_clone(_this, 0x01);
        GC_Release(_this);
        _this = copy;
    }
    UInt i = length;
    for (;;)
    {
        if (i == 0x00)
        {
            break;;
        }
        Memory_WriteByte(_this + 4 + i, Memory_ReadByte(_this + 4 + i - 0x01));
        
        i--;
    }
    Memory_WriteByte(_this + 4, Byte(ch));
    Memory_WriteWord(_this + 2, length + 0x01);
}

UInt HRString_Trim(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_TrimRight_R(copy);
    HRString_TrimLeft_R(copy);
    return copy;
}

void HRString_TrimRight_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    if (length == 0x00)
    {
        return;
    }
    UInt i = length - 0x01;
    for (;;)
    {
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        if (ch != ' ')
        {
            Memory_WriteWord(_this + 2, i + 0x01);
            break;;
        }
        if (i == 0x00)
        {
            Memory_WriteWord(_this + 2, 0x00);
            break;;
        }
        
        i--;
    }
}

void HRString_TrimLeft_R(UInt & _this)
{
    UInt length = HRString_GetLength(_this);
    UInt i = 0x00;
    for (;;)
    {
        if (i == length)
        {
            break;;
        }
        Char ch = Char(Memory_ReadByte(_this + 4 + i));
        if (ch != ' ')
        {
            break;;
        }
        
        i++;
    }
    HRString_Substring_R(_this, i);
}

UInt HRString_TrimLeft(UInt _this)
{
    UInt copy = HRString_Clone(_this);
    HRString_TrimLeft_R(copy);
    return copy;
}

UInt HRLong_NewFromConstant(UInt location)
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteWord(address + 0x02, Memory_ReadCodeWord(location));
    Memory_WriteWord(address + 0x04, Memory_ReadCodeWord(location + 0x02));
    return address;
}

UInt HRLong_ToBytes(UInt ichunk)
{
    UInt lst = HRList_New(Type::eByte);;
    for (Byte i = 0x00; i < 0x04; i++)
    {
        Byte b = Memory_ReadByte(ichunk + 0x02 + i);
        HRList_Append(lst, b, Type::eByte);
    }
    return lst;
}

UInt HRLong_ToUInt(UInt _this)
{
    UInt value = Memory_ReadWord(_this + 0x04);
    return Memory_ReadWord(_this + 0x02);
}

UInt HRLong_LongNegate(UInt top)
{
    UInt zero = HRUInt_ToLong(0x00);
    UInt result = External_LongSub(zero, top);
    GC_Release(zero);
    return result;
}

UInt HRLong_LongAddB(UInt next, UInt top)
{
    UInt argument = HRUInt_ToLong(top);
    UInt result = External_LongAdd(next, argument);
    GC_Release(argument);
    return result;
}

UInt HRLong_LongSubB(UInt next, UInt top)
{
    UInt argument = HRUInt_ToLong(top);
    UInt result = External_LongSub(next, argument);
    GC_Release(argument);
    return result;
}

UInt HRLong_New()
{
    UInt address = GC_New(0x04, Type::eLong);
    Memory_WriteWord(address + 0x02, 0x00);
    Memory_WriteWord(address + 0x04, 0x00);
    return address;
}

Char HRChar_ToUpper(Char _this)
{
    Byte b = 0;
    if (HRChar_IsLower(_this))
    {
        b = Byte(_this) - 0x61 + 0x41;
        _this = Char(b);
    }
    return _this;
}

Char HRChar_ToLower(Char _this)
{
    Byte b = 0;
    if (HRChar_IsUpper(_this))
    {
        b = Byte(_this) - 0x41 + 0x61;
        _this = Char(b);
    }
    return _this;
}

Bool HRChar_IsUpper(Char _this)
{
    Byte b = 0;
    b = Byte(_this);
    return ((b >= 0x41) && (b <= 0x5A));
}

Bool HRChar_IsLower(Char _this)
{
    Byte b = 0;
    b = Byte(_this);
    return ((b >= 0x61) && (b <= 0x7A));
}

Bool HRChar_IsDigit(Char _this)
{
    Byte b = 0;
    b = Byte(_this);
    return ((b >= 0x30) && (b <= 0x39));
}

Bool HRChar_IsLetterOrDigit(Char _this)
{
    Byte b = 0;
    b = Byte(_this);
    return ((b >= 0x30) && (b <= 0x39)) || ((b >= 0x41) && (b <= 0x5A)) || ((b >= 0x61) && (b <= 0x7A));
}

Bool HRChar_IsHexDigit(Char _this)
{
    Byte b = 0;
    b = Byte(_this);
    return ((b >= 0x30) && (b <= 0x39)) || ((b >= 0x41) && (b <= 0x46)) || ((b >= 0x61) && (b <= 0x66));
}

Char HRByte_ToDigit(Byte d)
{
    d = d + 0x30;
    return Char(d);
}

UInt HRUInt_ToLong(UInt ui)
{
    UInt _this = HRLong_New();
    Memory_WriteWord(_this + 0x02, ui);
    Memory_WriteWord(_this + 0x04, 0x00);
    return _this;
}

UInt HRInt_ToLong(UInt ichunk)
{
    UInt _this = HRLong_New();
    Memory_WriteWord(_this + 0x02, ichunk);
    if ((0x8000 & ichunk) != 0x00)
    {
        Memory_WriteWord(_this + 0x04, 0xFFFF);
    }
    else
    {
        Memory_WriteWord(_this + 0x04, 0x00);
    }
    return _this;
}

UInt HRInt_ToBytes(UInt ichunk)
{
    Byte lsb = Byte(ichunk & 0xFF);
    Byte msb = Byte(ichunk >> 0x08);
    UInt lst = HRList_New(Type::eByte);
    HRList_Append(lst, lsb, Type::eByte);
    HRList_Append(lst, msb, Type::eByte);
    return lst;
}

Byte HRInt_GetByte(UInt ichunk, UInt i)
{
    return ((i == 0x00)) ? (Byte(ichunk & 0xFF)) : (Byte(ichunk >> 0x08));
}

UInt HRInt_FromBytes(Byte b0, Byte b1)
{
    return b0 + (b1 << 0x08);
}

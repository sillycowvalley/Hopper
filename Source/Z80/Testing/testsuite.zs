program TestSuite
{

    uint cafe;
    uint errorLine;
    
    uint countTrue;
    uint countFalse;
        
    uses "/Source/Z80/Firmware/LCDDriver"
    uses "/Source/Z80/Firmware/Utilities"
    uses "/Source/Z80/Firmware/Memory"
    
    bool StackError(uint atLine)
    {
        cafe = 0xADDE;
        errorLine = atLine;
        PrintAt(0,1, 'S', 't', 'a', 'c', 'k', '!', char(0), char(0), char(0), char(0));
        
        uint c0 = (atLine & 0x000F);
        atLine = atLine >> 4;
        uint c1 = (atLine & 0x000F);
        atLine = atLine >> 4;
        uint c2 = (atLine & 0x000F);
        atLine = atLine >> 4;
        uint c3 = (atLine & 0x000F);
        
        PrintAt(0,2, 'L', 'i', 'n', 'e', ':', ' ', ToHexChar(c3), ToHexChar(c2), ToHexChar(c1), ToHexChar(c0));
        return false;
    }
    
    TestFailed(uint atLine, char a, char b, char c, char d, char e, char f, char g, char h, char i, char j)
    {
        cafe = 0xADDE;
        errorLine = atLine;
        PrintAt(0,1, 'F', 'a', 'i', 'l', 'e', 'd', '!', char(0), char(0), char(0));
        PrintAt(0,2, a, b, c, d, e, f, g, h, i, j);
        
        uint c0 = (atLine & 0x000F);
        atLine = atLine >> 4;
        uint c1 = (atLine & 0x000F);
        atLine = atLine >> 4;
        uint c2 = (atLine & 0x000F);
        atLine = atLine >> 4;
        uint c3 = (atLine & 0x000F);
        
        PrintAt(0,3, 'L', 'i', 'n', 'e', ':', ' ', ToHexChar(c3), ToHexChar(c2), ToHexChar(c1), ToHexChar(c0));
    }
    
    bool HeapAllocation()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            MemoryInitialize();

            if (MemoryMax() != 0x6FFE)
            {
                failLine = line;
                break;
            }
            if (MemoryAvail() != 0x6FFE)
            {
                failLine = line;
                break;
            }
            
            uint address0 = MemoryAllocate(256);
            uint address1 = MemoryAllocate(17);
            uint address2 = MemoryAllocate(23);
            uint address3 = MemoryAllocate(127);
            uint address4 = MemoryAllocate(40);
            uint address5 = MemoryAllocate(1000);
            uint address6 = MemoryAllocate(3);
            
            if (MemoryMax() != 0x6A35)
            {
                failLine = line;
                break;
            }
            if (MemoryAvail() != 0x6A35)
            {
                failLine = line;
                break;
            }
            
            MemoryFree(address1);
            MemoryFree(address4);
            MemoryFree(address3);
            MemoryFree(address6);
            
            if (MemoryMax() != 0x6A3B)
            {
                failLine = line;
                break;
            }
            if (MemoryAvail() != 0x6AF5)
            {
                failLine = line;
                break;
            }
            
            uint address7 = MemoryAllocate(1270);
            uint address8 = MemoryAllocate(401);
            uint address9 = MemoryAllocate(101);
            
            MemoryFree(address9);
            MemoryFree(address0);
            MemoryFree(address7);
            MemoryFree(address5);
            MemoryFree(address2);
            MemoryFree(address8);
            
            if (MemoryMax() != 0x6FFE)
            {
                failLine = line;
                break;
            }
            if (MemoryAvail() != 0x6FFE)
            {
                failLine = line;
                break;
            }
        
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'H', 'e', 'a', 'p', char(0), char(0), char(0), char(0), char(0), char(0));
        }
        if (startSP-2 != sp)
        {
            return StackError(line);
        }
        return success;
    }
    
    bool WriteMemory()
    {
        bool success = false;
        uint startSP = sp;
        loop
        {
            uint i = 0xFF00;
            byte b = 0;
            loop
            {
                memory[i] = b;
                b++;
                i++;
                if (b == 16)
                {
                    break;
                }
            }
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(line, 'W', 'r', 'i', 't', 'e', 'M', 'e', 'm', 'o', 'r');
        }
        if (startSP-2 != sp)
        {
            return StackError(line);
        }
        return success;
    }
    
    bool ReadMemory()
    {
        bool success = false;
        uint startSP = sp;
        loop
        {
            uint i = 0xFF00;
            byte b = 0;
            byte acc = 0;
            loop
            {
                acc = acc + memory[i];
                b++;
                i++;
                if (b == 16)
                {
                    break;
                }
            }
            success = acc == 120;
            break;
        }
        if (!success)
        {
            TestFailed(line, 'R', 'e', 'a', 'd', 'M', 'e', 'm', 'o', 'r', 'y');
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    
    bool Booleans()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            bool t = true;
            bool f = false;
            if (f == t)
            {
                failLine = line;
                break;
            }
            if (!(f != t))
            {
                failLine = line;
                break;
            }
            if (f == true)
            {
                failLine = line;
                break;
            }
            if (!(f != true))
            {
                failLine = line;
                break;
            }
            if (!(f == false))
            {
                failLine = line;
                break;
            }
            if (f != false)
            {
                failLine = line;
                break;
            }
            if (t == false)
            {
                failLine = line;
                break;
            }
            if (!(t != false))
            {
                failLine = line;
                break;
            }
            if (!(t == true))
            {
                failLine = line;
                break;
            }
            if (t != true)
            {
                failLine = line;
                break;
            }
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'B', 'o', 'o', 'l', 'e', 'a', 'n', 's', char(0), char(0));
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
        
    bool CountTrue()
    {
        countTrue++;
        return true;
    }
    bool CountFalse()
    {
        countFalse++;
        return false;
    }
    bool ShortCircuit()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            countTrue = 0; countFalse = 0;
            if (CountTrue() || CountTrue() || CountTrue())
            {
            }
            if (countTrue != 1)
            {
                failLine = line;
                break;
            }
            countTrue = 0; countFalse = 0;
            if (CountTrue() && CountTrue() && CountTrue())
            {
            }
            if (countTrue != 3)
            {
                failLine = line;
                break;
            }
            countTrue = 0; countFalse = 0;
            if (CountFalse() || CountFalse() || CountFalse())
            {
            }
            if (countFalse != 3)
            {
                failLine = line;
                break;
            }
            countTrue = 0; countFalse = 0;
            if (CountFalse() && CountFalse() && CountFalse())
            {
            }
            if (countFalse != 1)
            {
                failLine = line;
                break;
            }
            
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'S', 'h', 'o', 'r', 't', 'B', 'o', 'o', 'l', char(0));
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    bool Compares()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            if (0 > 1)
            {
                failLine = line;
                break;
            }
            if (!(0 < 1))
            {
                failLine = line;
                break;
            }
            if (0 < -1)
            {
                failLine = line;
                break;
            }
            if (!(0 > -1))
            {
                failLine = line;
                break;
            }
            if (0 > 0)
            {
                failLine = line;
                break;
            }
            if (0 < 0)
            {
                failLine = line;
                break;
            }
            
            if (0 >= 1)
            {
                failLine = line;
                break;
            }
            if (!(0 <= 1))
            {
                failLine = line;
                break;
            }
            if (0 <= -1)
            {
                failLine = line;
                break;
            }
            if (!(0 >= -1))
            {
                failLine = line;
                break;
            }
            if (!(0 >= 0))
            {
                failLine = line;
                break;
            }
            if (!(0 <= 0))
            {
                failLine = line;
                break;
            }
            
            
            if (500 > 1000)
            {
                failLine = line;
                break;
            }
            if (!(500 < 1000))
            {
                failLine = line;
                break;
            }
            if (500 < -1000)
            {
                failLine = line;
                break;
            }
            if (!(500 > -1000))
            {
                failLine = line;
                break;
            }
            if (500 > 500)
            {
                failLine = line;
                break;
            }
            if (500 < 500)
            {
                failLine = line;
                break;
            }
            
            if (500 >= 1000)
            {
                failLine = line;
                break;
            }
            if (!(500 <= 1000))
            {
                failLine = line;
                break;
            }
            if (500 <= -1000)
            {
                failLine = line;
                break;
            }
            if (!(500 >= -1000))
            {
                failLine = line;
                break;
            }
            if (!(500 >= 500))
            {
                failLine = line;
                break;
            }
            if (!(500 <= 500))
            {
                failLine = line;
                break;
            }
            
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'C', 'o', 'm', 'p', 'a', 'r', 'e', 's', char(0), char(0));
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    
    bool Equality()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            if (0 == 1)
            {
                failLine = line;
                break;
            }
            if (0 == -1)
            {
                failLine = line;
                break;
            }
            if (!(0 == 0))
            {
                failLine = line;
                break;
            }
            if (1 == -1)
            {
                failLine = line;
                break;
            }
            
            if (500 == 1000)
            {
                failLine = line;
                break;
            }
            if (500 == -1000)
            {
                failLine = line;
                break;
            }
            if (500 == -500)
            {
                failLine = line;
                break;
            }
            if (!(500 == 500))
            {
                failLine = line;
                break;
            }
            if (!(1000 == 1000))
            {
                failLine = line;
                break;
            }
            if (!(-1000 == -1000))
            {
                failLine = line;
                break;
            }
            
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'E', 'q', 'u', 'a', 'l', 'i', 't', 'y', char(0), char(0));
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    
    bool BitwiseOr()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            if ((0b0000 | 0b0001) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0b0001 | 0b0001) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0b0001 | 0b0000) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0b0000 | 0b0000) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0010 | 0b0000) != 0b0010)
            {
                failLine = line;
                break;
            }
            if ((0b0010 | 0b0001) != 0b0011)
            {
                failLine = line;
                break;
            }
            if ((0b0000 | 0b0001) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0x0F | 0xF0) != 0xFF)
            {
                failLine = line;
                break;
            }
            if ((0xFF | 0x00) != 0xFF)
            {
                failLine = line;
                break;
            }
            if ((0x00 | 0xFF) != 0xFF)
            {
                failLine = line;
                break;
            }
            if ((0xFF00 | 0x00FF) != 0xFFFF)
            {
                failLine = line;
                break;
            }
            if ((0x00FF | 0xFF00) != 0xFFFF)
            {
                failLine = line;
                break;
            }
            if ((0xFFFF | 0x0000) != 0xFFFF)
            {
                failLine = line;
                break;
            }
            if ((0x0000 | 0xFFFF) != 0xFFFF)
            {
                failLine = line;
                break;
            }
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'B', 'i', 't', 'w', 'i', 's', 'e', 'O', 'r', char(0));
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    
    bool BitwiseAnd()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            if ((0b0000 & 0b0001) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0001 & 0b0001) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0b0001 & 0b0000) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0000 & 0b0000) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0010 & 0b0000) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0010 & 0b0001) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0000 & 0b0001) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0x0F & 0xF0) != 0x00)
            {
                failLine = line;
                break;
            }
            if ((0xFF & 0x00) != 0x00)
            {
                failLine = line;
                break;
            }
            if ((0x00 & 0xFF) != 0x00)
            {
                failLine = line;
                break;
            }
            if ((0xFF & 0xFF) != 0xFF)
            {
                failLine = line;
                break;
            }
            if ((0xAA & 0xFF) != 0xAA)
            {
                failLine = line;
                break;
            }
            if ((0x55 & 0xAA) != 0x00)
            {
                failLine = line;
                break;
            }
            if ((0xFF00 & 0x00FF) != 0x0000)
            {
                failLine = line;
                break;
            }
            if ((0x00FF & 0xFF00) != 0x0000)
            {
                failLine = line;
                break;
            }
            if ((0xFFFF & 0x0000) != 0x0000)
            {
                failLine = line;
                break;
            }
            if ((0x0000 & 0xFFFF) != 0x0000)
            {
                failLine = line;
                break;
            }
            if ((0xFFFF & 0xFFFF) != 0xFFFF)
            {
                failLine = line;
                break;
            }
            if ((0xAAAA & 0x5555) != 0x0000)
            {
                failLine = line;
                break;
            }
            if ((0xFFFF & 0xAA55) != 0xAA55)
            {
                failLine = line;
                break;
            }
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'B', 'i', 't', 'w', 'i', 's', 'e', 'A', 'n', 'd');
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    
    bool ShiftLeft()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            if ((0b0001 << 1) != 0b0010)
            {
                failLine = line;
                break;
            }
            if ((0b0001 << 2) != 0b0100)
            {
                failLine = line;
                break;
            }
            if ((0b0000 << 1) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0x8000 << 1) != 0)
            {
                failLine = line;
                break;
            }
            if ((0x00FF << 4) != 0x0FF0)
            {
                failLine = line;
                break;
            }
            if ((0xFF00 << 4) != 0xF000)
            {
                failLine = line;
                break;
            }
            if ((0x00FF << 8) != 0xFF00)
            {
                failLine = line;
                break;
            }
            if ((0x0FF0 << 8) != 0xF000)
            {
                failLine = line;
                break;
            }
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'S', 'h', 'i', 'f', 't', 'L', 'e', 'f', 't', char(0));
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
    
    bool ShiftRight()
    {
        bool success = false;
        uint failLine = line;
        uint startSP = sp;
        loop
        {
            if ((0b0001 >> 1) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0010 >> 1) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0b0010 >> 2) != 0b0000)
            {
                failLine = line;
                break;
            }
            if ((0b0100 >> 2) != 0b0001)
            {
                failLine = line;
                break;
            }
            if ((0x0001 >> 1) != 0)
            {
                failLine = line;
                break;
            }
            if ((0x0FF0 >> 4) != 0x00FF)
            {
                failLine = line;
                break;
            }
            if ((0x00FF >> 4) != 0x000F)
            {
                failLine = line;
                break;
            }
            if ((0x00FF >> 8) != 0x0000)
            {
                failLine = line;
                break;
            }
            if ((0x0FF0 >> 8) != 0x000F)
            {
                failLine = line;
                break;
            }
            if ((0xFF00 >> 8) != 0x00FF)
            {
                failLine = line;
                break;
            }
            success = true;
            break;
        }
        if (!success)
        {
            TestFailed(failLine, 'S', 'h', 'i', 'f', 't', 'R', 'i', 'g', 'h', 't');
        }
        if (startSP-2 != sp)
        {
            success = StackError(line);
        }
        return success;
    }
        
        
    {
        LCDInitialize();
    
        cafe = 0xFECA; // all good
        loop
        {
            //PrintAt(0,0, 'H', 'E', 'R', 'E', ' ', '1', char(0), char(0), char(0), char(0));
            if (!ShortCircuit())
            {
                break;
            }
            if (!Booleans())
            {
                break;
            }
            if (!Compares())
            {
                break;
            }
            if (!Equality())
            {
                break;
            }
            if (!WriteMemory())
            {
                break;
            }
            if (!ReadMemory())
            {
                break;
            }
            if (!BitwiseOr())
            {
                break;
            }
            if (!BitwiseAnd())
            {
                break;
            }
            if (!ShiftLeft())
            {
                break;
            }
            if (!ShiftRight())
            {
                break;
            }
            if (!HeapAllocation())
            {
                break;
            }
            break;
        }
        if (cafe == 0xFECA)
        {
            PrintAt(0,0, 'C', 'A', 'F', 'E', '!', char(0), char(0), char(0), char(0), char(0));
        }
        else
        {
            PrintAt(0,0, 'D', 'E', 'A', 'D', '!', char(0), char(0), char(0), char(0), char(0));
        }
    }
}

unit Pages
{
    bool[0x100]   pageLoaded;
    byte[0xFFFF]  pageData; // really should be 0x10000 (but luckily that top byte, 0xFFFF, is ROM)
    
    <string,uint> zeroPage;
    
    // Zero Page FLAGS:
    flags HopperFlags
    {
        TraceOn        = 0x01,
        WarpSpeed      = 0x02,
        CheckedBuild   = 0x04,
        Stack8Bit      = 0x08,
        ProfileBuild   = 0x10,
        BreakpointsSet = 0x20,
        SingleStep     = 0x40,
        MCUPlatform    = 0x80,
    }
    
    bool IsPageLoaded(byte page)
    {
        return pageLoaded[page];
    }
    ClearPageData()
    {
        for (uint i=0; i < 256; i++)
        {
            pageLoaded[i] = false;
        }
        //OutputDebug("ClearPageData()");
    }
    LoadPageData(byte page)
    {
        if (!pageLoaded[page])
        {
            //OutputDebug("Loading: 0x"+page.ToHexString(2));
            Monitor.Command("F" + page.ToHexString(2), true, true);
            if (Pages.ParseHexPage(page))
            {
                pageLoaded[page] = true;
                //OutputDebug("Loaded: 0x"+page.ToHexString(2));
            }
        }
    }
    byte GetPageByte(uint address)
    {
        byte page = byte(address >> 8);
        if (!pageLoaded[page])
        {
            LoadPageData(page);
        }
        return pageData[address];
    }
    uint GetPageWord(uint address)
    {
        byte lsb = GetPageByte(address);
        address++;
        byte msb = GetPageByte(address);
        return lsb + (msb << 8);
    }
    
    
    bool ParseHexLine(uint address, ref string ln)
    {
        bool ok = true;
        loop
        {
            ln = ln.Trim();
            if (ln.Length > 0)
            {
                for (byte i = 0; i < 16; i++)
                {
                    uint index = address + i;
                    string countString = "0x" + ln.Substring(i*2, 2);
                    uint value;
                    if (!UInt.TryParse(countString, ref value)) 
                    {   
                        ok = false;
                        break;
                    }  
                    pageData[index] = byte(value);
                }
            }
            break;
        }
        ln = "";
        return ok;
    }
    bool ParseHexPage(uint page)
    {
        string serialOutput = GetSerialOutput();
        string ln;
        uint address = page << 8;
        foreach (var c in serialOutput)
        {
            if ((c == char(0x0D)) || (c == char(0x0A)))
            {
                if (ln.Length == 0)
                {
                    ln = "";
                }
                else
                {
                    //OutputDebug(address.ToHexString(4) + " " + ln);
                    if (!ParseHexLine(address, ref ln))
                    {
                        //OutputDebug("Failed:" + address.ToHexString(4));
                        return false; // failure
                    }
                    address = address + 16;
                }
            }
            else if (c == ' ')
            {
                // skip
            }
            else
            {
                Build(ref ln, c);
            }
        } // for
        return true; // success
    }
    LoadZeroPage(bool reload)
    {
        zeroPage.Clear();
        bool success = true;
        if (reload)
        {
            Monitor.Command("F00", true, true);
            success = ParseHexPage(0);
            //OutputDebug("ReloadPageData(0x00)");
        }
        if (success)
        {
            byte hopperFlags   = Pages.GetPageByte(0xBB);
            zeroPage["FLAGS"]  = hopperFlags;
            zeroPage["ACC"]    = Pages.GetPageWord(0xC0);
            zeroPage["TOP"]    = Pages.GetPageWord(0xC2);
            zeroPage["NEXT"]   = Pages.GetPageWord(0xC4);
            zeroPage["IDX"]    = Pages.GetPageWord(0xC6);
            zeroPage["IDY"]    = Pages.GetPageWord(0xC8);
            
            zeroPage["PC"]     = Pages.GetPageWord(0xB0);
            if (0 != hopperFlags & is8BitStack)
            {
                zeroPage["SP8"] = Pages.GetPageByte(0xB2);
                zeroPage["BP8"] = Pages.GetPageByte(0xB6);
            }
            else
            {
                zeroPage["SP"]  = Pages.GetPageWord(0xB2);
                zeroPage["TSP"] = Pages.GetPageWord(0xB4);
                zeroPage["BP"]  = Pages.GetPageWord(0xB6);
            }
            zeroPage["CSP"] = Pages.GetPageByte(0xB8);
            zeroPage["HEAPSTART"] = Pages.GetPageByte(0xEA);
            zeroPage["HEAPSIZE"] = Pages.GetPageByte(0xEB);
            zeroPage["FREELIST"] = Pages.GetPageWord(0xE8);
            zeroPage["CODESTART"] = Pages.GetPageByte(0xCA);
            
            //foreach (var kv in zeroPage)
            //{
            //    OutputDebug(kv.key + " = 0x" + kv.value.ToHexString(4));
            //}
        }
    }
    bool ZeroPageContains(string key)
    {
        return zeroPage.Contains(key);
    }
    uint GetZeroPage(string key)
    {
        return zeroPage[key];
    }
    <string, uint> GetZeroPageEntries()
    {
        return zeroPage;
    }
}

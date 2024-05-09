unit Pages
{
    uses "ZeroPage"
    
    bool[0x100]   pageLoaded;
    uint[0x8000]  pageWordData; // because 64K is 0x10000, not 0xFFFF
    
    <string,uint> zeroPage;
    
    bool isLoaded;
    bool IsLoaded { get { return isLoaded; }  set { isLoaded = value; } } // program is loaded
    
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
            else
            {
                OutputDebug("Failed: 0x"+page.ToHexString(2));
            }
        }
        else
        {
            //OutputDebug("Cached: 0x"+page.ToHexString(2));
        }
    }
    byte GetPageByte(uint address)
    {
        byte page = byte(address >> 8);
        if (!pageLoaded[page])
        {
            LoadPageData(page);
        }
        uint word = pageWordData[address >> 1];
        byte b;
        if ((address % 2) == 0)
        {
            b = byte(word & 0xFF); // LSB
        }
        else
        {
            b = byte(word >> 8); // MSB
        }
        //OutputDebug(address.ToHexString(4) + " " + b.ToHexString(2));
        return b;
    }
    uint GetPageWord(uint address)
    {
        return GetPageByte(address) + (GetPageByte(address+1) << 8);
    }
    
    
    bool ParseHexLine(uint address, ref string ln)
    {
        bool ok = true;
        loop
        {
            ln = ln.Trim();
            if (ln.Length != 0)
            {
                for (byte i = 0; i < 8; i++)
                {
                    uint index = address + (i << 1);
                    string countString = "0x" + ln.Substring(i*4+2, 2) + ln.Substring(i*4, 2);
                    uint word;
                    if (!UInt.TryParse(countString, ref word)) 
                    {   
                        ok = false;
                        break;
                    }  
                    //OutputDebug(index.ToHexString(4) + " " + word.ToHexString(4));
                    pageWordData[index >> 1] = word;
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
            if (c == Char.EOL)
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
                        OutputDebug("Failed:" + address.ToHexString(4));
                        OutputDebug(serialOutput);
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
                String.Build(ref ln, c);
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
            if (!success)
            {
                OutputDebug("LoadZeroPage: failed");
                Print("[Z-]");
            }
            //OutputDebug("ReloadPageData(0x00)");
        }
        if (success)
        {
            zeroPage["PC"]        = Pages.GetPageWord(ZP.ZPC);
            //zeroPage["CODESTART"] = Pages.GetPageWord(ZP.ZCODESTART);
            
            zeroPage["SP"]        = Pages.GetPageByte(ZP.ZSP);
            zeroPage["BP"]        = Pages.GetPageByte(ZP.ZBP);
            zeroPage["CSP"]       = Pages.GetPageByte(ZP.ZCSP);
            //zeroPage["CNP"]       = Pages.GetPageByte(ZP.ZCNP);
            
            HopperFlags hf = HopperFlags(Pages.GetPageByte(ZP.ZFLAGS));
            
            zeroPage["FLAGS"]         = uint(hf);
            zeroPage["PLUGNPLAY"]     = Pages.GetPageByte(ZP.ZPLUGNPLAY);
            
            zeroPage["FREELIST"]  = Pages.GetPageWord(ZP.ZFREELIST);
            zeroPage["HEAPSTART"] = Pages.GetPageByte(ZP.ZHEAPSTART) << 8;
            zeroPage["HEAPSIZE"]  = Pages.GetPageByte(ZP.ZHEAPSIZE) << 8;
            
            zeroPage["ACC"]    = Pages.GetPageWord(ZP.ZACC);
            zeroPage["TOP"]    = Pages.GetPageWord(ZP.ZTOP);
            zeroPage["NEXT"]   = Pages.GetPageWord(ZP.ZNEXT);
            zeroPage["IDX"]    = Pages.GetPageWord(ZP.ZIDX);
            zeroPage["IDY"]    = Pages.GetPageWord(ZP.ZIDY);
            
            IsLoaded = (HopperFlags.ProgramLoaded == (hf & HopperFlags.ProgramLoaded));
        }
    }
    bool ZeroPageContains(string key)
    {
        return zeroPage.Contains(key);
    }
    uint GetZeroPage(string key)
    {
        if (zeroPage.Contains(key))
        {
            return zeroPage[key];
        }
        return 0xAA55;
    }
    <string, uint> GetZeroPageEntries()
    {
        return zeroPage;
    }
}

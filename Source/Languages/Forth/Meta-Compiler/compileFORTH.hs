/*

----------------------------------------------------------------------

#   #                                   #####  ###  ####  ##### #   #
#   #  ###  ####  ####   ###  ####      #     #   # #   #   #   #   #
##### #   # #   # #   # ##### #   # ### ###   #   # ####    #   #####
#   # #   # #   # #   # #     #         #     #   # #  #    #   #   #
#   #  ###  ####  ####   #### #         #      ###  #   #   #   #   #
            #     #
            #     #

M E T A - C O M P I L E R

MIT License

Copyright (c) [2024] [Nils "slowcorners" Kullberg]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

// Notes about the Hopper 'port' so far:
//
// - I don't even want to pretend that this will run correctly. I ported it very quickly
//   with the intent of learning some Python and finding weaknesses in Hopper
//
// - Hopper has no tuple support:
//   - for number(), I use the Hopper TryParse style (return true on success, result as ref argument)
//   - for precompiled I simply made two dictionaries : one for the function delegates and one for the flags
//     Not ideal in terms of performance but we'll think of a better way later ...
//   - for the return value of findWord I return a <uint> (list of uints). Works if the members of the tuple are the same type.
//
// - no underscores in identifiers is a bit of a pain when the existing code has tons of them
//
// - in theory, identifiers starting with uppercase are public. It doesn't really matter except for me being very familiar
//   with a certain look and having non-exported symbols as uppercase just looks odd to me. Like I said, doesn't really matter.
//
// - made memSize 32K for now
//
// - Hopper has no exceptions so I replaced the code in 'stop' with a call to Diagnostics.Die(..) after printing the messages.
//   That will result in a Red Screen Of Death with a stack trace after the error messages.
//
// - certain methods like stb(..) and stw(..) had return values that were never consumed. Hopper insists that if a function
//   returns a value (it is an expression), it must be consumed. I removed some of the unused return values
//
// - I would not trust some of the 'signed' arithmetic operations without testing them first.


program CompilerFORTH
{

    #define SERIALCONSOLE
    
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/IO"
    
    enum OpCode // FORTH PRIMARIES
    {
        QRXT = 0,        QRX = 1,         TXSTO = 2,       STIO = 3,
        LIT = 4,         CALL = 5,        NEXT = 6,        QBRAN = 7,
        BRAN = 8,        EXEC = 9,        EXIT = 10,       STORE = 11,
        AT = 12,         CSTOR = 13,      CAT = 14,        RPAT = 15,
        RPSTO = 16,      TOR = 17,        FROMR = 18,      RAT = 19,
        SPAT = 20,       SPSTO = 21,      DROP = 22,       DUP = 23,
        SWAP = 24,       OVER = 25,       ZLESS = 26,      AND = 27,
        OR = 28,         XOR = 29,        UMPLU = 30,      CMOVE = 31,
        
        ONEP = 32,       TWOP = 33,       ONEM = 34,       TWOM = 35,
        ROT = 36,        TDROP = 37,      TDUP = 38,       NIP = 39,
        TUCK = 40,       NOT = 41,        NEG = 42,        DNEG = 43,
        DPLUS = 44,      SUB = 45,        PSTOR = 46,      INCR = 47,
        DECR = 48,       QDUP = 49,       ABS = 50,        ATEXE = 51,
        ZEQU = 52,       EQU = 53,        NEQ = 54,        ULESS = 55,
        LESS = 56,       GREAT = 57,      MAX = 58,        MIN = 59,
        WTHIN = 60,      UMSMO = 61,      MSMOD = 62,      UMSTA = 63,
        MSTAR = 64,      LSHFT = 65,      TWPWR = 66,      RSHFT = 67,
        PICON = 68,      PIFUN = 69,      DIR = 70,        FTOR = 71,
        FFRMR = 72
    }
    
    // ----------------------------------------------------------------------
    // GLOBAL DEFINES
    
    const uint TRUE  = 0xFFFF;
    const uint FALSE = 0x0000;
    
    // Input and output files
    const string forthFTH = "/Source/Languages/FORTH/meta-compiler/FORTH.FTH";
    const string forthPreCompiledHS  = "/Source/Languages/FORTH/meta-compiler/PrecompiledB.hs";
    const string forthPreCompiledLST = "/Source/Languages/FORTH/meta-compiler/Precompiled.lst";
    
    // Sizes of various areas
    const long memSize    = 0x8000;
    const uint bBuf       = 1024;
    const uint UAreaSize  = 80;
    const uint TIBSize    = 82;
    const uint PADSize    = 66;
    const uint stackSize  = 256;
    const uint rstackSize = 256;
    const uint dskBfSize  = bBuf + 10;
    const uint dskBfNum   = 3;
    const uint bootSize   = 40;
    
    // Memory Map
    const uint mmEnd = memSize - 1;
    const uint endBf = mmEnd;
    const uint dskBf = (endBf - (dskBfNum * dskBfSize));
    const uint UA0 = dskBf - UAreaSize;
    const uint TIB0 = UA0 - TIBSize;
    const uint RP0 = TIB0;
    const uint SP0 = RP0 - rstackSize;
    
    // USER VARIABLES
    const uint uVarSTATE = UA0 + 0;   const uint uVarBASE = UA0 + 2;    const uint uVarCONT = UA0 + 4;    const uint uVarCURR = UA0 + 6;
    const uint uVarDP = UA0 + 8;      const uint uVarOUT = UA0 + 10;    const uint uVarHLD = UA0 + 12;    const uint uVarNTIB = UA0 + 14;
    const uint uVarSPAN = UA0 + 16;   const uint uVarINPTR = UA0 + 18;  const uint uVarCSP = UA0 + 20;    const uint uVarHNDLR = UA0 + 22;
    const uint uVarLATES = UA0 + 24;  const uint uVarTMP = UA0 + 26;    const uint uVarVQKEY = UA0 + 28;  const uint uVarVEMIT = UA0 + 30;
    const uint uVarVECHO = UA0 + 32;  const uint uVarVEXPE = UA0 + 34;  const uint uVarVTAP = UA0 + 36;   const uint uVarVPRMP = UA0 + 38;
    const uint uVarVEVAL = UA0 + 40;  const uint uVarVNUMB = UA0 + 42;  const uint uVarSEED = UA0 + 44;   const uint uVarVUNIQ = UA0 + 46;
    const uint uVarNP = UA0 + 48;     const uint uVarVDISP = UA0 + 50;  const uint uVarTASKS = UA0 + 52;  const uint uVarALOAD = UA0 + 54;
    const uint uVarSCRN = UA0 + 56;   const uint uVarPOSN = UA0 + 58;   const uint uVarCURN = UA0 + 60;   const uint uVarLPS = UA0 + 62;
    const uint uVarLINES = UA0 + 64;  const uint uVarUSE = UA0 + 66;    const uint uVarPREV = UA0 + 68;   const uint uVarVUNIQ = UA0 + 70;
    const uint uVarFSYS = UA0 + 72;   const uint uVarFILE = UA0 + 74;   const uint uVarFILE2 = UA0 + 76;  const uint uVarTICKS = UA0 + 78;
    
    // BOOT TABLE
    const uint bootCOLD = 0;          const uint bootWARM = 2;          const uint bootfree5 = 4;         const uint bootfree6 = 6;
    const uint bootCPU = 8;           const uint bootVER = 10;          const uint bootSP0 = 12;          const uint bootRP0 = 14;
    const uint bootUA0 = 16;          const uint bootTIB = 18;          const uint bootFIRST = 20;        const uint bootLIMIT = 22;
    const uint bootCR = 24;           const uint bootNL = 26;           const uint bootBSP = 28;          const uint bootDP0 = 30;
    const uint bootfree1 = 32;        const uint bootfree2 = 34;        const uint bootfree3 = 36;        const uint bootfree4 = 38;
    
    
    const uint CR  = 0x0D0A;
    const uint NL  = 0x000D;
    const uint BSP = 0x0008;
    
    // ----------------------------------------------------------------------
    // GLOBAL VARIABLES\
    
    delegate WordDelegate();
    
    // ROM contents during compilation
    byte[memSize] ram;
    
    // Bit masks for immediate and compile-only words
    const byte immediate = 0x40;
    //const byte compOnly  = 0x20;
    // FORTH machine registers
    uint ip;
    uint wa;
    uint sp;
    uint rp;
    // Line width count when generating output
    uint lw;
    // Line number of FORTH.FTH during meta-compilation
    uint lineNum;
    // Dictionary of precompiled primaries
    <string, WordDelegate> precompiledFunctions;
    <string, byte> precompiledFlags;
    // A list of messages from the compilation
    <string> msgs;
    
    <OpCode, WordDelegate> primaries;
    
    consume(uint argument) {}
    
    // ----------------------------------------------------------------------
    // Lower level help functions

    nop()
    {
    }

    byte ldb(uint addr)
    {
        return ram[addr];
    }
    stb(uint addr, byte data)
    {
        // if data > 255 or data < 0: stop('stb(): <%d> does not fit into byte' % data)
        ram[addr] = data & 0xFF;
    }

    uint ldw(uint addr)
    {
        return ldb(addr) + (ldb(addr + 1) << 8);
    }

    stw(uint addr, uint data)
    {
        stb(addr, byte(data & 0xFF));
        stb(addr + 1, byte(data >> 8));
    }
    
    push(uint w)
    {
        sp -= 2; stw(sp, w);
    }

    uint pop()
    {
        uint w = ldw(sp); sp += 2;
        return w;
    }

    uint nth(uint n)
    {
        return ldw(sp + (n << 1));
    }

    uint top()
    {
        return ldw(sp);
    }

    rpush(uint w)
    {
        rp -= 2; stw(rp, w);
    } 

    uint rpop()
    {
        uint w = ldw(rp); rp += 2;
        return w;
    }

    uint rnth(uint n)
    {
        return ldw(rp + (n << 1));
    }

    uint rtop()
    {
        return ldw(rp);
    }

    uint imm()
    {
        uint tmp = ldw(ip); ip += 2;
        return tmp;
    }
    
    // ----------------------------------------------------------------------
    // The Virtual Machine (the actual functions run by the primaries)
    // ----------------------------------------------------------------------

    qrxt()
    {
        consume(pop()); push(0);
    }

    qrx()
    {
        push(0);
    }

    txsto()
    {
        IO.Write(char(pop())); // Serial or Console depending on SERIALCONSOLE
    }

    stio()
    {
    }

    lit()
    {
        push(imm());
    }

    call()
    {
        rpush(ip);
        ip = wa;
    }

    next()
    {
        uint tmp = rtop() - 1;
        if (tmp >= 0)
        {
            stw(rp, tmp);
            ip = ldw(ip);
        }
        else
        {
            rp += 2; ip += 2;
        }
    }

    qbranch()
    {
        if (pop() == 0) { ip = ldw(ip); }
        else { ip += 2; }
    }

    branch()
    {
        ip = ldw(ip);
    }

    EXIT()
    {
        ip = rpop();
    }

    EXEC()
    {
        uint wa = pop();
        uint tmp = ldb(wa); wa++;
        WordDelegate wordDelegate = primaries[OpCode(tmp)];
        wordDelegate();
    }

    STORE()
    {
        uint addr = pop();
        stw(addr, pop());
    }

    AT()
    {
        push(ldw(pop()));
    }

    CSTOR()
    {
        uint addr = pop();
        stb(addr, byte(pop() & 0xFF));
    }

    CAT()
    {
        push(ldb(pop()));
    }

    RPAT()
    {
        push(rp);
    }

    RPSTO()
    {
        rp = pop();
    }

    TOR()
    {
        rpush(pop());
    }

    FROMR()
    {
        push(rpop());
    }

    RAT()
    {
        push(rtop());
    }

    SPAT()
    {
        push(sp);
    }

    SPSTO()
    {
        sp = pop();
    }

    DROP()
    {
        sp += 2;
    }

    DUP()
    {
        push(top());
    }

    SWAP()
    {
        uint a = pop(); uint b = pop();
        push(a); push(b);
    }

    OVER()
    {
        push(nth(1));
    }

    ZLESS()
    {
        if ((pop() & 0x8000) != 0) { push(TRUE); }
        else { push(FALSE); }
    }

    AND()
    {
        push(pop() & pop());
    }

    OR()
    {
        push(pop() | pop());
    }

    XOR()
    {
        push(pop() ^ pop());
    }

    UMPLU()
    {
        long sum = pop() + pop();
        push(sum.GetByte(0) + (sum.GetByte(1) << 8));
        push(sum.GetByte(2) + (sum.GetByte(3) << 8));
    }

    PLUS()
    {
        UMPLU(); DROP();
    }

    CMOVE()
    {
        uint cnt = pop(); uint trg = pop(); uint src = pop();
        if ((trg == src) || (cnt == 0)) { return; }
        if (trg < src)
        {
            while (cnt != 0)
            {
                stb(trg, ldb(src));
                trg++; src++; cnt--;
            }
        }
        else
        {
            src += cnt; trg += cnt;
            while (cnt != 0)
            {
                trg--; src--; cnt--;
                stb(trg, ldb(src));
            }
        }
    }

    ZERO()
    {
        push(0);
    }

    ONE()
    {
        push(1);
    }

    TWO()
    {
        push(2);
    }

    MONE()
    {
        push(0xFFFF); // -1
    }

    MTWO()
    {
        push(0xFFFE); // -2
    }

    ONEP()
    {
        ONE(); PLUS();
    }

    TWOP()
    {
        TWO(); PLUS();
    }

    ONEM()
    {
        MONE(); PLUS();
    }

    TWOM()
    {
        MTWO(); PLUS();
    }

    ROT()
    {
        uint tmp = ldw(sp + 4);
        stw(sp + 4, ldw(sp + 2));
        stw(sp + 2, ldw(sp));
        stw(sp, tmp);
    }

    TDROP()
    {
        sp += 4;
    }

    TDUP()
    {
        OVER(); OVER();
    }

    NIP()
    {
        SWAP(); DROP();
    }

    TUCK()
    {
        SWAP(); OVER();
    }

    NOT()
    {
        stw(sp, ldw(sp) ^ 0xFFFF);
    }

    NEG()
    {
        NOT(); ONEP();
    }

    DNEG()
    {
        NOT(); TOR(); NOT(); ONE(); UMPLU(); FROMR(); PLUS();
    }

    DPLUS()
    {
        TOR(); SWAP(); TOR(); UMPLU(); FROMR(); FROMR(); PLUS(); PLUS();
    }

    SUB()
    {
        NEG(); PLUS();
    }

    PSTOR()
    {
        DUP(); AT(); ROT(); PLUS(); SWAP(); STORE();
    }

    INCR()
    {
        DUP(); AT(); ONEP(); SWAP(); STORE();
    }

    DECR()
    {
        DUP(); AT(); ONEM(); SWAP(); STORE();
    }

    QDUP()
    {
        if (top() != 0) { DUP(); }
    }

    ABS()
    {
        DUP(); ZLESS();
        if (pop() != 0) { NEG();}
    }

    ATEXE()
    {
        AT(); QDUP();
        if (pop() != 0)
        {
            EXEC();
        }
    }
    ZEQU()
    {
        if (pop() == 0) { push(TRUE); }
        else { push(FALSE); }
    }

    EQU()
    {
        SUB(); ZEQU();
    }

    NEQ()
    {
        EQU(); ZEQU();
    }

    ULESS()
    {
        uint tmp = pop();
        if (pop() < tmp) { push(TRUE); }
        else { push(FALSE); }
    }

    LESS()
    {
        SUB(); ZLESS();
    }

    GREAT()
    {
        SWAP(); LESS();
    }

    MAX()
    {
        uint tmp = pop();
        if (top() < tmp) { stw(sp, tmp); }
    }

    MIN()
    {
        uint tmp = pop();
        if (top() > tmp) { stw(sp, tmp); }
    }

    WTHIN()
    {
        OVER(); SUB(); TOR(); SUB(); FROMR(); ULESS();
    }

    UMSMO()
    {
        uint second = pop();
        long first = pop() << 16;
        first += pop();
        long mod = first % second;
        long div = first / second;
        push(mod.GetByte(0) + (mod.GetByte(1) << 8));
        push(div.GetByte(0) + (div.GetByte(1) << 8));
   }

    MSMOD()
    {
        DUP(); ZLESS(); DUP(); TOR();
        if (pop() != 0)
        {
            NEG(); TOR(); DNEG(); FROMR();
        }
        TOR(); DUP(); ZLESS();
        if (pop() != 0)
        {
            RAT(); PLUS();
        }
        FROMR(); UMSMO(); FROMR();
        if (pop() != 0)
        {
            SWAP(); NEG(); SWAP();
        }
    }

    UMSTA()
    {
        long prod = pop() * pop();
        push(prod.GetByte(0) + (prod.GetByte(1) << 8));
        push(prod.GetByte(2) + (prod.GetByte(3) << 8));
    }

    MSTAR()
    {
        uint first = pop();
        uint second = pop();
        bool sign1 = (first & 0x8000) != 0;
        bool sign2 = (second & 0x8000) != 0;
        long prod = first * second;
        if ((sign1 && !sign2) || (sign2 && !sign1)) { prod = -prod; }
        
        push(prod.GetByte(0) + (prod.GetByte(1) << 8));
        push(prod.GetByte(2) + (prod.GetByte(3) << 8));
    }

    LSHFT()
    {
        uint steps = pop();
        stw(sp, ldw(sp) << steps);
    }

    TWPWR()
    {
        stw(sp, 1 << ldw(sp));
    }

    RSHFT()
    {
        uint steps = pop();
        stw(sp, ldw(sp) >> steps);
    }

    // Pico-specific primaries

    PICON()
    {
    }

    PIFUN()
    {
    }

    DIR()
    {
    }

    FTOR()
    {
    }

    FFRMR()
    {
    }

    SEEK()
    {
    }

    RW()
    {
    }

    // ----------------------------------------------------------------------
    // INNER INTERPRETER
    
    // primaries[OPCODE]()
    initPrimaries()
    {
        WordDelegate 
        dl = qrxt;     primaries[OpCode.QRXT]  = dl;
        dl = qrx;      primaries[OpCode.QRX]   = dl;
        dl = txsto;    primaries[OpCode.TXSTO] = dl;
        dl = stio;     primaries[OpCode.STIO]  = dl;
        dl = lit;      primaries[OpCode.LIT]   = dl;
        dl = call;     primaries[OpCode.CALL]  = dl;
        dl = next;     primaries[OpCode.NEXT]  = dl;
        dl = qbranch;  primaries[OpCode.QBRAN] = dl;
        dl = branch;   primaries[OpCode.BRAN]  = dl;
        dl = EXEC;     primaries[OpCode.EXEC]  = dl;
        dl = EXIT;     primaries[OpCode.EXIT]  = dl;
        dl = STORE;    primaries[OpCode.STORE] = dl;
        dl = AT;       primaries[OpCode.AT]    = dl;
        dl = CSTOR;    primaries[OpCode.CSTOR] = dl;
        dl = CAT;      primaries[OpCode.CAT]   = dl;
        dl = RPAT;     primaries[OpCode.RPAT]  = dl;
        dl = RPSTO;    primaries[OpCode.RPSTO]  = dl;
        dl = TOR;      primaries[OpCode.TOR]  = dl;
        dl = FROMR;    primaries[OpCode.FROMR]  = dl;
        dl = RAT;      primaries[OpCode.RAT]  = dl;
        dl = SPAT;     primaries[OpCode.SPAT]  = dl;
        dl = SPSTO;    primaries[OpCode.SPSTO]  = dl;
        dl = DROP;     primaries[OpCode.DROP]  = dl;
        dl = DUP;      primaries[OpCode.DUP]  = dl;
        dl = SWAP;     primaries[OpCode.SWAP]  = dl;
        dl = OVER;     primaries[OpCode.OVER]  = dl;
        dl = ZLESS;    primaries[OpCode.ZLESS]  = dl;
        dl = AND;      primaries[OpCode.AND]  = dl;
        dl = OR;       primaries[OpCode.OR]  = dl;
        dl = XOR;      primaries[OpCode.XOR]  = dl;
        dl = UMPLU;    primaries[OpCode.UMPLU]  = dl;
        dl = CMOVE;    primaries[OpCode.CMOVE]  = dl;
        dl = ONEP;     primaries[OpCode.ONEP]  = dl;
        dl = TWOP;     primaries[OpCode.TWOP]  = dl;
        dl = ONEM;     primaries[OpCode.ONEM]  = dl;
        dl = TWOM;     primaries[OpCode.TWOM]  = dl;
        dl = ROT;      primaries[OpCode.ROT]  = dl;
        dl = TDROP;    primaries[OpCode.TDROP]  = dl;
        dl = TDUP;     primaries[OpCode.TDUP]  = dl;
        dl = NIP;      primaries[OpCode.NIP]  = dl;
        dl = TUCK;     primaries[OpCode.TUCK]  = dl;
        dl = NOT;      primaries[OpCode.NOT]  = dl;
        dl = NEG;      primaries[OpCode.NEG]  = dl;
        dl = DNEG;     primaries[OpCode.DNEG]  = dl;
        dl = DPLUS;    primaries[OpCode.DPLUS]  = dl;
        dl = SUB;      primaries[OpCode.SUB]  = dl;
        dl = PSTOR;    primaries[OpCode.PSTOR]  = dl;
        dl = INCR;     primaries[OpCode.INCR]  = dl;
        dl = DECR;     primaries[OpCode.DECR]  = dl;
        dl = QDUP;     primaries[OpCode.QDUP]  = dl;
        dl = ABS;      primaries[OpCode.ABS]  = dl;
        dl = ATEXE;    primaries[OpCode.ATEXE]  = dl;
        dl = ZEQU;     primaries[OpCode.ZEQU]  = dl;
        dl = EQU;      primaries[OpCode.EQU]  = dl;
        dl = NEQ;      primaries[OpCode.NEQ]  = dl;
        dl = ULESS;    primaries[OpCode.ULESS]  = dl;
        dl = LESS;     primaries[OpCode.LESS]  = dl;
        dl = GREAT;    primaries[OpCode.GREAT]  = dl;
        dl = MAX;      primaries[OpCode.MAX]  = dl;
        dl = MIN;      primaries[OpCode.MIN]  = dl;
        dl = WTHIN;    primaries[OpCode.WTHIN]  = dl;
        dl = UMSMO;    primaries[OpCode.UMSMO]  = dl;
        dl = MSMOD;    primaries[OpCode.MSMOD]  = dl;
        dl = UMSTA;    primaries[OpCode.UMSTA]  = dl;
        dl = MSTAR;    primaries[OpCode.MSTAR]  = dl;
        dl = LSHFT;    primaries[OpCode.LSHFT]  = dl;
        dl = TWPWR;    primaries[OpCode.TWPWR]  = dl;
        dl = RSHFT;    primaries[OpCode.RSHFT]  = dl;
        dl = PICON;    primaries[OpCode.PICON]  = dl;
        dl = PIFUN;    primaries[OpCode.PIFUN]  = dl;
        dl = DIR;      primaries[OpCode.DIR]  = dl;
        dl = FTOR;     primaries[OpCode.FTOR]  = dl;
        dl = FFRMR;    primaries[OpCode.FFRMR]  = dl;
    }

    inner()
    {
        loop
        {
            wa = ldw(ip); ip += 2;
            if (wa == 0xFFFF) { break; }
            OpCode opcode = OpCode(ldb(wa)); wa++;
            WordDelegate wordDelegate = primaries[opcode];
            wordDelegate();
        }
    }
    
    // ----------------------------------------------------------------------
    // TEMPORARY FORTH WORDS (USED IN THE BEGINNING OF META-COMPILATION)

    HEAD()
    {
        string name = token();
        if (name.Length == 0) { stop("HEAD(): Missing token"); }
        push(ldw(ldw(uVarCURR))); COMMA(); // Link to latest word in CURRENT vocabulary
        stw(uVarLATES, ldw(uVarDP));       // Latest now to point to this header
        push(name.Length | 0x80); CCOMM();    // Length byte
        foreach (var ch in name)
        {
            push(byte(ch)); CCOMM();    // Actual FORTH word name
        }
    }

    BSLAS()
    {
        stw(uVarINPTR, ldw(uVarNTIB));
    }

    CCOMM()
    {
        stb(ldw(uVarDP), byte(pop()));
        stw(uVarDP, ldw(uVarDP) + 1);
    }

    COMMA()
    {
        stw(ldw(uVarDP), pop());
        stw(uVarDP, ldw(uVarDP) + 2);
    }

    CALLC()
    {
        push(uint(OpCode.CALL)); CCOMM();
    }

    OVERT()
    {
        stw(ldw(uVarCURR), ldw(uVarLATES));
    }

    COLON()
    {
        HEAD(); CALLC();
        stw(uVarSTATE, TRUE);
    }

    SEMIC()
    {
        push(mustFind("EXIT")); COMMA();
        stw(uVarSTATE, FALSE);
        OVERT();
    }

    CREAT()
    {
        HEAD(); CALLC(); OVERT();
    }

    CONST()
    {
        CREAT(); push(mustFind("con")); COMMA(); COMMA();
    }

    USER()
    {
        CREAT(); push(mustFind("con")); COMMA(); push(UA0); PLUS(); COMMA();
    }

    VAR()
    {
        CREAT(); push(mustFind("var")); COMMA(); push(0); COMMA();
    }

    BCOMP()
    {
        push(mustFind(token())); COMMA();
    }

    DOTH()
    {
        uint h = pop();
        Write(" " + h.ToHexString(4) + " ");
    }

    DOTD()
    {
        uint d = pop();
        Write(" " + d.ToString() + " ");
    }

    temp(string name, WordDelegate function, byte flgs)
    {
        precompiledFunctions[name] = function;
        precompiledFlags[name] = flgs;
    }
    temp(string name, WordDelegate function)
    {
        temp(name, function, 0);
    }

    STOP()
    {
        stop("STOP encountered");
    }

    precompile()
    {
        WordDelegate 
        dl = HEAD; temp("HEAD", dl);
        dl = BSLAS; temp("\\", dl, immediate);
        dl = OVERT; temp("OVERT", dl);
        dl = CCOMM; temp("C,", dl);
        dl = COMMA; temp(",", dl);
        dl = CALLC; temp("call,", dl);
        dl = COLON; temp(":", dl);
        dl = SEMIC; temp(";", dl, immediate);
        dl = CREAT; temp("CREATE", dl);
        dl = CONST; temp("CONSTANT", dl);
        dl = USER; temp("USER", dl);
        dl = VAR; temp("VARIABLE", dl);
        dl = BCOMP; temp("[COMPILE]", dl, immediate);
        dl = DOTH; temp(".H", dl);
        dl = DOTD; temp(".D", dl);
        dl = STOP; temp("STOP", dl);
    }
    
    // ----------------------------------------------------------------------
    // THE ACTUAL META-COMPILER

    stop(string txt)
    {
        msgs.Append(txt);
        foreach (var msg in msgs)
        {
            WriteLn(msg);
        }
        Diagnostics.Die(0x0B);
    }

    string token()
    {
        string tok;
        
        // Skip leading blanks
        loop
        {
            byte ch = ldb(TIB0 + ldw(uVarINPTR));
            if (ch == 0)  { return tok; }
            if (ch != 32) { break; }
            stw(uVarINPTR, ldw(uVarINPTR) + 1);
        }
        // Collect the non-blank token
        loop
        {
            char ch = char(ldb(TIB0 + ldw(uVarINPTR)));
            if ((ch == char(0)) || (ch == ' '))
            {
                if (ch == ' ') { stw(uVarINPTR, ldw(uVarINPTR) + 1); }
                break;
            }
            tok = tok + ch;
            stw(uVarINPTR, ldw(uVarINPTR) + 1);
        }
        return tok;
    }

    bool number(string tok, ref uint value)
    {
        bool isNum;
        string prefix = (ldw(uVarBASE) == 16) ? "0x" : "";
        bool nega;
    
    
        // If number starts with $ or - or any combination of the two
        // then treat as hex and/or negative
        if (tok.StartsWith('$'))
        {
            prefix = "0x";
            tok = tok.Substring(1);
        }
        if (tok.StartsWith('-'))
        {
            nega = true;
            tok = tok.Substring(1);
        }
        if (tok.StartsWith('-'))
        {
            prefix = "0x";
            tok = tok.Substring(1);
        }
        long lvalue;
        if (Long.TryParse(prefix + tok,  ref lvalue))
        {
            isNum = true;
            if (nega) { lvalue = -lvalue; }
            value = lvalue.GetByte(0) + (lvalue.GetByte(1) << 8);
        }
        else
        {
            // ???
            // stop('number(): [%s] is not a number' % tok)
        }
        return isNum;
    }

    <uint> findWord(string name)
    {
        <uint> tuple;
        uint wrk = ldw(ldw(uVarCONT));
        loop
        {
            if (wrk == 0) { break; }
            if ((ldb(wrk) & 0x1F) == name.Length)
            {
                uint wrk2 = wrk + 1;
                bool wasBreak;
                for (uint i=0; i < name.Length; i++)
                {
                    if (char(ldb(wrk2 + i)) != name[i]) { wasBreak = true; break; }
                }
                if (!wasBreak) // Python for .. else
                {
                    tuple.Append(wrk2 + name.Length);
                    tuple.Append(ldb(wrk));
                    return tuple;
                }
            }
            wrk = ldw(wrk - 2);
        }
        wrk = ldw(ldw(uVarCURR));
        loop
        {
            if (wrk == 0) { return tuple; }
            if ((ldb(wrk) & 0x1F) == name.Length)
            {
                uint wrk2 = wrk + 1;
                bool wasBreak;
                for (uint i=0; i < name.Length; i++)
                {
                    if (char(ldb(wrk2 + i)) != name[i]) { wasBreak = true; break; }
                }
                if (!wasBreak) // Python for .. else
                {
                    tuple.Append(wrk2 + name.Length);
                    tuple.Append(ldb(wrk));
                    return tuple;
                }
            }
            wrk = ldw(wrk - 2);
        }
        return tuple; // unreachable
    }
    
    uint mustFind(string name)
    {
        <uint> tuple = findWord(name);
        if (tuple.Length == 0)
        {
            stop("Cannot find " + name);
        }
        return tuple[0];
    }

    bool findPrecompiled(string tok, ref WordDelegate primFunc, ref byte primFuncFlags)
    {
        if (precompiledFunctions.Contains(tok))
        {
            primFunc      = precompiledFunctions[tok];
            primFuncFlags = precompiledFlags[tok];
            return true;
        }
        return false;
    }

    runWord(uint addr)
    {
        rpush(ip); rpush(0xFFFF); rpush(addr);
        ip = rp; inner();
        consume(rpop()); consume(rpop()); ip = rpop();
    }

    eval(string tok)
    {
        <uint> tuple = findWord(tok);
        if (tuple.Length == 2)
        {
            // Name found in FORTH vocabulary
            uint addr = tuple[0];
            uint lb = tuple[1];
            if ((ldw(uVarSTATE) == FALSE) || ((lb & immediate) != 0))
            {
                runWord(addr);
            }
            else
            {
                push(addr); COMMA();
            }
        }
        else
        {
            WordDelegate primFunc;
            byte primFuncFlags;
            if (findPrecompiled(tok, ref primFunc, ref primFuncFlags))
            {
                // Name found within precompiled words
                if ((ldw(uVarSTATE) == FALSE) || ((primFuncFlags & immediate) != 0))
                {
                    primFunc();
                }
                else
                {
                    stop("eval(): Cannot compile precompiled " + tok);
                }
            }
            else
            {
                // Word not found, try to convert to number
                uint value;
                if (number(tok, ref value))
                {
                    push(value);
                    if (ldw(uVarSTATE) != 0)
                    {
                        push(mustFind("lit")); COMMA();
                        COMMA();
                    }
                }
                else
                {
                    stop("eval(): Unknown word " + tok);
                }
            }
        }
    }

    handleLine(string txt)
    {
        uint txtLen = txt.Length;
        if (txtLen > 80)
        {
            stop("handle_line(): Line too long");
        }
        // Copy input line to TIB
        for (uint i=0; i < txtLen; i++)
        {
            stb(TIB0 + i, byte(txt[i]));
        }
        stw(TIB0 + txtLen, 0);
        stw(uVarNTIB, txtLen);
        stw(uVarINPTR, 0);
        loop
        {
            string tok = token();
            if (tok.Length == 0) { return; }
            eval(tok);
        }
    }

    compileFORTH()
    {
        // Get FORTH.FTH
        <string> sourceFile;
        file src = File.Open(forthFTH);
        loop
        {
            string ln = src.ReadLine();
            if (!src.IsValid()) { break; }
            sourceFile.Append(ln);
        }
        
        // Initialize the FORTH virtual machine
        sp = SP0;
        rp = RP0;
        
        stw(uVarDP, 40);
        stw(uVarSTATE, 0);
        stw(uVarBASE, 10);
        stw(uVarNP, 0);
        stw(uVarCONT, uVarNP);
        stw(uVarCURR, uVarNP);
        stw(uVarVUNIQ, 0);
        // BOOT TABLE
        stw(bootSP0, SP0);
        stw(bootRP0, RP0);
        stw(bootUA0, UA0);
        stw(bootTIB, TIB0);
        stw(bootFIRST, dskBf);
        stw(bootLIMIT, endBf);
        stw(bootCR, CR);
        stw(bootNL, NL);
        stw(bootBSP, BSP);
        // Precompile temporary words for meta-compilation
        precompile();
        // Compile FORTH.FTH
        foreach (var sourceLine in sourceFile)
        {
            lineNum++;
            handleLine(sourceLine);
        }
    }
        
        
    // ----------------------------------------------------------------------
    // OUTPUT FILE GENERATION

    generateHSFile()
    {
        uint loc;
        uint highLoc = ldw(uVarDP);
        file objFile = File.Create(forthPreCompiledHS);
        
        // Contents of the FORTH RAM
        objFile.Append("unit Precompiled {" + char(0x0A));
        objFile.Append("const string PRECOMPILED = {" + char(0x0A));
        objFile.Append("0x" + (ldw(uVarDP) & 0xFF).ToHexString(2) + ", 0x" + (ldw(uVarDP) >> 8).ToHexString(2) + ", " + char(0x0A));
        while (loc < highLoc)
        {
            if (loc % 16 == 0) { objFile.Append("" + char(0x0A)); }
            objFile.Append("0x" + (ram[loc]).ToHexString(2) + ", ");
            loc++;
        }
        objFile.Append(char(0x0A) + "};}" + char(0x0A));
        
        // All done
        objFile.Flush();
    }

    string FORTHname(uint addr)
    {
        string fName = "";
        uint range = ldb(addr) & 0x1F;
        for (uint i = 0; i < range; i++)
        {
            fName = fName + char(ldb(addr + i + 1));
        }
        return fName;
    }

    string dump(uint addr, uint n)
    {
        uint i;
        string du;
        while (i < n)
        {
            du = du + char(0x0A);
            du = du + (addr + i).ToHexString(4) + " ";
            for (uint j = 0; j < 16; j++)
            {
                du = du + (ldb(addr + i + j)).ToHexString(2) + " ";
            }
            du = du + " ";
            for (uint j = 0; j < 16; j++)
            {
                byte ch = ldb(addr + i + j);
                if ((ch > 31) && (ch < 127)) { du = du + char(ch); }
                else { du = du + '.'; }
            }
            i += 16;
        }
        du = du + char(0x0A);
        return du;
    }

    listWords()
    {
        uint nwds;
        file lstFile = File.Create(forthPreCompiledLST);
        uint wrk = ldw(ldw(uVarCONT));
        lstFile.Append("" + char(0x0A));
        WriteLn();
        foreach (var msg in msgs)
        {
            lstFile.Append(msg + char(0x0A));
            WriteLn(msg);
        }
        while (wrk != 0)
        {
            if ((nwds % 3) == 0)
            {
                lstFile.Append("" + char(0x0A));
                WriteLn();
            }
            string fName = FORTHname(wrk);
            lstFile.Append(fName.Pad(' ', 14) + wrk.ToHexString(4));
            Write(fName.Pad(' ', 14) + wrk.ToHexString(4));
            wrk = ldw(wrk - 2);
            nwds++;
        }
        WriteLn();
        lstFile.Append(char(0x0A) + nwds.ToString() + " words." + char(0x0A) + dump(0, ldw(uVarDP))+ char(0x0A));
        Write(char(0x0A) + nwds.ToString() + " words." + char(0x0A) + dump(0, ldw(uVarDP))+ char(0x0A));
        foreach(var msg in msgs)
        {
            lstFile.Append(msg + char(0x0A));
            WriteLn(msg);
        }
        lstFile.Append("Total ROM size: " + (ldw(uVarDP)).ToString() +" bytes." + char(0x0A));
        WriteLn("Total ROM size: " + (ldw(uVarDP)).ToString() +" bytes.");
        lstFile.Flush();
    }
    
    // ----------------------------------------------------------------------
    //   " M A I N   P R O G R A M "
    {
        initPrimaries();   
        compileFORTH();
        msgs.Append("Reached end of FORTH.FTH");
        msgs.Append("Compilation finished with no exceptions.");
        generateHSFile();
        listWords();
    }
}


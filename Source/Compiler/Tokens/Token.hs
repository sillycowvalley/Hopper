unit Token
{
    enum HopperToken
    {
        Undefined,
        Error,

        DottedIdentifier, // Key.Enter, Keyboard.Key.Enter
        Identifier,       // privateIndentifier, PublicIdentifier
        Discarder,        // _
        Dot,
        Keyword,          // "system"
        Directive,        // #ifdef
        LabelIdentifier,  // <label>: for assembly

        Bool,    // true/false
        Char,    // ' '
        Integer, // 32 bit signed
        Float,   // 3.414

        StringConstant,  // "Hello World!"

        SemiColon,
        Comma,
        Colon,    // case x:
        Question, // (xxx) ? yyy : zzz

        LParen,   // (
        RParen,   // )
        LBrace,   // {
        RBrace,   // }
        LBracket, // [
        RBracket, // ]
        
        Hash, // #

        LT, // <
        GT, // >
        EQ, // ==
        LE, // <=
        GE, // >=
        NE, // !=

        ShiftLeft, // <<
        ShiftRight, // >>

        BooleanAnd, // &&
        BooleanOr,  // ||
        BooleanNot, // !

        BitAnd, // &
        BitOr,  // |
        BitNot, // ~
        BitXor, // ^

        Add,      // +
        Subtract, // -
        Multiply, // *
        Divide,   // /
        Modulus,  // %

        Increment, // ++;
        Decrement, // --

        Assign,         // =
        AssignAdd,      // +=
        AssignSubtract, // -=
        AssignMultiply, // *=
        AssignDivide,   // /=
        AssignModulus,  // %=
        AssignBitAnd,   // &=
        AssignBitOr,    // |=
        
        Instruction,
        Register,
        Condition,
        
        EOF
    }
    bool IsDelimiter(HopperToken tokenType)
    {
        switch (tokenType)
        {
            case HopperToken.LParen:   // (
            case HopperToken.RParen:   // )
            case HopperToken.LBrace:   // {
            case HopperToken.RBrace:   // }
            case HopperToken.LBracket: // [
            case HopperToken.RBracket: // ]
            
            case HopperToken.SemiColon:
            case HopperToken.Comma:
            case HopperToken.Dot:
            case HopperToken.Colon:    // case x:
            case HopperToken.Question: // (xxx) ? yyy : zzz
            
            case HopperToken.Hash:     // #
            {
                return true;
            }
        }
        return false;
    }
    string ToString(HopperToken tokenType)
    {
        switch (tokenType)
        {
            case HopperToken.LParen:    { return "("; }
            case HopperToken.RParen:    { return ")"; }
            case HopperToken.LBrace:    { return "{"; }
            case HopperToken.RBrace:    { return "}"; }
            case HopperToken.LBracket:  { return "["; }
            case HopperToken.RBracket:  { return "]"; }
            
            case HopperToken.SemiColon: { return ";"; }
            case HopperToken.Comma:     { return ","; }
            case HopperToken.Dot:
            case HopperToken.Colon:     { return ":"; }
            case HopperToken.Question:  { return "?"; }            
            case HopperToken.Hash:      { return "#"; }
        }
        return hopperTokenNames[tokenType];
    }
    
    flags CPUArchitecture
    {
        None    = 0x0000,
        Hopper  = 0x0001,
        M6502   = 0x0010, // original MOS instruction set
        W65C02  = 0x0030, // MOS set plus WDC / Rockwell set
        Z80     = 0x0100,
        M6809   = 0x1000,
    }
    
    string HopperTokenToString(HopperToken tokenType)
    {
        // <HopperToken, string> hopperTokenNames;
        string content;
        uint i;
        if (hopperTokenNames.Contains(tokenType))
        {
            content = hopperTokenNames[tokenType];
        }
        else
        {
            i = uint(tokenType);
            content = "0x" + i.ToHexString(4);
            PrintLn("TODO : token not implemented in Token.HopperTokenToString");
        }
        return content;
    }
    
    
    HopperToken GetType(<string, string> token)
    {
        HopperToken tokenType = HopperToken.Undefined;
        if (token.Contains("type"))
        {
            // <string, HopperToken> hopperTokenTypes;
            string name = token["type"];
            
            if (hopperTokenTypes.Contains(name))
            {
                tokenType = hopperTokenTypes[name];
            }
            else
            {
                PrintLn("TODO : token not implemented in Token.GetType");
            }
        }
        return tokenType;
    }

    <string,bool> typeKeywords;           // blue
    <string,bool> statementKeywords;      // purple
    <string,bool> directiveKeywords;      // dark gray
    
    <string,bool> instructionKeywords;
    <string,bool> registerKeywords;
    <string,bool> conditionKeywords;
    
    <string, HopperToken> hopperTokenTypes;
    <HopperToken, string> hopperTokenNames;
    
    Initialize()
    {
        typeKeywords.Clear();
        statementKeywords.Clear();
        directiveKeywords.Clear();
        
        instructionKeywords.Clear();
        registerKeywords.Clear();
        conditionKeywords.Clear();
        
        hopperTokenTypes.Clear();
        hopperTokenNames.Clear();
        
        hopperTokenTypes["EOF"] = HopperToken.EOF;
        hopperTokenTypes["Undefined"] = HopperToken.Undefined;
        hopperTokenTypes["Error"] = HopperToken.Error;
        hopperTokenTypes["Char"] = HopperToken.Char;
        hopperTokenTypes["Bool"] = HopperToken.Bool;
        hopperTokenTypes["Keyword"] = HopperToken.Keyword;
        hopperTokenTypes["Directive"] = HopperToken.Directive;
        hopperTokenTypes["Identifier"] = HopperToken.Identifier;
        hopperTokenTypes["Discarder"]  = HopperToken.Discarder;
        hopperTokenTypes["DottedIdentifier"] = HopperToken.DottedIdentifier;
#ifdef ASSEMBLER        
        hopperTokenTypes["LabelIdentifier"] = HopperToken.LabelIdentifier;
#endif
        hopperTokenTypes["Integer"] = HopperToken.Integer;
        hopperTokenTypes["Float"] = HopperToken.Float;
        hopperTokenTypes["StringConstant"] = HopperToken.StringConstant;
        hopperTokenTypes["Assign"] = HopperToken.Assign;
        
        hopperTokenTypes["AssignAdd"]      = HopperToken.AssignAdd;
        hopperTokenTypes["AssignSubtract"] = HopperToken.AssignSubtract;
        hopperTokenTypes["AssignMultiply"] = HopperToken.AssignMultiply;
        hopperTokenTypes["AssignDivide"]   = HopperToken.AssignDivide;
        hopperTokenTypes["AssignModulus"]   = HopperToken.AssignModulus;
        hopperTokenTypes["AssignBitAnd"]   = HopperToken.AssignBitAnd;
        hopperTokenTypes["AssignBitOr"]   = HopperToken.AssignBitOr;
        
        hopperTokenTypes["BitAnd"] = HopperToken.BitAnd;
        hopperTokenTypes["BitOr"] = HopperToken.BitOr;
        hopperTokenTypes["LParen"] = HopperToken.LParen;
        hopperTokenTypes["RParen"] = HopperToken.RParen;
        hopperTokenTypes["LBrace"] = HopperToken.LBrace;
        hopperTokenTypes["RBrace"] = HopperToken.RBrace;
        hopperTokenTypes["LBracket"] = HopperToken.LBracket;
        hopperTokenTypes["RBracket"] = HopperToken.RBracket;
        hopperTokenTypes["Hash"] = HopperToken.Hash;
        
        hopperTokenTypes["SemiColon"] = HopperToken.SemiColon;
        hopperTokenTypes["Colon"] = HopperToken.Colon;
        hopperTokenTypes["Question"] = HopperToken.Question;
        hopperTokenTypes["Comma"] = HopperToken.Comma;
        hopperTokenTypes["Dot"] = HopperToken.Dot;
        hopperTokenTypes["Add"] = HopperToken.Add;
        hopperTokenTypes["Subtract"] = HopperToken.Subtract;
        hopperTokenTypes["Multiply"] = HopperToken.Multiply;
        hopperTokenTypes["Divide"] = HopperToken.Divide;
        hopperTokenTypes["Modulus"] = HopperToken.Modulus;
        hopperTokenTypes["Increment"] = HopperToken.Increment;
        hopperTokenTypes["Decrement"] = HopperToken.Decrement;
        hopperTokenTypes["NE"] = HopperToken.NE;
        hopperTokenTypes["EQ"] = HopperToken.EQ;
        hopperTokenTypes["GT"] = HopperToken.GT;
        hopperTokenTypes["LT"] = HopperToken.LT;
        hopperTokenTypes["LE"] = HopperToken.LE;
        hopperTokenTypes["GE"] = HopperToken.GE;
        hopperTokenTypes["ShiftLeft"] = HopperToken.ShiftLeft;
        hopperTokenTypes["ShiftRight"] = HopperToken.ShiftRight;
        hopperTokenTypes["BooleanNot"] = HopperToken.BooleanNot;
        hopperTokenTypes["BooleanOr"] = HopperToken.BooleanOr;
        hopperTokenTypes["BooleanAnd"] = HopperToken.BooleanAnd;
        
        hopperTokenTypes["BitAnd"] = HopperToken.BitAnd;
        hopperTokenTypes["BitOr"] = HopperToken.BitOr;
        hopperTokenTypes["BitNot"] = HopperToken.BitNot;
        hopperTokenTypes["BitXor"] = HopperToken.BitXor;
        
        hopperTokenTypes["Instruction"] = HopperToken.Instruction;
        hopperTokenTypes["Condition"] = HopperToken.Condition;
        hopperTokenTypes["Register"] = HopperToken.Register;
        
        
        foreach (var kv in hopperTokenTypes)
        {
            hopperTokenNames[kv.value] = kv.key;
        }
    }
    InitializeHopper()
    {
        typeKeywords.Clear();
        directiveKeywords.Clear();
        statementKeywords.Clear();
        
        // actual 'type' keywords
        typeKeywords["array"] = true;
        typeKeywords["bool"] = true;
        typeKeywords["byte"] = true;
        typeKeywords["char"] = true;
        typeKeywords["delegate"] = true;
        typeKeywords["dictionary"] = true;
        typeKeywords["directory"] = true;
        typeKeywords["enum"] = true;
        typeKeywords["file"] = true;
        typeKeywords["flags"] = true;
        typeKeywords["float"] = true;
        typeKeywords["int"] = true;
        typeKeywords["uint"] = true;
        typeKeywords["list"] = true;
        typeKeywords["long"] = true;
        typeKeywords["pair"] = true;
        typeKeywords["string"] = true;
        typeKeywords["type"] = true;
        typeKeywords["variant"] = true;
        typeKeywords["record"] = true;
        
        typeKeywords["const"] = true;
        typeKeywords["ref"] = true;
        typeKeywords["var"] = true;
        
        typeKeywords["false"] = true;
        typeKeywords["true"] = true;
        
        directiveKeywords["#ifdef"] = true;
        directiveKeywords["#ifndef"] = true;
        directiveKeywords["#else"] = true;
        directiveKeywords["#endif"] = true;
        directiveKeywords["#define"] = true;
        directiveKeywords["#if"] = true;
        directiveKeywords["#error"] = true;
        directiveKeywords["defined"] = true;
        
        statementKeywords["break"] = true;
        statementKeywords["case"] = true;
        statementKeywords["continue"] = true;
        statementKeywords["default"] = true;
        statementKeywords["else"] = true;
        statementKeywords["for"] = true;
        statementKeywords["foreach"] = true;
        statementKeywords["get"] = true;
        statementKeywords["if"] = true;
        statementKeywords["in"] = true;
        statementKeywords["loop"] = true;
        statementKeywords["program"] = true;
        statementKeywords["return"] = true;
        statementKeywords["set"] = true;
        statementKeywords["switch"] = true;
        statementKeywords["system"] = true;
        statementKeywords["library"] = true;
        statementKeywords["typeof"] = true;
        statementKeywords["addrof"] = true;
        statementKeywords["unit"] = true;
        statementKeywords["uses"] = true;
        statementKeywords["while"] = true;
        statementKeywords["friend"] = true;
    }
    InitializeTiggerC()
    {
        typeKeywords.Clear();
        directiveKeywords.Clear();
        statementKeywords.Clear();
        
        // actual 'type' keywords
        typeKeywords["bool"] = true;
        typeKeywords["byte"] = true;
        typeKeywords["char"] = true;
        typeKeywords["int"] = true;
        typeKeywords["word"] = true;
        
        typeKeywords["const"] = true;
        
        typeKeywords["false"] = true;
        typeKeywords["true"] = true;
        typeKeywords["null"] = true;
        
        directiveKeywords["#ifdef"] = true;
        directiveKeywords["#ifndef"] = true;
        directiveKeywords["#else"] = true;
        directiveKeywords["#endif"] = true;
        directiveKeywords["#define"] = true;
        directiveKeywords["#if"] = true;
        directiveKeywords["defined"] = true;
        directiveKeywords["#include"] = true;
        
        statementKeywords["break"] = true;
        statementKeywords["case"] = true;
        statementKeywords["continue"] = true;
        statementKeywords["default"] = true;
        statementKeywords["else"] = true;
        statementKeywords["for"] = true;
        statementKeywords["if"] = true;
        statementKeywords["return"] = true;
        statementKeywords["switch"] = true;
        statementKeywords["uses"] = true;
        statementKeywords["while"] = true;
        statementKeywords["func"] = true;
        statementKeywords["mem"] = true;
        statementKeywords["as"] = true;
        statementKeywords["asm"] = true;
        
    }
    
    ClearAssembler()
    {
        instructionKeywords.Clear();
        registerKeywords.Clear();
        conditionKeywords.Clear();
    }
    InitializeAssembler(CPUArchitecture architecture)
    {    
        ClearAssembler();
        
        if (architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            // 6502 instructions:
            instructionKeywords["BEQ"] = true;
            instructionKeywords["BNE"] = true;
            
            instructionKeywords["JMP"] = true;
            instructionKeywords["STP"] = true;
            
            instructionKeywords["LDA"] = true;
            instructionKeywords["LDX"] = true;
            instructionKeywords["LDY"] = true;
            instructionKeywords["STA"] = true;
            instructionKeywords["STX"] = true;
            instructionKeywords["STY"] = true;
            
            instructionKeywords["TYA"] = true;
            instructionKeywords["TAY"] = true;
            instructionKeywords["TXA"] = true;
            instructionKeywords["TAX"] = true;
            instructionKeywords["TXS"] = true;
            instructionKeywords["TSX"] = true;
            
            instructionKeywords["CMP"] = true;
            instructionKeywords["CPX"] = true;
            instructionKeywords["CPY"] = true;
            
            instructionKeywords["JSR"] = true;
            instructionKeywords["RTS"] = true;
            instructionKeywords["BRK"] = true;
            instructionKeywords["RTI"] = true;
            
            instructionKeywords["BEQ"] = true;
            instructionKeywords["BNE"] = true;
            instructionKeywords["BCC"] = true;
            instructionKeywords["BCS"] = true;
            instructionKeywords["BPL"] = true;
            instructionKeywords["BMI"] = true;
            instructionKeywords["BVS"] = true;
            instructionKeywords["BVC"] = true;
            
            instructionKeywords["PHA"] = true;
            instructionKeywords["PLA"] = true;
            instructionKeywords["PHP"] = true;
            instructionKeywords["PLP"] = true;
            
            instructionKeywords["INC"] = true;
            instructionKeywords["DEC"] = true;
            instructionKeywords["INX"] = true;
            instructionKeywords["DEX"] = true;
            instructionKeywords["INY"] = true;
            instructionKeywords["DEY"] = true;
            
            instructionKeywords["AND"] = true;
            instructionKeywords["BIT"] = true;
            instructionKeywords["ORA"] = true;
            instructionKeywords["EOR"] = true;
            instructionKeywords["LSR"] = true;
            instructionKeywords["ASL"] = true;
            instructionKeywords["ROR"] = true;
            instructionKeywords["ROL"] = true;
            instructionKeywords["ADC"] = true;
            instructionKeywords["SBC"] = true;
            
            instructionKeywords["CLC"] = true;
            instructionKeywords["CLD"] = true;
            instructionKeywords["CLI"] = true;
            instructionKeywords["CLV"] = true;
            instructionKeywords["SEC"] = true;
            instructionKeywords["SED"] = true;
            instructionKeywords["SEI"] = true;
            
            instructionKeywords["NOP"] = true;
            
             // 6502 registers:
            registerKeywords["A"] = true;
            registerKeywords["X"] = true;
            registerKeywords["Y"] = true;
            
            //registerKeywords["PC"] = true;
            //registerKeywords["SP"] = true;
            
            // 6502 flags / 'conditions':
            conditionKeywords["C"] = true;  // Carry (BCS)
            conditionKeywords["Z"] = true;  // Zero (BEQ)
            conditionKeywords["V"] = true;  // Overflow (BVS)
            
            // 6502 'conditions':
            conditionKeywords["NC"] = true; // No Carry (BCC)
            conditionKeywords["NZ"] = true; // Not Zero (BNE)
            conditionKeywords["NV"] = true; // No Overflow (BVC)
            conditionKeywords["PL"] = true; // Positive (BPL)
            conditionKeywords["MI"] = true; // Minus (BMI)
        }
        if (architecture & CPUArchitecture.W65C02 != CPUArchitecture.None)
        {
            instructionKeywords["BRA"] = true;
            instructionKeywords["TSB"] = true;
            instructionKeywords["TRB"] = true;
            instructionKeywords["STZ"] = true;
            
            instructionKeywords["PHX"] = true;
            instructionKeywords["PLX"] = true;
            instructionKeywords["PHY"] = true;
            instructionKeywords["PLY"] = true;
            
            for (byte i=0; i < 8; i++)
            {
                instructionKeywords["RMB" + i.ToString()] = true;
                instructionKeywords["SMB" + i.ToString()] = true;
                instructionKeywords["BBR" + i.ToString()] = true;
                instructionKeywords["BBS" + i.ToString()] = true;
            }
        }
       
       if (architecture & CPUArchitecture.M6809 != CPUArchitecture.None)
{
    // 6809 instructions:
    instructionKeywords["LDA"] = true;
    instructionKeywords["LDB"] = true;
    instructionKeywords["LDD"] = true;
    instructionKeywords["LDS"] = true;
    instructionKeywords["LDU"] = true;
    instructionKeywords["LDX"] = true;
    instructionKeywords["LDY"] = true;
    
    instructionKeywords["STA"] = true;
    instructionKeywords["STB"] = true;
    instructionKeywords["STD"] = true;
    instructionKeywords["STS"] = true;
    instructionKeywords["STU"] = true;
    instructionKeywords["STX"] = true;
    instructionKeywords["STY"] = true;

    instructionKeywords["ADCA"] = true;
    instructionKeywords["ADCB"] = true;
    instructionKeywords["ADDA"] = true;
    instructionKeywords["ADDB"] = true;
    instructionKeywords["ADDD"] = true;
    instructionKeywords["SUBA"] = true;
    instructionKeywords["SUBB"] = true;
    instructionKeywords["SUBD"] = true;
    instructionKeywords["ANDA"] = true;
    instructionKeywords["ANDB"] = true;
    instructionKeywords["ORA"] = true;
    instructionKeywords["ORB"] = true;
    instructionKeywords["EORA"] = true;
    instructionKeywords["EORB"] = true;
    instructionKeywords["CMPA"] = true;
    instructionKeywords["CMPB"] = true;
    instructionKeywords["CMPD"] = true;
    instructionKeywords["CMPX"] = true;
    instructionKeywords["CMPY"] = true;
    instructionKeywords["CMPU"] = true;
    instructionKeywords["CMPS"] = true;

    instructionKeywords["INC"] = true;
    instructionKeywords["DEC"] = true;
    instructionKeywords["INCA"] = true;
    instructionKeywords["DECA"] = true;
    instructionKeywords["INCB"] = true;
    instructionKeywords["DECB"] = true;

    instructionKeywords["JMP"] = true;
    instructionKeywords["JSR"] = true;
    instructionKeywords["RTS"] = true;
    instructionKeywords["RTI"] = true;

    instructionKeywords["BRA"] = true;
    instructionKeywords["BSR"] = true;
    instructionKeywords["BEQ"] = true;
    instructionKeywords["BNE"] = true;
    instructionKeywords["BCC"] = true;
    instructionKeywords["BCS"] = true;
    instructionKeywords["BPL"] = true;
    instructionKeywords["BMI"] = true;
    instructionKeywords["BVC"] = true;
    instructionKeywords["BVS"] = true;
    instructionKeywords["BLT"] = true;
    instructionKeywords["BGT"] = true;
    instructionKeywords["BLE"] = true;
    instructionKeywords["BGE"] = true;

    instructionKeywords["PSHS"] = true;
    instructionKeywords["PULS"] = true;
    instructionKeywords["PSHU"] = true;
    instructionKeywords["PULU"] = true;
    instructionKeywords["SWI"] = true;
    instructionKeywords["CWAI"] = true;
    instructionKeywords["MUL"] = true;
    instructionKeywords["NOP"] = true;

    instructionKeywords["ASL"] = true;
    instructionKeywords["ASR"] = true;
    instructionKeywords["LSL"] = true;
    instructionKeywords["LSR"] = true;
    instructionKeywords["ROL"] = true;
    instructionKeywords["ROR"] = true;

    instructionKeywords["TFR"] = true;
    instructionKeywords["EXG"] = true;
    instructionKeywords["NEG"] = true;
    instructionKeywords["COM"] = true;
    instructionKeywords["CLR"] = true;

    instructionKeywords["ANDCC"] = true;
    instructionKeywords["ORCC"] = true;

    // 6809 registers:
    registerKeywords["A"] = true;
    registerKeywords["B"] = true;
    registerKeywords["D"] = true;
    registerKeywords["X"] = true;
    registerKeywords["Y"] = true;
    registerKeywords["U"] = true;
    registerKeywords["S"] = true;
    registerKeywords["PC"] = true;
    registerKeywords["DP"] = true;
    registerKeywords["CC"] = true;

    // 6809 flags / 'conditions' for 'if':
    conditionKeywords["C"] = true;  // Carry (uses BCC)
    conditionKeywords["Z"] = true;  // Zero (uses BNE)
    conditionKeywords["V"] = true;  // Overflow (uses BVC)
    conditionKeywords["NC"] = true; // No Carry (uses BCS)
    conditionKeywords["NZ"] = true; // Not Zero (uses BEQ)
    conditionKeywords["NV"] = true; // No Overflow (uses BVS)
    conditionKeywords["PL"] = true; // Not Negative (uses BMI)
    conditionKeywords["MI"] = true; // Negative (uses BPL)
    conditionKeywords["GE"] = true; // Greater or Equal (uses BLT) - signed
    conditionKeywords["LT"] = true; // Less Than (uses BGE) - signed
    conditionKeywords["GT"] = true; // Greater Than (uses BLE) - signed
    conditionKeywords["LE"] = true; // Less or Equal (uses BGT) - signed
    conditionKeywords["LO"] = true; // Less Than (uses BHS) - unsigned
    conditionKeywords["HS"] = true; // Greater or Equal (uses BLO) - unsigned
}

        
         
               
    }
    
    bool IsInstructionKeyword(string candidate)
    {
        return instructionKeywords.Contains(candidate);
    }
    bool IsRegisterKeyword(string candidate)
    {
        return registerKeywords.Contains(candidate);
    }
    bool IsConditionKeyword(string candidate)
    {
        return conditionKeywords.Contains(candidate);
    }
    
    bool IsDirectiveKeyword(string candidate)
    {
        return directiveKeywords.Contains(candidate);
    }
    bool IsTypeKeyword(string candidate)
    {
        return typeKeywords.Contains(candidate);
    }
    bool IsStatementKeyword(string candidate)
    {
        return statementKeywords.Contains(candidate);
    }
    bool IsReservedWord(string candidate)
    {
        return typeKeywords.Contains(candidate) || statementKeywords.Contains(candidate);
    }
    bool IsKeyword(string candidate)
    {
        return typeKeywords.Contains(candidate) || statementKeywords.Contains(candidate);
    }
    
    // ["type"]    - HopperToken
    // ["lexeme"]  - string
    // ["line"]    - uint
    // ["source"]  - string
    // ["pos"]     - uint - index in current parsed content string
    // ["literal"] - depends

    <string,string> New(HopperToken tokenType, string lexeme, uint ln, long pos, string sourcePath)
    {
        <string,string> token;
        token["type"] = HopperTokenToString(tokenType);
        token["lexeme"] = lexeme;
        token["line"] = ln.ToString();
        token["source"] = sourcePath;
        token["pos"] = pos.ToString();
        return token;
    }
    string ToString(<string, string> token)
    {
        HopperToken tokenType;
        string content = "Undefined";
        if (token.Count != 0)
        {
            content = token["type"];
            tokenType = Token.GetType(token);
            switch (tokenType)
            {
                case HopperToken.Identifier:
                case HopperToken.DottedIdentifier:
                case HopperToken.Discarder:
                case HopperToken.LabelIdentifier:
                {
                    content = content + " '" + token["lexeme"] + "'";
                }
                case HopperToken.StringConstant:
                {
                    content = content + " \"" + token["lexeme"] + '"';
                }
                case HopperToken.Char:
                {
                    content = content + " '" + token["lexeme"] + "'";
                }
                case HopperToken.Keyword:
                {
                    content = content + " " + token["lexeme"];
                }
                case HopperToken.Integer:
                {
                    content = content + " " + token["lexeme"];
                }
                case HopperToken.Bool:
                {
                    content = content + " " + token["lexeme"];
                }
            }
            content = content + "[" + token["source"] + ":" + token["line"] + "->" + token["pos"] + "]";
        }
        return content;
    }
    
}

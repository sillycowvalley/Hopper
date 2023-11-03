unit Token
{
    uses "/Source/System/Diagnostics"

    enum HopperToken
    {
        Undefined,
        Error,

        DottedIdentifier, // Key.Enter, Keyboard.Key.Enter
        Identifier,       // privateIndentifier, PublicIdentifier
        Dot,
        Keyword,          // "system"
        Directive,        // #ifdef

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

        Assign,    // =

        EOF
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
        // <string, HopperToken> hopperTokenTypes;
        string name = token["type"];
        HopperToken tokenType = HopperToken.Undefined;
        if (hopperTokenTypes.Contains(name))
        {
            tokenType = hopperTokenTypes[name];
        }
        else
        {
            PrintLn("TODO : token not implemented in Token.GetType");
        }
        return tokenType;
    }

#ifndef H6502        
    <string,bool> typeKeywords;           // blue
    <string,bool> statementKeywords;      // purple
    <string,bool> directiveKeywords;      // dark gray
    
    <string, HopperToken> hopperTokenTypes;
    <HopperToken, string> hopperTokenNames;
    
    Initialize()
    {
        typeKeywords.Clear();
        statementKeywords.Clear();
        
        hopperTokenTypes.Clear();
        
        hopperTokenTypes["EOF"] = HopperToken.EOF;
        hopperTokenTypes["Undefined"] = HopperToken.Undefined;
        hopperTokenTypes["Error"] = HopperToken.Error;
        hopperTokenTypes["Char"] = HopperToken.Char;
        hopperTokenTypes["Bool"] = HopperToken.Bool;
        hopperTokenTypes["Keyword"] = HopperToken.Keyword;
        hopperTokenTypes["Directive"] = HopperToken.Directive;
        hopperTokenTypes["Identifier"] = HopperToken.Identifier;
        hopperTokenTypes["DottedIdentifier"] = HopperToken.DottedIdentifier;
        hopperTokenTypes["Integer"] = HopperToken.Integer;
        hopperTokenTypes["Float"] = HopperToken.Float;
        hopperTokenTypes["StringConstant"] = HopperToken.StringConstant;
        hopperTokenTypes["Assign"] = HopperToken.Assign;
        hopperTokenTypes["BitAnd"] = HopperToken.BitAnd;
        hopperTokenTypes["BitOr"] = HopperToken.BitOr;
        hopperTokenTypes["LParen"] = HopperToken.LParen;
        hopperTokenTypes["RParen"] = HopperToken.RParen;
        hopperTokenTypes["LBrace"] = HopperToken.LBrace;
        hopperTokenTypes["RBrace"] = HopperToken.RBrace;
        hopperTokenTypes["LBracket"] = HopperToken.LBracket;
        hopperTokenTypes["RBracket"] = HopperToken.RBracket;
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
        
        foreach (var kv in hopperTokenTypes)
        {
            hopperTokenNames[kv.value] = kv.key;
        }
        
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
        statementKeywords["typeof"] = true;
        statementKeywords["addrof"] = true;
        statementKeywords["unit"] = true;
        statementKeywords["uses"] = true;
        statementKeywords["while"] = true;
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
#endif    
    
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
        if (token.Count > 0)
        {
            content = token["type"];
            tokenType = Token.GetType(token);
            switch (tokenType)
            {
                case HopperToken.Identifier:
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

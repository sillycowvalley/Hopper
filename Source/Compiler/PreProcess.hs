program PreProcess
{

  uses "/Source/System/System"
  uses "/Source/System/Diagnostics"
  uses "/Source/System/Screen"
  uses "/Source/System/Keyboard"
  uses "/Source/Compiler/Tokens/Token"
  uses "/Source/Compiler/Tokens/Scanner"
  uses "/Source/Compiler/Tokens/Parser"
  uses "/Source/Compiler/Tokens/SysCalls"
  uses "/Source/Compiler/Tokens/LibCalls"
  uses "/Source/Compiler/Symbols"
   
  uses "/Source/Compiler/Constant"
  
  uses "/Source/Compiler/Directives" 
  
  bool IsDebugger { get { return false; } }
  
  // Documentation:
  // - documentation comments
  // - .json to .md conversion? .pdf conversion?
  // Future:
  // - drop << and >> tokens? synthesize them when needed? or, deal with them in the context of types?
  // - block comments aware of strings ("...*/...")
    
    <string, bool> unitsParsed;
    string currentUnit;
        
    bool normalizeIdentifier(<string,string> idToken, ref string identifier, ref bool public, bool noDuplicates)
    {
        bool success = false;
        loop
        {
            identifier = idToken["lexeme"];
            if (identifier.Contains('.'))
            {
                Parser.ErrorAtCurrent("simple identifier expected");           
                break;
            }
            if (Token.IsReservedWord(identifier))
            {
                Parser.ErrorAtCurrent("identifier '" + identifier + "' is a reserved word");
                break;
            }
            public = Char.IsUpper(identifier[0]);
            identifier = currentUnit + "." + identifier; // append current namespace
            if (noDuplicates)
            {
                if (Symbols.GlobalExists(identifier))
                {
                    Parser.ErrorAtCurrent("identifier already in use");
                    break;
                }
            }
            success = true;
            break;
        }
        return success;
    }
    
    bool isValueType(string typeName)
    {
        bool found;
        loop
        {
            string valueTypes = "|bool|byte|char|uint|int|type|";
            typeName = "|" + typeName + "|";
            if (valueTypes.Contains(typeName))
            {
                found = true;
                break;
            }
            break;
        }
        return found;
    }
    
    bool tryParseTypeString(ref string typeString)
    {
        bool genericKAllowed = (currentUnit == "Dictionary") || (currentUnit == "Pair");
        bool genericVAllowed = genericKAllowed || (currentUnit == "Array") || (currentUnit == "List");  
        
        bool success = false;
        loop
        {
            bool isSimple = false;
            <string,string> typeToken = Parser.CurrentToken;
            if (Parser.Check(HopperToken.Keyword, "|bool|byte|char|uint|int|long|float|string|type|variant|file|directory|"))
            {
                isSimple = true;
            }
            if (Parser.Check(HopperToken.Identifier) || Parser.Check(HopperToken.DottedIdentifier))
            {
                if (genericKAllowed && (typeToken["lexeme"] == "K"))
                {
                    isSimple = true;
                }
                else if (genericVAllowed && (typeToken["lexeme"] == "V"))
                {
                    isSimple = true;
                }
                else
                {
                    isSimple = true; // on first pass, just assume it is a valid named value type
                    Symbols.DeferValidation(typeToken);
                }
            }
            if (isSimple)
            {
                // simple type
                success = true;
                typeString = typeString + typeToken["lexeme"];
                Parser.Advance();
                bool arrayUnitV = false;
                bool systemByteArray = false;
                if (Parser.Check(HopperToken.LBracket))
                {
                    // like byte[8192]
                    if (!IsValueType(typeString))
                    {
                        if ((currentUnit == "Array") && (typeString == "V"))
                        {
                            // V[] for "this"
                            arrayUnitV = true;
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("arrays can only contain value types");
                            success = false;
                            break;   
                        }
                    }
                    else if (((currentUnit == "System") || (currentUnit == "Runtime")) && (typeString == "byte"))
                    {
                        systemByteArray = true;
                    }
                    Parser.Advance(); // [
                    typeString = typeString + "[";
                    string value;
                    if ((arrayUnitV || systemByteArray) && Parser.Check(HopperToken.RBracket))
                    {
                        // empty range: 
                        //   'V[]' for Array system methods
                        //   'byte[]' for System.Call(..)
                    }
                    else
                    {
                        value = ParseConstantExpression("uint");
                    }
                    typeString = typeString + value;   
                    if (!Parser.Check(HopperToken.RBracket))
                    {
                        Parser.ErrorAtCurrent(']');
                        success = false;
                        break;
                    }
                    Parser.Advance(); // ]   
                    typeString = typeString + "]";
                    
                    break;
                }
            }
            else if (Parser.Check(HopperToken.LT))
            {
                Parser.Advance(); // <
                // <byte> and <string,byte>
                typeString = typeString + "<";
                success = tryParseTypeString(ref typeString);
                if (success)
                {
                    if (Parser.Check(HopperToken.Comma))
                    {
                        // TODO: make <byte,byte,byte> illegal
                        Parser.Advance(); // ,
                        typeString = typeString + ",";       
                        success = tryParseTypeString(ref typeString);
                    }
                }
                if (success)
                {
                    if (!Parser.Check(HopperToken.GT))
                    {
                        Parser.ErrorAtCurrent('>');
                        success = false;
                        break;
                    }
                    Parser.Advance(); // >
                    typeString = typeString + ">";
                }
            }
            break;
        }
        return success;
    }
  
    <string> walkBlock()
    {
      <string> blockPos;
      loop
      {
          bool isSystem = false;
          if (Parser.Check(HopperToken.Keyword, "system") || Parser.Check(HopperToken.Keyword, "library"))
          {
              isSystem = true;
          }
          else if (!Parser.Check(HopperToken.LBrace))
          {
              Parser.ErrorAtCurrent('{');
              break;
          }
          <string,string> currentToken = Parser.CurrentToken;
          long pos;
          if (Long.TryParse(currentToken["pos"], ref pos))
          {
          }
          if (isSystem)
          {
              // empty blockPos implies syscall or libcall
          }
          else
          {
              blockPos.Append(pos.ToString());
              blockPos.Append(currentToken["line"]);
              blockPos.Append(currentToken["source"]);
          }
          Parser.Advance();
          if (isSystem)
          {
              Parser.Consume(HopperToken.SemiColon, "';' expected");
              break;
          }
          int nested = 1;
          loop
          {
              if (HadError)
              {
                  Parser.ErrorAtCurrent('}');
                  break;
              }
              if (Parser.Check(HopperToken.RBrace))
              {
                  Parser.Advance(); // }
                  nested--;
                  if (nested == 0)
                  {
                      break;
                  }
                  continue;
              }
              if (Parser.Check(HopperToken.LBrace))
              {
                  // nested { .. }
                  nested++;
              }
              Parser.Advance(); // anything else
          }
          break;
        }
        return blockPos;      
    }

    constDeclaration()
    {
        string typeString;   
        loop
        {
            Parser.Advance(); // const
            if (   !tryParseTypeString(ref typeString)
                || (!IsValueType(typeString) && (typeString != "float") && (typeString != "long") && (typeString != "string"))
                || (typeString == "delegate")) 
            {
                Parser.ErrorAtCurrent("simple type expected");
                break;
            }        
            <string,string> idToken = Parser.CurrentToken;
            string identifier;
            bool public;
            if (!normalizeIdentifier(idToken, ref identifier, ref public, true)) // noDuplicates
            {
                break;   
            }
            Parser.Advance();
            Parser.Consume(HopperToken.Assign, "'=' expected");
            if (HadError)
            {
                break;
            }    
            string value = ParseConstantExpression(typeString);
            if (HadError)
            {
                break;
            }
            Symbols.AddConstant(idToken["lexeme"], value);   
            
            Parser.Consume(HopperToken.SemiColon, "';' expected");
            break;                                 
       }          
        
    }   
    enumDeclaration(bool isFlags)
    {
        loop
        {
            Parser.Advance(); // enum | flags
            
            if (!Parser.Check(HopperToken.Identifier) || Parser.Check(HopperToken.DottedIdentifier))
            {
                Parser.ErrorAtCurrent("identifier expected");       
                break;
            }
            <string,string> idToken = Parser.CurrentToken;
            string identifier;
            bool public;
            if (!normalizeIdentifier(idToken, ref identifier, ref public, true))
            {
                break;   
            }
            
            Parser.Advance(); // identifier           
            
            
            if (!Parser.Check(HopperToken.LBrace))
            {
                Parser.ErrorAtCurrent('{');
                break;
            }
            Parser.Advance(); // {
            <string,uint> values;
            uint currentValue = 0;
            loop
            {
                if (Parser.Check(HopperToken.RBrace))
                {
                    Parser.Advance(); // }
                    break;
                }
                if (Parser.Check(HopperToken.EOF))
                {
                    Parser.ErrorAtCurrent("unexpected end of file in enum/flags declaration");
                    break;
                }
                
                if (!Parser.Check(HopperToken.Identifier))
                {
                    Parser.ErrorAtCurrent("identifier expected");       
                    break;
                }
                
                <string,string> entryToken = Parser.CurrentToken;
                string qualifiedName = identifier + "." + entryToken["lexeme"];
                string valueString;
                Parser.Advance();
                if (Parser.Check(HopperToken.Assign))
                {
                    Parser.Advance(); // =
                    if (!Parser.Check(HopperToken.Integer))
                    {
                        Parser.ErrorAtCurrent("integer expected");       
                        break;        
                    }
                    <string,string> valueToken = Parser.CurrentToken;
                    valueString = valueToken["lexeme"];
                    if (UInt.TryParse(valueString, ref currentValue))
                    {
                        
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("'uint' constant expected"); 
                    }
                    Advance();
                }
                
                values[qualifiedName] = currentValue;
                currentValue++;
                bool expectComma = false;
                if (!Parser.Check(HopperToken.RBrace))
                {
                    expectComma = true;        
                }
                if (Parser.Check(HopperToken.Comma))
                {
                    Parser.Advance(); // ,
                }
                else if (expectComma)
                {
                    Parser.ErrorAtCurrent(',');       
                }
            }
                    
            if (HadError)
            {
                break;
            }
            if (isFlags)
            {
                Symbols.AddFlags(identifier, values);
            }
            else
            {
                Symbols.AddEnum(identifier, values);
            }
            break;
        }
    }
    enumDeclaration()
    {
        enumDeclaration(false);
    }   
    flagsDeclaration()
    {
        enumDeclaration(true);
    }   
    
    usesDeclaration()
    {
        loop
        {
          <string,string> prevToken = Parser.PreviousToken;
          <string,string> usesToken = Parser.CurrentToken;
          
          if (usesToken["line"] == prevToken["line"])
          {
              Parser.ErrorAtCurrent("'uses' must be first token on line");
              break;
          }
          Parser.Advance(); // uses
          if (!Parser.Check(HopperToken.StringConstant))
          {
              Parser.ErrorAtCurrent("path of unit source expected");
              break;
          }
          Parser.Advance();
          <string, string> pathToken = Parser.PreviousToken;
          string hsPath = pathToken["lexeme"];
          string hsPathLower = hsPath.ToLower();
          if (!hsPathLower.EndsWith(".hs"))
          {
              hsPath = hsPath + ".hs";
          }
          if (!File.Exists(hsPath))
          {
              Parser.ErrorAtCurrent("'" + hsPath + "' not found");
              break;
          }
          
          if (pathToken["line"] != usesToken["line"])
          {
              Parser.ErrorAtCurrent("'uses' and path must appear on one line");
              break;
          }
          
          hsPathLower = hsPath.ToLower();
          if (!unitsParsed.Contains(hsPathLower))
          {
              unitsParsed[hsPathLower] = false; // false means we're aware of it but we haven't parsed it yet
          }
          <string, string> nextToken = Parser.CurrentToken;
          // ["type"]    - HopperToken
          // ["lexeme"]  - string
          // ["line"]    - uint
          // ["source"]  - string
          // ["pos"]     - uint - index in current parsed content string
          // ["literal"] - depends
          if (nextToken["line"] == pathToken["line"])
          {
              Parser.ErrorAtCurrent("'uses' declaration must be alone on line");
              break;
          }
          break;
        }
    }   
    
    mainMethodDeclaration()
    {
        loop
        {
            <string> blockPos = walkBlock();
            if (HadError)
            {
                break;
            }
            < <string > > arguments;
            Symbols.AddMethod(currentUnit + ".main", arguments, blockPos);
            break;
        }
    }   
    
    < < string > > argumentsDeclaration()
    {
        < < string > > arguments;
        <string> argumentNames;
        
        Parser.Advance(); // (
        loop
        {
            if (Parser.Check(HopperToken.RParen))
            {
                Parser.Advance(); // )
                break; // done
            }
            if (arguments.Length != 0)
            {
                if (!Parser.Check(HopperToken.Comma))
                {
                    Parser.ErrorAtCurrent(',');       
                    break;
                }
                Parser.Advance();
            }
            string isReference = "";
            if (Parser.Check(HopperToken.Keyword, "ref"))
            {
                Parser.Advance(); // ref   
                isReference = "ref";   
            }
            string typeString;   
            if (!tryParseTypeString(ref typeString))
            {
                if (!HadError)
                {
                    Parser.ErrorAtCurrent("type expected");
                }
                break;
            }
            if (!Parser.Check(HopperToken.Identifier))
            {
                Parser.ErrorAtCurrent("identifier expected");   
                break;
            }  
            <string,string> currentToken = CurrentToken;
            string identifier = currentToken["lexeme"];
            if (argumentNames.Contains(identifier))
            {
                Parser.ErrorAtCurrent("argument '" + identifier + "' already exists");
                break;
            }       
            if (Token.IsReservedWord(identifier))
            {
                Parser.ErrorAtCurrent("identifier '" + identifier + "' is a reserved word");
                break;
            }
            argumentNames.Append(identifier);
            <string> argument;
            argument.Append(isReference);
            argument.Append(typeString);
            argument.Append(identifier);
            arguments.Append(argument);          
            Parser.Advance();
        }
        return arguments;
    }
       
    methodDeclaration(<string,string> idToken, bool isDelegate)
    {
        loop
        {
            string identifier;
            bool public;
            if (!normalizeIdentifier(idToken, ref identifier, ref public, false))
            {
                break;   
            }
            < <string > > arguments = argumentsDeclaration();
            if (HadError)
            {
                break;
            }
            if (isDelegate)
            {
                Parser.Consume(HopperToken.SemiColon, "';' expected");
                if (HadError)
                {
                    break;
                }
                Symbols.AddMethodDelegate(identifier, arguments);
                break;   
            }
            <string> blockPos = walkBlock();
            if (HadError)
            {
                break;
            }
            Symbols.AddMethod(identifier, arguments, blockPos);
            break;
        }
    }
     
    functionDeclaration(<string,string> idToken, string returnTypeString, bool isDelegate)
    {
        loop
        {
            string identifier;
            bool public;
            if (!normalizeIdentifier(idToken, ref identifier, ref public, false))
            {
                break;   
            }
            
            < <string > > arguments = argumentsDeclaration();
            if (HadError)
            {
                break;
            }
            if (isDelegate)
            {
                Parser.Consume(HopperToken.SemiColon, "';' expected");
                if (HadError)
                {
                    break;
                }
                Symbols.AddFunctionDelegate(identifier, arguments, returnTypeString);
                break;   
            }
            <string> blockPos = walkBlock();
            if (HadError)
            {
                break;
            }
            Symbols.AddFunction(identifier, arguments, returnTypeString, blockPos);
            break;
        }
    }   
    
    bool isThisType(string typeName)
    {
        bool found;
        loop
        {
            string valueTypes = "|array|dictionary|list|string|bool|byte|char|uint|int|long|float|type|file|directory|";
            typeName = "|" + typeName + "|";
            if (valueTypes.Contains(typeName))
            {
                found = true;
                break;
            }
            break;
        }
        return found;
    }
    
    
    propertyDeclaration(<string,string> idToken, string typeString)
    {
        bool hadGet;
        bool hadSet;
        loop
        {
            string identifier;
            bool public;
            if (!normalizeIdentifier(idToken, ref identifier, ref public, true))
            {
                break;   
            }
            
            Parser.Consume(HopperToken.LBrace, "'{' expected");
            if (HadError)
            {
                break;
            }
            loop
            {
                if (Parser.Check(HopperToken.Keyword, "get"))
                {
                    Parser.Advance();
                    if (hadGet)
                    {
                        Parser.ErrorAtCurrent("'get' already defined for " + identifier);    
                        break;    
                    }
                    hadGet = true;
                    <string> blockPos = walkBlock();
                    if (HadError)
                    {
                        break;
                    }
                    
                    string getterName = identifier + "_Get";
                    < <string > > arguments;
                    
                    uint iDot;
                    string typeName;
                    if (getterName.IndexOf('.', ref iDot))
                    {
                        typeName = getterName.Substring(0, iDot);
                        typeName = typeName.ToLower();
                        if (isThisType(typeName))
                        {
                            switch (typeName)
                            {
                                case "array":
                                {
                                    typeName = "V[]";
                                }
                                case "list":
                                {
                                    typeName = "<V>";
                                }
                                case "dictionary":
                                {
                                    typeName = "<K,V>";
                                }
                            }
                            <string> thisargument;
                            thisargument.Append(""); // not "ref"
                            thisargument.Append(typeName);
                            thisargument.Append("this");
                            arguments.Append(thisargument); 
                        }
                    }
                    
                    Symbols.AddFunction(getterName, arguments, typeString, blockPos);
                    continue;
                }
                else if (Parser.Check(HopperToken.Keyword, "set"))
                {
                    Parser.Advance();
                    if (hadSet)
                    {
                        Parser.ErrorAtCurrent("'set' already defined for " + identifier);        
                        break;
                    }
                    hadSet = true;
                    <string> blockPos = walkBlock();
                    if (HadError)
                    {
                        break;
                    }
                    string setterName = identifier + "_Set";
                    
                    < <string > > arguments;
                    
                    uint iDot;
                    string typeName;
                    if (setterName.IndexOf('.', ref iDot))
                    {
                        typeName = setterName.Substring(0, iDot);
                        typeName = typeName.ToLower();
                        if (isThisType(typeName))
                        {
                            switch (typeName)
                            {
                                case "array":
                                {
                                    typeName = "V[]";
                                }
                                case "list":
                                {
                                    typeName = "<V>";
                                }
                                case "dictionary":
                                {
                                    typeName = "<K,V>";
                                }
                            }
                            <string> thisargument;
                            thisargument.Append(""); // not "ref"
                            thisargument.Append(typeName);
                            thisargument.Append("this");
                            arguments.Append(thisargument); 
                        }
                    }
                    
                    <string> argument;
                    argument.Append(""); // not "ref"
                    argument.Append(typeString);
                    argument.Append("value");
                    arguments.Append(argument);          
                    
                    
                    Symbols.AddMethod(setterName, arguments, blockPos);
                    continue;
                }
                break;
            } // loop
            if (!hadGet && !hadSet)
            {
                Parser.ErrorAtCurrent("'get' or 'set' expected");        
                break;
            }
            
            Parser.Consume(HopperToken.RBrace, "'}' expected");
            break;
        }
    }
    
    globalDeclaration(<string,string> idToken, string typeString)
    {
        loop
        {
            string identifier;
            bool public;
            if (!normalizeIdentifier(idToken, ref identifier, ref public, true))
            {
                break;   
            }
            if (public)
            {
                Parser.ErrorAtCurrent("member variables must be private");
                break;
            }
            <string> blockPos;
            if (Parser.Check(HopperToken.Assign))
            {
                <string,string> currentToken = Parser.CurrentToken;
                long pos;
                if (Long.TryParse(currentToken["pos"], ref pos))
                {
                    
                }
                Parser.Advance();
                // current position is now at expression to initialize global member
                currentToken = Parser.CurrentToken;
                
                pos = pos + 1; // one beyond '='
                
                blockPos.Append(pos.ToString());
                blockPos.Append(currentToken["line"]);
                blockPos.Append(currentToken["source"]);
            }
            Symbols.AddGlobalMember(identifier, typeString, blockPos);
            
            loop
            {
                if (Parser.Check(HopperToken.SemiColon))
                {
                    Parser.Advance();
                    break;
                }
                if (Parser.Check(HopperToken.EOF))
                {
                    Parser.ErrorAtCurrent(';');
                    break;
                }
                Parser.Advance(); // gobble gobble
            } // loop
            break;
        } // loop
    }
    
    declaration(ref uint curlyDeclarations)
    {
        bool isDelegate;
        if (Parser.Check(HopperToken.Keyword, "delegate"))
        {
            isDelegate = true;
            Parser.Advance(); // delegate
        }
        
        if (Parser.Check(HopperToken.Directive, "#define"))
        {
            Directives.Declaration(); 
        }
        else if (Parser.Check(HopperToken.Directive))
        {
            //OutputDebug("Directive:");
            //DumpCurrent();
            Directives.Directive(); 
        }
        else
        {
            // not directive
            bool allDefined = Directives.IsAllDefined();
            if (!allDefined)
            {                                  
                loop
                {
                    if (Parser.Check(HopperToken.Directive))
                    {
                        
                        //DumpCurrent();
                        // The only directives we don't want to gobble are:
                        // - the #endif that flips allDefined
                        // - the #else that flips allDefined
                        if (Parser.Check(HopperToken.Directive, "#define"))
                        {
                            Directives.Declaration(); 
                        }
                        else
                        {
                            Directives.Directive(); 
                        }
                        allDefined = Directives.IsAllDefined();
                        if (allDefined)
                        {
                            //OutputDebug("allDefined->");
                            break;
                        }
                        else
                        {
                            //OutputDebug("Gobbled");
                            continue;
                        }
                    }
                    if (Parser.Check(HopperToken.EOF))
                    {
                        break;
                    }
                    Parser.Advance(); // gobble gobble
                } // loop
            }                                           
            else if (Parser.Check(HopperToken.LBrace))
            {
                mainMethodDeclaration();
                curlyDeclarations++;
            }
            else if (Parser.Check(HopperToken.Keyword, "const"))
            {
                constDeclaration();
            }
            else if (Parser.Check(HopperToken.Keyword, "enum"))
            {
                enumDeclaration();
                curlyDeclarations++;
            }
            else if (Parser.Check(HopperToken.Keyword, "flags"))
            {
                flagsDeclaration();
                curlyDeclarations++;
            }
            else if (Parser.Check(HopperToken.Keyword, "uses"))
            {
                if (curlyDeclarations > 0)
                {
                    Parser.ErrorAtCurrent("must appear before function, method, property, enum or flags declarations");
                }
                else
                {
                    usesDeclaration();
                }
            }
            else
            {   // global, method or function
                loop
                {
                    <string, string> idToken;
                    bool isFunction = false;
                    bool isProperty = false;
                    bool isMethod = false;
                    bool isGlobal = false;
                               
                    string typeString;
                    bool isType = false;
                  
                    <string,string> peekNextToken = Parser.Peek();
                    if (HopperToken.LParen != Token.GetType(peekNextToken))
                    {           
                        isType = tryParseTypeString(ref typeString);
                    }
                    if (isType)
                    {
                        if (Parser.Check(HopperToken.Identifier) || Parser.Check(HopperToken.DottedIdentifier))
                        {
                            idToken = Parser.CurrentToken;
                            Parser.Advance();
                            if (Parser.Check(HopperToken.LParen))
                            {
                                // return type followed by function id and then '('
                                isFunction = true;
                            }
                            else if (Parser.Check(HopperToken.SemiColon) || Parser.Check(HopperToken.Assign))
                            {
                                // type followed by global id and then ';' or '='
                                isGlobal = true;
                            }
                            else if (Parser.Check(HopperToken.LBrace))
                            {
                                // type followed by property id and then '{'
                                isProperty = true;
                            }
                            else
                            {
                                Parser.Error("'(' or ';' expected");
                                break;
                            }
                        }
                        else
                        {
                            Parser.ErrorAtCurrent("identifier expected, " + typeString);
                            break;
                        }
                    } // isType
                    else if (Parser.Check(HopperToken.Identifier) || Parser.Check(HopperToken.DottedIdentifier))
                    {
                        idToken = Parser.CurrentToken;
                        Parser.Advance();
                        if (Parser.Check(HopperToken.Identifier) || Parser.Check(HopperToken.DottedIdentifier))
                        {
                            // named type (like enum or flags)
                            <string,string> typeToken = idToken;
                            idToken = Parser.CurrentToken;
                            Parser.Advance();
                            // two consecutive identifiers implies:
                            if (Parser.Check(HopperToken.LParen))
                            {
                                // return type followed by function id and then '('
                                isFunction = true;
                            }
                            else if (Parser.Check(HopperToken.SemiColon) || Parser.Check(HopperToken.Assign))
                            {
                                // type followed by global id and then ';' or '='
                                isGlobal = true;
                            }
                            else if (Parser.Check(HopperToken.LBrace))
                            {
                                // type followed by property id and then '{'
                                isProperty = true;
                            }
                            else
                            {
                                Parser.ErrorAtCurrent("'(' or ';' expected");
                                break;
                            }
                            typeString = typeToken["lexeme"];
                        } // two identifiers   
                        else
                        {
                            // single identifier implies method name
                            if (!Parser.Check(HopperToken.LParen))
                            {
                                Parser.ErrorAtCurrent('(');
                                break;
                            }
                            isMethod = true;
                        }
                    } // identifier or two identifiers
                                            
                    if (isFunction)
                    {
                        functionDeclaration(idToken, typeString, isDelegate);
                        isDelegate = false;
                        curlyDeclarations++;
                    }          
                    else if (isProperty)
                    {
                        propertyDeclaration(idToken, typeString);
                        curlyDeclarations++;
                    }
                    else if (isMethod)
                    {
                        methodDeclaration(idToken, isDelegate);
                        isDelegate = false;
                        curlyDeclarations++;
                    }
                    else if (isGlobal)
                    {
                        globalDeclaration(idToken, typeString);
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("function, method, property or global declaration expected");
                    }
                    if (isDelegate)
                    {
                        Parser.ErrorAtCurrent("invalid delegate declaration");
                    }
                    break;
                } // loop
            } // global, method or function
        } // not directive
    } // declaration()
              
                          
  bool buildSymbols(string sourcePath)
  {
      bool success = true;
      unitsParsed.Clear();
      unitsParsed[sourcePath] = true; // make sure program is included in the source paths

      Symbols.New();
      Scanner.New();
      SysCalls.New();
      LibCalls.New();
                        
      bool firstUnit = true;
      loop
      {
          Parser.Reset();
          
          Directives.New();
                           
          Scanner.Load(sourcePath);
          long pos = 0;
          Scanner.Reset(pos, 1, sourcePath);
          Parser.Advance(); // load up first token
          if (firstUnit)
          {
              Parser.Consume(HopperToken.Keyword, "program", "'program' expected");
              Parser.Consume(HopperToken.Identifier, "Program name identifier expected");
          }
          else
          {
              Parser.Consume(HopperToken.Keyword, "unit", "'unit' expected");
              Parser.Consume(HopperToken.Identifier, "Unit name identifier expected");
          }
          if (Parser.HadError)
          {
              success = false;
              break;
          }
          <string,string> previousToken = Parser.PreviousToken;
          currentUnit = previousToken["lexeme"];
          AddNameSpace(currentUnit);
          
          Parser.Consume(HopperToken.LBrace, '{');
          if (Parser.HadError)
          {
              success = false;
              break;
          }
          firstUnit = false;
          bool endedProperly = false;
          uint curlyDeclarations;
          loop
          {
              if (Parser.Check(HopperToken.EOF))
              {
                  break;
              }
              if (Parser.Check(HopperToken.RBrace))
              {
                  endedProperly = true;
                  break;
              }
              if (Parser.HadError)
              {
                  success = false;
                  break;
              }
              declaration(ref curlyDeclarations);
              if (Parser.HadError)
              {
                  success = false;
                  break;
              }
              Parser.ProgressTick(".");
          }   
          if (Parser.HadError)
          {
              // already seen error message
              success = false;
          }
          else if (!endedProperly)
          {
              // can't "Consume" at the end of the file
              Parser.Error('}');
              success = false;
          }
          else
          {
              uint mIndex;
              if (!Symbols.GetFunctionIndex("main", ref mIndex))
              {
                  Parser.Error("program requires entry point");
                  success = false;
              }
              else if (Directives.IsStillOpen)
              {
                  Parser.ErrorAtCurrent("'#endif' expected before end of file");
                  success = false;
              }
          }
          if (Parser.HadError)
          {
              success = false;
              break;
          }
          // any more units to parse?
          sourcePath = "";
          foreach (var kv in unitsParsed)
          {
              if (!kv.value)
              {
                  // not yet parsed
                  sourcePath = kv.key;
                  unitsParsed[sourcePath] = true;
                  break;
              }
          }
          if (sourcePath == "")
          {
              break;
          }
      } 
      return success;
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for PREPROCESS:");
        PrintLn("  PREPROCESS [args] <source file>");    
        PrintLn("    -g <c> <r> : called from GUI, not console");
    }
    {  
        bool success = false;
        loop
        {
          <string> rawArgs = System.Arguments;
          <string> args;
          
          for (uint iArg = 0; iArg < rawArgs.Length; iArg++)
          {
              string arg = rawArgs[iArg];
              if ((arg.Length == 2) && (arg[0] == '-'))
              {
                  arg = arg.ToLower();
                  switch (arg)
                  {
                      case "-g":
                      {
                          uint col;
                            uint row;
                            iArg++;
                            if (UInt.TryParse(rawArgs[iArg], ref col))
                            {
                            }
                            iArg++;
                            if (UInt.TryParse(rawArgs[iArg], ref row))
                            {
                            }
                            Parser.SetInteractive(byte(col), byte(row));
                      }
                      default:
                      {
                          args.Clear();
                          break;
                      }
                  }
              }
              else
              {
                  args.Append(arg);
              }
          }
          
          if (args.Length != 1)
          {
            BadArguments();
            break;
          }
          bool sourceFound;
          string sourcePath = args[0];
          string ext = ".hs";
          string codePath = args[0];
          <string> sourceFolders;
          sourceFolders.Append("/Source/Compiler/");
          sourceFolders.Append("/Source/Debugger/");
          sourceFolders.Append("/Source/Editor/");
          sourceFolders.Append("/Source/Shell/");
          sourceFolders.Append("/Source/Testing/");
          foreach (var sourceFolder in sourceFolders)
          {
              if (File.Exists(ref sourcePath, ref ext, sourceFolder))
              {
                  sourceFound = true;
                  break;
              }
          }
          if (!sourceFound)
          {
              BadArguments();
              break;
          }
          sourcePath = Path.GetFullPath(sourcePath);
          
          long startTime = Millis;
          
          loop
          {
              string extension = Path.GetExtension(sourcePath);
              string jsonPath  = sourcePath.Replace(extension, ".json");
              jsonPath = Path.GetFileName(jsonPath);
              jsonPath = Path.Combine("/Debug/Obj/", jsonPath);
              if (File.Exists(jsonPath))
              {
                  // delete previous so no output on error
                  File.Delete(jsonPath); 
              }
              if (!buildSymbols(sourcePath))
              {
                 // error!
                 break;
              }
              
              if (!Symbols.DeferredValidation())
              {
                 // error!
                 break;
              }
              if (!Parser.IsInteractive())
              {
                  PrintLn();
                  PrintLn("Exporting: '" + jsonPath + "'");
              }
              if (!Symbols.Export(jsonPath))
              {
                  break;
              }
              if (!Parser.IsInteractive())
              {
                  Print("Success. ", Color.ProgressText, Color.ProgressFace);
                  long elapsedTime = Millis - startTime;
                  float seconds = elapsedTime / 1000.0;
                  PrintLn("  " +seconds.ToString() + "s", Color.ProgressHighlight, Color.ProgressFace); 
              }
              else
              {
                  Parser.ProgressDone();
              }
              success = true;
              break;
          } // main loop
          break;
        } // arguments loop
        if (!success)
        {
            Diagnostics.SetError(0x0E);
        }
      
    }
}

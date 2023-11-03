unit Tokenizer
{
    // #### globals at the top of the file so we can keep track of them:
    uint     gLineNumber;       // current line number in tokenizer
    // #### end of globals
    
    uses "/Source/6502/Samples/GIBL/Errors"
    
    uses "/Source/6502/Samples/GIBL/Source"      // source code lines
    uses "/Source/6502/Samples/GIBL/Expression"  // recursive descent expression parser
    uses "/Source/6502/Samples/GIBL/HopperCode"  // code related to Hopper VM byte code
    
    uint LineNumber { get { return gLineNumber; }  set { gLineNumber = value; } }
    
    char Peek(ref string content)
    {   
        while (content.StartsWith(' '))
        {
            content = content.Substring(1);
        }
        return content[0].ToUpper();
    }
    char PeekNth(ref string content, uint nth)
    {   
        while (content.StartsWith(' '))
        {
            content = content.Substring(1);
        }
        return (content.Length > nth ? content[nth] : char(0)).ToUpper();
    }
    Consume(char token, ref string content)
    {
        while (content.StartsWith(' '))
        {
            content = content.Substring(1);
        }
        if (!content.StartsWith(token))
        {
            char ltoken = token.ToLower();
            if (!content.StartsWith(ltoken))
            {
                if (content.Length != 0)
                {
                    Error(3, content[0]);
                }
                else
                {
                    Error(9); // Syntax error
                }
                return;
            }
        }
        content = content.Substring(1);
        while (content.StartsWith(' '))
        {
            content = content.Substring(1);
        }
    }
    TrimFront(uint trimChars, ref string content)
    {
        content = content.Substring(trimChars).TrimLeft();
    }
    
    TokenizeImmediate()
    {
        <string> localLoopStack; 
        <uint>   localLoopLineStack;
        loop
        {
            gLineNumber = 10000;
            string source = Source.GetLine(gLineNumber);
            HopperCode.StartNewLine(gLineNumber);
            Tokenize(source, ref localLoopStack, ref localLoopLineStack);
            if (Condition != Conditions.None) { break; }
            
            gLineNumber = 10001;
            source = Source.GetLine(gLineNumber);
            HopperCode.StartNewLine(gLineNumber);
            Tokenize(source, ref localLoopStack, ref localLoopLineStack);
            if (Condition != Conditions.None) { break; }
            
            gLineNumber = 10002;
            source = Source.GetLine(gLineNumber);
            HopperCode.StartNewLine(gLineNumber);
            Tokenize(source, ref localLoopStack, ref localLoopLineStack);
            if (Condition != Conditions.None) { break; }
            
            break;
        } // loop
        
        if (Condition == Conditions.None)
        {
            if (localLoopStack.Length > 0)
            {
                string next = localLoopStack[localLoopStack.Length-1];
                uint startLine = localLoopLineStack[localLoopLineStack.Length-1];
                if (next.StartsWith("UNTIL"))
                {
                    Error(19); // DO without UNTIL
                }
                else
                {
                    Error(13); // FOR without NEXT
                }
            }
        }
        if (Condition == Conditions.None)
        {
            HopperCode.Finish(true); // line number fixups and EXIT
        }
        
    }
    
    Tokenize()
    {
        <string> loopStack;         // for parsing FOR .. NEXT and DO .. UNTIL nesting
        <uint>   loopLineStack;
    
        // reset
        gLineNumber = 0;
        HopperCode.Clear();
        
        // clear TOP ram if it was used to give tokenizer the best chance
        // (next program may not use TOP)
        RAM.Free();  
        Write(' ');
        long start = Millis;
        loop
        {
            gLineNumber = Source.GetNextLine(gLineNumber);
            if (gLineNumber > Source.LastLine)
            {
                break; // done
            }
#ifdef DEBUG            
            long lineStart = Millis;
            loop
            {
                Write(Source.GetLine(gLineNumber).Pad(' ', 60));
                break;
            }
#endif            
            HopperCode.StartNewLine(gLineNumber);
            Tokenize(Source.GetLine(gLineNumber), ref loopStack, ref loopLineStack);
            if (Condition != Conditions.None) { break; }
            
#ifdef DEBUG           
            loop
            { 
                WriteLn(" " + (Millis - lineStart).ToString() + "ms " 
                            + Memory.Available().ToString() + " bytes left, " 
                            + HopperCode.CurrentCodeSize.ToString());
                break;
            }
#else
            Write('.');
#endif            
        } // loop
        
        if (Condition == Conditions.None)
        {
            if (loopStack.Length > 0)
            {
                string next = loopStack[loopStack.Length-1];
                uint startLine = loopLineStack[loopLineStack.Length-1];
                if (next.StartsWith("UNTIL"))
                {
                    Error(19, char(0), startLine); // DO without UNTIL
                }
                else
                {
                    Error(13, char(0), startLine); // FOR without NEXT
                }
            }
        }
        if (Condition == Conditions.None)
        {
#ifdef DEBUG            
            long postStart = Millis;
            string source;
            Write(source.Pad(' ', 60));
#endif                        
            HopperCode.Finish(false); // line number fixups and EXIT
#ifdef DEBUG            
            WriteLn(" " + (Millis - postStart).ToString() + "ms");
#else
            Write('.');
#endif                        
        }
        
        WriteLn(" " + (Millis - start).ToString() + "ms");
    }
    
    enum Basic
    {
        Undefined, Rem,
        Goto, Gosub, GotoIfFalse, Return, End, 
        PokeB, PokeW, Poke1,
        GetVariableRef, SetVariable, SetVariableRef,
        Print, PrintChar, PrintHex, PrintStringRef,
        RndSeed, Input,
    }
    
    flags ParseFlags
    {
        None,
        For   = 0x0001,
        Next  = 0x0002,
        Until = 0x0004,
        Do    = 0x0008,
        
        IgnoreRestOfLine = 0x0010,
    }
    flags ExpressionType
    {
        Undefined,
        Integer,
        Boolean,
    }
    
    Tokenize(string content, ref <string> loopStack, ref <uint> loopLineStack)
    {
        Basic prevInstruction;
        ExpressionType expressionType;
        content = content + ":";
        loop
        {
            Basic basicInstruction = Basic.Undefined;
            ParseFlags parseFlags;
            loop
            {
                byte trimLength = 0;
                uint length = content.Length;
                if (length > 0)
                {
                    string prefix = content.Substring(0,6).ToUpper();
                    if (prefix == "RETURN")
                    {
                        trimLength = 6;
                        basicInstruction = Basic.Return;
                        parseFlags = parseFlags | ParseFlags.IgnoreRestOfLine;
                    }
                    else
                    {
                        prefix = prefix.Substring(0,5);
                        if (prefix == "PRINT")
                        {
                            trimLength = 5;
                            basicInstruction = Basic.Print;
                        }
                        else if (prefix == "GOSUB")
                        {
                            trimLength = 5;
                            basicInstruction = Basic.Gosub;
                        }
                        else if (prefix == "INPUT")
                        {
                            trimLength = 5;
                            basicInstruction = Basic.Input;
                        }
                        else if (prefix == "UNTIL")
                        {
                            trimLength = 5;
                            parseFlags = parseFlags | ParseFlags.Until;
                        }
                        else
                        {
                            prefix = prefix.Substring(0,4);
                            if (prefix == "GOTO")
                            {
                                trimLength = 4;
                                basicInstruction = Basic.Goto;
                                
                                // A goto makes anything that follows it on the line unreachable
                                parseFlags = parseFlags & ~ParseFlags.IgnoreRestOfLine; // parse rest of line
                            }
                            else if (prefix == "NEXT")
                            {
                                trimLength = 4;
                                parseFlags = parseFlags | ParseFlags.Next;
                            }
                            else
                            {
                                prefix = prefix.Substring(0,3);
                                if (prefix == "FOR")
                                {
                                    trimLength = 3;
                                    basicInstruction = Basic.SetVariable;
                                    parseFlags = parseFlags | ParseFlags.For;
                                }
                                else if (prefix == "LET")
                                {
                                    trimLength = 3;
                                    basicInstruction = Basic.SetVariable;
                                }
                                else if (prefix == "END")
                                {
                                    trimLength = 3;
                                    basicInstruction = Basic.End;
                                    parseFlags = parseFlags | ParseFlags.IgnoreRestOfLine;
                                }
                                else if (prefix == "REM")
                                {
                                    trimLength = 3;
                                    basicInstruction = Basic.Rem;
                                    parseFlags = parseFlags | ParseFlags.IgnoreRestOfLine;
                                }
                                else if (prefix == "RND")
                                {
                                    trimLength = 3;
                                    basicInstruction = Basic.RndSeed;
                                }
                                else if (prefix == "VDU")
                                {
                                    trimLength = 3;
                                    basicInstruction = Basic.PrintChar;
                                }
                                else
                                {
                                    prefix = prefix.Substring(0,2);
                                    if (prefix == "IF")
                                    {
                                        trimLength = 2;
                                        basicInstruction = Basic.GotoIfFalse;
                                    }
                                    else if (prefix == "DO")
                                    {
                                        trimLength = 2;
                                        parseFlags = parseFlags | ParseFlags.Do;
                                    }
                                    else if (prefix == "PR")
                                    {
                                        trimLength = 2;
                                        basicInstruction = Basic.Print;
                                    }
                                    else
                                    {
                                        if (prefix[0] == '!')
                                        {
                                            trimLength = 1;
                                            basicInstruction = Basic.PokeW;
                                        }
                                        else if (prefix[0] == '?')
                                        {
                                            trimLength = 1;
                                            basicInstruction = Basic.PokeB;
                                        } 
                                        else if (prefix[0] == '@')
                                        {
                                            trimLength = 1;
                                            basicInstruction = Basic.Poke1;
                                        } 
                                        // 1 letter command
                                    }
                                    // 2 letter command
                                } 
                                // 3 letter command
                            } 
                            // 4 letter command
                        } 
                        // 5 letter command
                    } 
                    // 6 letter command
                } // if length > 0
                
                if (trimLength > 0)
                {
                    TrimFront(trimLength, ref content);
                }
                else if (length > 0)
                {
                    char token = content[0];
                    string pointerPrefix = "";
                    if ((token == '$') && (length > 1))
                    {
                        token = content[1];
                    }
                    if (token.IsUpper() || token.IsLower())
                    {
                        basicInstruction = Basic.SetVariable;
                    }
                    else
                    {
                        WriteLn("'" + content + "'");
                        Error(3, content[0]);
                    }
                }
                break;
            } // loop
            
            if (ParseFlags.Until == parseFlags & ParseFlags.Until)
            {
                if (loopStack.Length == 0) { Error(20);  break; }            // UNTIL without DO
                string untilContent = loopStack[loopStack.Length-1];
                if (!untilContent.StartsWith("UNTIL")) { Error(20); break; } // UNTIL without DO
                loopStack.Remove(loopStack.Length-1);
                loopLineStack.Remove(loopLineStack.Length-1);
                HopperCode.InsertBreakCheck();
                expressionType = ParseExpression(ref content);
                if (Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Boolean) { Error(23); break; }          // Boolean expression expected
                uint iA; if (untilContent.IndexOf('a', ref iA)) {}
                untilContent = untilContent.Substring(iA+1);
                int address; if (Int.TryParse(untilContent, ref address)) {}
                HopperCode.UntilJump(address); // is always Instruction.JZW
            }
            else if (ParseFlags.Next == parseFlags & ParseFlags.Next)
            {
                string variableName;
                if (content.StartsWith('$'))
                {
                    TrimFront(1, ref content);
                    basicInstruction = Basic.GetVariableRef;
                    variableName = "$";
                }
                variableName = variableName + content[0];
                variableName = variableName.ToUpper();
                if (loopStack.Length == 0)  { Error(14); break; }           // NEXT without FOR
                string nextContent = loopStack[loopStack.Length-1];
                if (!nextContent.StartsWith("NEXT"))  { Error(14); break; } // NEXT without FOR
                loopStack.Remove(loopStack.Length-1);
                loopLineStack.Remove(loopLineStack.Length-1);
                string replace = "NEXT " + variableName + ":";
                if (!nextContent.StartsWith(replace)) { Error(14); break; } // NEXT without FOR
                nextContent = nextContent.Replace(replace, "");
                uint iColon; bool ok = content.IndexOf(':', ref iColon);
                content = content.Substring(iColon);
                content      = ":" + nextContent + content;
            }
            else if (basicInstruction == Basic.GotoIfFalse)
            {
                //HopperCode.InsertBreakCheck();
                expressionType = ParseExpression(ref content);
                if (Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Boolean) { Error(23); break; }          // Boolean expression expected
                
                // if false, just go to next line
                HopperCode.GotoLine(basicInstruction, gLineNumber+1, true); // true -> JZW
                
                // optional "THEN"
                string upperContent = content.Substring(0,4).ToUpper();
                if (upperContent == "THEN")
                {
                    TrimFront(4, ref content);
                }
                
                content = ":" + content;
            }
            else if (basicInstruction == Basic.Input)
            {
                bool isString;
                if (content.StartsWith('$'))
                {
                    TrimFront(1, ref content);
                    isString = true;
                }
                char ch = content[0];
                if (!ch.IsLetter()) { Error(25); break; }   // variable name expected
                ch = ch.ToUpper();
                byte variableIndex  = byte(ch) - 65;
                TrimFront(1, ref content);
                HopperCode.Input(byte(variableIndex), isString);
            }
            else if (basicInstruction == Basic.SetVariable)
            {
                string variableName;
                if (content.StartsWith('$'))
                {
                    TrimFront(1, ref content);
                    basicInstruction = Basic.SetVariableRef;
                    variableName = "$";
                }
                char ch = content[0];
                if (!ch.IsLetter()) { Error(25); break; }   // variable name expected
                ch = ch.ToUpper();
                variableName = variableName + ch;
                
                byte variableIndex  = byte(ch) - 65;
                
                TrimFront(1, ref content);
                
                if (content.StartsWith("++:")) 
                {
                    HopperCode.IncVariable(variableIndex);
                    TrimFront(2, ref content);
                }
                else
                {
                    Consume('=', ref content);
                    if (Condition != Conditions.None) { break; }
                    if (variableName.StartsWith('$') && content.StartsWith('"'))
                    {
                        // $Z=".,'~=+:;*%&$OXB#@ "
                        content = content.Substring(1);
                        string str;
                        while (!content.StartsWith('"') && (content.Length != 0))
                        {
                            str = str + content[0];
                            content = content.Substring(1);
                        }
                        if (!content.StartsWith('"')) { Error(10); break; }   // " expected
                        HopperCode.PokeString(variableIndex, str);
                        content = content.Substring(1);
                    }
                    else
                    {
                        expressionType = ParseExpression(ref content);
                        if (Condition != Conditions.None) { break; }
                        if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                        if (basicInstruction == Basic.SetVariableRef)
                        {
                            // $<v> = <expr>
                            HopperCode.SetVariableRef(variableIndex);
                            break;
                        }
                        
                        HopperCode.SetVariable(variableIndex);
                        
                        if (ParseFlags.For == parseFlags & ParseFlags.For)
                        {
                            TrimFront(0, ref content);
                            if (   (content.Length < 2)
                                || ((content[0] != 'T') && (content[0] != 't'))
                                || ((content[1] != 'O') && (content[1] != 'o'))
                               )
                            {
                                Error(12); // TO expected
                            }
                            // "TO <n>" or "TO <n> STEP <m>"
                            TrimFront(2, ref content);
                            
                            uint iColon;
                            bool ok = content.IndexOf(':', ref iColon);
                            
                            // "<n>" or "<n> STEP <m>"
                            string limit = content.Substring(0, iColon).ToUpper();
                            string step;
                            content = content.Substring(iColon);
                            
                            <string> limitParts = limit.Split("STEP");
                            
                            string incrementCode = variableName + "++:";
                            if (limitParts.Length == 2)
                            {
                                limit = limitParts[0];
                                step  = limitParts[1];
                                incrementCode = variableName + "=" + variableName + "+" + step + ":";
                            }
                            uint nextInstruction = HopperCode.CurrentCodeSize;
                            HopperCode.ResetPeepholeBoundary();
                            incrementCode = incrementCode + "IF " + variableName + " <= " + limit
                                                         + " GOTO a" + nextInstruction.ToString();
                            
                            string nextKey = "NEXT " + variableName + ":" + incrementCode;
                            //uint used = Memory.Available();
                            loopStack.Append(nextKey);
                            loopLineStack.Append(gLineNumber);
                            //used = used - Memory.Available();
                            //Write("NN" + used.ToString() + " ");
                        }
                    }
                }
            }
            else if (ParseFlags.Do == parseFlags & ParseFlags.Do)
            {
                uint nextInstruction = HopperCode.CurrentCodeSize;
                HopperCode.ResetPeepholeBoundary();
                loopStack.Append("UNTIL:GOTOFALSE a" + nextInstruction.ToString());
                loopLineStack.Append(gLineNumber);
                //Write("UU");
            }
            else if (basicInstruction == Basic.PrintChar)
            {
                loop
                {
                    expressionType = ParseExpression(ref content);
                    if (Condition != Conditions.None) { break; }
                    if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                    HopperCode.PrintChar(); // assumes LSB = ch, MSB = 0 (null) is on the stack
                    
                    content = content.Trim();
                    if (content.StartsWith(','))
                    {
                        // optionally VDU can have more than one comma separated expression
                        TrimFront(1, ref content);
                        continue;
                    }
                    break;
                } // loop
            }
            else if (basicInstruction == Basic.RndSeed)
            {
                loop
                {
                    Consume('=', ref content);
                    if (Condition != Conditions.None) { break; }
                    expressionType = ParseExpression(ref content);
                    if (Condition != Conditions.None) { break; }
                    if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                    HopperCode.Seed();
                    break;
                } // loop
            }
            else if (basicInstruction == Basic.Print)
            {
                bool suppressEOL;
                loop
                {
                    uint length = content.Length;
                    if ((length != 0) && (content[0] == '"'))
                    {
                        // immediate string
                        uint i=1;
                        for (; i < length; i++)
                        {
                            if (content[i] == '"')
                            {
                                break;
                            }
                        }
                        if ((i>=length) || (content[i] != '"'))
                        {
                            Error(10); // " expected
                            break;
                        }
                        if (i > 1) // not ""
                        {
                            HopperCode.PrintString(content.Substring(1,i-1));
                        }
                        content = content.Substring(i+1);
                    }
                    else
                    {
                        if (length != 0)
                        {
                            if (content[0] == '~')
                            {
                                //   ~ prefix for fixed width 4 digit hex output
                                TrimFront(1, ref content);
                                basicInstruction = Basic.PrintHex;
                            }
                            else if (content[0] == '$')
                            {
                                //   $ prefix to treat variable as pointer to string
                                TrimFront(1, ref content);
                                basicInstruction = Basic.PrintStringRef;
                            }
                        }
                        
                        // print integer expression
                        expressionType = ParseExpression(ref content);
                        if (Condition != Conditions.None) { break; }
                        if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                        
                        if (basicInstruction == Basic.Print)
                        {
                            HopperCode.PrintInt();
                        }
                        else if (basicInstruction == Basic.PrintHex)
                        {
                            HopperCode.PrintHex();
                        }
                        else
                        {
                            //   $ prefix to treat variable as pointer to string
                            HopperCode.PrintRef();
                        }
                    }
                    content = content.Trim();
                    // , means pump out a space
                    if (content.StartsWith(','))
                    {
                        HopperCode.PrintString(" ");
                        TrimFront(1, ref content);
                    }
                    // ; means suppress newline (can only be at end of line)
                    if (content.StartsWith(';'))
                    {
                        TrimFront(1, ref content);
                        suppressEOL = true;
                        if (!content.StartsWith(':'))
                        {
                            Error(11); // EOL expected
                            break;
                        }
                    }
                    if (content.StartsWith(':'))
                    {
                        break;
                    }
                } // loop
                if ((Condition == Conditions.None) && !suppressEOL)
                {
                    HopperCode.PrintString("" + char(0x0D)); // newline
                }
            } // Print
            else if ((basicInstruction == Basic.PokeW) || (basicInstruction == Basic.PokeB) || (basicInstruction == Basic.Poke1))
            {
                // ?(<expr>) = <expr>  or !(<expr>) = <expr> or @(<expr>,<expr>) = <expr>
                byte argumentsExpected = 1;
                if (basicInstruction == Basic.Poke1)
                {
                    argumentsExpected = 2;
                }
                expressionType = ParseArguments(ref content, argumentsExpected);
                if (Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                
                Consume('=', ref content);
                if (Condition != Conditions.None) { break; }
                
                expressionType = ParseExpression(ref content);
                if (Condition != Conditions.None) { break; }
                if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                if (basicInstruction == Basic.PokeW)
                {
                    HopperCode.PokeW();
                }
                else if (basicInstruction == Basic.PokeW)
                {
                    HopperCode.PokeB();
                }
                else
                {
                    HopperCode.PokeBit();
                }
            } // Poke
            else if ((basicInstruction == Basic.Goto) || (basicInstruction == Basic.Gosub))
            {
                bool noFixup;
                if (content.StartsWith('a'))
                {
                    noFixup = true;
                    TrimFront(1, ref content);
                    HopperCode.InsertBreakCheck(); // FOR loop
                }
                expressionType = ParseExpression(ref content);
                if (expressionType != ExpressionType.Integer) { Error(15); break; } // Integer expression expected
                if (Condition != Conditions.None) { break; }
                
                if (noFixup)
                {
                    // FOR loop GOTO which is already a resolved address
                    if (!IsPushImmediate(LastInstruction))
                    { 
                        Error(21, 'e'); // Internal Error - should never arrive here with <expression> for line number
                        break; 
                    } 
                    if (basicInstruction == Basic.Gosub)
                    {
                        Error(21, 'e'); // Internal Error - should never arrive here for GOSUB
                        break; 
                    }
                    HopperCode.GotoAddress(Basic.Goto);
                }
                else
                {
                    HopperCode.GotoLine(basicInstruction, false); // false -> JW
                }
            } // Goto | Gosub
            else if (basicInstruction == Basic.Return)
            {
                HopperCode.Return(); 
            }
            else if (basicInstruction == Basic.End)
            {
                HopperCode.End();
            }
            else if (basicInstruction == Basic.Rem)
            {
                HopperCode.Rem();
            }
            
            prevInstruction = basicInstruction;
            TrimFront(0, ref content);
            if (!content.StartsWith(':') && (basicInstruction != Basic.Rem))
            {
                if (content.Length > 0)
                {
                    Error(9, content[0]);
                }
                else
                {
                    Error(9);
                }
                break;
            }
            if ((content.Length == 1) || (ParseFlags.IgnoreRestOfLine == parseFlags & ParseFlags.IgnoreRestOfLine)) // last trailing ':'
            {
                break;
            }
            TrimFront(1, ref content); // trim the front ':'
        } // loop
    }
}

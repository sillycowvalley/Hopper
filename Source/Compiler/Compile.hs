program Compile
{
    uses "/Source/System/System"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"
    
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/Symbols"
    
    uses "/Source/Compiler/Types"
    uses "/Source/Compiler/Expression"
    uses "/Source/Compiler/Constant"
    
    uses "/Source/Compiler/Directives"
    
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/CodeGen/CodeStream"
    uses "/Source/Compiler/CodeGen/Block"
    
    // Notes:
    //
    //   RUNTIME:
    //     - memory allocator (abstract?)
    //     - run from disk? cache methods? load constants?
    //
    //   TODO:
    //     - count hits on method calls to assign method numbers (CALLB vs CALLW)
    //     - implement general '.' expression extensions?
    //
    //   EDIT:       (launched by running .hs)
    //   PREPROCESS: 
    //   COMPILE:    (launched by running .obj)
    //   DASM:
    //     - if launched from <xxx>.cmd, each error output to <xxx>.err
    //
    //   CODEGEN:    (launched by running .code)
    //     - if launched from <xxx>.cmd, each error output to <xxx>.err
    //
    //   ERRORS:     (launched by running .err)
    //     - .err -> list of clickable links
    //
    //   RUN:     (launched by running .hexe2)
    //     - -D launch debugger
    //
    //   .cmd
    //     - arguments? syntax?
    //     - exit codes from apps? on error, stop?
    //     - HOPPER.hexe -> PREPROCESS, COMPILE, DASM, CODEGEN, ERRORS
    //
    // - verify uniqueness of case values (Zopper OpCode)
    //
    // - add Build menu to editor
    //   - progress popup / console?
    //   - clickable error list?
    // - edit should default to .hs from command line
    
    uint iCurrentOverload;
    
    uint spaces; 
    DebugComment(string caption, bool prev)
    {
        string content;
        for (uint i=0; i < spaces; i++)
        {
            content = content + "..";
        }
        <string,string> lineToken = CurrentToken;
        if (prev)
        {
            lineToken = PreviousToken;
        }
        content = content + caption + ":" + lineToken["line"] + "," + lineToken["pos"];
        if (lineToken["type"] == "Keyword")
        {
            content = content + ",'" + lineToken["lexeme"] + "'";
        }
        else
        {
            content = content + "," + lineToken["type"];
        }
        OutputDebug(content);
    } 
     
    
    bool compileIfStatement()
    {
        bool success = false;
        bool wasElse = false;
        <uint> jumpEnds;
        loop
        {
            CodeStream.InsertDebugInfo(false); // could be a 2nd 'else if'
            
            Parser.Advance(); // if
            
            Parser.Consume(HopperToken.LParen, "'(' expected");
            if (Parser.HadError)
            {
                break;
            }
            
            string ifCheckType = CompileExpression("bool");
            if ("bool" != ifCheckType)
            {
                Parser.Error("boolean expression expected, (was '" + ifCheckType + "')");
                break;
            }      
            
            Parser.Consume(HopperToken.RParen, "')' expected");
            if (Parser.HadError)
            {
                break;
            }
            // if false jump past
            uint jumpPast = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JZW, uint(0));
            
            Block.PushBlock(false); // not a loop context
            compileBlock();
            Block.PopBlock();
            if (Parser.HadError)
            {
                break;
            }
            
            CodeStream.InsertDebugInfo(true);
            
            // jump end (past a potential "else" block)
            uint jumpEnd = CodeStream.NextAddress;
            jumpEnds.Append(jumpEnd);
            CodeStream.AddInstruction(Instruction.JW, uint(0));
// past:    
            uint pastAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpPast, pastAddress);        
                        
            if (Parser.Check(HopperToken.Keyword, "else"))
            {
                wasElse = true; // there was at least one else clause
                Advance(); // else
                if (Parser.Check(HopperToken.Keyword, "if"))
                {
                    continue; // else if
                }   
                // final else block
                Block.PushBlock(false); // not a loop context
                compileBlock();
                Block.PopBlock();
            }
// end:     
            if (wasElse)
            {       
                foreach (var jump in jumpEnds)
                {
                    CodeStream.PatchJump(jump, CodeStream.NextAddress);
                }
            }
            else
            {
                // simple if with no "else" clause/s
                CodeStream.PopTail(3);
                CodeStream.PatchJump(jumpPast, pastAddress-3);
            }
            success = true;                    
            break;
        }
        return success;
    }
    
    bool compileReturn()
    {
        bool success = false;
        loop
        {
            Parser.Advance(); // return
            string returnType = "void";
            <string,variant> blockContext = Block.GetMethodBlock();
            uint returnBytes = 0;
            if (blockContext.Count > 0)
            {
                if (blockContext.Contains("returntype"))
                {
                    returnType = blockContext["returntype"];
                    string actualReturnType = CompileExpression(returnType);
                    if (actualReturnType != returnType)
                    {
                        if (Types.AutomaticUpCastTop(actualReturnType, returnType))
                        {
                            // ok
                        }
                        else
                        {
                            Parser.Error("invalid return type, (was '" + actualReturnType + "', expected '"+ returnType +"')");
                            break;
                        }
                    }
                    returnBytes = 2;       
                }
            }
            if (CodeStream.CheckedBuild)
            {            
                // if you pop locals and the return value, sp == bp
                uint localsToPop = Block.GetLocalsToPop(false, false) + returnBytes;
                CodeStream.AddInstruction(Instruction.TESTBPB, byte(localsToPop));
            }
            
            // bytesToPop = locals + arguments
            uint bytesToPop = Block.GetLocalsToPop(true, false);
            if (bytesToPop == 0)
            {
                // if there is a return value, then it is already exactly where it needs to be
                CodeStream.AddInstruction(Instruction.RET0);
            }
            else if (returnBytes > 0)
            {
                if (bytesToPop < 256)
                {
                    CodeStream.AddInstruction(Instruction.RETRETB, byte(bytesToPop));
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.RETRETW, bytesToPop);
                }
            }
            else
            {
                if (bytesToPop < 256)
                {
                    CodeStream.AddInstruction(Instruction.RETB, byte(bytesToPop));
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.RETW, bytesToPop);
                }
            }
            success = true;
            break;
        }
        return success;
    }
    
    bool compileBreak()
    {
        bool success = true;
        
        Parser.Advance(); // break;
        
        // - pop all locals till inner loop
        uint bytesToPop = Block.GetBytesToPop(true, false);
        if (bytesToPop > 0)
        {
            if (bytesToPop > 255)
            {
                Die(0x0B); // limit
            }
            CodeStream.AddInstruction(Instruction.DECSP, byte(bytesToPop));
        }
               
        // - jump to current inner loop 'exit'
        uint breakJump = CodeStream.NextAddress;
        CodeStream.AddInstruction(Instruction.JW, uint(0));
        if (!Block.AddBreakPatch(breakJump))
        {
            Parser.ErrorAtCurrent("'break' must be inside loop block");
            success = false;
        }
        
        return success;
    }
    
    bool compileContinue()
    {
        bool success = true;
        
        Parser.Advance(); // continue;
        
        // - pop all locals till inner loop
        uint bytesToPop = Block.GetBytesToPop(true, true);
        if (bytesToPop > 0)
        {
            if (bytesToPop > 255)
            {
                Die(0x0B); // limit
            }
            CodeStream.AddInstruction(Instruction.DECSP, byte(bytesToPop));
        }
        // - jump to current inner loop 'next'
        uint continueJump = CodeStream.NextAddress;
        CodeStream.AddInstruction(Instruction.JW, uint(0));
        if (!Block.AddContinuePatch(continueJump))
        {
            Parser.ErrorAtCurrent("'continue' must be inside loop block");
            success = false;
        }
        
        return success;
    }
    
    bool compileWhile()
    {
        bool success = false;
        loop
        {
            Parser.Advance(); // while
            
            Parser.Consume(HopperToken.LParen, "'(' expected");
            if (Parser.HadError)
            {
                break;
            }

// continueAddress:    
            uint continueAddress = CodeStream.NextAddress;                    
            
            string whileCheckType = CompileExpression("bool");
            if ("bool" != whileCheckType)
            {
                Parser.Error("boolean expression expected, (was '" + whileCheckType + "')");
                break;
            }      
            
            Parser.Consume(HopperToken.RParen, "')' expected");
            if (Parser.HadError)
            {
                break;
            }
            // if false jump exit
            uint jumpExit = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JZW, uint(0));
            
            Block.PushBlock(true); // loop block context
            
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            CodeStream.InsertDebugInfo(true);
            
            CodeStream.AddInstructionJump(Instruction.JW, continueAddress);
// exit:            
            uint breakAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpExit, breakAddress);
            
            Block.PopBlock(continueAddress, breakAddress);
            
            if (Parser.HadError)
            {
                break;
            }
            success = true;                    
            break;
        }
        return success;
    }
    
    bool compileLoop()
    {
        bool success = false;
        loop
        {
            Parser.Advance(); // loop

            Block.PushBlock(true); // loop context
            
            uint continueAddress = CodeStream.NextAddress;
            
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            CodeStream.InsertDebugInfo(true);
            
            CodeStream.AddInstructionJump(Instruction.JW, continueAddress);
            uint breakAddress = CodeStream.NextAddress;
            
            Block.PopBlock(continueAddress, breakAddress);
            if (Parser.HadError)
            {
                break;
            }
            success = true;                    
            break;
        }
        return success;
    }
    
    bool compileFor()
    {
        bool success = false;
        loop
        {
            Block.PushBlock(true); // new loop block context
            
            Parser.Advance(); // for
            Parser.Consume(HopperToken.LParen, "'(' expected");
            if (Parser.HadError)
            {
                break;
            }
            
            if (!compileStatement(false, false)) // initialization statement
            {
                break;
            }
            
            uint loopAddress = CodeStream.NextAddress;
            string exitCheckType = CompileExpression("bool");
            if ("bool" != exitCheckType)
            {
                Parser.Error("boolean expression expected, (was '" + exitCheckType + "')");
                break;
            }
            Parser.Consume(HopperToken.SemiColon, "';' expected");
            if (Parser.HadError)
            {
                break;
            }
            uint jumpExit = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JZW, uint(0));
            
            <byte> mainStream = CodeStream.CurrentStream;
            
            CodeStream.New();
            if (!compileStatement(true, false)) // increment statement (no semicolon)
            {
                break;
            }
            <byte> incrementStream = CodeStream.CurrentStream;
            CodeStream.New(mainStream);
            
            Parser.Consume(HopperToken.RParen, "')' expected");
            if (Parser.HadError)
            {
                break;
            }
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            if (Parser.HadError)
            {
                break;
            }
            
            CodeStream.InsertDebugInfo(true);
            
            uint continueAddress = CodeStream.NextAddress;
            CodeStream.AppendCode(incrementStream);
            CodeStream.AddInstructionJump(Instruction.JW, loopAddress);
// exit:                      
            uint breakAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpExit, breakAddress);
            Block.PopBlock(continueAddress, breakAddress);
            
            success = true;
            break;
        } // loop
        
        
        return success;
    }
    
    bool compileForEachString(string identifier)
    {
        bool success = false;
        loop
        {
            Block.PushBlock(true); // new loop block context
            Block.AddLocal("string", identifier+ "_c");    // string collection object (expression in [top])
            CodeStream.AddInstruction(Instruction.PUSHI0); // char: identifier
            CodeStream.AddInstruction(Instruction.PUSHI0); // uint: identifier_i
            // kv has same type as collection if dictionary
            Block.AddLocal("char", identifier);
            Block.AddLocal("uint", identifier+"_i");
            
            
            bool isRef;
            byte identifierOffset = CodeStream.IntToByte(Block.GetOffset(identifier, ref isRef));
            byte iteratorOffset   = CodeStream.IntToByte(Block.GetOffset(identifier+"_i", ref isRef));
            byte collectionOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_c", ref isRef));
//next:            
            uint nextAddress = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // string collection object
            CodeStream.AddInstructionSysCall0("String", "Length_Get");
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            CodeStream.AddInstruction(Instruction.EQ);
            uint jumpExit = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JNZW, uint(0));
            
            // load identifier using identifier_i
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // string collection object
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            CodeStream.AddInstructionSysCall0("String", "GetChar");
            
            // identifier <- collection[iterator]
            CodeStream.AddInstruction(Instruction.POPLOCALB, identifierOffset);
            
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            if (Parser.HadError)
            {
                break;
            }
//continue:
            uint continueAddress = CodeStream.NextAddress;            
            CodeStream.InsertDebugInfo(true); 
            
            // iterator++            
            CodeStream.AddInstruction(Instruction.INCLOCAL, iteratorOffset);
            
            CodeStream.AddInstructionJump(Instruction.JW, nextAddress);
// exit:            
            uint breakAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpExit, breakAddress);
            Block.PopBlock(continueAddress, breakAddress);
            
            success = true;   
            break;
        }
        return success;
    }
    
    bool compileForEachList(string identifier, string iteratorType, string collectionType)
    {
        bool success = false;
        loop
        {
            Block.PushBlock(true); // new loop block context
            Block.AddLocal(collectionType, identifier+ "_c");    // list collection object (expression in [top])
            CodeStream.AddInstruction(Instruction.PUSHI0); // iteratorType: identifier
            CodeStream.AddInstruction(Instruction.PUSHI0); // uint: identifier_i
            
            Block.AddLocal(iteratorType, identifier);
            Block.AddLocal("uint", identifier+"_i");
            
            bool isReference = !Types.IsValueType(iteratorType);
            
            bool isRef;
            byte identifierOffset = CodeStream.IntToByte(Block.GetOffset(identifier, ref isRef));
            byte iteratorOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_i", ref isRef));
            byte collectionOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_c", ref isRef));

// next:
            uint nextAddress = CodeStream.NextAddress;            
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // list collection object
            CodeStream.AddInstructionSysCall0("List", "Length_Get");
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            CodeStream.AddInstruction(Instruction.EQ);
            uint jumpExit = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JNZW, uint(0));
            
            // load identifier using identifier_i
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // list collection object
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            if (iteratorType == "variant")
            {
                // int type is not known at compile time
                CodeStream.AddInstructionSysCall0("List", "GetItemAsVariant");
            }
            else
            {
                // item type is known at compile time
                CodeStream.AddInstructionSysCall0("List", "GetItem");
            }
            
            // identifier <- collection[iterator]
            if (isReference)
            {
                CodeStream.AddInstruction(Instruction.COPYNEXTPOP);
            }
            CodeStream.AddInstruction(Instruction.POPLOCALB, identifierOffset);
            
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            if (Parser.HadError)
            {
                break;
            }
// continue:
            uint continueAddress = CodeStream.NextAddress;            
            CodeStream.InsertDebugInfo(true);
            
            // iterator++            
            CodeStream.AddInstruction(Instruction.INCLOCAL, iteratorOffset);
            
            CodeStream.AddInstructionJump(Instruction.JW, nextAddress);
// exit:            
            uint breakAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpExit, breakAddress);
            Block.PopBlock(continueAddress, breakAddress);
            
            success = true;   
            break;
        }
        return success;
    }
    
    bool compileForEachArray(string identifier, string iteratorType, string collectionType)
    {
        bool success = false;
        loop
        {
            Block.PushBlock(true); // new loop block context
            Block.AddLocal(collectionType, identifier+ "_c");    // array collection object (expression in [top])
            CodeStream.AddInstruction(Instruction.PUSHI0); // iteratorType: identifier
            CodeStream.AddInstruction(Instruction.PUSHI0); // uint: identifier_i
            
            Block.AddLocal(iteratorType, identifier);
            Block.AddLocal("uint", identifier+"_i");
            
            bool isReference = !Types.IsValueType(iteratorType);
            if (isReference)
            {
                Die(0x0B); // how did an array end up with reference members?
            }
            
            bool isRef;
            byte identifierOffset = CodeStream.IntToByte(Block.GetOffset(identifier, ref isRef));
            byte iteratorOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_i", ref isRef));
            byte collectionOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_c", ref isRef));
            
            uint nextAddress = CodeStream.NextAddress;
// next:            
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // array collection object
            CodeStream.AddInstructionSysCall0("Array", "Count_Get");
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            CodeStream.AddInstruction(Instruction.EQ);
            uint jumpExit = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JNZW, uint(0));
            
            // load identifier using identifier_i
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // array collection object
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            CodeStream.AddInstructionSysCall0("Array", "GetItem");
            
            // identifier <- collection[iterator]
            CodeStream.AddInstruction(Instruction.POPLOCALB, identifierOffset);
            
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            if (Parser.HadError)
            {
                break;
            }
//continue:
            uint continueAddress = CodeStream.NextAddress;
            CodeStream.InsertDebugInfo(true); 
            
            // iterator++            
            CodeStream.AddInstruction(Instruction.INCLOCAL, iteratorOffset);
            
            CodeStream.AddInstructionJump(Instruction.JW, nextAddress);
// exit:            
            uint breakAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpExit, breakAddress);
            Block.PopBlock(continueAddress, breakAddress);
            
            success = true;   
            break;
        }
        return success;
    }
    
    bool compileForEachDictionary(string identifier, string iteratorType, string collectionType)
    {
        bool success = false;
        loop
        {
            Block.PushBlock(true); // new loop block context
            
            Block.AddLocal(collectionType, identifier+ "_c");    // dictionary collection object (expression in [top])
            
            string keyType = Types.GetKeyFromCollection(collectionType);
            string valueType = Types.GetKeyFromCollection(collectionType);
            
            byte kt = Types.ToByte(keyType);
            byte vt = Types.ToByte(valueType);
            CodeStream.AddInstructionPUSHI(kt);
            CodeStream.AddInstructionPUSHI(vt);
            CodeStream.AddInstructionSysCall0("Pair", "New");
            CodeStream.AddInstructionPUSHI(0);
            
            Block.AddLocal(iteratorType, identifier); // pair is same type as collection
            Block.AddLocal("uint", identifier+"_i");
            
            bool isRef;
            byte identifierOffset = CodeStream.IntToByte(Block.GetOffset(identifier, ref isRef));
            byte iteratorOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_i", ref isRef));
            byte collectionOffset = CodeStream.IntToByte(Block.GetOffset(identifier+"_c", ref isRef));
                                    
            uint continueAddress = CodeStream.NextAddress;
// continue:            
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, collectionOffset); // dictionary collection object
            CodeStream.AddInstruction(Instruction.PUSHLOCALB, iteratorOffset);
            CodeStream.AddInstructionSysCall0("Dictionary", "Next");
            
            CodeStream.AddInstruction(Instruction.POPLOCALB, iteratorOffset); // iterator (identifier_i)
            CodeStream.AddInstruction(Instruction.POPLOCALB, identifierOffset); // pair (identifier)
            
            uint jumpExit = CodeStream.NextAddress;
            CodeStream.AddInstruction(Instruction.JZW, uint(0));
            
            Block.PushBlock(false); // for block locals
            compileBlock();
            Block.PopBlock();
            
            if (Parser.HadError)
            {
                break;
            }
            
            CodeStream.InsertDebugInfo(true); 
            
            CodeStream.AddInstructionJump(Instruction.JW, continueAddress);
// exit            
            uint breakAddress = CodeStream.NextAddress;
            CodeStream.PatchJump(jumpExit, breakAddress);    
            Block.PopBlock(continueAddress, breakAddress);
            
            success = true;   
            break;
        }
        return success;
    }
        
    bool compileForEach()
    {
        bool success = false;
        loop
        {
            Parser.Advance(); // foreach
            Parser.Consume(HopperToken.LParen, "'(' expected");
            if (Parser.HadError)
            {
                break;
            }
            if (!Parser.Check(HopperToken.Keyword, "var"))
            {
                Parser.Error("'var' expected");    
            }
            Parser.Advance(); // var
            
            if (!Parser.Check(HopperToken.Identifier))
            {
                Parser.Error("identifier expected A");
                break;
            }
            <string,string> currentToken = CurrentToken;
            string identifier = currentToken["lexeme"];
            Parser.Advance(); // identifier
                   
            if (!Parser.Check(HopperToken.Keyword, "in"))
            {
                Parser.Error("'in' expected");
                break;
            }
            Parser.Advance(); // in
            
            string collectionType = CompileExpression("");
            if (Parser.HadError)
            {
                break;
            }
            Parser.Consume(HopperToken.RParen, "')' expected");
            if (Parser.HadError)
            {
                break;
            }
            string iteratorType = Types.GetIteratorFromCollection(collectionType);
            if (collectionType == "string")
            {
                if (iteratorType != "char")
                {
                    Die(0x0B); // why not?
                }
                success = compileForEachString(identifier);
            }
            else if (Types.IsList(collectionType))
            {
                success = compileForEachList(identifier, iteratorType, collectionType);
            }
            else if (Types.IsArray(collectionType))
            {
                success = compileForEachArray(identifier, iteratorType, collectionType);
            }
            else if (Types.IsDictionary(collectionType))
            {
                success = compileForEachDictionary(identifier, iteratorType, collectionType);
            }
            else
            {
                Die(0x0AB); // what's this?
            }
            break;
        } // loop
        
        
        return success;
    }
    
    bool compileSwitch()
    {
        bool success = false;
        loop
        {
            <uint> jumpEnds;
            Parser.Advance(); // switch
            Parser.Consume(HopperToken.LParen, "'(' expected");
            if (Parser.HadError)
            {
                break;
            }
            
            string switchType = CompileExpression("");
            
            uint unique = CodeStream.NextAddress;
            string switchName ="switch_" + unique.ToHexString(4);
            Block.PushBlock(false); // block for switch variable
            Block.AddLocal(switchType, switchName);
            
            bool isRef;
            byte switchOffset = CodeStream.IntToByte(Block.GetOffset(switchName, ref isRef));
            
            if (Parser.HadError)
            {
                break;
            }
            if (!Types.IsValueType(switchType) && (switchType != "string"))
            {
                Parser.ErrorAtCurrent("'switch' expression type must be value type or string");
            }
            
            Parser.Consume(HopperToken.RParen, "')' expected");
            if (Parser.HadError)
            {
                break;
            }
            Parser.Consume(HopperToken.LBrace, "'{' expected A");
            if (Parser.HadError)
            {
                break;
            }
            bool defaultSeen = false;
            loop
            {
                if (Parser.Check(HopperToken.RBrace))
                {
                    success = true;
                    Parser.Advance(); // }
                    break;
                }
                if (Parser.Check(HopperToken.EOF))
                {
                    Parser.ErrorAtCurrent("unexpected EOF in 'switch'");
                    break;
                }
                
                CodeStream.InsertDebugInfo(false);
                
                bool isDefault = false;
                if (!Parser.Check(HopperToken.Keyword, "case"))
                {
                    if (Parser.Check(HopperToken.Keyword, "default"))
                    {
                        isDefault = true;       
                        if (defaultSeen)
                        {
                            Parser.ErrorAtCurrent("'default' can only occur once");
                            break;    
                        }
                        defaultSeen = true;
                    }
                    else
                    {
                        Parser.ErrorAtCurrent("'case' expected");
                        break;
                    }
                }
                if (defaultSeen && !isDefault)
                {
                    Parser.ErrorAtCurrent("'default' must be last case");
                    break;    
                }                
                Parser.Advance();
// next:         
                if (!isDefault)
                {   
                    string caseConstant = ParseConstantExpression(switchType);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    if (switchType == "string")
                    {
                        uint constantAddress = CodeStream.CreateStringConstant(caseConstant);
                        CodeStream.AddInstructionPUSHI(constantAddress);
                        CodeStream.AddInstructionPUSHI(caseConstant.Length);
                        CodeStream.AddInstructionSysCall0("String", "NewFromConstant");
                    }
                    else if (switchType == "char")
                    {
                        char c = caseConstant[0];
                        CodeStream.AddInstructionPUSHI(byte(c));   
                    }
                    else
                    {
                        uint cc;
                        if (Token.TryParseUInt(caseConstant, ref cc))
                        {
                            CodeStream.AddInstructionPUSHI(cc);
                        }
                        else
                        {
                            Parser.Error("unexpected 'case' constant '" + caseConstant + "'");                
                        }
                    }
                }
                Parser.Consume(HopperToken.Colon, "':' expected");
                if (Parser.HadError)
                {
                    break;
                }
                uint jumpNext;
                if (!isDefault)
                {
                    CodeStream.AddInstruction(Instruction.PUSHLOCALB, switchOffset); // switch variable
                    // compare with case constant
                    if (switchType == "string")
                    {
                        CodeStream.AddInstructionSysCall0("String", "Compare");
                        CodeStream.AddInstruction(Instruction.PUSHI0);
                    }
                    CodeStream.AddInstruction(Instruction.EQ); // => 1
                    jumpNext = CodeStream.NextAddress;
                    CodeStream.AddInstruction(Instruction.JZW, uint(0)); // next
                }
                
                Block.PushBlock(false); // not loop context
                compileBlock();
                Block.PopBlock();
                
                CodeStream.InsertDebugInfo(true); 
                
                uint jumpEnd = CodeStream.NextAddress;
                CodeStream.AddInstruction(Instruction.JW, uint(0));
                jumpEnds.Append(jumpEnd);
// next:        
                if (!isDefault)
                {        
                    uint nextAddress = CodeStream.NextAddress;
                    CodeStream.PatchJump(jumpNext, nextAddress);   
                }
            } // loop
// end:         
            uint endAddress = CodeStream.NextAddress;
            foreach (var jumpEnd in jumpEnds)
            {
                uint ui = jumpEnd;
                CodeStream.PatchJump(ui, endAddress);
            }
            Block.PopBlock();
            break;
        } // loop
        return success;
    }
    
    bool compileIncrementDecrement(string variableName, HopperToken tokenType)
    {
        bool success = false;
        loop
        {
            Parser.Advance(); // ++ or --
            string qualifiedName;
            string variableType = Types.GetTypeString(variableName, true, ref qualifiedName);
            if (Parser.HadError)
            {
                break;
            }
            if (!Types.IsNumericType(variableType))
            {
                Parser.ErrorAtCurrent("++ and -- operations only legal for numeric types");
                break;
            }
            if ((variableType == "uint") || (variableType == "byte") || (variableType == "int"))
            {
                if (!Symbols.GlobalMemberExists(qualifiedName))
                {
                    bool isRef;
                    int offset = Block.GetOffset(variableName, ref isRef);
                    if ((offset > -129) && (offset < 128) && !isRef)
                    {
                        byte operand =  CodeStream.IntToByte(offset);
                        if (tokenType == HopperToken.Increment)
                        {
                            CodeStream.AddInstruction(Instruction.INCLOCAL, operand);
                        }
                        else
                        {
                            CodeStream.AddInstruction(Instruction.DECLOCAL, operand);
                        }
                        success = true;           
                        break;
                    }
                }
            }
            
            CodeStream.AddInstructionPushVariable(qualifiedName);
            CodeStream.AddInstructionPUSHI(byte(1));
            if (tokenType == HopperToken.Increment)
            {
                switch (variableType)
                {
                    case "long":
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                        CodeStream.AddInstructionSysCall0("Long", "Add");
                    }
                    case "float":
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToFloat");
                        CodeStream.AddInstructionSysCall0("Float", "Add");
                    }
                    default:
                    {
                        if (Types.IsSignedIntType(variableType))
                        {
                            CodeStream.AddInstruction(Instruction.ADDI);
                        }
                        else
                        {
                            CodeStream.AddInstruction(Instruction.ADD);
                        }
                    }
                }
            }
            else if (tokenType == HopperToken.Decrement)
            {
                switch (variableType)
                {
                    case "long":
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToLong");
                        CodeStream.AddInstructionSysCall0("Long", "Sub");
                    }
                    case "float":
                    {
                        CodeStream.AddInstructionSysCall0("UInt", "ToFloat");
                        CodeStream.AddInstructionSysCall0("Float", "Sub");
                    }
                    default:
                    {
                        if (Types.IsSignedIntType(variableType))
                        {
                            CodeStream.AddInstruction(Instruction.SUBI);
                        }
                        else
                        {
                            CodeStream.AddInstruction(Instruction.SUB);
                        }
                    }
                }
            }
            CodeStream.AddInstructionPopVariable(variableType, qualifiedName);
            success = true;           
            break;
        }
        return success;
    } 
    bool compileAssignment(string variableName, bool ignoreZero)
    {
        bool success = false;
        loop
        {
            <string,string> leftToken = PreviousToken;
            Parser.Consume(HopperToken.Assign, "'=' expected");
            if (Parser.HadError)
            {
                break;
            }
            
            // uses Blocks, respects namespaces, Parser.Error on failure
            string qualifiedName;
            string variableType = Types.GetTypeString(variableName, false, ref qualifiedName);
            bool isSetter = false;
            uint iOverload;
            
            if (ignoreZero)
            {
                
                ignoreZero = Types.IsValueType(variableType);
                if (ignoreZero)
                {
                    <string,string> nextToken = Parser.Peek();
                    HopperToken nextTokenType = Token.GetType(nextToken);   
                    ignoreZero = nextTokenType == HopperToken.SemiColon;
                }
                if (ignoreZero)
                {
                    <string,string> currentToken = Parser.CurrentToken;
                    HopperToken tokenType = Token.GetType(currentToken);   
                    if ((variableType == "bool") 
                     && (tokenType == HopperToken.Bool)
                     && (currentToken["lexeme"] == "false"))
                    {
                        // "bool value;" is the same as "bool value = false;"
                        Parser.Advance(); // 'false'
                        success = true;        
                        break;
                    }
                    else if (((variableType == "byte") || (variableType == "int") || (variableType == "uint")) 
                          && (tokenType == HopperToken.Integer)
                          && (currentToken["lexeme"] == "0"))
                    {
                        // "uint value;" is the same as "uint value = 0;"
                        Parser.Advance(); // '0'
                        success = true;        
                        break;
                    }
                }
            }
            
            if (variableType.Length == 0)
            {
                // perhaps it is a setter
                string setterMethod = variableName + "_Set";
                setterMethod = Types.QualifyMethodName(setterMethod);
                uint fIndex;
                if (!Symbols.GetFunctionIndex(setterMethod, ref fIndex))
                {
                    Parser.ErrorAt(leftToken, "undefined identifier");   
                    break;
                }
                
                <uint> overloads = Symbols.GetFunctionOverloads(fIndex);
                if (overloads.Length != 1)
                {
                    Parser.ErrorAt(leftToken, "setter method should only have one overload");   
                    break;
                }
                iOverload = overloads[0];
                < < string > > arguments = Symbols.GetOverloadArguments(iOverload); 
                
                if (arguments.Length != 1)
                {
                    Parser.ErrorAt(leftToken, "setter method should only have one argument");   
                    break;
                }
                <string> argument = arguments[0];
                variableType = argument[1];   
                isSetter = true;
                Symbols.OverloadToCompile(iOverload);
            }
            string expressionType = CompileExpression(variableType);
            if (Parser.HadError)
            {
                break;
            }
            if (expressionType != variableType)
            {
                if (!Types.AutomaticUpCastTop(expressionType, variableType))
                {
                    bool isVerified = false;
                    if ((expressionType == "variant") || (expressionType == "K") || (expressionType == "V"))
                    {
                        if (Types.IsSimpleType(variableType))
                        {
                            Types.VerifyTopType(variableType, "");
                            isVerified = true;
                        }
                        else if (Types.IsList(variableType))
                        {
                            Types.VerifyTopType("list", "");
                            string variableValueType = Types.GetValueFromCollection(variableType);
                            if (Types.IsSimpleType(variableValueType))
                            {    
                                Types.VerifyTopValueType(variableValueType, "list");
                                isVerified = true;
                            }
                        }
                        else if (Types.IsDictionary(variableType))
                        {
                            Types.VerifyTopType("dictionary", "");
                            string variableKeyType = Types.GetKeyFromCollection(variableType);
                            string variableValueType = Types.GetValueFromCollection(variableType);
                            if (Types.IsSimpleType(variableKeyType))
                            {
                                Types.VerifyTopType(variableKeyType, "Key");
                                if (Types.IsSimpleType(variableValueType))
                                {    
                                    Types.VerifyTopValueType(variableValueType, "dictionary");
                                    isVerified = true;
                                }
                            }
                        }
                    }
                    if (!isVerified)
                    {
                        if (expressionType == "variant")
                        {
                            // TODO : runtime type check

//PrintLn();
//Print("compileAssignment TODO A: runtime type check: " + variableType + " " + variableName + " <- " + expressionType);

                        }
                        else if (expressionType == "V")
                        {
                            // TODO : resolve "V" to type
//PrintLn();
//Print("compileAssignment TODO D: resolve 'V': " + variableType + " " + variableName + " <- " + expressionType);
                        }
                        else if (expressionType == "K")
                        {
                            // TODO : resolve "K" to type
//PrintLn();
//Print("compileAssignment TODO E: resolve 'K': " + variableType + " " + variableName+ " <- " + expressionType);
                        }
                        else
                        {
                            Parser.ErrorAt(leftToken, 
                              "type mismatch in assignment, expect '" + variableType + "', was '" + expressionType + "'");       
                            break;
                        }
                    }
                }
            }
            
            if (!isSetter)
            {
                CodeStream.AddInstructionPopVariable(variableType, qualifiedName);
            }
            else
            {
                // call setter method with expression result on stack as argument
                string returnType = CompileMethodCall(variableName + "_Set", "");
                if (returnType != "void")
                {
                    Die(0x0B);
                }
            }
            
            success = true;
            break;
        }              
        return success;
    }
    
    bool compileLocalDeclaration()
    {
        bool success = false;
        loop
        {
            string variableType = Types.ParseType();
            if (Parser.HadError)
            {
                break;
            }
            if (!Parser.Check(HopperToken.Identifier))
            {
                if (!Parser.Check(HopperToken.DottedIdentifier))
                {
                    Parser.ErrorAtCurrent("identifier expected B");
                    break;
                }
            }
            <string,string> idToken   = Parser.CurrentToken;
            string identifier = idToken["lexeme"];
            
            char firstCharacter = identifier[0];
            if (firstCharacter.IsUpper())
            {
                Parser.ErrorAtCurrent("public identifier for local declaration is invalid");
                break;
            }
            Parser.Advance(); // identifier
            
            if (!Parser.Check(HopperToken.Assign) && !Parser.Check(HopperToken.SemiColon))
            {
                Parser.ErrorAtCurrent("';' or '=' expected");
                break;
            }
            // to reserve slot
            InitializeVariable(variableType);
            
            Block.AddLocal(variableType, identifier);
            if (Parser.Check(HopperToken.SemiColon))
            {
                success = true;
                break;               
            }
            success = compileAssignment(identifier, true);
            break;
        } // loop
        return success;
    }
    
    bool compileStatement(bool noSemiColon, bool debugInfoInsert)
    {
        bool success = false;
        <string,string> currentToken = Parser.CurrentToken;
        string tokenString = currentToken["lexeme"];
        HopperToken tokenType = Token.GetType(currentToken);
        if (debugInfoInsert)
        {
            CodeStream.InsertDebugInfo(false);
        }
        
        switch (tokenType)
        {
            case HopperToken.Keyword:
            {
                if (tokenString == "if")
                {
                    success = compileIfStatement();
                    noSemiColon = true;
                }
                else if (tokenString == "return")
                {
                    success = compileReturn();
                }
                else if (tokenString == "break")
                {
                    success = compileBreak();
                }
                else if (tokenString == "continue")
                {
                    success = compileContinue();
                }
                else if (tokenString == "while")
                {
                    success = compileWhile();
                    noSemiColon = true;
                }
                else if (tokenString == "loop")
                {
                    success = compileLoop();
                    noSemiColon = true;
                }
                else if (tokenString == "for")
                {
                    success = compileFor();
                    noSemiColon = true;
                }
                else if (tokenString == "foreach")
                {
                    success = compileForEach();
                    noSemiColon = true;
                }
                else if (tokenString == "switch")
                {
                    success = compileSwitch();
                    noSemiColon = true;
                }
                else
                {
                    // simple type
                    success = compileLocalDeclaration();
                }
            }
            case HopperToken.LT:
            {
                // compound type
                success = compileLocalDeclaration();
            }
            default:
            {
                if ((tokenType == HopperToken.Identifier) || (tokenType == HopperToken.DottedIdentifier))
                {
                    bool isDotted = (tokenType == HopperToken.DottedIdentifier);
                    <string,string> nextToken = Parser.Peek();
                    HopperToken nextTokenType = Token.GetType(nextToken);   
                    if (Types.IsEnum(tokenString) || Types.IsFlags(tokenString))
                    {
                        success = compileLocalDeclaration();
                    }
                    else if (nextTokenType == HopperToken.Identifier)
                    {
                        success = compileLocalDeclaration();
                    }
                    else
                    {
                        Advance();
                        <string,string> idToken   = Parser.PreviousToken;
                        tokenString = idToken["lexeme"];
                        nextToken = Parser.CurrentToken;
                        tokenType = Token.GetType(nextToken);
                        if (tokenType == HopperToken.Assign)
                        {
                            // assignment
                            success = compileAssignment(tokenString, false);
                        }
                        else if (tokenType == HopperToken.LBracket)
                        {
                            // array member assignment
                            Advance(); // [
                            loop
                            {
                                string collectionVariable = tokenString;
                                string qualifiedName;
                                string collectionType = Types.GetTypeString(collectionVariable, true, ref qualifiedName);
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                bool isArray = false;
                                bool isList = true;
                                bool isDictionary = false;
                                if (Types.IsArray(collectionType))
                                {
                                    // push first argument: the array
                                    CodeStream.AddInstructionPushVariable(qualifiedName);
                                                                    
                                    // second argument, the index
                                    string indexType = CompileExpression("uint");
                                    if (Parser.HadError)
                                    {
                                        break;
                                    }
                                    
                                    if (indexType != "uint")
                                    {
                                        if (Types.AutomaticUpCastTop(indexType, "uint"))
                                        {
                                            indexType = "uint";
                                        }                     
                                    }
                                    if (indexType != "uint")
                                    {
                                        Parser.ErrorAtCurrent("array index type invalid");
                                        break;
                                    }
                                    isArray = true;
                                }
                                else if (Types.IsList(collectionType))
                                {
                                    // push first argument: the list
                                    CodeStream.AddInstructionPushVariable(qualifiedName);
                                    
                                    // second argument, the index
                                    string indexType = CompileExpression("uint");
                                    if (Parser.HadError)
                                    {
                                        break;
                                    }
                                    
                                    if (indexType != "uint")
                                    {
                                        if (Types.AutomaticUpCastTop(indexType, "uint"))
                                        {
                                            indexType = "uint";
                                        }                     
                                    }
                                    if (indexType != "uint")
                                    {
                                        Parser.ErrorAtCurrent("list index type invalid");
                                        break;
                                    }
                                    isList = true;
                                    break;
                                }
                                else if (Types.IsDictionary(collectionType))
                                {
                                    // push first argument: the dictionary
                                    CodeStream.AddInstructionPushVariable(qualifiedName);
                                    
                                    // <string,string>
                                    string keyType = Types.GetKeyFromCollection(collectionType);
                                    
                                    // second argument, the key
                                    string actualType = CompileExpression(keyType);
                                    if (Parser.HadError)
                                    {
                                        break;
                                    }
                                    if (keyType != actualType)
                                    {
                                        if ((keyType == "string") && (actualType != "string"))
                                        {
                                            Parser.ErrorAtCurrent("dictionary key type should be 'string', (not '" + actualType +"')");
                                            break;
                                        }
                                        if (Types.AutomaticUpCastTop(actualType, keyType))
                                        {
                                            actualType = keyType;
                                        }                     
                                        if (keyType != actualType)
                                        {
                                            Parser.ErrorAtCurrent("dictionary key type should be '" + keyType + "'");
                                            break;
                                        }
                                    }
                                    isDictionary = true;
                                }
                                else
                                {
                                    Parser.ErrorAtCurrent("identifer not array, dictionary or list type");
                                    break;
                                }
                                
                                Parser.Consume(HopperToken.RBracket, "']' expected");
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                
                                Parser.Consume(HopperToken.Assign, "'=' expected");
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                
                                // final argument: value
                                string expressionType = CompileExpression(collectionType);
                                if (Parser.HadError)
                                {
                                    break;
                                }
                                string valueType = Types.GetValueFromCollection(collectionType);
                                if (expressionType != valueType)
                                {
                                    if (valueType == "variant")
                                    {
                                        // a variant value container can contain anything
                                        if (Types.IsValueType(expressionType))
                                        {
                                            // box value types before adding to collection as variant                                            
                                            byte vt = Types.ToByte(expressionType);
                                            CodeStream.AddInstructionPUSHI(vt);
                                            CodeStream.AddInstructionSysCall0("Variant", "Box");
                                        }
                                    }
                                    else if (!Types.AutomaticUpCastTop(expressionType, valueType))
                                    {
                                        Parser.ErrorAtCurrent(
                          "type mismatch in assignment, expect '" + valueType + "', was '" + expressionType + "'");       
                                        break;
                                    }
                                }                          
                                if (isArray)
                                {           
                                    CodeStream.AddInstructionSysCall0("Array", "SetItem");   
                                }
                                else if (isDictionary)
                                {
                                    CodeStream.AddInstructionSysCall0("Dictionary", "Set");   
                                }
                                else if (isList)
                                {
                                    CodeStream.AddInstructionSysCall0("List", "SetItem");
                                }
                                else
                                {
                                    Die(0x0B); // what's this? string?
                                }
                                success = true;
                                break;
                            }
                        }
                        else if ((tokenType == HopperToken.Increment) || (tokenType == HopperToken.Decrement))
                        {
                            // ++ or --
                            success = compileIncrementDecrement(tokenString, tokenType);
                        }
                        else if (tokenType == HopperToken.LParen)
                        {
                            // method call
                            string returnType = CompileMethodCall(tokenString, "");
                            if (returnType != "void")
                            {
                                Parser.ErrorAtCurrent("function cannot be used as method (return value must be consumed)");
                            }
                            else
                            {
                                success = true;
                            }
                        }
                        else
                        {
                            if (!Parser.HadError)
                            {
                                Parser.ErrorAt(nextToken, "'(' or '=' expected");
                            }
                        }
                    }
                }
                else
                {
                    // failed
                }
            }
        } // switch (tokenType)
        if (success && !noSemiColon)
        {
            if (!Parser.Check(HopperToken.SemiColon))
            {
                DumpPrevious();
                DumpCurrent();
            }
            Parser.Consume(HopperToken.SemiColon, "';' expected B");
        }
        return success;
    }
                                                                 
    compileBlock()
    {
        loop
        {
            if (!Parser.Check(HopperToken.LBrace))
            {
                Parser.ErrorAtCurrent("'{' expected");
                break;
            }
            
            Parser.Advance(); // {
            //CodeStream.InsertDebugInfo(true);
            loop
            {
                // next statement ..
                if (Parser.HadError)
                {
                    break;
                }
                if (Parser.Check(HopperToken.RBrace))
                {
                    Advance(); // }
                    //CodeStream.InsertDebugInfo(true);
                    break; // end of method
                }
                else if (Parser.Check(HopperToken.Directive))
                {
                    // preprocessor directives (#ifdef, #ifndef, #else, #endif")
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
                                break;
                            }
                            if (Parser.Check(HopperToken.EOF))
                            {
                                break;
                            }
                            Parser.Advance(); // gobble gobble
                        }
                    }
                    else
                    {      
                        <string,string> currentToken = Parser.CurrentToken;
                        HopperToken tokenType = Token.GetType(currentToken);
                        if (   (tokenType == HopperToken.Keyword)           // simple type, "if", "while", ...
                            || (tokenType == HopperToken.LT)                // compound type
                            || (tokenType == HopperToken.Identifier)        // assignment, procedure call
                            || (tokenType == HopperToken.DottedIdentifier) 
                            )
                        {
                            if (!compileStatement(false, true))
                            {
                                if (!Parser.HadError)
                                {
                                    Parser.ErrorAtCurrent("bad statement?");
                                }
                                break;
                            }
                        }
                        else if (tokenType == HopperToken.LBrace)
                        {
                            Parser.ErrorAt(currentToken, "unexpected '{'");
                            break;
                        }
                        else
                        {
                            Parser.ErrorAt(currentToken, "keyword or identifier expected");
                            break;
                        }
                    }
                }   
            } // loop
            break;
        } // loop
    }
                                                                                                                 
    bool compile()
    {
        
        <byte> globalCode = initializeGlobals();
        
        bool success = false;
        bool isMain = true;
        loop
        {
            // compile fIndex
            <string, string> startToken = Symbols.GetOverloadStart(iCurrentOverload);
            Scanner.Reset(startToken);
            Parser.Reset();
            Directives.New();
            CodeStream.New();
            
            if (globalCode.Length > 0)
            {
                CodeStream.AppendCode(globalCode);
                globalCode.Clear();
            }
            
            Types.SetCurrentMethod(iCurrentOverload);
                                                                                                                                                                    
            Parser.Advance(); // load first token
            
            if (Parser.Check(HopperToken.Keyword, "system"))
            {
                // no need to compile system calls
                Symbols.OverloadWasCompiled(iCurrentOverload);         
                if (!Symbols.OverloadNextToCompile(ref iCurrentOverload))
                {
                    success = true; // all done
                    break;
                }
                continue;
            }
                              
            <string,string> braceToken = Parser.CurrentToken; // for CodeGen location
            
            //CodeStream.InsertDebugInfo(true);
            CodeStream.AddInstruction(Instruction.ENTER);
            
            if (!isMain) // already pushed with globals
            {
                Block.PushBlock(false); // new block context
            }
            
            <string,variant> blockContext = Block.Top();
            < < string > > arguments = Symbols.GetOverloadArguments(iCurrentOverload);
            blockContext["arguments"] = arguments;
            string returnType = Symbols.GetOverloadReturnType(iCurrentOverload);
            if (returnType != "void")
            {                    
                blockContext["returntype"] = returnType;
            }
            Block.ReplaceTop(blockContext);
                     
            compileBlock();
            
            if (Parser.HadError)
            {
                break;
            }
            Parser.ProgressTick(".");
            
            // check that #ifdef nesting is zero
            if (Directives.IsStillOpen)
            {
                Parser.ErrorAtCurrent("'#endif' expected before end of method");
                break;
            }
            
            if (returnType != "void")
            {
                Instruction lastInstruction = CodeStream.GetLastInstruction();
                if ((lastInstruction != Instruction.RET0)
                 && (lastInstruction != Instruction.RETRETB)
                 && (lastInstruction != Instruction.RETRETW)
                   )
                {
                    Parser.ErrorAtCurrent("'return' expected");
                    break;
                }
            }
            else
            {
                
                <string,string> previousToken = Parser.PreviousToken;
                HopperToken tokenType = Token.GetType(previousToken);
                if (tokenType != HopperToken.RBrace)
                {
                    Parser.ErrorAt(previousToken, "'}' expected in compile()!!");
                    Die(0x0B);
                }
                
                
                CodeStream.InsertDebugInfo(true);
                
                if (CodeStream.CheckedBuild)
                {
                    // if you pop locals, sp == bp
                    uint localsToPop = Block.GetLocalsToPop(false, false);
                    CodeStream.AddInstruction(Instruction.TESTBPB, byte(localsToPop));
                }

                uint bytesToPop = Block.GetLocalsToPop(true, isMain);
                if (bytesToPop == 0)
                {
                    CodeStream.AddInstruction(Instruction.RET0);
                }
                else if (bytesToPop < 256)
                {
                    CodeStream.AddInstruction(Instruction.RETB, byte(bytesToPop));
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.RETW, bytesToPop);
                }
            }
            if (!isMain)
            {
                Block.PopBlock();
            }
            else
            {
                // pop "main" locals and arguments but keep globals
                <string,variant> mainContext = Block.Top();
                < <string> > empty;
                mainContext["locals"] = empty;
                mainContext["arguments"] = empty;
                Block.ReplaceTop(mainContext);
            }
            
            <byte> codeStream = CodeStream.CurrentStream;
            <string,string> debugInfo = CodeStream.DebugInfo;
            Symbols.SetCodeStream(iCurrentOverload, codeStream, debugInfo);
            CodeStream.ClearDebugInfo();
            
            
            Symbols.OverloadWasCompiled(iCurrentOverload);         
            if (!Symbols.OverloadNextToCompile(ref iCurrentOverload))
            {
                success = true; // all done
                break;
            }
            isMain = false;
        }
        
        return success;
    }
    
    <byte> initializeGlobals()
    {
        <string,variant> top = Block.Top();
        < <string> > globals = top["globals"];
        
        
        // globals
        //   <string> gNames;
        //   <string,uint> gIndex;
        //   <uint, string> gTypes;
        // code location of initialization code:
        //   <uint, long> gStartPos;
        //   <uint, uint> gStartLine;
        //   <uint, string> gSourcePath;
        
        CodeStream.New();
                             
        uint gCount = Symbols.GetGlobalCount();
        for (uint gIndex = 0; gIndex < gCount; gIndex++)
        {
            <string> global;                     
            string variableType = Symbols.GetGlobalType(gIndex);
            string identifier   = Symbols.GetGlobalName(gIndex);
            global.Append(variableType);
            global.Append(identifier);
            globals.Append(global);
            
            // for reference times, initialize
            // to reserve slot
            InitializeVariable(variableType);
            
            // compile gIndex
            <string, string> startToken = Symbols.GetGlobalStart(gIndex);
            if (startToken.Count > 0)
            {
                // execute initialization code if there is any
                Scanner.Reset(startToken);
                Parser.Reset();
                Directives.New();
                
                Parser.Advance(); // load first token
                
                string actualType = CompileExpression(variableType);
                if (Parser.HadError)
                {
                    Parser.Error("failure compiling global initializer");
                    break;
                }
                if (actualType != variableType)
                {
                    if (Types.AutomaticUpCastTop(actualType, variableType))
                    {
                        // ok
                    }
                    else
                    {
                        Parser.Error("type mismatch for initializer, (was '" + actualType + "', expected '"+ variableType +"')");
                        break;
                    }
                }
                uint globalAddress = Symbols.GetGlobalAddress(identifier);
                if (globalAddress < 256)
                {
                    CodeStream.AddInstruction(Instruction.POPGLOBALB, byte(globalAddress));
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.POPGLOBALW, globalAddress);
                }
            }
            
        }
        top["globals"] = globals;
        Block.ReplaceTop(top);
        
        <byte> code = CodeStream.CurrentStream;
        return code;
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for COMPILE:");
        PrintLn("  COMPILE <object json>");
        PrintLn("    -g : called from GUI, not console");
        PrintLn("    -o : optimized (less runtime checks)");
    }
    
    {
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            bool checkedBuild = true;
          
            foreach (var arg in rawArgs)
            {
                if ((arg.Length == 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
                        case "-g":
                        {
                            Parser.SetInteractive(true);    
                        }
                        case "-o":
                        {
                            checkedBuild = false;   
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
            
            string jsonPath = args[0];
            if (!File.Exists(jsonPath))
            {
                string ext = Path.GetExtension(jsonPath);
                if (ext == ".")
                {
                    jsonPath = jsonPath + ".json";
                }
                if (!File.Exists(jsonPath))
                {
                    BadArguments();
                    break;       
                }
            }
            CodeStream.CheckedBuild = checkedBuild;
            
            long startTime = Millis;
            loop
            {
                SysCalls.New();
                Symbols.New();
                if (!Symbols.Import(jsonPath))
                {
                    break;
                }
                
                uint mIndex;
                if (!Symbols.GetFunctionIndex("main", ref mIndex))   
                {
                    Parser.Error("where is 'main'?");
                    break;
                }
                <uint> mOverloads = Symbols.GetFunctionOverloads(mIndex);
                if (mOverloads.Length != 1)
                {
                    Parser.Error("'main' has overloads?");
                    break;
                }
                
                Scanner.New();
                
                iCurrentOverload = mOverloads[0];
                Symbols.AddFunctionCall(iCurrentOverload); // yup, main is called at least once

                Block.PushBlock(false); // new block context
                if (!compile())
                {
                    break;
                }
                Block.PopBlock();
                
                string extension = Path.GetExtension(jsonPath);
                string codePath = jsonPath.Replace(extension, ".code");
                if (!Symbols.ExportCode(codePath))
                {
                    break;
                }
                if (!IsInteractive())
                {
                    PrintLn();
                    Print("Success.", Color.DarkGreen, Color.LightGray);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() + "s", Color.MatrixBlue, Color.LightGray);
                }
                else
                {
                    Parser.ProgressDone();
                }
                break;
            }
            break;
        }
    }
}

unit Block
{
    
    uses "/Source/Compiler/Tokens/Parser"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/CodeGen/CodeStream"
    
    // "arguments" [BP-offset]
    // "locals"    [BP+offset]
    // isLoop? (how break and continue behave)
    //   - 'break' jumps to patch
    //   - 'continue' jumps to patch
    
    < <string,variant> > blockList;
    
    uint BlockDepth() { return blockList.Length; }
    
    <string,variant> Top()
    {
        uint iLast = blockList.Length;
        iLast--;
        <string,variant> blockContext = blockList[iLast];
        return blockContext;
    }
    <string,variant> GetMethodBlock()
    {
        <string,variant> methodBlock;
        uint iBlock = blockList.Length;
        loop
        {   
            if (iBlock == 0)
            {
                break;
            }
            iBlock--;
            <string,variant> blockContext = blockList[iBlock];
            if (blockContext.Contains("arguments"))
            {
                methodBlock = blockContext;
                break;
            }
        }
        return methodBlock;
    }
    
    AddLocal(string variableType, string identifier)
    {
        <string,variant> top = Top();
        < <string> > locals = top["locals"];
        <string> local;
        local.Append(variableType);
        local.Append(identifier);
        locals.Append(local);
        top["locals"] = locals;
        ReplaceTop(top);
    }
    
    ReplaceTop(<string,variant> blockContext)
    {
        uint iLast = blockList.Length;
        iLast--;
        blockList.SetItem(iLast, blockContext);
    }
    
    bool AddBreakPatch(uint address)
    {
        bool success = false;
        uint iLoop = blockList.Length;
        loop
        {
            if (iLoop == 0)
            {
                break;
            }
            iLoop--;
            <string,variant> blockContext = blockList[iLoop];
            if (blockContext.Contains("breaks"))
            {
                <uint> breakPatches = blockContext["breaks"];
                breakPatches.Append(address);
                blockContext["breaks"] = breakPatches;
                blockList.SetItem(iLoop, blockContext);
                success = true;
                break;
            }
        }
        return success;    
    }
    
    bool AddContinuePatch(uint address)
    {
        bool success = false;
        uint iLoop = blockList.Length;
        loop
        {
            if (iLoop == 0)
            {
                break;
            }
            iLoop--;
            <string,variant> blockContext = blockList[iLoop];
            if (blockContext.Contains("continues"))
            {
                <uint> continuePatches = blockContext["continues"];
                continuePatches.Append(address);
                blockContext["continues"] = continuePatches;
                blockList.SetItem(iLoop, blockContext);
                success = true;
                break;
            }
        }
        return success;    
    }
    
    PushBlock(bool isLoopBlock)
    {
        <string,variant> blockContext;
        if (isLoopBlock)
        {
            <uint> breakPatches;
            blockContext["breaks"] = breakPatches;
            <uint> continuePatches;
            blockContext["continues"] = continuePatches;
        }
        < <string> > locals;
        blockContext["locals"] = locals;
        if (blockList.Length == 0)
        {
            < <string> > globals;
            blockContext["globals"] = globals;
        }
        blockList.Append(blockContext);        
    }    
    PopBlock()
    {
        PopBlock(0, 0);
    }
    PopBlock(uint continueTarget, uint breakTarget)
    {
        uint iLast = blockList.Length;
        iLast--;
        
        uint bytesToPop = GetBytesToPop();
        if (bytesToPop > 0)
        {
            if (bytesToPop > 255)
            {
                Die(0x0B); // need a DECSPW
            }
            
            Instruction previousInstruction = CodeStream.GetLastInstruction();
            if ((previousInstruction != Instruction.RETB) 
             && (previousInstruction != Instruction.RETW)
             && (previousInstruction != Instruction.RETRETB) 
             && (previousInstruction != Instruction.RETRETW)
             && (previousInstruction != Instruction.RET0)
               )
            {
                <string,string> previousToken = Parser.PreviousToken;
                HopperToken tokenType = Token.GetType(previousToken);
                if (tokenType != HopperToken.RBrace)
                {
                    if (!Parser.HadError)
                    {
                        Parser.ErrorAt(previousToken, "'}' expected in PopBlock(..)!!");
                        Die(0x0B);
                    }
                }               
                CodeStream.InsertDebugInfo(true); // PreviousToken is '{'
                
                CodeStream.AddInstruction(Instruction.DECSP, byte(bytesToPop));
                if (breakTarget != 0)
                {
                    breakTarget = breakTarget + 2; // break past the above DECSP
                }
            }
        }
        
        <string,variant> blockContext = blockList[iLast];   
        if (blockContext.Contains("breaks"))
        {
            if ((continueTarget == 0) && (breakTarget == 0))
            {
                if (!Parser.HadError)
                {
                    Die(0x0B);
                }
            }
            <uint> breakPatches = blockContext["breaks"];
            foreach (var breakJump in breakPatches)
            {
                CodeStream.PatchJump(breakJump, breakTarget);
            }
        }
        if (blockContext.Contains("continues"))
        {
            if ((continueTarget == 0) && (breakTarget == 0))
            {
                if (!Parser.HadError)
                {
                    Die(0x0B);
                }
            }
            <uint> continuePatches = blockContext["continues"];
            foreach (var continueJump in continuePatches)
            {
                PatchJump(continueJump, continueTarget);
            }
        }
        blockList.Remove(iLast);
    }
    
    uint GetBytesToPop()
    {
        return GetBytesToPop(false, false);
    }
    uint GetBytesToPop(bool toLoop, bool isContinue)
    {
        uint bytesToPop = 0;
        uint iLast = blockList.Length;
        loop
        {
            if (iLast == 0)
            {
                break;
            }
            iLast--;
            <string,variant> blockContext = blockList[iLast];
            uint popMore;
            if (blockContext.Contains("arguments"))
            {
                < < string > > arguments = blockContext["arguments"];
                popMore = popMore + arguments.Length * 2; // 2 bytes per stack slot for arguments
            }
            if (blockContext.Contains("locals"))
            {
                < < string > > locals = blockContext["locals"];
                popMore = popMore + locals.Length * 2; // 2 bytes per stack slot for locals
            }
            if (blockContext.Contains("globals"))
            {
                < < string > > globals = blockContext["globals"];
                popMore = popMore + globals.Length * 2; // 2 bytes per stack slot for globals
            }
            bytesToPop = bytesToPop + popMore;
            if (!toLoop)
            {
                break;
            }
            if (blockContext.Contains("breaks")) // loop block
            {
                if (isContinue)
                {
                    bytesToPop = bytesToPop - popMore;
                }
                break;
            }
        }
    
        return bytesToPop;
    }
    
    uint GetLocalsToPop(bool andArguments, bool andGlobals)
    {
        uint localsToPop = 0;
        uint iLast = blockList.Length;
        loop
        {
            if (iLast == 0)
            {
                break;
            }
            iLast--;
            <string,variant> blockContext = blockList[iLast];
            if (blockContext.Contains("locals"))
            {
                < < string > > locals = blockContext["locals"];
                localsToPop = localsToPop + locals.Length * 2; // 2 bytes per stack slot for locals
            }
            if (andArguments && blockContext.Contains("arguments"))
            {
                < < string > > arguments = blockContext["arguments"];
                localsToPop = localsToPop + arguments.Length * 2; // 2 bytes per stack slot for arguments
            }
            if (andGlobals && blockContext.Contains("globals"))
            {
                < < string > > globals = blockContext["globals"];
                localsToPop = localsToPop + globals.Length * 2; // 2 bytes per stack slot for globals
            }
        }
        return localsToPop;
    }
    
    int GetOffset(string identifier, ref bool isRef)
    {
        int offset;
        bool found;
        uint iCurrent = blockList.Length;
        isRef = false;
        loop
        {
            if (iCurrent == 0)
            {
                break;
            }
            iCurrent--;
            <string,variant> blockContext = blockList[iCurrent];
            if (blockContext.Contains("locals"))
            {
                < < string > > locals = blockContext["locals"];        
                uint nlocals = locals.Length;   
                for (uint i=0; i < nlocals; i++)
                {
                    <string> local = locals[i];
                    string name = local[1];
                    if (name == identifier)
                    {
                        offset = int(i) * 2;
                        found = true;
                        break;
                    }
                }
            }
            if (blockContext.Contains("arguments"))
            {
                < < string > > arguments = blockContext["arguments"];
                uint narguments = arguments.Length;
                for (uint i=0; i < narguments; i++)
                {
                    <string> argument = arguments[i];
                    string reference = argument[0];
                    string name = argument[2];
                    if (name == identifier)
                    {
                        if (found)
                        {
                            if (!Parser.HadError)
                            {
                                // This implies we have a local at the top scope with the same name as an argument
                                Die(0x0B); // Compiler should have caught this!
                            }
                        }
                        offset = (int(narguments) - int(i)) * 2;
                        offset = 0 - offset;
                        if (reference.Length > 0)
                        {
                            isRef = true; // content of the stack slot is an absolute stack address
                        }
                        found = true;
                        break;
                    }
                }
            }
            if (found)
            {
                break;
            }
        } // loop
        
        if (found && (iCurrent != 0))
        {
            // must have been a local found in a nested block
            loop
            {
                // add the local size of all the surrounding blocks
                if (iCurrent == 0)
                {
                    break;
                }
                iCurrent--;
                <string,variant> blockContext = blockList[iCurrent];
                if (blockContext.Contains("locals"))
                {
                    < < string > > locals = blockContext["locals"];
                    uint nlocals = locals.Length;
                    offset = offset + int(nlocals) * 2;
                }
            }
        }
        if (!found)
        {
            if (!Parser.HadError)
            {
                Parser.Error("offset not found for '" + identifier + "'");
                Die(0x0B); // Compiler should have caught this!
            }
        }
        
        return offset;
    } 
    
    string GetType(string identifier)
    {
        string typeString;
        uint iCurrent = blockList.Length;
        loop
        {
            if (iCurrent == 0)
            {
                break;
            }
            iCurrent--;
            // TODO : namespaces?
            <string,variant> blockContext = blockList[iCurrent];
            if (blockContext.Contains("arguments"))
            {
                // arguments: < <ref,type,name> >
                < <string> > arguments = blockContext["arguments"];
                foreach (var argument in arguments)
                {
                    string name = argument[2];
                    if (name == identifier)
                    {
                        typeString = argument[1];
                        break;
                    }        
                }
            }       
            if (blockContext.Contains("locals"))
            {
                // locals: < <type,name> >
                < <string> > locals = blockContext["locals"];
                foreach (var local in locals)
                {
                    string name = local[1];
                    if (name == identifier)
                    {
                        typeString = local[0];
                        break;
                    }            
                }   
            }       
            if (blockContext.Contains("globals"))
            {
                // globals: < <type,name> >
                < <string> > globals = blockContext["globals"];
                foreach (var global in globals)
                {
                    string name = global[1];
                    if (name == identifier)
                    {
                        typeString = global[0];
                        break;
                    }            
                }
            }       
        }
        return typeString;
    }
}

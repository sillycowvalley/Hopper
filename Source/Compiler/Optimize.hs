program Optimize
{
    #define JSON_EXPRESS // .code and .json are generated by us so assume .json files have no errors
    
    //#define DIAGNOSTICS  
      
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/JSON/Code"
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/Parser"
    
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/Tokens/LibCalls"
    
    uses "/Source/Compiler/CodeModel/CodePoints"
    
    <string,variant> symbols;
    
    <uint,bool> methodsCalled;
    
    bool     verbose;
    bool     showSizes;
    bool     experimental;
    <string> outputLinesRemoved;
    <string> outputLinesSizes;
    uint     totalBytesRemoved;
    uint     totalMethodsRemoved;
    uint     totalMethodBytes;
    
    const uint progressSteps = 256;
    uint progressInstructions;
    ProgessNudge()
    {
        progressInstructions++;
        if ((progressInstructions % progressSteps) == 0)
        {
            Parser.ProgressTick(".");
        }
    }
    
    bool target6502;
    bool Target6502 
    { 
        get { return target6502; }
    }
    bool isTinyHopper;
    bool mergedRET0Exists;
    bool IsTinyHopper { get { return isTinyHopper; }}
    bool IsExperimental { get { return experimental; }}
    
    bool MergedRET0Exists { get { return mergedRET0Exists; } set { mergedRET0Exists = value; }}
    
    CheckTarget()
    {
        foreach (var kv in symbols)
        {
            switch (kv.key)
            {
                case "symbols":
                {
                    // preprocessor symbols
                    <string,string> pdValues = kv.value;
                    if (pdValues.Contains("HOPPER_6502"))
                    {
                        target6502 = true;
                    }
                    if (pdValues.Contains("TINY_HOPPER"))
                    {
                        isTinyHopper = true;
                    }
                    break;
                }
            }
        } // kv
    }
    
    bool CompareBeforeAndAfter(string codePath, string optPath)
    {
        bool success = true;
        // compare before and after
        file cFile = File.Open(codePath);
        file oFile = File.Open(optPath);
        long pos = 0;
        loop
        {
            if (!cFile.IsValid())
            {
                if (oFile.IsValid())
                {
                    PrintLn(".opt file has more content!");
                    success = false;
                }
                break;
            }
            if (!oFile.IsValid())
            {
                PrintLn(".opt file has less content!");
                success = false;
                break;
            }
            byte c = cFile.Read();
            byte o = oFile.Read();
            if (c != o)
            {
                PrintLn("First difference at position " + pos.ToString());
                PrintLn("  c=0x" + c.ToHexString(2));
                PrintLn("  o=0x" + o.ToHexString(2));
                success = false;
                break;
            }
            pos++;
        }
        return success;
    }
    
    ReportMethodSizes()
    {
        outputLinesSizes.Clear();
        totalMethodBytes = 0;
        
        foreach (var methodCall in methodsCalled)
        {
            uint methodIndex = methodCall.key;
            bool isCalled = methodCall.value || 
                            (methodIndex == 0); // "main" is an exception
            <byte> code = Code.GetMethodCode(methodIndex);
            uint  sizeInBytes = code.Count;
            string methodName = Code.GetMethodName(methodIndex);
            string sizeString = sizeInBytes.ToString();
            if (!isCalled)
            {
                continue;
            }
            outputLinesSizes.Append("  " + methodName.Pad(' ', 40) + sizeString.LeftPad(' ', 5));  
            totalMethodBytes = totalMethodBytes +  sizeInBytes;
        }
    }
    ReportUnreachable()
    {
        foreach (var methodCall in methodsCalled)
        {
            uint methodIndex = methodCall.key;
            bool isCalled = methodCall.value || 
                            (methodIndex == 0); // "main" is an exception
            <byte> code = Code.GetMethodCode(methodIndex);
            uint sizeInBytes = code.Count;
            string methodName = Code.GetMethodName(methodIndex);
            string sizeString = sizeInBytes.ToString();
            if (isCalled)
            {
                continue;
            }
            outputLinesRemoved.Append("  " + methodName.Pad(' ', 40) + sizeString.LeftPad(' ', 5));  
            totalBytesRemoved = totalBytesRemoved +  sizeInBytes;
            totalMethodsRemoved++;
        }
    }
    
    bool RemoveUnreachableMethods()
    {
        if (verbose)
        {
            ReportUnreachable();
        }
        bool removed;
        <string, <string,variant> > debugSymbols = GetDebugSymbols();
        <string, <string,variant> > newDebugSymbols;
        foreach (var methodCall in methodsCalled)
        {
            uint methodIndex = methodCall.key;
            if (methodCall.value || (methodIndex == 0))
            {
                 string index = "0x" + methodIndex.ToHexString(4);
                 newDebugSymbols[index] = debugSymbols[index];
            }
            else
            {
                removed = true;
            }
        }
        if (removed)
        {
            if (debugSymbols.Contains("globals"))
            {
                newDebugSymbols["globals"] = debugSymbols["globals"];
            }
            SetDebugSymbols(ref newDebugSymbols);
            
        }
        return removed;
    }

    
    bool Optimize(uint pass, ref long codeBefore, ref long codeAfter)
    {
        //PrintLn("Optimize: " + pass.ToString());
        
        bool modified = false;
        <uint> indices = Code.GetMethodIndices();
        methodsCalled.Clear();
        <uint, bool> methodsWalked;
        foreach (var index in indices)
        {
            methodsCalled[index] = false;
            methodsWalked[index] = false;
        }
        codeAfter = 0;
        
        // Inlining and inlineMethodCandidates:
        //
        // - CodePoints.Reset() clears the list of inline method candidates
        // - CodePoints.OptimizeFrameRemoval() builds the new list of inline method candidates based on size after frame removal
        // - at the end of each Optimize pass, if InlineMethodCandidatesExist, call InlineSmallMethods() which will ..
        //   .. cause RemoveUnreachableMethods() to remove completely inlined methods on the next pass
        CodePoints.Reset(); 
        
        
        
        uint methodIndex = 0; // "main"
        methodsCalled[methodIndex] = true; // "main"
        loop
        {
            methodsWalked[methodIndex] = true;
            uint size = CodePoints.Load(methodIndex, "before pass " + pass.ToString());
            if (pass == 0)
            {
                codeBefore = codeBefore + size;
            }
#ifdef DIAGNOSTICS
            bool logging = (methodIndex == 0x0000);
            if (logging)
            {
                CodePoints.DumpInstructions("Optimize: pass=" + pass.ToString() + ", methodIndex=" + methodIndex.ToHexString(4));
            }
#endif            
            CodePoints.MarkReachableInstructions();
            if (!IsTinyHopper)
            {
                if (!IsTinyHopper && CodePoints.OptimizeINCDEC())
                {
#ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeINCDEC");
                    }
#endif
                    modified = true;
                }
                if (CodePoints.OptimizeFrameRemoval())
                {
                    modified = true;
#ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeFrameRemoval");
                    }
#endif
                }
                if (CodePoints.OptimizeENTERPUSHI0())
                {
#ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeENTERPUSHI0");
                    }
#endif
                    modified = true;
                }
            }
            if (CodePoints.OptimizeDECSPRET())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeDECSPRET");
                }
#endif
                modified = true;
            }
            if (CodePoints.OptimizePUSHRETRES())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizePUSHRETRES");
                }
#endif
                modified = true;
            }
            if (CodePoints.OptimizeUnconditionalJumps())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeUnconditionalJumps");
                }
#endif
                modified = true;
            }
            if (!IsTinyHopper)
            {
                if (CodePoints.OptimizeJumpW()) // W->B
                {
#ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeJumpW");
                    }
#endif
                    modified = true;
                }
            }
            if (OptimizeJumpToJump())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeJumpToJump");
                }
#endif
                modified = true;    
            }
            if (CodePoints.OptimizePUSHPUSHSWAP())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizePUSHPUSHSWAP");
                }
#endif
                modified = true;
            }
            if (CodePoints.OptimizeCommutativeSWAP())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeCommutativeSWAP");
                }
#endif
                modified = true;
            }
            if (CodePoints.OptimizePUSH01LEEQ())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizePUSH01LEEQ");
                }
#endif
                modified = true;
            }
            if (CodePoints.OptimizeLongAddSub())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeLongAddSub");
                }
#endif
                modified = true;
            }
            
            if (Target6502)
            {
                // Only works on 6502 because 'long' is not currently a reference type on Windows:
                //    Windows would fail if we remove the POPCOPY at the end.
                // Long.Add -> Long.AddRef | Long.Inc
                //
                // TODO:
                //
                //if (OptimizeLongRef())
                //{
                //    modified = true;
                //}
                //
            
            
            }
            //
            // TODO
            //
            //if (OptimizeJumpFollowedByJump())
            //{
            //    modified = true;
            //}
            
            
            if (CodePoints.OptimizeSetters())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeSetters");
                }
#endif
                modified = true;
            }
            
            // str = str.Trim() -> Trim(ref str),  str = str.Append(x) -> Build(ref str, x), etc.
            if (CodePoints.OptimizeStringRef())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeStringRef");
                }
#endif
                modified = true;
            }
            
            // not just NOP, also JMP -> JMP + 1, can cause more short JumpToJump's to work
            if (CodePoints.OptimizeRemoveNOPs())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeRemoveNOPs");
                }
#endif
                modified = true;
            }
            if (!IsTinyHopper && CodePoints.OptimizeCOPYPOP())
            {
#ifdef DIAGNOSTICS
                if (logging)
                {
                    CodePoints.DumpInstructions("OptimizeCOPYPOP");
                }
#endif                
                modified = true;
            }
            if (pass > 0) // allow inlining to happen first
            {
                if (CodePoints.OptimizeRemoveUnreachable())
                {
                    modified = true;
#ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeRemoveUnreachable");
                    }
#endif                
                }
            }
            CodePoints.CollectMethodCalls(ref methodsCalled);
            
            size = CodePoints.Save();
            codeAfter = codeAfter + size;
            ProgessNudge();
            
            bool walkNextMethod;
            foreach (var kv in methodsCalled)
            {
                bool isCalled = kv.value;
                if (isCalled)
                {
                    uint iMethod = kv.key;
                    if (!methodsWalked[iMethod])
                    {
                        // called but not yet walked
                        methodIndex = iMethod;
                        walkNextMethod = true;
                        break;
                    }
                }
            }
            if (!walkNextMethod) // no more to walk
            {
                break;
            }
        } // loop
        
        if (RemoveUnreachableMethods())
        {
            modified = true;
        }
        if (showSizes)
        {
            ReportMethodSizes();
        }
        methodsCalled.Clear(); // just to be sure ..
        
        if (CodePoints.InlineCandidatesExist)
        {
            indices = Code.GetMethodIndices(); // reload: some methods may be gone
            foreach (var methodIndex in indices)
            {
                uint size = CodePoints.Load(methodIndex, "after pass " + pass.ToString());
                <byte> rawCode = Code.GetMethodCode(methodIndex);
                if (InlineSmallMethods(ref rawCode))
                {
                    // update the method with the optimized code: no size change, pure replacement          
                    Code.SetMethodCode(methodIndex, rawCode);
                    modified = true;
                }
                ProgessNudge();
           }
        }
        
        
        return modified;
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for Optimize:");
        PrintLn("  OPTIMIZE <code file>");
        PrintLn("    -g <c> <r> : called from GUI, not console");
        PrintLn("    -v         : verbose output");
        PrintLn("    -t         : method size after optimization");
        PrintLn("    -x         : experimental");
        
    }
    
    {
        bool success = false;
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            for (uint iArg = 0; iArg < rawArgs.Count; iArg++)
            {
                string arg = rawArgs[iArg];
                if ((arg.Length >= 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
                        case "-v":
                        {
                            verbose = true;
                        }
                        case "-t":
                        {
                            showSizes = true;
                        }
                        case "-x":
                        {
                            experimental = true;
                        }
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
          
            if (args.Count != 1)
            {
                BadArguments();
                break;
            }
            
            string ext = ".code";
            string codePath = args[0];
            if (!File.Exists(ref codePath, ref ext, "/Debug/Obj/"))
            {
                BadArguments();
            }
            
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(codePath);
                if (!Code.ParseCode(codePath, true, true))
                {
                    break;
                }
                long codeBefore;
                long codeAfter;
                string optPath = codePath;
                string symbolsPath = codePath.Replace(extension, ".json");
                string verbosePath = codePath.Replace(extension, ".txt");
                
                if (File.Exists(symbolsPath))
                {
                    if (JSON.Read(symbolsPath, ref symbols))
                    {
                        CheckTarget();
                    }
                }
                SysCalls.New(); // initialize
                LibCalls.New();
                CodePoints.LoadSysCallIndices();
                
                uint pass = 0;
                loop
                {
                    if (!Optimize(pass, ref codeBefore, ref codeAfter))
                    {
                        break;
                    }
                    pass++;
                }
                if (MergedRET0Exists)
                {
                    <uint> indices = Code.GetMethodIndices(); // reload: some methods may be gone
                    foreach (var methodIndex in indices)
                    {
                        uint size = CodePoints.Load(methodIndex, "replacing MERGEDRET0");
                        <byte> rawCode = Code.GetMethodCode(methodIndex);
                        if (ReplaceMergedRET0(ref rawCode))
                        {
                            Code.SetMethodCode(methodIndex, rawCode);
                        }
                   }
                }
                
                File.Delete(optPath);
                if (!Code.ExportCode(optPath)) // after
                {
                    break;
                }
                success = true;
                if (!Parser.IsInteractive())
                {
                    PrintLn();
                    if (verbose || showSizes)
                    {
                        file logFile = File.Create(verbosePath);
                        bool addNewLine;
                        if (outputLinesSizes.Count != 0)
                        {
                            string content = "Method sizes after optimization:";
                            logFile.Append(content + char(0x0D));
                            PrintLn(content);
                            uint count;
                            foreach (var str in outputLinesSizes)
                            {
                                logFile.Append(str + char(0x0D));
                                PrintLn(str);
                                count++;
                            }
                            string space;
                            string sizeString = totalMethodBytes.ToString();
                            content = space.Pad(' ', 42) + sizeString.LeftPad(' ', 5) +
                                      " (" + count.ToString() + " methods)";
                            logFile.Append(content + char(0x0D));
                            PrintLn(content);
                            addNewLine = true;
                        }
                        if (totalMethodsRemoved > 0)
                        {
                            if (addNewLine)
                            {
                                logFile.Append("" + char(0x0D));
                                PrintLn();
                            }
                            string content = "Unreachable Code (includes inlined and removed):";
                            logFile.Append(content + char(0x0D));
                            PrintLn(content);
                            foreach (var str in outputLinesRemoved)
                            {
                                logFile.Append(str + char(0x0D));
                                PrintLn(str);
                            }
                            string space;
                            string sizeString = totalBytesRemoved.ToString();
                            content = space.Pad(' ', 42) + sizeString.LeftPad(' ', 5) +
                                      " (" + totalMethodsRemoved.ToString() + " methods)";
                            logFile.Append(content + char(0x0D));
                            PrintLn(content);
                        }
                        <string,variant> gValues;
                        foreach (var kv in symbols)
                        {
                            switch (kv.key)
                            {
                                case "globals":
                                {
                                    // globals
                                    gValues = kv.value;
                                    break;
                                }
                            }
                        } // kv
                        logFile.Flush();
                    }
                    Print("Success, " + codeBefore.ToString() + "->" + codeAfter.ToString() + " bytes of code,", Colour.ProgressText, Colour.ProgressFace);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() + "s", Colour.ProgressHighlight, Colour.ProgressFace);
                }
                else
                {
                    Parser.ProgressDone();
                }
                
                break;
            }
            break;
        } // loop
        if (!success)
        {
            Diagnostics.SetError(0x0E);
        }   
    } // main
    
    
}

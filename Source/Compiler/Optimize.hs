program Optimize
{
    #define JSON_EXPRESS // .code and .json are generated by us so assume .json files have no errors
    //#define DIAGNOSTICS  
    
    //#define EXPERIMENTAL
      
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "JSON/JSON"
    uses "JSON/Code"
    
    uses "Tokens/Token"
    uses "Tokens/Scanner"
    uses "Tokens/Parser"
    
    uses "CodeGen/Instructions"
    uses "Tokens/SysCalls"
    uses "Tokens/LibCalls"
    
    uses "CodeModel/CodePoints"
    
    <string,variant> symbols;
    <uint,bool> methodsCalled;
    <uint,bool> methodsModified;
    
    bool     verbose;
    bool     showSizes;
    <string> outputLinesRemoved;
    <string> outputLinesSizes;
    uint     totalBytesRemoved;
    uint     totalMethodsRemoved;
    uint     totalMethodBytes;
    
    const uint progressSteps = 512;
    uint progressInstructions;
    
    ProgessNudge()
    {
        progressInstructions++;
        if ((progressInstructions % progressSteps) == 0)
        {
            Parser.ProgressTick("o"); // optimizer
        }
    }

    bool isValueTypeRuntime;
    bool IsValueTypeRuntime { get { return isValueTypeRuntime; } set { isValueTypeRuntime = value; }}
        
    bool mergedRET0Exists;
    bool MergedRET0Exists { get { return mergedRET0Exists; } set { mergedRET0Exists = value; }}
    
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
            SetDebugSymbols(newDebugSymbols);
            
        }
        return removed;
    }
    long verboseStart;
    string verboseName;
    bool verboseBrief;
    showElapsedStart(bool brief)
    {
        verboseBrief = brief;
        if (!verboseBrief) { PrintLn(); }
        verboseName = "";
        verboseStart = Millis;
    }
    updateElapsed()
    {
        long elapsed = Millis - verboseStart;
        if (verboseName.Length != 0)
        {
            if (elapsed > 250)
            {
                Print(" " + verboseName + "(" + elapsed.ToString() + ")", Colour.Red, Colour.Black);
            }
            else if (elapsed > 50)
            {
                Print(" " + verboseName + "(" + elapsed.ToString() + ")", Colour.MatrixRed, Colour.Black);
            }
            else if (!verboseBrief)
            {
                Print(" " + verboseName, Colour.Ocean, Colour.Black);
            }
        }
    }
    showElapsed(string name)
    {
        updateElapsed();
        verboseStart = Millis;
        verboseName = name;
    }
    showElapsedEnd()
    {
        updateElapsed();
        if (!verboseBrief) { PrintLn(); }
    }
    
    bool Optimize(uint pass, ref long codeBefore, ref long codeAfter)
    {
        //PrintLn("Optimize: " + pass.ToString());
        
        pairList.Clear();
        
        bool anyModified = false;
        <uint> indices = Code.GetMethodIndices();
        methodsCalled.Clear();
        if (pass == 0)
        {
            methodsModified.Clear();
            foreach (var index in indices)
            {
                methodsModified[index] = true;
            }
        }
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
            bool methodModified = false;
            
            bool optimizeMethod = methodsModified[methodIndex];
            methodsModified[methodIndex] = false;
            
            long start = Millis;
            bool verboseTarget = IsExperimental && ( false
                                           // || (methodIndex == 0x0015) 
                                           // || (methodIndex == 0x00F3) 
                                           // || (methodIndex == 0x0174)
                                             );
            bool verbose = IsExperimental;
            
            methodsWalked[methodIndex] = true;
            uint size = CodePoints.Load(methodIndex, "before pass " + pass.ToString());
            if (pass == 0)
            {
                codeBefore += size;
            }
#ifdef DIAGNOSTICS
            bool logging = (methodIndex == 0x0000);
            if (logging)
            {
                CodePoints.DumpInstructions("Optimize: pass=" + pass.ToString() + ", methodIndex=" + methodIndex.ToHexString(4));
            }
#endif       
            if (verbose) { showElapsedStart(!verboseTarget); } 
            if (verbose) { showElapsed("@"); } 
            CodePoints.MarkReachableInstructions();
            
            if (optimizeMethod)
            {
                if (!NoPackedInstructions)
                {
                    if (verbose) { showElapsed("a"); } 
                    if (!NoPackedInstructions && CodePoints.OptimizeINCDEC())
                    {
    #ifdef DIAGNOSTICS
                        if (logging)
                        {
                            CodePoints.DumpInstructions("OptimizeINCDEC");
                        }
    #endif
                        methodModified = true;
                    }
                    if (verbose) { showElapsed("b"); } 
                    if (CodePoints.OptimizeFrameRemoval())
                    {
                        methodModified = true;
    #ifdef DIAGNOSTICS
                        if (logging)
                        {
                            CodePoints.DumpInstructions("OptimizeFrameRemoval");
                        }
    #endif
                    }
                    if (verbose) { showElapsed("c"); } 
                    if (CodePoints.OptimizeENTERPUSHI0())
                    {
    #ifdef DIAGNOSTICS
                        if (logging)
                        {
                            CodePoints.DumpInstructions("OptimizeENTERPUSHI0");
                        }
    #endif
                        methodModified = true;
                    }
                }
                if (verbose) { showElapsed("d"); } 
                if (CodePoints.OptimizeDECSPRET())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeDECSPRET");
                    }
    #endif
                    methodModified = true;
                }
                if (verbose) { showElapsed("e"); } 
                if (CodePoints.OptimizePUSHRETRES())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizePUSHRETRES");
                    }
    #endif
                    methodModified = true;
                }
                if (verbose) { showElapsed("f"); } 
                if (CodePoints.OptimizeSymmetricCompare())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeSymetricCompare");
                    }
    #endif
                    methodModified = true;
                }
                if (verbose) { showElapsed("g"); } 
                if (CodePoints.OptimizeUnconditionalJumps(/*verbose*/))
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeUnconditionalJumps");
                    }
    #endif
                    methodModified = true;
                }
                if (!NoPackedInstructions)
                {
                    if (verbose) { showElapsed("h"); } 
                    if (CodePoints.OptimizeJumpW()) // W->B
                    {
    #ifdef DIAGNOSTICS
                        if (logging)
                        {
                            CodePoints.DumpInstructions("OptimizeJumpW");
                        }
    #endif
                        methodModified = true;
                    }
                }
                if (verbose) { showElapsed("i"); } 
                if (CodePoints.OptimizeJumpToJump())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeJumpToJump");
                    }
    #endif
                    methodModified = true;    
                }
                // JXx +2 Jx d -> JNXx d
                if (verbose) { showElapsed("j"); } 
                if (CodePoints.OptimizeJZxJx())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeJZxJx");
                    }
    #endif
                    methodModified = true;    
                }
                // PUSHIB 2 MUL -> PUSHIB 1 BITSHL
                if (verbose) { showElapsed("k"); } 
                if (false && CodePoints.OptimizeMULSHL())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeMULSHL");
                    }
    #endif
                    methodModified = true;    
                }
                if (verbose) { showElapsed("l"); } 
                if (CodePoints.OptimizePUSHPUSHSWAP())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizePUSHPUSHSWAP");
                    }
    #endif
                    methodModified = true;
                }
                if (verbose) { showElapsed("m"); } 
                if (CodePoints.OptimizeCommutativeSWAP())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeCommutativeSWAP");
                    }
    #endif
                    methodModified = true;
                }
                if (CodePoints.OptimizePUSH01LEEQ())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizePUSH01LEEQ");
                    }
    #endif
                    methodModified = true;
                }
                if (verbose) { showElapsed("n"); } 
                if (CodePoints.OptimizeLongAddSub())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeLongAddSub");
                    }
    #endif
                    methodModified = true;
                }
                
                if ((CodeStream.Target6502 || CodeStream.TargetMinimal))
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
            
                if (verbose) { showElapsed("o"); } 
                if (CodePoints.OptimizeSetters())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeSetters");
                    }
    #endif
                    methodModified = true;
                }
                
                // str = str.Trim() -> Trim(ref str),  str = str.Append(x) -> Build(ref str, x), etc.
                if (verbose) { showElapsed("p"); } 
                if (CodePoints.OptimizeStringRef())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeStringRef");
                    }
    #endif
                    methodModified = true;
                }
                
                // not just NOP, also JMP -> JMP + 1, can cause more short JumpToJump's to work
                if (verbose) { showElapsed("q"); } 
                if (CodePoints.OptimizeRemoveNOPs(/*verboseTarget*/))
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeRemoveNOPs");
                    }
    #endif
                    methodModified = true;
                }
                if (verbose) { showElapsed("r"); } 
                if (!NoPackedInstructions && CodePoints.OptimizeCOPYPOP())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeCOPYPOP");
                    }
    #endif                
                    methodModified = true;
                }
                if (verbose) { showElapsed("s"); } 
                if (!NoPackedInstructions && CodePoints.OptimizeSYSCALL00())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeSYSCALL00");
                    }
    #endif                
                    methodModified = true;
                }
                if (verbose) { showElapsed("t"); } 
                if (!NoPackedInstructions && CodePoints.OptimizePUSHIBB())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizePUSHIBB");
                    }
    #endif                
                    methodModified = true;
                }
                if (verbose) { showElapsed("u"); } 
                if (!NoPackedInstructions && CodePoints.OptimizePUSHLOCALBB())
                {
    #ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizePUSHLOCALBB");
                    }
    #endif                
                    methodModified = true;
                }
            } // optimizeMethod
            
            // allow inlining to happen first
            bool removeUnreachables = (pass == 1) || optimizeMethod;
            if (removeUnreachables)
            {
                if (verbose) { showElapsed("v"); } 
                if (CodePoints.OptimizeRemoveUnreachable())
                {
                    methodModified = true;
#ifdef DIAGNOSTICS
                    if (logging)
                    {
                        CodePoints.DumpInstructions("OptimizeRemoveUnreachable");
                    }
#endif                
                }
            }
            if (optimizeMethod)
            {
                if (verbose) { showElapsed("w"); } 
            }
            CodePoints.CollectMethodCalls(methodsCalled);
            if (verbose) { showElapsedEnd(); } 
            
            if (false && IsExperimental)
            {
                CodePoints.CountPairs(pairList);
            }
            
            if (methodModified)
            {
                size = CodePoints.Save();
                codeAfter = codeAfter + size;
                anyModified = true;
            }
            else
            {
                codeAfter = codeAfter + size; // same as size before
            }
            
            methodsModified[methodIndex] = methodModified;
            
            if (!IsExperimental)
            {
                ProgessNudge();
            }
                       
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
        } // method loop
        
        if (RemoveUnreachableMethods())
        {
            anyModified = true;
        }
        if (showSizes)
        {
            ReportMethodSizes();
        }
        methodsCalled.Clear(); // just to be sure ..
        if (false && IsExperimental)
        {
            ListCompareDelegate sorter = CompairPair;
            pairList.Sort(sorter);
            uint topN = 35;
            PrintLn();
            foreach (var opcodepair in pairList)
            {
                Instruction opCode0 = opcodepair.OpCode0;
                Instruction opCode1 = opcodepair.OpCode1;
                Instruction opCode2 = opcodepair.OpCode2;
                
                Print((opcodepair.Count).ToString());
                if (opCode2 != Instruction.NOP)
                {
                    Print(" " + Instructions.ToString(opCode2)); 
                }
                PrintLn(" " + Instructions.ToString(opCode1) 
                            + " " + Instructions.ToString(opCode0));
                topN--;
                if (topN == 0) { break; }
            }
        }
        
        if (CodePoints.InlineCandidatesExist)
        {
            indices = Code.GetMethodIndices(); // reload: some methods may be gone
            foreach (var methodIndex in indices)
            {
                uint size = CodePoints.Load(methodIndex, "after pass " + pass.ToString());
                <byte> rawCode = Code.GetMethodCode(methodIndex);
                if (InlineSmallMethods(rawCode))
                {
                    // update the method with the optimized code: no size change, pure replacement          
                    Code.SetMethodCode(methodIndex, rawCode);
                    anyModified = true;
                    methodsModified[methodIndex] = true;
                }
                ProgessNudge();
           }
        }
        return anyModified;
    }
    
    record OpCodePair
    {
        Instruction OpCode0;
        Instruction OpCode1;
        Instruction OpCode2;
        bool        private;
        long        Count;
    }
    <OpCodePair> pairList;
    int CompairPair(<OpCodePair> pairList, uint ai, uint bi)
    {
        int result;
        
        OpCodePair a = pairList[ai];
        OpCodePair b = pairList[bi];
        long aCount = a.Count;
        long bCount = b.Count;
        
        // descending order:
        if      (aCount < bCount) { result = +1; }
        else if (aCount > bCount) { result = -1; }
        return result;
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
            bool argIsExperimental;
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
                            argIsExperimental = true;
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
                string verbosePath = codePath.Replace(extension, ".txt");
                string symbolsPath = codePath.Replace(extension, ".sym");
                
                Symbols.New();
                if (File.Exists(symbolsPath))
                {
                    if (Symbols.Import(symbolsPath, false))
                    {
                        CodeStream.InitializeSymbolShortcuts();
                        IsValueTypeRuntime   = Symbols.DefineExists("VALUE_TYPE_RUNTIME");
                    }
                }
                if (argIsExperimental)
                {
                    IsExperimental = argIsExperimental;
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
                        if (ReplaceMergedRET0(rawCode))
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
                            if (kv.key == "globals")
                            {
                                // globals
                                gValues = kv.value;
                                break;
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

program Inline
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/CodeGen/Instructions"

    uint space;
    uint result;
    
    byte[512] code; 

    WriteCode()
    {
        code[0] = byte(Instruction.PUSHIB);      // push 7
        code[1] = 7;
        code[2] = byte(Instruction.PUSHIB);      // push 9
        code[3] = 9;
        code[4] = byte(Instruction.MUL);         // multiply
        code[5] = byte(Instruction.POPGLOBALB);  // pop result to a global
        code[6] = byte(&result);
        code[7] = byte(Instruction.EXIT);
    }   
    RunCode()
    {
        if (System.Call(code) == 0)
        {
        
        }
    }
    {
        WriteCode();
        uint pc = 0;
        loop
        {   
            PrintLn();
            byte opCode = code[pc];
            pc++;
            Instruction instruction = Instruction(opCode);
            byte operands = Instructions.GetSimpleOperandWidth(instruction); // works because we never inline JIX
            Print("0x" + pc.ToHexString(4) + " " + Instructions.ToString(instruction));
            while (operands > 0)
            {
                byte b = code[pc];
                Print(" 0x" + b.ToHexString(2));
                operands--;
                pc++;
            }
            if (instruction == Instruction.EXIT) 
            {
                PrintLn();
                break;
            }
        }
        
        RunCode();
        
        PrintLn("result=" + result.ToString());
        
        Key k = ReadKey();
    }
}

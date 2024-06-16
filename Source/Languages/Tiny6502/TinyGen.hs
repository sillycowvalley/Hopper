unit TinyGen
{
    uses "TinyCode"
    
    record Instruction
    {
        string Name;
        string Type;
        uint   Operand;
    }
    
    // Instructions:
    //
    //
    
    <Instruction> currentStream;
    
    Append(string name, string tp, uint operand)
    {
        Instruction instruction;
        instruction.Name = name;
        instruction.Type = tp;
        instruction.Operand = operand;
        currentStream.Append(instruction);
    }
    Append(string name, string tp)
    {
        Instruction instruction;
        instruction.Name = name;
        instruction.Type = tp;
        currentStream.Append(instruction);
    }
    Append(string name)
    {
        Instruction instruction;
        instruction.Name = name;
        currentStream.Append(instruction);
    }
}

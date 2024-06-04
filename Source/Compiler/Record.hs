unit Record
{
    uses "Symbols"
    
    < <string> > Find(string recordName)
    {
        return Symbols.FindRecord(recordName);
    }
    
    // record ShellObject {
    //     string Path;
    // }                  'ShellObject'          'Path'                0                   'string'
    bool FindMember(string thisTypeString, string memberName, ref byte iMember, ref string actualType) 
    {
        bool success;
        loop
        {
            if (!Types.IsRecord(thisTypeString))
            {  
                break;
            }
            string recordName = Types.QualifyRecord(thisTypeString);
            < <string> > members = Record.Find(recordName);
            bool memberFound;
            if (members.Count != 0)
            {
                iMember = 0;
                foreach (var v in members)
                {
                    string mName = v[0];
                    string mType = v[1];
                    if (mName == memberName)
                    {
                        actualType = mType;
                        memberFound = true;
                        break;
                    }
                    iMember++;
                }
            }
            if (!memberFound)
            {
                Parser.Error("invalid record member");
                break;
            }
            if ((memberName[0]).IsLower() && !recordName.StartsWith(Types.CurrentNamespace + ".")) // private member
            {
                Parser.Error("'" + memberName + "' is a private member");
                break;
            }
            success = true;
            break;
        } // loop
        return success;
    }
    
    
    InitializeMembers(string thisTypeString)
    {
        bool success;
        string recordName = Types.QualifyRecord(thisTypeString);
        < <string> > members = Record.Find(recordName);
        foreach (var v in members)
        {
            string memberName = v[0];
            string memberType = v[1];
            CodeStream.AddInstruction(Instruction.DUP0);
            Expression.InitializeVariable(memberType, true); // new variable at [top]
            CodeStream.AddInstructionSysCall0("List", "Append");
        }
    }
    
    LazyInitializeMembers(string thisTypeString)
    {
        // list is at [top]
        CodeStream.AddInstruction(Instruction.DUP0); 
        CodeStream.AddInstructionSysCall0("List", "Count_Get");
        uint jumpPast = CodeStream.NextAddress;
        CodeStream.AddInstructionJump(Instruction.JNZ);
        
        string recordName = Types.QualifyRecord(thisTypeString);
        < <string> > members = Record.Find(recordName);
        foreach (var v in members)
        {
            string memberName = v[0];
            string memberType = v[1];
            CodeStream.AddInstruction(Instruction.DUP0);
            InitializeVariable(memberType, true); // new variable at [top]
            CodeStream.AddInstructionSysCall0("List", "Append");
        }
        uint pastAddress = CodeStream.NextAddress;
        CodeStream.PatchJump(jumpPast, pastAddress);
    }
    
}

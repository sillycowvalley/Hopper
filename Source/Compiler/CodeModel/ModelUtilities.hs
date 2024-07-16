unit ModelUtilities
{
    bool IsJumpOrExitInstruction(Instruction opCode)
    {
        return IsJumpInstruction(opCode)
            || IsJumpIXInstruction(opCode)
            || IsMethodExitInstruction(opCode);
    }
    bool IsJumpInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.J:
            case Instruction.JB:
            case Instruction.JZ:
            case Instruction.JZB:
            case Instruction.JNZ:
            case Instruction.JNZB:
            {
                return true;
            }
        }
        return false;
    }
    
    bool IsJumpIXInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.JIX:
            case Instruction.JIXB:
            {
                return true;
            }
        }
        return false;
    }
    bool IsJumpWInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.J:
            case Instruction.JZ:
            case Instruction.JNZ:
            {
                return true;
            }
        }
        return false;
    }
    bool IsJumpBInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            {
                return true;
            }
        }
        return false;
    }
    bool IsConditionalJumpInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.JZ:
            case Instruction.JZB:
            case Instruction.JNZ:
            case Instruction.JNZB:
            {
                return true;
            }
        }
        return false;
    }
    bool IsUnconditionalJumpInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.J:
            case Instruction.JB:
            {
                return true;
            }
        }
        return false;
    }
    bool IsMethodExitInstruction(Instruction opCode)
    {
        switch(opCode)
        {
            case Instruction.RETFAST:
            case Instruction.RETB:
            case Instruction.RET:
            case Instruction.RET0:
            case Instruction.MERGEDRET0:
            case Instruction.RETRES:
            case Instruction.RETRESB:
            case Instruction.DIE:
            {
                return true;
            }
        }
        return false;
    }
    bool IsLOCALInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.PUSHLOCALB:
            case Instruction.POPLOCALB:
            case Instruction.PUSHLOCAL:
            case Instruction.POPLOCAL:
            case Instruction.PUSHLOCALB00:
            case Instruction.PUSHLOCALB01:
            case Instruction.POPLOCALB00:
            case Instruction.POPLOCALB01:
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYLOCALB00:
            case Instruction.PUSHLOCALBB:
            case Instruction.POPCOPYLOCAL:
            case Instruction.POPCOPYLOCALB01:
            {
                return true;
            }
        }
        return false;
    }
    bool IsLOCALCOPYInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYLOCALB00:
            case Instruction.POPCOPYLOCAL:
            case Instruction.POPCOPYLOCALB01:
            {
                return true;
            }
        }
        return false;
    }
    Instruction SwitchToCOPYPOP(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.POPLOCALB:
            {
                opCode = Instruction.POPCOPYLOCALB;
            }
            case Instruction.POPLOCALB00:
            {
                opCode = Instruction.POPCOPYLOCALB00;
            }
            case Instruction.POPLOCALB01:
            {
                opCode = Instruction.POPCOPYLOCALB01;
            }
            case Instruction.POPLOCAL:
            {
                opCode = Instruction.POPCOPYLOCAL;
            }
            case Instruction.POPGLOBALB:
            {
                opCode = Instruction.POPCOPYGLOBALB;
            }
            case Instruction.POPGLOBAL:
            {
                opCode = Instruction.POPCOPYGLOBAL;
            }
            case Instruction.POPRELB:
            {
                opCode = Instruction.POPCOPYRELB;
            }
            case Instruction.POPREL:
            {
                opCode = Instruction.POPCOPYREL;
            }
        }
        return opCode;
    }
    bool IsPOPInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.POPLOCALB:   
            case Instruction.POPGLOBALB:   
            case Instruction.POPLOCAL:   
            case Instruction.POPGLOBAL:   
            case Instruction.POPLOCALB00:   
            case Instruction.POPLOCALB01:   
            case Instruction.POPRELB:   
            case Instruction.POPREL:   
            {
                return true;
            }
        }
        return false;
    }
    
    bool IsSinglePUSHInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.PUSHIB:
            case Instruction.PUSHI0:
            case Instruction.PUSHI1:
            case Instruction.PUSHI:
            case Instruction.PUSHIM1:
            case Instruction.PUSHLOCALB:
            case Instruction.PUSHGLOBALB:
            case Instruction.PUSHRELB:
            case Instruction.PUSHLOCAL:
            case Instruction.PUSHGLOBAL:
            case Instruction.PUSHREL:
            case Instruction.PUSHLOCALB00:
            case Instruction.PUSHLOCALB01:
            {
                return true;
            }
        }
        return false;
    }
    bool IsPUSHImmediateInstruction(Instruction opCode, ref uint operand)
    {
        switch (opCode)
        {
            case Instruction.PUSHI:
            case Instruction.PUSHIB:
            {
                // from code
                // operand = operand;
                return true;
            }
            case Instruction.PUSHI0:
            {
                operand = 0;
                return true;
            }
            case Instruction.PUSHI1:
            {
                operand = 1;
                return true;
            }
        }
        return false;
    }
    
}

unit ModelUtilities
{
    bool IsJumpInstruction(Instruction opCode)
    {
        switch (opCode)
        {
            case Instruction.JW:
            case Instruction.JB:
            case Instruction.JZW:
            case Instruction.JZB:
            case Instruction.JNZW:
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
            case Instruction.JIXW:
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
            case Instruction.JW:
            case Instruction.JZW:
            case Instruction.JNZW:
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
            case Instruction.JZW:
            case Instruction.JZB:
            case Instruction.JNZW:
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
            case Instruction.JW:
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
            case Instruction.RETW:
            case Instruction.RET0:
            case Instruction.RETRETW:
            case Instruction.RETRETB:
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
            case Instruction.PUSHLOCALW:
            case Instruction.POPLOCALW:
            case Instruction.PUSHLOCALB00:
            case Instruction.PUSHLOCALB02:
            case Instruction.POPLOCALB00:
            case Instruction.POPLOCALB02:
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYLOCALB00:
            case Instruction.PUSHLOCALBB:
            case Instruction.POPCOPYLOCALW:
            case Instruction.POPCOPYLOCALB02:
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
            case Instruction.POPCOPYLOCALW:
            case Instruction.POPCOPYLOCALB02:
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
            case Instruction.POPLOCALB02:
            {
                opCode = Instruction.POPCOPYLOCALB02;
            }
            case Instruction.POPLOCALW:
            {
                opCode = Instruction.POPCOPYLOCALW;
            }
            case Instruction.POPGLOBALB:
            {
                opCode = Instruction.POPCOPYGLOBALB;
            }
            case Instruction.POPGLOBALW:
            {
                opCode = Instruction.POPCOPYGLOBALW;
            }
            case Instruction.POPRELB:
            {
                opCode = Instruction.POPCOPYRELB;
            }
            case Instruction.POPRELW:
            {
                opCode = Instruction.POPCOPYRELW;
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
            case Instruction.POPRELB:   
            case Instruction.POPLOCALW:   
            case Instruction.POPGLOBALW:   
            case Instruction.POPRELW:   
            case Instruction.POPLOCALB00:   
            case Instruction.POPLOCALB02:   
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
            case Instruction.PUSHIW:
            case Instruction.PUSHIM1:
            case Instruction.PUSHLOCALB:
            case Instruction.PUSHGLOBALB:
            case Instruction.PUSHRELB:
            case Instruction.PUSHLOCALW:
            case Instruction.PUSHGLOBALW:
            case Instruction.PUSHRELW:
            case Instruction.PUSHLOCALB00:
            case Instruction.PUSHLOCALB02:
            {
                return true;
            }
        }
        return false;
    }
    
}

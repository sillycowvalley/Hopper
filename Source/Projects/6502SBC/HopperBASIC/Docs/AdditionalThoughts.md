### 6. Value Stack Helper (Value.asm - new unit)

```asm
unit Value
{
    // Check if value stack has a value
    // Output: C if value exists, NC if empty
    HasValue()
    {
        loop
        {
            LDA ZP.VSP
            if (Z) { CLC } else { SEC }
            break;
        }
    }
    
    // Push void value
    PushVoid()
    {
        loop
        {
            LDA #Types.Void
            PHA
            STZ ZP.ACCL
            STZ ZP.ACCH
            Stacks.PushValue();
            break;
        }
    }
    
    // Pop and discard top value
    Pop()
    {
        loop
        {
            Stacks.PopValue();
            break;
        }
    }
}
```
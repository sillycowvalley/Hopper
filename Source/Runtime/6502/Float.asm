unit Float
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    New()
    {
        // IEEE +0.0
        LDA # 0
        STA ZP.LNEXT0
        STA ZP.LNEXT1
        STA ZP.LNEXT2
        STA ZP.LNEXT3
        LDA # Types.Float
        Long.pushNewFromL();
    }
    
    NewFromConstant()
    {
        LDA # Types.Float
        Long.newFromConstant();
    }
    FromBytes()
    {
        LDA # Types.Float
        Long.fromBytes();
    }
    GetByte()
    {
        Long.GetByte();
    }
}

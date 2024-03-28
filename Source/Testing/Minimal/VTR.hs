program ValueTypeRuntimeTests
{
    #define CPU_Z80
    
    uses "/Source/Minimal/System"
    uses "/Source/Minimal/Serial"
    uses "/Source/Minimal/Diagnostics"
    
    Failure(uint instance)
    {
        uint local0 = instance;
        uint local1 = instance;
        uint local2 = instance;
        uint local3 = instance;
        Die(0x0B);    
    }
    
    uint uiGlobal;
    
    uint uiFunction()
    {
        uint a = 1;
        a = a + 36;
        return a;
    }
    
    ADD()
    {
        uiGlobal = 2;
        uint local = 2;
        local = local + uiFunction();
        if (local != 39)
        {
            Failure(1);
        }
        local = uiGlobal + 2 + local;
        if (local != 43)
        {
            Failure(2);
        }
        local++;
        if (local != 44)
        {
            Failure(3);
        }
        uiGlobal++;
        if (uiGlobal != 3)
        {
            Failure(4);
        }
        
    }
    SUB()
    {
        uiGlobal = 4;
        uint local = 2;
        local = uiGlobal - local;
        if (local != 2)
        {
            Failure(5);
        }
        local--;
        if (local != 1)
        {
            Failure(6);
        }
        uiGlobal--;
        if (uiGlobal != 3)
        {
            Failure(7);
        }
        local = 10;
        local = local - 2;
        local -= 2;
        if (local != 6)
        {
            Failure(8);
        }
        uiGlobal = local - 2;
        if (uiGlobal != 4)
        {
            Failure(9);
        }
    }
    EQ()
    {
        uint a = 42;
        uint b = 42;
        if (3 == 4)
        {
             Failure(10);       
        }
        if (4 != 4)
        {
             Failure(11);       
        }
        if (4 < 4)
        {
             Failure(12);       
        }
        if (4 > 4)
        {
             Failure(13);       
        }
        if (4 > 2)
        {
        }
        else
        {
            Failure(14);       
        }
        if (2 < 4)
        {
        }
        else
        {
            Failure(15);       
        }
        if (4 <= 4)
        {
             
        }
        else
        {
            Failure(16);       
        }
        if (4 >= 4)
        {
             
        }
        else
        {
            Failure(17);       
        }
        if (4 >= 2)
        {
        }
        else
        {
            Failure(18);       
        }
        if (2 <= 4)
        {
        }
        else
        {
            Failure(19);       
        }
        if (a == b)
        {
        }
        else
        {
            Failure(20);       
        }
        if (a != b)
        {
            Failure(21);       
        }
        if (a <= b)
        {
        }
        else
        {
            Failure(21);       
        }
        
        
    }
    
    
    Hopper()
    {
        ADD(); // ADD, ADDB, INCLOCALB, INCGLOBALB NE, JZB
        SUB(); // SUB, SUBB, DECLOCALB, DECGLOBALB
        EQ();  // EQ NE
    }
}

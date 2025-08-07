unit BASICSysCalls
{
   
   // System call IDs for SYSCALL opcode
   // Bit layout: Bit 7-3: Function ID, Bit 2: Return Value, Bit 1-0: Argument Count
   enum SysCallType
   {
       // System functions (ID 1-2)
       PrintValue   = (0b00001 << 3) | (0 << 2) | 0b01,  // ID=1, void,    1 arg  = 0x09
       PrintChar    = (0b00010 << 3) | (0 << 2) | 0b01,  // ID=2, void,    1 arg  = 0x11
       
       // Built-in functions (ID 3-9)
       Abs          = (0b00011 << 3) | (1 << 2) | 0b01,  // ID=3, returns, 1 arg  = 0x1D
       Rnd          = (0b00100 << 3) | (1 << 2) | 0b01,  // ID=4, returns, 1 arg  = 0x25
       Millis       = (0b00101 << 3) | (1 << 2) | 0b00,  // ID=5, returns, 0 args = 0x2C  
       Seconds      = (0b00110 << 3) | (1 << 2) | 0b00,  // ID=6, returns, 0 args = 0x34
       Delay        = (0b00111 << 3) | (0 << 2) | 0b01,  // ID=7, void,    1 arg  = 0x39
       Peek         = (0b01000 << 3) | (1 << 2) | 0b01,  // ID=8, returns, 1 arg  = 0x45
       Poke         = (0b01001 << 3) | (0 << 2) | 0b10,  // ID=9, void,    2 args = 0x4A
   }
   
   // Execute SYSCALL opcode - system call with flags-based dispatch
   const string executeSysCallTrace = "SYSCALL // System call";
   ExecuteSysCall()
   {
#ifdef TRACE
       LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL  LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH  Trace.MethodEntry();
#endif
       loop
       {

           FetchOperandByte();  // A = SYSCALL ID
           States.CanContinue();
           if (NC) { break; }
           
           TAY  // Preserve full SYSCALL ID in Y
           
#ifdef TRACE
           //TYA Debug.HOut(); Debug.NL(); 
           //CMP # SysCallType.Delay
           //if (Z)
           //{
           //   DumpStack();
           //}
#endif           
           // Handle arguments based on count (bits 1-0)
           TYA
           AND #0b00000011   // A = argument count
           switch (A)
           {
               case 0: { /* No arguments to pop */ }
               case 1: 
               { 
                   Stacks.PopTop();  // Arg in ZP.TOP*
               }
               case 2: 
               { 
                   Stacks.PopTopNext(); // First arg in ZP.NEXT, Second arg in ZP.TOP*
               }
               case 3: 
               { 
                   // Handle 3-argument functions (future expansion)
                   TODO(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }
           
           switch (Y) // full SYSCALL ID
           {
               case SysCallType.PrintValue:    // ID = 1
               {
                   CLC // no quotes
                   BASICTypes.PrintValue();  // Uses ZP.TOP*, CLC = no quotes
                   SEC // all good
               }
               case SysCallType.PrintChar:     // ID = 2
               {
                   // Character to print is in ZP.TOP (1 argument)
                   LDA ZP.TOPL
                   Serial.WriteChar();
               }
               case SysCallType.Abs:           // ID = 3
               {
                   // ABS function - compute absolute value
                   // Input: ZP.TOP* contains value and type
                   // Output: ZP.TOP* contains absolute value
                   LDA ZP.TOPT
                   switch (A)
                   {
                       case BASICType.INT:
                       {
                           // INT absolute value - check if negative
                           LDA ZP.TOPH
                           if (MI)
                           {
                               // Negate using two's complement
                               SEC
                               LDA #0
                               SBC ZP.TOPL
                               STA ZP.TOPL
                               LDA #0
                               SBC ZP.TOPH
                               STA ZP.TOPH
                           }
                       }
                       case BASICType.WORD:
                       case BASICType.BYTE:
                       {
                           // WORD/BYTE always positive (unsigned)
    
                       }
                       default:
                       {
                           Error.TypeMismatch(); BIT ZP.EmulatorPCL
                           States.SetFailure();
                           break;
                       }
                   }
               }
               case SysCallType.Rnd:           // ID = 4
               {
                   // RND function - random number generation
                   // Input: ZP.TOP* contains max value
                   // Output: ZP.TOP* contains random number 0 to max-1
                   
                   // TODO: Replace with actual implementation
                   TODO(); BIT ZP.EmulatorPCL
               }
               case SysCallType.Millis:        // ID = 5
               {
                   // MILLIS function - get system timer
                   LDA ZP.TICK0 STA ZP.TOPL     // Get system timer
                   LDA ZP.TICK1 STA ZP.TOPH
                   LDA #BASICType.WORD STA ZP.TOPT
               }
               case SysCallType.Seconds:       // ID = 6
               {
                   // SECONDS function - get elapsed seconds
                   Time.Seconds();              
                   LDY #0 // already pushed the result
               }
               case SysCallType.Delay:         // ID = 7
               {
                   // DELAY function - delay in milliseconds
                   Time.DelayTOP();            // Uses ZP.TOP*
               }
               case SysCallType.Peek:          // ID = 8
               {
                   // PEEK function - read memory byte
                   // Input: ZP.TOP* contains address
                   // Output: ZP.TOP* contains byte value
                   
                   // Validate address is WORD or INT type
                   LDA ZP.TOPT
                   switch(A)
                   {
                       case BASICType.WORD:
                       case BASICType.BYTE:
                       {
                       }
                       case BASICType.INT:
                       {
                           // Check INT is positive (valid address)
                           LDA ZP.TOPH
                           if (MI)
                           {
                               Error.TypeMismatch(); BIT ZP.EmulatorPCL
                               States.SetFailure();
                               break;
                           }
                       }
                       default:
                       {
                           Error.TypeMismatch(); BIT ZP.EmulatorPCL
                           States.SetFailure();
                           break;
                       }
                   } // switch on type            
                   // Read byte from memory address
                   LDA [ZP.TOP]
                   STA ZP.TOPL
                   STZ ZP.TOPH
                   LDA #BASICType.BYTE
                   STA ZP.TOPT
               }
               case SysCallType.Poke:          // ID = 9
               {
                   // POKE function - write memory byte
                   // Input: ZP.NEXT* contains address, ZP.TOP* contains value
                   
                   // Validate address is WORD or INT type
                   LDA ZP.NEXTT
                   switch (A)
                   {
                       case BASICType.WORD:
                       case BASICType.BYTE:
                       {
                           // Validate value is 0..255 (any type)
                           LDA ZP.TOPH
                           if (NZ)
                           {
                               Error.TypeMismatch(); BIT ZP.EmulatorPCL
                               States.SetFailure();
                               break;
                           }
                       }
                       case BASICType.INT:
                       {
                           // Check INT is positive (valid address)
                           LDA ZP.NEXTH
                           if (MI)
                           {
                               Error.TypeMismatch(); BIT ZP.EmulatorPCL
                               States.SetFailure();
                               break;
                           }
                           
                           // Validate value is 0..255 (any type)
                           LDA ZP.TOPH
                           if (NZ)
                           {
                               Error.TypeMismatch(); BIT ZP.EmulatorPCL
                               States.SetFailure();
                               break;
                           }
                       }
                       default:
                       {
                           Error.TypeMismatch(); BIT ZP.EmulatorPCL
                           States.SetFailure();
                           break;
                       }
                   } // switch on type
                   // Write byte to memory address
                   LDA ZP.TOPL  // Get value to write
                   STA [ZP.NEXT]   // Write to address
               }
               default:
               {
                   TODO(); BIT ZP.EmulatorPCL
                   States.SetFailure();
                   break;
               }
           }
           
           // Handle return value (bit 2)
           TYA              // Restore full SYSCALL ID
           AND # 0b00000100 // Test return value bit
           if (NZ) 
           {
               Stacks.PushTop();  // Push return value from ZP.TOP*
           }
           
           States.SetSuccess();
           break;
       } // loop exit
#ifdef TRACE
       LDA #(executeSysCallTrace % 256) STA ZP.TraceMessageL LDA #(executeSysCallTrace / 256) STA ZP.TraceMessageH Trace.MethodExit();
#endif
   }
}

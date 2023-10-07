
; zero page usage:

; Hopper scratch uses $90...
; Hopper uses $A0...

; used by LCD firmware
  .ifdef VIALCD
LCDColumn           = $D5 ; global: current LCD column
LCDRow              = $D6 ; global: current LCD row
LEDState            = $D7 ; global: current state of the built-in LED (actually only need bit 0 but LEDWrite would need to be improved to not stomp the other bits)

ScratchByte         = $D8 ; local: working area (can only be used in code sections that don't make subroutine calls like lcdWait and lcdDataRead)
ScratchPrevAddress  = $D9 ; local: currently only used within LCDScroll
ScratchCurrAddress  = $DA ; local: currently only used within LCDScroll
ScratchActiveChar   = $DB ; local: currently only used within LCDScroll
ScratchPointer      = $DC
ScratchPointerL     = $DC ; local: low byte of data pointer currently only used within LCDString
ScratchPointerH     = $DD ; local: low byte of data pointer currently only used within LCDString

  .else
  .ifdef VIALED  
LEDState            = $C2 ; global: current state of the built-in LED (actually only need bit 0 but LEDWrite would need to be improved to not stomp the other bits)
  .endif
  .endif

; used by PS/2 keyboard firmware for the keyboard buffer
  .ifdef VIAKEYBOARD
KeyboardWritePointer = $D0 ; global
KeyboardReadPointer  = $D1 ; global
KeyboardFlags        = $D2 ; global (modifier key states)
  .endif

; 65C51 and M6850 ACAI serial input buffer
  .ifdef ACIASERIAL
SerialInWritePointer = $D3 ; global
SerialInReadPointer  = $D4 ; global
SerialBreakFlag      = $DE ; global
  .endif

; used by Timer firmware
  .ifdef VIATIMER
Ticks                = $E0 ; global
Ticks1               = $E1 ; global
Ticks2               = $E2 ; global
Ticks3               = $E3 ; global
Target               = $E4 ; only used as local within TimerDelay
Target1              = $E5 ; only used as local within TimerDelay
Target2              = $E6 ; only used as local within TimerDelay
Target3              = $E7 ; only used as local within TimerDelay
  .endif

; IO ports are on the zero page:

  .ifdef UNOSERIAL
; UNO emulator
UNODATA    = $EC
UNOSTATUS  = $ED
  .endif

; 65C51 ACAI

  .ifdef ACIASERIAL
  
  .ifdef ACIAMOTOROLLA
ACIACONTROL  = $EC ; 00 - write only
ACIASTATUS   = $EC ; 00 - read only
ACIADATA     = $ED ; 01
  
  .else
ACIADATA    = $EC ; 00
ACIASTATUS  = $ED ; 01
ACIACOMMAND = $EE ; 10
ACIACONTROL = $EF ; 11

  .endif
  .endif
 
; 65C22 VIA
PORTB = $F0
PORTA = $F1
DDRB  = $F2
DDRA  = $F3
T1CL  = $F4 ; Timer 1 counter low
T1CH  = $F5 ; Timer 1 counter high

ACR   = $FB ; Auxiliary Control Register
PCR   = $FC ; Peripheral Control Register
IFR   = $FD ; Interrupt Flag Register
IER   = $FE ; Interrupt Enable Register

; other RAM allocations
  .ifdef VIAKEYBOARD
KeyboardBuffer = $0200  ; 256-byte buffer 0200-02ff
  .endif
  .ifdef ACIASERIAL
SerialInBuffer = $0300  ; 256-byte buffer 0300-03ff
  .endif

#include "Common.h"

static const uint8_t A0 = (26u);
static const uint8_t A1 = (27u);
static const uint8_t A2 = (28u);
static const uint8_t A3 = (29u);

typedef Bool (*LibCallMethod)();

LibCallMethod libcallJumps[256];

enum LibCall {
    eWireBegin = 0x0000,
    eWireBeginTx = 0x0001,
    eWireEndTx = 0x0002,
    eWireWrite = 0x0003,
    eMCUPinMode = 0x0004,
    eMCUDigitalRead = 0x0005,
    eMCUDigitalWrite = 0x0006,
    eMCUAnalogRead = 0x0007,
    eMCUAnalogWrite = 0x0008,
    eMCUAnalogWriteResolution = 0x0009,
    eMCUAttachToPin = 0x000A,
    eGraphicsConfigureDisplay = 0x000B,
    eGraphicsConfigureSPI = 0x000C,
    eGraphicsConfigureSPIPort = 0x000D,
    eGraphicsConfigureReset = 0x000E,
    eGraphicsConfigureI2C = 0x000F,
    eGraphicsConfigureMatrix = 0x0010,
    eGraphicsBegin = 0x0011,
    eGraphicsEnd = 0x0012,
    eGraphicsInvertDisplay = 0x0013,
    eGraphicsFlipDisplay = 0x0014,
    eGraphicsClear = 0x0015,
    eGraphicsWidthGet = 0x0016,
    eGraphicsHeightGet = 0x0017,
    eGraphicsSetPixel = 0x0018,
    eGraphicsLine = 0x0019,
    eGraphicsHorizontalLine = 0x001A,
    eGraphicsVerticalLine = 0x001B,
    eGraphicsRectangle = 0x001C,
    eGraphicsFilledRectangle = 0x001D,
    eGraphicsCircle = 0x001E,
    eGraphicsFilledCircle = 0x001F,
    eGraphicsShow = 0x0020,
    eGraphicsDrawChar = 0x0021,
};

bool undefinedLib()
{
    printf("\nundefined libcall at 0x%04X", GetPC());
    SetError(0x0A, 10);
    return false;
}

bool mcuPinMode()
{
    Byte mode = Byte(VMPop());
    Byte pin  = Byte(VMPop());
    
    gpio_init(pin);
    switch (mode) 
    {
        case 0: // INPUT
            gpio_set_dir(pin, GPIO_IN);
            gpio_disable_pulls(pin);
            break;
        case 1: // OUTPUT
            gpio_set_dir(pin, GPIO_OUT);
            gpio_disable_pulls(pin);
            break;
        case 2: // INPUT_PULLUP
            gpio_set_dir(pin, GPIO_IN);
            gpio_pull_up(pin);
            break;
        case 3: // INPUT_PULLDOWN
            gpio_set_dir(pin, GPIO_IN);
            gpio_pull_down(pin);
            break;
        
    }
    return true;
}
Bool mcuDigitalWrite()
{
    Byte value = Byte(VMPop());
    Byte pin   = Byte(VMPop());
    gpio_put(pin, value ? 1 : 0);
    return true;
}

void LibCalls_PopulateJumpTable()
{
    for (UInt i = 0; i < 256; i++)
    {
        libcallJumps[i] = undefinedLib;
    }
    libcallJumps[LibCall::eMCUPinMode] = mcuPinMode;
    libcallJumps[LibCall::eMCUDigitalWrite] = mcuDigitalWrite;
    
    // TODO
}

Bool LibCall(Byte iLibCall)
{
    return libcallJumps[iLibCall]();
}


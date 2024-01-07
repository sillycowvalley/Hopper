#include "Common.h"

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
    printf("\nundefined libcall at 0x%04X", GetPC() - 2);
    SetError(0x0A, (13));
    return false;
}

// Background material and ideas sourced from:
//    Earle F. Philhower's arduino-pico/cores/rp2040/wiring_analog.cpp

static const Byte A0 = 26;
static const Byte A1 = 27;
static const Byte A2 = 28;
static const Byte A3 = 29;

static UInt32 analogScale = 255;
static UInt32 analogFrequency = 1000;
static UInt32 pwmInitialized = 0;
static Bool   scaleInitialized = false;
static Bool   adcInitialized   = false;
static UInt32 adcGPIOInit = 0;
static Byte   lastADCMux = 0;
static Byte   readBits = 10;
static UInt   analogWritePseudoScale = 1;
static UInt   analogWriteSlowScale = 1;

bool mcuAnalogWriteResolution()
{
    UInt resolution = VMPop();
    if ((resolution >= 2) && (resolution <= 16)) 
    {
        UInt32 range = ((1 << resolution) - 1);
        if (range != analogScale) 
        {
            if ((range >= 3) && (range <= 65535)) 
            {
                analogScale = range;
                pwmInitialized = 0;
                scaleInitialized = false;
            }
        }
    } 
    return true;
}
bool mcuAnalogWrite()
{
    UInt value = VMPop();
    UInt pin   = VMPop();
    if (pin <= 28)
    {
        adcGPIOInit &= ~(1 << pin); // clear pin
        if (!scaleInitialized) 
        {
            analogWritePseudoScale = 1;
            while (((clock_get_hz(clk_sys) / ((Float)analogScale * analogFrequency)) > 255.0) && (analogScale < 32678)) 
            {
                analogWritePseudoScale++;
                analogScale *= 2;
            }
            analogWriteSlowScale = 1;
            while (((clock_get_hz(clk_sys) / ((Float)analogScale * analogFrequency)) < 1.0) && (analogScale >= 6)) 
            {
                analogWriteSlowScale++;
                analogScale /= 2;
            }
            scaleInitialized = true;
        }
        if (!(pwmInitialized & (1 << pwm_gpio_to_slice_num(pin)))) 
        {
            pwm_config c = pwm_get_default_config();
            pwm_config_set_clkdiv(&c, clock_get_hz(clk_sys) / ((Float)analogScale * analogFrequency));
            pwm_config_set_wrap(&c, analogScale - 1);
            pwm_init(pwm_gpio_to_slice_num(pin), &c, true);
            pwmInitialized |= 1 << pwm_gpio_to_slice_num(pin);
        }

        value <<= analogWritePseudoScale;
        value >>= analogWriteSlowScale;

        if (value < 0) 
        {
            value = 0;
        } 
        else if ((UInt32)value > analogScale) 
        {
            value = analogScale;
        }

        gpio_set_function(pin, GPIO_FUNC_PWM);
        pwm_set_gpio_level(pin, value);
    }
    return true;
}
bool mcuAnalogRead()
{
    UInt pin   = VMPop();
    
    Byte maxPin = std::max(A0, A3);
    Byte minPin = std::min(A0, A3);
    
    UInt value = 0;
    if ((pin >= minPin) && (pin <= maxPin))
    {
        if (!adcInitialized)
        {
            adc_init();
            adcInitialized = true;
        }
        if (!(adcGPIOInit & (1 << pin))) 
        {
            adc_gpio_init(pin);
            adcGPIOInit |= 1 << pin;
        }
        if (lastADCMux != pin) 
        {
            adc_select_input(pin - minPin);
            lastADCMux = pin;
        }
        value = (readBits < 12) ? adc_read() >> (12 - readBits) : adc_read() << (readBits - 12);
    }
    
    VMPush(value, Type::eUInt);
    return true;
}


bool mcuPinMode()
{
    Byte mode = Byte(VMPop());
    Byte pin  = Byte(VMPop());
    
    if (pin == 32) // fake pin for Pico W LED
    {
        return true;
    }
    
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
    if (pin == 32) // fake pin for Pico W LED
    {
#ifdef PICO_CYW43_SUPPORTED    
        cyw43_arch_gpio_put(CYW43_WL_GPIO_LED_PIN, value ? 1 : 0);
#endif
    }
    else 
    {
        gpio_put(pin, value ? 1 : 0);
    }
    return true;
}

void LibCalls_PopulateJumpTable()
{
    for (UInt i = 0; i < 256; i++)
    {
        libcallJumps[i] = undefinedLib;
    }
    libcallJumps[LibCall::eMCUPinMode]      = mcuPinMode;
    libcallJumps[LibCall::eMCUDigitalWrite] = mcuDigitalWrite;
    
    libcallJumps[LibCall::eMCUAnalogWriteResolution] = mcuAnalogWriteResolution;
    libcallJumps[LibCall::eMCUAnalogWrite]           = mcuAnalogWrite;
    libcallJumps[LibCall::eMCUAnalogRead]            = mcuAnalogRead;
    
    // TODO
}

bool LibCall()
{
    //printf("\nLibCall: 0x%04X 0x%02X", GetPC()-1, codeMemoryBlock[pc]);
    return libcallJumps[codeMemoryBlock[pc++]]();
}


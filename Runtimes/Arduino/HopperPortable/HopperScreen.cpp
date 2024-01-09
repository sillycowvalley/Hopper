#include "HopperScreen.h"

/*

#include <Wire.h>

#include <SPI.h>
#ifdef RP2040
SPIClassRP2040* screenSPI;
#else
SPIClass* screenSPI;
#endif
SPISettings screenSPISettings;

Byte columns;
Byte rows;
Byte suspended;
Byte cursorX;
Byte cursorY;

//Display currentDisplay = Display::eNoDisplay;
uint pixelHeight = 0;
uint pixelWidth = 0;
uint visiblePixelHeight = 0;
uint visiblePixelWidth = 0;
bool spiConfigured = false;
bool i2cConfigured = false;
bool matrixConfigured = false;
bool resetPinConfigured = false;
bool portPinsConfigured = false;
bool isRGB444 = false;
Byte matrixClockPin;
Byte matrixDataPin;
Byte matrixIntensity;
Byte spiChipSelectPin;
Byte spiDataCommandPin;
Byte spiTxPin = 0;
Byte spiClkPin = 0;
Byte resetPin = 0;
Byte i2cAddress = 0;

Byte xFudge = 0;
Byte yFudge = 0;

UInt * frameBuffer;     // 4 bytes per pixel (for reading)
Byte * monoFrameBuffer; // 1 bit per pixel for monochrome (OLED, LED Matrix, etc)

#define CELLWIDTH   6
#define CELLHEIGHT 10

void HRGraphics_ConfigureDisplay(Display display, UInt width, UInt height)
{
    currentDisplay = display;
    visiblePixelWidth = pixelWidth  = width;
    visiblePixelHeight = pixelHeight = height;

    // Pico-LCD-1.14
    if ((display == Display::eST7789) && (visiblePixelWidth == 240) && (visiblePixelHeight == 135))
    {
        pixelWidth  = 320;
        pixelHeight = 240;
    }
}
void HRGraphics_ConfigureSPI(Byte chipSelectPin, Byte dataCommandPin)
{
    spiChipSelectPin = chipSelectPin;
    spiDataCommandPin = dataCommandPin;
    spiConfigured = true;
}
void HRGraphics_ConfigureSPIPort(Byte txPin, Byte clkPin)
{
    spiTxPin = txPin;
    spiClkPin = clkPin;
    portPinsConfigured = true;
}
void HRGraphics_ConfigureReset(Byte rst)
{
    resetPin = rst;
    resetPinConfigured = true;
}
void HRGraphics_ConfigureI2C(Byte address)
{
    i2cAddress = address;
    i2cConfigured = true;
}
void HRGraphics_ConfigureMatrix(Byte clockPin, Byte dataPin, Byte intensity)
{
    matrixClockPin = clockPin;
    matrixDataPin  = dataPin;
    matrixIntensity = intensity;
    matrixConfigured = true;
}

Byte fontMap[256];
UInt fontBuffer[CELLWIDTH*CELLHEIGHT];


#define SSD1306_CHARGEPUMP      0x8D
#define SSD1306_DISPLAYON       0xAF
#define SSD1306_DISPLAYOFF      0xAE
#define SSD1306_MEMORYMODE      0x20
#define SSD1306_COLUMNADDR      0x21
#define SSD1306_PAGEADDR        0x22

#define SSD1306_DEACTIVATE_SCROLL 0x2E

#define SSD1306_SETSTARTLINE 0x40

#define SSD1306_SETCONTRAST 0x81

#define SSD1306_COMSCANDEC      0xC8
#define SSD1306_COMSCANINC      0xC0
#define SSD1306_SETSEGMENTREMAP 0xA1
#define SSD1306_SEGREMAP        0xA0

#define SSD1306_NORMALDISPLAY   0xA6
#define SSD1306_INVERTDISPLAY   0xA7

#define SSD1306_DISPLAYALLON_RESUME 0xA4
#define SSD1306_SETMULTIPLEX 0xA8
#define SSD1306_SETDISPLAYOFFSET 0xD3
#define SSD1306_SETDISPLAYCLOCKDIV 0xD5
#define SSD1306_SETPRECHARGE 0xD9
#define SSD1306_SETCOMPINS 0xDA
#define SSD1306_SETVCOMDETECT 0xDB


#define SPI_DEFAULT_FREQ 24000000 // Default SPI data clock frequency

// Generic commands TFT commands

#define TFT_NOP     0x00
#define TFT_SWRST   0x01

#define TFT_SLPOUT 0x11     //  Sleep Out

#define TFT_INVOFF  0x20
#define TFT_INVON   0x21

#define TFT_CASET   0x2A
#define TFT_PASET   0x2B
#define TFT_RAMWR   0x2C
#define TFT_RAMRD   0x2E

#define TFT_MADCTL  0x36

#define MADCTL_MY  0x80  // Bottom to top
#define MADCTL_MX  0x40  // Right to left
#define MADCTL_MV  0x20  // Reverse Mode
#define MADCTL_BGR 0x08  // Blue-Green-Red pixel order
#define MADCTL_RGB 0x00


#define ST7796_RDDID 0x04      //  Read display identification information
#define ST7796_RDDST 0x09      //  Read Display Status

#define ST7796_SLPIN 0x10      //  Enter Sleep Mode

#define ST7796_PTLON 0x12      //  Partial Mode ON
#define ST7796_NORON 0x13      //  Normal Display Mode ON

#define ST7796_RDMODE 0x0A     //  Read Display Power Mode
#define ST7796_RDMADCTL 0x0B   //  Read Display MADCTL
#define ST7796_RDPIXFMT 0x0C   //  Read Display Pixel Format
#define ST7796_RDIMGFMT 0x0D   //  Read Display Image Format
#define ST7796_RDSELFDIAG 0x0F //  Read Display Self-Diagnostic Result

#define ST7796_GAMMASET 0x26   //  Gamma Set
#define ST7796_DISPOFF 0x28    //  Display OFF
#define ST7796_DISPON 0x29     //  Display ON

#define ST7796_PTLAR 0x30      //  Partial Area
#define ST7796_VSCRDEF 0x33    //  Vertical Scrolling Definition
#define ST7796_VSCRSADD 0x37   //  Vertical Scrolling Start Address
#define ST7796_PIXFMT 0x3A     //  COLMOD: Pixel Format Set

#define ST7796_FRMCTR1 0xB1
#define ST7796_INVCTR  0xB4    // Column inversion
#define ST7796_DFUNCTR 0xB6    // Display Function Control
#define ST7796_PWCTR1  0xC0
#define ST7796_PWCTR2  0xC1
#define ST7796_PWCTR3  0xC2

#define ST7796_VMCTR1  0xC5
#define ST7796_VMCTR2  0xC7

#define ST7796_GMCTRP1 0xE0
#define ST7796_GMCTRN1 0xE1



#define ST7735_SWRESET 0x01
#define ST7735_NORON   0x13

#define ST7735_INVOFF  0x20
#define ST7735_INVON   0x21
#define ST7735_DISPON  0x29

#define ST7735_COLMOD  0x3A


#define ST7735_FRMCTR1 0xB1
#define ST7735_FRMCTR2 0xB2
#define ST7735_FRMCTR3 0xB3
#define ST7735_INVCTR  0xB4

#define ST7735_PWCTR1  0xC0
#define ST7735_PWCTR2  0xC1
#define ST7735_PWCTR3  0xC2
#define ST7735_PWCTR4  0xC3
#define ST7735_PWCTR5  0xC4
#define ST7735_VMCTR1  0xC5

#define ST7735_GMCTRP1 0xE0
#define ST7735_GMCTRN1 0xE1

// CS, DC, MOSI CLK, RST
// Adafruit_ST7735 tft = Adafruit_ST7735(9, 8, 11, 10, 12);
// tft.initR(INITR_144GREENTAB);

// https://github.com/Bodmer/TFT_eSPI/blob/master/TFT_Drivers/ST7735_Init.h
static const uint8_t 
#ifndef LOLIND1MINI
PROGMEM
#endif
initcmdSPI_ST7735_144[] =
{
  //  (COMMAND_BYTE), n, data_bytes....
  TFT_SLPOUT    , 0x80,     // Sleep exit
  ST7735_FRMCTR1, 3      ,  //  3: Frame rate ctrl - normal mode, 3 args:
    0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
  ST7735_FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
    0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
  ST7735_FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
    0x01, 0x2C, 0x2D,       //     Dot inversion mode
    0x01, 0x2C, 0x2D,       //     Line inversion mode
  ST7735_INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
    0x07,                   //     No inversion
  ST7735_PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
    0xC5,                   //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
  ST7735_PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
    0x0A,                   //     Opamp current small
    0x00,                   //     Boost frequency
  ST7735_VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
    0x0E,
  
  // Pico-LCD-1.44:
  ST7735_INVOFF , 0      ,  // 13: Don't invert display, no args, no delay
  TFT_MADCTL      , 1, (   MADCTL_BGR),              // Memory Access Control

  ST7735_COLMOD , 1      ,  // 15: set color mode, 1 arg, no delay:
    0x05,
  ST7735_GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
  ST7735_GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
  ST7735_NORON  ,    0x80, //  3: Normal display on, no args, w/delay
  ST7735_DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
  0x00                        // End of list
};

static const uint8_t 
#ifndef LOLIND1MINI
PROGMEM
#endif
initcmdSPI_ST7735_096[] =
{
  //  (COMMAND_BYTE), n, data_bytes....
  TFT_SLPOUT    , 0x80,     // Sleep exit
  ST7735_FRMCTR1, 3      ,  //  3: Frame rate ctrl - normal mode, 3 args:
    0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
  ST7735_FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
    0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
  ST7735_FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
    0x01, 0x2C, 0x2D,       //     Dot inversion mode
    0x01, 0x2C, 0x2D,       //     Line inversion mode
  ST7735_INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
    0x07,                   //     No inversion
  ST7735_PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
    0xC5,                   //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
  ST7735_PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
    0x0A,                   //     Opamp current small
    0x00,                   //     Boost frequency
  ST7735_VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
    0x0E,
  
  // Pico-LCD-0.96:
  ST7735_INVON , 0      ,  // 13: Invert display, no args, no delay
  TFT_MADCTL      , 1, (  MADCTL_MY | MADCTL_MV | MADCTL_BGR),              // Memory Access Control : 

  ST7735_COLMOD , 1      ,  // 15: set color mode, 1 arg, no delay:
    0x05,
  ST7735_GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
  ST7735_GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
  ST7735_NORON  ,    0x80, //  3: Normal display on, no args, w/delay
  ST7735_DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
  0x00                        // End of list
};

static const uint8_t 
#ifndef LOLIND1MINI
PROGMEM
#endif
initcmdSPI_ST7789_114[] =
{
  //  (COMMAND_BYTE), n, data_bytes....
  TFT_SLPOUT    , 0x80,     // Sleep exit
  ST7735_FRMCTR1, 3      ,  //  3: Frame rate ctrl - normal mode, 3 args:
    0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
  ST7735_FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
    0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
  ST7735_FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
    0x01, 0x2C, 0x2D,       //     Dot inversion mode
    0x01, 0x2C, 0x2D,       //     Line inversion mode
  ST7735_INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
    0x07,                   //     No inversion
  ST7735_PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
    0xC5,                   //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
  ST7735_PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
    0x0A,                   //     Opamp current small
    0x00,                   //     Boost frequency
  ST7735_VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
    0x0E,
  
  // Pico-LCD-1.14:
  ST7735_INVON , 0      ,  // 13: Invert display, no args, no delay
  TFT_MADCTL      , 1, (   MADCTL_MX | MADCTL_MV | MADCTL_RGB),              // Memory Access Control : 

  ST7735_COLMOD , 1      ,  // 15: set color mode, 1 arg, no delay:
    0x05,
  ST7735_GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
  ST7735_GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
  ST7735_NORON  ,    0x80, //  3: Normal display on, no args, w/delay
  ST7735_DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
  0x00                        // End of list
};

// https://github.com/Bodmer/TFT_eSPI/blob/master/TFT_Drivers/ST7796_Init.h
static const uint8_t 
#ifndef LOLIND1MINI
PROGMEM
#endif
initcmdSPI_ST7796[] =
{
  //  (COMMAND_BYTE), n, data_bytes....
  TFT_SLPOUT   , 0x80,     // Sleep exit
  0xF0            , 1, 0xC3,
  0xF0            , 1, 0x96,
  TFT_MADCTL      , 1, (MADCTL_MX | MADCTL_MY | MADCTL_MV | MADCTL_BGR),            // Memory Access Control
  ST7796_PIXFMT   , 1, 0x55,
  ST7796_INVCTR   , 1, 0x01,  // Column inversion (1 dot)
  ST7796_DFUNCTR  , 3, 0x80, 0x02, 0x3B,
  0xE8            , 8, 0x40, 0x8A, 0x00, 0x00, 0x29, 0x19, 0xA5, 0x33,
  ST7796_PWCTR2   , 1, 0x06,
  ST7796_PWCTR3   , 1, 0xA7,
  ST7796_VMCTR1   , 1, 0x18,
  ST7796_GMCTRP1  , 14, 0xF0, 0x09, 0x0B, 0x06, 0x04, 0x15, 0x2F, 0x54, 0x42, 0x3C, 0x17, 0x14, 0x18, 0x1B, // Set Gamma
  ST7796_GMCTRN1  , 14, 0xE0, 0x09, 0x0B, 0x06, 0x04, 0x03, 0x2B, 0x43, 0x42, 0x3B, 0x16, 0x14, 0x17, 0x36, // Set Gamma
  0xF0            , 1, 0x3C,
  0xF0            , 1, 0x69,
  ST7796_DISPON   , 0x80,                // Display on
  0x00                        // End of list
};


#define ILI9341_GAMMASET 0x26 // Gamma Set
#define ILI9341_DISPON   0x29 // Display ON
#define ILI9341_VSCRSADD 0x37 // Vertical Scrolling Start Address
#define ILI9341_PIXFMT   0x3A // COLMOD: Pixel Format Set
#define ILI9341_FRMCTR1  0xB1 // Frame Rate Control
#define ILI9341_DFUNCTR  0xB6 // Display Function Control
#define ILI9341_PWCTR1   0xC0 // Power Control 1
#define ILI9341_PWCTR2   0xC1 // Power Control 2
#define ILI9341_VMCTR1   0xC5 // VCOM Control 1
#define ILI9341_VMCTR2   0xC7 // VCOM Control 2
#define ILI9341_GMCTRP1  0xE0 // Positive Gamma Correction
#define ILI9341_GMCTRN1  0xE1 // Negative Gamma Correction

// https://github.com/Bodmer/TFT_eSPI/blob/master/TFT_Drivers/ILI9341_Init.h
static const uint8_t 
#ifndef LOLIND1MINI
PROGMEM
#endif
initcmdSPI_ILI9341[] =
{
  0xEF, 3, 0x03, 0x80, 0x02,
  0xCF, 3, 0x00, 0xC1, 0x30,
  0xED, 4, 0x64, 0x03, 0x12, 0x81,
  0xE8, 3, 0x85, 0x00, 0x78,
  0xCB, 5, 0x39, 0x2C, 0x00, 0x34, 0x02,
  0xF7, 1, 0x20,
  0xEA, 2, 0x00, 0x00,
  ILI9341_PWCTR1  , 1, 0x23,             // Power control VRH[5:0]
  ILI9341_PWCTR2  , 1, 0x10,             // Power control SAP[2:0];BT[3:0]
  ILI9341_VMCTR1  , 2, 0x3e, 0x28,       // VCM control
  ILI9341_VMCTR2  , 1, 0x86,             // VCM control2
  TFT_MADCTL      , 1, (MADCTL_MX | MADCTL_MY | MADCTL_MV | MADCTL_BGR), //0x48,             // Memory Access Control
  ILI9341_PIXFMT  , 1, 0x55,
  ILI9341_FRMCTR1 , 2, 0x00, 0x13,       // 0x18 79Hz, 0x1B default 70Hz, 0x13 100Hz
  ILI9341_DFUNCTR , 3, 0x08, 0x82, 0x27, // Display Function Control
  0xF2, 1, 0x00,                         // 3Gamma Function Disable
  ILI9341_GAMMASET , 1, 0x01,             // Gamma curve selected
  ILI9341_GMCTRP1 , 15, 0x0F, 0x31, 0x2B, 0x0C, 0x0E, 0x08, 0x4E, 0xF1, 0x37, 0x07, 0x10, 0x03, 0x0E, 0x09, 0x00, // Set Gamma
  ILI9341_GMCTRN1 , 15, 0x00, 0x0E, 0x14, 0x03, 0x11, 0x07, 0x31, 0xC1, 0x48, 0x08, 0x0F, 0x0C, 0x31, 0x36, 0x0F, // Set Gamma
  TFT_SLPOUT  , 0x80,                // Exit Sleep
  ILI9341_DISPON  , 0x80,                // Display on
  0x00                                   // End of list
};

static const UInt 
#ifndef LOLIND1MINI
PROGMEM
#endif
tinyFont[][9] =
{
    // ' ' 0x20
    { 0x20, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '!' 0x21
    { 0x21, 0xFB7F, 0xFB7F, 0xFE7F, 0xFF7F, 0xFFFF, 0xFB5F, 0xFFFF, 0xFFFF} ,
    // '"' 0x22
    { 0x22, 0xF377, 0xF3B7, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '#' 0x23
    { 0x23, 0xFFFF, 0xF6D7, 0x3000, 0xF5BB, 0x3000, 0xF77D, 0xFFFF, 0xFFFF} ,
    // '$' 0x24
    { 0x24, 0xFF6F, 0xC103, 0x896F, 0xE61A, 0xFBB1, 0x7006, 0xF7FF, 0xFFFF} ,
    // '%' 0x25
    { 0x25, 0x50D6, 0x4098, 0xFF5F, 0xF8BF, 0xE660, 0x6E50, 0xFFFF, 0xFFFF} ,
    // '&' 0x26
    { 0x26, 0xE10D, 0xB79B, 0xE28F, 0x7882, 0x3E53, 0xA013, 0xFFFF, 0xFFFF} ,
    // ''' 0x27
    { 0x27, 0xFB3F, 0xFB7F, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '(' 0x28
    { 0x28, 0xFF8D, 0xFD6F, 0xF6DF, 0xF3FF, 0xF3FF, 0xF5DF, 0xFD6F, 0xFF7D} ,
    // ')' 0x29
    { 0x29, 0xF7EF, 0xFC5F, 0xFF4E, 0xFF7B, 0xFF7B, 0xFF4F, 0xFD6F, 0xF6EF} ,
    // '*' 0x2A
    { 0x2A, 0xFF7F, 0xD748, 0xFF7F, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '+' 0x2B
    { 0x2B, 0xFFFF, 0xFB7F, 0xFB7F, 0x3000, 0xFB7F, 0xFB7F, 0xFFFF, 0xFFFF} ,
    // ',' 0x2C
    { 0x2C, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFC2F, 0xF07F, 0xFFFF} ,
    // '-' 0x2D
    { 0x2D, 0xFFFF, 0xFFFF, 0xFFFF, 0xF007, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '.' 0x2E
    { 0x2E, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xF95F, 0xFFFF, 0xFFFF} ,
    // '/' 0x2F
    { 0x2F, 0xFFE5, 0xFF8C, 0xFF4F, 0xFC8F, 0xF5EF, 0xE6FF, 0x9AFF, 0xFFFF} ,
    // '0' 0x30
    { 0x30, 0xFFFF, 0xE207, 0x7890, 0x7926, 0x74D1, 0xD109, 0xFFFF, 0xFFFF} ,
    // '1' 0x31
    { 0x31, 0xFFFF, 0x903F, 0xFF3F, 0xFF3F, 0xFF3F, 0xB000, 0xFFFF, 0xFFFF} ,
    // '2' 0x32
    { 0x32, 0xFFFF, 0xE20A, 0xBBE4, 0xFF8C, 0xE6CF, 0x7000, 0xFFFF, 0xFFFF} ,
    // '3' 0x33
    { 0x33, 0xFFFF, 0xB00A, 0xFFA7, 0xF30B, 0xFFE3, 0x7019, 0xFFFF, 0xFFFF} ,
    // '4' 0x34
    { 0x34, 0xFFFF, 0xFE27, 0xF597, 0x8BB7, 0x3000, 0xFFB7, 0xFFFF, 0xFFFF} ,
    // '5' 0x35
    { 0x35, 0xFFFF, 0xB007, 0xB7FF, 0xB009, 0xFFD3, 0xB01B, 0xFFFF, 0xFFFF} ,
    // '6' 0x36
    { 0x36, 0xFFFF, 0xF603, 0xA7FF, 0x7505, 0x85F1, 0xD107, 0xFFFF, 0xFFFF} ,
    // '7' 0x37
    { 0x37, 0xFFFF, 0x7000, 0xFFD5, 0xFF5D, 0xFC7F, 0xF4EF, 0xFFFF, 0xFFFF} ,
    // '8' 0x38
    { 0x38, 0xFFFF, 0xD107, 0xB7D5, 0xF20A, 0x87D2, 0xC006, 0xFFFF, 0xFFFF} ,
    // '9' 0x39
    { 0x39, 0xFFFF, 0xC10A, 0x7CC3, 0xA027, 0xFFB4, 0xB03D, 0xFFFF, 0xFFFF} ,
    // ':' 0x3A
    { 0x3A, 0xFFFF, 0xFFFF, 0xFB5F, 0xFFFF, 0xFFFF, 0xFB5F, 0xFFFF, 0xFFFF} ,
    // ';' 0x3B
    { 0x3B, 0xFFFF, 0xFFFF, 0xF95F, 0xFFFF, 0xFFFF, 0xFC2F, 0xF07F, 0xFFFF} ,
    // '<' 0x3C
    { 0x3C, 0xFFFF, 0xFFFF, 0xFF8B, 0xE5BF, 0xE5BF, 0xFF8B, 0xFFFF, 0xFFFF} ,
    // '=' 0x3D
    { 0x3D, 0xFFFF, 0xFFFF, 0x7000, 0xFFFF, 0x7000, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '>' 0x3E
    { 0x3E, 0xFFFF, 0xFFFF, 0xE4DF, 0xFE5A, 0xFE5A, 0xE4DF, 0xFFFF, 0xFFFF} ,
    // '?' 0x3F
    { 0x3F, 0xF35E, 0xFF78, 0xFFA7, 0xF70B, 0xFFFF, 0xF79F, 0xFFFF, 0xFFFF} ,
    // '@' 0x40
    { 0x40, 0xF605, 0xB5F6, 0x5A07, 0x6497, 0x736A, 0x5322, 0x5CFF, 0xC00C} ,
    // 'A' 0x41
    { 0x41, 0xFFFF, 0xF96F, 0xF79C, 0xE7E6, 0x9001, 0x5FF7, 0xFFFF, 0xFFFF} ,
    // 'B' 0x42
    { 0x42, 0xFFFF, 0x7008, 0x7BD4, 0x7009, 0x7BF1, 0x7007, 0xFFFF, 0xFFFF} ,
    // 'C' 0x43
    { 0x43, 0xFFFF, 0xE300, 0x87FF, 0x7BFF, 0x77FF, 0xE300, 0xFFFF, 0xFFFF} ,
    // 'D' 0x44
    { 0x44, 0xFFFF, 0x7009, 0x7BD1, 0x7BF3, 0x7BD2, 0x701A, 0xFFFF, 0xFFFF} ,
    // 'E' 0x45
    { 0x45, 0xFFFF, 0xB003, 0xB7FF, 0xB003, 0xB7FF, 0xB003, 0xFFFF, 0xFFFF} ,
    // 'F' 0x46
    { 0x46, 0xFFFF, 0xB003, 0xB7FF, 0xB007, 0xB7FF, 0xB7FF, 0xFFFF, 0xFFFF} ,
    // 'G' 0x47
    { 0x47, 0xFFFF, 0xE300, 0x68FF, 0x3F30, 0x59F3, 0xD301, 0xFFFF, 0xFFFF} ,
    // 'H' 0x48
    { 0x48, 0xFFFF, 0x7BF3, 0x7BF3, 0x7000, 0x7BF3, 0x7BF3, 0xFFFF, 0xFFFF} ,
    // 'I' 0x49
    { 0x49, 0xFFFF, 0xB003, 0xFB7F, 0xFB7F, 0xFB7F, 0xB003, 0xFFFF, 0xFFFF} ,
    // 'J' 0x4A
    { 0x4A, 0xFFFF, 0xB007, 0xFFB7, 0xFFB7, 0xFFA7, 0xB01D, 0xFFFF, 0xFFFF} ,
    // 'K' 0x4B
    { 0x4B, 0xFFFF, 0x7BC5, 0x7B5E, 0x73CF, 0x7A4F, 0x7BC6, 0xFFFF, 0xFFFF} ,
    // 'L' 0x4C
    { 0x4C, 0xFFFF, 0xF3FF, 0xF3FF, 0xF3FF, 0xF3FF, 0xF000, 0xFFFF, 0xFFFF} ,
    // 'M' 0x4D
    { 0x4D, 0xFFFF, 0x79E0, 0x68A7, 0x3B97, 0x3FF7, 0x3FF7, 0xFFFF, 0xFFFF} ,
    // 'N' 0x4E
    { 0x4E, 0xFFFF, 0x77F3, 0x77F3, 0x7BA3, 0x7BB3, 0x7BC2, 0xFFFF, 0xFFFF} ,
    // 'O' 0x4F
    { 0x4F, 0xFFFF, 0xD107, 0x5AE3, 0x3FF7, 0x5CE3, 0xC109, 0xFFFF, 0xFFFF} ,
    // 'P' 0x50
    { 0x50, 0xFFFF, 0x7006, 0x7BF1, 0x7008, 0x7BFF, 0x7BFF, 0xFFFF, 0xFFFF} ,
    // 'Q' 0x51
    { 0x51, 0xFFFF, 0xD106, 0x5AE3, 0x3FF7, 0x5AE3, 0xC107, 0xFF20, 0xFFFF} ,
    // 'R' 0x52
    { 0x52, 0xFFFF, 0xB009, 0xB7D3, 0xB00B, 0xB786, 0xB7F2, 0xFFFF, 0xFFFF} ,
    // 'S' 0x53
    { 0x53, 0xFFFF, 0xC103, 0x88FF, 0xE73B, 0xFFE1, 0x7007, 0xFFFF, 0xFFFF} ,
    // 'T' 0x54
    { 0x54, 0xFFFF, 0x3000, 0xFB7F, 0xFB7F, 0xFB7F, 0xFB7F, 0xFFFF, 0xFFFF} ,
    // 'U' 0x55
    { 0x55, 0xFFFF, 0x7BF3, 0x7BF3, 0x7BF3, 0x7AE1, 0xC007, 0xFFFF, 0xFFFF} ,
    // 'V' 0x56
    { 0x56, 0xFFFF, 0x3FF6, 0x8AF3, 0xD5D6, 0xF38C, 0xF81F, 0xFFFF, 0xFFFF} ,
    // 'W' 0x57
    { 0x57, 0xFFFF, 0x3FF7, 0x3FF7, 0x5D67, 0x7997, 0x77D1, 0xFFFF, 0xFFFF} ,
    // 'X' 0x58
    { 0x58, 0xFFFF, 0x7AE4, 0xF35B, 0xF92F, 0xE35A, 0x6BE2, 0xFFFF, 0xFFFF} ,
    // 'Y' 0x59
    { 0x59, 0xFFFF, 0x5DF5, 0xD4B7, 0xF83F, 0xFB7F, 0xFB7F, 0xFFFF, 0xFFFF} ,
    // 'Z' 0x5A
    { 0x5A, 0xFFFF, 0x7002, 0xFFCC, 0xFE9F, 0xF8FF, 0x9000, 0xFFFF, 0xFFFF} ,
    // '[' 0x5B
    { 0x5B, 0xF30B, 0xF3FF, 0xF3FF, 0xF3FF, 0xF3FF, 0xF3FF, 0xF3FF, 0xF30B} ,
    // '\' 0x5C
    { 0x5C, 0xC8FF, 0xF4FF, 0xF9BF, 0xFE5F, 0xFF6E, 0xFFB9, 0xFFF5, 0xFFFF} ,
    // ']' 0x5D
    { 0x5D, 0xF00F, 0xFF3F, 0xFF3F, 0xFF3F, 0xFF3F, 0xFF3F, 0xFF3F, 0xF00F} ,
    // '^' 0x5E
    { 0x5E, 0xFFFF, 0xF94F, 0xF69B, 0xB9E4, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF} ,
    // '_' 0x5F
    { 0x5F, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0x0000} ,
    // 'a' 0x61
    { 0x61, 0xFFFF, 0xFFFF, 0xB008, 0xD103, 0x79D3, 0xB025, 0xFFFF, 0xFFFF} ,
    // 'b' 0x62
    { 0x62, 0x7BFF, 0x7BFF, 0x7306, 0x77F2, 0x7BE2, 0x9009, 0xFFFF, 0xFFFF} ,
    // 'c' 0x63
    { 0x63, 0xFFFF, 0xFFFF, 0xF403, 0xB6FF, 0xB7FF, 0xE303, 0xFFFF, 0xFFFF} ,
    // 'd' 0x64
    { 0x64, 0xFFF3, 0xFFF3, 0xE203, 0x79F3, 0x7AC3, 0xC036, 0xFFFF, 0xFFFF} ,
    // 'e' 0x65
    { 0x65, 0xFFFF, 0xFFFF, 0xE207, 0x7000, 0x79FF, 0xD103, 0xFFFF, 0xFFFF} ,
    // 'f' 0x66
    { 0x66, 0xFD10, 0xF7AF, 0x3003, 0xF7BF, 0xF7BF, 0xF7BF, 0xFFFF, 0xFFFF} ,
    // 'g' 0x67
    { 0x67, 0xFFFF, 0xFFFF, 0xE100, 0xB9D3, 0xC009, 0x9002, 0x7AF2, 0xA006} ,
    // 'h' 0x68
    { 0x68, 0x7BFF, 0x7BFF, 0x7306, 0x75F3, 0x7BF3, 0x7BF3, 0xFFFF, 0xFFFF} ,
    // 'i' 0x69
    { 0x69, 0xFB5F, 0xFFFF, 0xB03F, 0xFF3F, 0xFF3F, 0xB003, 0xFFFF, 0xFFFF} ,
    // 'j' 0x6A
    { 0x6A, 0xFF59, 0xFFFF, 0xB00B, 0xFF7B, 0xFF7B, 0xFF7B, 0xFF6B, 0x703F} ,
    // 'k' 0x6B
    { 0x6B, 0xB7FF, 0xB7FF, 0xB795, 0xB26F, 0xB64E, 0xB7C4, 0xFFFF, 0xFFFF} ,
    // 'l' 0x6C
    { 0x6C, 0xB03F, 0xFF3F, 0xFF3F, 0xFF3F, 0xFF3F, 0xB003, 0xFFFF, 0xFFFF} ,
    // 'm' 0x6D
    { 0x6D, 0xFFFF, 0xFFFF, 0x3230, 0x3B27, 0x3F37, 0x3F37, 0xFFFF, 0xFFFF} ,
    // 'n' 0x6E
    { 0x6E, 0xFFFF, 0xFFFF, 0x7306, 0x75F3, 0x7BF3, 0x7BF3, 0xFFFF, 0xFFFF} ,
    // 'o' 0x6F
    { 0x6F, 0xFFFF, 0xFFFF, 0xD107, 0x7CE2, 0x7CE2, 0xC109, 0xFFFF, 0xFFFF} ,
    // 'p' 0x70
    { 0x70, 0xFFFF, 0xFFFF, 0x7306, 0x77F2, 0x7BE2, 0x7009, 0x7BFF, 0x7BFF} ,
    // 'q' 0x71
    { 0x71, 0xFFFF, 0xFFFF, 0xE203, 0x79F3, 0x7AC3, 0xC023, 0xFFF3, 0xFFF3} ,
    // 'r' 0x72
    { 0x72, 0xFFFF, 0xFFFF, 0xB404, 0xB2E2, 0xB7FF, 0xB7FF, 0xFFFF, 0xFFFF} ,
    // 's' 0x73
    { 0x73, 0xFFFF, 0xFFFF, 0xE207, 0xD3AF, 0xFE94, 0xB008, 0xFFFF, 0xFFFF} ,
    // 't' 0x74
    { 0x74, 0xFFFF, 0xF4FF, 0x3003, 0xF3FF, 0xF3EF, 0xFA03, 0xFFFF, 0xFFFF} ,
    // 'u' 0x75
    { 0x75, 0xFFFF, 0xFFFF, 0x7BF3, 0x7BF3, 0x99C3, 0xD015, 0xFFFF, 0xFFFF} ,
    // 'v' 0x76
    { 0x76, 0xFFFF, 0xFFFF, 0x6CF5, 0xD7E6, 0xF69C, 0xF97F, 0xFFFF, 0xFFFF} ,
    // 'w' 0x77
    { 0x77, 0xFFFF, 0xFFFF, 0x5FF7, 0x5D67, 0x7A96, 0xA5C4, 0xFFFF, 0xFFFF} ,
    // 'x' 0x78
    { 0x78, 0xFFFF, 0xFFFF, 0xB7E4, 0xF75E, 0xF77D, 0xA7E3, 0xFFFF, 0xFFFF} ,
    // 'y' 0x79
    { 0x79, 0xFFFF, 0xFFFF, 0x7CF4, 0xD6D7, 0xF68D, 0xFA5F, 0xF89F, 0x33FF} ,
    // 'z' 0x7A
    { 0x7A, 0xFFFF, 0xFFFF, 0xB006, 0xFF8F, 0xFCEF, 0xA003, 0xFFFF, 0xFFFF} ,
    // '{' 0x7B
    { 0x7B, 0xFE17, 0xFB6F, 0xFA9F, 0x70DF, 0xFAAF, 0xFB7F, 0xFB6F, 0xFE27} ,
    // '|' 0x7C
    { 0x7C, 0xFB7F, 0xFB7F, 0xFB7F, 0xFB7F, 0xFB7F, 0xFB7F, 0xFB7F, 0xFB7F} ,
    // '} ,' 0x7D
    { 0x7D, 0xB09F, 0xFE3F, 0xFF2F, 0xFF53, 0xFF3F, 0xFF3F, 0xFE3F, 0xB0AF} ,
    // '~' 0x7E
    { 0x7E, 0xFFFF, 0xFFFF, 0xFFFF, 0xA096, 0x3E22, 0xFFFF, 0xFFFF, 0xFFFF}
};

static const uint8_t toneToShade[] = { 0, 37, 60, 81, 99, 116, 133, 148, 163, 177, 191, 204, 217, 230, 243, 255 };

UInt blendPixelColor(Byte tone, UInt foreColor, UInt backColor)
{
    // tone: 0..15
    Byte shade = toneToShade[tone];
    Byte shade255 = 255 - shade;
    // shade: 0..255

    Byte rForeground = (foreColor >> 8);
    Byte gForeground = ((foreColor >> 4) & (0x0F));
    Byte bForeground = (foreColor & 0x0F);

    Byte rBackground = (backColor >> 8);
    Byte gBackground = ((backColor >> 4) & (0x0F));
    Byte bBackground = (backColor & 0x0F);

    UInt rBlended = ((shade255 * rForeground) + (shade * rBackground)) >> 8;
    UInt gBlended = ((shade255 * gForeground) + (shade * gBackground)) >> 8;
    UInt bBlended = ((shade255 * bForeground) + (shade * bBackground)) >> 8;

    UInt c565 = (UInt)((rBlended << 12) + (gBlended << 7) + (bBlended << 1));
    if (rBlended & 0x01 != 0)
    {
        c565 |= 0x0800;
    }
    if (gBlended & 0x01 != 0)
    {
        c565 |= 0x0060;
    }
    if (bBlended & 0x01 != 0)
    {
        c565 |= 0x0001;
    }
    return c565;
}

UInt convertToRGB565(UInt rgb444)
{
    Byte rColor = (rgb444 >> 8);
    Byte gColor = ((rgb444 >> 4) & (0x0F));
    Byte bColor = (rgb444 & 0x0F);
    UInt c565 = (UInt)((rColor << 12) + (gColor << 7) + (bColor << 1));
    if (rColor & 0x01 != 0)
    {
        c565 |= 0x0800;
    }
    if (gColor & 0x01 != 0)
    {
        c565 |= 0x0060;
    }
    if (bColor & 0x01 != 0)
    {
        c565 |= 0x0001;
    }
    return c565;
}

// https://www.eevblog.com/forum/projects/5x7-or-7x9-dot-matrix-font-files-in-cc/

static const Byte 
#ifndef LOLIND1MINI
PROGMEM
#endif
monoFont[][5] =
{
  //  Hitachi HD44780A00
  { 0x00, 0x00, 0x00, 0x00, 0x00 },  // 20  32
	{ 0x00, 0x00, 0x4F, 0x00, 0x00 },  // 21  33  !
	{ 0x00, 0x07, 0x00, 0x07, 0x00 },  // 22  34  "
	{ 0x14, 0x7F, 0x14, 0x7F, 0x14 },  // 23  35  #
	{ 0x24, 0x2A, 0x7F, 0x2A, 0x12 },  // 24  36  $
	{ 0x23, 0x13, 0x08, 0x64, 0x62 },  // 25  37  %
	{ 0x36, 0x49, 0x55, 0x22, 0x50 },  // 26  38  &
	{ 0x00, 0x05, 0x03, 0x00, 0x00 },  // 27  39  '
	{ 0x00, 0x1C, 0x22, 0x41, 0x00 },  // 28  40  (
	{ 0x00, 0x41, 0x22, 0x1C, 0x00 },  // 29  41  )
	{ 0x14, 0x08, 0x3E, 0x08, 0x14 },  // 2A  42  *
	{ 0x08, 0x08, 0x3E, 0x08, 0x08 },  // 2B  43  +
	{ 0x00, 0x50, 0x30, 0x00, 0x00 },  // 2C  44  ,
	{ 0x08, 0x08, 0x08, 0x08, 0x08 },  // 2D  45  -
	{ 0x00, 0x60, 0x60, 0x00, 0x00 },  // 2E  46  .
	{ 0x20, 0x10, 0x08, 0x04, 0x02 },  // 2F  47  /
	{ 0x3E, 0x51, 0x49, 0x45, 0x3E },  // 30  48  0
	{ 0x00, 0x42, 0x7F, 0x40, 0x00 },  // 31  49  1
	{ 0x42, 0x61, 0x51, 0x49, 0x46 },  // 32  50  2
	{ 0x21, 0x41, 0x45, 0x4B, 0x31 },  // 33  51  3
	{ 0x18, 0x14, 0x12, 0x7F, 0x10 },  // 34  52  4
	{ 0x27, 0x45, 0x45, 0x45, 0x39 },  // 35  53  5
	{ 0x3C, 0x4A, 0x49, 0x49, 0x30 },  // 36  54  6
	{ 0x03, 0x01, 0x71, 0x09, 0x07 },  // 37  55  7
	{ 0x36, 0x49, 0x49, 0x49, 0x36 },  // 38  56  8
	{ 0x06, 0x49, 0x49, 0x29, 0x1E },  // 39  57  9
	{ 0x00, 0x36, 0x36, 0x00, 0x00 },  // 3A  58  :
	{ 0x00, 0x56, 0x36, 0x00, 0x00 },  // 3B  59  ;
	{ 0x08, 0x14, 0x22, 0x41, 0x00 },  // 3C  60  <
	{ 0x14, 0x14, 0x14, 0x14, 0x14 },  // 3D  61  =
	{ 0x00, 0x41, 0x22, 0x14, 0x08 },  // 3E  62  >
	{ 0x02, 0x01, 0x51, 0x09, 0x06 },  // 3F  63  ?
	{ 0x32, 0x49, 0x79, 0x41, 0x3E },  // 40  64  @
	{ 0x7E, 0x11, 0x11, 0x11, 0x7E },  // 41  65  A
	{ 0x7F, 0x49, 0x49, 0x49, 0x36 },  // 42  66  B
	{ 0x3E, 0x41, 0x41, 0x41, 0x22 },  // 43  67  C
	{ 0x7F, 0x41, 0x41, 0x22, 0x1C },  // 44  68  D
	{ 0x7F, 0x49, 0x49, 0x49, 0x41 },  // 45  69  E
	{ 0x7F, 0x09, 0x09, 0x09, 0x01 },  // 46  70  F
	{ 0x3E, 0x41, 0x49, 0x49, 0x7A },  // 47  71  G
	{ 0x7F, 0x08, 0x08, 0x08, 0x7F },  // 48  72  H
	{ 0x00, 0x41, 0x7F, 0x41, 0x00 },  // 49  73  I
	{ 0x20, 0x40, 0x41, 0x3F, 0x01 },  // 4A  74  J
	{ 0x7F, 0x08, 0x14, 0x22, 0x41 },  // 4B  75  K
	{ 0x7F, 0x40, 0x40, 0x40, 0x40 },  // 4C  76  L
	{ 0x7F, 0x02, 0x0C, 0x02, 0x7F },  // 4D  77  M
	{ 0x7F, 0x04, 0x08, 0x10, 0x7F },  // 4E  78  N
	{ 0x3E, 0x41, 0x41, 0x41, 0x3E },  // 4F  79  O
	{ 0x7F, 0x09, 0x09, 0x09, 0x06 },  // 50  80  P
	{ 0x3E, 0x41, 0x51, 0x21, 0x5E },  // 51  81  Q
	{ 0x7F, 0x09, 0x19, 0x29, 0x46 },  // 52  82  R
	{ 0x46, 0x49, 0x49, 0x49, 0x31 },  // 53  83  S
	{ 0x01, 0x01, 0x7F, 0x01, 0x01 },  // 54  84  T
	{ 0x3F, 0x40, 0x40, 0x40, 0x3F },  // 55  85  U
	{ 0x1F, 0x20, 0x40, 0x20, 0x1F },  // 56  86  V
	{ 0x3F, 0x40, 0x38, 0x40, 0x3F },  // 57  87  W
	{ 0x63, 0x14, 0x08, 0x14, 0x63 },  // 58  88  X
	{ 0x07, 0x08, 0x70, 0x08, 0x07 },  // 59  89  Y
	{ 0x61, 0x51, 0x49, 0x45, 0x43 },  // 5A  90  Z
	{ 0x7F, 0x41, 0x41, 0x00, 0x00 },  // 5B  91  [
	{ 0x15, 0x16, 0x7C, 0x16, 0x15 },  // 5C  92  '\'
	{ 0x00, 0x41, 0x41, 0x7F, 0x00 },  // 5D  93  ]
	{ 0x04, 0x02, 0x01, 0x02, 0x04 },  // 5E  94  ^
	{ 0x40, 0x40, 0x40, 0x40, 0x40 },  // 5F  95  _
	{ 0x00, 0x01, 0x02, 0x04, 0x00 },  // 60  96  `
	{ 0x20, 0x54, 0x54, 0x54, 0x78 },  // 61  97  a
	{ 0x7F, 0x48, 0x44, 0x44, 0x38 },  // 62  98  b
	{ 0x38, 0x44, 0x44, 0x44, 0x20 },  // 63  99  c
	{ 0x38, 0x44, 0x44, 0x48, 0x7F },  // 64 100  d
	{ 0x38, 0x54, 0x54, 0x54, 0x18 },  // 65 101  e
	{ 0x08, 0x7E, 0x09, 0x01, 0x02 },  // 66 102  f
	{ 0x0C, 0x52, 0x52, 0x52, 0x3E },  // 67 103  g
	{ 0x7F, 0x08, 0x04, 0x04, 0x78 },  // 68 104  h
	{ 0x00, 0x44, 0x7D, 0x40, 0x00 },  // 69 105  i
	{ 0x20, 0x40, 0x44, 0x3D, 0x00 },  // 6A 106  j
	{ 0x7F, 0x10, 0x28, 0x44, 0x00 },  // 6B 107  k
	{ 0x00, 0x41, 0x7F, 0x40, 0x00 },  // 6C 108  l
	{ 0x7C, 0x04, 0x18, 0x04, 0x78 },  // 6D 109  m
	{ 0x7C, 0x08, 0x04, 0x04, 0x78 },  // 6E 110  n
	{ 0x38, 0x44, 0x44, 0x44, 0x38 },  // 6F 111  o
	{ 0x7C, 0x14, 0x14, 0x14, 0x08 },  // 70 112  p
	{ 0x08, 0x14, 0x14, 0x18, 0x7C },  // 71 113  q
	{ 0x7C, 0x08, 0x04, 0x04, 0x08 },  // 72 114  r
	{ 0x48, 0x54, 0x54, 0x54, 0x20 },  // 73 115  s
	{ 0x04, 0x3F, 0x44, 0x40, 0x20 },  // 74 116  t
	{ 0x3C, 0x40, 0x40, 0x20, 0x7C },  // 75 117  u
	{ 0x1C, 0x20, 0x40, 0x20, 0x1C },  // 76 118  v
	{ 0x3C, 0x40, 0x38, 0x40, 0x3C },  // 77 119  w
	{ 0x44, 0x28, 0x10, 0x28, 0x44 },  // 78 120  x
	{ 0x0C, 0x50, 0x50, 0x50, 0x3C },  // 79 121  y
	{ 0x44, 0x64, 0x54, 0x4C, 0x44 },  // 7A 122  z
	{ 0x00, 0x08, 0x36, 0x41, 0x00 },  // 7B 123  {
	{ 0x00, 0x00, 0x7F, 0x00, 0x00 },  // 7C 124  |
	{ 0x00, 0x41, 0x36, 0x08, 0x00 },  // 7D 125  }
	{ 0x08, 0x08, 0x2A, 0x1C, 0x08 },  // 7E 126  ~
	{ 0x08, 0x1C, 0x2A, 0x08, 0x08 }   // 7F 127
};

Byte HRScreen_Columns_Get()
{
    return columns;
}
Byte HRScreen_Rows_Get()
{
    return rows;
}

UInt HRGraphics_Width_Get()  
{
    return visiblePixelWidth;  
}
UInt HRGraphics_Height_Get() 
{ 
    return visiblePixelHeight; 
}

void sendCommandI2C(Byte command)
{
  Wire.beginTransmission(i2cAddress);
  Wire.write(0x80);
  Wire.write(command);
  Wire.endTransmission();
}
void initScreenSPI(uint32_t freq)
{
    if (portPinsConfigured) // by default, both are zero
    {
        screenSPI->setTX(spiTxPin);
        screenSPI->setSCK(spiClkPin);
    }
    pinMode(spiChipSelectPin, OUTPUT);
    digitalWrite(spiChipSelectPin, HIGH); // Deselect
    pinMode(spiDataCommandPin, OUTPUT);
    digitalWrite(spiDataCommandPin, HIGH); // Data mode

    screenSPISettings = SPISettings(freq, MSBFIRST, SPI_MODE0);
    screenSPI->begin();
    if (resetPinConfigured)
    {
        // hardware reset
        pinMode(resetPin, OUTPUT);
        digitalWrite(resetPin, HIGH);
        delay(100);
        digitalWrite(resetPin, LOW);
        delay(100);
        digitalWrite(resetPin, HIGH);
        delay(200);
    }
}

void sendCommandSPI(Byte commandByte, Byte* dataBytes, Byte numDataBytes)
{
    screenSPI->beginTransaction(screenSPISettings);
    digitalWrite(spiChipSelectPin, LOW);

    digitalWrite(spiDataCommandPin, LOW); // Command mode
    screenSPI->transfer(commandByte);  // Send the command byte

    digitalWrite(spiDataCommandPin, HIGH);
    for (int i = 0; i < numDataBytes; i++)
    {
        screenSPI->transfer(*dataBytes); // Send the data bytes
        dataBytes++;
    }

    digitalWrite(spiChipSelectPin, HIGH);
    screenSPI->endTransaction();
}
void sendCommandSPI(Byte commandByte, const Byte* dataBytes, Byte numDataBytes)
{
    screenSPI->beginTransaction(screenSPISettings);
    digitalWrite(spiChipSelectPin, LOW);

    digitalWrite(spiDataCommandPin, LOW); // Command mode
    screenSPI->transfer(commandByte);  // Send the command byte

    digitalWrite(spiDataCommandPin, HIGH);
    for (int i = 0; i < numDataBytes; i++)
    {
        screenSPI->transfer(*dataBytes); // Send the data bytes
        dataBytes++;
    }
    digitalWrite(spiChipSelectPin, HIGH);
    screenSPI->endTransaction();
}

void writeCommandSPI(Byte cmd)
{
    digitalWrite(spiDataCommandPin, LOW);
    screenSPI->transfer(cmd);
    digitalWrite(spiDataCommandPin, HIGH);
}

void setAddrWindowSPI(UInt x1, UInt y1, UInt x2, UInt y2, bool write)
{
    x1 += xFudge;
    x2 += xFudge;
    y1 += yFudge;
    y2 += yFudge;
    writeCommandSPI(TFT_CASET); // Column address set
    screenSPI->transfer16(x1);
    screenSPI->transfer16(x2);
    writeCommandSPI(TFT_PASET); // Row address set
    screenSPI->transfer16(y1);
    screenSPI->transfer16(y2);
    writeCommandSPI(write ? TFT_RAMWR : TFT_RAMRD); // Write or Read RAM
}

void startTxSPI(void)
{
    screenSPI->beginTransaction(screenSPISettings);
    digitalWrite(spiChipSelectPin, LOW);
}
void endTxSPI(void)
{
    digitalWrite(spiChipSelectPin, HIGH);
    screenSPI->endTransaction();
}

void writeSPI(Byte b)
{
    screenSPI->transfer(b);
}

void write16SPI(UInt w)
{
    screenSPI->transfer16(w);
}
void writeBufferSPI(UInt size)
{
#ifdef LOLIND1MINI      
    screenSPI->transferBytes((const uint8_t * )frameBuffer, nullptr, size);  
#else
    screenSPI->transfer((const void *)frameBuffer, nullptr, size);
#endif
}
void readBufferSPI(UInt size)
{
#ifdef LOLIND1MINI      
    screenSPI->transferBytes(nullptr, (uint8_t * )frameBuffer, size);  
#else
    screenSPI->transfer(nullptr, (void *)frameBuffer, size);
#endif
}

UInt readColorSPI()
{
    Byte r;
    Byte g;
    Byte b;
    
    r = screenSPI->transfer(r);
    r = screenSPI->transfer(r);
    g = screenSPI->transfer(g);
    b = screenSPI->transfer(b);

    return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
}

DisplayState HRGraphics_Begin()
{
#ifdef DIAGNOSTICS
    Serial.println("HRGraphics_Begin<");
#endif    
    // in case Begin is called more than once
    if (nullptr != frameBuffer)
    {
        free(frameBuffer);
        frameBuffer = nullptr;
    }
    if (nullptr != monoFrameBuffer)
    {
        free(monoFrameBuffer);
        monoFrameBuffer = nullptr;
    }
    DisplayState displayState = DisplayState::eDisplayNotSet;
    for (;;)
    {
        frameBuffer = nullptr;
        monoFrameBuffer = nullptr;
        isRGB444 = false;
        switch (currentDisplay)
        {
          case Display::eILI9341:
          case Display::eST7735:
          case Display::eST7789:
          case Display::eST7796:

              // Color / RGB444
              if (spiConfigured)
              {
                  frameBuffer = (UInt*)malloc(pixelWidth*4); // 4 bytes per pixel (for reading)
                  memset(frameBuffer, 0, pixelWidth*4);
                  isRGB444 = true;
                  displayState = DisplayState::eOK;
                  i2cConfigured = false;
                  matrixConfigured = false;
              }
              else
              {
                  displayState = DisplayState::eSPIPinsNotSet;
#ifdef DIAGNOSTICS                  
                  Serial.println("DisplayState::eSPIPinsNotSet");
#endif
              }
              break;
          case Display::eLedMatrix:
              // Mono / 1 bpp
              if (matrixConfigured)
              {
                  monoFrameBuffer = (Byte*)malloc(pixelWidth*pixelHeight/8); // 1 bit per pixel for monochrome (OLED, LED Matrix, etc)
                  memset(monoFrameBuffer, 0, pixelWidth*pixelHeight/8);
                  displayState = DisplayState::eOK;
                  i2cConfigured = false;
                  spiConfigured = false;
              }
              else
              {
                  displayState = DisplayState::eMatrixNotConfigured;
#ifdef DIAGNOSTICS
                  Serial.println("DisplayState::eMatrixNotConfigured");
#endif
              }
              break;
          case Display::eSSD1306:
              // Mono / 1 bpp
              if (i2cConfigured)
              {
                  monoFrameBuffer = (Byte*)malloc(pixelWidth*pixelHeight/8); // 1 bit per pixel for monochrome (OLED, LED Matrix, etc)
                  memset(monoFrameBuffer, 0, pixelWidth*pixelHeight/8);
                  displayState = DisplayState::eOK;
                  spiConfigured = false;
                  matrixConfigured = false;
              }
              else
              {
                  displayState = DisplayState::eI2CAddressNotSet;
#ifdef DIAGNOSTICS
                  Serial.println("DisplayState::eI2CAddressNotSet");
#endif
              }
              break;
          default:
              currentDisplay = Display::eNoDisplay;
#ifdef DIAGNOSTICS
              Serial.println("DisplayState::eNoDisplay");
#endif
              break;
        }
        if ((pixelWidth == 0) || (pixelWidth % 8 != 0))
        {
            displayState = DisplayState::eBadWidth;
#ifdef DIAGNOSTICS
            Serial.println("DisplayState::eBadWidth");
#endif
        }
        if ((pixelHeight == 0) || (pixelHeight % 8 != 0))
        {
            displayState = DisplayState::eBadHeight;
#ifdef DIAGNOSTICS
            Serial.println("DisplayState::eBadHeight");
#endif
        }
        if (displayState != DisplayState::eOK)
        {
            break;  
        }
        
        // 5x8 character cells
        columns = visiblePixelWidth  / CELLWIDTH;
        rows    = visiblePixelHeight / CELLHEIGHT;
        suspended = 0;

        if (spiConfigured)
        {
            screenSPI = &SPI;
                    
            const Byte* addr = nullptr;
            switch (currentDisplay)
            {
                case Display::eILI9341:
                    addr = initcmdSPI_ILI9341;
                    break;

                case Display::eST7735:
                    
                    if ((pixelWidth == 160) && (pixelHeight == 80))
                    {
                        // Pico-LCD-0.96:
                        addr = initcmdSPI_ST7735_096;
                        xFudge = 1;
                        yFudge = 26;
                    }
                    else // pixelHeight == 128
                    {
                        // Pico-LCD-1.44:
                        addr = initcmdSPI_ST7735_144;
                        xFudge = 2;
                        yFudge = 1;
                    }
#ifdef RP2040
                    if (portPinsConfigured) 
                    {
                        screenSPI = &SPI1; // why?
                    }
#endif
                    break;
                case Display::eST7789:
                    // Pico-LCD-1.14:
                    addr = initcmdSPI_ST7789_114;
                    xFudge = 40;
                    yFudge = 53;
#ifdef RP2040
                    if (portPinsConfigured) 
                    {
                        screenSPI = &SPI1; // why?
                    }
#endif
                    break;
                case Display::eST7796:
                    addr = initcmdSPI_ST7796;
                    break;
            }

            initScreenSPI(SPI_DEFAULT_FREQ);
            Byte cmd, x, numArgs;
            
            while ((cmd = pgm_read_byte(addr++)) > 0)
            {
                x = pgm_read_byte(addr++);
                numArgs = x & 0x7F;
                sendCommandSPI(cmd, addr, numArgs);
                addr += numArgs;
                if (x & 0x80)
                {
                    delay(150);
                }
            }
#ifdef DIAGNOSTICS
            Serial.println("SPI initialized");
#endif
        }
        if (i2cConfigured)
        {
            // // https://www.instructables.com/Getting-Started-With-OLED-Displays/
            //
            // 0x40: Data stream
            // 0xC0: Single Data Byte
            // 0x80: Single Command byte
            // 0x00: Command Stream

            Wire.begin();
            Wire.beginTransmission(i2cAddress);
            
            
            Wire.write(0x00);
            Wire.write(SSD1306_DISPLAYOFF);
            Wire.write(SSD1306_CHARGEPUMP); // Enable charge pump regulator (RESET = )
            Wire.write(0x14);               // Generate the high voltage from the 3.3v line internally
            Wire.write(SSD1306_MEMORYMODE); // Set Memory Addressing Mode to Horizontal Addressing Mode (RESET = Page Addressing Mode)
            Wire.write(0x00);
            Wire.write(SSD1306_COLUMNADDR); // Reset Column Address (for horizontal addressing)
            if (pixelWidth == 64)
            {
                Wire.write(32);
                Wire.write(32+pixelWidth-1);
            }
            else
            {
                Wire.write(0x00);
                Wire.write(pixelWidth-1);
            }
            Wire.write(SSD1306_PAGEADDR);   // Reset Page Address (for horizontal addressing)
            Wire.write(0x00);
            Wire.write((pixelHeight/8)-1);
            
            Wire.write(SSD1306_SETMULTIPLEX);
            Wire.write(pixelHeight- 1);
            Wire.write(SSD1306_DISPLAYON);  // Display On (RESET = )

            Wire.endTransmission();
#ifdef DIAGNOSTICS
            Serial.println("I2C initialized");
#endif
        }

        UInt chr;
        Byte index;
    
        if (isRGB444)
        {
            for (UInt i = 0; i < 256; i++)
            {
                fontMap[i] = 0;
            }
            
            const UInt* faddr = (const UInt*)tinyFont;
            index = 0;
            for (;;)
            {
                chr = pgm_read_word(faddr);
                faddr += 9;
                fontMap[(Byte)chr] = index;
                if (chr == 0x007E)
                {
                    break;
                }
                index++;
            }
#ifdef DIAGNOSTICS
            Serial.println("Tiny Font initialized");
#endif
        }

        if (matrixConfigured)
        {
            pinMode(matrixClockPin, OUTPUT);
            pinMode(matrixDataPin, OUTPUT);
            digitalWrite(matrixClockPin, HIGH);
            digitalWrite(matrixDataPin, HIGH);
#ifdef DIAGNOSTICS
            Serial.println("Matrix initialized");
#endif
        }

        break;
    } // for (;;)
#ifdef DIAGNOSTICS
    Serial.println(">");
#endif
    return displayState;
}
void HRGraphics_End()
{
#ifdef DIAGNOSTICS
    Serial.println();
    Serial.print("HRGraphics_End<");
#endif    
    for(;;)
    {
        if (currentDisplay == Display::eNoDisplay)
        {
            break;
        }
        if (nullptr != frameBuffer)
        {
            free(frameBuffer);
            frameBuffer = nullptr;
        }
        if (nullptr != monoFrameBuffer)
        {
            free(monoFrameBuffer);
            monoFrameBuffer = nullptr;
        }
        spiConfigured = false;
        i2cConfigured = false;
        matrixConfigured = false;
        resetPinConfigured = false;
        isRGB444 = false;
        currentDisplay = Display::eNoDisplay;
        break;
    }
#ifdef DIAGNOSTICS
    Serial.println(">");
#endif
}
void sendMatrix(Byte data)
{
    for (int i = 0; i < 8; i++) 
    {
        digitalWrite(matrixClockPin, LOW);
        digitalWrite(matrixDataPin, data & 1 ? HIGH : LOW);
        data >>= 1;
        digitalWrite(matrixClockPin, HIGH);
    }
}
void sendMatrixCommand(Byte cmd)
{
    digitalWrite(matrixDataPin, LOW);
    sendMatrix(cmd);
    digitalWrite(matrixDataPin, HIGH);
}
void sendMatrixData(Byte address, Byte data)
{
      sendMatrixCommand(0x44);
      digitalWrite(matrixDataPin, LOW);
      sendMatrix(0xC0 | address);
      sendMatrix(data);
      digitalWrite(matrixDataPin, HIGH);
}
// https://github.com/ThingPulse/esp8266-oled-ssd1306/blob/master/src/OLEDDisplay.cpp
void HRGraphics_InvertDisplay(Bool invertColours)
{
    if (i2cConfigured)
    {
        sendCommandI2C(invertColours ? SSD1306_INVERTDISPLAY : SSD1306_NORMALDISPLAY);
    }
}
void HRGraphics_FlipDisplay(Bool flipVertical)
{
    if (i2cConfigured)
    {
        sendCommandI2C(flipVertical ? SSD1306_COMSCANDEC : SSD1306_COMSCANINC);
        sendCommandI2C(flipVertical ? SSD1306_SETSEGMENTREMAP : SSD1306_SEGREMAP);
    }
}
void HRGraphics_Show(Bool on)
{
    if (i2cConfigured)
    {
        sendCommandI2C(on ? SSD1306_DISPLAYON : SSD1306_DISPLAYOFF);
    }
}

void HRScreen_Resume(Bool isInteractive)
{
    if (suspended > 0)
    {
        suspended--;
    }
    if (suspended == 0)
    {
        if (i2cConfigured)
        {
            // update screen from buffer
            UInt address = 0;
            for (UInt y = 0; y < pixelHeight; y++) 
            {
                Wire.beginTransmission(i2cAddress);
                Wire.write(0x40);
                for(UInt i = 0; i < pixelWidth/8; i++) 
                {
                    Wire.write(monoFrameBuffer[address]);
                    address++;
                }
                Wire.endTransmission();
            }
        }
        if (matrixConfigured)
        {
            for(uint8_t i=0;i<8;i++)
            {
                sendMatrixData(i, monoFrameBuffer[i]);

                digitalWrite(matrixDataPin, LOW);
                digitalWrite(matrixClockPin, LOW);
                digitalWrite(matrixClockPin, HIGH);
                digitalWrite(matrixDataPin, HIGH);
            }
            sendMatrixCommand(0x88|(matrixIntensity));
        }
        External_WatchDog();
    }
    
}
void HRScreen_Suspend()
{
    suspended++;
}

void HRGraphics_SetPixel(Int x, Int y, UInt colour)
{
    if ((x < 0) || (y < 0) || (x >= pixelWidth) || (y >= pixelHeight)) { return; }
    HRScreen_Suspend();
    if (spiConfigured)
    {
        UInt rgb565;
        if (colour == 0xF000) // Invert
        {
            startTxSPI();
            setAddrWindowSPI(x, y, x, y, false);
            rgb565 = readColorSPI();
            endTxSPI();
            rgb565 = ~rgb565;
        }
        else
        {
            rgb565 = convertToRGB565(colour);
        }
        startTxSPI();
        setAddrWindowSPI(x, y, x, y, true);
        write16SPI(rgb565);
        endTxSPI();
    }
    if (i2cConfigured)
    {
        UInt offset = ((UInt(y) & 0xFFF8) * (pixelWidth/8)) + UInt(x);
        if (colour == 0xF000) // Color.Invert
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ (1 << (y & 0x07));
        }
        else if (colour == 0x0000) // Color.Black
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] & ~(1 << (y & 0x07));
        }
        else
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] | (1 << (y & 0x07));
        }
    }
    if (matrixConfigured)
    {
        if (colour == 0xF000) // Color.Invert
        {
            monoFrameBuffer[y] = monoFrameBuffer[y] ^ (1 << x);
        }
        else if (colour == 0x0000) // Color.Black
        {
            monoFrameBuffer[y] = monoFrameBuffer[y] & ~(1 << x);
        }
        else
        {
            monoFrameBuffer[y] = monoFrameBuffer[y] | (1 << x);
        }
    }
    HRScreen_Resume(false);
}
void HRScreen_Clear()
{
    HRGraphics_Clear(0x0000); // black
    cursorX = 0;
    cursorY = 0;
}
void HRScreen_Print(Char ch)
{
    HRScreen_DrawChar(cursorX, cursorY, ch, 0x7F7, 0x000);
    //HRGraphics_DrawChar(cursorX * (CELLWIDTH-1), cursorY * CELLHEIGHT, ch, 0x7F7, 0x000, 1, true);
    cursorX++;
    if (cursorX == columns)
    {
        HRScreen_PrintLn();
    }
}
void HRScreen_PrintLn()
{
    cursorX = 0;
    cursorY++;
    // TODO : scroll
}

void HRGraphics_Clear(UInt colour)
{
    if (spiConfigured)
    {
        uint16_t rgb565 = convertToRGB565(colour);
        rgb565 = (rgb565 << 8) | (rgb565 >> 8);
        startTxSPI();
        setAddrWindowSPI(0, 0, pixelWidth-1, pixelHeight-1, true);
        for (UInt y = 0; y < pixelHeight; y++)
        {
            for (UInt x=0; x < pixelWidth; x++)
            {
                frameBuffer[x] = rgb565;
            }
            writeBufferSPI(pixelWidth*2);
        }
        endTxSPI();
    }

    if (i2cConfigured || matrixConfigured)
    {
        HRScreen_Suspend();
        for (int y = 0; y < pixelHeight; y++)
        {
            for (int x = 0; x < pixelWidth; x++)
            {
                HRGraphics_SetPixel(x, y, colour);
            }
        }
        HRScreen_Resume(false);
    }

}

void HRGraphics_HorizontalLine(UInt x1, UInt y1, UInt x2, UInt y2, UInt colour)
{
    if (x1 > x2)
    {
        UInt t = x1;
        x1 = x2;
        x2 = t;
    }
    if (spiConfigured)
    {
        UInt rgb565;
        UInt length = x2-x1+1;
        if (colour == 0xF000)
        {
            UInt i = 0;
            Byte r;
            Byte g;
            Byte b;
            startTxSPI();
            setAddrWindowSPI(x1, y1, x2, y2, false);
            readBufferSPI(length*4);
            endTxSPI();
            for (UInt x=0; x < length; x++)
            {
                r = frameBuffer[x*2] & 0xFF;
                g = frameBuffer[x*2+1] >> 8;
                b = frameBuffer[x*2+1] & 0xFF;
                frameBuffer[x] = ~(((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3));
            }
            startTxSPI();
            setAddrWindowSPI(x1, y1, x2, y2, true);
            writeBufferSPI(length*2);
            endTxSPI();
        }
        else
        {
            rgb565 = convertToRGB565(colour);
            rgb565 = (rgb565 << 8) | (rgb565 >> 8);
            startTxSPI();
            setAddrWindowSPI(x1, y1, x2, y2, true);
            for (UInt x=0; x < length; x++)
            {
                frameBuffer[x] = rgb565;
            }
            writeBufferSPI(length*2);
            endTxSPI();
        }
    } // spiConfigured

    if (i2cConfigured || matrixConfigured)
    {
        HRScreen_Suspend();
        for (UInt x=x1; x <= x2; x++)
        {
            HRGraphics_SetPixel(x, y1, colour);
        }
        HRScreen_Resume(false);
    }

}
void HRGraphics_VerticalLine(UInt x1, UInt y1, UInt x2, UInt y2, UInt colour)
{
    if (y1 > y2)
    {
        UInt t = y1;
        y1 = y2;
        y2 = t;
    }
    if (spiConfigured)
    {
        if (colour == 0xF000)
        {
            for (UInt y=y1; y <= y2; y++)
            {
                HRGraphics_SetPixel(x1, y, colour);
            }
        }
        else
        {
            uint16_t rgb565 = convertToRGB565(colour);
            rgb565 = (rgb565 << 8) | (rgb565 >> 8);
            startTxSPI();
            setAddrWindowSPI(x1, y1, x2, y2, true);
            UInt length = y2-y1+1;
            for (UInt y=0; y < length; y++)
            {
                frameBuffer[y] = rgb565;
            }
            writeBufferSPI(length*2);
            endTxSPI();
        }
    }  
    if (i2cConfigured || matrixConfigured)
    {
        HRScreen_Suspend();
        for (UInt y=y1; y <= y2; y++)
        {
            HRGraphics_SetPixel(x1, y, colour);
        }
        HRScreen_Resume(false);
    }
}
void HRGraphics_Rectangle(UInt x, UInt y, UInt w, UInt h, UInt colour)
{
    HRScreen_Suspend();
    HRGraphics_HorizontalLine(x,y,x+w-1,y, colour);
    HRGraphics_HorizontalLine(x,y+h-1,x+w-1,y+h-1, colour);
    HRGraphics_VerticalLine(x,y,x,y+h-1,colour);
    HRGraphics_VerticalLine(x+w-1,y,x+w-1,y+h-1,colour);
    HRScreen_Resume(false);
}
void HRGraphics_FilledRectangle(UInt x, UInt y, UInt w, UInt h, UInt colour)
{
    HRScreen_Suspend();
    for (UInt i=y; i < y+h; i++)
    {
        HRGraphics_HorizontalLine(x, i, x+w-1, i, colour);
    }
    HRScreen_Resume(false);
}

void drawCirclePoints(uint xc, uint yc, int x, int y, UInt colour) 
{ 
    HRGraphics_SetPixel(xc+x, yc+y, colour); 
    HRGraphics_SetPixel(xc-x, yc+y, colour); 
    HRGraphics_SetPixel(xc+x, yc-y, colour); 
    HRGraphics_SetPixel(xc-x, yc-y, colour); 
    HRGraphics_SetPixel(xc+y, yc+x, colour); 
    HRGraphics_SetPixel(xc-y, yc+x, colour); 
    HRGraphics_SetPixel(xc+y, yc-x, colour); 
    HRGraphics_SetPixel(xc-y, yc-x, colour); 
} 
void HRGraphics_Circle(UInt xc, UInt yc, UInt r, UInt colour)
{
    HRScreen_Suspend();
    
    int x = 0, y = r; 
    int d = 3 - 2 * r; 
    drawCirclePoints(xc, yc, x, y, colour); 
    while (y >= x) 
    { 
        x++; 
        if (d > 0) 
        { 
            y--;  
            d = d + 4 * (x - y) + 10; 
        } 
        else
        {
            d = d + 4 * x + 6; 
        }
        drawCirclePoints(xc, yc, x, y, colour); 
    } 
    
    HRScreen_Resume(false);
}

void HRGraphics_FilledCircle(UInt xc, UInt yc, UInt r, UInt colour)
{
    HRScreen_Suspend();
    
    int r2 = r*r;
    for (int y=-r; y<=r; y++)
    {
        int y2 = y*y;
        for (int x=-r; x<=r; x++)
        {
            if(x*x+y2 <= r2)
            {
                HRGraphics_SetPixel(xc+x, yc+y, colour);
            }
        }
    }

    
    //while (r != 0)
    //{
    //    HRGraphics_Circle(xc, yc, r, colour);
    //    r--;
    //} 
    
    HRScreen_Resume(false);
}


void lineLow(int x0, int y0, int x1, int y1, uint colour)
{
    int dx = x1 - x0;
    int dy = y1 - y0;
    int yi = 1;
    if (dy < 0)
    {
        yi = -1;
        dy = -dy;
    }
    int d = (2 * dy) - dx;
    int y = y0;
    for (int x = x0; x <= x1; x++)
    {    
        HRGraphics_SetPixel(uint(x), uint(y), colour);
        if (d > 0)
        {
            y = y + yi;
            d = d + (2 * (dy - dx));
        }
        else
        {
            d = d + 2*dy;
        }
    }
}

void lineHigh(int x0, int y0, int x1, int y1, uint colour)
{
    int dx = x1 - x0;
    int dy = y1 - y0;
    int xi = 1;
    if (dx < 0)
    {
        xi = -1;
        dx = -dx;
    }
    int d = (2 * dx) - dy;
    int x = x0;
    for (int y = y0; y <= y1; y++)
    {
        HRGraphics_SetPixel(uint(x), uint(y), colour);
        if (d > 0)
        {
            x = x + xi;
            d = d + (2 * (dx - dy));
        }
        else
        {
            d = d + 2*dx;
        }
    }
}

// alternate Bresenham : https://www.instructables.com/Getting-Started-With-OLED-Displays/
void HRGraphics_Line(UInt x0, UInt y0, UInt x1, UInt y1, UInt colour)
{
    HRScreen_Suspend();
    if (x0 == x1)      { HRGraphics_VerticalLine(x0, y0, x1, y1, colour);   }
    else if (y0 == y1) { HRGraphics_HorizontalLine(x0, y0, x1, y1, colour); }
    else if (abs((int)(y1)-(int)(y0)) < abs((int)(x1)-(int)(x0)))
    {
        if (x0 > x1)
        {
            lineLow((int)(x1), (int)(y1), (int)(x0), (int)(y0), colour);
        }
        else
        {
            lineLow((int)(x0), (int)(y0), (int)(x1), (int)(y1), colour);
        }
    }
    else
    {
        if (y0 > y1)
        {
            lineHigh((int)(x1), (int)(y1), (int)(x0), (int)(y0), colour);
        }
        else
        {
            lineHigh((int)(x0), (int)(y0), (int)(x1), (int)(y1), colour);
        }
    }
    HRScreen_Resume(false);
}
void render5x8Character(Char chr, UInt foreColour, UInt backColour)
{
    Byte * buffer = (Byte*)&fontBuffer;
    Byte index = fontMap[chr]; // if it is missing from the font, index will be 0 (' ')
    UInt pixelb = blendPixelColor(0xF, backColour, backColour);
    if (index == 0)
    {
        // ' '
        UInt pixelCount = 50; // 5x10/2
        for (UInt i = 0; i < 50; i++)
        {
            buffer[i * 2] = pixelb >> 8;
            buffer[i * 2 + 1] = pixelb & 0xFF;
        }
        return;
    }

    UInt bi = 0;
    // top row
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;

    const UInt* addr = (const UInt*)tinyFont;
    addr = addr + (9 * index);
    addr++;

    Byte tonel = 16; // not possible
    UInt pixell = 0;
    for (Byte y = 0; y < 8; y++)
    {
        UInt tones = pgm_read_word(addr);
        addr++;

        Byte tone0 = ((tones >> 12) & 0x0F);
        Byte tone1 = ((tones >> 8) & 0x0F);
        UInt pixel0 = (tonel == tone0) ? pixell : blendPixelColor(tone0, foreColour, backColour);
        UInt pixel1 = (tone0 == tone1) ? pixel0 : blendPixelColor(tone1, foreColour, backColour);
        buffer[bi++] = pixel0 >> 8;
        buffer[bi++] = pixel0 & 0xFF;
        buffer[bi++] = pixel1 >> 8;
        buffer[bi++] = pixel1 & 0xFF;

        Byte tone2 = ((tones >> 4) & 0x0F);
        Byte tone3 = (tones & 0x0F);
        UInt pixel2 = (tone1 == tone2) ? pixel1 : blendPixelColor(tone2, foreColour, backColour);
        UInt pixel3 = (tone2 == tone3) ? pixel2 : blendPixelColor(tone3, foreColour, backColour);
        buffer[bi++] = pixel2 >> 8;
        buffer[bi++] = pixel2 & 0xFF;
        buffer[bi++] = pixel3 >> 8;
        buffer[bi++] = pixel3 & 0xFF;


        //right column
        buffer[bi++] = pixelb >> 8;
        buffer[bi++] = pixelb & 0xFF;

        pixell = pixel3;
        tonel = tone3;
    }

    // bottom row
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
}

void render6x8MonoCharacter(Char chr, UInt foreColour, UInt backColour)
{
    Byte * buffer = (Byte*)&fontBuffer;
    UInt pixelb;
    UInt pixelf;
    if (isRGB444)
    {
        pixelb = blendPixelColor(0xF, backColour, backColour);
        pixelf = blendPixelColor(0xF, foreColour, foreColour);
    }
    else
    {
        pixelb = ((backColour == 0x0000) || (backColour == 0xF000)) ? 0x0000 : 0x0FFF;
        pixelf = ((foreColour == 0x0000) || (foreColour == 0xF000)) ? 0x0000 : 0x0FFF;
    }
    if ((chr <= 32) || (chr > 127))
    {
        // ' '
        UInt pixelCount = 60; // 6x10/2
        for (UInt i = 0; i < 60; i++)
        {
            buffer[i * 2]     = pixelb >> 8;
            buffer[i * 2 + 1] = pixelb & 0xFF;
        }
        return;
    }
    UInt bi = 0;
    // top row
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;

    const Byte* addr = (const Byte*)monoFont;
    addr = addr + (5 * (chr-32));
    Byte colColours[5];
    for (Byte x = 0; x < 5; x++)
    {
        colColours[x] = pgm_read_byte(addr);
        addr++;
    }

    for (Byte y = 0; y < 8; y++)     // 0..7
    {
        for (Byte x = 0; x < 5; x++) // 0..4
        {
            bool isSet = (colColours[x] & (1 << y)) != 0;    
            buffer[bi++] = (isSet ? pixelf : pixelb) >> 8;
            buffer[bi++] = (isSet ? pixelf : pixelb) & 0xFF;
        }
        // space on right
        buffer[bi++] = pixelb >> 8;
        buffer[bi++] = pixelb & 0xFF;
    }

    // bottom row
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
    buffer[bi++] = pixelb >> 8;
    buffer[bi++] = pixelb & 0xFF;
}

void drawMonoChar(UInt x0, UInt y0, Char chr, UInt foreColour, UInt backColour, Byte scale)
{
    HRScreen_Suspend();
    if (scale > 3)
    {
        scale = 3;
    }
    for (;;)
    {
        render6x8MonoCharacter(chr, foreColour, backColour);

        if (isRGB444 || spiConfigured)
        {
            startTxSPI();
            setAddrWindowSPI(x0, y0, x0 + (CELLWIDTH * scale) - 1, y0 + (CELLHEIGHT * scale) - 1, true); 
            switch (scale)
            {
                case 1:
    #ifdef LOLIND1MINI      
                    screenSPI->transferBytes((const uint8_t * )&fontBuffer, nullptr, CELLWIDTH * CELLHEIGHT * 2);  
    #else
                    screenSPI->transfer(&fontBuffer, nullptr, CELLWIDTH * CELLHEIGHT * 2);
    #endif
                    break;
                case 2:
                case 3:
                    {
                        UInt charBuffer[CELLWIDTH * CELLHEIGHT * 3 * 3];
                        UInt i = 0;
                        for (Byte y = 0; y < CELLHEIGHT*scale; y++)
                        {
                            UInt yb = y / scale;
                            for (Byte x = 0; x < CELLWIDTH; x++)
                            {
                                for (Byte c=0; c < scale; c++)
                                {
                                    charBuffer[i] = fontBuffer[x + yb * CELLWIDTH];
                                    i++;
                                }
                            }
                        }
    #ifdef LOLIND1MINI      
                        screenSPI->transferBytes((const uint8_t * )&charBuffer, nullptr, CELLWIDTH * CELLHEIGHT * 2 * scale * scale);  
    #else
                        screenSPI->transfer(&charBuffer, nullptr, CELLWIDTH * CELLHEIGHT * 2 * scale * scale);
    #endif
                    }
                    break;
            }
            endTxSPI();
        } // isRGB444 || spiConfigured

        if (i2cConfigured || matrixConfigured)
        {
            UInt i = 0;
            for (UInt y=0; y < CELLHEIGHT*scale; y++)
            {
                UInt yb = y / scale;
                for (UInt x=0; x < CELLWIDTH; x++)
                {
                    for (Byte c=0; c < scale; c++)
                    {
                        UInt colour = fontBuffer[x + yb * CELLWIDTH];
                        HRGraphics_SetPixel(x*scale+c+x0, y+y0, colour);
                    }
                }
            }
        }

        break;
    }
    HRScreen_Resume(false);
}

void drawAliasedChar(UInt x0, UInt y0, Char chr, UInt foreColour, UInt backColour, Byte scale)
{
    HRScreen_Suspend();
    if (scale > 3)
    {
        scale = 3;
    }
    for (;;)
    {
        if (spiConfigured)
        {
            render5x8Character(chr, foreColour, backColour);
            startTxSPI();
            setAddrWindowSPI(x0, y0, x0 + (CELLWIDTH-1) * scale - 1, y0 + CELLHEIGHT * scale - 1, true); 
            switch (scale)
            {
                case 1:
    #ifdef LOLIND1MINI      
                    screenSPI->transferBytes((const uint8_t * )&fontBuffer, nullptr, (CELLWIDTH-1) * CELLHEIGHT * 2);  
    #else
                    screenSPI->transfer(&fontBuffer, nullptr, (CELLWIDTH-1) * CELLHEIGHT * 2);
    #endif
                    break;
                case 2:
                case 3:
                    {
                        UInt charBuffer[(CELLWIDTH-1) * CELLHEIGHT * 2 * 3 * 3];
                        UInt i = 0;
                        for (Byte y = 0; y < CELLHEIGHT*scale; y++)
                        {
                            UInt yb = y / scale;
                            for (Byte x = 0; x < (CELLWIDTH-1); x++)
                            {
                                for (Byte c=0; c < scale; c++)
                                {
                                    charBuffer[i] = fontBuffer[x + yb * (CELLWIDTH-1)];
                                    i++;
                                }
                            }
                        }
    #ifdef LOLIND1MINI      
                        screenSPI->transferBytes((const uint8_t * )&charBuffer, nullptr, (CELLWIDTH-1) * CELLHEIGHT * 2 * scale * scale);  
    #else
                        screenSPI->transfer(&charBuffer, nullptr, (CELLWIDTH-1) * CELLHEIGHT * 2 * scale * scale);
    #endif
                    }
                    break;
            }
            endTxSPI();
        } // spiConfigured
        break;
    } // for (;;)
    HRScreen_Resume(false);
}

void HRGraphics_DrawChar(UInt x, UInt y, Char chr, UInt foreColour, UInt backColour, Byte scale, Bool antiAliased)
{

    if (antiAliased && isRGB444)
    {
        if ((x+(CELLWIDTH-1)*scale >= pixelWidth) || (y+CELLHEIGHT*scale >= pixelHeight))
        {
            return; // clipping
        }
        drawAliasedChar(x, y, chr, foreColour, backColour, scale);
    }
    else
    {
        if ((x+CELLWIDTH*scale >= pixelWidth) || (y+CELLHEIGHT*scale >= pixelHeight))
        {
            return; // clipping
        }
        drawMonoChar(x, y, chr, foreColour, backColour, scale);
    }
}
void HRScreen_DrawChar(UInt col, UInt row, Char chr, UInt foreColour, UInt backColour)
{
    if ((col >= columns) || (row >= rows))
    {
        return; // clipping
    }
    drawMonoChar(col * CELLWIDTH, row * CELLHEIGHT, chr, foreColour, backColour, 1);
}
*/
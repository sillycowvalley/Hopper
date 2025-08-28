# I2C Commands Documentation for Hopper BASIC

## Overview
I2C (Inter-Integrated Circuit) is a two-wire serial communication protocol commonly used to connect microcontrollers to peripherals like displays, sensors, and memory chips. Hopper BASIC provides six commands for I2C communication.

## I2C Commands Reference

### Device Discovery
**`I2CFIND(address)`**
- Tests if a device responds at the specified I2C address
- **Parameter**: address (0-127)
- **Returns**: TRUE if device responds, FALSE if no response
- **Example**: `IF I2CFIND(60) THEN PRINT "Device found"`

### Write Operations
**`I2CBEGIN(address)`**
- Starts an I2C write transaction
- **Parameter**: address (0-127)
- **Note**: Must be followed by `I2CEND()` to complete transaction

**`I2CPUT(byte)`**
- Sends one byte in the current write transaction
- **Parameter**: byte value (0-255)
- **Note**: Can be called multiple times between `I2CBEGIN` and `I2CEND`

**`I2CEND()`**
- Completes the current I2C transaction
- **Returns**: TRUE if all operations were acknowledged, FALSE if any NACK received

### Read Operations
**`I2CGET(address, count)`**
- Reads specified number of bytes from a device
- **Parameters**: 
  - address (0-127)
  - count (0-255) - number of bytes to read
- **Returns**: Actual number of bytes read (may be less than requested)
- **Note**: Complete transaction - includes START and STOP

**`I2CNEXT()`**
- Retrieves the next byte from the I2C receive buffer
- **Returns**: Next byte value (0-255), or 0 if buffer empty
- **Note**: Used after `I2CGET` to retrieve buffered data

## Common Device Addresses
```basic
CONST OLED = 60      ! SSD1306 OLED display (0x3C)
CONST EEPROM = 80    ! 24C256 Serial EEPROM (0x50)
CONST RTC = 104      ! DS3231 Real-Time Clock (0x68)
CONST TEMP = 72      ! LM75 Temperature sensor (0x48)
```

## Usage Examples

### Scanning the I2C Bus
```basic
! Scan for all I2C devices
PRINT "Scanning I2C bus..."
VAR found = 0
FOR addr = 8 TO 119
    IF I2CFIND(addr) THEN
        PRINT "Device at address"; addr
        found = found + 1
    ENDIF
NEXT addr
PRINT found; "devices found"
```

### Writing to an OLED Display
```basic
! Initialize SSD1306 OLED display
CONST OLED = 60

IF NOT I2CFIND(OLED) THEN
    PRINT "OLED not found!"
    END
ENDIF

! Send initialization commands
I2CBEGIN(OLED)
I2CPUT(0)       ! Command mode
I2CPUT(174)     ! Display off (0xAE)
I2CPUT(213)     ! Set display clock (0xD5)
I2CPUT(128)     ! Suggested ratio
I2CPUT(168)     ! Set multiplex (0xA8)
I2CPUT(63)      ! 1/64 duty
I2CPUT(211)     ! Set display offset (0xD3)
I2CPUT(0)       ! No offset
I2CPUT(175)     ! Display on (0xAF)

IF I2CEND() THEN
    PRINT "OLED initialized"
ELSE
    PRINT "OLED init failed"
ENDIF
```

### Reading from EEPROM
```basic
! Read 16 bytes from EEPROM at address 0x0000
CONST EEPROM = 80

! Set read address (write 2-byte address, then read)
I2CBEGIN(EEPROM)
I2CPUT(0)       ! Address high byte
I2CPUT(0)       ! Address low byte
IF NOT I2CEND() THEN
    PRINT "Failed to set EEPROM address"
    END
ENDIF

! Read 16 bytes
VAR count = I2CGET(EEPROM, 16)
PRINT "Read"; count; "bytes:"

FOR i = 1 TO count
    VAR b = I2CNEXT()
    PRINT b;
    IF i MOD 8 = 0 THEN PRINT ENDIF
NEXT i
```

### Reading Temperature Sensor
```basic
! Read temperature from LM75 sensor
CONST LM75 = 72

IF NOT I2CFIND(LM75) THEN
    PRINT "Temperature sensor not found"
    END
ENDIF

! Read 2 bytes from temperature register
VAR bytes = I2CGET(LM75, 2)
IF bytes = 2 THEN
    VAR msb = I2CNEXT()
    VAR lsb = I2CNEXT()
    
    ! Convert to temperature (11-bit, 0.125°C resolution)
    VAR temp = msb
    IF msb > 127 THEN
        temp = msb - 256  ! Handle negative temps
    ENDIF
    
    ! Add fractional part (top 3 bits of lsb)
    IF lsb > 127 THEN temp = temp + 1 ENDIF
    
    PRINT "Temperature:"; temp; "°C"
ELSE
    PRINT "Failed to read temperature"
ENDIF
```

## Error Handling
- All I2C addresses must be in range 0-127
- Invalid addresses cause a range error
- Check return values from `I2CEND()` and `I2CGET()` to detect communication failures
- Use `I2CFIND()` before attempting communication with a device

## Hardware Requirements
- I2C hardware must be properly configured with pull-up resistors on SDA and SCL lines
- Default pins are defined in the I2C unit (typically GPIO pins 0 and 1)
- Maximum bus speed depends on hardware implementation

## Limitations
- No support for 10-bit addressing
- No support for clock stretching
- Single master mode only
- Maximum 255 bytes per read operation
- Read buffer shared between all read operations
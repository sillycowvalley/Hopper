unit RIOT
{
    // Original written by Anders Nielsen, 2023-2024
    // License: https://creativecommons.org/licenses/by-nc/4.0/legalcode
    
    const byte riot    = 0x80;
    const byte DRA     = riot + 0x00; // DRA  ('A' side data register)
    const byte DDRA    = riot + 0x01; // DDRA ('A' side data direction register)
    const byte DRB     = riot + 0x02; // DRB  ('B' side data register)
    const byte DDRB    = riot + 0x03; // DDRB ('B' side data direction register)
    const byte READTDI = riot + 0x04; // Read timer (disable interrupt)

    const byte WEDGC   = riot + 0x04; // Write edge-detect control (negative edge-detect,disable interrupt)
    const byte RRIFR   = riot + 0x05; // Read interrupt flag register (bit 7 = timer, bit 6 PA7 edge-detect) Clear PA7 flag
    const byte A7PEDI  = riot + 0x05; // Write edge-detect control (positive edge-detect,disable interrupt)
    const byte A7NEEI  = riot + 0x06; // Write edge-detect control (negative edge-detect, enable interrupt)
    const byte A7PEEI  = riot + 0x07; // Write edge-detect control (positive edge-detect enable interrupt)

    const byte READTEI = riot + 0x0C; // Read timer (enable interrupt)
    const byte WTD1DI  = riot + 0x14; // Write timer (divide by 1, disable interrupt)
    const byte WTD8DI  = riot + 0x15; // Write timer (divide by 8, disable interrupt)
    const byte WTD64DI = riot + 0x16; // Write timer (divide by 64, disable interrupt)
    const byte WTD1KDI = riot + 0x17; // Write timer (divide by 1024, disable interrupt)

    const byte WTD1EI  = riot + 0x1C; // Write timer (divide by 1, enable interrupt)
    const byte WTD8EI  = riot + 0x1D; // Write timer (divide by 8, enable interrupt)
    const byte WTD64EI = riot + 0x1E; // Write timer (divide by 64, enable interrupt)
    const byte WTD1KEI = riot + 0x1F; // Write timer (divide by 1024, enable interrupt)
    
}

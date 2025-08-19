unit Serialize
{

// Start saving a program to EEPROM
// Input: ZP.STR = pointer to filename (null-terminated, uppercase, 1-12 chars)
// Output: C set if file created successfully, NC if error
//         File system ready to receive token streams
// Preserves: X, Y
// Munts: A, file system state
StartSave()
{
}

// Append token stream to current save operation
// Input: ZP.FSOURCEADDRESS = pointer to token stream
//        ZP.FLENGTH = length of token stream in bytes
// Output: C set if data written successfully, NC if error
// Preserves: X, Y
// Munts: A, file system state
// Note: First token indicates object type (Token.FUNC, Token.BEGIN, Token.CONST, Token.VAR, etc.)
AppendStream()
{
    TODO(); BIT ZP.EmulatorPCL
}

// Complete save operation and close file
// Output: C set if file saved successfully, NC if error
// Preserves: X, Y
// Munts: A, file system state
EndSave()
{
    TODO(); BIT ZP.EmulatorPCL
}

// Start loading a program from EEPROM
// Input: ZP.STR = pointer to filename (null-terminated, uppercase)
//        ZP.IDX = pointer to working buffer
//        ZP.FLENGTH = length of working buffer in bytes
// Output: C set if file opened successfully, NC if error (file not found)
//         File system ready to return token streams
// Preserves: X, Y
// Munts: A, file system state
StartLoad()
{
    TODO(); BIT ZP.EmulatorPCL
}

// Get next token stream from current load operation
// Output: C set if stream available, NC if end of file
//         ZP.FSOURCEADDRESS = pointer to token stream (if C set)
//         ZP.FLENGTH = length of token stream in bytes (if C set)
//         First token indicates object type
// Preserves: X, Y
// Munts: A, file system state
// Note: Caller must process/copy stream before next call
NextStream()
{
    TODO(); BIT ZP.EmulatorPCL
}





}
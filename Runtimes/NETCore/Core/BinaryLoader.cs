using System;
using System.Collections.Generic;

namespace HopperRuntime.Core
{
    /// <summary>
    /// Represents the result of loading a Hopper binary file
    /// </summary>
    public class LoadedProgram
    {
        public byte[] Program { get; set; } = new byte[0];
        public byte[] Constants { get; set; } = new byte[0];
        public Dictionary<uint, uint> MethodTable { get; set; } = new Dictionary<uint, uint>();
        public uint Version { get; set; }
        public uint ConstantOffset { get; set; }
        public uint EntryPoint { get; set; }
    }

    /// <summary>
    /// Handles loading and parsing of Hopper binary files (.hexe)
    /// Responsible for file format validation, method table extraction, and call optimization
    /// </summary>
    public static class BinaryLoader
    {
        /// <summary>
        /// Load a Hopper binary file and return the parsed program components
        /// </summary>
        /// <param name="bytecode">Raw bytecode from file</param>
        /// <returns>Parsed program ready for execution</returns>
        /// <exception cref="InvalidOperationException">Thrown when file format is invalid</exception>
        public static LoadedProgram LoadProgram(byte[] bytecode)
        {
            if (bytecode.Length < 6)
                throw new InvalidOperationException("Invalid Hopper binary: file too small for header");

            var result = new LoadedProgram();

            // Parse header
            result.Version = BitConverter.ToUInt16(bytecode, 0);
            result.ConstantOffset = BitConverter.ToUInt16(bytecode, 2);
            result.EntryPoint = BitConverter.ToUInt16(bytecode, 4);

            // Validate header values
            ValidateHeader(bytecode, result);

            // Load method table (between header and constants)
            LoadMethodTable(bytecode, result);

            // Extract program and constants sections
            ExtractProgramSection(bytecode, result);
            ExtractConstantsSection(bytecode, result);

            // Optimize CALL instructions to CALLI at load time
            OptimizeCallInstructions(result);

            return result;
        }

        /// <summary>
        /// Validate the binary header for correctness
        /// </summary>
        private static void ValidateHeader(byte[] bytecode, LoadedProgram program)
        {
            if (program.ConstantOffset < 6)
                throw new InvalidOperationException($"Invalid constant offset: {program.ConstantOffset} (must be >= 6)");

            if (program.EntryPoint < program.ConstantOffset)
                throw new InvalidOperationException($"Invalid entry point: {program.EntryPoint} (must be >= constant offset {program.ConstantOffset})");

            if (program.EntryPoint >= bytecode.Length)
                throw new InvalidOperationException($"Invalid entry point: {program.EntryPoint} (exceeds file size {bytecode.Length})");

            // Check that method table size is valid (must be multiple of 4 bytes)
            uint methodTableSize = program.ConstantOffset - 6;
            if (methodTableSize % 4 != 0)
                throw new InvalidOperationException($"Invalid method table size: {methodTableSize} (must be multiple of 4)");
        }

        /// <summary>
        /// Load the method table from the binary
        /// </summary>
        private static void LoadMethodTable(byte[] bytecode, LoadedProgram program)
        {
            uint methodTableStart = 6; // After header
            uint methodTableEnd = program.ConstantOffset;

            for (uint i = methodTableStart; i < methodTableEnd; i += 4)
            {
                if (i + 3 >= bytecode.Length)
                    throw new InvalidOperationException($"Method table entry at offset {i} exceeds file size");

                uint methodIndex = BitConverter.ToUInt16(bytecode, (int)i);
                uint methodAddress = BitConverter.ToUInt16(bytecode, (int)i + 2);

                // Validate method address is within the program section
                if (methodAddress >= (bytecode.Length - program.EntryPoint))
                    throw new InvalidOperationException($"Method address {methodAddress:X4} for index {methodIndex:X4} exceeds program size");

                program.MethodTable[methodIndex] = methodAddress;
            }
        }

        /// <summary>
        /// Extract the program section from the binary
        /// </summary>
        private static void ExtractProgramSection(byte[] bytecode, LoadedProgram program)
        {
            long programLength = bytecode.Length - program.EntryPoint;
            if (programLength <= 0)
                throw new InvalidOperationException($"Invalid program section: length {programLength}");

            program.Program = new byte[programLength];
            Array.Copy(bytecode, program.EntryPoint, program.Program, 0, programLength);
        }

        /// <summary>
        /// Extract the constants section from the binary
        /// </summary>
        private static void ExtractConstantsSection(byte[] bytecode, LoadedProgram program)
        {
            long constantLength = program.EntryPoint - program.ConstantOffset;

            if (constantLength < 0)
                throw new InvalidOperationException($"Invalid constants section: length {constantLength}");

            program.Constants = new byte[constantLength];
            if (constantLength > 0)
            {
                Array.Copy(bytecode, (int)program.ConstantOffset, program.Constants, 0, constantLength);
            }
        }

        /// <summary>
        /// Optimize CALL instructions to CALLI for better runtime performance
        /// This replaces method table lookups with direct address calls
        /// </summary>
        private static void OptimizeCallInstructions(LoadedProgram program)
        {
            for (int i = 0; i < program.Program.Length - 2; i++)
            {
                if (program.Program[i] == (byte)OpCode.CALL)
                {
                    // Read the method index from the instruction
                    uint methodIndex = (uint)(program.Program[i + 1] | (program.Program[i + 2] << 8));

                    if (program.MethodTable.TryGetValue(methodIndex, out uint address))
                    {
                        // Replace CALL with CALLI and method index with direct address
                        program.Program[i] = (byte)OpCode.CALLI;
                        program.Program[i + 1] = (byte)(address & 0xFF);
                        program.Program[i + 2] = (byte)((address >> 8) & 0xFF);
                    }
                    else
                    {
                        throw new InvalidOperationException($"Method table missing entry for index {methodIndex:X4} at program offset {i:X4}");
                    }
                }
            }
        }

        /// <summary>
        /// Get a summary of the loaded program for debugging
        /// </summary>
        public static string GetProgramSummary(LoadedProgram program)
        {
            return $"Hopper Program Summary:\n" +
                   $"  Version: 0x{program.Version:X4}\n" +
                   $"  Constants: {program.Constants.Length} bytes\n" +
                   $"  Program: {program.Program.Length} bytes\n" +
                   $"  Method Table: {program.MethodTable.Count} entries\n" +
                   $"  Entry Point: 0x{program.EntryPoint:X4}\n" +
                   $"  Constant Offset: 0x{program.ConstantOffset:X4}";
        }

        /// <summary>
        /// Validate that a loaded program is ready for execution
        /// </summary>
        public static void ValidateProgram(LoadedProgram program)
        {
            if (program.Program.Length == 0)
                throw new InvalidOperationException("Program section is empty");

            // Additional validation could be added here:
            // - Check for valid instruction sequences
            // - Verify all CALLI addresses are within bounds
            // - Validate constant references
        }
    }
}
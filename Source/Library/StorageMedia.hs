unit StorageMedia
{
    // Constant size of each sector in bytes.
    const uint SectorSize = 512; // Standard size for FAT12

    // Constant total number of sectors available on the storage media.
    const uint TotalSectors = 1024; // Total sectors for 512K serial EEPROM
    
    const uint reservedSectors = 1;
    const uint numberOfFATs = 2;
    const uint maxRootEntries = 512;
    const uint rootDirSectors = (maxRootEntries * 32) / SectorSize;

    // Initialize the storage media.
    // Returns: true if initialization is successful, otherwise false.
    bool Initialize() library;

    // Mount the storage media.
    // Returns: true if mounting is successful, otherwise false.
    bool Mount() library;

    // Unmount the storage media.
    // Returns: true if unmounting is successful, otherwise false.
    bool Unmount() library;

    // Read data from the specified sector index into the provided byte array.
    // Parameters:
    // - index: The zero-based index of the sector to read.
    // - data: A reference to a byte array that will be filled with the sector data.
    // Returns: true if the read operation is successful, otherwise false.
    bool ReadSector(uint index, ref byte[] data) library;

    // Write data to the specified sector index from the provided byte array.
    // Parameters:
    // - index: The zero-based index of the sector to write.
    // - data: A byte array containing the data to be written to the sector.
    // Returns: true if the write operation is successful, otherwise false.
    bool WriteSector(uint index, byte[] data) library;

    // Formats the storage media by setting up the necessary file system structures.
    // This includes initializing the FAT table and root directory.
    // Returns: true if formatting is successful, otherwise false.
    bool Format()
    {
        if (!StorageMedia.Initialize())
        {
            return false;
        }

        uint dataSectors = TotalSectors - reservedSectors - rootDirSectors;
        uint sectorsPerFAT = ((dataSectors + 2) * 3 / 2) / SectorSize + 1;

        byte[SectorSize] buffer;

        // Zero out all sectors to initialize the media
        for (uint i = 0; i < SectorSize; i++)
        {
            buffer[i] = 0;
        }

        for (uint sector = 0; sector < TotalSectors; sector++)
        {
            if (!StorageMedia.WriteSector(sector, buffer))
            {
                return false;
            }
        }

        // Write the boot sector with FAT12-specific information
        if (!writeBootSector(sectorsPerFAT, rootDirSectors))
        {
            return false;
        }

        // Write the FAT tables
        if (!writeFat(sectorsPerFAT))
        {
            return false;
        }

        // Write the root directory
        if (!writeRootDirectory(rootDirSectors))
        {
            return false;
        }

        return true;
    }

    // Writes the boot sector, setting up essential FAT12 parameters
    bool writeBootSector(uint sectorsPerFAT, uint rootDirSectors)
    {
        byte[SectorSize] bootSector;

        for (uint i = 0; i < SectorSize; i++)
        {
            bootSector[i] = 0;
        }

        // Setting up BPB fields
        string oemName = "HOPPER  ";
        for (uint i = 0; i < oemName.Length; i++)
        {
            bootSector[3 + i] = byte(oemName[i]);
        }

        // BIOS Parameter Block (BPB)
        bootSector[11] = byte(SectorSize & 0xFF);
        bootSector[12] = byte(SectorSize >> 8);
        bootSector[13] = 1; // Sectors per cluster
        bootSector[14] = 1; // Reserved sectors
        bootSector[16] = 2; // Number of FATs
        bootSector[17] = byte(maxRootEntries & 0xFF); // Max root directory entries
        bootSector[18] = byte(maxRootEntries >> 8);
        bootSector[19] = byte(TotalSectors & 0xFF);
        bootSector[20] = byte(TotalSectors >> 8);
        bootSector[21] = 0xF0; // Media descriptor for removable media
        bootSector[22] = byte(sectorsPerFAT & 0xFF);
        bootSector[23] = byte(sectorsPerFAT >> 8);
        bootSector[24] = 1; // Sectors per track (minimal requirement)
        bootSector[25] = 0;
        bootSector[26] = 1; // Number of heads (minimal requirement)
        bootSector[27] = 0;

        // Volume information
        bootSector[36] = 0x80; // Drive number
        bootSector[39] = 0x32;
        bootSector[40] = 0x67;
        bootSector[41] = 0x98;
        bootSector[42] = 0xA5; // Volume serial number

        // Volume label and file system type
        string volumeLabel = "HOPPER    ";
        for (uint i = 0; i < volumeLabel.Length; i++)
        {
            bootSector[43 + i] = byte(volumeLabel[i]);
        }
        string fileSystemType = "FAT12   ";
        for (uint i = 0; i < fileSystemType.Length; i++)
        {
            bootSector[54 + i] = byte(fileSystemType[i]);
        }

        // Boot sector signature
        bootSector[510] = 0x55;
        bootSector[511] = 0xAA;

        return StorageMedia.WriteSector(0, bootSector);
    }

    // Writes the File Allocation Tables (FAT) for FAT12
    bool writeFat(uint sectorsPerFAT)
    {
        byte[SectorSize] fat;

        for (uint i = 0; i < SectorSize; i++)
        {
            fat[i] = 0;
        }

        // Initial FAT entries
        fat[0] = 0xF0; // Media descriptor
        fat[1] = 0xFF;
        fat[2] = 0xFF;

        // Write primary FAT
        for (uint i = 1; i <= sectorsPerFAT; i++)
        {
            if (!StorageMedia.WriteSector(i, fat))
            {
                return false;
            }
        }

        // Write secondary FAT
        for (uint i = sectorsPerFAT + 1; i <= 2 * sectorsPerFAT; i++)
        {
            if (!StorageMedia.WriteSector(i, fat))
            {
                return false;
            }
        }

        return true;
    }

    // Initializes the root directory for FAT12
    bool writeRootDirectory(uint rootDirSectors)
    {
        byte[SectorSize] rootDir;

        for (uint i = 0; i < SectorSize; i++)
        {
            rootDir[i] = 0;
        }

        // Write the root directory sectors
        uint rootDirStart = 1 + 2 * rootDirSectors; // Start after boot sector and FATs
        for (uint i = rootDirStart; i < rootDirStart + rootDirSectors; i++)
        {
            if (!StorageMedia.WriteSector(i, rootDir))
            {
                return false;
            }
        }

        return true;
    }
}


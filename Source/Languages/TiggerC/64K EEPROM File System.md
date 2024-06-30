### EEPROM 64K File System Structure

The file system is designed to efficiently manage files and directories within a 64K EEPROM, optimized for the 8-bit instructions of the 6502 microprocessor. The EEPROM is divided into 256 pages, each 256 bytes, allowing the use of single-byte ChainList entries to simplify block management.

#### Special Block Values
- **0**: Unused block (in ChainList) or free directory entry (if `Start Block` is zero).
- **1**: Last block in chain (in ChainList) or end of chain in directory entry `Start Block` for an empty file.

### Structure Overview

#### 1. ChainLists
- **Description**: Contains chains of blocks for files and directories that span multiple blocks.
- **Location**: Page 0.

```plaintext
+-----------+
| ChainList |
|  (Page 0) |
+-----------+
```

#### 2. Root Directory
- **Description**: The starting point of the file system, storing the root directory entries.
- **Location**: Page 1.

```plaintext
+----------------+
| Root Directory |
|    (Page 1)    |
+----------------+
```

### Descriptor Record Structure (within Directory Pages)
- **Filename**: Offset 0, 12 bytes (null-terminated if shorter).
- **File Type**: Offset 12, 1 byte (high nibble: type, low nibble: name length).
  - `0x00`: File
  - `0x10`: Directory
- **Start Block**: Offset 13, 1 byte (block number where the file/directory starts, `1` if empty or end of chain).
- **File Size**: Offset 14, 2 bytes (size of the file in bytes).

```plaintext
+-----------+-------+---------+-----------+
|  Filename | Type  | Start   | File Size |
| (12 bytes)| 1 byte| Block   | (2 bytes) |
+-----------+-------+---------+-----------+
```

### Diagram of EEPROM Structure

```plaintext
+-----------+
| Block 0   | -> ChainLists
+-----------+
| Block 1   | -> Root Directory
+-----------+
| Block 2   | -> File/Directory Data
|    ...    |
| Block N   | -> File/Directory Data
+-----------+
```

### File API Temporary Structures

File handles and directory handles are temporary structures in memory to facilitate the operations of the file APIs, distinct from the permanent structures on the EEPROM.

#### File Handle Structure (byte array):
- **[0]** - Directory block index (byte)
- **[1]** - File entry index within the directory block (byte)
- **[2-3]** - Current position in file (2 bytes)

```plaintext
+-------------+----------------+------------------+
| DirBlockIdx | FileEntryIdx   | FilePosition     |
|   (1 byte)  |    (1 byte)    |   (2 bytes)      |
+-------------+----------------+------------------+
```

#### Directory Handle Structure (byte array):
- **[0]** - Directory block index (byte)
- **[1]** - Current entry index within the directory block (byte)

```plaintext
+-------------+----------------+
| DirBlockIdx | DirEntryIdx    |
|   (1 byte)  |   (1 byte)     |
+-------------+----------------+
```

This structure allows for flexible file and directory management within the constraints of the EEPROM, supporting both file storage and hierarchical directory structures. The use of single-byte block references facilitates efficient memory access and management, crucial for 8-bit systems like the 6502.

### EEPROM 64K File System Structure

The file system is designed to efficiently manage files and directories within a 64K EEPROM. It is structured as follows:

#### 1. ChainLists
- **Description**: Contains chains of blocks for files and directories that span multiple blocks.
- **Location**: Page 0.

```plaintext
+---------+
| ChainList|
|  (Page 0)|
+---------+
```

#### 2. Root Directory
- **Description**: The starting point of the file system, storing the root directory entries.
- **Location**: Page 1.

```plaintext
+--------------+
| Root Directory|
|    (Page 1)   |
+--------------+
```

### Descriptor Record Structure (within Directory Pages)
- **Filename**: Offset 0, 12 bytes (null-terminated if shorter).
- **File Type**: Offset 12, 1 byte (high nibble: type, low nibble: name length).
  - `0x00`: File
  - `0x10`: Directory
- **Start Block**: Offset 13, 1 byte (block number where the file/directory starts, 1 if empty or end of chain).
- **File Size**: Offset 14, 2 bytes (size of the file in bytes).

```plaintext
+-----------+-------+---------+-----------+
|  Filename |  Type | Start   | File Size |
|  (12 bytes)| 1 byte| Block   |  (2 bytes)|
+-----------+-------+---------+-----------+
```

### Directory Handle Structure (byte array)
- **[0]** - Directory descriptor index (byte).
- **[1]** - Current position in directory (byte).
- **Total**: 2 bytes.

```plaintext
+--------------------+
|  Dir Index | Position |
|   (1 byte)  | (1 byte)|
+--------------------+
```

### Directory Entry Structure (byte array)
- **[0]** - Entry type (0 for file, 1 for directory) (byte).
- **[1-12]** - Entry name (up to 12 bytes, null-terminated if shorter).
- **Total**: 13 bytes.

```plaintext
+-----------+------------------+
|  Type     | Entry Name       |
| (1 byte)  | (12 bytes)       |
+-----------+------------------+
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

This structure allows for flexible file and directory management within the constraints of the EEPROM, supporting both file storage and hierarchical directory structures.

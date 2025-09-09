# SimpleEditor Feature Roadmap

## Implementation Notes
### Shared Flags Byte (0x9F)
```hopper
const byte edFlags = 0x9F;
// Bit 0: Selection active (0=no, 1=yes)
// Bit 1: Modified flag (0=clean, 1=modified)
// Bits 2-3: Undo state (00=empty, 01=can_undo, 10=can_redo, 11=reserved)
// Bits 4-7: Reserved for future use
```

## File Management
- [ ] New File (Ctrl+N or launch with no args)
  - Initialize GapBuffer to 4KB
  - Clear filename, clear bit 1 of edFlags (modified=false)
- [ ] Load File  
  - Command line argument or prompt (Ctrl+O)
  - Initialize GapBuffer to filesize + 2KB (min 4KB)
  - Preserve filename for display
  - Clear bit 1 of edFlags (modified=false)
- [ ] Save File (Ctrl+S)
  - Use preserved filename
  - Clear bit 1 of edFlags (modified=false)
- [ ] Save As (Ctrl+Shift+S)
  - Prompt for new filename
  - Update preserved filename
  - Clear bit 1 of edFlags
- [ ] Modified flag tracking
  - Set bit 1 of edFlags on any buffer change
  - Display "*" in status when bit 1 set
  - Check bit 1 on exit/new/load, prompt if set

## Text Selection (View-owned)
- [ ] Selection state tracking
  ```hopper
  const uint vwSelectionStart = 0x9B;  // Logical position
  const uint vwSelectionEnd = 0x9D;    // Logical position
  // Bit 0 of edFlags = selection active
  ```
- [ ] Shift+Arrow keys for selection
- [ ] Parse VT100 modifier sequences (ESC[1;2X)
- [ ] Highlight selected text during render
- [ ] Select All (Ctrl+A) - set bit 0 of edFlags
- [ ] Clear bit 0 of edFlags on navigation without Shift

## Clipboard Operations  
- [ ] Dynamic clipboard buffer allocation
- [ ] Copy (Ctrl+C) - selection to clipboard
- [ ] Cut (Ctrl+X) - copy then delete selection
- [ ] Paste (Ctrl+V) - insert at cursor

## Find/Replace
- [ ] Find (Ctrl+F) - prompt for search text
- [ ] Find Next (F3 or Ctrl+G)
- [ ] Replace (Ctrl+H) - prompt for replacement
- [ ] Replace All option

## Undo/Redo
- [ ] Single-level alternating undo buffer
- [ ] Undo (Ctrl+Z) - check bits 2-3 of edFlags:
  - If 01 (can_undo): perform undo, set to 10 (can_redo)
  - If 10 (can_redo): perform redo, set to 01 (can_undo)
  - If 00 (empty): do nothing
- [ ] Clear undo (set bits 2-3 to 00) on new edit after undo

## Editing Enhancements
- [ ] Tab inserts 4 spaces
- [ ] Auto-indent on Enter (match previous line indentation)

## Status Bar
- [ ] Show filename (or "[New File]")
- [ ] Show "*" when bit 1 of edFlags set (modified)
- [ ] Show line:col position (✓ already done)

## Memory Management
- [ ] Move GapBuffer initialization from View to File operations
- [ ] Dynamic allocation for:
  - Clipboard buffer
  - Undo buffer
  - Search string buffer
  - Filename buffer



# Turbo Pascal Key Bindings

## Cursor Movement
| Command              | Turbo Pascal Key | Modern Alias |
|---------------------|------------------|--------------|
| Character left      | Ctrl+S           | ←            |
| Character right     | Ctrl+D           | →            |
| Line up             | Ctrl+E           | ↑            |
| Line down           | Ctrl+X           | ↓            |
| Word left           | Ctrl+A           | Ctrl+←       |
| Word right          | Ctrl+F           | Ctrl+→       |
| Page up             | Ctrl+R           | PgUp         |
| Page down           | Ctrl+C           | PgDn         |
| Beginning of line   | Ctrl+Q S         | Home         |
| End of line         | Ctrl+Q D         | End          |
| Beginning of file   | Ctrl+Q R         | Ctrl+Home    |
| End of file         | Ctrl+Q C         | Ctrl+End     |
| Go to block begin   | Ctrl+Q B         | -            |
| Go to block end     | Ctrl+Q K         | -            |

## Block Operations
| Command              | Turbo Pascal Key | Modern Alias |
|---------------------|------------------|--------------|
| Mark block begin    | Ctrl+K B         | -            |
| Mark block end      | Ctrl+K K         | -            |
| Mark single word    | Ctrl+K T         | -            |
| Copy block          | Ctrl+K C         | Ctrl+C       |
| Move block (cut)    | Ctrl+K V         | Ctrl+X       |
| Paste               | -                | Ctrl+V       |
| Delete block        | Ctrl+K Y         | -            |
| Hide/unmark block   | Ctrl+K H         | -            |
| Select all          | -                | Ctrl+A       |
| Write block to file | Ctrl+K W         | -            |
| Read file           | Ctrl+K R         | -            |

## Delete Operations
| Command              | Turbo Pascal Key | Modern Alias |
|---------------------|------------------|--------------|
| Delete left         | Ctrl+H           | Backspace    |
| Delete character    | Ctrl+G           | Delete       |
| Delete word right   | Ctrl+T           | -            |
| Delete entire line  | Ctrl+Y           | -            |
| Delete to end of line| Ctrl+Q Y        | -            |

## File Operations
| Command              | Turbo Pascal Key | Modern Alias |
|---------------------|------------------|--------------|
| Save file           | F2, Ctrl+K S     | Ctrl+S       |
| Save and continue   | Ctrl+K S         | -            |
| Save and exit       | Ctrl+K D         | -            |
| Exit                | Ctrl+K X         | Alt+X        |
| Quit (no save)      | Ctrl+K Q         | -            |
| Open file           | F3               | Ctrl+O       |
| New file            | -                | Ctrl+N       |

## Find/Replace
| Command              | Turbo Pascal Key | Modern Alias |
|---------------------|------------------|--------------|
| Find text           | Ctrl+Q F         | Ctrl+F       |
| Find and replace    | Ctrl+Q A         | Ctrl+H       |
| Repeat last find    | Ctrl+L           | F3, Ctrl+G   |

## Miscellaneous
| Command              | Turbo Pascal Key | Modern Alias |
|---------------------|------------------|--------------|
| Show help           | F1, Ctrl+J       | F1           |
| Insert tab (4 spaces)| Tab             | Tab          |
| Undo/Redo toggle    | Alt+Backspace    | Ctrl+Z       |
| Insert control char | Ctrl+P           | -            |



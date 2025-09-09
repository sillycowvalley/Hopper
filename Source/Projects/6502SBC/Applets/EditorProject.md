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
```

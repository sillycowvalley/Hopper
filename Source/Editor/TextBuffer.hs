unit TextBuffer
{
    uses "/Source/System/Diagnostics"
    
    uses "JournalEntry"
    
    // JournalRecord < JournalEntry >
    
    // TextBuffer 
    //    textBuffer: < string >
    //    undo: < JournalRecord >
    //    redo: < JournalRecord >
    //    currentRecord: JournalEntry
    
    <string> textBuffer;
    < < <string, uint > > > undo;
    < < <string, uint > > > redo;
    < <string, uint> > currentRecord;
    
    Initialize()
    {
        <string> tb;
        textBuffer = tb; // initialize with correct type
        textBuffer.Append(""); // start with a single empty line
        
        undo = JournalEntry.NewJournalRecord(); // < < JournalEntry > >
        redo = JournalEntry.NewJournalRecord(); // < < JournalEntry > >
        currentRecord = JournalEntry.New();
    }
    
    
    AddFile(file textFile)
    {
        loop
        {
            string ln = textFile.ReadLine();
            if (ln.Length == 0)
            {
                if (!textFile.IsValid())
                {
                    break;
                }
            }
            textBuffer.Append(ln);
        }
        uint count = GetLineCount();
        if (count == 0)
        {
            // empty file
            textBuffer.Append(""); // start with a single empty line
        }
    }
    
    string GetLine(uint lineIndex)
    {
        string ln = textBuffer.GetItem(lineIndex);
        return ln;
    }
    uint GetLineCount()
    {
        uint count = textBuffer.Count;
        return count;
    }
    uint GetLineLength(uint lineIndex)
    {
        string ln = GetLine(lineIndex);
        return ln.Length;
    }
    
    bool CanUndo()
    {
        return undo.Count != 0;
    }
    bool CanRedo()
    {
        return redo.Count != 0;
    }
    
    Clear()
    {
        textBuffer.Clear();
        ClearUndo();
    }
    ClearUndo()
    {
        undo = JournalEntry.NewJournalRecord(); // clear
        redo = JournalEntry.NewJournalRecord(); // clear
    }
    
    < <string, uint > > GetUndo()
    {
        uint undoLength = undo.Count;
        < <string, uint > > rec = undo[undoLength-1];
        undo.Remove(undoLength-1);
        redo.Append(rec);
        return rec;
    }
    < <string, uint > > GetRedo()
    {
        uint redoLength = redo.Count;
        < <string, uint > > rec = redo[redoLength-1];
        redo.Remove(redoLength-1);
        undo.Append(rec);
        return rec;
    }
    
    StartJournal()
    {
        currentRecord = JournalEntry.New();
    }
    EndJournal()
    {
        if (currentRecord.Count != 0)
        {
            undo.Append(currentRecord);
            redo = JournalEntry.NewJournalRecord(); // clear
        }
        Editor.UpdateTitle(); // file has been modified
    }
    Insert(ref uint x, ref uint y, char c)
    {
        <string, uint> entry;
        entry["x"] = x;
        entry["y"] = y;
        entry["c"] = uint(c);
        entry["t"] = 0; // insertion
        currentRecord.Append(entry);
        
        string yLine = textBuffer.GetItem(y);
        uint yL = yLine.Length;
        if (c == Char.EOL) // special case - inserting a line
        {
            string remainder;
            if (yL > x)
            {
                remainder = yLine.Substring(x);
                textBuffer.SetItem(y, yLine.Substring(0, x));
            }
            if ((y+1 == textBuffer.Count) || (textBuffer.Count == 0))
            {
                textBuffer.Append(remainder);
            }
            else
            {
                textBuffer.Insert(y+1, remainder);
            }
            x = 0;
            y++;
        }    
        else
        {
            if (x == yL)
            {
                textBuffer.SetItem(y, yLine.Append(c));
            }
            else
            {
                textBuffer.SetItem(y, yLine.InsertChar(x, c));
            }
            x++;
        }
    }
    
    bool Delete(ref uint x, ref uint y)
    {   
        bool success = false;
        string yLine = textBuffer.GetItem(y);
        uint yL = yLine.Length;
        if (x < yL)
        {
            <string, uint> entry;
            entry["x"] = x;
            entry["y"] = y;
            char c = yLine.GetChar(x);
            entry["c"] = uint(c);
            entry["t"] = 1; // deletion
            currentRecord.Append(entry);
            
            string remainder = yLine.Substring(x+1);
            yLine = yLine.Substring(0, x);
            yLine = yLine + remainder;
            textBuffer.SetItem(y, yLine);
            success = true;
        }
        else if (y < textBuffer.Count -1)
        {
            <string, uint> entry;
            entry["x"] = x;
            entry["y"] = y;
            entry["c"] = 0x0A; // special case - deleting the end of line
            entry["t"] = 1; // deletion
            currentRecord.Append(entry);
            
            string nextLine = textBuffer.GetItem(y+1);
            yLine = yLine + nextLine;
            textBuffer.SetItem(y, yLine);
            textBuffer.Remove(y+1);
            success = true;
        }
        return success;
    }
 
#ifdef PROFILER 
    SaveJournalEntry(file diag, < <string, uint > > record)
    {
        string eol = (Char.EOL).ToString();
        foreach (var item in record)
        {
            uint i = item["c"];
            char c = char(i);
            string ln = "x=" + (item["x"]).ToString() + ", " + "y=" + (item["y"]).ToString()+ ", " + "c='" + c + "' t=" + (item["t"]).ToString();
            File.Append(diag, ln + eol);
        }
    }
    SaveJournalEntries(file diag, < < <string, uint > > > currentRecords)
    {
        string eol = (Char.EOL).ToString();
        foreach (var record in currentRecords)
        {
            foreach (var item in record)
            {
                uint i = item["c"];
                char c = char(i);
                string ln = "x=" + (item["x"]).ToString() + ", " + "y=" + (item["y"]).ToString()+ ", " + "c='" + c + "' t=" + (item["t"]).ToString();
                File.Append(diag, ln + eol);
            }
        }
    }
    
    
    SaveDiagnostics(string path)
    {
        if (File.Exists(path))
        {
            File.Delete(path);
        }
        file diag = File.Create(path);
        string eol = (Char.EOL).ToString();
        File.Append(diag, "textBuffer:" + eol);
        foreach (var ln in textBuffer)
        {
            File.Append(diag, ln + eol);
        }
        File.Append(diag, eol);
        
        File.Append(diag, "currentRecord:" + eol);
        SaveJournalEntry(diag, currentRecord);
        File.Append(diag, eol);
        
        File.Append(diag, "undo:" + eol);
        SaveJournalEntries(diag, undo);
        File.Append(diag, eol);
        
        File.Append(diag, "redo:" + eol);
        SaveJournalEntries(diag, redo);
        File.Append(diag, eol);
        
        File.Flush(diag);
    }
#endif    
}

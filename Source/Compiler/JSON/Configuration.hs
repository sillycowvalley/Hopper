unit Configuration
{
    uses "JSON"
    
    <string> ReadSymbols(<string> appSymbols, string appKey)
    {
        // <string> appSymbols was populated by -d option/s on the command line
        
        // convert list to dictionary
        <string, bool> allSymbols;
        foreach (var appSymbol in appSymbols)
        {
            allSymbols[appSymbol] = true;
        }
        string configSymbolsPath = Path.MakeOptions("Configuration.options");
        loop
        {
            if (!File.Exists(configSymbolsPath))
            {
                break;
            }
            <string, variant> dict;
            if (JSON.Read(configSymbolsPath, ref dict))
            {
                <string, string> symbols;
                if (dict.Contains(appKey))
                {
                    symbols = dict[appKey];
                }
                foreach (var kv in symbols)
                {
                    // if a symbol is found in .options that is 
                    // "true" and not already in cliSymbols,
                    // it is defined (ignore "false" symbols)
                    if (kv.value == "true")
                    {
                        allSymbols[kv.key] = true;
                    }
                }
            }
            configSymbolsPath = configSymbolsPath.Replace(".options", ".local");
            if (!File.Exists(configSymbolsPath))
            {
                break;
            }
            dict.Clear();
            if (JSON.Read(configSymbolsPath, ref dict))
            {
                <string, string> symbols;
                if (dict.Contains(appKey))
                {
                    symbols = dict[appKey];
                }
                foreach (var kv in symbols)
                {
                    // if a symbol is found in .local it
                    // is set or cleared (override if exists)
                    allSymbols[kv.key] = (kv.value == "true");
                }
            }
            break;
        }
        // build the results: only symbols that are still defined:
        <string> resultSymbols;
        foreach (var kv in allSymbols)
        {
            if (kv.value)
            {
                resultSymbols.Append(kv.key);
            }
        }
        return resultSymbols;
    }
}

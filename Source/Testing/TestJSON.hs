program TestJSON
{
//#define MCU
    uses "/Source/System/System"
    uses "/Source/System/IO"
    uses "/Source/System/Serialize"
    
    // https://w3.cs.jmu.edu/lam2mo/cs430_2015_01/pa-json_parser.html
    
    PrintFailed(string message)
    {
        //Trace = false;
        WriteLn("  " + message);
        Diagnostics.Die(0x0B); // system failure / internal error
    }
    TestManual()
    {
        WriteLn("TestManual");
        loop
        {
            <string> lst;
            lst.Append("a");
            lst.Append("b");
            lst.Append("c");
            <string, variant> testDict;
            
            delegate null; // uninitialized delegate
            
            testDict["string"] = "teststring containing \"string\"";
            testDict["number"] = 10;
            testDict["bool"] = true;
            testDict["list"] = lst;
            testDict["null"] = null;
            
            
            string jsonPath = "/temp/testpath.json";
            
            string bjson;
            if (!Serialize.TryToJSON(testDict, ref bjson))
            {
                PrintFailed("Serialize failed");    
                break;
            }
            
            file jf = File.Create(jsonPath);
            jf.Append(bjson);
            jf.Flush();
            
            string ajson;
            if (!File.TryReadAllText(jsonPath, ref ajson))
            {
                PrintFailed("Read failed");    
            }
            
            if (bjson != ajson)
            {
                PrintFailed("File roundtrip failed");
                break;
            }
            <string, variant> returnDict;
            if (!Serialize.TryFromJSON(ajson, returnDict))
            {
                PrintFailed("Deserialize failed");    
                break;
            }
            
            uint success = 0;
            foreach (var kv in returnDict)
            {
                if (!testDict.Contains(kv.key))
                {
                    success = 1;
                    break;
                }
                switch (kv.key)
                {
                    case "string":
                    {
                        string str = kv.value;
                        if (str != "teststring containing \"string\"")
                        {
                            success = 2;
                            break;
                        }
                    }
                    case "number":
                    {
                        uint number = kv.value;
                        if (number != 10)
                        {
                            success = 3;
                            break;
                        }
                    }
                    case "bool":
                    { 
                        bool boolean = kv.value;
                        if (boolean != true)
                        {
                            success = 4;
                            break;
                        }
                    }
                    case "null":
                    { 
                        if ((uint(kv.value) != 0) || (typeof(kv.value) != delegate))
                        {
                            success = 10;
                            break;
                        }
                    }
                    case "list":
                    {
                        <string> la = kv.value;
                        if (la.Count != 3)
                        {
                            success = 5;
                            break;
                        }
                        if (la[0] != "a")
                        {
                            success = 6;
                            break;
                        }
                        if (la[1] != "b")
                        {
                            success = 7;
                            break;
                        }
                        if (la[2] != "c")
                        {
                            success = 8;
                            break;
                        }
                    }
                    default:
                    {
                        success = 9;
                        break;
                    }
                }
            }
            if (success != 0)
            {
                PrintFailed("Round trip failed: " + success.ToString());
                break;
            }
            break;
        } // loop
    }
    TestTime()
    {
        WriteLn("TestTime");
        <string, variant> time;
        string content = "{\"abbreviation\":\"NZDT\",\"client_ip\":\"206.83.102.48\",\"datetime\":\"2023-11-15T15:43:00.302348+13:00\",\"day_of_week\":3,\"day_of_year\":319,\"dst\":true,\"dst_from\":\"2023-09-23T14:00:00+00:00\",\"dst_offset\":3600,\"dst_until\":\"2024-04-06T14:00:00+00:00\",\"raw_offset\":43200,\"timezone\":\"Pacific/Auckland\",\"unixtime\":1700016180,\"utc_datetime\":\"2023-11-15T02:43:00.302348+00:00\",\"utc_offset\":\"+13:00\",\"week_number\":46}";
        if (!Serialize.TryFromJSON(content, time))
        {
            PrintFailed("Time deserialize failed");    
        }
    }
    TestSunset()
    {
        WriteLn("TestSunset");
        <string, variant> sunset;
        string content = "{\"results\":{\"sunrise\":\"2023-11-14T16:57:02+00:00\",\"sunset\":\"2023-11-15T07:25:27+00:00\",\"solar_noon\":\"2023-11-15T00:11:15+00:00\",\"day_length\":52105,\"civil_twilight_begin\":\"2023-11-14T16:27:25+00:00\",\"civil_twilight_end\":\"2023-11-15T07:55:05+00:00\",\"nautical_twilight_begin\":\"2023-11-14T15:48:43+00:00\",\"nautical_twilight_end\":\"2023-11-15T08:33:47+00:00\",\"astronomical_twilight_begin\":\"2023-11-14T15:05:36+00:00\",\"astronomical_twilight_end\":\"2023-11-15T09:16:54+00:00\"},\"status\":\"OK\"}";
        if (!Serialize.TryFromJSON(content, sunset))
        {
            PrintFailed("Sunset deserialize failed");    
        }
    }
    TestFile()
    {
        WriteLn("TestFile");
        loop
        {
            // https://w3.cs.jmu.edu/lam2mo/cs430_2015_01/pa-json_parser.html
            string contentBefore;
            if (!File.TryReadAllText("/source/testing/example.json", ref contentBefore))
            {
                PrintFailed("Read failed");
                break;
            }
            <string, variant> jsonObject;
            if (!Serialize.TryFromJSON(contentBefore, jsonObject))
            {
                PrintFailed("Deserialize failed");    
                break;
            }
            string contentAfter;
            if (!Serialize.TryToJSON(jsonObject, ref contentAfter))
            {
                PrintFailed("Serialize failed");    
                break;
            }
            contentBefore = contentBefore.Replace(char(0x09), ' ');
            contentBefore = contentBefore.Replace(char(0x0D), ' ');
            contentBefore = contentBefore.Replace(char(0x0A), ' ');
            contentBefore = contentBefore.Replace(" ", "");
            contentAfter  = contentAfter.Replace(" ", "");
            
            //WriteLn();
            //WriteLn(contentBefore);
            //WriteLn();
            //WriteLn(contentAfter);
                
            if (contentAfter.Length != contentBefore.Length)
            {
                PrintFailed("Roundtrip failed");    
            }
           
            break;
        } // loop
    }
    
    {
        TestManual();
#ifndef MCU        
        TestFile(); // need to upload the test file to MCU ..
#endif
        TestTime();
        TestSunset();
        
        WriteLn();
        WriteLn("TestJSON Success");
    }
}

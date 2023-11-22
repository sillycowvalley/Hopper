unit HRHttpClient
{
    bool GetRequest(uint url, ref uint content)
    {
        return External.HttpClientGetRequest(url, ref content);
    }
}

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace HopperNET
{
    public class WebServer
    {
        public int Port = 8080;

        private HttpListener listener;
        private Dictionary<String, Dictionary<String, UInt16>> handlers = new Dictionary<string, Dictionary<string, ushort>>();
        HttpListenerResponse response = null;

        // From Admin console on Windows 10: (keep in mind that "everyone" is locale specific : "jeder" in German for example)
        //
        //   netsh http add urlacl url="http://*:8080/" user=everyone
        //


        //   https://gist.github.com/define-private-public/d05bc52dd0bed1c4699d49e2737e80e7
        // Good Arduino code:
        // https://create.arduino.cc/projecthub/instanceofMA/fetch-the-easiest-way-to-make-http-requests-on-your-arduino-65bb24

        static WebServer _this_ = null;
        Runtime runtimeForISRCallbacks;
        Hopper hopper;
        bool started = false;

        public WebServer(Runtime runtime, Hopper hopper)
        {
            this.runtimeForISRCallbacks = runtime;
            this.hopper = hopper;
            URL = String.Empty;
            Method = String.Empty;
            Arguments = new Dictionary<string, string>();
            _this_ = this;
        }

        public void Start()
        {
            listener = new HttpListener();
            listener.Prefixes.Add("http://*:" + Port.ToString() + "/");
            listener.Start();
            Receive();
            started = true;
        }

        public void Stop()
        {
            if (null != listener)
            {
                listener.Stop();
            }
        }

        private void Receive()
        {
            listener.BeginGetContext(new AsyncCallback(ListenerCallback), listener);
        }

        public static string Method { get; private set; }
        public static string URL { get; private set; }
        public static Dictionary<string, string> Arguments { get; private set; }

        public static void RegisterHandler(string url, string httpMethod, UInt16 methodIndex)
        {
            if (!_this_.started)
            {
                _this_.Start();
            }
            if (httpMethod != "GET")
            {
                throw new NotImplementedException();
            }
            if (!_this_.handlers.ContainsKey(httpMethod))
            {
                _this_.handlers[httpMethod] = new Dictionary<string, ushort>();
            }
            _this_.handlers[httpMethod][url] = methodIndex;
        }

        private void ListenerCallback(IAsyncResult result)
        {
            if (!hopper.Exiting && listener.IsListening)
            {
                var context = listener.EndGetContext(result);
                var request = context.Request;

                string url = request.Url.ToString();
                if (url.Contains("favicon.ico"))
                {
                    // ignore it
                }
                else
                {
                    // yes, they are static but if they are consumed during the On(..) handler below it will work
                    URL = url;
                    Method = request.HttpMethod;

                    Arguments = new Dictionary<string, string>();
                    // initialize Arguments from URL
                    foreach (string key in request.QueryString.Keys)
                    {
                        string modkey = key;
                        string value = request.QueryString[modkey];
                        if (modkey == null)
                        {
                            modkey = value;
                            value = String.Empty;
                        }
                        Arguments[modkey] = value;
                    }

                    // Obtain a response object.
                    response = context.Response;
                    response.StatusCode = 404;

                    string urlPath = request.RawUrl;
                    int qIndex = urlPath.IndexOf('?');
                    if (qIndex != -1)
                    {
                        urlPath = urlPath.Substring(0, qIndex);
                    }

                    UInt16 handlerIndex = 0;
                    int matchLength = 0;
                    if (handlers.ContainsKey(Method))
                    {
                        // match the appropriate handler
                        Dictionary<String, UInt16> callBacks = handlers[Method];
                        foreach (KeyValuePair<string, UInt16> kv in callBacks)
                        {
                            if (kv.Key == "*")
                            {
                                if (handlerIndex == 0) // better than nothing
                                {
                                    handlerIndex = kv.Value;
                                    matchLength = 0; // anything else is better than this
                                }
                            }
                            else if (kv.Key == urlPath)
                            {
                                handlerIndex = kv.Value; // always favour exact match
                                matchLength = kv.Key.Length;
                            }
                            else if (kv.Key.EndsWith("*"))
                            {
                                string key = kv.Key.Substring(0, kv.Key.Length - 1);
                                if (urlPath.StartsWith(key))
                                {
                                    if (matchLength < key.Length) // better match?
                                    {
                                        handlerIndex = kv.Value;
                                        matchLength = key.Length;
                                    }
                                }
                            }
                        }
                    }
                    if (handlerIndex == 0)
                    {
                        Send(404, "text/plain", "");
                    }
                    else
                    {
                        //call the 'handlerIndex' handler as an interrupt service routine
                        runtimeForISRCallbacks.ISR(handlerIndex);
                    }
                    response = null;
                }

                Receive();
            }
        }

        public static void Send(ushort statusCode, string contentType, string content)
        {
            byte[] buffer = System.Text.Encoding.UTF8.GetBytes(content);
            // Get a response stream and write the response to it.
            _this_.response.ContentLength64 = buffer.Length;
            _this_.response.StatusCode = statusCode;
            _this_.response.ContentType = contentType;
            System.IO.Stream output = _this_.response.OutputStream;
            output.Write(buffer, 0, buffer.Length);
            // You must close the output stream.
            output.Close();

        }

        public static void ClearHandlers()
        {
            _this_.handlers.Clear();
        }
    }

    public class HopperHttpClient
    {
        public static bool GetResponse(string url, ref string responseString)
        {
            bool success = false;
            try
            {
                using (var client = new HttpClient())
                {
                    var response = client.GetAsync(url).GetAwaiter().GetResult();
                    if (response.IsSuccessStatusCode)
                    {
                        var responseContent = response.Content;
                        responseString = responseContent.ReadAsStringAsync().GetAwaiter().GetResult();
                        success = true;
                    }
                }
            }
            catch (HttpRequestException /*ex*/)
            {
                //int why = 0;
            }
            catch (Exception /*ex*/)
            {
                //int why = 0;
            }
            return success;
        }
    }
}

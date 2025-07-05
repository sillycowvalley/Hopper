using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Globalization;
using HopperNET;

using Terminal.Gui.App;
using Terminal.Gui.Configuration;
using Terminal.Gui.Drawing;
using Terminal.Gui.Input;
using Terminal.Gui.ViewBase;
using Terminal.Gui.Views;

namespace HopperRuntime
{

#nullable enable

    struct TextCell
    {
        public bool isChanged;
        public char c;
        public Terminal.Gui.Drawing.Attribute colors;
    }

    public class TextGridView : View
    {
        int cx;
        int cy;
        ushort rows;
        ushort columns;

        TextCell[,] ? textCells;

        
        public TextGridView()
        {
            Width = Dim.Fill();
            Height = Dim.Fill();
            CanFocus = true;
            CursorVisibility = Terminal.Gui.Drivers.CursorVisibility.Default;

            Initialized += TextGridView_Initialized;
            FrameChanged += TextGridView_FrameChanged;

            KeyDown += TextGridView_KeyDown;
            MouseEvent += TextGridView_MouseEvent;
        }

        private void TextGridView_FrameChanged(object? sender, EventArgs<Rectangle> e)
        {
            if ((textCells == null) && (Frame.Width != 0))
            {
                columns = (ushort)Frame.Width;
                rows = (ushort)Frame.Height;
                textCells = new TextCell[columns, rows];
            }
        }

        private void TextGridView_Initialized(object? sender, EventArgs e)
        {
            Program.Running = true;
            NeedsDraw = true;
        }

        public void DrawChar(ushort x, ushort y, char c, Terminal.Gui.Drawing.Color foreground, Terminal.Gui.Drawing.Color background)
        {
            var colors = new Terminal.Gui.Drawing.Attribute(foreground, background);

            if (null != textCells)
            {
                lock (textCells)
                {
                    if ((textCells[x, y].c != c) || (textCells[x, y].colors != colors))
                    {
                        textCells[x, y].isChanged = true;
                        textCells[x, y].c = c;
                        textCells[x, y].colors = colors;
                    }
                }
            }
        }

        internal void ScrollUp()
        {
            if (null != textCells)
            {
                lock (textCells)
                {
                    for (int y = 1; y < rows; y++)
                    {
                        for (int x = 0; x < columns; x++)
                        {
                            textCells[x, y - 1].colors = textCells[x, y].colors;
                            textCells[x, y - 1].c = textCells[x, y].c;
                        }
                    }
                    var colors = new Terminal.Gui.Drawing.Attribute(Terminal.Gui.Drawing.Color.Black, Terminal.Gui.Drawing.Color.Black);
                    for (int x = 0; x < columns; x++)
                    {
                        textCells[x, rows - 1].colors = colors;
                        textCells[x, rows - 1].c = ' ';
                    }
                    NeedsDraw = true;
                }
            }
        }

        protected override bool OnDrawingContent(DrawContext? context)
        {
            if (null != textCells)
            {
                lock (textCells)
                {
                    var currentColour = new Terminal.Gui.Drawing.Attribute(Terminal.Gui.Drawing.Color.Black, Terminal.Gui.Drawing.Color.Black);
                    for (int y = 0; y < rows; y++)
                    {
                        for (int x = 0; x < columns; x++)
                        {
                            if (currentColour != textCells[x, y].colors)
                            {
                                currentColour = textCells[x, y].colors;
                                this.SetAttribute(currentColour);
                            }
                            this.Move(x, y);
                            this.AddRune(textCells[x, y].c);
                        }
                    }
                }
            }
            return true;
        }
        

        private void TextGridView_KeyDown(object? sender, Terminal.Gui.Input.Key key)
        {
            if (null != Keyboard)
            {
                Keyboard.PushToKeyboardBuffer(key);
            }
        }
        

        private void TextGridView_MouseEvent(object? sender, MouseEventArgs mouseArgs)
        {
            if (null != Keyboard)
            {
                Keyboard.PushClick(mouseArgs);
            }
        }


        internal ushort Columns { get { return columns; } }
        internal ushort Rows    { get { return rows; } }

        public Keyboard? Keyboard { get; internal set; }

        public bool CursorVisible
        {
            get
            {
                return CursorVisibility != Terminal.Gui.Drivers.CursorVisibility.Invisible;
            }
            internal set
            {
                CursorVisibility = value ? Terminal.Gui.Drivers.CursorVisibility.Default : Terminal.Gui.Drivers.CursorVisibility.Invisible;
            }
        }


        public override Point? PositionCursor()
        {
            //Trace.Write("\nPC: " + cx.ToString() + "," + cy.ToString());
            return new Point(cx, cy);
        }

        internal void SetCursor(ushort x, ushort y)
        {
            cx = x;
            cy = y;
            //Trace.Write("\nSC: " + x.ToString() + "," + y.ToString());
        }
        internal void UpdateTextCells()
        {
            NeedsDraw = true;
        }
        
    }
#nullable restore

    public class Program
    {
        public static List<String> Arguments { get; private set; } = new List<String>();
        public static String HexePath { get; private set; } = String.Empty;
        public static bool Running { get; set; }

        public static BackgroundWorker worker;
        public static TextGridView textView;
        public static Toplevel window;

        private static void Worker_DoWork(object sender, DoWorkEventArgs e)
        {
            while (!Running)
            {
            }
            
            Runtime runtime = new Runtime(textView);
            textView.Keyboard = runtime.Keyboard;

            runtime.Screen.Clear();
            runtime.Screen.ShowCursor(true);
            

            HopperSystem hopperSystem = new HopperSystem();
            int exitCode = hopperSystem.Load(HexePath, Arguments, runtime, false);
            if (exitCode == 0)
            {
                ushort setError = 0;
                exitCode = hopperSystem.Execute(runtime, ref setError, false);
            }
            Running = false;
            Application.RequestStop();
        }

        public static int Main(string[] args)
        {
            CultureInfo.CurrentCulture = new CultureInfo("en-US");
            PowerShellAnsiEnabler.EnableAnsiSupport();
            ParseArguments(args);
            
            Application.Init(null, "v2");
            
            window = new Toplevel();
            textView = new TextGridView
            {
                Height = Dim.Fill(),
                Width = Dim.Fill(),
            };
            window.Title = "";
            window.BorderStyle = LineStyle.None;
            window.Add(textView);

            window.SizeChanging += Window_SizeChanging;
            
            worker = new BackgroundWorker();
            worker.DoWork += Worker_DoWork;
            worker.RunWorkerAsync();

            Application.Run(window);
            Application.Shutdown();
            
            return 0;

        }

        private static void Window_SizeChanging(object sender, SizeChangedEventArgs e)
        {
            // TODO : deal with top level window resize
        }

        private static void ParseArguments(string[] args)
        {
            string exePath = Environment.ProcessPath;

            /*
            FileVersionInfo fileVersionInfo = FileVersionInfo.GetVersionInfo(exePath);
            string versionName = fileVersionInfo.FileVersion;
            if (!String.IsNullOrEmpty(versionName))
            {
                Console.Title = exePath + " [" + versionName + "]";
            }
            */
            HopperPath.InitializeFolders();

            // Check for trace flag and find program file
            string programFile = null;
            foreach (string arg in args)
            {
                if (programFile == null)
                {
                    programFile = arg;
                }
                else
                {
                    Arguments.Add(arg);
                }
            }

            if (String.IsNullOrEmpty(programFile))
            {
                programFile = "Shell"; // default to launching the Hopper shell
            }
            HexePath = programFile;
        }
    }
}
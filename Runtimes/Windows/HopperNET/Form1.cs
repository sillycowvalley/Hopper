using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Reflection;
using System.Windows.Forms;

namespace HopperNET
{
    public partial class Hopper : Form, IHopper
    {
        WebServer httpServer;

        public Hopper()
        {
            InitializeComponent();
            this.ClientSize = new System.Drawing.Size((int)Console.CanvasWidth, (int)Console.CanvasHeight);

            string versionName =  FileVersionInfo.GetVersionInfo(Assembly.GetExecutingAssembly().Location).ProductVersion;

            this.Text += " [" + versionName + "]";
            

            Console = new Console(this);
            Screen = new Screen(Console);
            Keyboard = new Keyboard(Console);
            Runtime = new Runtime(Screen, Keyboard);
            
            Timer timer = new Timer();
            timer.Interval = 500; // 500ms
            timer.Tick += Timer_Tick;
            timer.Start();

            httpServer = new WebServer(Runtime, this);
        }

        private void Timer_Tick(object sender, EventArgs e)
        {
            Console.ServiceCursor();
        }
        
        BackgroundWorker worker;
        
        public Keyboard Keyboard { get; set; }
        
        public Console Console { get; set; }
        
        public Screen  Screen { get; set; }
        
        public Runtime Runtime { get; set; }
        
        public bool Exiting { get; set; }

        public void HopperInvalidate()
        {
            Invalidate();
        }

        private void Worker_DoWork(object sender, DoWorkEventArgs e)
        {
            Screen.Clear();
            Console.ShowCursor(true);
            ushort setError = 0;
            Runtime.Load("SHELL", new List<String>());
            int exitCode = Runtime.Execute(ref setError, true);
            Exiting = true;
            System.Windows.Forms.Application.Exit();
        }

       

        private void Hopper_Paint(object sender, PaintEventArgs e)
        {
            Console.Redraw(e.Graphics);
        }

        protected override void OnPaintBackground(PaintEventArgs pevent)
        {
            // do nothing
        }

        private void Hopper_MouseUp(object sender, MouseEventArgs e)
        {
            Keyboard.PushClick(e, false);
        }

        private void Hopper_MouseDown(object sender, MouseEventArgs e)
        {
            Keyboard.PushClick(e, true);
        }
        
        protected override void OnMouseWheel(MouseEventArgs e)
        {
            Keyboard.PushScroll(e);
        }

        private void Hopper_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyData == Keys.F10)
            {
                e.SuppressKeyPress = true;
            }
            if (e.KeyData == Keys.F12)
            {
                //Console.Save(@"D:\Repos\Maker\Z80\Hopper\HS\Temp\Console.txt");
                Console.Save(@"C:\Repos\Hopper\Temp\Console.txt");
            }

            //TODO
            /*
            Key modifiers = Key.NoKey;
            Keys currentModifierState = Hopper.ModifierKeys;

            if ((currentModifierState & Keys.Shift) != 0)
            {
                modifiers |= Key.Shift;
            }
            if ((currentModifierState & Keys.Control) != 0)
            {
                modifiers |= Key.Control;
            }
            if ((currentModifierState & Keys.Alt) != 0)
            {
                modifiers |= Key.Alt;
            }

            var key = Keyboard.TranslateCtrlToHopperKey(e.KeyCode, modifiers);

            e.SuppressKeyPress = e.Handled = Keyboard.PushToKeyboardBuffer(key); 
            */
        }

        private void Hopper_KeyPress(object sender, KeyPressEventArgs e)
        {
            Keys currentModifierState = Hopper.ModifierKeys;
            Key modifiers = Key.NoKey;
            if ((currentModifierState & Keys.Shift) != 0)
            {
                modifiers |= Key.Shift;
            }
            if ((currentModifierState & Keys.Control) != 0)
            {
                modifiers |= Key.Control;
            }
            if ((currentModifierState & Keys.Alt) != 0)
            {
                modifiers |= Key.Alt;
            }

            var key = Keyboard.TranslateAsciiToHopperKey(e.KeyChar, modifiers);

            e.Handled = Keyboard.PushToKeyboardBuffer(key); 
        }

        private void Hopper_KeyDown(object sender, KeyEventArgs e)
        {          
            if (e.KeyData == Keys.F10)
            {
                e.SuppressKeyPress = true;
            }

            Key modifiers = Key.NoKey;
            Keys currentModifierState = Hopper.ModifierKeys;

            if ((currentModifierState & Keys.Shift) != 0)
            {
                modifiers |= Key.Shift;
            }
            if ((currentModifierState & Keys.Control) != 0)
            {
                modifiers |= Key.Control;
            }
            if ((currentModifierState & Keys.Alt) != 0)
            {
                modifiers |= Key.Alt;
            }

            var key = Keyboard.TranslateCtrlToHopperKey(e.KeyCode, modifiers);

            e.SuppressKeyPress = e.Handled = Keyboard.PushToKeyboardBuffer(key);
        }

        delegate bool hasClipboardText();
        
        public bool HasClipboardText()
        {
            if (this.InvokeRequired)
            {
                hasClipboardText safeHas = HasClipboardText;
                return (bool)this.Invoke(safeHas);
            }
            else
            {
                return Clipboard.ContainsText(TextDataFormat.Text);
            }
        }

        delegate void setClipboardText(string s);
        
        public void SetClipboardText(string text)
        {
            if (this.InvokeRequired)
            {
                setClipboardText safeSet = SetClipboardText;
                this.Invoke(safeSet, text);
            }
            else
            {
                Clipboard.SetText(text, TextDataFormat.Text);
            }
        }
        
        delegate string getClipboardText();
        
        public string GetClipboardText() 
        {
            if (this.InvokeRequired)
            {
                getClipboardText safeGet = GetClipboardText;
                return (string)this.Invoke(safeGet);
            }
            else
            {
                string clipboardText = Clipboard.GetText(TextDataFormat.Text);
                return clipboardText;
            }
        }

        private void Hopper_FormClosing(object sender, FormClosingEventArgs e)
        {
            if ((Hopper.ModifierKeys == Keys.Alt) || (Hopper.ModifierKeys == Keys.F4)) // <-- this looks like a mistake?
            {
                e.Cancel = true; // we don't want <Alt><F4> to close the main window (by mistake)
                return;
            }
            Exiting = true;
            httpServer.Stop();
            Console.ConsoleFree();
            Keyboard.Free();
            Settings.Default.Maximized = (WindowState == FormWindowState.Maximized);
            Settings.Default.Save();
        }

        private void Hopper_Load(object sender, EventArgs e)
        {
            if (!HopperPath.InitializeFolders(this))
            {
                this.Close();
            }
            else
            {
                if (Settings.Default.Maximized)
                {
                    WindowState = FormWindowState.Maximized;
                }
                worker = new BackgroundWorker();
                worker.DoWork += Worker_DoWork;
                worker.RunWorkerAsync();
            }
        }

        private void Hopper_ClientSizeChanged(object sender, EventArgs e)
        {   
            uint rows    = (uint)(this.ClientRectangle.Height / Console.TextCellHeight);
            uint columns = (uint)(this.ClientRectangle.Width  / Console.TextCellWidth);
            if ((rows != Console.Rows) || (columns != Console.Columns))
            {
                Console.Rows    = rows;
                Console.Columns = columns;
                Console.Resize();
                HopperInvalidate();
            }
        }


    }
}

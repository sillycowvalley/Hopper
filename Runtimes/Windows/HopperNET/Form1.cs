using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using static System.Net.Mime.MediaTypeNames;

namespace HopperNET
{
    public partial class Hopper : Form, IHopper
    {
        WebServer httpServer;
        public Hopper()
        {
            InitializeComponent();
            this.ClientSize = new System.Drawing.Size((int)Console.CanvasWidth, (int)Console.CanvasHeight);
            
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

        private void Hopper_Load(object sender, EventArgs e)
        {
            worker = new BackgroundWorker();
            worker.DoWork += Worker_DoWork;
            worker.RunWorkerAsync();
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

        private void Hopper_FormClosing(object sender, FormClosingEventArgs e)
        {
            if ((Control.ModifierKeys == Keys.Alt) || (Control.ModifierKeys == Keys.F4))
            {
                e.Cancel = true;
                return;
            }
            Exiting = true;
            httpServer.Stop();
            Console.ConsoleFree();
            Keyboard.Free();
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
                string consoleDumpFile = HopperPath.ToWindowsPath("/Temp/Console.txt");
                Console.Save(consoleDumpFile);
            }
        }

        private void Hopper_KeyDown(object sender, KeyEventArgs e)
        {
            Keyboard.PushKey(e);
            if (e.KeyData == Keys.F10)
            {
                e.SuppressKeyPress = true;
            }
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
    }
}

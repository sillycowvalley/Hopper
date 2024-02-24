using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace HopperNET
{
    struct ConsoleCell
    {
        public char character;
        public uint foreColor;
        public uint backColor;
    };

    public class Console
    {
        IHopper hopper;

        const int uiScale = 1;

        
        
        const int hAdvance = 8;
        const int vAdvance = 16;

        const uint textCellWidth = hAdvance;
        const uint textCellHeight = vAdvance;

        const uint colorMatrixGreen = 0x7F7;

        int buffering = 0;
        //bool cursorBlinking = true;

        // location where the blinky thing needs to happen
        uint hardwareCursorX;
        uint hardwareCursorY;
        bool cursorToggle = false;
        bool cursorVisible = false;

        // indices are y x width + x
        ConsoleCell[] consoleBuffer = null;
        ConsoleCell[] consoleCache = null;
        
        Bitmap graphicsBitmap = null;
        bool graphicsMode = false;

        const ushort invertColour = 0xF000;

        // for videos
        //static uint textColumns = 100;
        //static uint textRows = 30;

        static uint textColumns = 140;
        static uint textRows = 44;


        static public uint TextCellWidth  { get { return textCellWidth; } }
        static public uint TextCellHeight { get { return textCellHeight; } }

        static public uint Columns
        {
            get { return textColumns; }
            set { textColumns = value; }
        }
        static public uint Rows
        {
            get { return textRows; }
            set { textRows = value; }
        }


        static public uint CanvasWidth // width in pixels
        {
            get { return textColumns * textCellWidth * uiScale; }
        }
        static public uint CanvasHeight // height in pixels
        {
            get { return textRows * textCellHeight * uiScale; }
        }
        public static bool FlipVertical { get; internal set; }
        public void ClearPixels(ushort colour)
        {
            Color color = ToColor(colour);
            lock (graphicsBitmap)
            {
                using (Graphics graphics = Graphics.FromImage(graphicsBitmap))
                {
                    SolidBrush brush = new SolidBrush(color);
                    graphics.FillRectangle(brush, 0, 0, CanvasWidth, CanvasHeight);
                }
            }
            graphicsMode = (colour != 0x000);
        }
        public void SetPixel(ushort x, ushort y, ushort colour)
        {
            lock (graphicsBitmap)
            {
                if (colour == invertColour)
                {
                    Color color = graphicsBitmap.GetPixel(x, y);
                    color = Color.FromArgb((byte)~color.R, (byte)~color.G, (byte)~color.B);
                    graphicsBitmap.SetPixel(x, y, color);
                }
                else
                {
                    Color color = ToColor(colour);
                    graphicsBitmap.SetPixel(x, y, color);
                }
            }
            graphicsMode = true;
        }
        public void GraphicsClear(ushort colour)
        {
            ClearPixels(colour);
            if (colour == 0x000)
            {
                Reset();
                hopper.HopperInvalidate();
            }
        }

        void lineLow(int x0, int y0, int x1, int y1, ushort colour)
        {
            int dx = x1 - x0;
            int dy = y1 - y0;
            int yi = 1;
            if (dy < 0)
            {
                yi = -1;
                dy = -dy;
            }
            int d = (2 * dy) - dx;
            int y = y0;
            for (int x = x0; x <= x1; x++)
            {
                SetPixel((ushort)x, (ushort)y, colour);
                if (d > 0)
                {
                    y = y + yi;
                    d = d + (2 * (dy - dx));
                }
                else
                {
                    d = d + 2 * dy;
                }
            }
        }

        void lineHigh(int x0, int y0, int x1, int y1, ushort colour)
        {
            int dx = x1 - x0;
            int dy = y1 - y0;
            int xi = 1;
            if (dx < 0)
            {
                xi = -1;
                dx = -dx;
            }
            int d = (2 * dx) - dy;
            int x = x0;
            for (int y = y0; y <= y1; y++)
            {
                SetPixel((ushort)x, (ushort)y, colour);
                if (d > 0)
                {
                    x = x + xi;
                    d = d + (2 * (dx - dy));
                }
                else
                {
                    d = d + 2 * dx;
                }
            }
        }

        internal void Line(ushort x0, ushort y0, ushort x1, ushort y1, ushort colour)
        {
            if (x0 == x1) { VerticalLine(x0, y0, x1, y1, colour); }
            else if (y0 == y1) { HorizontalLine(x0, y0, x1, y1, colour); }
            else if (Math.Abs((int)(y1) - (int)(y0)) < Math.Abs((int)(x1) - (int)(x0)))
            {
                if (x0 > x1)
                {
                    lineLow((int)(x1), (int)(y1), (int)(x0), (int)(y0), colour);
                }
                else
                {
                    lineLow((int)(x0), (int)(y0), (int)(x1), (int)(y1), colour);
                }
            }
            else
            {
                if (y0 > y1)
                {
                    lineHigh((int)(x1), (int)(y1), (int)(x0), (int)(y0), colour);
                }
                else
                {
                    lineHigh((int)(x0), (int)(y0), (int)(x1), (int)(y1), colour);
                }
            }
        }

        internal void HorizontalLine(ushort x1, ushort y1, ushort x2, ushort y2, ushort colour)
        {
            if (x1 > x2)
            {
                ushort t = x1;
                x1 = x2;
                x2 = t;
            }

            Color color = ToColor(colour);
            lock (graphicsBitmap)
            {
                BitmapData bmpData = graphicsBitmap.LockBits(new Rectangle(0, 0, graphicsBitmap.Width, graphicsBitmap.Height), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
                unsafe
                {
                    byte* p = (byte*)(void*)bmpData.Scan0;
                    p += bmpData.Stride * y1 + x1 * 4;
                    for (ushort x = x1; x <= x2; x++)
                    {
                        if (colour == invertColour)
                        {
                            p[0] = (byte)(~p[0]);
                            p[1] = (byte)(~p[1]);
                            p[2] = (byte)(~p[2]);
                        }
                        else
                        {
                            p[0] = color.B;
                            p[1] = color.G;
                            p[2] = color.R;
                        }
                        p += 4;
                    }
                }
                graphicsBitmap.UnlockBits(bmpData);
                graphicsMode = true;
            }
        }

        internal void VerticalLine(ushort x1, ushort y1, ushort x2, ushort y2, ushort colour)
        {
            if (y1 > y2)
            {
                ushort t = y1;
                y1 = y2;
                y2 = t;
            }
            Color color = ToColor(colour);
            lock (graphicsBitmap)
            {
                BitmapData bmpData = graphicsBitmap.LockBits(new Rectangle(0, 0, graphicsBitmap.Width, graphicsBitmap.Height), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb);
                unsafe
                {
                    byte* p = (byte*)(void*)bmpData.Scan0;
                    p += bmpData.Stride * y1 + x1 * 4;
                    for (ushort y = y1; y <= y2; y++)
                    {
                        if (colour == invertColour)
                        {
                            p[0] = (byte)(~p[0]);
                            p[1] = (byte)(~p[1]);
                            p[2] = (byte)(~p[2]);
                        }
                        else
                        {
                            p[0] = color.B;
                            p[1] = color.G;
                            p[2] = color.R;
                        }
                        p += bmpData.Stride;
                    }
                }
                graphicsBitmap.UnlockBits(bmpData);
                graphicsMode = true;
            }
        }

        internal void Rectangle(ushort x, ushort y, ushort w, ushort h, ushort colour)
        {
            HorizontalLine(x, y, (ushort)(x + w - 1), y, colour);
            HorizontalLine(x, (ushort)(y + h - 1), (ushort)(x + w - 1), (ushort)(y + h - 1), colour);
            VerticalLine(x, y, x, (ushort)(y + h - 1), colour);
            VerticalLine((ushort)(x + w - 1), y, (ushort)(x + w - 1), (ushort)(y + h - 1), colour);
        }

        internal void FillRectangle(ushort x, ushort y, ushort w, ushort h, ushort colour)
        {
            for (ushort i = y; i < y + h; i++)
            {
                HorizontalLine(x, i, (ushort)(x + w - 1), i, colour);
            }
        }
        internal void Circle(ushort x, ushort y, ushort r, ushort colour)
        {
            Color color = ToColor(colour);
            lock (graphicsBitmap)
            {
                using (Graphics graphics = Graphics.FromImage(graphicsBitmap))
                {
                    Pen pen = new Pen(color);
                    graphics.DrawEllipse(pen, x, y, r, r);
                }
                graphicsMode = true;
            }
        }

        internal void FillCircle(ushort x, ushort y, ushort r, ushort colour)
        {
            Color color = ToColor(colour);
            lock (graphicsBitmap)
            {
                using (Graphics graphics = Graphics.FromImage(graphicsBitmap))
                {
                    SolidBrush brush = new SolidBrush(color);
                    graphics.FillEllipse(brush, x, y, r, r);
                }
                graphicsMode = true;
            }
        }

        

        
        public uint CursorX { get { return hardwareCursorX; } } // TODO
        public uint CursorY { get { return hardwareCursorY; } } // TODO

        Color ToColor(uint c444)
        {
            if (c444 == 0)
            {
                return Color.Black;
            }
            byte r = (byte)(c444 >> 8);
            byte g = (byte)((c444 >> 4) & (0x0F));
            byte b = (byte)(c444 & 0x0F);
            if (r <= 7)
            {
                r = (byte)(r << 4);
            }
            else
            {
                r = (byte)((r << 4) | 0x0F);
            }
            if (g <= 7)
            {
                g = (byte)(g << 4);
            }
            else
            {
                g = (byte)((g << 4) | 0x0F);
            }
            if (b <= 7)
            {
                b = (byte)(b << 4);
            }
            else
            {
                b = (byte)((b << 4) | 0x0F);
            }
            return Color.FromArgb(r, g, b);
        }

        static public byte TransformXtoC(uint x)
        {
            uint width = CanvasWidth;
            uint columns = Columns;
            uint cellWidth = width / columns;
            uint cellWidth2 = cellWidth / 2;

            uint result = (x + cellWidth2) / cellWidth;
            return (byte)result;
        }
        static public byte TransformYtoR(uint y)
        {
            uint height = CanvasHeight;
            uint rows = Rows;
            uint cellHeight = height / rows;

            uint result = y / cellHeight;
            return (byte)result;
        }

        
        public void Redraw(Graphics graphics)
        {
            if (hopper.Exiting || (consoleBuffer == null) || (consoleCache == null))
            {
                return; // exiting
            }
            if (buffering > 0)
            {
                return; // wait for Resume(..)
            }
            uint width = CanvasWidth;
            uint height = CanvasHeight;
            uint columns = Columns;
            uint rows = Rows;

            uint cellWidth = width / columns;
            uint cellHeight = height / rows;

            if (graphicsMode)
            {
                lock (graphicsBitmap)
                {
                    graphics.DrawImage(graphicsBitmap, 0, 0);
                }
                return;
            }

            int cursorCellIndex = (int)(CursorX + CursorY * textColumns);
            int cursorCellIndexL = -1;
            if ((CursorX == Columns) && (CursorX > 0))
            {
                cursorCellIndexL = (int)((CursorX - 1) + CursorY * textColumns);
            }

            Font font = new Font("Consolas", cellWidth + 2, FontStyle.Regular, GraphicsUnit.Point);

            for (uint r = 0; r < rows; r++)
            {
                for (uint c = 0; c < columns; c++)
                {
                    uint cellIndex = c + r * columns;
                    ConsoleCell consoleCell = consoleBuffer[cellIndex];
                    ConsoleCell cacheCell = consoleCache[cellIndex];

                    if (cellIndex == cursorCellIndex)
                    {
                        // always draw cursor
                    }
                    else if (cellIndex == cursorCellIndexL)
                    {
                        // or the cell left of the cursor at the right edge
                    }
                    else if (
                               (cacheCell.character == consoleCell.character)
                            && (cacheCell.backColor == consoleCell.backColor)
                            && (cacheCell.foreColor == consoleCell.foreColor)
                            )
                    {
                        continue;
                    }

                    cacheCell.character = consoleCell.character;
                    cacheCell.backColor = consoleCell.backColor;
                    cacheCell.foreColor = consoleCell.foreColor;
                    consoleCache[cellIndex] = cacheCell;

                    RectangleF cell = new RectangleF((int)(c * cellWidth), (int)(r * cellHeight), (int)cellWidth, (int)cellHeight);

                    SolidBrush backgroundBrush = new SolidBrush(ToColor(consoleCell.backColor));
                    graphics.FillRectangle(backgroundBrush, cell);

                    if (consoleCell.character != ' ')
                    {
                        SolidBrush foregroundBrush = new SolidBrush(ToColor(consoleCell.foreColor));
                        char ch = consoleCell.character;
                        if (ch == 0x95)
                        {
                            ch = (char)0x2022; // hack for the breakpoint red dots in the debugger
                        }
                        graphics.DrawString(ch.ToString(), font, foregroundBrush, cell, StringFormat.GenericTypographic);
                    }
                }
            }
            if (cursorVisible && cursorToggle)
            {
                if (cursorCellIndex < consoleBuffer.Length)
                {
                    ConsoleCell consoleCell = consoleBuffer[cursorCellIndex];
                    Color color = ToColor(consoleCell.backColor);
                    color = Color.FromArgb((byte)~color.R, (byte)~color.G, (byte)~color.B);

                    consoleCache[cursorCellIndex].character = (char)0; // force this cell to be drawn again even if the cursor moves (like CLS)

                    int dx = 0;
                    if (CursorX == Columns)
                    {
                        dx = -1;
                    }

                    RectangleF cursor = new RectangleF((int)(CursorX * cellWidth + dx), (int)(CursorY * cellHeight), 2, (int)cellHeight);

                    SolidBrush backgroundBrush = new SolidBrush(color);
                    graphics.FillRectangle(backgroundBrush, cursor);
                }
            }
        }

        public void DrawChar(uint column, uint row, char character, uint foreColor, uint backColor, bool force = false)
        {
            uint cellIndex = column + row * textColumns;
            ConsoleCell consoleCell = consoleBuffer[cellIndex];
            //if ((consoleCell.character != character) || (consoleCell.foreColor != foreColor) || (consoleCell.backColor != backColor))
            {
                consoleCell.character = character;
                consoleCell.foreColor = foreColor;
                consoleCell.backColor = backColor;
                consoleBuffer[cellIndex] = consoleCell;
            }
        }

        public void Redraw()
        {
            for (uint row = 0; row < textRows; row++)
            {
                for (uint column = 0; column < textColumns; column++)
                {
                    uint cellIndex = column + row * textColumns;
                    ConsoleCell consoleCell = consoleBuffer[cellIndex];
                    char c = consoleCell.character;
                    if (c != 0)
                    {
                        consoleCell.character = (char)0; // force a redraw
                        DrawChar(column, row, c, consoleCell.foreColor, consoleCell.backColor);
                        consoleBuffer[cellIndex] = consoleCell;
                    }
                }
            }
        }
        


        public void EnterBuffering(bool reset)
        {
            if (reset)
            {
                buffering = 0;
            }
            buffering++;
        }

        public void ExitBuffering(bool isInteractive)
        {
            if (buffering > 0)
            {
                buffering--;
            }
            if (buffering == 0)
            {
                hopper.HopperInvalidate();

                // We are entering keystrokes
                // which means message pumping is being done in ReadHopperKey()
                if (!isInteractive)
                {
                    //Application.DoEvents();
                }
            }
        }

        public Console(IHopper hopper)
        {
            this.hopper = hopper;
            hardwareCursorX = 0;
            hardwareCursorY = 0;

            Resize();
        }
        public void Resize()
        {
            consoleCache = new ConsoleCell[textColumns * textRows];
            consoleBuffer = new ConsoleCell[textColumns * textRows];

            graphicsBitmap = new Bitmap((int)CanvasWidth, (int)CanvasHeight);

            ClearPixels(0x000);
            Reset();
        }

        public void Reset()
        {
            for (int i = 0; i < textColumns * textRows; i++)
            {
                consoleCache[i].character = (char)0;
            }
        }

        public IHopper Hopper { get { return hopper; } }

        public void ConsoleFree()
        {
            // avoid crashes on exit
            //consoleCache = null;
            //consoleBuffer = null;
        }

        public void SetCursor(uint x, uint y)
        {
            if ((x < 0) || (y < 0) || (x > textColumns) || (y >= textRows))
            {
                return; //Die("Failed in ConsoleSetCursor - coordinates out of range");
            }
            hardwareCursorX = x;
            hardwareCursorY = y;
        }

        public void WriteCharacter(uint x, uint y, char character, uint foreColor, uint backColor)
        {
            if ((x < 0) || (y < 0) || (x >= (short)textColumns) || (y >= textRows))
            {
                return; //Die("Failed in ConsoleWriteCharacter - coordinates out of range");
            }

            if (null == consoleBuffer)
            {
                return;
            }

            EnterBuffering(false);

            DrawChar(x, y, character, foreColor, backColor);

            uint bufferIndex = y * textColumns + x;
            consoleBuffer[bufferIndex].character = character;
            consoleBuffer[bufferIndex].foreColor = foreColor;
            consoleBuffer[bufferIndex].backColor = backColor;

            ExitBuffering(false);
        }

        public void ServiceCursor()
        {
            lock (hopper)
            {
                if (cursorVisible && !hopper.Exiting && !graphicsMode)
                {
                    //ConsoleDrawCursor(ConsoleGetCursorX(), ConsoleGetCursorY(), cursorToggle);
                    cursorToggle = !cursorToggle;
                    hopper.HopperInvalidate();
                    //Application.DoEvents();
                }
            }
        }
        public void ShowCursor(bool show)
        {
            lock (hopper)
            {
                if (!hopper.Exiting)
                {
                    if (!show && cursorVisible && cursorToggle)
                    {
                        // cursorToggle == false means cell background colour
                        // cursorToggle == true means inverted cell background colour
                        cursorToggle = false;
                        hopper.HopperInvalidate();
                    }
                    cursorVisible = show;
                    cursorToggle = show;
                    //ConsoleDrawCursor(ConsoleGetCursorX(), ConsoleGetCursorY(), cursorToggle);
                }
            }
        }
        public bool CursorVisible 
        { 
            get { return cursorVisible; } 
        }
        public void ScrollUp()
        {
            if (null == consoleBuffer)
            {
                return;
            }
            EnterBuffering(false);
            // scroll up one line
            for (uint y = 0; y < textRows - 1; y++)
            {
                for (uint x = 0; x < textColumns; x++)
                {
                    uint destinationBufferIndex = y * textColumns + x;
                    uint sourceBufferIndex = (y + 1) * textColumns + x;
                    consoleBuffer[destinationBufferIndex] = consoleBuffer[sourceBufferIndex];
                }
            }
            // clear the bottom line
            for (uint x = 0; x < textColumns; x++)
            {
                uint bufferIndex = (textRows - 1) * textColumns + x;
                consoleBuffer[bufferIndex].character = ' ';
                consoleBuffer[bufferIndex].foreColor = 0x000;
                consoleBuffer[bufferIndex].backColor = 0x000;
            }
            // redraw
            // first line
            for (uint x = 0; x < textColumns; x++)
            {
                uint bufferIndex = 0 * textColumns + x;
                WriteCharacter(x, 0, consoleBuffer[bufferIndex].character, consoleBuffer[bufferIndex].foreColor, consoleBuffer[bufferIndex].backColor);
            }
            // subsequent lines
            for (uint y = 1; y < textRows; y++)
            {
                for (uint x = 0; x < textColumns; x++)
                {
                    uint previousBufferIndex = (y - 1) * textColumns + x;
                    uint bufferIndex = y * textColumns + x;
                    if ((consoleBuffer[previousBufferIndex].character == consoleBuffer[bufferIndex].character)
                        && (consoleBuffer[previousBufferIndex].foreColor == consoleBuffer[bufferIndex].foreColor)
                        && (consoleBuffer[previousBufferIndex].backColor == consoleBuffer[bufferIndex].backColor))
                    {
                        // no change (often blank)
                    }
                    else
                    {
                        WriteCharacter(x, y, consoleBuffer[bufferIndex].character, consoleBuffer[bufferIndex].foreColor, consoleBuffer[bufferIndex].backColor);
                    }
                }
            }
            ExitBuffering(false);
        }

        internal void Save(string txtPath)
        {
            StringBuilder sb = new StringBuilder();

            for (uint y = 0; y < textRows; y++)
            {
                for (uint x = 0; x < textColumns; x++)
                {
                    uint bufferIndex = y * textColumns + x;
                    string line =        x.ToString() +
                                  ", " + y.ToString() +
                                  ", 0x" + ((uint)consoleBuffer[bufferIndex].character).ToString("X2") +
                                  ", 0x" + ((uint)consoleBuffer[bufferIndex].foreColor).ToString("X3") +
                                  ", 0x" + ((uint)consoleBuffer[bufferIndex].backColor).ToString("X3");
                    sb.AppendLine(line);
                }
            }

            File.WriteAllText(txtPath, sb.ToString());
        }

        
    }
}

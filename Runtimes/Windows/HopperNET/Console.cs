using System;
using System.Collections.Generic;
using System.Drawing;
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

        const uint textColumns = 140;
        const uint textRows = 44;
        
        // for videos
        //const uint textColumns = 100;
        //const uint textRows = 30;
        
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


        static public uint CanvasWidth // width in pixels
        {
            get { return textColumns * textCellWidth * uiScale; }
        }
        static public uint CanvasHeight // height in pixels
        {
            get { return textRows * textCellHeight * uiScale; }
        }

        static public uint Columns
        {
            get { return textColumns; }
        }
        static public uint Rows
        {
            get { return textRows; }
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

            uint width = CanvasWidth;
            uint height = CanvasHeight;
            uint columns = Columns;
            uint rows = Rows;

            uint cellWidth = width / columns;
            uint cellHeight = height / rows;

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
                    else if ((cacheCell.character == consoleCell.character)
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
        public void Reset()
        {
            for (int i = 0; i < textColumns * textRows; i++)
            {
                consoleCache[i].character = (char)0;
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

            consoleCache = new ConsoleCell[textColumns * textRows];
            consoleBuffer = new ConsoleCell[textColumns * textRows];
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
            if (cursorVisible && !hopper.Exiting)
            {
                //ConsoleDrawCursor(ConsoleGetCursorX(), ConsoleGetCursorY(), cursorToggle);
                cursorToggle = !cursorToggle;
                hopper.HopperInvalidate();
                //Application.DoEvents();
            }
        }
        public void ShowCursor(bool show)
        {
            if (!hopper.Exiting)
            {
                cursorVisible = show;
                cursorToggle = show;
                //ConsoleDrawCursor(ConsoleGetCursorX(), ConsoleGetCursorY(), cursorToggle);
            }
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

program HopperTetris
{
    #define MCU
    
#ifdef MCU    
    uses "/Source/Library/Devices/WSPicoLCD144"
    uses "/Source/Library/Fonts/Hitachi5x7"
#else
    uses "/Source/System/Screen"
#endif    
        
    uses "/Source/System/System"
    uses "Input"
    uses "GameGrid"
    uses "Pieces"
    uses "DisplayHelper"

    Hopper()
    {
#ifdef MCU
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize Waveshare Pico-LCD-1.44");
            return;
        }
#endif          
        GameGrid.Initialize();
        Pieces.Initialize();

        loop
        {
            Input.Update();

            // Clear the current piece from the grid
            byte[Pieces.PieceSize * Pieces.PieceSize] shape = Pieces.GetCurrentShape();
            for (byte i = 0; i < Pieces.PieceSize; i++)
            {
                for (byte j = 0; j < Pieces.PieceSize; j++)
                {
                    if (shape[i + j * Pieces.PieceSize] != 0)
                    {
                        GameGrid.SetCell(Pieces.currentX + i, Pieces.currentY + j, Colour.Black);
                    }
                }
            }

            // Handle input
            if (Input.Left && Pieces.IsValidPosition(Pieces.currentX - 1, Pieces.currentY, shape))
            {
                Pieces.currentX--;
            }
            if (Input.Right && Pieces.IsValidPosition(Pieces.currentX + 1, Pieces.currentY, shape))
            {
                Pieces.currentX++;
            }
            if (Input.Down && Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY + 1, shape))
            {
                Pieces.currentY++;
            }
            if (Input.Space)
            {
                Pieces.Rotate();
                shape = Pieces.GetCurrentShape();
                if (!Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY, shape))
                {
                    // Revert rotation if it's not valid
                    Pieces.Rotate();
                    Pieces.Rotate();
                    Pieces.Rotate();
                    shape = Pieces.GetCurrentShape();
                }
            }
            if (Input.Exit)
            {
                break;
            }

            // Move piece down
            if (Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY + 1, shape))
            {
                Pieces.currentY++;
            }
            else
            {
                // Place the piece and generate a new one
                Pieces.PlaceCurrentShape();
                Input.Clear();  // Clear the input buffer when spawning a new piece

                // Check for game over
                Pieces.Initialize();
                if (!Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY, Pieces.GetCurrentShape()))
                {
                    GameGrid.Render();
                    DisplayHelper.DrawText(GameGrid.Width + 2, 5, "Game Over", Colour.White, Colour.Black);
                    break;
                }

                // Check for full rows
                for (byte row = 0; row < GameGrid.Height; row++)
                {
                    if (GameGrid.IsRowFull(row))
                    {
                        GameGrid.ClearRow(row);
                    }
                }
            }

            // Render the current piece on the grid
            uint currentColor = DisplayHelper.GetColorForShape(Pieces.currentShape);
            for (byte i = 0; i < Pieces.PieceSize; i++)
            {
                for (byte j = 0; j < Pieces.PieceSize; j++)
                {
                    if (shape[i + j * Pieces.PieceSize] != 0)
                    {
                        GameGrid.SetCell(Pieces.currentX + i, Pieces.currentY + j, currentColor);
                    }
                }
            }

            // Render the entire grid
            GameGrid.Render();

            // Delay to control game speed
            Time.Delay(250);
        }
    }
}


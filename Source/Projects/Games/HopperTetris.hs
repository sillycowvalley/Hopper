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
        PinISRDelegate buttonDelegate = ButtonISR;
        if (!DeviceDriver.Begin(buttonDelegate))
        {
            DisplayHelper.DebugLog("Failed to initialize Waveshare Pico-LCD-1.44");
            return;
        }
#endif          
        GameGrid.Initialize();
        Pieces.Initialize();

        loop
        {
            Input.Update();

            // Clear the current piece from the grid
            DisplayHelper.DebugLog("Clearing current piece");
            Pieces.ClearCurrentShape();

            // Handle input
            if (Input.Left && Pieces.IsValidPosition(Pieces.currentX - 1, Pieces.currentY, Pieces.GetCurrentShape()))
            {
                Pieces.currentX--;
                DisplayHelper.DebugLog("Moving piece left");
            }
            if (Input.Right && Pieces.IsValidPosition(Pieces.currentX + 1, Pieces.currentY, Pieces.GetCurrentShape()))
            {
                Pieces.currentX++;
                DisplayHelper.DebugLog("Moving piece right");
            }
            if (Input.Space)
            {
                Pieces.Rotate();
                if (!Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY, Pieces.GetCurrentShape()))
                {
                    // Revert rotation if it's not valid
                    Pieces.Rotate();
                    Pieces.Rotate();
                    Pieces.Rotate();
                    DisplayHelper.DebugLog("Rotation invalid, reverting");
                }
                else
                {
                    DisplayHelper.DebugLog("Piece rotated");
                }
            }
            if (Input.Down)
            {
                DisplayHelper.DebugLog("Dropping piece");
                while (Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY + 1, Pieces.GetCurrentShape()))
                {
                    Pieces.ClearCurrentShape();
                    Pieces.currentY++;
                }
                Pieces.DrawCurrentShape();
                DisplayHelper.DebugLog("Piece dropped");
            }
            if (Input.Exit)
            {
                DisplayHelper.DebugLog("Exiting game");
                break;
            }

            // Move piece down
            if (Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY + 1, Pieces.GetCurrentShape()))
            {
                Pieces.currentY++;
                DisplayHelper.DebugLog("Piece moved down");
            }
            else
            {
                // Place the piece and generate a new one
                Pieces.PlaceCurrentShape();
                DisplayHelper.DebugLog("Piece placed, generating new piece");
                Input.Clear();  // Clear the input buffer when spawning a new piece

                // Check for game over
                Pieces.Initialize();
                if (!Pieces.IsValidPosition(Pieces.currentX, Pieces.currentY, Pieces.GetCurrentShape()))
                {
                    GameGrid.Render();
                    DisplayHelper.DrawText(GameGrid.Width + 2, 5, "Game Over", Colour.White, Colour.Black);
                    DisplayHelper.DebugLog("Game Over");
#ifdef MCU
                    loop
                    {
                    }                  
#endif
                    break;
                }

                // Check for full rows
                for (byte row = 0; row < GameGrid.Height; row++)
                {
                    if (GameGrid.IsRowFull(row))
                    {
                        GameGrid.ClearRow(row);
                        DisplayHelper.DebugLog("Clearing full row: " + row.ToString());
                    }
                }
            }

            // Render the current piece on the grid
            DisplayHelper.DebugLog("Rendering current piece");
            Pieces.DrawCurrentShape();

            // Render the entire grid
            GameGrid.Render();

            // Delay to control game speed
            Time.Delay(250);
        }
    }
}


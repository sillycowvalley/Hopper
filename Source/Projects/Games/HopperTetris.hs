program HopperTetris
{
    uses "/Source/System/System"
    uses "Input"
    uses "GameGrid"
    uses "Pieces"

    Hopper()
    {
        GameGrid.Initialize();
        Pieces.Initialize();
        
        loop
        {
            Input.Update();
            
            // Clear the current piece from the grid
            byte[] shape = Pieces.GetCurrentShape();
            for (byte i = 0; i < Pieces.PieceSize; i++)
            {
                for (byte j = 0; j < Pieces.PieceSize; j++)
                {
                    if (shape[i + j * Pieces.PieceSize] != 0)
                    {
                        GameGrid.SetCell(Pieces.currentX + i, Pieces.currentY + j, false);
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
                Pieces.Initialize();
                
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
            shape = Pieces.GetCurrentShape();
            for (byte i = 0; i < Pieces.PieceSize; i++)
            {
                for (byte j = 0; j < Pieces.PieceSize; j++)
                {
                    if (shape[i + j * Pieces.PieceSize] != 0)
                    {
                        GameGrid.SetCell(Pieces.currentX + i, Pieces.currentY + j, true);
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


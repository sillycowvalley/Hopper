using System;
using System.Diagnostics;
using Terminal.Gui;
using static Terminal.Gui.View;

namespace HopperRuntimeConsole
{
    using System;
    using System.Diagnostics;
    using Terminal.Gui;

    
    public class MouseHandlerView : View
    {
        private DateTime _lastClickTime;
        private Point _lastClickPosition;

        public MouseHandlerView()
        {
            CanFocus = true;
            _lastClickTime = DateTime.Now;
            _lastClickPosition = new Point(-1, -1);
        }

        public override bool OnMouseEvent(MouseEvent mouseEvent)
        {
            var now = DateTime.Now;
            var elapsed = (now - _lastClickTime).TotalMilliseconds;

            Debug.WriteLine($"Mouse event: {mouseEvent.Flags}, Position: ({mouseEvent.X}, {mouseEvent.Y}), Elapsed: {elapsed} ms");
            
            if (mouseEvent.Flags.HasFlag(MouseFlags.Button1Clicked))
            {
                /*
                // Check for double click
                if (_lastClickPosition == new Point(mouseEvent.X, mouseEvent.Y) &&
                    elapsed <= 500)
                {
                    Debug.WriteLine("Custom double-click detected");
                    DrawCharacterAt(mouseEvent.X, mouseEvent.Y, 'X');
                    _lastClickTime = DateTime.MinValue; // reset
                }
                else
                */
                {
                    Debug.WriteLine("Button1Clicked detected");
                    DrawCharacterAt(mouseEvent.X, mouseEvent.Y, '█');
                    _lastClickTime = now;
                    _lastClickPosition = new Point(mouseEvent.X, mouseEvent.Y);
                }
            }

            return true;
        }

        private void DrawCharacterAt(int x, int y, char c)
        {
            // Create a new view to display the character
            var characterView = new Label(c.ToString())
            {
                X = x,
                Y = y
            };

            // Add the view to the parent container
            this.Add(characterView);

            // Ensure the Application refreshes to show the new character
            Application.Refresh();
        }
    }
    


    class Program
    {
        static void Main(string[] args)
        {
            Application.Init();

            // Set the console size for demonstration purposes
            Console.SetWindowSize(80, 25);

            // Create a top-level view to handle key presses
            var top = Application.Top;

            var label = new Label("Hopper Runtime using Terminal.Gui:")
            {
                X = 0,
                Y = 0
            };

            top.Add(label);
            label = new Label("Press 'Q' to quit, click for █'s ..")
            {
                X = 0,
                Y = 1
            };

            top.Add(label);

            // Add the custom mouse handler view
            var mouseHandler = new MouseHandlerView
            {
                X = 0,
                Y = 1,
                Width = Dim.Fill(),
                Height = Dim.Fill()
            };

            top.Add(mouseHandler);

            // Increase the frequency of event loop processing
            Application.MainLoop.AddTimeout(TimeSpan.FromMilliseconds(50), (_) =>
            {
                return true; // Keep processing
            });

            // Key press event handler
            top.KeyPress += (e) =>
            {
                HandleKeyPress(e);
                e.Handled = true;
            };

            // Run the application (this will block until Application.RequestStop is called)
            Application.Run();
        }

        static void HandleKeyPress(KeyEventEventArgs e)
        {
            // Example key handling logic
            switch (e.KeyEvent.Key)
            {
                case Key.Q:
                case Key.q:
                    Application.RequestStop();
                    break;
                default:
                    Debug.WriteLine($"Key pressed: {e.KeyEvent.Key}");
                    break;
            }
        }

        static void DrawCharacterAt(int x, int y, char c)
        {
            // Create a new view to display the character
            var characterView = new Label(c.ToString())
            {
                X = x,
                Y = y
            };

            // Add the view to the top-level container
            Application.Top.Add(characterView);

            // Ensure the Application refreshes to show the new character
            Application.Refresh();
        }
    }
}

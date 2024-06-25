using System;
using System.Globalization;
using System.Windows.Forms;

namespace HopperNET
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            CultureInfo.CurrentCulture = new CultureInfo("en-US");

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            HopperPath.InitializeFolders();
            Application.Run(new Hopper());
        }
    }
}


namespace HopperNET
{
    partial class Hopper
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Hopper));
            this.SuspendLayout();
            // 
            // Hopper
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.BackColor = System.Drawing.Color.Black;
            this.ClientSize = new System.Drawing.Size(1046, 516);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.KeyPreview = true;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Hopper";
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.Text = "Hopper .NET Runtime";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Hopper_FormClosing);
            this.Load += new System.EventHandler(this.Hopper_Load);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.Hopper_Paint);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Hopper_KeyDown);
            this.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.Hopper_KeyPress);
            this.KeyUp += new System.Windows.Forms.KeyEventHandler(this.Hopper_KeyUp);
            this.MouseDown += new System.Windows.Forms.MouseEventHandler(this.Hopper_MouseDown);
            this.MouseUp += new System.Windows.Forms.MouseEventHandler(this.Hopper_MouseUp);
            this.ResumeLayout(false);

        }

        #endregion
    }
}


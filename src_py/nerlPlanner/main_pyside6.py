"""
Modern Nerlplanner Application
PySide6-based GUI for distributed neural network configuration

This is the updated main entry point for the Nerlplanner application,
migrated from PySimpleGUI to PySide6 for enhanced user experience.
"""

import sys
import os

# Add the current directory to Python path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, current_dir)

# Set environment variable for GUI framework
os.environ['QT_AUTO_SCREEN_SCALE_FACTOR'] = '1'

try:
    from ui.main_window import main
    
    if __name__ == "__main__":
        # Initialize and run the modern PySide6 application
        main()
        
except ImportError as e:
    print(f"Import Error: {e}")
    print("Please ensure PySide6 is installed: pip install PySide6")
    sys.exit(1)
except Exception as e:
    print(f"Application Error: {e}")
    sys.exit(1)

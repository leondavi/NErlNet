"""
Nerlplanner Main Entry Point
Modernized version with PySide6 support and PySimpleGUI fallback

This version automatically detects available GUI frameworks and runs
the appropriate version. Preserves all original functionality while
providing enhanced user experience with PySide6.
"""

import sys
import os

# Add the current directory to Python path for imports
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, current_dir)

def check_gui_frameworks():
    """Check which GUI frameworks are available"""
    has_pyside6 = False
    has_pysimplegui = False
    
    try:
        import PySide6
        has_pyside6 = True
    except ImportError:
        pass
    
    try:
        import PySimpleGUI
        has_pysimplegui = True
    except ImportError:
        pass
    
    return has_pyside6, has_pysimplegui

def run_modern_version():
    """Run the modern PySide6 version"""
    try:
        from ui.main_window import main
        main()
        return True
    except Exception as e:
        print(f"Error running modern version: {e}")
        return False

def run_legacy_version():
    """Run the legacy PySimpleGUI version"""
    try:
        # Import and run the original main_original.py
        print("Running in legacy PySimpleGUI mode...")
        
        # Import the backed-up original main
        import importlib.util
        spec = importlib.util.spec_from_file_location("main_original", 
                                                     os.path.join(current_dir, "main_original.py"))
        main_original = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(main_original)
        
        return True
        
    except Exception as e:
        print(f"Error running legacy version: {e}")
        return False

if __name__ == "__main__":
    # Print welcome message
    print("=== Nerlplanner - Neural Erlang Network Planner ===")
    
    # Check available GUI frameworks
    has_pyside6, has_pysimplegui = check_gui_frameworks()
    
    if has_pyside6:
        print("Starting with modern PySide6 interface...")
        success = run_modern_version()
        
        if not success and has_pysimplegui:
            print("Falling back to legacy PySimpleGUI interface...")
            run_legacy_version()
            
    elif has_pysimplegui:
        print("Starting with legacy PySimpleGUI interface...")
        print("Consider installing PySide6 for enhanced experience: pip install PySide6")
        run_legacy_version()
        
    else:
        print("No GUI framework available!")
        print("Please install either:")
        print("  - PySide6 (recommended): pip install PySide6")
        print("  - PySimpleGUI (legacy): pip install PySimpleGUI")
        sys.exit(1)

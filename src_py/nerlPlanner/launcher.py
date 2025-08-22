#!/usr/bin/env python3
"""
Nerlplanner Launcher
Smart launcher that can run either the legacy PySimpleGUI version
or the modern PySide6 version based on availability and user preference
"""

import sys
import os
import argparse

# Add current directory to path
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, current_dir)

def check_gui_framework_availability():
    """Check which GUI frameworks are available"""
    frameworks = {
        'pyside6': False,
        'pysimplegui': False
    }
    
    try:
        import PySide6
        frameworks['pyside6'] = True
        print("✓ PySide6 is available")
    except ImportError:
        print("✗ PySide6 is not available")
    
    try:
        import PySimpleGUI
        frameworks['pysimplegui'] = True
        print("✓ PySimpleGUI is available")
    except ImportError:
        print("✗ PySimpleGUI is not available")
    
    return frameworks

def run_pyside6_version():
    """Run the modern PySide6 version"""
    try:
        print("Starting Nerlplanner with PySide6...")
        from ui.main_window import main
        main()
    except ImportError as e:
        print(f"Failed to import PySide6 version: {e}")
        print("Please install PySide6: pip install PySide6")
        return False
    except Exception as e:
        print(f"Error running PySide6 version: {e}")
        return False
    return True

def run_legacy_version():
    """Run the legacy PySimpleGUI version"""
    try:
        print("Starting Nerlplanner with PySimpleGUI (legacy)...")
        # Import the original main.py
        import main as legacy_main
        # The original main.py should handle its own execution
        return True
    except ImportError as e:
        print(f"Failed to import legacy version: {e}")
        print("Please install PySimpleGUI: pip install PySimpleGUI")
        return False
    except Exception as e:
        print(f"Error running legacy version: {e}")
        return False

def print_banner():
    """Print application banner"""
    banner = """
╔══════════════════════════════════════════════════════════════╗
║                      NERLPLANNER LAUNCHER                   ║
║              Neural Erlang Network Planner                  ║
║                                                              ║
║  Modern PySide6 GUI with enhanced user experience          ║
║  Legacy PySimpleGUI support for compatibility              ║
╚══════════════════════════════════════════════════════════════╝
    """
    print(banner)

def main():
    """Main launcher function"""
    parser = argparse.ArgumentParser(description='Nerlplanner GUI Launcher')
    parser.add_argument('--gui', choices=['auto', 'pyside6', 'legacy'], 
                       default='auto', help='Choose GUI framework')
    parser.add_argument('--check', action='store_true', 
                       help='Check available GUI frameworks and exit')
    parser.add_argument('--install-deps', action='store_true',
                       help='Install required dependencies')
    
    args = parser.parse_args()
    
    print_banner()
    
    if args.check:
        print("Checking available GUI frameworks...")
        frameworks = check_gui_framework_availability()
        
        if frameworks['pyside6']:
            print("✓ Recommended: PySide6 (modern, enhanced UX)")
        if frameworks['pysimplegui']:
            print("✓ Available: PySimpleGUI (legacy, compatibility)")
        
        if not any(frameworks.values()):
            print("✗ No GUI frameworks available!")
            print("Please install dependencies:")
            print("  For modern interface: pip install PySide6")
            print("  For legacy interface: pip install PySimpleGUI")
        
        return
    
    if args.install_deps:
        print("Installing dependencies...")
        import subprocess
        
        # Try to install PySide6 first (recommended)
        try:
            subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'PySide6'])
            print("✓ PySide6 installed successfully")
        except subprocess.CalledProcessError:
            print("✗ Failed to install PySide6")
            
            # Fallback to PySimpleGUI
            try:
                subprocess.check_call([sys.executable, '-m', 'pip', 'install', 'PySimpleGUI'])
                print("✓ PySimpleGUI installed as fallback")
            except subprocess.CalledProcessError:
                print("✗ Failed to install any GUI framework")
                return
        
        print("Dependencies installed. You can now run the application.")
        return
    
    # Check available frameworks
    frameworks = check_gui_framework_availability()
    
    if args.gui == 'pyside6':
        if not frameworks['pyside6']:
            print("PySide6 is not available. Please install it first:")
            print("pip install PySide6")
            return
        success = run_pyside6_version()
        
    elif args.gui == 'legacy':
        if not frameworks['pysimplegui']:
            print("PySimpleGUI is not available. Please install it first:")
            print("pip install PySimpleGUI")
            return
        success = run_legacy_version()
        
    else:  # auto mode
        # Prefer PySide6 if available, fallback to PySimpleGUI
        if frameworks['pyside6']:
            print("Auto-selecting PySide6 (recommended)")
            success = run_pyside6_version()
        elif frameworks['pysimplegui']:
            print("Auto-selecting PySimpleGUI (legacy fallback)")
            success = run_legacy_version()
        else:
            print("No GUI frameworks available!")
            print("Please install dependencies:")
            print("  Recommended: pip install PySide6")
            print("  Alternative: pip install PySimpleGUI")
            print("")
            print("Or run with --install-deps to automatically install")
            return
    
    if not success:
        print("Failed to start the application.")
        sys.exit(1)

if __name__ == "__main__":
    main()

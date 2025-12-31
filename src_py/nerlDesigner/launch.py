#!/usr/bin/env python3
"""
NerlDesigner Launcher Script
Simple script to launch the NerlDesigner application
"""

import sys
import subprocess
from pathlib import Path

def check_dependencies():
    """Check if required dependencies are installed"""
    required_packages = [
        'nicegui',
        'pydantic',
        'fastapi'
    ]
    
    missing_packages = []
    
    for package in required_packages:
        try:
            __import__(package)
        except ImportError:
            missing_packages.append(package)
    
    if missing_packages:
        print("❌ Missing required packages:")
        for package in missing_packages:
            print(f"   - {package}")
        print("\n📦 Install missing packages with:")
        print(f"   pip install {' '.join(missing_packages)}")
        return False
    
    print("✅ All required packages are installed")
    return True

def launch_designer():
    """Launch the NerlDesigner application"""
    # Add the project root to Python path
    project_root = Path(__file__).parent.parent.parent
    sys.path.insert(0, str(project_root))
    
    try:
        # Import and run the main application
        from src_py.nerlDesigner.main import main
        print("🚀 Starting NerlDesigner...")
        print("📱 Open your browser to: http://localhost:8080")
        print("⏹️  Press Ctrl+C to stop the application")
        main()
    except KeyboardInterrupt:
        print("\n👋 NerlDesigner stopped by user")
    except Exception as e:
        print(f"❌ Error starting NerlDesigner: {str(e)}")
        sys.exit(1)

def main():
    """Main launcher function"""
    print("🧠 NerlDesigner - NerlNet Web Designer")
    print("=" * 40)
    
    if not check_dependencies():
        sys.exit(1)
    
    launch_designer()

if __name__ == "__main__":
    main()
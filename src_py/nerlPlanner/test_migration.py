#!/usr/bin/env python3
"""
Test script to verify the PySide6 migration preserves critical functionality
Tests auto-generation capabilities and core module imports
"""

import sys
import os

# Add current directory to path
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, current_dir)

def test_imports():
    """Test that all critical modules can be imported"""
    print("Testing module imports...")
    
    try:
        import Definitions
        print("✓ Definitions module imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import Definitions: {e}")
        return False
    
    try:
        import Handlers
        print("✓ Handlers module imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import Handlers: {e}")
        return False
    
    try:
        import JsonElements
        print("✓ JsonElements module imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import JsonElements: {e}")
        return False
    
    try:
        from CppHeadersExporter import gen_header_worker_parameters_definitions
        print("✓ CppHeadersExporter imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import CppHeadersExporter: {e}")
        return False
    
    try:
        from ErlHeadersExporter import gen_worker_fields_hrl
        print("✓ ErlHeadersExporter imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import ErlHeadersExporter: {e}")
        return False
    
    try:
        import logger
        print("✓ Logger module imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import Logger: {e}")
        return False
    
    return True

def test_pyside6_import():
    """Test PySide6 availability"""
    print("\\nTesting PySide6 availability...")
    
    try:
        import PySide6
        from PySide6.QtWidgets import QApplication
        print("✓ PySide6 is available and functional")
        return True
    except ImportError as e:
        print(f"✗ PySide6 not available: {e}")
        return False

def test_ui_modules():
    """Test UI module imports"""
    print("\\nTesting UI modules...")
    
    try:
        from ui.main_window import NerlplannerMainWindow
        print("✓ Main window class imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import main window: {e}")
        return False
    
    try:
        from ui.dialogs import WorkerDialog, CommunicationMapDialog, ExperimentFlowDialog
        print("✓ Dialog classes imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import dialogs: {e}")
        return False
    
    try:
        from ui.migration_utils import PySide6Helper
        print("✓ Migration utilities imported successfully")
    except ImportError as e:
        print(f"✗ Failed to import migration utilities: {e}")
        return False
    
    return True

def test_auto_generation_availability():
    """Test that auto-generation functions are available"""
    print("\\nTesting auto-generation functionality...")
    
    try:
        from CppHeadersExporter import gen_header_worker_parameters_definitions
        # Test that the function exists and is callable
        if callable(gen_header_worker_parameters_definitions):
            print("✓ C++ header generation function is available")
        else:
            print("✗ C++ header generation function is not callable")
            return False
    except Exception as e:
        print(f"✗ C++ header generation test failed: {e}")
        return False
    
    try:
        from ErlHeadersExporter import gen_worker_fields_hrl
        # Test that the function exists and is callable
        if callable(gen_worker_fields_hrl):
            print("✓ Erlang header generation function is available")
        else:
            print("✗ Erlang header generation function is not callable")
            return False
    except Exception as e:
        print(f"✗ Erlang header generation test failed: {e}")
        return False
    
    return True

def test_configuration_compatibility():
    """Test configuration system compatibility"""
    print("\\nTesting configuration compatibility...")
    
    try:
        # Test that we can create basic configuration structures
        from ui.migration_utils import convert_legacy_worker_config, validate_json_configuration
        
        # Test legacy config conversion
        legacy_config = {
            'worker_name': 'test_worker',
            'input_neurons': 784,
            'output_neurons': 10,
            'hidden_neurons': '128,64',
            'learn_rate': 0.001
        }
        
        new_config = convert_legacy_worker_config(legacy_config)
        if 'name' in new_config and new_config['name'] == 'test_worker':
            print("✓ Legacy configuration conversion works")
        else:
            print("✗ Legacy configuration conversion failed")
            return False
        
        # Test JSON validation
        test_config = {
            'name': 'test_config',
            'version': '1.0',
            'devices': [],
            'workers': []
        }
        
        errors = validate_json_configuration(test_config)
        if len(errors) == 0:
            print("✓ JSON validation works")
        else:
            print(f"✗ JSON validation failed: {errors}")
            return False
            
    except Exception as e:
        print(f"✗ Configuration compatibility test failed: {e}")
        return False
    
    return True

def test_gui_framework_detection():
    """Test GUI framework detection"""
    print("\\nTesting GUI framework detection...")
    
    try:
        # Import the detection function from main
        sys.path.insert(0, current_dir)
        import main
        
        has_pyside6, has_pysimplegui = main.check_gui_frameworks()
        
        print(f"PySide6 available: {has_pyside6}")
        print(f"PySimpleGUI available: {has_pysimplegui}")
        
        if has_pyside6:
            print("✓ GUI framework detection working")
            return True
        else:
            print("✗ No GUI frameworks detected")
            return False
            
    except Exception as e:
        print(f"✗ GUI framework detection failed: {e}")
        return False

def main():
    """Run all tests"""
    print("=== Nerlplanner PySide6 Migration Test Suite ===\\n")
    
    tests = [
        ("Core Module Imports", test_imports),
        ("PySide6 Availability", test_pyside6_import),
        ("UI Module Imports", test_ui_modules),
        ("Auto-Generation Availability", test_auto_generation_availability),
        ("Configuration Compatibility", test_configuration_compatibility),
        ("GUI Framework Detection", test_gui_framework_detection)
    ]
    
    passed = 0
    total = len(tests)
    
    for test_name, test_func in tests:
        print(f"Running {test_name}...")
        try:
            if test_func():
                passed += 1
                print(f"✓ {test_name} PASSED\\n")
            else:
                print(f"✗ {test_name} FAILED\\n")
        except Exception as e:
            print(f"✗ {test_name} FAILED with exception: {e}\\n")
    
    print("=== Test Results ===")
    print(f"Passed: {passed}/{total}")
    print(f"Success Rate: {(passed/total)*100:.1f}%")
    
    if passed == total:
        print("\\n🎉 All tests passed! Migration is successful.")
        return True
    else:
        print(f"\\n⚠️  {total-passed} test(s) failed. Please review the issues above.")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)

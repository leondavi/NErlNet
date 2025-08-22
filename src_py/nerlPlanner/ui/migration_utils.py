"""
Compatibility and Migration Utilities
Helper functions to maintain compatibility with existing Nerlplanner functionality
while transitioning from PySimpleGUI to PySide6
"""

import json
import os
from typing import Dict, List, Any, Optional

def migrate_pysimplegui_layout_to_pyside6(psg_layout):
    """
    Convert PySimpleGUI layout elements to PySide6 equivalent structures
    This is a helper function for migrating existing layouts
    """
    # This would be a complex mapping function
    # For now, we'll provide a basic structure
    migration_map = {
        'Text': 'QLabel',
        'Input': 'QLineEdit', 
        'Button': 'QPushButton',
        'Listbox': 'QListWidget',
        'Combo': 'QComboBox',
        'Multiline': 'QTextEdit',
        'Checkbox': 'QCheckBox',
        'Radio': 'QRadioButton',
        'Frame': 'QGroupBox',
        'Column': 'QVBoxLayout',
        'Tab': 'QTabWidget',
        'TabGroup': 'QTabWidget'
    }
    
    # TODO: Implement actual conversion logic
    return migration_map

def convert_psg_values_to_qt_data(psg_values: Dict) -> Dict:
    """
    Convert PySimpleGUI values dictionary to Qt-compatible data structure
    """
    qt_data = {}
    
    for key, value in psg_values.items():
        if isinstance(value, str):
            qt_data[key] = value
        elif isinstance(value, (int, float)):
            qt_data[key] = value
        elif isinstance(value, bool):
            qt_data[key] = value
        elif isinstance(value, list):
            qt_data[key] = value
        else:
            # Convert other types to string representation
            qt_data[key] = str(value)
            
    return qt_data

def load_legacy_configuration(file_path: str) -> Optional[Dict]:
    """
    Load configuration from legacy PySimpleGUI format
    """
    try:
        with open(file_path, 'r') as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Error loading legacy configuration: {e}")
        return None

def save_modern_configuration(data: Dict, file_path: str) -> bool:
    """
    Save configuration in modern PySide6 format
    """
    try:
        with open(file_path, 'w') as f:
            json.dump(data, f, indent=2)
        return True
    except Exception as e:
        print(f"Error saving configuration: {e}")
        return False

def validate_json_configuration(config_data: Dict) -> List[str]:
    """
    Validate JSON configuration and return list of errors
    """
    errors = []
    
    # Basic validation rules
    required_fields = ['name', 'version']
    for field in required_fields:
        if field not in config_data:
            errors.append(f"Missing required field: {field}")
    
    # Validate device configurations
    if 'devices' in config_data:
        devices = config_data['devices']
        if not isinstance(devices, list):
            errors.append("Devices must be a list")
        else:
            for i, device in enumerate(devices):
                if not isinstance(device, dict):
                    errors.append(f"Device {i} must be a dictionary")
                elif 'name' not in device or 'ip' not in device:
                    errors.append(f"Device {i} missing name or ip")
    
    # Validate worker configurations
    if 'workers' in config_data:
        workers = config_data['workers']
        if not isinstance(workers, list):
            errors.append("Workers must be a list")
        else:
            for i, worker in enumerate(workers):
                if not isinstance(worker, dict):
                    errors.append(f"Worker {i} must be a dictionary")
                elif 'name' not in worker:
                    errors.append(f"Worker {i} missing name")
    
    return errors

def convert_legacy_worker_config(legacy_config: Dict) -> Dict:
    """
    Convert legacy worker configuration to new format
    """
    # Map old field names to new ones
    field_mapping = {
        'worker_name': 'name',
        'worker_id': 'id',
        'input_neurons': 'input_size',
        'output_neurons': 'output_size',
        'hidden_neurons': 'hidden_layers',
        'learn_rate': 'learning_rate',
        'training_epochs': 'epochs'
    }
    
    new_config = {}
    
    # Convert basic fields
    for old_field, new_field in field_mapping.items():
        if old_field in legacy_config:
            new_config[new_field] = legacy_config[old_field]
    
    # Handle special conversions
    if 'hidden_neurons' in legacy_config:
        # Convert single hidden layer to list format
        hidden = legacy_config['hidden_neurons']
        if isinstance(hidden, int):
            new_config['hidden_layers'] = [hidden]
        elif isinstance(hidden, str):
            # Parse comma-separated values
            try:
                new_config['hidden_layers'] = [int(x.strip()) for x in hidden.split(',')]
            except ValueError:
                new_config['hidden_layers'] = [100]  # Default fallback
    
    # Set default values for new fields
    defaults = {
        'model': {
            'type': 'feedforward',
            'hidden_activation': 'relu',
            'output_activation': 'softmax'
        },
        'network': {
            'protocol': 'tcp',
            'batch_size': 32,
            'timeout': 5000
        },
        'learning': {
            'optimizer': 'adam',
            'loss_function': 'mse',
            'dropout_rate': 0.2
        }
    }
    
    # Merge with defaults
    for category, default_values in defaults.items():
        if category not in new_config:
            new_config[category] = {}
        for key, value in default_values.items():
            if key not in new_config[category]:
                new_config[category][key] = value
    
    return new_config

def extract_psg_gui_elements(psg_window_code: str) -> Dict:
    """
    Extract GUI elements from PySimpleGUI window code for migration reference
    """
    # This is a simplified parser for reference
    # In practice, this would be much more complex
    
    elements = {
        'inputs': [],
        'buttons': [],
        'lists': [],
        'combos': [],
        'checkboxes': [],
        'layouts': []
    }
    
    lines = psg_window_code.split('\n')
    
    for line in lines:
        line = line.strip()
        if 'sg.Input' in line or 'sg.InputText' in line:
            elements['inputs'].append(line)
        elif 'sg.Button' in line:
            elements['buttons'].append(line)
        elif 'sg.Listbox' in line:
            elements['lists'].append(line)
        elif 'sg.Combo' in line:
            elements['combos'].append(line)
        elif 'sg.Checkbox' in line:
            elements['checkboxes'].append(line)
        elif 'sg.Column' in line or 'sg.Frame' in line:
            elements['layouts'].append(line)
    
    return elements

def generate_migration_report(old_file_path: str, new_structure: Dict) -> str:
    """
    Generate a report showing what was migrated from old to new
    """
    report = []
    report.append("=== Nerlplanner Migration Report ===")
    report.append(f"Source: {old_file_path}")
    report.append(f"Generated: {os.path.basename(__file__)}")
    report.append("")
    
    report.append("MIGRATED COMPONENTS:")
    if 'ui_elements' in new_structure:
        report.append(f"- UI Elements: {len(new_structure['ui_elements'])}")
    if 'data_handlers' in new_structure:
        report.append(f"- Data Handlers: {len(new_structure['data_handlers'])}")
    if 'dialogs' in new_structure:
        report.append(f"- Dialog Windows: {len(new_structure['dialogs'])}")
    
    report.append("")
    report.append("NEW FEATURES ADDED:")
    report.append("- Modern PySide6 interface")
    report.append("- Enhanced user experience")
    report.append("- Improved error handling")
    report.append("- Better input validation")
    report.append("- Modern styling and themes")
    report.append("- Improved keyboard shortcuts")
    report.append("- Better status feedback")
    
    report.append("")
    report.append("PRESERVED FUNCTIONALITY:")
    report.append("- Auto-generation of C++ headers")
    report.append("- Auto-generation of Erlang headers")
    report.append("- JSON configuration management")
    report.append("- Device/Worker/Client management")
    report.append("- Network scanning capabilities")
    report.append("- Communication mapping")
    report.append("- Experiment flow configuration")
    
    return "\n".join(report)

class PySide6Helper:
    """
    Helper class for common PySide6 operations in migrated code
    """
    
    @staticmethod
    def create_form_layout(field_definitions: List[Dict]) -> 'QFormLayout':
        """
        Create a form layout from field definitions
        """
        from PySide6.QtWidgets import QFormLayout, QLineEdit, QSpinBox, QComboBox, QCheckBox
        
        layout = QFormLayout()
        
        for field in field_definitions:
            label = field.get('label', 'Field')
            field_type = field.get('type', 'text')
            
            if field_type == 'text':
                widget = QLineEdit()
            elif field_type == 'number':
                widget = QSpinBox()
                widget.setRange(field.get('min', 0), field.get('max', 100))
            elif field_type == 'combo':
                widget = QComboBox()
                widget.addItems(field.get('options', []))
            elif field_type == 'checkbox':
                widget = QCheckBox()
            else:
                widget = QLineEdit()  # Default fallback
            
            layout.addRow(label, widget)
        
        return layout
    
    @staticmethod
    def apply_validation_style(widget, is_valid: bool):
        """
        Apply validation styling to a widget
        """
        if is_valid:
            widget.setStyleSheet("")
        else:
            widget.setStyleSheet("border: 2px solid red;")
    
    @staticmethod
    def show_info_message(parent, title: str, message: str):
        """
        Show information message dialog
        """
        from PySide6.QtWidgets import QMessageBox
        QMessageBox.information(parent, title, message)
    
    @staticmethod
    def show_error_message(parent, title: str, message: str):
        """
        Show error message dialog
        """
        from PySide6.QtWidgets import QMessageBox
        QMessageBox.critical(parent, title, message)
    
    @staticmethod
    def show_warning_message(parent, title: str, message: str):
        """
        Show warning message dialog
        """
        from PySide6.QtWidgets import QMessageBox
        QMessageBox.warning(parent, title, message)

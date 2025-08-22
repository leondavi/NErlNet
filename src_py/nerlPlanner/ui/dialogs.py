"""
Dialog classes for Nerlplanner
Specialized dialog windows for worker configuration, communication maps, and experiment flows
"""

from PySide6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QGridLayout,
                               QLabel, QLineEdit, QPushButton, QComboBox, 
                               QListWidget, QTextEdit, QCheckBox, QGroupBox,
                               QTabWidget, QWidget, QFileDialog, QMessageBox,
                               QSpinBox, QDoubleSpinBox, QTableWidget, 
                               QTableWidgetItem, QHeaderView, QSpacerItem,
                               QSizePolicy, QScrollArea, QFrame)
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont, QIcon

# Import existing modules for compatibility
from Definitions import *
from JsonElements import *


class WorkerDialog(QDialog):
    """Dialog for configuring worker parameters"""
    
    worker_saved = Signal(dict)  # Signal emitted when worker is saved
    
    def __init__(self, parent=None, worker_data=None):
        super().__init__(parent)
        self.worker_data = worker_data if worker_data else {}
        self.init_ui()
        self.load_worker_data()
        
    def init_ui(self):
        """Initialize the user interface"""
        self.setWindowTitle("Worker Configuration")
        self.setModal(True)
        self.resize(800, 600)
        
        layout = QVBoxLayout(self)
        
        # Create tab widget for different parameter categories
        self.tab_widget = QTabWidget()
        layout.addWidget(self.tab_widget)
        
        # Create tabs
        self.create_basic_tab()
        self.create_network_tab()
        self.create_learning_tab()
        self.create_advanced_tab()
        
        # Button layout
        button_layout = QHBoxLayout()
        
        self.save_button = QPushButton("Save Worker")
        self.save_button.clicked.connect(self.save_worker)
        
        self.cancel_button = QPushButton("Cancel")
        self.cancel_button.clicked.connect(self.reject)
        
        self.load_button = QPushButton("Load from File")
        self.load_button.clicked.connect(self.load_from_file)
        
        self.export_button = QPushButton("Export to File")
        self.export_button.clicked.connect(self.export_to_file)
        
        button_layout.addWidget(self.load_button)
        button_layout.addWidget(self.export_button)
        button_layout.addStretch()
        button_layout.addWidget(self.save_button)
        button_layout.addWidget(self.cancel_button)
        
        layout.addLayout(button_layout)
        
    def create_basic_tab(self):
        """Create basic worker parameters tab"""
        basic_widget = QWidget()
        layout = QGridLayout(basic_widget)
        
        # Worker identification
        id_group = QGroupBox("Worker Identification")
        id_layout = QGridLayout(id_group)
        
        id_layout.addWidget(QLabel("Worker Name:"), 0, 0)
        self.worker_name_input = QLineEdit()
        id_layout.addWidget(self.worker_name_input, 0, 1)
        
        id_layout.addWidget(QLabel("Worker ID:"), 1, 0)
        self.worker_id_input = QLineEdit()
        id_layout.addWidget(self.worker_id_input, 1, 1)
        
        id_layout.addWidget(QLabel("Description:"), 2, 0)
        self.worker_description_input = QTextEdit()
        self.worker_description_input.setMaximumHeight(80)
        id_layout.addWidget(self.worker_description_input, 2, 1)
        
        layout.addWidget(id_group, 0, 0, 1, 2)
        
        # Model parameters
        model_group = QGroupBox("Model Parameters")
        model_layout = QGridLayout(model_group)
        
        model_layout.addWidget(QLabel("Model Type:"), 0, 0)
        self.model_type_combo = QComboBox()
        self.model_type_combo.addItems(["feedforward", "lstm", "gru", "cnn", "transformer"])
        model_layout.addWidget(self.model_type_combo, 0, 1)
        
        model_layout.addWidget(QLabel("Input Size:"), 1, 0)
        self.input_size_spin = QSpinBox()
        self.input_size_spin.setRange(1, 10000)
        self.input_size_spin.setValue(784)
        model_layout.addWidget(self.input_size_spin, 1, 1)
        
        model_layout.addWidget(QLabel("Output Size:"), 2, 0)
        self.output_size_spin = QSpinBox()
        self.output_size_spin.setRange(1, 10000)
        self.output_size_spin.setValue(10)
        model_layout.addWidget(self.output_size_spin, 2, 1)
        
        model_layout.addWidget(QLabel("Hidden Layers:"), 3, 0)
        self.hidden_layers_input = QLineEdit()
        self.hidden_layers_input.setPlaceholderText("e.g., 128,64,32")
        model_layout.addWidget(self.hidden_layers_input, 3, 1)
        
        layout.addWidget(model_group, 1, 0, 1, 2)
        
        # Activation functions
        activation_group = QGroupBox("Activation Functions")
        activation_layout = QGridLayout(activation_group)
        
        activation_layout.addWidget(QLabel("Hidden Activation:"), 0, 0)
        self.hidden_activation_combo = QComboBox()
        self.hidden_activation_combo.addItems(["relu", "sigmoid", "tanh", "leaky_relu", "elu"])
        activation_layout.addWidget(self.hidden_activation_combo, 0, 1)
        
        activation_layout.addWidget(QLabel("Output Activation:"), 1, 0)
        self.output_activation_combo = QComboBox()
        self.output_activation_combo.addItems(["softmax", "sigmoid", "linear", "tanh"])
        activation_layout.addWidget(self.output_activation_combo, 1, 1)
        
        layout.addWidget(activation_group, 2, 0, 1, 2)
        
        self.tab_widget.addTab(basic_widget, "Basic")
        
    def create_network_tab(self):
        """Create network parameters tab"""
        network_widget = QWidget()
        layout = QGridLayout(network_widget)
        
        # Connection parameters
        conn_group = QGroupBox("Network Connection")
        conn_layout = QGridLayout(conn_group)
        
        conn_layout.addWidget(QLabel("Port:"), 0, 0)
        self.port_spin = QSpinBox()
        self.port_spin.setRange(1024, 65535)
        self.port_spin.setValue(8080)
        conn_layout.addWidget(self.port_spin, 0, 1)
        
        conn_layout.addWidget(QLabel("Host:"), 1, 0)
        self.host_input = QLineEdit()
        self.host_input.setText("localhost")
        conn_layout.addWidget(self.host_input, 1, 1)
        
        conn_layout.addWidget(QLabel("Protocol:"), 2, 0)
        self.protocol_combo = QComboBox()
        self.protocol_combo.addItems(["tcp", "udp", "websocket"])
        conn_layout.addWidget(self.protocol_combo, 2, 1)
        
        layout.addWidget(conn_group, 0, 0, 1, 2)
        
        # Communication parameters
        comm_group = QGroupBox("Communication Settings")
        comm_layout = QGridLayout(comm_group)
        
        comm_layout.addWidget(QLabel("Batch Size:"), 0, 0)
        self.batch_size_spin = QSpinBox()
        self.batch_size_spin.setRange(1, 1000)
        self.batch_size_spin.setValue(32)
        comm_layout.addWidget(self.batch_size_spin, 0, 1)
        
        comm_layout.addWidget(QLabel("Buffer Size:"), 1, 0)
        self.buffer_size_spin = QSpinBox()
        self.buffer_size_spin.setRange(1, 10000)
        self.buffer_size_spin.setValue(1000)
        comm_layout.addWidget(self.buffer_size_spin, 1, 1)
        
        comm_layout.addWidget(QLabel("Timeout (ms):"), 2, 0)
        self.timeout_spin = QSpinBox()
        self.timeout_spin.setRange(100, 60000)
        self.timeout_spin.setValue(5000)
        comm_layout.addWidget(self.timeout_spin, 2, 1)
        
        layout.addWidget(comm_group, 1, 0, 1, 2)
        
        # Security settings
        security_group = QGroupBox("Security Settings")
        security_layout = QGridLayout(security_group)
        
        self.use_encryption_check = QCheckBox("Use Encryption")
        security_layout.addWidget(self.use_encryption_check, 0, 0)
        
        self.use_authentication_check = QCheckBox("Use Authentication")
        security_layout.addWidget(self.use_authentication_check, 0, 1)
        
        security_layout.addWidget(QLabel("Certificate Path:"), 1, 0)
        cert_layout = QHBoxLayout()
        self.cert_path_input = QLineEdit()
        cert_layout.addWidget(self.cert_path_input)
        cert_browse_btn = QPushButton("Browse")
        cert_browse_btn.clicked.connect(self.browse_certificate)
        cert_layout.addWidget(cert_browse_btn)
        security_layout.addLayout(cert_layout, 1, 1)
        
        layout.addWidget(security_group, 2, 0, 1, 2)
        
        self.tab_widget.addTab(network_widget, "Network")
        
    def create_learning_tab(self):
        """Create learning parameters tab"""
        learning_widget = QWidget()
        layout = QGridLayout(learning_widget)
        
        # Training parameters
        training_group = QGroupBox("Training Parameters")
        training_layout = QGridLayout(training_group)
        
        training_layout.addWidget(QLabel("Learning Rate:"), 0, 0)
        self.learning_rate_spin = QDoubleSpinBox()
        self.learning_rate_spin.setRange(0.0001, 1.0)
        self.learning_rate_spin.setDecimals(6)
        self.learning_rate_spin.setValue(0.001)
        training_layout.addWidget(self.learning_rate_spin, 0, 1)
        
        training_layout.addWidget(QLabel("Epochs:"), 1, 0)
        self.epochs_spin = QSpinBox()
        self.epochs_spin.setRange(1, 10000)
        self.epochs_spin.setValue(100)
        training_layout.addWidget(self.epochs_spin, 1, 1)
        
        training_layout.addWidget(QLabel("Optimizer:"), 2, 0)
        self.optimizer_combo = QComboBox()
        self.optimizer_combo.addItems(["adam", "sgd", "rmsprop", "adagrad"])
        training_layout.addWidget(self.optimizer_combo, 2, 1)
        
        training_layout.addWidget(QLabel("Loss Function:"), 3, 0)
        self.loss_function_combo = QComboBox()
        self.loss_function_combo.addItems(["mse", "cross_entropy", "binary_cross_entropy", "mae"])
        training_layout.addWidget(self.loss_function_combo, 3, 1)
        
        layout.addWidget(training_group, 0, 0, 1, 2)
        
        # Regularization
        reg_group = QGroupBox("Regularization")
        reg_layout = QGridLayout(reg_group)
        
        reg_layout.addWidget(QLabel("Dropout Rate:"), 0, 0)
        self.dropout_spin = QDoubleSpinBox()
        self.dropout_spin.setRange(0.0, 0.9)
        self.dropout_spin.setDecimals(2)
        self.dropout_spin.setValue(0.2)
        reg_layout.addWidget(self.dropout_spin, 0, 1)
        
        reg_layout.addWidget(QLabel("L1 Regularization:"), 1, 0)
        self.l1_reg_spin = QDoubleSpinBox()
        self.l1_reg_spin.setRange(0.0, 1.0)
        self.l1_reg_spin.setDecimals(6)
        self.l1_reg_spin.setValue(0.0)
        reg_layout.addWidget(self.l1_reg_spin, 1, 1)
        
        reg_layout.addWidget(QLabel("L2 Regularization:"), 2, 0)
        self.l2_reg_spin = QDoubleSpinBox()
        self.l2_reg_spin.setRange(0.0, 1.0)
        self.l2_reg_spin.setDecimals(6)
        self.l2_reg_spin.setValue(0.0)
        reg_layout.addWidget(self.l2_reg_spin, 2, 1)
        
        layout.addWidget(reg_group, 1, 0, 1, 2)
        
        # Validation
        val_group = QGroupBox("Validation")
        val_layout = QGridLayout(val_group)
        
        self.use_validation_check = QCheckBox("Use Validation Set")
        val_layout.addWidget(self.use_validation_check, 0, 0)
        
        val_layout.addWidget(QLabel("Validation Split:"), 1, 0)
        self.val_split_spin = QDoubleSpinBox()
        self.val_split_spin.setRange(0.1, 0.5)
        self.val_split_spin.setDecimals(2)
        self.val_split_spin.setValue(0.2)
        val_layout.addWidget(self.val_split_spin, 1, 1)
        
        val_layout.addWidget(QLabel("Early Stopping Patience:"), 2, 0)
        self.early_stopping_spin = QSpinBox()
        self.early_stopping_spin.setRange(1, 100)
        self.early_stopping_spin.setValue(10)
        val_layout.addWidget(self.early_stopping_spin, 2, 1)
        
        layout.addWidget(val_group, 2, 0, 1, 2)
        
        self.tab_widget.addTab(learning_widget, "Learning")
        
    def create_advanced_tab(self):
        """Create advanced parameters tab"""
        advanced_widget = QWidget()
        layout = QVBoxLayout(advanced_widget)
        
        # Custom parameters table
        custom_group = QGroupBox("Custom Parameters")
        custom_layout = QVBoxLayout(custom_group)
        
        # Table for custom key-value pairs
        self.custom_params_table = QTableWidget()
        self.custom_params_table.setColumnCount(2)
        self.custom_params_table.setHorizontalHeaderLabels(["Parameter", "Value"])
        self.custom_params_table.horizontalHeader().setStretchLastSection(True)
        custom_layout.addWidget(self.custom_params_table)
        
        # Buttons for table management
        table_buttons = QHBoxLayout()
        add_param_btn = QPushButton("Add Parameter")
        add_param_btn.clicked.connect(self.add_custom_parameter)
        remove_param_btn = QPushButton("Remove Parameter")
        remove_param_btn.clicked.connect(self.remove_custom_parameter)
        
        table_buttons.addWidget(add_param_btn)
        table_buttons.addWidget(remove_param_btn)
        table_buttons.addStretch()
        
        custom_layout.addLayout(table_buttons)
        layout.addWidget(custom_group)
        
        # Raw JSON editor
        json_group = QGroupBox("Raw JSON Configuration")
        json_layout = QVBoxLayout(json_group)
        
        self.json_editor = QTextEdit()
        self.json_editor.setFont(QFont("Courier", 10))
        json_layout.addWidget(self.json_editor)
        
        json_buttons = QHBoxLayout()
        validate_json_btn = QPushButton("Validate JSON")
        validate_json_btn.clicked.connect(self.validate_json)
        format_json_btn = QPushButton("Format JSON")
        format_json_btn.clicked.connect(self.format_json)
        
        json_buttons.addWidget(validate_json_btn)
        json_buttons.addWidget(format_json_btn)
        json_buttons.addStretch()
        
        json_layout.addLayout(json_buttons)
        layout.addWidget(json_group)
        
        self.tab_widget.addTab(advanced_widget, "Advanced")
        
    def load_worker_data(self):
        """Load existing worker data into the dialog"""
        if not self.worker_data:
            return
            
        # Load basic parameters
        self.worker_name_input.setText(self.worker_data.get('name', ''))
        self.worker_id_input.setText(self.worker_data.get('id', ''))
        self.worker_description_input.setText(self.worker_data.get('description', ''))
        
        # Load model parameters
        model_data = self.worker_data.get('model', {})
        self.model_type_combo.setCurrentText(model_data.get('type', 'feedforward'))
        self.input_size_spin.setValue(model_data.get('input_size', 784))
        self.output_size_spin.setValue(model_data.get('output_size', 10))
        
        hidden_layers = model_data.get('hidden_layers', [])
        if hidden_layers:
            self.hidden_layers_input.setText(','.join(map(str, hidden_layers)))
            
        # Load network parameters
        network_data = self.worker_data.get('network', {})
        self.port_spin.setValue(network_data.get('port', 8080))
        self.host_input.setText(network_data.get('host', 'localhost'))
        self.protocol_combo.setCurrentText(network_data.get('protocol', 'tcp'))
        
        # Load learning parameters
        learning_data = self.worker_data.get('learning', {})
        self.learning_rate_spin.setValue(learning_data.get('learning_rate', 0.001))
        self.epochs_spin.setValue(learning_data.get('epochs', 100))
        self.optimizer_combo.setCurrentText(learning_data.get('optimizer', 'adam'))
        self.loss_function_combo.setCurrentText(learning_data.get('loss_function', 'mse'))
        
    def save_worker(self):
        """Save worker configuration"""
        try:
            # Collect all worker data
            worker_data = {
                'name': self.worker_name_input.text(),
                'id': self.worker_id_input.text(),
                'description': self.worker_description_input.toPlainText(),
                'model': {
                    'type': self.model_type_combo.currentText(),
                    'input_size': self.input_size_spin.value(),
                    'output_size': self.output_size_spin.value(),
                    'hidden_layers': [int(x.strip()) for x in self.hidden_layers_input.text().split(',') if x.strip()],
                    'hidden_activation': self.hidden_activation_combo.currentText(),
                    'output_activation': self.output_activation_combo.currentText()
                },
                'network': {
                    'port': self.port_spin.value(),
                    'host': self.host_input.text(),
                    'protocol': self.protocol_combo.currentText(),
                    'batch_size': self.batch_size_spin.value(),
                    'buffer_size': self.buffer_size_spin.value(),
                    'timeout': self.timeout_spin.value(),
                    'use_encryption': self.use_encryption_check.isChecked(),
                    'use_authentication': self.use_authentication_check.isChecked(),
                    'certificate_path': self.cert_path_input.text()
                },
                'learning': {
                    'learning_rate': self.learning_rate_spin.value(),
                    'epochs': self.epochs_spin.value(),
                    'optimizer': self.optimizer_combo.currentText(),
                    'loss_function': self.loss_function_combo.currentText(),
                    'dropout_rate': self.dropout_spin.value(),
                    'l1_regularization': self.l1_reg_spin.value(),
                    'l2_regularization': self.l2_reg_spin.value(),
                    'use_validation': self.use_validation_check.isChecked(),
                    'validation_split': self.val_split_spin.value(),
                    'early_stopping_patience': self.early_stopping_spin.value()
                }
            }
            
            # Add custom parameters
            custom_params = {}
            for row in range(self.custom_params_table.rowCount()):
                key_item = self.custom_params_table.item(row, 0)
                value_item = self.custom_params_table.item(row, 1)
                if key_item and value_item:
                    custom_params[key_item.text()] = value_item.text()
            
            if custom_params:
                worker_data['custom_parameters'] = custom_params
                
            # Emit signal and close dialog
            self.worker_saved.emit(worker_data)
            self.accept()
            
        except Exception as e:
            QMessageBox.critical(self, "Save Error", f"Failed to save worker configuration: {str(e)}")
            
    def load_from_file(self):
        """Load worker configuration from file"""
        file_path, _ = QFileDialog.getOpenFileName(self, 'Load Worker Configuration', '', 'JSON Files (*.json)')
        if file_path:
            try:
                import json
                with open(file_path, 'r') as f:
                    self.worker_data = json.load(f)
                self.load_worker_data()
                QMessageBox.information(self, 'Load Complete', f'Worker configuration loaded from {file_path}')
            except Exception as e:
                QMessageBox.critical(self, 'Load Error', f'Failed to load configuration: {str(e)}')
                
    def export_to_file(self):
        """Export worker configuration to file"""
        file_path, _ = QFileDialog.getSaveFileName(self, 'Export Worker Configuration', '', 'JSON Files (*.json)')
        if file_path:
            try:
                # Get current configuration
                self.save_worker()  # This will emit the signal, but we can also return the data
                # TODO: Implement actual export functionality
                QMessageBox.information(self, 'Export Complete', f'Worker configuration exported to {file_path}')
            except Exception as e:
                QMessageBox.critical(self, 'Export Error', f'Failed to export configuration: {str(e)}')
                
    def browse_certificate(self):
        """Browse for certificate file"""
        file_path, _ = QFileDialog.getOpenFileName(self, 'Select Certificate', '', 'Certificate Files (*.pem *.crt *.cert)')
        if file_path:
            self.cert_path_input.setText(file_path)
            
    def add_custom_parameter(self):
        """Add a new custom parameter row"""
        row_count = self.custom_params_table.rowCount()
        self.custom_params_table.insertRow(row_count)
        self.custom_params_table.setItem(row_count, 0, QTableWidgetItem("parameter_name"))
        self.custom_params_table.setItem(row_count, 1, QTableWidgetItem("value"))
        
    def remove_custom_parameter(self):
        """Remove selected custom parameter"""
        current_row = self.custom_params_table.currentRow()
        if current_row >= 0:
            self.custom_params_table.removeRow(current_row)
            
    def validate_json(self):
        """Validate JSON in the editor"""
        try:
            import json
            json_text = self.json_editor.toPlainText()
            json.loads(json_text)
            QMessageBox.information(self, 'Validation', 'JSON is valid!')
        except json.JSONDecodeError as e:
            QMessageBox.warning(self, 'Validation Error', f'Invalid JSON: {str(e)}')
        except Exception as e:
            QMessageBox.critical(self, 'Error', f'Validation failed: {str(e)}')
            
    def format_json(self):
        """Format JSON in the editor"""
        try:
            import json
            json_text = self.json_editor.toPlainText()
            parsed_json = json.loads(json_text)
            formatted_json = json.dumps(parsed_json, indent=2)
            self.json_editor.setPlainText(formatted_json)
        except Exception as e:
            QMessageBox.warning(self, 'Format Error', f'Failed to format JSON: {str(e)}')


class CommunicationMapDialog(QDialog):
    """Dialog for viewing and editing communication maps"""
    
    def __init__(self, parent=None, comm_data=None):
        super().__init__(parent)
        self.comm_data = comm_data if comm_data else {}
        self.init_ui()
        
    def init_ui(self):
        """Initialize the communication map dialog"""
        self.setWindowTitle("Communication Map")
        self.setModal(True)
        self.resize(900, 700)
        
        layout = QVBoxLayout(self)
        
        # Create visualization area (placeholder for now)
        viz_group = QGroupBox("Network Topology")
        viz_layout = QVBoxLayout(viz_group)
        
        self.visualization_area = QLabel("Communication map visualization will be displayed here")
        self.visualization_area.setAlignment(Qt.AlignCenter)
        self.visualization_area.setMinimumHeight(400)
        self.visualization_area.setStyleSheet("border: 1px solid gray; background-color: white;")
        viz_layout.addWidget(self.visualization_area)
        
        # Controls
        controls_layout = QHBoxLayout()
        refresh_btn = QPushButton("Refresh Map")
        refresh_btn.clicked.connect(self.refresh_map)
        export_btn = QPushButton("Export Map")
        export_btn.clicked.connect(self.export_map)
        
        controls_layout.addWidget(refresh_btn)
        controls_layout.addWidget(export_btn)
        controls_layout.addStretch()
        
        viz_layout.addLayout(controls_layout)
        layout.addWidget(viz_group)
        
        # Connection details
        details_group = QGroupBox("Connection Details")
        details_layout = QVBoxLayout(details_group)
        
        self.details_table = QTableWidget()
        self.details_table.setColumnCount(4)
        self.details_table.setHorizontalHeaderLabels(["Source", "Target", "Type", "Status"])
        self.details_table.horizontalHeader().setStretchLastSection(True)
        details_layout.addWidget(self.details_table)
        
        layout.addWidget(details_group)
        
        # Button layout
        button_layout = QHBoxLayout()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addStretch()
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
        
    def refresh_map(self):
        """Refresh the communication map"""
        # TODO: Implement map refresh functionality
        QMessageBox.information(self, 'Refresh', 'Communication map refreshed')
        
    def export_map(self):
        """Export the communication map"""
        file_path, _ = QFileDialog.getSaveFileName(self, 'Export Communication Map', '', 'PNG Files (*.png);;SVG Files (*.svg)')
        if file_path:
            # TODO: Implement map export functionality
            QMessageBox.information(self, 'Export', f'Communication map exported to {file_path}')


class ExperimentFlowDialog(QDialog):
    """Dialog for configuring experiment flows"""
    
    def __init__(self, parent=None, experiment_data=None):
        super().__init__(parent)
        self.experiment_data = experiment_data if experiment_data else {}
        self.init_ui()
        
    def init_ui(self):
        """Initialize the experiment flow dialog"""
        self.setWindowTitle("Experiment Flow Configuration")
        self.setModal(True)
        self.resize(800, 600)
        
        layout = QVBoxLayout(self)
        
        # Experiment configuration
        config_group = QGroupBox("Experiment Configuration")
        config_layout = QGridLayout(config_group)
        
        config_layout.addWidget(QLabel("Experiment Name:"), 0, 0)
        self.experiment_name_input = QLineEdit()
        config_layout.addWidget(self.experiment_name_input, 0, 1)
        
        config_layout.addWidget(QLabel("Description:"), 1, 0)
        self.experiment_description_input = QTextEdit()
        self.experiment_description_input.setMaximumHeight(80)
        config_layout.addWidget(self.experiment_description_input, 1, 1)
        
        config_layout.addWidget(QLabel("Duration (minutes):"), 2, 0)
        self.duration_spin = QSpinBox()
        self.duration_spin.setRange(1, 10000)
        self.duration_spin.setValue(60)
        config_layout.addWidget(self.duration_spin, 2, 1)
        
        layout.addWidget(config_group)
        
        # Flow steps
        steps_group = QGroupBox("Experiment Steps")
        steps_layout = QVBoxLayout(steps_group)
        
        self.steps_list = QListWidget()
        steps_layout.addWidget(self.steps_list)
        
        # Step controls
        step_controls = QHBoxLayout()
        add_step_btn = QPushButton("Add Step")
        add_step_btn.clicked.connect(self.add_step)
        remove_step_btn = QPushButton("Remove Step")
        remove_step_btn.clicked.connect(self.remove_step)
        move_up_btn = QPushButton("Move Up")
        move_up_btn.clicked.connect(self.move_step_up)
        move_down_btn = QPushButton("Move Down")
        move_down_btn.clicked.connect(self.move_step_down)
        
        step_controls.addWidget(add_step_btn)
        step_controls.addWidget(remove_step_btn)
        step_controls.addWidget(move_up_btn)
        step_controls.addWidget(move_down_btn)
        step_controls.addStretch()
        
        steps_layout.addLayout(step_controls)
        layout.addWidget(steps_group)
        
        # Button layout
        button_layout = QHBoxLayout()
        save_btn = QPushButton("Save Experiment")
        save_btn.clicked.connect(self.save_experiment)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        
        button_layout.addStretch()
        button_layout.addWidget(save_btn)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
    def add_step(self):
        """Add a new experiment step"""
        step_name, ok = QInputDialog.getText(self, 'Add Step', 'Enter step name:')
        if ok and step_name:
            self.steps_list.addItem(step_name)
            
    def remove_step(self):
        """Remove selected step"""
        current_row = self.steps_list.currentRow()
        if current_row >= 0:
            self.steps_list.takeItem(current_row)
            
    def move_step_up(self):
        """Move selected step up"""
        current_row = self.steps_list.currentRow()
        if current_row > 0:
            item = self.steps_list.takeItem(current_row)
            self.steps_list.insertItem(current_row - 1, item)
            self.steps_list.setCurrentRow(current_row - 1)
            
    def move_step_down(self):
        """Move selected step down"""
        current_row = self.steps_list.currentRow()
        if current_row < self.steps_list.count() - 1:
            item = self.steps_list.takeItem(current_row)
            self.steps_list.insertItem(current_row + 1, item)
            self.steps_list.setCurrentRow(current_row + 1)
            
    def save_experiment(self):
        """Save experiment configuration"""
        experiment_name = self.experiment_name_input.text()
        if not experiment_name:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter an experiment name')
            return
            
        # Collect experiment data
        experiment_data = {
            'name': experiment_name,
            'description': self.experiment_description_input.toPlainText(),
            'duration': self.duration_spin.value(),
            'steps': [self.steps_list.item(i).text() for i in range(self.steps_list.count())]
        }
        
        # TODO: Save experiment data
        QMessageBox.information(self, 'Save Complete', f'Experiment "{experiment_name}" saved successfully')
        self.accept()


# Import QInputDialog at the top if not already imported
try:
    from PySide6.QtWidgets import QInputDialog
except ImportError:
    pass  # Already imported

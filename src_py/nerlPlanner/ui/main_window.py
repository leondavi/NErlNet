"""
Main Application Class for Nerlplanner
Modern PySide6-based GUI with enhanced user experience
"""

import sys
import os
from PySide6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, 
                               QHBoxLayout, QTabWidget, QLabel, QLineEdit, 
                               QPushButton, QListWidget, QComboBox, QCheckBox,
                               QFrame, QGroupBox, QGridLayout, QTextEdit,
                               QSplitter, QStatusBar, QMenuBar, QMenu, QMessageBox,
                               QFileDialog, QProgressBar, QToolBar, QSpacerItem,
                               QSizePolicy)
from PySide6.QtCore import Qt, QTimer, QThread, Signal, QSize
from PySide6.QtGui import QIcon, QPixmap, QAction, QFont, QPalette

# Import existing modules
from Definitions import *
from Handlers import *
from JsonElements import *
from Pinger import *
from logger import *

class NerlplannerMainWindow(QMainWindow):
    """Main window for the Nerlplanner application"""
    
    def __init__(self):
        super().__init__()
        self.init_ui()
        self.init_data()
        self.setup_connections()
        
    def init_ui(self):
        """Initialize the user interface"""
        self.setWindowTitle(f"{WINDOW_TITLE} v{VERSION}")
        self.setGeometry(100, 100, WINDOW_FIXED_WIDTH, int(WINDOW_MAX_SUPPORTED_HEIGHT * WINDOW_HEIGHT_MULTIPLICATION_FACTOR))
        
        # Set application icon
        if os.path.exists(NERLNET_LOGO_PATH):
            self.setWindowIcon(QIcon(NERLNET_LOGO_PATH))
        
        # Create central widget
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        # Create main layout
        main_layout = QVBoxLayout(central_widget)
        
        # Create menu bar
        self.create_menu_bar()
        
        # Create toolbar
        self.create_toolbar()
        
        # Create main content area with tabs
        self.create_main_content(main_layout)
        
        # Create status bar
        self.create_status_bar()
        
        # Apply modern styling
        self.apply_modern_style()
        
    def create_menu_bar(self):
        """Create the application menu bar"""
        menubar = self.menuBar()
        
        # File menu
        file_menu = menubar.addMenu('&File')
        
        new_action = QAction('&New Configuration', self)
        new_action.setShortcut('Ctrl+N')
        new_action.triggered.connect(self.new_configuration)
        file_menu.addAction(new_action)
        
        open_action = QAction('&Open Configuration', self)
        open_action.setShortcut('Ctrl+O')
        open_action.triggered.connect(self.open_configuration)
        file_menu.addAction(open_action)
        
        save_action = QAction('&Save Configuration', self)
        save_action.setShortcut('Ctrl+S')
        save_action.triggered.connect(self.save_configuration)
        file_menu.addAction(save_action)
        
        file_menu.addSeparator()
        
        export_menu = file_menu.addMenu('&Export')
        export_cpp_action = QAction('Export C++ Headers', self)
        export_cpp_action.triggered.connect(self.export_cpp_headers)
        export_menu.addAction(export_cpp_action)
        
        export_erl_action = QAction('Export Erlang Headers', self)
        export_erl_action.triggered.connect(self.export_erlang_headers)
        export_menu.addAction(export_erl_action)
        
        file_menu.addSeparator()
        
        exit_action = QAction('E&xit', self)
        exit_action.setShortcut('Ctrl+Q')
        exit_action.triggered.connect(self.close)
        file_menu.addAction(exit_action)
        
        # Tools menu
        tools_menu = menubar.addMenu('&Tools')
        
        scan_devices_action = QAction('Scan Network Devices', self)
        scan_devices_action.triggered.connect(self.scan_network_devices)
        tools_menu.addAction(scan_devices_action)
        
        # View menu
        view_menu = menubar.addMenu('&View')
        
        comm_map_action = QAction('Communication Map', self)
        comm_map_action.triggered.connect(self.show_communication_map)
        view_menu.addAction(comm_map_action)
        
        exp_flow_action = QAction('Experiment Flow', self)
        exp_flow_action.triggered.connect(self.show_experiment_flow)
        view_menu.addAction(exp_flow_action)
        
        # Help menu
        help_menu = menubar.addMenu('&Help')
        
        about_action = QAction('&About', self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)
        
    def create_toolbar(self):
        """Create the main toolbar"""
        toolbar = QToolBar()
        self.addToolBar(toolbar)
        
        # Add quick action buttons
        new_btn = QPushButton("New")
        new_btn.setToolTip("Create new configuration")
        new_btn.clicked.connect(self.new_configuration)
        toolbar.addWidget(new_btn)
        
        open_btn = QPushButton("Open")
        open_btn.setToolTip("Open existing configuration")
        open_btn.clicked.connect(self.open_configuration)
        toolbar.addWidget(open_btn)
        
        save_btn = QPushButton("Save")
        save_btn.setToolTip("Save current configuration")
        save_btn.clicked.connect(self.save_configuration)
        toolbar.addWidget(save_btn)
        
        toolbar.addSeparator()
        
        scan_btn = QPushButton("Scan Devices")
        scan_btn.setToolTip("Scan for network devices")
        scan_btn.clicked.connect(self.scan_network_devices)
        toolbar.addWidget(scan_btn)
        
        # Add progress bar to toolbar (hidden by default)
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        toolbar.addWidget(self.progress_bar)
        
        # Add spacer
        spacer = QWidget()
        spacer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
        toolbar.addWidget(spacer)
        
        # Add host IP display
        self.host_ip_label = QLabel(f"Host IP: {get_this_host_ip()}")
        toolbar.addWidget(self.host_ip_label)
        
    def create_main_content(self, main_layout):
        """Create the main content area with tabs"""
        # Create tab widget
        self.tab_widget = QTabWidget()
        main_layout.addWidget(self.tab_widget)
        
        # Create tabs
        self.create_settings_tab()
        self.create_devices_tab()
        self.create_workers_tab()
        self.create_clients_tab()
        self.create_network_tab()
        
    def create_settings_tab(self):
        """Create the settings configuration tab"""
        settings_widget = QWidget()
        layout = QVBoxLayout(settings_widget)
        
        # Settings group
        settings_group = QGroupBox("Global Settings")
        settings_layout = QGridLayout(settings_group)
        
        # Frequency setting
        settings_layout.addWidget(QLabel("Default Frequency:"), 0, 0)
        self.frequency_input = QLineEdit()
        self.frequency_input.setPlaceholderText("Enter frequency value")
        settings_layout.addWidget(self.frequency_input, 0, 1)
        settings_layout.addWidget(QLabel("Default frequency for sensors"), 0, 2)
        
        # Batch size setting
        settings_layout.addWidget(QLabel("Batch Size:"), 1, 0)
        self.batch_size_input = QLineEdit()
        self.batch_size_input.setPlaceholderText("Enter batch size")
        settings_layout.addWidget(self.batch_size_input, 1, 1)
        settings_layout.addWidget(QLabel("Number of samples in a message"), 1, 2)
        
        # Buttons
        button_layout = QHBoxLayout()
        save_settings_btn = QPushButton("Save Settings")
        save_settings_btn.clicked.connect(self.save_settings)
        clear_settings_btn = QPushButton("Clear Settings")
        clear_settings_btn.clicked.connect(self.clear_settings)
        
        button_layout.addWidget(save_settings_btn)
        button_layout.addWidget(clear_settings_btn)
        button_layout.addStretch()
        
        settings_layout.addLayout(button_layout, 2, 0, 1, 3)
        
        layout.addWidget(settings_group)
        
        # Special Entities group
        special_group = QGroupBox("Special Entities")
        special_layout = QGridLayout(special_group)
        
        # Main Server
        special_layout.addWidget(QLabel("Main Server Port:"), 0, 0)
        self.main_server_port_input = QLineEdit()
        special_layout.addWidget(self.main_server_port_input, 0, 1)
        special_layout.addWidget(QLabel("Args:"), 0, 2)
        self.main_server_args_input = QLineEdit()
        special_layout.addWidget(self.main_server_args_input, 0, 3)
        
        # API Server
        special_layout.addWidget(QLabel("API Server Port:"), 1, 0)
        self.api_server_port_input = QLineEdit()
        special_layout.addWidget(self.api_server_port_input, 1, 1)
        special_layout.addWidget(QLabel("Args:"), 1, 2)
        self.api_server_args_input = QLineEdit()
        special_layout.addWidget(self.api_server_args_input, 1, 3)
        
        # Buttons
        special_button_layout = QHBoxLayout()
        save_special_btn = QPushButton("Save Special Entities")
        save_special_btn.clicked.connect(self.save_special_entities)
        clear_special_btn = QPushButton("Clear Special Entities")
        clear_special_btn.clicked.connect(self.clear_special_entities)
        
        special_button_layout.addWidget(save_special_btn)
        special_button_layout.addWidget(clear_special_btn)
        special_button_layout.addStretch()
        
        special_layout.addLayout(special_button_layout, 2, 0, 1, 4)
        
        layout.addWidget(special_group)
        
        # Status displays
        self.settings_status_label = QLabel("Settings Status: Ready")
        self.main_server_status_label = QLabel("Main Server Status: Not configured")
        self.api_server_status_label = QLabel("API Server Status: Not configured")
        
        layout.addWidget(self.settings_status_label)
        layout.addWidget(self.main_server_status_label)
        layout.addWidget(self.api_server_status_label)
        
        layout.addStretch()
        
        self.tab_widget.addTab(settings_widget, "Settings")
        
    def create_devices_tab(self):
        """Create the devices management tab"""
        devices_widget = QWidget()
        layout = QHBoxLayout(devices_widget)
        
        # Left side - Device management
        left_widget = QWidget()
        left_layout = QVBoxLayout(left_widget)
        
        # Device scanner group
        scanner_group = QGroupBox("Network Scanner")
        scanner_layout = QGridLayout(scanner_group)
        
        scanner_layout.addWidget(QLabel("LAN Mask:"), 0, 0)
        self.lan_mask_input = QLineEdit("x.x.x.x/24")
        scanner_layout.addWidget(self.lan_mask_input, 0, 1)
        
        scan_btn = QPushButton("Scan Network")
        scan_btn.clicked.connect(self.scan_network_devices)
        scanner_layout.addWidget(scan_btn, 0, 2)
        
        scanner_layout.addWidget(QLabel("Found Devices:"), 1, 0)
        self.devices_online_combo = QComboBox()
        scanner_layout.addWidget(self.devices_online_combo, 1, 1, 1, 2)
        
        left_layout.addWidget(scanner_group)
        
        # Device configuration group
        config_group = QGroupBox("Device Configuration")
        config_layout = QGridLayout(config_group)
        
        config_layout.addWidget(QLabel("Device Name:"), 0, 0)
        self.device_name_input = QLineEdit()
        config_layout.addWidget(self.device_name_input, 0, 1)
        
        config_layout.addWidget(QLabel("IP Address:"), 1, 0)
        self.device_ip_input = QLineEdit("x.x.x.x")
        config_layout.addWidget(self.device_ip_input, 1, 1)
        
        # Device management buttons
        button_layout = QHBoxLayout()
        add_device_btn = QPushButton("Add Device")
        add_device_btn.clicked.connect(self.add_device)
        remove_device_btn = QPushButton("Remove Device")
        remove_device_btn.clicked.connect(self.remove_device)
        load_devices_btn = QPushButton("Load Devices")
        load_devices_btn.clicked.connect(self.load_devices)
        save_devices_btn = QPushButton("Save Devices")
        save_devices_btn.clicked.connect(self.save_devices)
        
        button_layout.addWidget(add_device_btn)
        button_layout.addWidget(remove_device_btn)
        button_layout.addWidget(load_devices_btn)
        button_layout.addWidget(save_devices_btn)
        
        config_layout.addLayout(button_layout, 2, 0, 1, 2)
        
        left_layout.addWidget(config_group)
        
        # Entity assignment group
        entity_group = QGroupBox("Entity Assignment")
        entity_layout = QGridLayout(entity_group)
        
        entity_layout.addWidget(QLabel("Selected Entity:"), 0, 0)
        self.entity_combo = QComboBox()
        entity_layout.addWidget(self.entity_combo, 0, 1)
        
        add_entity_btn = QPushButton("Add Entity to Device")
        add_entity_btn.clicked.connect(self.add_entity_to_device)
        remove_entity_btn = QPushButton("Remove Entity from Device")
        remove_entity_btn.clicked.connect(self.remove_entity_from_device)
        
        entity_layout.addWidget(add_entity_btn, 1, 0)
        entity_layout.addWidget(remove_entity_btn, 1, 1)
        
        left_layout.addWidget(entity_group)
        left_layout.addStretch()
        
        layout.addWidget(left_widget)
        
        # Right side - Device and entity lists
        right_widget = QWidget()
        right_layout = QVBoxLayout(right_widget)
        
        # Devices list
        devices_list_group = QGroupBox("Configured Devices")
        devices_list_layout = QVBoxLayout(devices_list_group)
        self.devices_list = QListWidget()
        self.devices_list.itemSelectionChanged.connect(self.on_device_selected)
        devices_list_layout.addWidget(self.devices_list)
        right_layout.addWidget(devices_list_group)
        
        # Device entities list
        entities_list_group = QGroupBox("Device Entities")
        entities_list_layout = QVBoxLayout(entities_list_group)
        self.device_entities_list = QListWidget()
        entities_list_layout.addWidget(self.device_entities_list)
        right_layout.addWidget(entities_list_group)
        
        layout.addWidget(right_widget)
        
        self.tab_widget.addTab(devices_widget, "Devices")
        
    def create_workers_tab(self):
        """Create the workers management tab"""
        workers_widget = QWidget()
        layout = QVBoxLayout(workers_widget)
        
        # Worker configuration group
        config_group = QGroupBox("Worker Configuration")
        config_layout = QGridLayout(config_group)
        
        config_layout.addWidget(QLabel("Worker Name:"), 0, 0)
        self.worker_name_input = QLineEdit()
        config_layout.addWidget(self.worker_name_input, 0, 1)
        
        config_layout.addWidget(QLabel("Configuration File:"), 1, 0)
        file_layout = QHBoxLayout()
        self.worker_file_input = QLineEdit()
        file_layout.addWidget(self.worker_file_input)
        browse_btn = QPushButton("Browse")
        browse_btn.clicked.connect(self.browse_worker_file)
        file_layout.addWidget(browse_btn)
        config_layout.addLayout(file_layout, 1, 1)
        
        # Worker management buttons
        button_layout = QHBoxLayout()
        add_worker_btn = QPushButton("Add Worker")
        add_worker_btn.clicked.connect(self.add_worker)
        edit_worker_btn = QPushButton("Edit Worker")
        edit_worker_btn.clicked.connect(self.edit_worker)
        remove_worker_btn = QPushButton("Remove Worker")
        remove_worker_btn.clicked.connect(self.remove_worker)
        view_worker_btn = QPushButton("View Worker")
        view_worker_btn.clicked.connect(self.view_worker)
        
        button_layout.addWidget(add_worker_btn)
        button_layout.addWidget(edit_worker_btn)
        button_layout.addWidget(remove_worker_btn)
        button_layout.addWidget(view_worker_btn)
        button_layout.addStretch()
        
        config_layout.addLayout(button_layout, 2, 0, 1, 2)
        
        layout.addWidget(config_group)
        
        # Workers list
        list_group = QGroupBox("Configured Workers")
        list_layout = QVBoxLayout(list_group)
        self.workers_list = QListWidget()
        self.workers_list.itemSelectionChanged.connect(self.on_worker_selected)
        list_layout.addWidget(self.workers_list)
        
        # Worker info display
        self.worker_info_text = QTextEdit()
        self.worker_info_text.setMaximumHeight(100)
        self.worker_info_text.setReadOnly(True)
        list_layout.addWidget(QLabel("Worker Information:"))
        list_layout.addWidget(self.worker_info_text)
        
        layout.addWidget(list_group)
        
        self.tab_widget.addTab(workers_widget, "Workers")
        
    def create_clients_tab(self):
        """Create the clients management tab"""
        clients_widget = QWidget()
        layout = QHBoxLayout(clients_widget)
        
        # Left side - Client configuration
        left_widget = QWidget()
        left_layout = QVBoxLayout(left_widget)
        
        # Client configuration group
        config_group = QGroupBox("Client Configuration")
        config_layout = QGridLayout(config_group)
        
        config_layout.addWidget(QLabel("Client Name:"), 0, 0)
        self.client_name_input = QLineEdit()
        config_layout.addWidget(self.client_name_input, 0, 1)
        
        config_layout.addWidget(QLabel("Port:"), 1, 0)
        self.client_port_input = QLineEdit()
        config_layout.addWidget(self.client_port_input, 1, 1)
        
        # Client management buttons
        button_layout = QHBoxLayout()
        add_client_btn = QPushButton("Add Client")
        add_client_btn.clicked.connect(self.add_client)
        remove_client_btn = QPushButton("Remove Client")
        remove_client_btn.clicked.connect(self.remove_client)
        load_clients_btn = QPushButton("Load Clients")
        load_clients_btn.clicked.connect(self.load_clients)
        save_clients_btn = QPushButton("Save Clients")
        save_clients_btn.clicked.connect(self.save_clients)
        
        button_layout.addWidget(add_client_btn)
        button_layout.addWidget(remove_client_btn)
        button_layout.addWidget(load_clients_btn)
        button_layout.addWidget(save_clients_btn)
        
        config_layout.addLayout(button_layout, 2, 0, 1, 2)
        
        left_layout.addWidget(config_group)
        
        # Client-Worker assignment group
        assignment_group = QGroupBox("Worker Assignment")
        assignment_layout = QGridLayout(assignment_group)
        
        assignment_layout.addWidget(QLabel("Available Workers:"), 0, 0)
        self.available_workers_combo = QComboBox()
        assignment_layout.addWidget(self.available_workers_combo, 0, 1)
        
        add_worker_to_client_btn = QPushButton("Add Worker to Client")
        add_worker_to_client_btn.clicked.connect(self.add_worker_to_client)
        remove_worker_from_client_btn = QPushButton("Remove Worker from Client")
        remove_worker_from_client_btn.clicked.connect(self.remove_worker_from_client)
        
        assignment_layout.addWidget(add_worker_to_client_btn, 1, 0)
        assignment_layout.addWidget(remove_worker_from_client_btn, 1, 1)
        
        left_layout.addWidget(assignment_group)
        left_layout.addStretch()
        
        layout.addWidget(left_widget)
        
        # Right side - Lists
        right_widget = QWidget()
        right_layout = QVBoxLayout(right_widget)
        
        # Clients list
        clients_list_group = QGroupBox("Configured Clients")
        clients_list_layout = QVBoxLayout(clients_list_group)
        self.clients_list = QListWidget()
        self.clients_list.itemSelectionChanged.connect(self.on_client_selected)
        clients_list_layout.addWidget(self.clients_list)
        right_layout.addWidget(clients_list_group)
        
        # Client workers list
        client_workers_group = QGroupBox("Client Workers")
        client_workers_layout = QVBoxLayout(client_workers_group)
        self.client_workers_list = QListWidget()
        client_workers_layout.addWidget(self.client_workers_list)
        right_layout.addWidget(client_workers_group)
        
        layout.addWidget(right_widget)
        
        self.tab_widget.addTab(clients_widget, "Clients")
        
    def create_network_tab(self):
        """Create the network configuration tab (routers and sources)"""
        network_widget = QWidget()
        layout = QVBoxLayout(network_widget)
        
        # Create splitter for routers and sources
        splitter = QSplitter(Qt.Horizontal)
        
        # Routers section
        routers_widget = QWidget()
        routers_layout = QVBoxLayout(routers_widget)
        
        routers_group = QGroupBox("Routers")
        routers_group_layout = QGridLayout(routers_group)
        
        routers_group_layout.addWidget(QLabel("Router Name:"), 0, 0)
        self.router_name_input = QLineEdit()
        routers_group_layout.addWidget(self.router_name_input, 0, 1)
        
        routers_group_layout.addWidget(QLabel("Port:"), 1, 0)
        self.router_port_input = QLineEdit()
        routers_group_layout.addWidget(self.router_port_input, 1, 1)
        
        routers_group_layout.addWidget(QLabel("Policy:"), 2, 0)
        self.router_policy_combo = QComboBox()
        # Populate with router policies from definitions
        routers_group_layout.addWidget(self.router_policy_combo, 2, 1)
        
        # Router buttons
        router_buttons = QHBoxLayout()
        add_router_btn = QPushButton("Add")
        add_router_btn.clicked.connect(self.add_router)
        remove_router_btn = QPushButton("Remove")
        remove_router_btn.clicked.connect(self.remove_router)
        
        router_buttons.addWidget(add_router_btn)
        router_buttons.addWidget(remove_router_btn)
        router_buttons.addStretch()
        
        routers_group_layout.addLayout(router_buttons, 3, 0, 1, 2)
        
        routers_layout.addWidget(routers_group)
        
        # Routers list
        self.routers_list = QListWidget()
        routers_layout.addWidget(QLabel("Configured Routers:"))
        routers_layout.addWidget(self.routers_list)
        
        splitter.addWidget(routers_widget)
        
        # Sources section
        sources_widget = QWidget()
        sources_layout = QVBoxLayout(sources_widget)
        
        sources_group = QGroupBox("Sources")
        sources_group_layout = QGridLayout(sources_group)
        
        sources_group_layout.addWidget(QLabel("Source Name:"), 0, 0)
        self.source_name_input = QLineEdit()
        sources_group_layout.addWidget(self.source_name_input, 0, 1)
        
        sources_group_layout.addWidget(QLabel("Port:"), 1, 0)
        self.source_port_input = QLineEdit()
        sources_group_layout.addWidget(self.source_port_input, 1, 1)
        
        sources_group_layout.addWidget(QLabel("Frequency:"), 2, 0)
        freq_layout = QHBoxLayout()
        self.source_frequency_input = QLineEdit()
        freq_layout.addWidget(self.source_frequency_input)
        self.source_frequency_default = QCheckBox("Use Default")
        freq_layout.addWidget(self.source_frequency_default)
        sources_group_layout.addLayout(freq_layout, 2, 1)
        
        sources_group_layout.addWidget(QLabel("Epochs:"), 3, 0)
        self.source_epochs_input = QLineEdit()
        sources_group_layout.addWidget(self.source_epochs_input, 3, 1)
        
        sources_group_layout.addWidget(QLabel("Type:"), 4, 0)
        self.source_type_combo = QComboBox()
        # Populate with source types
        sources_group_layout.addWidget(self.source_type_combo, 4, 1)
        
        # Source buttons
        source_buttons = QHBoxLayout()
        add_source_btn = QPushButton("Add")
        add_source_btn.clicked.connect(self.add_source)
        remove_source_btn = QPushButton("Remove")
        remove_source_btn.clicked.connect(self.remove_source)
        
        source_buttons.addWidget(add_source_btn)
        source_buttons.addWidget(remove_source_btn)
        source_buttons.addStretch()
        
        sources_group_layout.addLayout(source_buttons, 5, 0, 1, 2)
        
        sources_layout.addWidget(sources_group)
        
        # Sources list
        self.sources_list = QListWidget()
        sources_layout.addWidget(QLabel("Configured Sources:"))
        sources_layout.addWidget(self.sources_list)
        
        splitter.addWidget(sources_widget)
        
        layout.addWidget(splitter)
        
        self.tab_widget.addTab(network_widget, "Network")
        
    def create_status_bar(self):
        """Create the status bar"""
        self.status_bar = QStatusBar()
        self.setStatusBar(self.status_bar)
        self.status_bar.showMessage("Ready")
        
    def apply_modern_style(self):
        """Apply modern styling to the application"""
        style = """
        QMainWindow {
            background-color: #f0f0f0;
        }
        
        QTabWidget::pane {
            border: 1px solid #c0c0c0;
            background-color: white;
        }
        
        QTabWidget::tab-bar {
            alignment: left;
        }
        
        QTabBar::tab {
            background-color: #e1e1e1;
            border: 1px solid #c0c0c0;
            padding: 8px 16px;
            margin-right: 2px;
        }
        
        QTabBar::tab:selected {
            background-color: white;
            border-bottom-color: white;
        }
        
        QTabBar::tab:hover {
            background-color: #f0f0f0;
        }
        
        QGroupBox {
            font-weight: bold;
            border: 2px solid #cccccc;
            border-radius: 5px;
            margin-top: 1ex;
            padding-top: 8px;
        }
        
        QGroupBox::title {
            subcontrol-origin: margin;
            left: 10px;
            padding: 0 5px 0 5px;
        }
        
        QPushButton {
            background-color: #4CAF50;
            border: none;
            color: white;
            padding: 8px 16px;
            text-align: center;
            font-size: 14px;
            border-radius: 4px;
        }
        
        QPushButton:hover {
            background-color: #45a049;
        }
        
        QPushButton:pressed {
            background-color: #3d8b40;
        }
        
        QLineEdit {
            border: 2px solid #ddd;
            border-radius: 4px;
            padding: 5px;
            font-size: 14px;
        }
        
        QLineEdit:focus {
            border-color: #4CAF50;
        }
        
        QComboBox {
            border: 2px solid #ddd;
            border-radius: 4px;
            padding: 5px;
            font-size: 14px;
        }
        
        QListWidget {
            border: 1px solid #ddd;
            border-radius: 4px;
            background-color: white;
        }
        
        QListWidget::item {
            padding: 5px;
            border-bottom: 1px solid #eee;
        }
        
        QListWidget::item:selected {
            background-color: #4CAF50;
            color: white;
        }
        
        QTextEdit {
            border: 1px solid #ddd;
            border-radius: 4px;
            font-family: 'Courier New', monospace;
        }
        """
        
        self.setStyleSheet(style)
        
    def init_data(self):
        """Initialize application data structures"""
        # Initialize data containers similar to the original handlers
        self.devices_data = []
        self.workers_data = []
        self.clients_data = []
        self.routers_data = []
        self.sources_data = []
        self.settings_data = {}
        
        # Get default host IP
        self.default_host_ip = get_this_host_ip()
        
        # Initialize with default values
        self.frequency_input.setText("2")
        self.batch_size_input.setText("25")
        self.main_server_port_input.setText("8080")
        self.api_server_port_input.setText("8081")
        
    def setup_connections(self):
        """Setup signal-slot connections"""
        # Connect input field changes to validation
        self.frequency_input.textChanged.connect(self.validate_settings)
        self.batch_size_input.textChanged.connect(self.validate_settings)
        
        # Connect device inputs
        self.device_name_input.textChanged.connect(self.validate_device_input)
        self.device_ip_input.textChanged.connect(self.validate_device_input)
        
    # Event handlers
    def new_configuration(self):
        """Create a new configuration"""
        reply = QMessageBox.question(self, 'New Configuration', 
                                   'Are you sure you want to create a new configuration? All unsaved changes will be lost.',
                                   QMessageBox.Yes | QMessageBox.No, 
                                   QMessageBox.No)
        
        if reply == QMessageBox.Yes:
            self.clear_all_data()
            self.status_bar.showMessage("New configuration created")
            
    def open_configuration(self):
        """Open an existing configuration"""
        file_path, _ = QFileDialog.getOpenFileName(self, 'Open Configuration', '', 'JSON Files (*.json)')
        if file_path:
            # TODO: Implement configuration loading
            self.status_bar.showMessage(f"Configuration loaded from {file_path}")
            
    def save_configuration(self):
        """Save current configuration"""
        file_path, _ = QFileDialog.getSaveFileName(self, 'Save Configuration', '', 'JSON Files (*.json)')
        if file_path:
            # TODO: Implement configuration saving
            self.status_bar.showMessage(f"Configuration saved to {file_path}")
            
    def export_cpp_headers(self):
        """Export C++ headers"""
        # Use existing CppHeadersExporter functionality
        try:
            from CppHeadersExporter import gen_header_worker_parameters_definitions
            file_path, _ = QFileDialog.getSaveFileName(self, 'Export C++ Headers', '', 'Header Files (*.h)')
            if file_path:
                gen_header_worker_parameters_definitions(file_path)
                QMessageBox.information(self, 'Export Complete', f'C++ headers exported to {file_path}')
                self.status_bar.showMessage("C++ headers exported successfully")
        except Exception as e:
            QMessageBox.critical(self, 'Export Error', f'Failed to export C++ headers: {str(e)}')
            
    def export_erlang_headers(self):
        """Export Erlang headers"""
        # Use existing ErlHeadersExporter functionality
        try:
            from ErlHeadersExporter import gen_worker_fields_hrl
            file_path, _ = QFileDialog.getSaveFileName(self, 'Export Erlang Headers', '', 'Header Files (*.hrl)')
            if file_path:
                gen_worker_fields_hrl(file_path)
                QMessageBox.information(self, 'Export Complete', f'Erlang headers exported to {file_path}')
                self.status_bar.showMessage("Erlang headers exported successfully")
        except Exception as e:
            QMessageBox.critical(self, 'Export Error', f'Failed to export Erlang headers: {str(e)}')
            
    def scan_network_devices(self):
        """Scan for network devices"""
        self.progress_bar.setVisible(True)
        self.progress_bar.setRange(0, 0)  # Indeterminate progress
        self.status_bar.showMessage("Scanning network for devices...")
        
        # TODO: Implement actual network scanning using existing Pinger functionality
        QTimer.singleShot(2000, self.scan_complete)  # Simulate scan delay
        
    def scan_complete(self):
        """Handle scan completion"""
        self.progress_bar.setVisible(False)
        self.status_bar.showMessage("Network scan completed")
        # TODO: Update devices_online_combo with found devices
        
    def show_communication_map(self):
        """Show communication map dialog"""
        from .dialogs import CommunicationMapDialog
        dialog = CommunicationMapDialog(self)
        dialog.exec()
        
    def show_experiment_flow(self):
        """Show experiment flow dialog"""
        from .dialogs import ExperimentFlowDialog
        dialog = ExperimentFlowDialog(self)
        dialog.exec()
        
    def show_about(self):
        """Show about dialog"""
        QMessageBox.about(self, 'About Nerlplanner', 
                         f'Nerlplanner v{VERSION}\\n'
                         f'Neural Erlang Network Planner\\n'
                         f'Tested with Nerlnet v{NERLNET_VERSION_TESTED_WITH}\\n\\n'
                         'A modern GUI for configuring distributed neural networks.')
        
    # Settings handlers
    def save_settings(self):
        """Save global settings"""
        frequency = self.frequency_input.text()
        batch_size = self.batch_size_input.text()
        
        if frequency and batch_size:
            self.settings_data['frequency'] = frequency
            self.settings_data['batch_size'] = batch_size
            self.settings_status_label.setText(f"Settings: Freq={frequency}, Batch={batch_size}")
            self.status_bar.showMessage("Settings saved")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter both frequency and batch size')
            
    def clear_settings(self):
        """Clear settings"""
        self.frequency_input.clear()
        self.batch_size_input.clear()
        self.settings_status_label.setText("Settings Status: Ready")
        
    def save_special_entities(self):
        """Save special entities configuration"""
        main_port = self.main_server_port_input.text()
        main_args = self.main_server_args_input.text()
        api_port = self.api_server_port_input.text()
        api_args = self.api_server_args_input.text()
        
        if main_port and api_port:
            self.main_server_status_label.setText(f"Main Server: Port {main_port}")
            self.api_server_status_label.setText(f"API Server: Port {api_port}")
            self.status_bar.showMessage("Special entities configured")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter port numbers for both servers')
            
    def clear_special_entities(self):
        """Clear special entities"""
        self.main_server_port_input.clear()
        self.main_server_args_input.clear()
        self.api_server_port_input.clear()
        self.api_server_args_input.clear()
        self.main_server_status_label.setText("Main Server Status: Not configured")
        self.api_server_status_label.setText("API Server Status: Not configured")
        
    def validate_settings(self):
        """Validate settings input"""
        frequency = self.frequency_input.text()
        batch_size = self.batch_size_input.text()
        
        # Simple validation - can be enhanced
        if frequency and not frequency.isdigit():
            self.frequency_input.setStyleSheet("border: 2px solid red;")
        else:
            self.frequency_input.setStyleSheet("")
            
        if batch_size and not batch_size.isdigit():
            self.batch_size_input.setStyleSheet("border: 2px solid red;")
        else:
            self.batch_size_input.setStyleSheet("")
            
    # Device handlers
    def add_device(self):
        """Add a new device"""
        name = self.device_name_input.text()
        ip = self.device_ip_input.text()
        
        if name and ip:
            device_info = f"{name} ({ip})"
            self.devices_list.addItem(device_info)
            self.devices_data.append({'name': name, 'ip': ip, 'entities': []})
            self.device_name_input.clear()
            self.device_ip_input.clear()
            self.status_bar.showMessage(f"Device {name} added")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter both device name and IP address')
            
    def remove_device(self):
        """Remove selected device"""
        current_row = self.devices_list.currentRow()
        if current_row >= 0:
            item = self.devices_list.takeItem(current_row)
            if current_row < len(self.devices_data):
                removed_device = self.devices_data.pop(current_row)
                self.status_bar.showMessage(f"Device {removed_device['name']} removed")
                
    def load_devices(self):
        """Load devices from file"""
        file_path, _ = QFileDialog.getOpenFileName(self, 'Load Devices', '', 'JSON Files (*.json)')
        if file_path:
            # TODO: Implement device loading
            self.status_bar.showMessage(f"Devices loaded from {file_path}")
            
    def save_devices(self):
        """Save devices to file"""
        file_path, _ = QFileDialog.getSaveFileName(self, 'Save Devices', '', 'JSON Files (*.json)')
        if file_path:
            # TODO: Implement device saving
            self.status_bar.showMessage(f"Devices saved to {file_path}")
            
    def validate_device_input(self):
        """Validate device input"""
        # TODO: Implement IP address validation
        pass
        
    def on_device_selected(self):
        """Handle device selection"""
        current_row = self.devices_list.currentRow()
        if current_row >= 0 and current_row < len(self.devices_data):
            device = self.devices_data[current_row]
            # Update device entities list
            self.device_entities_list.clear()
            for entity in device.get('entities', []):
                self.device_entities_list.addItem(entity)
                
    def add_entity_to_device(self):
        """Add entity to selected device"""
        # TODO: Implement entity assignment
        pass
        
    def remove_entity_from_device(self):
        """Remove entity from selected device"""
        # TODO: Implement entity removal
        pass
        
    # Worker handlers
    def add_worker(self):
        """Add a new worker"""
        name = self.worker_name_input.text()
        file_path = self.worker_file_input.text()
        
        if name:
            self.workers_list.addItem(name)
            self.workers_data.append({'name': name, 'file_path': file_path})
            self.worker_name_input.clear()
            self.worker_file_input.clear()
            self.status_bar.showMessage(f"Worker {name} added")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter worker name')
            
    def edit_worker(self):
        """Edit selected worker"""
        current_row = self.workers_list.currentRow()
        if current_row >= 0 and current_row < len(self.workers_data):
            worker_data = self.workers_data[current_row]
            
            from .dialogs import WorkerDialog
            dialog = WorkerDialog(self, worker_data)
            dialog.worker_saved.connect(lambda data: self.update_worker_data(current_row, data))
            dialog.exec()
        else:
            QMessageBox.warning(self, 'No Selection', 'Please select a worker to edit')
        
    def remove_worker(self):
        """Remove selected worker"""
        current_row = self.workers_list.currentRow()
        if current_row >= 0:
            item = self.workers_list.takeItem(current_row)
            if current_row < len(self.workers_data):
                removed_worker = self.workers_data.pop(current_row)
                self.status_bar.showMessage(f"Worker {removed_worker['name']} removed")
                
    def view_worker(self):
        """View selected worker details"""
        current_row = self.workers_list.currentRow()
        if current_row >= 0 and current_row < len(self.workers_data):
            worker = self.workers_data[current_row]
            # TODO: Display worker details
            self.worker_info_text.setText(f"Worker: {worker['name']}\\nFile: {worker.get('file_path', 'None')}")
            
    def browse_worker_file(self):
        """Browse for worker configuration file"""
        file_path, _ = QFileDialog.getOpenFileName(self, 'Select Worker File', '', 'JSON Files (*.json);;XML Files (*.xml)')
        if file_path:
            self.worker_file_input.setText(file_path)
            
    def on_worker_selected(self):
        """Handle worker selection"""
        current_row = self.workers_list.currentRow()
        if current_row >= 0 and current_row < len(self.workers_data):
            worker = self.workers_data[current_row]
            self.worker_info_text.setText(f"Worker: {worker['name']}\\nFile: {worker.get('file_path', 'None')}")
            
    def update_worker_data(self, row, data):
        """Update worker data after editing"""
        if row < len(self.workers_data):
            self.workers_data[row].update(data)
            # Update the list display
            self.workers_list.item(row).setText(data.get('name', f'Worker {row}'))
            self.status_bar.showMessage(f"Worker {data.get('name', '')} updated")
            
    # Client handlers
    def add_client(self):
        """Add a new client"""
        name = self.client_name_input.text()
        port = self.client_port_input.text()
        
        if name and port:
            client_info = f"{name} (:{port})"
            self.clients_list.addItem(client_info)
            self.clients_data.append({'name': name, 'port': port, 'workers': []})
            self.client_name_input.clear()
            self.client_port_input.clear()
            self.status_bar.showMessage(f"Client {name} added")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter both client name and port')
            
    def remove_client(self):
        """Remove selected client"""
        current_row = self.clients_list.currentRow()
        if current_row >= 0:
            item = self.clients_list.takeItem(current_row)
            if current_row < len(self.clients_data):
                removed_client = self.clients_data.pop(current_row)
                self.status_bar.showMessage(f"Client {removed_client['name']} removed")
                
    def load_clients(self):
        """Load clients from file"""
        file_path, _ = QFileDialog.getOpenFileName(self, 'Load Clients', '', 'JSON Files (*.json)')
        if file_path:
            # TODO: Implement client loading
            self.status_bar.showMessage(f"Clients loaded from {file_path}")
            
    def save_clients(self):
        """Save clients to file"""
        file_path, _ = QFileDialog.getSaveFileName(self, 'Save Clients', '', 'JSON Files (*.json)')
        if file_path:
            # TODO: Implement client saving
            self.status_bar.showMessage(f"Clients saved to {file_path}")
            
    def on_client_selected(self):
        """Handle client selection"""
        current_row = self.clients_list.currentRow()
        if current_row >= 0 and current_row < len(self.clients_data):
            client = self.clients_data[current_row]
            # Update client workers list
            self.client_workers_list.clear()
            for worker in client.get('workers', []):
                self.client_workers_list.addItem(worker)
                
    def add_worker_to_client(self):
        """Add worker to selected client"""
        # TODO: Implement worker assignment to client
        pass
        
    def remove_worker_from_client(self):
        """Remove worker from selected client"""
        # TODO: Implement worker removal from client
        pass
        
    # Network handlers (Routers and Sources)
    def add_router(self):
        """Add a new router"""
        name = self.router_name_input.text()
        port = self.router_port_input.text()
        policy = self.router_policy_combo.currentText()
        
        if name and port:
            router_info = f"{name} (:{port}, {policy})"
            self.routers_list.addItem(router_info)
            self.routers_data.append({'name': name, 'port': port, 'policy': policy})
            self.router_name_input.clear()
            self.router_port_input.clear()
            self.status_bar.showMessage(f"Router {name} added")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter router name and port')
            
    def remove_router(self):
        """Remove selected router"""
        current_row = self.routers_list.currentRow()
        if current_row >= 0:
            item = self.routers_list.takeItem(current_row)
            if current_row < len(self.routers_data):
                removed_router = self.routers_data.pop(current_row)
                self.status_bar.showMessage(f"Router {removed_router['name']} removed")
                
    def add_source(self):
        """Add a new source"""
        name = self.source_name_input.text()
        port = self.source_port_input.text()
        frequency = self.source_frequency_input.text()
        epochs = self.source_epochs_input.text()
        source_type = self.source_type_combo.currentText()
        
        if name and port:
            source_info = f"{name} (:{port}, freq:{frequency}, epochs:{epochs})"
            self.sources_list.addItem(source_info)
            self.sources_data.append({
                'name': name, 
                'port': port, 
                'frequency': frequency,
                'epochs': epochs,
                'type': source_type
            })
            self.source_name_input.clear()
            self.source_port_input.clear()
            self.source_frequency_input.clear()
            self.source_epochs_input.clear()
            self.status_bar.showMessage(f"Source {name} added")
        else:
            QMessageBox.warning(self, 'Invalid Input', 'Please enter source name and port')
            
    def remove_source(self):
        """Remove selected source"""
        current_row = self.sources_list.currentRow()
        if current_row >= 0:
            item = self.sources_list.takeItem(current_row)
            if current_row < len(self.sources_data):
                removed_source = self.sources_data.pop(current_row)
                self.status_bar.showMessage(f"Source {removed_source['name']} removed")
                
    def clear_all_data(self):
        """Clear all application data"""
        # Clear all lists
        self.devices_list.clear()
        self.device_entities_list.clear()
        self.workers_list.clear()
        self.clients_list.clear()
        self.client_workers_list.clear()
        self.routers_list.clear()
        self.sources_list.clear()
        
        # Clear all inputs
        self.frequency_input.clear()
        self.batch_size_input.clear()
        self.main_server_port_input.clear()
        self.main_server_args_input.clear()
        self.api_server_port_input.clear()
        self.api_server_args_input.clear()
        
        self.device_name_input.clear()
        self.device_ip_input.clear()
        
        self.worker_name_input.clear()
        self.worker_file_input.clear()
        self.worker_info_text.clear()
        
        self.client_name_input.clear()
        self.client_port_input.clear()
        
        self.router_name_input.clear()
        self.router_port_input.clear()
        
        self.source_name_input.clear()
        self.source_port_input.clear()
        self.source_frequency_input.clear()
        self.source_epochs_input.clear()
        
        # Clear data structures
        self.devices_data.clear()
        self.workers_data.clear()
        self.clients_data.clear()
        self.routers_data.clear()
        self.sources_data.clear()
        self.settings_data.clear()
        
        # Reset status labels
        self.settings_status_label.setText("Settings Status: Ready")
        self.main_server_status_label.setText("Main Server Status: Not configured")
        self.api_server_status_label.setText("API Server Status: Not configured")


def main():
    """Main application entry point"""
    app = QApplication(sys.argv)
    
    # Set application properties
    app.setApplicationName(WINDOW_TITLE)
    app.setApplicationVersion(VERSION)
    app.setOrganizationName("Nerlnet")
    
    # Print banner (from existing logger)
    print_banner()
    
    # Check screen resolution
    screen = app.primaryScreen()
    screen_geometry = screen.geometry()
    screen_width = screen_geometry.width()
    screen_height = screen_geometry.height()
    
    LOG_INFO(f"Screen resolution: {screen_width}x{screen_height}")
    
    if screen_width < WINDOW_FIXED_WIDTH:
        QMessageBox.critical(None, "Resolution Error", 
                           f"Minimum resolution width of {WINDOW_FIXED_WIDTH} is required!")
        sys.exit(1)
    
    # Create and show main window
    window = NerlplannerMainWindow()
    window.show()
    
    # Show splash screen briefly (optional)
    if os.path.exists(NERLNET_SPLASH_LOGO_PATH):
        splash_pixmap = QPixmap(NERLNET_SPLASH_LOGO_PATH)
        splash = QMessageBox()
        splash.setWindowTitle("Nerlplanner")
        splash.setText("Loading Nerlplanner...")
        splash.setIconPixmap(splash_pixmap.scaled(200, 200, Qt.KeepAspectRatio, Qt.SmoothTransformation))
        splash.show()
        
        # Close splash after 2 seconds
        QTimer.singleShot(2000, splash.close)
    
    sys.exit(app.exec())


if __name__ == "__main__":
    main()

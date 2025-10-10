#!/usr/bin/env python3
"""
NerlDesigner - Modern Web-based Designer for NerlNet
Built with NiceGUI for cross-platform comp                # Connection Configuration  
                self.create_json_section(
                    title='Connection Map',
                    description='Device connections and network topology', 
                    file_type='connection',
                    prefixes=['conn', 'connection', 'topology'],
                    color='red-700'
                )y
"""

import sys
import os
import subprocess
import signal
import re
import select
from pathlib import Path
from typing import Dict, Any, List, Optional
import json
import asyncio

# Add the project root to the path for imports
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

from nicegui import ui, app, run

# Import NerlNet definitions from autogen
try:
    from src_py.autogen.Definitions import VERSION as NERLPLANNER_VERSION
    from src_py.autogen.JsonElementsDefinitions import *
    from src_py.autogen.JsonElementWorkerDefinitions import *
    from src_py.autogen.JsonDistributedConfigDefs import *
except ImportError:
    # Fallback to relative imports
    sys.path.insert(0, str(Path(__file__).parent.parent / "autogen"))
    try:
        from Definitions import VERSION as NERLPLANNER_VERSION
        from JsonElementsDefinitions import *
        from JsonElementWorkerDefinitions import *
        from JsonDistributedConfigDefs import *
    except ImportError:
        # If autogen imports fail, use defaults
        NERLPLANNER_VERSION = "1.0.0"

# Import local modules
from models.worker_model import WorkerModel
from models.connection_model import ConnectionModel
from models.distributed_config_model import DistributedConfigModel
from components.worker_designer import WorkerDesigner
from components.connection_designer import ConnectionDesigner
from components.experiment_designer import ExperimentDesigner
from components.dc_designer import DCDesigner
from components.graph_visualizer import GraphVisualizer
from utils.json_handler import JsonHandler

class NerlDesigner:
    """Main NerlDesigner application class"""
    
    def __init__(self):
        self.current_project: Dict[str, Any] = {}
        self.worker_model = WorkerModel()
        self.connection_model = ConnectionModel() 
        self.distributed_config_model = DistributedConfigModel()
        self.json_handler = JsonHandler()
        
        # UI Components
        self.worker_designer: Optional[WorkerDesigner] = None
        self.connection_designer: Optional[ConnectionDesigner] = None
        self.dc_designer: Optional[DCDesigner] = None
        self.experiment_designer = ExperimentDesigner()
        self.graph_visualizer: Optional[GraphVisualizer] = None
        
        # Jupyter Lab process management
        self.jupyter_process: Optional[subprocess.Popen] = None
        self.jupyter_status_label: Optional[ui.label] = None
        self.jupyter_url: Optional[str] = None
        self.loading_dialog = None
        self.jupyter_startup_timeout = 0
        
        # Setup paths
        self.project_root = project_root
        self.examples_path = project_root / "inputJsonsFiles"
        
    def setup_ui(self):
        """Setup the main UI layout"""
        # Configure static files
        app.add_static_files('/static', str(Path(__file__).parent / 'static'))
        app.add_static_files('/assets', str(Path(__file__).parent / 'assets'))
        
        # Create the main page
        @ui.page('/')
        def index():
            self.create_main_page()
    
    def create_main_page(self):
        """Create the main page content"""
        
        # Header with logo and title - White Background Theme
        with ui.header().classes('bg-white text-gray-800 shadow-lg border-b border-gray-200'):
            with ui.row().classes('w-full items-center px-4'):
                # Logo and title section
                with ui.row().classes('items-center gap-4'):
                    logo_path = Path(__file__).parent / 'assets' / 'logo' / 'nerlnet_logo.png'
                    if logo_path.exists():
                        ui.image('/assets/logo/nerlnet_logo.png').classes('h-12 w-12')
                    else:
                        ui.icon('account_tree', size='3rem').classes('text-red-900')
                    with ui.column().classes('gap-0'):
                        ui.label('NerlDesigner').classes('text-h4 font-bold text-red-900')
                        ui.label('Neural Network Configuration Tool').classes('text-caption text-gray-600 opacity-80')
                
                ui.space()
                
        # File Management Toolbar with Dark Red Theme
        with ui.row().classes('w-full bg-red-900 px-4 py-3 shadow-lg justify-between items-center'):
            # Left side - Project and Jupyter controls
            with ui.row().classes('gap-3 items-center'):
                # New Project button - Black
                ui.button('New Project', 
                         on_click=self.new_project).props('unelevated').classes('bg-black hover:bg-gray-800 text-white')
                
                # Jupyter Lab controls
                ui.separator().props('vertical').classes('bg-red-600')
                ui.button('Launch Jupyter Lab', 
                         on_click=self.launch_jupyter_lab_with_dialog).props('unelevated').classes('bg-black hover:bg-gray-800 text-white')
                ui.button('Stop Jupyter Lab', 
                         on_click=self.stop_jupyter_lab).props('unelevated').classes('bg-black hover:bg-gray-800 text-white')
                
                # Jupyter status
                self.jupyter_status_label = ui.label('Jupyter: Not Running').classes('text-white text-sm opacity-80')
            
            # Right side - Status and version info
            with ui.row().classes('gap-4 items-center'):
                self.status_label = ui.label('Ready').classes('text-white text-sm')
                ui.separator().props('vertical').classes('bg-red-600')
                ui.label(f'NerlDesigner v{NERLPLANNER_VERSION}').classes('text-white text-sm opacity-80')
        
        # Dedicated JSON Import/Export Sections
        with ui.column().classes('w-full p-4 bg-red-50'):
            ui.label('JSON Configuration Manager').classes('text-h5 font-bold text-red-900 mb-4')
            
            # Grid layout for different file types
            with ui.grid(columns=2).classes('gap-6 w-full'):
                # Worker Configuration
                self.create_json_section(
                    title='Worker Configuration', 
                    description='Neural network architecture and training parameters',
                    file_type='worker',
                    prefixes=['worker', 'neural', 'nn'],
                    color='red-800'
                )
                
                # Connection Configuration  
                self.create_json_section(
                    title='� Connection Map',
                    description='Device connections and network topology', 
                    file_type='connection',
                    prefixes=['conn', 'connection', 'topology'],
                    color='red-700'
                )
                
                # Distributed Configuration
                self.create_json_section(
                    title='Distributed Config',
                    description='Distributed system settings and parameters',
                    file_type='distributed', 
                    prefixes=['dc', 'dist', 'distributed'],
                    color='red-600'
                )
                
                # Experiment Configuration
                self.create_json_section(
                    title='Experiment Setup',
                    description='Experiment parameters and configurations',
                    file_type='experiment',
                    prefixes=['exp', 'experiment', 'test'],
                    color='red-500'
                )
        
        # Main content area with dark red themed tabs
        with ui.column().classes('flex-1 w-full'):
            with ui.tabs().classes('w-full bg-red-900 shadow-lg') as tabs:
                worker_tab = ui.tab('Worker Designer', icon='psychology').classes('text-white hover:bg-red-800')
                connection_tab = ui.tab('Connections', icon='share').classes('text-white hover:bg-red-800')
                dc_tab = ui.tab('Distributed Config', icon='settings_ethernet').classes('text-white hover:bg-red-800')
                graph_tab = ui.tab('Graph View', icon='account_tree').classes('text-white hover:bg-red-800')
                experiment_tab = ui.tab('Experiments', icon='science').classes('text-white hover:bg-red-800')
            
            with ui.tab_panels(tabs, value=worker_tab).classes('flex-1 w-full bg-red-50'):
                # Worker Designer Tab
                with ui.tab_panel(worker_tab).classes('p-0'):
                    self.worker_designer = WorkerDesigner(self.worker_model)
                    self.worker_designer.create_ui()
                    
                    # Load any pending worker data
                    if hasattr(self, 'pending_worker_data'):
                        self.worker_designer.load_from_json(self.pending_worker_data)
                        delattr(self, 'pending_worker_data')
                
                # Connection Designer Tab  
                with ui.tab_panel(connection_tab).classes('p-0'):
                    self.connection_designer = ConnectionDesigner(self.connection_model)
                    self.connection_designer.create_ui()
                
                # Distributed Configuration Tab
                with ui.tab_panel(dc_tab).classes('p-0'):
                    self.dc_designer = DCDesigner(self.distributed_config_model)
                    self.dc_designer.create_ui()
                
                # Graph Visualization Tab
                with ui.tab_panel(graph_tab).classes('p-0'):
                    self.graph_visualizer = GraphVisualizer()
                    self.graph_visualizer.create_ui()
                    
                # Experiment Configuration Tab
                with ui.tab_panel(experiment_tab).classes('p-0'):
                    self.create_experiment_ui()
    
    def create_experiment_ui(self):
        """Create the experiment configuration UI"""
        self.experiment_designer.create_ui()
    
    def new_project(self):
        """Create a new project"""
        self.current_project = {}
        # Reset all models to default values
        if hasattr(self.worker_model, 'reset'):
            self.worker_model.reset()
        if hasattr(self.connection_model, 'reset'):
            self.connection_model.reset()
        if hasattr(self.distributed_config_model, 'reset'):
            self.distributed_config_model.reset()
        
        # Update status
        self.update_status('New project created')
        ui.notify('New project created successfully!', type='positive')
    
    def launch_jupyter_lab(self):
        """Launch Jupyter Lab server"""
        try:
            # Check if Jupyter is already running
            if self.jupyter_process and self.jupyter_process.poll() is None:
                ui.notify('Jupyter Lab is already running!', type='warning')
                return
            
            # Look for the Jupyter Lab launch script
            jupyter_script = self.project_root / "NerlnetJupyterLaunch.sh"
            
            if not jupyter_script.exists():
                ui.notify('Jupyter Lab launch script not found!', type='negative')
                return
            
            # Make script executable
            os.chmod(jupyter_script, 0o755)
            
            # Launch Jupyter Lab in background with text output buffering disabled
            self.jupyter_process = subprocess.Popen(
                [str(jupyter_script)],
                cwd=str(self.project_root),
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,  # Combine stderr with stdout
                universal_newlines=True,
                bufsize=1,  # Line buffered
                preexec_fn=os.setsid  # Create new process group
            )
            
            # Update UI
            self.jupyter_status_label.text = 'Jupyter: Starting...'
            self.update_status('Launching Jupyter Lab...')
            ui.notify('Jupyter Lab is starting up...', type='positive')
            
            # Start monitoring for URL with faster polling
            ui.timer(0.5, self.monitor_jupyter_startup, once=False)
            
        except Exception as e:
            ui.notify(f'Failed to launch Jupyter Lab: {str(e)}', type='negative')
            self.jupyter_status_label.text = 'Jupyter: Error'
            
            # Close loading dialog if it exists
            if hasattr(self, 'loading_dialog') and self.loading_dialog:
                try:
                    self.loading_dialog.close()
                except:
                    pass
    
    def stop_jupyter_lab(self):
        """Stop Jupyter Lab server"""
        try:
            if not self.jupyter_process or self.jupyter_process.poll() is not None:
                ui.notify('Jupyter Lab is not running', type='warning')
                self.jupyter_status_label.text = 'Jupyter: Not Running'
                return
            
            # Terminate the process group to kill all child processes
            os.killpg(os.getpgid(self.jupyter_process.pid), signal.SIGTERM)
            
            # Wait for process to terminate, then force kill if needed
            try:
                self.jupyter_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(os.getpgid(self.jupyter_process.pid), signal.SIGKILL)
                self.jupyter_process.wait()
            
            self.jupyter_process = None
            self.jupyter_url = None
            self.jupyter_status_label.text = 'Jupyter: Stopped'
            self.update_status('Jupyter Lab stopped')
            ui.notify('Jupyter Lab stopped successfully', type='positive')
            
        except Exception as e:
            ui.notify(f'Failed to stop Jupyter Lab: {str(e)}', type='negative')
            self.jupyter_status_label.text = 'Jupyter: Error'
    
    def monitor_jupyter_startup(self):
        """Monitor Jupyter startup and extract URL"""
        try:
            if not self.jupyter_process:
                return False  # Stop the timer
            
            # Increment timeout counter (0.5 second intervals)
            self.jupyter_startup_timeout += 0.5
            print(f"DEBUG: Jupyter startup monitoring, timeout: {self.jupyter_startup_timeout}s")
            
            # Timeout after 60 seconds - show fallback URL
            if self.jupyter_startup_timeout >= 60:
                print("DEBUG: Jupyter startup timeout reached, showing fallback URL")
                self.jupyter_url = "http://localhost:8888/lab"
                self.jupyter_status_label.text = 'Jupyter: Running (fallback)'
                self.update_status('Jupyter Lab running (using default URL)')
                ui.notify('Jupyter Lab timeout - using default URL', type='warning')
                
                # Close loading dialog and show URL dialog
                if hasattr(self, 'loading_dialog') and self.loading_dialog:
                    try:
                        self.loading_dialog.close()
                    except:
                        pass
                
                self.show_jupyter_url_dialog()
                return False  # Stop the timer
            
            if self.jupyter_process.poll() is not None:
                # Process has stopped
                if self.jupyter_process and self.jupyter_process.returncode != 0:
                    ui.notify(f'Jupyter Lab failed to start (exit code: {self.jupyter_process.returncode})', type='negative')
                    self.jupyter_status_label.text = 'Jupyter: Error'
                else:
                    self.jupyter_status_label.text = 'Jupyter: Not Running'
                
                # Close loading dialog if it exists
                if hasattr(self, 'loading_dialog') and self.loading_dialog:
                    try:
                        self.loading_dialog.close()
                    except:
                        pass
                    
                return False  # Stop the timer
            
            # Read all available output lines at once for efficiency
            if self.jupyter_process.stdout:
                try:
                    # Use non-blocking read with select
                    if select.select([self.jupyter_process.stdout], [], [], 0)[0]:
                        # Read multiple lines if available
                        lines = []
                        while True:
                            try:
                                if select.select([self.jupyter_process.stdout], [], [], 0)[0]:
                                    line = self.jupyter_process.stdout.readline()
                                    if line:
                                        lines.append(line.strip())
                                    else:
                                        break
                                else:
                                    break
                            except:
                                break
                        
                        # Process all lines for URL patterns (with debugging)
                        for line in lines:
                            if line:
                                print(f"DEBUG: Jupyter output line: {line}")
                                # Look for URL patterns in the output
                                if ('http://' in line or 'https://' in line) and ('lab' in line or 'token' in line):
                                    print(f"DEBUG: Found potential URL line: {line}")
                                    # Extract URL from line
                                    url_match = re.search(r'(https?://[^\s]+)', line)
                                    if url_match:
                                        self.jupyter_url = url_match.group(1)
                                        # Clean up the URL (remove any trailing characters)
                                        self.jupyter_url = self.jupyter_url.rstrip('/')
                                        
                                        self.jupyter_status_label.text = 'Jupyter: Running'
                                        self.update_status(f'Jupyter Lab running')
                                        ui.notify(f'Jupyter Lab is ready!', type='positive')
                                        print(f"DEBUG: Found Jupyter URL: {self.jupyter_url}")
                                        
                                        # Close loading dialog if it exists
                                        if hasattr(self, 'loading_dialog') and self.loading_dialog:
                                            try:
                                                self.loading_dialog.close()
                                            except:
                                                pass
                                        
                                        # Show the URL dialog automatically
                                        self.show_jupyter_url_dialog()
                                        return False  # Stop the timer
                                # Also look for simpler patterns that might indicate the server is ready
                                elif 'jupyter lab' in line.lower() and 'running' in line.lower():
                                    print(f"DEBUG: Found Jupyter running indication: {line}")
                                elif 'server is running' in line.lower():
                                    print(f"DEBUG: Found server running indication: {line}")
                                elif 'localhost' in line.lower() or '127.0.0.1' in line:
                                    print(f"DEBUG: Found localhost reference: {line}")
                except:
                    pass  # Continue monitoring
            
            return True  # Continue monitoring
            
        except Exception as e:
            self.jupyter_status_label.text = 'Jupyter: Error'
            ui.notify(f'Error monitoring Jupyter startup: {str(e)}', type='negative')
            
            # Close loading dialog if it exists
            if hasattr(self, 'loading_dialog') and self.loading_dialog:
                try:
                    self.loading_dialog.close()
                except:
                    pass
            
            return False  # Stop the timer
    
    def launch_jupyter_lab_with_dialog(self):
        """Launch Jupyter Lab and show dialog immediately with default URL"""
        try:
            # Show loading dialog
            self.show_loading_dialog()
            
            # Launch Jupyter Lab in background
            jupyter_script = self.project_root / "NerlnetJupyterLaunch.sh"
            
            if not jupyter_script.exists():
                ui.notify('Jupyter Lab launch script not found!', type='negative')
                if hasattr(self, 'loading_dialog') and self.loading_dialog:
                    self.loading_dialog.close()
                return
            
            # Make script executable
            os.chmod(jupyter_script, 0o755)
            
            # Launch Jupyter Lab with output redirected to log file
            log_file = self.project_root / "jupyter_output.log" 
            with open(log_file, 'w') as log:
                self.jupyter_process = subprocess.Popen(
                    [str(jupyter_script)],
                    cwd=str(self.project_root),
                    stdout=log,
                    stderr=subprocess.STDOUT,
                    universal_newlines=True,
                    preexec_fn=os.setsid
                )
            
            # Update status
            self.jupyter_status_label.text = 'Jupyter: Starting...'
            self.update_status('Launching Jupyter Lab...')
            
            # Wait longer for Jupyter to fully start up and write output
            ui.timer(15.0, self.show_jupyter_dialog_after_delay, once=True)
            
        except Exception as e:
            ui.notify(f'Failed to launch Jupyter Lab: {str(e)}', type='negative')
            if hasattr(self, 'loading_dialog') and self.loading_dialog:
                self.loading_dialog.close()
    
    def show_jupyter_dialog_after_delay(self):
        """Show Jupyter dialog after a short delay with real URL detection"""
        # Try to find the real URL from the output first
        real_url = self.extract_jupyter_url_from_output()
        
        if real_url:
            self.jupyter_url = real_url
            ui.notify('Jupyter Lab is ready with real URL!', type='positive')
        else:
            # Fallback to checking common ports
            for port in [8888, 8889, 8890, 8891]:
                test_url = f"http://localhost:{port}/lab"
                self.jupyter_url = test_url
                break
            ui.notify('Jupyter Lab should be starting - check the URL!', type='info')
        
        self.jupyter_status_label.text = 'Jupyter: Running'
        self.update_status('Jupyter Lab running')
        
        # Close loading dialog
        if hasattr(self, 'loading_dialog') and self.loading_dialog:
            try:
                self.loading_dialog.close()
            except:
                pass
        
        # Show URL dialog
        self.show_jupyter_url_dialog()
    
    def extract_jupyter_url_from_output(self):
        """Extract Jupyter URL from the log file"""
        try:
            log_file = self.project_root / "jupyter_output.log"
            if log_file.exists():
                with open(log_file, 'r') as f:
                    content = f.read()
                    print(f"DEBUG: Checking log content for URL...")
                    
                    # Look for the URL pattern in the log
                    url_match = re.search(r'http://localhost:\d+/lab\?token=[a-f0-9]+', content)
                    if url_match:
                        url = url_match.group(0)
                        print(f"DEBUG: Found real Jupyter URL in log: {url}")
                        return url
        except Exception as e:
            print(f"DEBUG: Error reading log file: {e}")
            
        return None
    
    def show_jupyter_url_dialog(self):
        """Show dialog with Jupyter Lab URL and clickable link"""
        print(f"DEBUG: show_jupyter_url_dialog called with URL: {self.jupyter_url}")
        
        with ui.dialog().props('persistent') as dialog, ui.card():
            ui.label('Jupyter Lab is Ready!').classes('text-h6 font-bold mb-4')
            
            with ui.column().classes('gap-4 min-w-96'):
                ui.label('Your Jupyter Lab server is running at:').classes('text-sm text-gray-600')
                
                # URL display with copy functionality
                with ui.row().classes('w-full items-center gap-2 p-3 bg-gray-100 rounded border'):
                    if self.jupyter_url:
                        ui.label(self.jupyter_url).classes('text-sm font-mono flex-1 text-blue-600')
                        ui.button('Copy', on_click=lambda: self.copy_to_clipboard(self.jupyter_url)).props('flat dense').classes('bg-black hover:bg-gray-800 text-white text-xs')
                    else:
                        ui.label('URL not available').classes('text-sm text-red-500')
                
                # Instructions
                ui.label('Click the link below to open Jupyter Lab in a new tab:').classes('text-sm text-gray-600')
                
                # Clickable link
                if self.jupyter_url:
                    ui.link(text=self.jupyter_url, target=self.jupyter_url).classes('text-blue-600 hover:text-blue-800 underline break-all')
                
                # Action buttons
                with ui.row().classes('w-full justify-end gap-2 mt-4'):
                    if self.jupyter_url:
                        ui.button('Open in New Tab', 
                                 on_click=lambda: [self.open_url_in_new_tab(self.jupyter_url), dialog.close()]).classes('bg-black hover:bg-gray-800 text-white')
                    ui.button('Close', on_click=dialog.close).props('flat').classes('bg-black hover:bg-gray-800 text-white')
        
        print("DEBUG: Opening dialog...")
        dialog.open()
        print("DEBUG: Dialog opened")
    
    def show_loading_dialog(self):
        """Show loading dialog while Jupyter Lab starts"""
        self.loading_dialog = ui.dialog().props('persistent')
        
        with self.loading_dialog, ui.card():
            ui.label('Starting Jupyter Lab...').classes('text-h6 font-bold mb-4')
            
            with ui.column().classes('gap-4 min-w-96 items-center'):
                # Loading spinner
                ui.spinner(size='lg').classes('mb-4')
                
                ui.label('Please wait while Jupyter Lab starts up.').classes('text-sm text-gray-600 text-center')
                ui.label('This may take a few moments...').classes('text-sm text-gray-600 text-center')
                
                # Cancel button
                with ui.row().classes('w-full justify-center mt-4'):
                    ui.button('Cancel', 
                             on_click=lambda: [self.stop_jupyter_lab(), self.loading_dialog.close() if self.loading_dialog else None]).props('flat').classes('bg-gray-600 hover:bg-gray-700 text-white')
        
        self.loading_dialog.open()
    
    def open_url_in_new_tab(self, url: str):
        """Open URL in a new tab using JavaScript"""
        ui.run_javascript(f'window.open("{url}", "_blank");')
    
    def copy_to_clipboard(self, text: str):
        """Copy text to clipboard"""
        ui.run_javascript(f'navigator.clipboard.writeText("{text}")')
        ui.notify('URL copied to clipboard!', type='positive')
    
    def check_jupyter_status(self):
        """Check if Jupyter Lab is running and update status"""
        try:
            if self.jupyter_process and self.jupyter_process.poll() is None:
                if not self.jupyter_url:
                    self.jupyter_status_label.text = 'Jupyter: Starting...'
                else:
                    self.jupyter_status_label.text = 'Jupyter: Running'
                    self.update_status('Jupyter Lab is running')
            else:
                self.jupyter_status_label.text = 'Jupyter: Not Running'
                self.jupyter_url = None
                if self.jupyter_process:
                    # Process has terminated, check if it was an error
                    returncode = self.jupyter_process.returncode
                    if returncode != 0:
                        ui.notify(f'Jupyter Lab failed to start (exit code: {returncode})', type='negative')
        except Exception as e:
            self.jupyter_status_label.text = 'Jupyter: Error'
            ui.notify(f'Error checking Jupyter status: {str(e)}', type='negative')
    
    def import_json_data(self, data: dict, filename: str, expected_type: str = None) -> bool:
        """Import JSON data into appropriate model"""
        try:
            imported = False
            
            # If expected type is specified, try that first
            if expected_type == 'worker' or 'model_sha' in data or 'modelType' in data:
                # Worker configuration - handle both direct model config and distributed config with model_sha
                try:
                    if hasattr(self, 'worker_designer') and self.worker_designer:
                        success = self.worker_designer.load_from_json(data)
                        if success:
                            imported = True
                    else:
                        # Fallback: store data for later loading
                        self.pending_worker_data = data
                        imported = True
                except Exception as e:
                    print(f"Worker import error: {e}")
                    pass
                    
            elif expected_type == 'connection' and ('connections' in data or 'devices' in data or 'connectionsMap' in data):
                # Connection configuration
                try:
                    if hasattr(self.connection_model, 'from_dict'):
                        print(f"DEBUG: Loading connection config with data keys: {list(data.keys())}")
                        self.connection_model.from_dict(data)
                    imported = True
                except Exception as e:
                    print(f"DEBUG: Connection config import error: {e}")
                    import traceback
                    traceback.print_exc()
                    pass
                    
            elif expected_type == 'distributed' and ('distributed_system_type' in data or 'devices' in data):
                # Distributed configuration
                try:
                    if hasattr(self.distributed_config_model, 'from_dict'):
                        print(f"DEBUG: Loading distributed config with data keys: {list(data.keys())}")
                        self.distributed_config_model.from_dict(data)
                    imported = True
                except Exception as e:
                    print(f"DEBUG: Distributed config import error: {e}")
                    pass
                    
            elif expected_type == 'experiment' and ('experiment' in data or 'experimentName' in data or 'Phases' in data):
                # Experiment configuration
                try:
                    print(f"DEBUG: Loading experiment config with data keys: {list(data.keys())}")
                    success = self.experiment_designer.load_from_json(data)
                    if success:
                        imported = True
                except Exception as e:
                    print(f"DEBUG: Experiment import error: {e}")
                    import traceback
                    traceback.print_exc()
                    pass
            
            # Fallback: Auto-detect based on content if expected type didn't work
            if not imported:
                if 'modelType' in data or 'model_type' in data or 'layer_sizes' in data:
                    # Worker configuration
                    try:
                        if hasattr(self.worker_model, 'from_dict'):
                            self.worker_model.from_dict(data)
                        imported = True
                    except:
                        pass
                        
                elif 'devices' in data or 'connections' in data or 'connectionsMap' in data:
                    # Connection/Distributed config
                    try:
                        if ('connections' in data or 'connectionsMap' in data) and hasattr(self.connection_model, 'from_dict'):
                            print(f"DEBUG: Auto-detecting connection config")
                            self.connection_model.from_dict(data)
                        if 'devices' in data and hasattr(self.distributed_config_model, 'from_dict'):
                            print(f"DEBUG: Auto-detecting distributed config with devices")
                            self.distributed_config_model.from_dict(data)
                        imported = True
                    except Exception as e:
                        print(f"DEBUG: Auto-detect import error: {e}")
                        import traceback
                        traceback.print_exc()
                        pass
                        
                elif 'worker' in data and 'connection' in data:
                    # Complete project
                    try:
                        if 'worker' in data and hasattr(self.worker_model, 'from_dict'):
                            self.worker_model.from_dict(data['worker'])
                        if 'connection' in data and hasattr(self.connection_model, 'from_dict'):
                            self.connection_model.from_dict(data['connection'])
                        if 'distributed_config' in data and hasattr(self.distributed_config_model, 'from_dict'):
                            self.distributed_config_model.from_dict(data['distributed_config'])
                        imported = True
                    except:
                        pass
            
            # Update UI if import was successful
            if imported:
                # Refresh UI components to show imported data
                self.refresh_ui_with_imported_data()
                return True
            else:
                ui.notify(f'Could not import {filename} - format not recognized', type='warning')
                return False
                
        except Exception as e:
            ui.notify(f'Error importing {filename}: {str(e)}', type='negative')
            return False
    
    def refresh_ui_with_imported_data(self):
        """Refresh UI components to display imported data"""
        try:
            # Refresh worker designer if it exists
            if hasattr(self, 'worker_designer') and self.worker_designer:
                if hasattr(self.worker_designer, 'refresh_from_model'):
                    self.worker_designer.refresh_from_model()
                elif hasattr(self.worker_designer, 'update_ui_from_model'):
                    self.worker_designer.update_ui_from_model()
            
            # Refresh other components as needed
            if hasattr(self, 'connection_designer') and self.connection_designer:
                if hasattr(self.connection_designer, 'refresh_from_model'):
                    self.connection_designer.refresh_from_model()
            
            ui.notify('UI updated with imported data', type='info')
        except Exception as e:
            print(f"Warning: Could not refresh UI: {e}")
    
    def create_json_section(self, title: str, description: str, file_type: str, prefixes: list, color: str):
        """Create a JSON import/export section for a specific file type"""
        with ui.card().classes('w-full border-2 border-red-200 hover:border-red-400 transition-colors'):
            with ui.card_section():
                # Header
                ui.label(title).classes(f'text-h6 font-bold text-{color} mb-2')
                ui.label(description).classes('text-sm text-gray-600 mb-4')
                
                # Import section with drag & drop
                with ui.column().classes('w-full gap-3'):
                    # Drop zone
                    drop_zone = ui.element('div').classes(
                        f'w-full h-24 border-2 border-dashed border-{color} rounded-lg '
                        'flex items-center justify-center cursor-pointer '
                        f'hover:bg-red-50 hover:border-{color} transition-colors'
                    )
                    
                    with drop_zone:
                        with ui.column().classes('items-center gap-2'):
                            ui.icon('cloud_upload', size='lg').classes(f'text-{color}')
                            ui.label(f'Drop {file_type} JSON here or click to browse').classes(f'text-{color} font-medium')
                            ui.label(f'Supports: {", ".join(prefixes)}*.json').classes('text-xs text-gray-500')
                    
                    # Export button only
                    with ui.row().classes('w-full justify-center'):
                        ui.button(f'Export {file_type.title()}', 
                                 on_click=lambda ft=file_type: self.export_json(ft)).classes('bg-black hover:bg-gray-800 text-white')
                    
                    # Enhanced drag and drop functionality with click to browse
                    upload = ui.upload(
                        on_upload=lambda e, ft=file_type, pf=prefixes: self.handle_file_upload(e, ft, pf),
                        auto_upload=True,
                        multiple=True
                    ).props('accept=.json').style('display: none')
                    
                    drop_zone.on('drop', lambda e, ft=file_type, pf=prefixes: self.handle_drop_event(e, ft, pf))
                    drop_zone.on('dragover', lambda e: e.preventDefault())
                    drop_zone.on('dragenter', lambda e: e.preventDefault())
                    drop_zone.on('click', lambda: upload.run_method('pickFiles'))
    
    def handle_drop_event(self, e, file_type: str, prefixes: list):
        """Handle drag and drop file events"""
        try:
            # Access event data
            event_data = e.args if hasattr(e, 'args') else e
            
            # Try different ways to access the files
            files = None
            if isinstance(event_data, dict):
                dt = event_data.get('dataTransfer', {})
                files = dt.get('files', [])
            
            # If no files found, try alternative approach
            if not files:
                ui.notify('Drop a JSON file here to import it', type='info')
                return
            
            for file_data in files:
                filename = file_data.get('name', '')
                if filename.endswith('.json'):
                    # Read file content
                    content = file_data.get('content', '') or file_data.get('text', '')
                    if content:
                        try:
                            data = json.loads(content)
                            success = self.import_json_data(data, filename, expected_type=file_type)
                            if success:
                                ui.notify(f'Successfully imported {filename}', type='positive')
                            else:
                                ui.notify(f'Failed to import {filename}', type='negative')
                        except json.JSONDecodeError as ex:
                            ui.notify(f'Invalid JSON in {filename}: {str(ex)}', type='negative')
                    else:
                        ui.notify(f'Could not read content from {filename}', type='negative')
                else:
                    ui.notify(f'Only JSON files are supported: {filename}', type='warning')
        except Exception as ex:
            ui.notify(f'Error processing dropped files: {str(ex)}', type='negative')

    def handle_file_upload(self, e, file_type: str, prefixes: list):
        """Handle file upload from click browsing"""
        try:
            print(f"DEBUG: handle_file_upload called with file_type={file_type}, prefixes={prefixes}")
            print(f"DEBUG: Upload event type: {type(e)}")
            print(f"DEBUG: Upload event attributes: {dir(e)}")
            
            # Try different ways to access files from upload event
            files = None
            if hasattr(e, 'files'):
                files = e.files
            elif hasattr(e, 'file'):
                files = [e.file] if e.file else []
            elif hasattr(e, 'content'):
                # Single file upload case
                files = [e]
            else:
                print(f"DEBUG: Looking for files in event object...")
                # Try to find files in the event object
                for attr in dir(e):
                    if not attr.startswith('_'):
                        attr_value = getattr(e, attr)
                        print(f"DEBUG: Attribute {attr}: {type(attr_value)} = {attr_value}")
                        if isinstance(attr_value, list) and attr_value:
                            files = attr_value
                            break
                
            if not files:
                ui.notify(f'Error: No files found in upload event', type='negative')
                return
            
            # Check if files is callable (method) and call it if needed
            if callable(files):
                print(f"DEBUG: files is callable, calling it...")
                files = files()
            
            # Ensure files is iterable
            if not hasattr(files, '__iter__'):
                files = [files]
            
            # Now it's safe to get the length
            print(f"DEBUG: Number of files: {len(files)}")
            
            for file_info in files:
                print(f"DEBUG: Processing file: {getattr(file_info, 'name', 'unknown')}")
                
                # Get file name
                filename = getattr(file_info, 'name', 'unknown')
                if not filename.endswith('.json'):
                    ui.notify(f'Only JSON files supported. Skipped: {filename}', type='negative')
                    continue
                
                # Check if filename matches expected prefixes
                filename_lower = filename.lower()
                matches_prefix = any(filename_lower.startswith(prefix) for prefix in prefixes)
                
                if not matches_prefix:
                    ui.notify(f'File "{filename}" doesn\'t match expected prefixes: {", ".join(prefixes)}', 
                             type='warning', timeout=5000)
                
                try:
                    # Read and parse JSON content
                    print(f"DEBUG: Reading content from {filename}")
                    
                    # Try different ways to get content
                    content = None
                    
                    # Method 1: Check if it's a NiceGUI UploadEventArguments object
                    try:
                        if hasattr(file_info, 'content'):
                            content_attr = file_info.content
                            # Check if content is a method/callable - call it first
                            if callable(content_attr):
                                content_attr = content_attr()
                            
                            # Now try to read from it
                            if hasattr(content_attr, 'read'):
                                # Reset file pointer if possible
                                if hasattr(content_attr, 'seek'):
                                    content_attr.seek(0)
                                content = content_attr.read()
                                if isinstance(content, bytes):
                                    content = content.decode('utf-8')
                            elif isinstance(content_attr, (str, bytes)):
                                content = content_attr if isinstance(content_attr, str) else content_attr.decode('utf-8')
                    except Exception as e1:
                        print(f"DEBUG: Method 1 failed: {e1}")
                    
                    # Method 2: Try alternative approaches
                    if not content:
                        try:
                            if hasattr(file_info, 'data'):
                                content = file_info.data
                            elif hasattr(file_info, 'text'):
                                content = file_info.text
                            elif hasattr(file_info, 'content') and isinstance(file_info.content, str):
                                content = file_info.content
                        except Exception as e2:
                            print(f"DEBUG: Method 2 failed: {e2}")
                    
                    # Method 3: Try direct file reading if path is available
                    if not content:
                        try:
                            if hasattr(file_info, 'path') and file_info.path:
                                with open(file_info.path, 'r', encoding='utf-8') as f:
                                    content = f.read()
                        except Exception as e3:
                            print(f"DEBUG: Method 3 failed: {e3}")
                    
                    print(f"DEBUG: Final content type: {type(content)}, length: {len(content) if content else 0}")
                    
                    if not content:
                        ui.notify(f'Could not read content from {filename}', type='negative')
                        continue
                        
                    print(f"DEBUG: Content length: {len(content)}")
                    
                    data = json.loads(content)
                    print(f"DEBUG: JSON parsed successfully, keys: {list(data.keys()) if isinstance(data, dict) else 'not a dict'}")
                    
                    # Import the data
                    success = self.import_json_data(data, filename, expected_type=file_type)
                    print(f"DEBUG: Import result: {success}")
                    
                    if success:
                        status_msg = "Success" if matches_prefix else "Warning"
                        ui.notify(f'{status_msg}: Imported {filename} as {file_type} config!', 
                                 type='positive' if matches_prefix else 'warning')
                        self.update_status(f'Imported {file_type}: {filename}')
                    else:
                        ui.notify(f'Failed to import {filename}', type='negative')
                        
                except json.JSONDecodeError as json_ex:
                    ui.notify(f'Invalid JSON in {filename}: {str(json_ex)}', type='negative')
                except Exception as file_ex:
                    ui.notify(f'Error processing {filename}: {str(file_ex)}', type='negative')
                    print(f"DEBUG: File processing error: {file_ex}")
                    import traceback
                    traceback.print_exc()
                    
        except Exception as ex:
            print(f"DEBUG: General upload error: {ex}")
            import traceback
            traceback.print_exc()
            ui.notify(f'Error uploading file: {str(ex)}', type='negative')

    def handle_specific_import(self, e, file_type: str, prefixes: list):
        """Handle import for a specific file type with prefix detection"""
        for file_info in e.files:
            if not file_info.name.endswith('.json'):
                ui.notify(f'Only JSON files supported. Skipped: {file_info.name}', type='negative')
                continue
            
            # Check if filename matches expected prefixes
            filename_lower = file_info.name.lower()
            matches_prefix = any(filename_lower.startswith(prefix) for prefix in prefixes)
            
            if not matches_prefix:
                ui.notify(f'File "{file_info.name}" doesn\'t match expected prefixes: {", ".join(prefixes)}', 
                         type='warning', timeout=5000)
            
            try:
                # Read and parse JSON content
                content = file_info.content.read().decode('utf-8')
                data = json.loads(content)
                
                # Import the data
                success = self.import_json_data(data, file_info.name, expected_type=file_type)
                
                if success:
                    status_msg = "Success" if matches_prefix else "Warning"
                    ui.notify(f'{status_msg}: Imported {file_info.name} as {file_type} config!', 
                             type='positive' if matches_prefix else 'warning')
                    self.update_status(f'Imported {file_type}: {file_info.name}')
                else:
                    ui.notify(f'Failed to import {file_info.name}', type='negative')
                    
            except json.JSONDecodeError as e:
                ui.notify(f'Invalid JSON in {file_info.name}: {str(e)}', type='negative')
            except Exception as e:
                ui.notify(f'Error importing {file_info.name}: {str(e)}', type='negative')
    
    def update_status(self, message: str):
        """Update status message"""
        if hasattr(self, 'status_label'):
            self.status_label.text = message
    
    def cleanup(self):
        """Cleanup resources when shutting down"""
        try:
            # Stop Jupyter Lab if running
            if self.jupyter_process and self.jupyter_process.poll() is None:
                print("Shutting down Jupyter Lab...")
                os.killpg(os.getpgid(self.jupyter_process.pid), signal.SIGTERM)
                try:
                    self.jupyter_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    os.killpg(os.getpgid(self.jupyter_process.pid), signal.SIGKILL)
                    self.jupyter_process.wait()
                print("Jupyter Lab stopped.")
                
            # Reset Jupyter state
            self.jupyter_process = None
            self.jupyter_url = None
        except Exception as e:
            print(f"Error during cleanup: {e}")
    
    def export_json(self, export_type: str = 'worker'):
        """Export JSON configuration"""
        try:
            timestamp = asyncio.get_event_loop().time() if hasattr(asyncio, 'get_event_loop') else '1'
            
            if export_type == 'worker':
                data = self.get_worker_data()
                filename = f'worker_config_{int(timestamp)}.json'
                
            elif export_type == 'connection':
                data = self.get_connection_data()
                filename = f'connection_map_{int(timestamp)}.json'
                
            elif export_type == 'distributed':
                data = self.get_distributed_data()
                filename = f'distributed_config_{int(timestamp)}.json'
                
            elif export_type == 'complete':
                data = {
                    'worker': self.get_worker_data(),
                    'connection': self.get_connection_data(),
                    'distributed': self.get_distributed_data(),
                    'metadata': {
                        'version': NERLPLANNER_VERSION,
                        'created_with': 'NerlDesigner',
                        'timestamp': int(timestamp)
                    }
                }
                filename = f'nerlnet_project_{int(timestamp)}.json'
            
            else:
                ui.notify(f'Unknown export type: {export_type}', type='negative')
                return
            
            # Convert to JSON and trigger download
            json_str = json.dumps(data, indent=2)
            ui.download(json_str.encode(), filename=filename)
            
            self.update_status(f'Exported {filename}')
            ui.notify(f'Exported {filename} successfully!', type='positive')
            
        except Exception as e:
            ui.notify(f'Export failed: {str(e)}', type='negative')
    
    def get_worker_data(self) -> dict:
        """Get current worker configuration data"""
        # Return worker model data or default structure
        try:
            if hasattr(self.worker_model, 'model_dump'):
                return self.worker_model.model_dump()
            elif hasattr(self.worker_model, 'dict'):
                return self.worker_model.dict()
            elif hasattr(self.worker_model, 'to_dict'):
                return self.worker_model.to_dict()
            else:
                # Return a basic structure
                return {
                    'modelType': 'neural_network',
                    'infraType': 'sequential',
                    'layerSizes': [784, 128, 10],
                    'activationFunction': 'relu',
                    'lossMethod': 'mean_squared_error',
                    'optimizerType': 'adam'
                }
        except Exception:
            return {'error': 'Could not serialize worker data'}
    
    def get_connection_data(self) -> dict:
        """Get current connection configuration data"""
        try:
            if hasattr(self.connection_model, 'dict'):
                return self.connection_model.dict()
            elif hasattr(self.connection_model, 'to_dict'):
                return self.connection_model.to_dict()
            else:
                return {
                    'connections': [],
                    'devices': []
                }
        except Exception:
            return {'error': 'Could not serialize connection data'}
    
    def get_distributed_data(self) -> dict:
        """Get current distributed configuration data"""
        try:
            if hasattr(self.distributed_config_model, 'dict'):
                return self.distributed_config_model.dict()
            elif hasattr(self.distributed_config_model, 'to_dict'):
                return self.distributed_config_model.to_dict()
            else:
                return {
                    'distributed_system_type': 'federated',
                    'devices': [],
                    'parameters': {}
                }
        except Exception:
            return {'error': 'Could not serialize distributed data'}
        
        dialog.open()
    
    def export_worker_json(self, dialog):
        """Export worker configuration"""
        data = self.worker_model.to_dict()
        # For now, just show the JSON
        ui.notify('Worker JSON would be exported here', type='info')
        dialog.close()
    
    def export_connection_json(self, dialog):
        """Export connection configuration"""
        data = self.connection_model.to_dict()
        ui.notify('Connection JSON would be exported here', type='info')
        dialog.close()
    
    def export_distributed_json(self, dialog):
        """Export distributed configuration"""
        data = self.distributed_config_model.to_dict()
        ui.notify('Distributed config JSON would be exported here', type='info')
        dialog.close()
    
    def export_all_json(self, dialog):
        """Export all configurations"""
        ui.notify('All JSON files would be exported here', type='info')
        dialog.close()

def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='NerlDesigner - NerlNet Web Designer')
    parser.add_argument('--host', default='0.0.0.0', help='Host to bind to (default: 0.0.0.0)')
    parser.add_argument('--port', type=int, default=8082, help='Port to bind to (default: 8082)')
    parser.add_argument('--dark', action='store_true', help='Use dark theme')
    parser.add_argument('--reload', action='store_true', help='Enable auto-reload for development')
    
    args = parser.parse_args()
    
    # Check for environment variables (set by launcher script)
    host = os.getenv('NERLDESIGNER_HOST', args.host)
    port = int(os.getenv('NERLDESIGNER_PORT', args.port))
    
    # Print startup information
    print("🧠 NerlDesigner - NerlNet Web Designer")
    print("=" * 50)
    print(f"🌐 Server starting on {host}:{port}")
    if host == '0.0.0.0':
        print("📱 Local access:")
        print(f"   http://localhost:{port}")
        print(f"   or http://127.0.0.1:{port}")
        print("")
        print("📡 SSH Tunnel (if connecting remotely):")
        print("   On your local machine, run:")
        print(f"   ssh -L {port}:localhost:{port} <username>@<server-address>")
        print(f"   Then open: http://localhost:{port}")
        print("")
    else:
        print(f"📱 Direct access: http://{host}:{port}")
    print("⏹️  Press Ctrl+C to stop the server")
    print("=" * 50)
    
    designer = NerlDesigner()
    designer.setup_ui()
    
    # Register cleanup function for app shutdown
    import atexit
    atexit.register(designer.cleanup)
    
    # Configure favicon - use emoji (NiceGUI requirement)
    # NiceGUI only supports emoji or properly configured favicon files
    # Using brain emoji to represent neural networks
    favicon_icon = '🧠'
    
    # Configure and run the app
    ui.run(
        title='NerlDesigner - NerlNet Web Designer',
        port=port,
        host=host,
        favicon=favicon_icon,
        dark=args.dark,
        show=True,
        reload=args.reload
    )

if __name__ in {"__main__", "__mp_main__"}:
    main()
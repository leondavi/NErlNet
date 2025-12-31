"""
Distributed Configuration Designer Component - UI for designing distributed configurations
"""

from typing import Dict, Any, List
import json
from nicegui import ui
from models.distributed_config_model import DistributedConfigModel

class DCDesigner:
    """UI component for designing distributed configurations"""
    
    def __init__(self, dc_model: DistributedConfigModel):
        self.dc_model = dc_model
        self.current_config = {}
        
    def create_ui(self):
        """Create the DC designer UI"""
        with ui.column().classes('w-full gap-4'):
            # Header
            ui.label('Distributed Configuration Designer').classes('text-h5 font-bold mb-4')
            
            # Settings Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    ui.label('NerlNet Settings').classes('text-h6 font-bold mb-3')
                    
                    with ui.row().classes('w-full gap-4'):
                        self.frequency_input = ui.number(
                            label='Frequency (Hz)',
                            value=1,
                            min=1,
                            on_change=self.on_frequency_change
                        ).classes('flex-1')
                        
                        self.batch_size_input = ui.number(
                            label='Batch Size',
                            value=50,
                            min=1,
                            on_change=self.on_batch_size_change
                        ).classes('flex-1')
            
            # Main Server & API Server Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    ui.label('Special Entities').classes('text-h6 font-bold mb-3')
                    
                    with ui.row().classes('w-full gap-4'):
                        # Main Server
                        with ui.column().classes('flex-1'):
                            ui.label('Main Server').classes('font-bold')
                            self.main_server_port = ui.number(
                                label='Port',
                                value=8080,
                                min=1000,
                                max=65535,
                                on_change=self.on_main_server_change
                            ).classes('w-full')
                            self.main_server_args = ui.input(
                                label='Arguments',
                                value='',
                                on_change=self.on_main_server_change
                            ).classes('w-full')
                        
                        # API Server
                        with ui.column().classes('flex-1'):
                            ui.label('API Server').classes('font-bold')
                            self.api_server_port = ui.number(
                                label='Port',
                                value=8081,
                                min=1000,
                                max=65535,
                                on_change=self.on_api_server_change
                            ).classes('w-full')
                            self.api_server_args = ui.input(
                                label='Arguments',
                                value='',
                                on_change=self.on_api_server_change
                            ).classes('w-full')
            
            # Devices Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    with ui.row().classes('w-full items-center justify-between mb-3'):
                        ui.label('Devices').classes('text-h6 font-bold')
                        with ui.row().classes('gap-2'):
                            ui.button('Add Device', 
                                     on_click=self.show_add_device_dialog,
                                     icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                            ui.button('Scan Network', 
                                     on_click=self.show_network_scanner,
                                     icon='wifi_find').classes('bg-black hover:bg-gray-800 text-white')
                    
                    # Devices list
                    self.devices_container = ui.column().classes('w-full gap-2')
                    self.render_devices()
            
            # Routers Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    with ui.row().classes('w-full items-center justify-between mb-3'):
                        ui.label('Routers').classes('text-h6 font-bold')
                        ui.button('Add Router', 
                                 on_click=self.show_add_router_dialog,
                                 icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                    
                    self.routers_container = ui.column().classes('w-full gap-2')
                    self.render_routers()
            
            # Sources Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    with ui.row().classes('w-full items-center justify-between mb-3'):
                        ui.label('Sources').classes('text-h6 font-bold')
                        ui.button('Add Source', 
                                 on_click=self.show_add_source_dialog,
                                 icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                    
                    self.sources_container = ui.column().classes('w-full gap-2')
                    self.render_sources()
            
            # Clients Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    with ui.row().classes('w-full items-center justify-between mb-3'):
                        ui.label('Clients').classes('text-h6 font-bold')
                        ui.button('Add Client', 
                                 on_click=self.show_add_client_dialog,
                                 icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                    
                    self.clients_container = ui.column().classes('w-full gap-2')
                    self.render_clients()
            
            # Workers Section
            with ui.card().classes('w-full'):
                with ui.card_section():
                    with ui.row().classes('w-full items-center justify-between mb-3'):
                        ui.label('Workers').classes('text-h6 font-bold')
                        with ui.row().classes('gap-2'):
                            ui.button('Add Worker',
                                     on_click=self.show_add_worker_dialog,
                                     icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                            ui.button('Import from JSON',
                                     on_click=self.show_import_worker_dialog,
                                     icon='upload_file').classes('bg-green-600 hover:bg-green-500 text-white')
                    
                    self.workers_container = ui.column().classes('w-full gap-2')
                    self.render_workers()
    
    def render_devices(self):
        """Render the devices list"""
        self.devices_container.clear()
        
        if not hasattr(self.dc_model, 'devices') or not self.dc_model.devices:
            with self.devices_container:
                ui.label('No devices configured').classes('text-gray-500 italic text-center py-4')
        else:
            with self.devices_container:
                for i, device in enumerate(self.dc_model.devices):
                    with ui.card().classes('w-full bg-gray-50'):
                        with ui.card_section().classes('p-3'):
                            with ui.row().classes('w-full items-center justify-between'):
                                with ui.column().classes('flex-1'):
                                    ui.label(f'{device.get("name", "Unknown")}').classes('font-bold')
                                    ui.label(f'IP: {device.get("ipv4", "N/A")}').classes('text-sm text-gray-600')
                                    ui.label(f'Entities: {device.get("entities", "None")}').classes('text-sm text-gray-600')
                                
                                with ui.row().classes('gap-2'):
                                    ui.button('Edit', icon='edit',
                                             on_click=lambda idx=i: self.edit_device(idx)).classes('bg-black hover:bg-gray-800 text-white text-xs px-2 py-1')
                                    ui.button('Remove', icon='delete',
                                             on_click=lambda idx=i: self.remove_device(idx)).classes('bg-red-600 hover:bg-red-500 text-white text-xs px-2 py-1')
    
    def render_routers(self):
        """Render the routers list"""
        self.routers_container.clear()
        
        if not hasattr(self.dc_model, 'routers') or not self.dc_model.routers:
            with self.routers_container:
                ui.label('No routers configured').classes('text-gray-500 italic text-center py-4')
        else:
            with self.routers_container:
                for i, router in enumerate(self.dc_model.routers):
                    with ui.card().classes('w-full bg-blue-50'):
                        with ui.card_section().classes('p-3'):
                            with ui.row().classes('w-full items-center justify-between'):
                                with ui.column().classes('flex-1'):
                                    # Handle both RouterDefinition objects and dict formats
                                    router_name = router.name if hasattr(router, 'name') else router.get('name', 'Unknown')
                                    router_port = router.port if hasattr(router, 'port') else router.get('port', 'N/A')
                                    router_policy = router.policy if hasattr(router, 'policy') else router.get('policy', '0')
                                    
                                    ui.label(f'{router_name}').classes('font-bold')
                                    ui.label(f'Port: {router_port}').classes('text-sm text-gray-600')
                                    ui.label(f'Policy: {router_policy}').classes('text-sm text-gray-600')
                                
                                with ui.row().classes('gap-2'):
                                    ui.button('Edit', icon='edit',
                                             on_click=lambda idx=i: self.edit_router(idx)).classes('bg-black hover:bg-gray-800 text-white text-xs px-2 py-1')
                                    ui.button('Remove', icon='delete',
                                             on_click=lambda idx=i: self.remove_router(idx)).classes('bg-red-600 hover:bg-red-500 text-white text-xs px-2 py-1')
    
    def render_sources(self):
        """Render the sources list"""
        self.sources_container.clear()
        
        if not hasattr(self.dc_model, 'sources') or not self.dc_model.sources:
            with self.sources_container:
                ui.label('No sources configured').classes('text-gray-500 italic text-center py-4')
        else:
            with self.sources_container:
                for i, source in enumerate(self.dc_model.sources):
                    with ui.card().classes('w-full bg-green-50'):
                        with ui.card_section().classes('p-3'):
                            with ui.row().classes('w-full items-center justify-between'):
                                with ui.column().classes('flex-1'):
                                    ui.label(f'{source.get("name", "Unknown")}').classes('font-bold')
                                    ui.label(f'Port: {source.get("port", "N/A")}').classes('text-sm text-gray-600')
                                    ui.label(f'Epochs: {source.get("epochs", "N/A")} | Type: {source.get("type", "0")}').classes('text-sm text-gray-600')
                                
                                with ui.row().classes('gap-2'):
                                    ui.button('Edit', icon='edit',
                                             on_click=lambda idx=i: self.edit_source(idx)).classes('bg-black hover:bg-gray-800 text-white text-xs px-2 py-1')
                                    ui.button('Remove', icon='delete',
                                             on_click=lambda idx=i: self.remove_source(idx)).classes('bg-red-600 hover:bg-red-500 text-white text-xs px-2 py-1')
    
    def render_clients(self):
        """Render the clients list"""
        self.clients_container.clear()
        
        if not hasattr(self.dc_model, 'clients') or not self.dc_model.clients:
            with self.clients_container:
                ui.label('No clients configured').classes('text-gray-500 italic text-center py-4')
        else:
            with self.clients_container:
                for i, client in enumerate(self.dc_model.clients):
                    with ui.card().classes('w-full bg-purple-50'):
                        with ui.card_section().classes('p-3'):
                            with ui.row().classes('w-full items-center justify-between'):
                                with ui.column().classes('flex-1'):
                                    # Handle both ClientDefinition objects and dict formats
                                    client_name = client.name if hasattr(client, 'name') else client.get('name', 'Unknown')
                                    client_port = client.port if hasattr(client, 'port') else client.get('port', 'N/A')
                                    client_workers = client.workers if hasattr(client, 'workers') else client.get('workers', 'None')
                                    
                                    ui.label(f'{client_name}').classes('font-bold')
                                    ui.label(f'Port: {client_port}').classes('text-sm text-gray-600')
                                    ui.label(f'Workers: {client_workers}').classes('text-sm text-gray-600')
                                
                                with ui.row().classes('gap-2'):
                                    ui.button('Edit', icon='edit',
                                             on_click=lambda idx=i: self.edit_client(idx)).classes('bg-black hover:bg-gray-800 text-white text-xs px-2 py-1')
                                    ui.button('Remove', icon='delete',
                                             on_click=lambda idx=i: self.remove_client(idx)).classes('bg-red-600 hover:bg-red-500 text-white text-xs px-2 py-1')
    
    # Event handlers
    def on_frequency_change(self, e):
        """Handle frequency change"""
        self.current_config['frequency'] = str(e.value)
    
    def on_batch_size_change(self, e):
        """Handle batch size change"""
        self.current_config['batchSize'] = str(e.value)
    
    def on_main_server_change(self, e=None):
        """Handle main server changes"""
        self.current_config['mainServer'] = {
            'port': str(self.main_server_port.value),
            'args': self.main_server_args.value
        }
    
    def on_api_server_change(self, e=None):
        """Handle API server changes"""
        self.current_config['apiServer'] = {
            'port': str(self.api_server_port.value),
            'args': self.api_server_args.value
        }
    
    # Helper methods
    def check_worker_conflicts(self, workers_string: str, exclude_client: str = None) -> tuple[bool, str]:
        """
        Check if any workers in the string are already assigned to other clients
        Returns (has_conflict, conflict_message)
        """
        if not workers_string or not workers_string.strip():
            return False, ""
        
        # Parse workers from the input string
        requested_workers = [w.strip() for w in workers_string.split(',') if w.strip()]
        
        conflicts = []
        for worker in requested_workers:
            # Check all existing clients
            for client in self.dc_model.clients:
                client_name = client.name if hasattr(client, 'name') else client.get('name', '')
                client_workers_str = client.workers if hasattr(client, 'workers') else client.get('workers', '')
                
                # Skip the client we're excluding (for edit operations)
                if exclude_client and client_name == exclude_client:
                    continue
                
                # Parse workers assigned to this client
                if client_workers_str:
                    assigned_workers = [w.strip() for w in client_workers_str.split(',') if w.strip()]
                    if worker in assigned_workers:
                        conflicts.append(f"Worker '{worker}' is already assigned to client '{client_name}'")
        
        if conflicts:
            return True, "; ".join(conflicts)
        return False, ""

    # Dialog methods
    def show_add_device_dialog(self):
        """Show dialog to add a new device"""
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-lg'):
            ui.html('<h3>Add New Device</h3>', sanitize=False)
            
            name_input = ui.input('Device Name', placeholder='e.g., NerlController-1').classes('w-full')
            ip_input = ui.input('IPv4 Address', placeholder='e.g., 192.168.1.100').classes('w-full')
            entities_input = ui.input('Entities', placeholder='e.g., r1,s1,c1').classes('w-full')
            
            def add_device():
                if name_input.value and ip_input.value:
                    device = {
                        'name': name_input.value,
                        'ipv4': ip_input.value,
                        'entities': entities_input.value or ''
                    }
                    
                    if not hasattr(self.dc_model, 'devices'):
                        self.dc_model.devices = []
                    self.dc_model.devices.append(device)
                    
                    self.render_devices()
                    ui.notify(f'Added device: {name_input.value}', color='positive')
                    dialog.close()
                else:
                    ui.notify('Name and IP address are required', color='negative')
            
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Add Device', on_click=add_device).classes('bg-black hover:bg-gray-800 text-white')
                ui.button('Cancel', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    def show_network_scanner(self):
        """Show network scanner dialog"""
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-md'):
            ui.html('<h3>Network Scanner</h3>', sanitize=False)
            
            subnet_input = ui.input('Network Subnet', 
                                   placeholder='e.g., 192.168.1.0/24',
                                   value='192.168.1.0/24').classes('w-full')
            
            scan_results = ui.column().classes('w-full mt-4')
            
            def start_scan():
                scan_results.clear()
                with scan_results:
                    ui.spinner(size='lg')
                    ui.label('Scanning network...').classes('text-center')
                
                # Simulate network scan (in real implementation, this would use the pinger)
                ui.timer(2.0, lambda: show_scan_results(), once=True)
            
            def show_scan_results():
                scan_results.clear()
                with scan_results:
                    ui.label('Found Devices:').classes('font-bold mb-2')
                    # Mock results - in real implementation, use the pinger functionality
                    mock_devices = [
                        '192.168.1.1 (Router)',
                        '192.168.1.100 (Available)',
                        '192.168.1.101 (Available)',
                        '192.168.1.102 (Available)'
                    ]
                    for device in mock_devices:
                        with ui.row().classes('w-full items-center justify-between p-2 border border-gray-200 rounded'):
                            ui.label(device)
                            ui.button('Add', icon='add', 
                                     on_click=lambda d=device: add_scanned_device(d)).classes('bg-green-600 hover:bg-green-500 text-white text-xs px-2 py-1')
            
            def add_scanned_device(device_info):
                ip = device_info.split(' ')[0]
                name = f'Device-{ip.split(".")[-1]}'
                
                device = {
                    'name': name,
                    'ipv4': ip,
                    'entities': ''
                }
                
                if not hasattr(self.dc_model, 'devices'):
                    self.dc_model.devices = []
                self.dc_model.devices.append(device)
                
                self.render_devices()
                ui.notify(f'Added device: {name}', color='positive')
            
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Scan Network', on_click=start_scan).classes('bg-black hover:bg-gray-800 text-white')
                ui.button('Close', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    def show_add_router_dialog(self):
        """Show dialog to add a new router"""
        print("DEBUG: Opening add router dialog")
        
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 500px; max-width: 90vw'):
                ui.label('Add New Router').classes('text-h6 mb-4')
                
                name_input = ui.input('Router Name', placeholder='e.g., r1').classes('w-full mb-3')
                port_input = ui.number('Port', value=8090, min=1000, max=65535).classes('w-full mb-3')
                policy_input = ui.select(
                    options={'0': 'Policy 0', '1': 'Policy 1', '2': 'Policy 2'}, 
                    label='Policy', 
                    value='0'
                ).classes('w-full mb-4')
                
                def add_router():
                    print(f"DEBUG: Adding router with name: {name_input.value}")
                    if name_input.value:
                        try:
                            # Use the model's add_router method
                            success = self.dc_model.add_router(
                                name=name_input.value,
                                port=str(port_input.value),
                                policy=policy_input.value
                            )
                            
                            if success:
                                print(f"DEBUG: Router added successfully. Total routers: {len(self.dc_model.routers)}")
                                
                                # Refresh the display
                                if hasattr(self, 'routers_container'):
                                    self.render_routers()
                                else:
                                    print("DEBUG: routers_container not found")
                                
                                ui.notify(f'Added router: {name_input.value}', type='positive')
                                dialog.close()
                            else:
                                ui.notify('Router name already exists', type='warning')
                                
                        except Exception as e:
                            print(f"DEBUG: Error adding router: {e}")
                            ui.notify(f'Error adding router: {str(e)}', type='negative')
                    else:
                        ui.notify('Router name is required', type='warning')
                
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Add Router', on_click=add_router).classes('bg-black hover:bg-gray-800 text-white')
        
        dialog.open()
    
    def show_add_source_dialog(self):
        """Show dialog to add a new source"""
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-lg'):
            ui.html('<h3>Add New Source</h3>', sanitize=False)
            
            name_input = ui.input('Source Name', placeholder='e.g., s1').classes('w-full')
            port_input = ui.number('Port', value=8086, min=1000, max=65535).classes('w-full')
            frequency_input = ui.number('Frequency', value=1, min=1).classes('w-full')
            policy_input = ui.select(['0', '1', '2'], label='Policy', value='0').classes('w-full')
            epochs_input = ui.number('Epochs', value=15, min=1).classes('w-full')
            type_input = ui.select(['0', '1'], label='Type', value='0').classes('w-full')
            
            def add_source():
                if name_input.value:
                    source = {
                        'name': name_input.value,
                        'port': str(port_input.value),
                        'frequency': str(frequency_input.value),
                        'policy': policy_input.value,
                        'epochs': str(epochs_input.value),
                        'type': type_input.value
                    }
                    
                    if not hasattr(self.dc_model, 'sources'):
                        self.dc_model.sources = []
                    self.dc_model.sources.append(source)
                    
                    self.render_sources()
                    ui.notify(f'Added source: {name_input.value}', color='positive')
                    dialog.close()
                else:
                    ui.notify('Source name is required', color='negative')
            
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Add Source', on_click=add_source).classes('bg-black hover:bg-gray-800 text-white')
                ui.button('Cancel', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    def show_add_client_dialog(self):
        """Show dialog to add a new client"""
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-lg'):
            ui.html('<h3>Add New Client</h3>', sanitize=False)
            
            # Information section
            with ui.row().classes('w-full mb-3 p-2 bg-blue-50 rounded'):
                ui.icon('info', size='sm').classes('text-blue-600')
                ui.label('Workers can only be assigned to one client at a time. Use comma-separated names (e.g., w1,w2).').classes('text-sm text-blue-800')
            
            name_input = ui.input('Client Name', placeholder='e.g., c1').classes('w-full')
            port_input = ui.number('Port', value=8082, min=1000, max=65535).classes('w-full')
            workers_input = ui.input('Workers', placeholder='e.g., w1,w2').classes('w-full')
            
            # Validation feedback area
            validation_label = ui.label('').classes('text-sm text-red-600 mt-2')
            
            def validate_workers():
                """Validate workers in real-time"""
                has_conflict, conflict_msg = self.check_worker_conflicts(workers_input.value or '')
                
                if has_conflict:
                    validation_label.text = f'⚠️ {conflict_msg}'
                    validation_label.classes('text-sm text-red-600 mt-2')
                elif workers_input.value and workers_input.value.strip():
                    validation_label.text = '✅ Workers available'
                    validation_label.classes('text-sm text-green-600 mt-2')
                else:
                    validation_label.text = ''
            
            # Add real-time validation
            workers_input.on('input', lambda: validate_workers())
            
            def add_client():
                if name_input.value:
                    # Check for worker conflicts before adding
                    has_conflict, conflict_msg = self.check_worker_conflicts(workers_input.value or '')
                    
                    if has_conflict:
                        ui.notify(f'Worker conflict: {conflict_msg}', type='negative')
                        return
                    
                    success = self.dc_model.add_client(
                        name=name_input.value,
                        port=str(port_input.value),
                        workers=workers_input.value or ''
                    )
                    
                    if success:
                        self.render_clients()
                        ui.notify(f'Added client: {name_input.value}', type='positive')
                        dialog.close()
                    else:
                        ui.notify('Client name already exists', type='warning')
                else:
                    ui.notify('Client name is required', type='negative')
            
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Add Client', on_click=add_client).classes('bg-black hover:bg-gray-800 text-white')
                ui.button('Cancel', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    # Edit and remove methods (simplified for now)
    def edit_device(self, index):
        ui.notify(f'Edit device {index + 1} functionality coming soon!', color='info')
    
    def remove_device(self, index):
        if hasattr(self.dc_model, 'devices') and index < len(self.dc_model.devices):
            removed = self.dc_model.devices.pop(index)
            self.render_devices()
            ui.notify(f'Removed device: {removed.get("name", "Unknown")}', color='positive')
    
    def edit_router(self, index):
        ui.notify(f'Edit router {index + 1} functionality coming soon!', color='info')
    
    def remove_router(self, index):
        if hasattr(self.dc_model, 'routers') and index < len(self.dc_model.routers):
            try:
                # Get the router name before removing
                router_name = self.dc_model.routers[index].name if hasattr(self.dc_model.routers[index], 'name') else self.dc_model.routers[index].get('name', 'Unknown')
                
                # Use the model's remove_router method
                success = self.dc_model.remove_router(router_name)
                
                if success:
                    self.render_routers()
                    ui.notify(f'Removed router: {router_name}', color='positive')
                else:
                    ui.notify('Router not found', color='warning')
                    
            except Exception as e:
                print(f"DEBUG: Error removing router: {e}")
                ui.notify(f'Error removing router: {str(e)}', color='negative')
    
    def edit_source(self, index):
        ui.notify(f'Edit source {index + 1} functionality coming soon!', color='info')
    
    def remove_source(self, index):
        if hasattr(self.dc_model, 'sources') and index < len(self.dc_model.sources):
            removed = self.dc_model.sources.pop(index)
            self.render_sources()
            ui.notify(f'Removed source: {removed.get("name", "Unknown")}', color='positive')
    
    def edit_client(self, index):
        """Edit an existing client"""
        if not hasattr(self.dc_model, 'clients') or index >= len(self.dc_model.clients):
            ui.notify('Client not found', type='negative')
            return
        
        client = self.dc_model.clients[index]
        client_name = client.name if hasattr(client, 'name') else client.get('name', '')
        client_port = client.port if hasattr(client, 'port') else client.get('port', '8082')
        client_workers = client.workers if hasattr(client, 'workers') else client.get('workers', '')
        
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 500px; max-width: 90vw'):
                ui.label(f'Edit Client: {client_name}').classes('text-h6 mb-4')
                
                name_input = ui.input('Client Name', value=client_name).classes('w-full mb-3')
                port_input = ui.number('Port', value=int(client_port), min=1000, max=65535).classes('w-full mb-3')
                workers_input = ui.input('Workers', value=client_workers, placeholder='e.g., w1,w2').classes('w-full')
                
                # Validation feedback area
                validation_label = ui.label('').classes('text-sm text-gray-600 mt-2 mb-4')
                
                def validate_workers_edit():
                    """Validate workers in real-time for edit dialog"""
                    has_conflict, conflict_msg = self.check_worker_conflicts(
                        workers_input.value or '', 
                        exclude_client=client_name
                    )
                    
                    if has_conflict:
                        validation_label.text = f'⚠️ {conflict_msg}'
                        validation_label.classes('text-sm text-red-600 mt-2 mb-4')
                    elif workers_input.value and workers_input.value.strip():
                        validation_label.text = '✅ Workers available'
                        validation_label.classes('text-sm text-green-600 mt-2 mb-4')
                    else:
                        validation_label.text = ''
                
                # Add real-time validation
                workers_input.on('input', lambda: validate_workers_edit())
                
                def save_client():
                    if name_input.value:
                        # Check for worker conflicts (exclude current client from check)
                        has_conflict, conflict_msg = self.check_worker_conflicts(
                            workers_input.value or '', 
                            exclude_client=client_name
                        )
                        
                        if has_conflict:
                            ui.notify(f'Worker conflict: {conflict_msg}', type='negative')
                            return
                        
                        try:
                            # Update the client object directly
                            if hasattr(client, 'name'):
                                client.name = name_input.value
                                client.port = str(port_input.value)
                                client.workers = workers_input.value or ''
                            else:
                                # Handle dict format
                                client['name'] = name_input.value
                                client['port'] = str(port_input.value)
                                client['workers'] = workers_input.value or ''
                            
                            self.render_clients()
                            ui.notify(f'Updated client: {name_input.value}', type='positive')
                            dialog.close()
                            
                        except Exception as e:
                            print(f"DEBUG: Error updating client: {e}")
                            ui.notify(f'Error updating client: {str(e)}', type='negative')
                    else:
                        ui.notify('Client name is required', type='negative')
                
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Save Changes', on_click=save_client).classes('bg-black hover:bg-gray-800 text-white')
        
        dialog.open()
    
    def remove_client(self, index):
        if hasattr(self.dc_model, 'clients') and index < len(self.dc_model.clients):
            try:
                # Get the client name before removing
                client_name = self.dc_model.clients[index].name if hasattr(self.dc_model.clients[index], 'name') else self.dc_model.clients[index].get('name', 'Unknown')
                
                # Use the model's remove_client method
                success = self.dc_model.remove_client(client_name)
                
                if success:
                    self.render_clients()
                    ui.notify(f'Removed client: {client_name}', type='positive')
                else:
                    ui.notify('Client not found', type='warning')
                    
            except Exception as e:
                print(f"DEBUG: Error removing client: {e}")
                ui.notify(f'Error removing client: {str(e)}', type='negative')
    
    def build_dc_json(self):
        """Build complete distributed configuration JSON"""
        return {
            "nerlnetSettings": {
                "frequency": self.current_config.get('frequency', '1'),
                "batchSize": self.current_config.get('batchSize', '50')
            },
            "mainServer": self.current_config.get('mainServer', {
                "port": "8080",
                "args": ""
            }),
            "apiServer": self.current_config.get('apiServer', {
                "port": "8081", 
                "args": ""
            }),
            "devices": [device.to_dict() if hasattr(device, 'to_dict') else device for device in getattr(self.dc_model, 'devices', [])],
            "routers": [router.to_dict() if hasattr(router, 'to_dict') else router for router in getattr(self.dc_model, 'routers', [])],
            "sources": getattr(self.dc_model, 'sources', []),
            "clients": [client.to_dict() if hasattr(client, 'to_dict') else client for client in getattr(self.dc_model, 'clients', [])],
            "workers": [worker.to_dict() if hasattr(worker, 'to_dict') else worker for worker in getattr(self.dc_model, 'workers', [])]
        }
    
    def render_workers(self):
        """Render the workers list"""
        self.workers_container.clear()
        
        if not hasattr(self.dc_model, 'workers') or not self.dc_model.workers:
            with self.workers_container:
                ui.label('No workers configured').classes('text-gray-500 italic text-center py-4')
        else:
            with self.workers_container:
                for i, worker in enumerate(self.dc_model.workers):
                    with ui.card().classes('w-full bg-purple-50'):
                        with ui.card_section().classes('p-3'):
                            with ui.row().classes('w-full items-center justify-between'):
                                with ui.column().classes('flex-1'):
                                    ui.label(f'{worker.get("name", "Unknown")}').classes('font-bold')
                                    ui.label(f'Model SHA: {worker.get("model_sha", "N/A")}').classes('text-sm text-gray-600')
                                
                                with ui.row().classes('gap-2'):
                                    ui.button('Edit', icon='edit',
                                             on_click=lambda idx=i: self.edit_worker(idx)).classes('bg-purple-600 hover:bg-purple-500 text-white text-xs px-2 py-1')
                                    ui.button('Remove', icon='delete',
                                             on_click=lambda idx=i: self.remove_worker(idx)).classes('bg-red-600 hover:bg-red-500 text-white text-xs px-2 py-1')
    
    def show_add_worker_dialog(self):
        """Show dialog to add a new worker"""
        print("DEBUG: Opening add worker dialog")
        
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 500px; max-width: 90vw'):
                ui.label('Add New Worker').classes('text-h6 mb-4')
                
                name_input = ui.input('Worker Name', placeholder='e.g., w1').classes('w-full mb-3')
                model_sha_input = ui.input('Model SHA', placeholder='9c5f1261068be7be96487a2cae282aa22e8c1cb482a5bf8d557bc8e1e2b6fef0').classes('w-full mb-4')
                
                def add_worker():
                    print(f"DEBUG: Adding worker with name: {name_input.value}")
                    if name_input.value and model_sha_input.value:
                        worker = {
                            'name': name_input.value,
                            'model_sha': model_sha_input.value
                        }
                        
                        if not hasattr(self.dc_model, 'workers'):
                            self.dc_model.workers = []
                            print("DEBUG: Created workers list")
                        
                        self.dc_model.workers.append(worker)
                        print(f"DEBUG: Worker added. Total workers: {len(self.dc_model.workers)}")
                        
                        # Refresh the display
                        if hasattr(self, 'workers_container'):
                            self.render_workers()
                        else:
                            print("DEBUG: workers_container not found")
                        
                        ui.notify(f'Added worker: {name_input.value}', type='positive')
                        dialog.close()
                    else:
                        ui.notify('Worker name and model SHA are required', type='warning')
                
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Add Worker', on_click=add_worker).classes('bg-black hover:bg-gray-800 text-white')
        
        dialog.open()
    
    def show_import_worker_dialog(self):
        """Show dialog to import worker from JSON file"""
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 600px; max-width: 90vw'):
                ui.label('Import Worker from JSON').classes('text-h6 mb-4')
                
                # Upload area
                with ui.column().classes('w-full mb-4'):
                    ui.label('Select Worker Configuration JSON file:').classes('text-sm font-semibold mb-2')
                    
                    upload = ui.upload(
                        on_upload=lambda e: self.handle_worker_import(e, dialog),
                        auto_upload=True,
                        multiple=False
                    ).props('accept=.json').classes('w-full')
                
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
        
        dialog.open()
    
    def handle_worker_import(self, e, dialog):
        """Handle worker JSON import"""
        try:
            for file_info in e.files:
                if not file_info.name.endswith('.json'):
                    ui.notify('Only JSON files are supported', type='warning')
                    continue
                
                # Read file content
                content = file_info.content.read().decode('utf-8')
                data = json.loads(content)
                
                # Extract worker information
                if 'model_sha' in data:
                    # This looks like a distributed config with model_sha
                    for sha, model_data in data['model_sha'].items():
                        worker_name = f"worker_{sha[:8]}"  # Use first 8 chars of SHA as name
                        worker = {
                            'name': worker_name,
                            'model_sha': sha
                        }
                        
                        if not hasattr(self.dc_model, 'workers'):
                            self.dc_model.workers = []
                        
                        self.dc_model.workers.append(worker)
                        
                elif 'modelType' in data:
                    # This is a direct worker configuration - generate SHA
                    import hashlib
                    worker_json = json.dumps(data, sort_keys=True)
                    sha = hashlib.sha256(worker_json.encode()).hexdigest()
                    
                    worker_name = data.get('name', f"worker_{sha[:8]}")
                    worker = {
                        'name': worker_name,
                        'model_sha': sha
                    }
                    
                    if not hasattr(self.dc_model, 'workers'):
                        self.dc_model.workers = []
                    
                    self.dc_model.workers.append(worker)
                
                self.render_workers()
                ui.notify(f'Imported worker from {file_info.name}', type='positive')
                dialog.close()
                
        except Exception as ex:
            ui.notify(f'Error importing worker: {str(ex)}', type='negative')
    
    def edit_worker(self, index):
        """Edit an existing worker"""
        ui.notify('Worker editing coming soon!', type='info')
    
    def remove_worker(self, index):
        """Remove a worker"""
        if index < len(self.dc_model.workers):
            worker_name = self.dc_model.workers[index].get('name', 'Unknown')
            self.dc_model.workers.pop(index)
            self.render_workers()
            ui.notify(f'Removed worker: {worker_name}', type='positive')
"""
Connection Designer Component - UI for designing network connections and device topology
"""

from typing import Dict, Any, List
from nicegui import ui
from models.connection_model import ConnectionModel, Device, Connection

class ConnectionDesigner:
    """UI component for designing network connections and device topology"""
    
    def __init__(self, connection_model: ConnectionModel):
        self.model = connection_model
        self.ui_elements = {}
        self.selected_device = None
        self.selected_connection = None
        
    def create_ui(self):
        """Create the connection designer UI"""
        with ui.column().classes('w-full p-4'):
            ui.label('Network Connection Designer').classes('text-h5 mb-4')
            
            # Device Management Section
            with ui.card().classes('w-full mb-4'):
                with ui.card_section():
                    ui.label('Device Management').classes('text-h6 mb-2')
                    
                    # Add Device Form
                    with ui.row().classes('gap-4 w-full mb-4'):
                        self.ui_elements['device_name'] = ui.input(
                            label='Device Name',
                            placeholder='e.g. NerlControllerG'
                        )
                        self.ui_elements['device_ip'] = ui.input(
                            label='IPv4 Address',
                            placeholder='e.g. 172.31.91.176'
                        )
                        self.ui_elements['device_entities'] = ui.input(
                            label='Entities',
                            placeholder='e.g. r1,s1,c1'
                        )
                        ui.button('Add Device', on_click=self.add_device).props('color=primary')
                    
                    # Device List
                    ui.label('Devices:').classes('text-subtitle1 mb-2')
                    self.device_list_container = ui.column().classes('w-full')
                    self.update_device_list()
            
            # Connection Management Section
            with ui.card().classes('w-full mb-4'):
                with ui.card_section():
                    ui.label('Connection Management').classes('text-h6 mb-2')
                    
                    # Add Connection Form
                    with ui.row().classes('gap-4 w-full mb-4'):
                        self.ui_elements['from_entity'] = ui.select(
                            {},
                            label='From Entity'
                        )
                        self.ui_elements['to_entity'] = ui.select(
                            {},
                            label='To Entity'
                        )
                        self.ui_elements['connection_type'] = ui.select(
                            {'data': 'Data', 'control': 'Control', 'feedback': 'Feedback'},
                            label='Connection Type',
                            value='data'
                        )
                        ui.button('Add Connection', on_click=self.add_connection).props('color=primary')
                    
                    # Connection List
                    ui.label('Connections:').classes('text-subtitle1 mb-2')
                    self.connection_list_container = ui.column().classes('w-full')
                    self.update_connection_list()
            
            # Network Topology Visualization
            with ui.card().classes('w-full mb-4'):
                with ui.card_section():
                    ui.label('Network Topology').classes('text-h6 mb-2')
                    
                    with ui.row().classes('gap-4 mb-4'):
                        ui.button('Refresh Topology', on_click=self.refresh_topology).props('color=secondary')
                        ui.button('Auto Layout', on_click=self.auto_layout).props('color=secondary')
                        ui.button('Export Graph', on_click=self.export_graph).props('color=secondary')
                    
                    # Topology visualization container
                    self.topology_container = ui.column().classes('w-full h-96 border')
                    self.create_topology_visualization()
            
            # Action Buttons
            with ui.row().classes('gap-4 mt-4'):
                ui.button('Clear All', on_click=self.clear_all).props('color=warning')
                ui.button('Validate Topology', on_click=self.validate_topology).props('color=primary')
                ui.button('Preview JSON', on_click=self.preview_json).props('color=secondary')
    
    def update_device_list(self):
        """Update the device list display"""
        self.device_list_container.clear()
        
        with self.device_list_container:
            if not self.model.devices:
                ui.label('No devices configured').classes('text-grey')
            else:
                for i, device in enumerate(self.model.devices):
                    with ui.card().classes('w-full mb-2'):
                        with ui.card_section():
                            with ui.row().classes('w-full justify-between items-center'):
                                with ui.column():
                                    ui.label(f'{device.name} ({device.ipv4})').classes('text-subtitle1')
                                    ui.label(f'Entities: {", ".join(device.entities)}').classes('text-caption')
                                
                                with ui.row().classes('gap-2'):
                                    ui.button('Edit', 
                                            on_click=lambda d=device: self.edit_device(d)).props('size=sm color=secondary')
                                    ui.button('Delete', 
                                            on_click=lambda d=device: self.delete_device(d)).props('size=sm color=negative')
        
        # Update entity selects
        self.update_entity_selects()
    
    def update_connection_list(self):
        """Update the connection list display"""
        self.connection_list_container.clear()
        
        with self.connection_list_container:
            if not self.model.connections:
                ui.label('No connections configured').classes('text-grey')
            else:
                for connection in self.model.connections:
                    with ui.card().classes('w-full mb-2'):
                        with ui.card_section():
                            with ui.row().classes('w-full justify-between items-center'):
                                ui.label(f'{connection.from_entity} → {connection.to_entity} ({connection.connection_type})').classes('text-subtitle1')
                                ui.button('Delete', 
                                        on_click=lambda c=connection: self.delete_connection(c)).props('size=sm color=negative')
    
    def update_entity_selects(self):
        """Update the entity select dropdowns"""
        entities = self.model.get_all_entities()
        entity_options = {entity: entity for entity in entities}
        
        # Only update if the elements exist
        if 'from_entity' in self.ui_elements:
            self.ui_elements['from_entity'].options = entity_options
        if 'to_entity' in self.ui_elements:
            self.ui_elements['to_entity'].options = entity_options
    
    def add_device(self):
        """Add a new device"""
        name = self.ui_elements['device_name'].value
        ip = self.ui_elements['device_ip'].value
        entities_str = self.ui_elements['device_entities'].value
        
        if not name or not ip:
            ui.notify('Device name and IP address are required', type='negative')
            return
        
        entities = [e.strip() for e in entities_str.split(',') if e.strip()] if entities_str else []
        
        # Check for duplicate names
        if any(d.name == name for d in self.model.devices):
            ui.notify('Device name already exists', type='negative')
            return
        
        self.model.add_device(name, ip, entities)
        
        # Clear form
        self.ui_elements['device_name'].value = ''
        self.ui_elements['device_ip'].value = ''
        self.ui_elements['device_entities'].value = ''
        
        self.update_device_list()
        ui.notify(f'Device "{name}" added successfully', type='positive')
    
    def edit_device(self, device: Device):
        """Edit an existing device"""
        # Populate form with device data
        self.ui_elements['device_name'].value = device.name
        self.ui_elements['device_ip'].value = device.ipv4
        self.ui_elements['device_entities'].value = ','.join(device.entities)
        
        # Remove the device temporarily
        self.model.remove_device(device.name)
        self.update_device_list()
        
        ui.notify(f'Editing device "{device.name}". Click "Add Device" to save changes.', type='info')
    
    def delete_device(self, device: Device):
        """Delete a device"""
        self.model.remove_device(device.name)
        self.update_device_list()
        ui.notify(f'Device "{device.name}" deleted', type='info')
    
    def add_connection(self):
        """Add a new connection"""
        from_entity = self.ui_elements['from_entity'].value
        to_entity = self.ui_elements['to_entity'].value
        conn_type = self.ui_elements['connection_type'].value
        
        if not from_entity or not to_entity:
            ui.notify('Both source and destination entities must be selected', type='negative')
            return
        
        if from_entity == to_entity:
            ui.notify('Source and destination must be different', type='negative')
            return
        
        # Check for duplicate connections
        if any(c.from_entity == from_entity and c.to_entity == to_entity 
               for c in self.model.connections):
            ui.notify('Connection already exists', type='negative')
            return
        
        self.model.add_connection(from_entity, to_entity, conn_type)
        self.update_connection_list()
        ui.notify(f'Connection added: {from_entity} → {to_entity}', type='positive')
    
    def delete_connection(self, connection: Connection):
        """Delete a connection"""
        self.model.remove_connection(connection.from_entity, connection.to_entity)
        self.update_connection_list()
        ui.notify(f'Connection deleted: {connection.from_entity} → {connection.to_entity}', type='info')
    
    def create_topology_visualization(self):
        """Create the network topology visualization"""
        self.topology_container.clear()
        
        with self.topology_container:
            if not self.model.devices and not self.model.connections:
                with ui.column().classes('w-full h-full justify-center items-center'):
                    ui.icon('account_tree', size='4rem').classes('text-grey')
                    ui.label('No topology to display').classes('text-grey')
                    ui.label('Add devices and connections to see the network graph').classes('text-grey text-caption')
            else:
                # Simple text-based topology view for now
                with ui.column().classes('w-full p-4'):
                    ui.label('Network Topology (Text View)').classes('text-h6 mb-2')
                    
                    if self.model.devices:
                        ui.label('Devices:').classes('text-subtitle1 mb-1')
                        for device in self.model.devices:
                            ui.label(f'  • {device.name} ({device.ipv4}) - Entities: {", ".join(device.entities)}').classes('text-body2 mb-1')
                    
                    if self.model.connections:
                        ui.label('Connections:').classes('text-subtitle1 mb-1 mt-2')
                        for connection in self.model.connections:
                            ui.label(f'  • {connection.from_entity} → {connection.to_entity} ({connection.connection_type})').classes('text-body2 mb-1')
    
    def refresh_topology(self):
        """Refresh the topology visualization"""
        self.create_topology_visualization()
        ui.notify('Topology refreshed', type='info')
    
    def auto_layout(self):
        """Auto-layout the topology (placeholder)"""
        ui.notify('Auto-layout functionality coming soon!', type='info')
    
    def export_graph(self):
        """Export the graph visualization (placeholder)"""
        ui.notify('Graph export functionality coming soon!', type='info')
    
    def clear_all(self):
        """Clear all devices and connections"""
        with ui.dialog() as dialog, ui.card():
            ui.label('Clear All Data').classes('text-h6')
            ui.label('Are you sure you want to clear all devices and connections?')
            with ui.row().classes('gap-4 mt-4'):
                ui.button('Cancel', on_click=dialog.close)
                ui.button('Clear All', 
                         on_click=lambda: (self.model.reset(), self.update_device_list(), 
                                          self.update_connection_list(), self.create_topology_visualization(),
                                          dialog.close(), ui.notify('All data cleared', type='info'))
                         ).props('color=negative')
        dialog.open()
    
    def validate_topology(self):
        """Validate the network topology"""
        issues = []
        
        # Check for orphaned entities
        all_entities = set(self.model.get_all_entities())
        connected_entities = set()
        for conn in self.model.connections:
            connected_entities.add(conn.from_entity)
            connected_entities.add(conn.to_entity)
        
        orphaned = all_entities - connected_entities
        if orphaned:
            issues.append(f'Orphaned entities (not connected): {", ".join(orphaned)}')
        
        # Check for circular connections (basic check)
        # More sophisticated cycle detection could be implemented
        
        if not issues:
            ui.notify('Topology validation passed!', type='positive')
        else:
            ui.notify(f'Topology issues found: {"; ".join(issues)}', type='warning')
    
    def preview_json(self):
        """Show a preview of the generated JSON"""
        try:
            import json
            json_data = self.model.to_dict()
            formatted_json = json.dumps(json_data, indent=2)
            
            with ui.dialog() as dialog, ui.card().classes('w-96 max-w-full'):
                ui.label('Connection Map JSON').classes('text-h6')
                with ui.scroll_area().classes('w-full h-96'):
                    ui.code(formatted_json, language='json').classes('w-full')
                ui.button('Close', on_click=dialog.close)
            dialog.open()
        except Exception as e:
            ui.notify(f'Error generating JSON: {str(e)}', type='negative')
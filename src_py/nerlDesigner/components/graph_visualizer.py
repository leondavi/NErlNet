"""
Graph Visualizer Component - Interactive network topology visualization
"""

from typing import Dict, Any, List, Optional, Tuple
from nicegui import ui
import json
import uuid
from models.distributed_config_model import DistributedConfigModel

class GraphVisualizer:
    """UI component for visualizing network topology as interactive graphs"""
    
    def __init__(self, dc_model: Optional[DistributedConfigModel] = None):
        self.graph_data = {'nodes': [], 'edges': []}
        self.visualization_type = 'network'
        self.dc_model = dc_model or DistributedConfigModel()
        self.selected_nodes = []  # For connection creation
        self.connection_mode = False
        self.node_positions = {}  # For drag & drop positioning
        
    def create_ui(self):
        """Create the graph visualizer UI"""
        with ui.column().classes('w-full p-4'):
            ui.label('Interactive Network Graph Designer').classes('text-h5 mb-4')
            
            # Visualization Controls
            with ui.card().classes('w-full mb-4'):
                with ui.card_section():
                    ui.label('Visualization Controls').classes('text-h6 mb-2')
                    
                    with ui.row().classes('gap-4 w-full mb-4'):
                        # Visualization Type
                        ui.select(
                            {'network': 'Network Topology', 'hierarchy': 'Hierarchical', 'circular': 'Circular'},
                            label='Visualization Type',
                            value='network',
                            on_change=self.on_visualization_type_change
                        )
                        
                        # Layout Options
                        ui.select(
                            {'force': 'Force-directed', 'tree': 'Tree Layout', 'grid': 'Grid Layout'},
                            label='Layout Algorithm',
                            value='force',
                            on_change=self.on_layout_change
                        )
                        
                        # Display Options
                        ui.checkbox('Show Labels', value=True, on_change=self.on_labels_change)
                        ui.checkbox('Show Connection Types', value=True, on_change=self.on_connection_types_change)
                        
                        # Connection Mode Toggle
                        self.connection_mode_toggle = ui.checkbox(
                            'Connection Mode', 
                            value=False, 
                            on_change=self.toggle_connection_mode
                        ).classes('text-orange-700')
                    
                    with ui.row().classes('gap-4'):
                        ui.button('Load From DC Config', on_click=self.load_from_dc_config).classes('bg-orange-700 hover:bg-orange-600 text-white')
                        ui.button('Export Graph', on_click=self.export_graph).classes('bg-black hover:bg-gray-800 text-white')
                        ui.button('Clear Graph', on_click=self.clear_graph).classes('bg-gray-600 hover:bg-gray-500 text-white')
                        ui.button('Reset View', on_click=self.reset_view).classes('bg-gray-600 hover:bg-gray-500 text-white')
            
            # Component Palette
            with ui.card().classes('w-full mb-4'):
                with ui.card_section():
                    ui.label('Add Components').classes('text-h6 mb-2')
                    
                    with ui.row().classes('gap-2 flex-wrap'):
                        ui.button('Add Device', on_click=lambda: self.show_add_component_dialog('device')).classes('bg-blue-600 hover:bg-blue-500 text-white')
                        ui.button('Add Router', on_click=lambda: self.show_add_component_dialog('router')).classes('bg-green-600 hover:bg-green-500 text-white')
                        ui.button('Add Client', on_click=lambda: self.show_add_component_dialog('client')).classes('bg-purple-600 hover:bg-purple-500 text-white')
                        ui.button('Add Worker', on_click=lambda: self.show_add_component_dialog('worker')).classes('bg-orange-600 hover:bg-orange-500 text-white')
                        ui.button('Add Source', on_click=lambda: self.show_add_component_dialog('source')).classes('bg-teal-600 hover:bg-teal-500 text-white')
                        ui.button('Add Server', on_click=lambda: self.show_add_component_dialog('server')).classes('bg-red-600 hover:bg-red-500 text-white')
            
            # Connection Status
            self.connection_status = ui.label('Click components to connect them').classes('text-sm text-gray-600 mb-2').style('display: none')
            
            # Graph Container
            with ui.card().classes('w-full'):
                with ui.card_section():
                    ui.label('Interactive Graph').classes('text-h6 mb-2')
                    
                    # Graph visualization area with increased height
                    self.graph_container = ui.column().classes('w-full min-h-96 border-2 border-dashed border-gray-300 rounded-lg p-4')
                    self.create_interactive_graph_canvas()
            
            # Selected Components Info
            with ui.card().classes('w-full mt-4'):
                with ui.card_section():
                    with ui.row().classes('w-full items-center justify-between'):
                        ui.label('Graph Information').classes('text-h6 mb-2')
                        ui.button('Sync to DC Config', on_click=self.sync_to_dc_config).classes('bg-orange-700 hover:bg-orange-600 text-white')
                    
                    self.info_container = ui.column().classes('w-full')
                    self.update_graph_info()
    
    def create_interactive_graph_canvas(self):
        """Create the main interactive graph canvas"""
        self.graph_container.clear()
        
        with self.graph_container:
            if not self.graph_data.get('nodes'):
                with ui.column().classes('w-full h-full justify-center items-center min-h-80'):
                    ui.icon('account_tree', size='4rem').classes('text-gray-400')
                    ui.label('Interactive Graph Designer').classes('text-h6 text-gray-600')
                    ui.label('Add components using the buttons above, then connect them in Connection Mode').classes('text-gray-500 text-center')
                    
                    # Quick start buttons
                    with ui.row().classes('gap-2 mt-4'):
                        ui.button('Load Sample Network', on_click=self.load_sample_network).classes('bg-blue-600 hover:bg-blue-500 text-white')
                        ui.button('Import from DC Config', on_click=self.load_from_dc_config).classes('bg-orange-600 hover:bg-orange-500 text-white')
            else:
                self.render_interactive_graph()
    
    def render_interactive_graph(self):
        """Render the interactive graph with clickable components"""
        self.graph_container.clear()
        
        with self.graph_container:
            # Graph canvas with drag-drop area
            graph_canvas = ui.column().classes('w-full min-h-80 relative bg-gray-50 rounded-lg p-4')
            
            with graph_canvas:
                # Connection lines (drawn first, behind nodes)
                if self.graph_data.get('edges'):
                    for edge in self.graph_data['edges']:
                        self.render_connection_line(edge)
                
                # Node components
                for node in self.graph_data.get('nodes', []):
                    self.render_node_component(node)
    
    def render_node_component(self, node: Dict[str, Any]):
        """Render an interactive node component"""
        node_id = node.get('id', '')
        node_type = node.get('type', 'unknown')
        node_label = node.get('label', node_id)
        
        # Visual styling based on node type
        icon_map = {
            'device': 'computer',
            'router': 'router', 
            'client': 'person',
            'server': 'dns',
            'worker': 'psychology',
            'source': 'sensors'
        }
        
        color_map = {
            'device': 'blue',
            'router': 'green',
            'client': 'purple', 
            'server': 'red',
            'worker': 'orange',
            'source': 'teal'
        }
        
        icon = icon_map.get(node_type, 'circle')
        color = color_map.get(node_type, 'gray')
        
        # Check if node is selected for connection
        is_selected = node_id in self.selected_nodes
        selection_class = 'ring-4 ring-orange-400' if is_selected else ''
        
        # Position (for future drag & drop support)
        position = self.node_positions.get(node_id, {'x': 50, 'y': 50})
        
        with ui.card().classes(f'inline-block m-2 p-3 cursor-pointer hover:shadow-lg transition-all {selection_class}').style(
            f'position: relative; min-width: 120px; border: 2px solid {color}; background: white;'
        ):
            # Make the card clickable
            with ui.column().classes('items-center gap-2'):
                ui.icon(icon, size='2rem').classes(f'text-{color}-600')
                ui.label(node_label).classes('text-sm font-semibold text-center')
                ui.label(node_type.title()).classes('text-xs text-gray-500')
                
                # Connection port indicator
                if self.connection_mode:
                    ui.icon('radio_button_unchecked' if not is_selected else 'radio_button_checked', size='1rem').classes('text-orange-600')
            
            # Click handler for selection/connection
            ui.element().on('click', lambda node_id=node_id: self.on_node_click(node_id))
            
            # Context menu for node operations  
            with ui.context_menu():
                ui.menu_item('Edit Properties', on_click=lambda node_id=node_id: self.edit_node_properties(node_id))
                ui.menu_item('Delete Node', on_click=lambda node_id=node_id: self.delete_node(node_id))
                ui.separator()
                ui.menu_item('Set as Source', on_click=lambda node_id=node_id: self.set_node_as_source(node_id))
                ui.menu_item('Set as Target', on_click=lambda node_id=node_id: self.set_node_as_target(node_id))
    
    def render_connection_line(self, edge: Dict[str, Any]):
        """Render a connection line between nodes (simplified version)"""
        source_id = edge.get('source', '')
        target_id = edge.get('target', '')
        edge_type = edge.get('type', 'data')
        
        # For now, just show as a connection summary
        # In a full implementation, this would draw SVG lines between nodes
        with ui.row().classes('items-center gap-2 text-sm text-gray-600 mb-1'):
            ui.icon('arrow_forward', size='1rem')
            ui.label(f'{source_id} → {target_id}')
            ui.chip(edge_type, color='primary' if edge_type == 'data' else 'secondary').props('size=sm')
            ui.button(icon='delete', on_click=lambda: self.delete_connection(source_id, target_id)).props('size=sm flat').classes('text-red-500')
    
    def get_node_color(self, node_type: str) -> str:
        """Get color for different node types"""
        color_map = {
            'device': 'blue',
            'router': 'green', 
            'client': 'purple',
            'server': 'red',
            'worker': 'orange',
            'source': 'teal'
        }
        return color_map.get(node_type, 'grey')
    
    def update_graph_info(self):
        """Update the graph information panel"""
        self.info_container.clear()
        
        with self.info_container:
            nodes = self.graph_data.get('nodes', [])
            edges = self.graph_data.get('edges', [])
            
            if not nodes and not edges:
                ui.label('No components in graph').classes('text-gray-500')
                return
            
            with ui.grid(columns=3).classes('gap-4 w-full'):
                ui.label(f'Components: {len(nodes)}').classes('text-body1 font-semibold')
                ui.label(f'Connections: {len(edges)}').classes('text-body1 font-semibold')
                ui.label(f'Selected: {len(self.selected_nodes)}').classes('text-body1 font-semibold')
            
            # Node type breakdown
            if nodes:
                node_types = {}
                for node in nodes:
                    node_type = node.get('type', 'unknown')
                    node_types[node_type] = node_types.get(node_type, 0) + 1
                
                ui.label('Component Types:').classes('text-subtitle2 mt-3 mb-1')
                with ui.row().classes('gap-2 flex-wrap'):
                    for node_type, count in node_types.items():
                        color = self.get_node_color(node_type)
                        ui.chip(f'{node_type.title()}: {count}', color=color).props('size=sm')
            
            # Connection type breakdown  
            if edges:
                edge_types = {}
                for edge in edges:
                    edge_type = edge.get('type', 'data')
                    edge_types[edge_type] = edge_types.get(edge_type, 0) + 1
                
                ui.label('Connection Types:').classes('text-subtitle2 mt-3 mb-1')
                with ui.row().classes('gap-2 flex-wrap'):
                    for edge_type, count in edge_types.items():
                        ui.chip(f'{edge_type}: {count}', color='primary' if edge_type == 'data' else 'secondary').props('size=sm')
            
            # Selected nodes info
            if self.selected_nodes:
                ui.label('Selected Components:').classes('text-subtitle2 mt-3 mb-1')
                with ui.column().classes('gap-1'):
                    for node_id in self.selected_nodes:
                        node = next((n for n in nodes if n.get('id') == node_id), None)
                        if node:
                            ui.label(f'• {node.get("label", node_id)} ({node.get("type", "unknown")})').classes('text-sm')
    
    def on_visualization_type_change(self, e):
        """Handle visualization type change"""
        self.visualization_type = e.value
        if self.graph_data:
            self.create_interactive_graph(self.graph_data)
        ui.notify(f'Visualization type changed to {e.value}', type='info')
    
    def on_layout_change(self, e):
        """Handle layout algorithm change"""
        ui.notify(f'Layout changed to {e.value}', type='info')
    
    def on_labels_change(self, e):
        """Handle labels visibility change"""
        ui.notify(f'Labels {"shown" if e.value else "hidden"}', type='info')
    
    def on_connection_types_change(self, e):
        """Handle connection types visibility change"""
        ui.notify(f'Connection types {"shown" if e.value else "hidden"}', type='info')
    
    # Legacy methods kept for compatibility - now redirect to new interactive system
    def load_connection_map(self):
        """Load and visualize a connection map (legacy method)"""
        self.load_sample_network()
    
    def load_distributed_config(self):
        """Load and visualize a distributed configuration (legacy method)"""
        self.load_from_dc_config()
    
    def export_graph(self):
        """Export the graph visualization"""
        if not self.graph_data:
            ui.notify('No graph data to export', type='warning')
            return
        
        try:
            formatted_json = json.dumps(self.graph_data, indent=2)
            
            with ui.dialog() as dialog, ui.card().classes('w-96 max-w-full'):
                ui.label('Export Graph Data').classes('text-h6')
                with ui.scroll_area().classes('w-full h-96'):
                    ui.code(formatted_json, language='json').classes('w-full')
                ui.button('Close', on_click=dialog.close)
            dialog.open()
        except Exception as e:
            ui.notify(f'Error exporting graph: {str(e)}', type='negative')
    
    def reset_view(self):
        """Reset the graph view to default"""
        self.graph_data = {'nodes': [], 'edges': []}
        self.selected_nodes = []
        self.connection_mode = False
        self.connection_mode_toggle.value = False
        self.connection_status.style('display: none')
        self.create_interactive_graph_canvas()
        self.update_graph_info()
        ui.notify('Graph view reset', type='info')
    
    def clear_graph(self):
        """Clear all components and connections"""
        self.graph_data = {'nodes': [], 'edges': []}
        self.selected_nodes = []
        self.node_positions = {}
        self.create_interactive_graph_canvas()
        self.update_graph_info()
        ui.notify('Graph cleared', type='positive')
    
    def toggle_connection_mode(self, e):
        """Toggle connection creation mode"""
        self.connection_mode = e.value
        self.selected_nodes = []  # Clear selections when toggling
        
        if self.connection_mode:
            self.connection_status.style('display: block')
            self.connection_status.text = 'Connection Mode: Click two components to connect them'
            ui.notify('Connection mode enabled - click components to connect', type='info')
        else:
            self.connection_status.style('display: none')
            ui.notify('Connection mode disabled', type='info')
        
        # Re-render to show/hide connection indicators  
        if self.graph_data.get('nodes'):
            self.render_interactive_graph()
        self.update_graph_info()
    
    def on_node_click(self, node_id: str):
        """Handle node click for selection and connection"""
        if not self.connection_mode:
            # Just select/deselect the node
            if node_id in self.selected_nodes:
                self.selected_nodes.remove(node_id)
            else:
                self.selected_nodes = [node_id]  # Single selection when not in connection mode
        else:
            # Connection mode logic
            if node_id in self.selected_nodes:
                self.selected_nodes.remove(node_id)
                self.connection_status.text = f'Deselected {node_id}. Click components to connect them.'
            else:
                self.selected_nodes.append(node_id)
                
                if len(self.selected_nodes) == 1:
                    self.connection_status.text = f'Selected {node_id}. Click another component to create connection.'
                elif len(self.selected_nodes) == 2:
                    # Create connection
                    source_id, target_id = self.selected_nodes
                    self.create_connection(source_id, target_id)
                    self.selected_nodes = []  # Clear after connection
                    self.connection_status.text = 'Connection created! Click components to create more connections.'
                elif len(self.selected_nodes) > 2:
                    # Reset to just the current selection
                    self.selected_nodes = [node_id]
                    self.connection_status.text = f'Selected {node_id}. Click another component to create connection.'
        
        # Re-render to show selection changes
        if self.graph_data.get('nodes'):
            self.render_interactive_graph()
        self.update_graph_info()
    
    def create_connection(self, source_id: str, target_id: str, connection_type: str = 'data'):
        """Create a connection between two nodes"""
        # Check if connection already exists
        existing_connection = next((
            edge for edge in self.graph_data.get('edges', [])
            if (edge.get('source') == source_id and edge.get('target') == target_id) or
               (edge.get('source') == target_id and edge.get('target') == source_id)
        ), None)
        
        if existing_connection:
            ui.notify(f'Connection already exists between {source_id} and {target_id}', type='warning')
            return False
        
        # Create new connection
        new_edge = {
            'id': f'{source_id}-{target_id}',
            'source': source_id,
            'target': target_id,
            'type': connection_type,
            'bidirectional': False
        }
        
        if 'edges' not in self.graph_data:
            self.graph_data['edges'] = []
        
        self.graph_data['edges'].append(new_edge)
        
        # Get node names for notification
        source_node = next((n for n in self.graph_data.get('nodes', []) if n.get('id') == source_id), None)
        target_node = next((n for n in self.graph_data.get('nodes', []) if n.get('id') == target_id), None)
        
        source_name = source_node.get('label', source_id) if source_node else source_id
        target_name = target_node.get('label', target_id) if target_node else target_id
        
        ui.notify(f'Connected {source_name} → {target_name}', type='positive')
        return True
    
    def delete_connection(self, source_id: str, target_id: str):
        """Delete a connection between nodes"""
        initial_count = len(self.graph_data.get('edges', []))
        
        self.graph_data['edges'] = [
            edge for edge in self.graph_data.get('edges', [])
            if not ((edge.get('source') == source_id and edge.get('target') == target_id) or
                   (edge.get('source') == target_id and edge.get('target') == source_id))
        ]
        
        if len(self.graph_data['edges']) < initial_count:
            ui.notify(f'Connection deleted between {source_id} and {target_id}', type='positive')
            self.render_interactive_graph()
            self.update_graph_info()
        else:
            ui.notify('Connection not found', type='warning')
    
    def show_add_component_dialog(self, component_type: str):
        """Show dialog to add a new component"""
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-md'):
            ui.label(f'Add {component_type.title()} Component').classes('text-h6 mb-4')
            
            # Component properties form
            name_input = ui.input(
                label='Component Name',
                placeholder=f'Enter {component_type} name'
            ).classes('w-full mb-2')
            
            label_input = ui.input(
                label='Display Label',
                placeholder='Optional display label'
            ).classes('w-full mb-2')
            
            # Type-specific properties
            properties = {}
            
            if component_type in ['device', 'server']:
                properties['ip'] = ui.input(
                    label='IP Address',
                    placeholder='192.168.1.1'
                ).classes('w-full mb-2')
                
                properties['port'] = ui.number(
                    label='Port',
                    value=8080,
                    min=1000,
                    max=65535
                ).classes('w-full mb-2')
            
            elif component_type == 'router':
                properties['port'] = ui.number(
                    label='Port',
                    value=9090,
                    min=1000,
                    max=65535
                ).classes('w-full mb-2')
                
                properties['policy'] = ui.select(
                    options={'0': 'Round Robin', '1': 'Load Balance', '2': 'Priority'},
                    label='Routing Policy',
                    value='0'
                ).classes('w-full mb-2')
            
            elif component_type == 'client':
                properties['port'] = ui.number(
                    label='Port',
                    value=8090,
                    min=1000,
                    max=65535
                ).classes('w-full mb-2')
                
                properties['workers'] = ui.input(
                    label='Associated Workers',
                    placeholder='worker1,worker2'
                ).classes('w-full mb-2')
            
            elif component_type == 'worker':
                properties['worker_file'] = ui.input(
                    label='Worker Configuration File',
                    placeholder='path/to/worker.json'
                ).classes('w-full mb-2')
            
            elif component_type == 'source':
                properties['data_type'] = ui.select(
                    options={'image': 'Image Data', 'text': 'Text Data', 'sensor': 'Sensor Data', 'file': 'File Data'},
                    label='Data Type',
                    value='image'
                ).classes('w-full mb-2')
            
            def add_component():
                """Add the component to the graph"""
                if not name_input.value:
                    ui.notify('Component name is required', type='warning')
                    return
                
                # Check for duplicate names
                existing_names = [node.get('id') for node in self.graph_data.get('nodes', [])]
                if name_input.value in existing_names:
                    ui.notify('Component name already exists', type='warning')
                    return
                
                # Create new node
                new_node = {
                    'id': name_input.value,
                    'label': label_input.value or name_input.value,
                    'type': component_type,
                    'properties': {}
                }
                
                # Add type-specific properties
                for prop_name, prop_input in properties.items():
                    if hasattr(prop_input, 'value'):
                        new_node['properties'][prop_name] = prop_input.value
                
                # Add to graph
                if 'nodes' not in self.graph_data:
                    self.graph_data['nodes'] = []
                
                self.graph_data['nodes'].append(new_node)
                
                # Set initial position (could be enhanced with better positioning logic)
                self.node_positions[name_input.value] = {
                    'x': 50 + len(self.graph_data['nodes']) * 30,
                    'y': 50 + (len(self.graph_data['nodes']) % 3) * 80
                }
                
                # Re-render graph
                self.create_interactive_graph_canvas()
                self.update_graph_info()
                
                ui.notify(f'{component_type.title()} "{name_input.value}" added successfully', type='positive')
                dialog.close()
            
            # Dialog buttons
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Add Component', on_click=add_component).classes('bg-orange-700 hover:bg-orange-600 text-white')
                ui.button('Cancel', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    def edit_node_properties(self, node_id: str):
        """Edit properties of an existing node"""
        node = next((n for n in self.graph_data.get('nodes', []) if n.get('id') == node_id), None)
        if not node:
            ui.notify('Node not found', type='warning')
            return
        
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-md'):
            ui.label(f'Edit {node.get("type", "").title()} Properties').classes('text-h6 mb-4')
            
            # Editable properties
            label_input = ui.input(
                label='Display Label',
                value=node.get('label', '')
            ).classes('w-full mb-2')
            
            # Type-specific property editors
            property_inputs = {}
            for prop_name, prop_value in node.get('properties', {}).items():
                if isinstance(prop_value, str):
                    property_inputs[prop_name] = ui.input(
                        label=prop_name.replace('_', ' ').title(),
                        value=prop_value
                    ).classes('w-full mb-2')
                elif isinstance(prop_value, (int, float)):
                    property_inputs[prop_name] = ui.number(
                        label=prop_name.replace('_', ' ').title(),
                        value=prop_value
                    ).classes('w-full mb-2')
                else:
                    property_inputs[prop_name] = ui.input(
                        label=prop_name.replace('_', ' ').title(),
                        value=str(prop_value)
                    ).classes('w-full mb-2')
            
            def save_changes():
                """Save changes to the node"""
                node['label'] = label_input.value or node_id
                
                for prop_name, prop_input in property_inputs.items():
                    node['properties'][prop_name] = prop_input.value
                
                self.render_interactive_graph()
                ui.notify(f'Properties updated for {node_id}', type='positive')
                dialog.close()
            
            # Dialog buttons
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Save Changes', on_click=save_changes).classes('bg-orange-700 hover:bg-orange-600 text-white')
                ui.button('Cancel', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    def delete_node(self, node_id: str):
        """Delete a node and all its connections"""
        # Remove the node
        initial_node_count = len(self.graph_data.get('nodes', []))
        self.graph_data['nodes'] = [n for n in self.graph_data.get('nodes', []) if n.get('id') != node_id]
        
        # Remove connections involving this node
        initial_edge_count = len(self.graph_data.get('edges', []))
        self.graph_data['edges'] = [
            e for e in self.graph_data.get('edges', [])
            if e.get('source') != node_id and e.get('target') != node_id
        ]
        
        # Remove from selections
        if node_id in self.selected_nodes:
            self.selected_nodes.remove(node_id)
        
        # Remove position
        if node_id in self.node_positions:
            del self.node_positions[node_id]
        
        if len(self.graph_data['nodes']) < initial_node_count:
            edges_removed = initial_edge_count - len(self.graph_data.get('edges', []))
            ui.notify(f'Deleted node {node_id} and {edges_removed} connections', type='positive')
            self.create_interactive_graph_canvas()
            self.update_graph_info()
        else:
            ui.notify('Node not found', type='warning')
    
    def set_node_as_source(self, node_id: str):
        """Mark a node as a data source"""
        node = next((n for n in self.graph_data.get('nodes', []) if n.get('id') == node_id), None)
        if node:
            if 'properties' not in node:
                node['properties'] = {}
            node['properties']['is_source'] = True
            ui.notify(f'Marked {node_id} as data source', type='positive')
            self.render_interactive_graph()
    
    def set_node_as_target(self, node_id: str):
        """Mark a node as a data target"""
        node = next((n for n in self.graph_data.get('nodes', []) if n.get('id') == node_id), None)
        if node:
            if 'properties' not in node:
                node['properties'] = {}
            node['properties']['is_target'] = True
            ui.notify(f'Marked {node_id} as data target', type='positive')
            self.render_interactive_graph()
    
    def load_from_dc_config(self):
        """Load components from the distributed configuration"""
        if not self.dc_model:
            ui.notify('No distributed configuration model available', type='warning')
            return
        
        # Clear existing graph
        self.graph_data = {'nodes': [], 'edges': []}
        
        # Add devices
        for device in self.dc_model.devices:
            node = {
                'id': device.name,
                'label': device.name,
                'type': 'device',
                'properties': {
                    'ip': device.ipv4,
                    'entities': device.entities
                }
            }
            self.graph_data['nodes'].append(node)
        
        # Add routers
        for router in self.dc_model.routers:
            node = {
                'id': router.name,
                'label': router.name,
                'type': 'router',
                'properties': {
                    'port': router.port,
                    'policy': router.policy
                }
            }
            self.graph_data['nodes'].append(node)
        
        # Add clients
        for client in self.dc_model.clients:
            node = {
                'id': client.name,
                'label': client.name,
                'type': 'client',
                'properties': {
                    'port': client.port,
                    'workers': client.workers
                }
            }
            self.graph_data['nodes'].append(node)
        
        # Add workers
        for worker in self.dc_model.workers:
            node = {
                'id': worker.worker_id,
                'label': worker.worker_id,
                'type': 'worker',
                'properties': {
                    'worker_file': worker.worker_file
                }
            }
            self.graph_data['nodes'].append(node)
        
        # Add main and API servers
        if self.dc_model.main_server.port:
            node = {
                'id': 'mainServer',
                'label': 'Main Server',
                'type': 'server',
                'properties': {
                    'port': self.dc_model.main_server.port,
                    'args': self.dc_model.main_server.args
                }
            }
            self.graph_data['nodes'].append(node)
        
        if self.dc_model.api_server.port:
            node = {
                'id': 'apiServer',
                'label': 'API Server',
                'type': 'server',
                'properties': {
                    'port': self.dc_model.api_server.port,
                    'args': self.dc_model.api_server.args
                }
            }
            self.graph_data['nodes'].append(node)
        
        # Generate some initial positions
        for i, node in enumerate(self.graph_data['nodes']):
            self.node_positions[node['id']] = {
                'x': 50 + (i % 4) * 150,
                'y': 50 + (i // 4) * 100
            }
        
        self.create_interactive_graph_canvas()
        self.update_graph_info()
        
        component_count = len(self.graph_data['nodes'])
        ui.notify(f'Loaded {component_count} components from distributed configuration', type='positive')
    
    def sync_to_dc_config(self):
        """Synchronize graph components back to distributed configuration"""
        if not self.dc_model:
            ui.notify('No distributed configuration model available', type='warning')
            return
        
        # Clear existing components in DC model
        self.dc_model.devices.clear()
        self.dc_model.routers.clear()
        self.dc_model.clients.clear()
        self.dc_model.workers.clear()
        
        # Sync nodes back to DC model
        for node in self.graph_data.get('nodes', []):
            node_type = node.get('type')
            node_id = node.get('id')
            properties = node.get('properties', {})
            
            if node_type == 'device':
                self.dc_model.add_device(
                    name=node_id,
                    ipv4=properties.get('ip', '127.0.0.1'),
                    entities=properties.get('entities', '')
                )
            elif node_type == 'router':
                self.dc_model.add_router(
                    name=node_id,
                    port=str(properties.get('port', '9090')),
                    policy=str(properties.get('policy', '0'))
                )
            elif node_type == 'client':
                self.dc_model.add_client(
                    name=node_id,
                    port=str(properties.get('port', '8090')),
                    workers=properties.get('workers', '')
                )
            elif node_type == 'worker':
                from models.distributed_config_model import WorkerDefinition
                worker = WorkerDefinition(
                    worker_id=node_id,
                    worker_file=properties.get('worker_file', '')
                )
                self.dc_model.workers.append(worker)
            elif node_type == 'server':
                if node_id == 'mainServer':
                    self.dc_model.main_server.port = str(properties.get('port', '8080'))
                    self.dc_model.main_server.args = properties.get('args', '')
                elif node_id == 'apiServer':
                    self.dc_model.api_server.port = str(properties.get('port', '8081'))
                    self.dc_model.api_server.args = properties.get('args', '')
        
        synced_count = len(self.graph_data.get('nodes', []))
        ui.notify(f'Synchronized {synced_count} components to distributed configuration', type='positive')
    
    def load_sample_network(self):
        """Load a sample network for demonstration"""
        self.graph_data = {
            'nodes': [
                {'id': 'source1', 'label': 'Data Source', 'type': 'source', 'properties': {'data_type': 'image'}},
                {'id': 'router1', 'label': 'Main Router', 'type': 'router', 'properties': {'port': '9090', 'policy': '0'}},
                {'id': 'client1', 'label': 'Client A', 'type': 'client', 'properties': {'port': '8090', 'workers': 'worker1'}},
                {'id': 'client2', 'label': 'Client B', 'type': 'client', 'properties': {'port': '8091', 'workers': 'worker2'}},
                {'id': 'worker1', 'label': 'Worker 1', 'type': 'worker', 'properties': {'worker_file': 'worker1.json'}},
                {'id': 'worker2', 'label': 'Worker 2', 'type': 'worker', 'properties': {'worker_file': 'worker2.json'}},
                {'id': 'mainServer', 'label': 'Main Server', 'type': 'server', 'properties': {'port': '8080', 'ip': '127.0.0.1'}}
            ],
            'edges': [
                {'id': 'source1-router1', 'source': 'source1', 'target': 'router1', 'type': 'data'},
                {'id': 'router1-client1', 'source': 'router1', 'target': 'client1', 'type': 'data'},
                {'id': 'router1-client2', 'source': 'router1', 'target': 'client2', 'type': 'data'},
                {'id': 'client1-worker1', 'source': 'client1', 'target': 'worker1', 'type': 'task'},
                {'id': 'client2-worker2', 'source': 'client2', 'target': 'worker2', 'type': 'task'},
                {'id': 'worker1-mainServer', 'source': 'worker1', 'target': 'mainServer', 'type': 'result'},
                {'id': 'worker2-mainServer', 'source': 'worker2', 'target': 'mainServer', 'type': 'result'}
            ]
        }
        
        # Set positions for sample nodes
        positions = {
            'source1': {'x': 50, 'y': 150},
            'router1': {'x': 200, 'y': 150},
            'client1': {'x': 350, 'y': 100},
            'client2': {'x': 350, 'y': 200},
            'worker1': {'x': 500, 'y': 100},
            'worker2': {'x': 500, 'y': 200},
            'mainServer': {'x': 650, 'y': 150}
        }
        self.node_positions.update(positions)
        
        self.create_interactive_graph_canvas()
        self.update_graph_info()
        ui.notify('Sample network loaded', type='positive')
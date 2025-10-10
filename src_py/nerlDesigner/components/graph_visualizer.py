"""
Graph Visualizer Component - Interactive network topology visualization
"""

from typing import Dict, Any, List, Optional
from nicegui import ui
import json

class GraphVisualizer:
    """UI component for visualizing network topology as interactive graphs"""
    
    def __init__(self):
        self.graph_data = {}
        self.visualization_type = 'network'
        
    def create_ui(self):
        """Create the graph visualizer UI"""
        with ui.column().classes('w-full p-4'):
            ui.label('Network Graph Visualization').classes('text-h5 mb-4')
            
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
                    
                    with ui.row().classes('gap-4'):
                        ui.button('Load Connection Map', on_click=self.load_connection_map).props('color=primary')
                        ui.button('Load Distributed Config', on_click=self.load_distributed_config).props('color=primary')
                        ui.button('Export Graph', on_click=self.export_graph).props('color=secondary')
                        ui.button('Reset View', on_click=self.reset_view).props('color=secondary')
            
            # Graph Container
            with ui.card().classes('w-full'):
                with ui.card_section():
                    ui.label('Interactive Graph').classes('text-h6 mb-2')
                    
                    # Graph visualization area
                    self.graph_container = ui.column().classes('w-full h-96 border')
                    self.create_placeholder_graph()
            
            # Graph Information Panel
            with ui.card().classes('w-full mt-4'):
                with ui.card_section():
                    ui.label('Graph Information').classes('text-h6 mb-2')
                    
                    self.info_container = ui.column().classes('w-full')
                    self.update_graph_info()
    
    def create_placeholder_graph(self):
        """Create a placeholder graph visualization"""
        self.graph_container.clear()
        
        with self.graph_container:
            with ui.column().classes('w-full h-full justify-center items-center'):
                ui.icon('account_tree', size='4rem').classes('text-grey')
                ui.label('Interactive Graph Visualization').classes('text-h6 text-grey')
                ui.label('Load a connection map or distributed config to see the network graph').classes('text-grey text-center')
                
                # Sample network visualization using ASCII art for now
                with ui.card().classes('mt-4 p-4 bg-grey-1'):
                    ui.label('Sample Network Topology:').classes('text-subtitle1 mb-2')
                    ui.code('''
    [Source] ──→ [Router 1] ──→ [Client 1]
         │            │              ↓
         │            ↓          [Worker 1]
         │       [Router 2] ──→ [Client 2]
         │            │              ↓
         └────────────┴──────→ [Main Server]
                    ''', language='text').classes('text-sm')
    
    def create_interactive_graph(self, graph_data: Dict[str, Any]):
        """Create an interactive graph from the provided data"""
        self.graph_container.clear()
        
        with self.graph_container:
            # For now, create a structured text representation
            # In a full implementation, this would use a library like Cytoscape.js or D3.js
            
            if 'nodes' in graph_data and 'edges' in graph_data:
                with ui.column().classes('w-full p-4'):
                    ui.label('Network Graph').classes('text-h6 mb-2')
                    
                    # Nodes section
                    ui.label('Nodes:').classes('text-subtitle1 mb-1')
                    for node in graph_data['nodes']:
                        node_type = node.get('type', 'unknown')
                        node_label = node.get('label', node.get('id', 'unnamed'))
                        
                        icon_map = {
                            'device': 'computer',
                            'router': 'router',
                            'client': 'person',
                            'server': 'dns',
                            'worker': 'psychology',
                            'source': 'sensors'
                        }
                        icon = icon_map.get(node_type, 'circle')
                        
                        with ui.row().classes('items-center gap-2 mb-1'):
                            ui.icon(icon).classes(f'text-{self.get_node_color(node_type)}')
                            ui.label(f'{node_label} ({node_type})').classes('text-body2')
                    
                    # Edges section
                    if graph_data['edges']:
                        ui.label('Connections:').classes('text-subtitle1 mb-1 mt-3')
                        for edge in graph_data['edges']:
                            source = edge.get('source', 'unknown')
                            target = edge.get('target', 'unknown')
                            edge_type = edge.get('type', 'data')
                            
                            with ui.row().classes('items-center gap-2 mb-1'):
                                ui.icon('arrow_forward').classes('text-blue')
                                ui.label(f'{source} → {target}').classes('text-body2')
                                ui.chip(edge_type, color='primary' if edge_type == 'data' else 'secondary').props('size=sm')
            else:
                with ui.column().classes('w-full h-full justify-center items-center'):
                    ui.icon('error', size='2rem').classes('text-orange')
                    ui.label('Invalid graph data format').classes('text-grey')
    
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
            if not self.graph_data:
                ui.label('No graph data loaded').classes('text-grey')
            else:
                nodes = self.graph_data.get('nodes', [])
                edges = self.graph_data.get('edges', [])
                
                with ui.grid(columns=2).classes('gap-4 w-full'):
                    ui.label(f'Total Nodes: {len(nodes)}').classes('text-body1')
                    ui.label(f'Total Connections: {len(edges)}').classes('text-body1')
                    
                    # Node type counts
                    node_types = {}
                    for node in nodes:
                        node_type = node.get('type', 'unknown')
                        node_types[node_type] = node_types.get(node_type, 0) + 1
                    
                    if node_types:
                        ui.label('Node Types:').classes('text-subtitle2 mt-2')
                        for node_type, count in node_types.items():
                            ui.label(f'  {node_type.title()}: {count}').classes('text-body2')
    
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
    
    def load_connection_map(self):
        """Load and visualize a connection map"""
        # This would typically open a file dialog or load from the current connection model
        sample_connection_data = {
            'nodes': [
                {'id': 'r1', 'label': 'Router 1', 'type': 'router'},
                {'id': 'r2', 'label': 'Router 2', 'type': 'router'},
                {'id': 'c1', 'label': 'Client 1', 'type': 'client'},
                {'id': 'c2', 'label': 'Client 2', 'type': 'client'},
                {'id': 's1', 'label': 'Source 1', 'type': 'source'},
                {'id': 'mainServer', 'label': 'Main Server', 'type': 'server'}
            ],
            'edges': [
                {'source': 's1', 'target': 'r1', 'type': 'data'},
                {'source': 'r1', 'target': 'c1', 'type': 'data'},
                {'source': 'r1', 'target': 'r2', 'type': 'data'},
                {'source': 'r2', 'target': 'c2', 'type': 'data'},
                {'source': 'c1', 'target': 'mainServer', 'type': 'result'},
                {'source': 'c2', 'target': 'mainServer', 'type': 'result'}
            ]
        }
        
        self.graph_data = sample_connection_data
        self.create_interactive_graph(sample_connection_data)
        self.update_graph_info()
        ui.notify('Sample connection map loaded', type='positive')
    
    def load_distributed_config(self):
        """Load and visualize a distributed configuration"""
        sample_distributed_data = {
            'nodes': [
                {'id': 'NerlControllerG', 'label': 'NerlController G', 'type': 'device'},
                {'id': 'NerlControllerD', 'label': 'NerlController D', 'type': 'device'},
                {'id': 'Nerl-Powerful', 'label': 'Nerl Powerful', 'type': 'device'},
                {'id': 'mainServer', 'label': 'Main Server', 'type': 'server'},
                {'id': 'apiServer', 'label': 'API Server', 'type': 'server'}
            ],
            'edges': [
                {'source': 'NerlControllerG', 'target': 'NerlControllerD', 'type': 'control'},
                {'source': 'NerlControllerD', 'target': 'mainServer', 'type': 'hosting'},
                {'source': 'NerlControllerD', 'target': 'apiServer', 'type': 'hosting'},
                {'source': 'Nerl-Powerful', 'target': 'NerlControllerG', 'type': 'data'}
            ]
        }
        
        self.graph_data = sample_distributed_data
        self.create_interactive_graph(sample_distributed_data)
        self.update_graph_info()
        ui.notify('Sample distributed config loaded', type='positive')
    
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
        self.graph_data = {}
        self.create_placeholder_graph()
        self.update_graph_info()
        ui.notify('Graph view reset', type='info')
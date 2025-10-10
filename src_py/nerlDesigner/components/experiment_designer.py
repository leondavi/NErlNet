"""
Experiment Designer Component - UI for designing experiment configurations
"""

from typing import Dict, Any, List
import json
from nicegui import ui

class ExperimentDesigner:
    """UI component for designing experiment configurations"""
    
    def __init__(self):
        self.experiment_data = {
            'experimentName': '',
            'experimentType': 'classification',
            'batchSize': 50,
            'csvFilePath': '',
            'numOfFeatures': '',
            'numOfLabels': '',
            'headersNames': '',
            'Phases': []
        }
        
    def create_ui(self):
        """Create the experiment designer UI"""
        with ui.column().classes('w-full h-full p-4'):
            # Header
            with ui.row().classes('w-full items-center mb-4 p-3 bg-purple-900 rounded-lg'):
                ui.icon('science', size='md').classes('text-white')
                ui.label('Experiment Designer').classes('text-h6 font-bold text-white')
                ui.space()
                ui.button('Export Experiment', on_click=self.export_experiment, 
                         icon='download').classes('bg-green-700 hover:bg-green-600 text-white')
            
            # Basic Experiment Settings
            with ui.card().classes('w-full mb-4'):
                ui.label('Basic Settings').classes('text-h6 font-bold mb-3')
                
                with ui.grid(columns=2).classes('gap-4 w-full'):
                    self.exp_name_input = ui.input('Experiment Name', 
                                                  placeholder='mnist_rr',
                                                  value=self.experiment_data['experimentName']).classes('w-full')
                    
                    self.exp_type_select = ui.select(
                        options={'classification': 'Classification', 'regression': 'Regression', 'clustering': 'Clustering'},
                        label='Experiment Type',
                        value=self.experiment_data['experimentType']
                    ).classes('w-full')
                    
                    self.batch_size_input = ui.number('Batch Size', 
                                                    value=self.experiment_data['batchSize'],
                                                    min=1, step=1).classes('w-full')
                    
                    self.csv_path_input = ui.input('CSV File Path',
                                                  placeholder='/tmp/nerlnet/data/...',
                                                  value=self.experiment_data['csvFilePath']).classes('w-full')
                    
                    self.num_features_input = ui.input('Number of Features',
                                                      placeholder='784',
                                                      value=self.experiment_data['numOfFeatures']).classes('w-full')
                    
                    self.num_labels_input = ui.input('Number of Labels',
                                                    placeholder='10',
                                                    value=self.experiment_data['numOfLabels']).classes('w-full')
                    
                    self.headers_input = ui.input('Header Names',
                                                 placeholder='0,1,2,3,4,5,6,7,8,9',
                                                 value=self.experiment_data['headersNames']).classes('w-full')
            
            # Phases Configuration
            with ui.card().classes('w-full'):
                with ui.row().classes('w-full items-center justify-between mb-3'):
                    ui.label('Experiment Phases').classes('text-h6 font-bold')
                    ui.button('Add Phase', on_click=self.show_add_phase_dialog, 
                             icon='add').classes('bg-purple-700 hover:bg-purple-600 text-white')
                
                # Phases container
                self.phases_container = ui.column().classes('w-full')
                self.render_phases()
    
    def render_phases(self):
        """Render all experiment phases"""
        self.phases_container.clear()
        
        if not self.experiment_data['Phases']:
            with self.phases_container:
                ui.label('No phases configured. Add phases to define experiment flow.').classes('text-center text-gray-500 py-8')
            return
        
        with self.phases_container:
            for i, phase in enumerate(self.experiment_data['Phases']):
                self.render_phase_card(phase, i)
    
    def render_phase_card(self, phase: Dict[str, Any], index: int):
        """Render a single phase card"""
        with ui.card().classes('w-full mb-3 border-l-4 border-purple-400'):
            with ui.card_section():
                # Phase header
                with ui.row().classes('w-full items-center justify-between mb-2'):
                    with ui.column():
                        ui.label(f"Phase {index + 1}: {phase['phaseName']}").classes('text-h6 font-bold')
                        ui.label(f"Type: {phase['phaseType']}").classes('text-sm text-gray-600')
                    
                    with ui.row().classes('gap-2'):
                        ui.button('Edit', on_click=lambda idx=index: self.edit_phase(idx),
                                 icon='edit').props('size=sm').classes('bg-black hover:bg-gray-800 text-white')
                        ui.button('Delete', on_click=lambda idx=index: self.delete_phase(idx),
                                 icon='delete').props('size=sm').classes('bg-red-600 hover:bg-red-500 text-white')
                
                # Source pieces summary
                source_pieces = phase.get('sourcePieces', [])
                if source_pieces:
                    ui.label(f"Source Pieces: {len(source_pieces)}").classes('text-sm')
                    
                    # Show source pieces in a compact table
                    with ui.expansion(f'Source Pieces ({len(source_pieces)})', icon='list').classes('w-full mt-2'):
                        with ui.grid(columns=6).classes('gap-2 w-full text-sm'):
                            # Headers
                            ui.label('Source').classes('font-bold')
                            ui.label('Start Sample').classes('font-bold')
                            ui.label('Batches').classes('font-bold')
                            ui.label('Workers').classes('font-bold')
                            ui.label('Tensor Type').classes('font-bold')
                            ui.label('Actions').classes('font-bold')
                            
                            # Source piece rows
                            for j, piece in enumerate(source_pieces):
                                ui.label(piece.get('sourceName', ''))
                                ui.label(piece.get('startingSample', ''))
                                ui.label(piece.get('numOfBatches', ''))
                                ui.label(piece.get('workers', ''))
                                ui.label(piece.get('nerltensorType', ''))
                                ui.button('Edit', on_click=lambda p_idx=index, s_idx=j: self.edit_source_piece(p_idx, s_idx),
                                         icon='edit').props('size=xs flat').classes('text-black')
    
    def show_add_phase_dialog(self):
        """Show dialog to add a new phase"""
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 600px; max-width: 90vw'):
                ui.label('Add New Phase').classes('text-h6 mb-4')
                
                phase_name_input = ui.input('Phase Name', placeholder='training_phase1').classes('w-full mb-3')
                
                phase_type_select = ui.select(
                    options={'training': 'Training', 'prediction': 'Prediction', 'validation': 'Validation'},
                    label='Phase Type',
                    value='training'
                ).classes('w-full mb-4')
                
                # Source pieces section
                ui.label('Source Pieces').classes('text-subtitle1 font-bold mb-2')
                source_pieces = []
                
                def add_source_piece():
                    source_pieces.append({
                        'sourceName': '',
                        'startingSample': '0',
                        'numOfBatches': '50',
                        'workers': 'w1',
                        'nerltensorType': 'float'
                    })
                    render_source_pieces()
                
                def render_source_pieces():
                    source_container.clear()
                    with source_container:
                        for i, piece in enumerate(source_pieces):
                            with ui.card().classes('w-full mb-2 p-2 bg-gray-50'):
                                with ui.grid(columns=3).classes('gap-2 w-full'):
                                    piece_source = ui.input('Source Name', value=piece['sourceName'], placeholder='s1').classes('w-full')
                                    piece_start = ui.input('Starting Sample', value=piece['startingSample'], placeholder='0').classes('w-full')
                                    piece_batches = ui.input('Num Batches', value=piece['numOfBatches'], placeholder='50').classes('w-full')
                                    piece_workers = ui.input('Workers', value=piece['workers'], placeholder='w1').classes('w-full')
                                    piece_tensor = ui.select(
                                        options={'float': 'Float', 'int': 'Integer', 'double': 'Double'},
                                        label='Tensor Type',
                                        value=piece['nerltensorType']
                                    ).classes('w-full')
                                    ui.button('Remove', on_click=lambda idx=i: remove_source_piece(idx),
                                             icon='delete').props('size=sm').classes('bg-red-500 text-white')
                                    
                                    # Update piece data when inputs change
                                    piece_source.on('update:model-value', lambda e, idx=i: update_piece(idx, 'sourceName', e.value))
                                    piece_start.on('update:model-value', lambda e, idx=i: update_piece(idx, 'startingSample', e.value))
                                    piece_batches.on('update:model-value', lambda e, idx=i: update_piece(idx, 'numOfBatches', e.value))
                                    piece_workers.on('update:model-value', lambda e, idx=i: update_piece(idx, 'workers', e.value))
                                    piece_tensor.on('update:model-value', lambda e, idx=i: update_piece(idx, 'nerltensorType', e.value))
                
                def update_piece(idx, field, value):
                    if idx < len(source_pieces):
                        source_pieces[idx][field] = value
                
                def remove_source_piece(idx):
                    if idx < len(source_pieces):
                        source_pieces.pop(idx)
                        render_source_pieces()
                
                source_container = ui.column().classes('w-full mb-3')
                render_source_pieces()
                
                ui.button('Add Source Piece', on_click=add_source_piece, 
                         icon='add').classes('bg-green-600 hover:bg-green-500 text-white mb-4')
                
                # Dialog buttons
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Add Phase', on_click=lambda: self.add_phase(
                        phase_name_input.value, phase_type_select.value, source_pieces, dialog
                    )).classes('bg-purple-800 hover:bg-purple-700 text-white')
        
        dialog.open()
    
    def add_phase(self, phase_name: str, phase_type: str, source_pieces: List[Dict], dialog):
        """Add a new phase to the experiment"""
        if not phase_name:
            ui.notify('Please enter a phase name', type='warning')
            return
        
        print(f"DEBUG: Adding phase '{phase_name}' with {len(source_pieces)} source pieces")
        
        new_phase = {
            'phaseName': phase_name,
            'phaseType': phase_type,
            'sourcePieces': source_pieces.copy()
        }
        
        self.experiment_data['Phases'].append(new_phase)
        print(f"DEBUG: Total phases now: {len(self.experiment_data['Phases'])}")
        
        # Force refresh the phases display
        if hasattr(self, 'phases_container'):
            self.render_phases()
        else:
            print("DEBUG: phases_container not found")
        
        ui.notify(f'Added phase: {phase_name}', type='positive')
        dialog.close()
    
    def edit_phase(self, index: int):
        """Edit an existing phase"""
        if index >= len(self.experiment_data['Phases']):
            return
        
        # Implementation for editing phases - similar to add_phase_dialog but with existing data
        ui.notify('Phase editing coming soon!', type='info')
    
    def delete_phase(self, index: int):
        """Delete a phase"""
        if index >= len(self.experiment_data['Phases']):
            return
        
        phase_name = self.experiment_data['Phases'][index]['phaseName']
        self.experiment_data['Phases'].pop(index)
        self.render_phases()
        
        ui.notify(f'Deleted phase: {phase_name}', type='positive')
    
    def edit_source_piece(self, phase_index: int, source_index: int):
        """Edit a source piece within a phase"""
        ui.notify('Source piece editing coming soon!', type='info')
    
    def load_from_json(self, data: Dict[str, Any]) -> bool:
        """Load experiment data from JSON"""
        try:
            self.experiment_data = data.copy()
            
            # Update UI elements
            if hasattr(self, 'exp_name_input'):
                self.exp_name_input.value = data.get('experimentName', '')
            if hasattr(self, 'exp_type_select'):
                self.exp_type_select.value = data.get('experimentType', 'classification')
            if hasattr(self, 'batch_size_input'):
                self.batch_size_input.value = data.get('batchSize', 50)
            if hasattr(self, 'csv_path_input'):
                self.csv_path_input.value = data.get('csvFilePath', '')
            if hasattr(self, 'num_features_input'):
                self.num_features_input.value = data.get('numOfFeatures', '')
            if hasattr(self, 'num_labels_input'):
                self.num_labels_input.value = data.get('numOfLabels', '')
            if hasattr(self, 'headers_input'):
                self.headers_input.value = data.get('headersNames', '')
            
            # Re-render phases
            self.render_phases()
            
            return True
        except Exception as e:
            print(f"Error loading experiment data: {e}")
            return False
    
    def export_experiment(self):
        """Export experiment configuration"""
        try:
            # Update experiment data from UI
            self.experiment_data['experimentName'] = self.exp_name_input.value
            self.experiment_data['experimentType'] = self.exp_type_select.value
            self.experiment_data['batchSize'] = int(self.batch_size_input.value)
            self.experiment_data['csvFilePath'] = self.csv_path_input.value
            self.experiment_data['numOfFeatures'] = self.num_features_input.value
            self.experiment_data['numOfLabels'] = self.num_labels_input.value
            self.experiment_data['headersNames'] = self.headers_input.value
            
            # Generate JSON
            json_str = json.dumps(self.experiment_data, indent=2)
            
            # Trigger download
            ui.download(json_str.encode(), filename=f'exp_{self.experiment_data["experimentName"] or "experiment"}.json')
            ui.notify('Experiment configuration exported!', type='positive')
            
        except Exception as e:
            ui.notify(f'Export failed: {str(e)}', type='negative')
    
    def get_experiment_data(self) -> Dict[str, Any]:
        """Get current experiment data"""
        return self.experiment_data.copy()
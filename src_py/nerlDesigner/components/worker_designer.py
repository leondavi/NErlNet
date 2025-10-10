"""
Worker Designer Component - UI for designing neural network workers
"""

from typing import Dict, Any, List
import json
from nicegui import ui
from models.worker_model import WorkerModel, ModelType, LayerType, ActivationFunction, LossMethod, OptimizerType, InfraType, DistributedSystemType

class WorkerDesigner:
    """UI component for designing neural network workers"""
    
    def __init__(self, worker_model: WorkerModel):
        self.model = worker_model
        self.ui_elements = {}
        
    def create_ui(self):
        """Create the comprehensive worker designer UI"""
        # Initialize data structures
        self.current_config = {
            'modelType': '0',
            'modelArgs': '',
            'layersSizes': '',
            'layerTypesList': '',
            'layers_functions': '',
            'lossMethod': '2',
            'lossArgs': '',
            'lr': '0.001',
            'epochs': '100',
            'optimizer': '2',
            'optimizerArgs': 'none',
            'infraType': '0',
            'distributedSystemType': '0',
            'distributedSystemArgs': 'none',
            'distributedSystemToken': 'none'
        }
        
        # Visual network layers
        self.network_layers = []
        self.visual_layers = []
        self.selected_layer = None
        
        with ui.splitter(value=30).classes('w-full h-full') as splitter:
            # Left panel - Configuration
            with splitter.before:
                with ui.column().classes('w-full h-full p-4 bg-red-50'):
                    # Header with quick actions
                    with ui.row().classes('w-full items-center mb-4 p-3 bg-red-900 rounded-lg'):
                        ui.icon('psychology', size='md').classes('text-white')
                        ui.label('Neural Network Designer').classes('text-h6 font-bold text-white')
                        ui.space()
                        ui.button('Load Config', on_click=self.load_configuration).props('flat dense').classes('bg-orange-800 hover:bg-orange-700 text-white')
                    
                    # Scrollable configuration area
                    with ui.scroll_area().classes('w-full h-full'):
                        # Model Configuration
                        with ui.expansion('Model Configuration', icon='settings', value=True).classes('w-full mb-2 bg-red-100 border border-red-300'):
                            with ui.column().classes('p-4 gap-4'):
                                # Model Type
                                with ui.row().classes('w-full items-center gap-2'):
                                    ui.label('Model Type:').classes('font-bold')
                                    ui.button('Help', on_click=self.show_model_type_help, icon='help').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                
                                self.model_type_select = ui.select(
                                    self.get_model_type_options(),
                                    label='Select Model Type',
                                    value='Neural Network',
                                    on_change=self.on_model_type_change
                                ).classes('w-full')
                                
                                # Model Arguments
                                with ui.row().classes('w-full items-end gap-2'):
                                    self.model_args_input = ui.input(
                                        label='Model Arguments',
                                        placeholder='Optional model arguments',
                                        on_change=self.on_model_args_change
                                    ).classes('flex-1')
                                    self.model_args_count = ui.label('(0)').classes('text-sm text-gray-600')
                        
                        # Layer Architecture - The main section
                        with ui.expansion('Layer Architecture', icon='account_tree', value=True).classes('w-full mb-2 bg-red-100 border border-red-300'):
                            with ui.column().classes('p-4 gap-4'):
                                ui.label('Design your neural network architecture:').classes('text-subtitle1 mb-2')
                                
                                # Layer Sizes Configuration
                                with ui.card().classes('w-full mb-4'):
                                    with ui.card_section():
                                        ui.label('Layer Sizes').classes('text-h6 font-bold mb-3')
                                        ui.label('Enter layer sizes (supports CNN format like 28x28x1k5x5x1x8p0s1t1)').classes('text-sm text-gray-600 mb-2')
                                        
                                        with ui.row().classes('w-full items-end gap-2'):
                                            self.layers_sizes_input = ui.input(
                                                label='Layers Sizes',
                                                placeholder='e.g., 28x28x1k5x5x1x8p0s1t1,28x28x8k2x2p0s2,256,128,10',
                                                on_change=self.on_layers_sizes_change
                                            ).classes('flex-1')
                                            self.layers_count = ui.label('(0)').classes('text-sm text-gray-600')
                                            ui.button('Help', on_click=self.show_layers_help, icon='help').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                
                                # Layer Types Configuration  
                                with ui.card().classes('w-full mb-4'):
                                    with ui.card_section():
                                        ui.label('Layer Types').classes('text-h6 font-bold mb-3')
                                        
                                        with ui.row().classes('w-full items-end gap-2'):
                                            self.layer_type_select = ui.select(
                                                self.get_layer_type_options_for_display(),
                                                label='Select Layer Type'
                                            ).classes('flex-1')
                                            ui.button('Add', on_click=self.add_layer_type, icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                                            ui.button('Help', on_click=self.show_layer_type_help, icon='help').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                            ui.button('Clear', on_click=self.clear_layer_types, icon='clear').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                        
                                        with ui.row().classes('w-full items-end gap-2 mt-2'):
                                            self.layer_types_input = ui.input(
                                                label='Layer Types List',
                                                placeholder='Layer type codes (0,2,4,3,3,3,5)',
                                                on_change=self.on_layer_types_change
                                            ).classes('flex-1')
                                            self.layer_types_count = ui.label('(0)').classes('text-sm text-gray-600')
                                
                                # Layer Functions Configuration
                                with ui.card().classes('w-full mb-4'):
                                    with ui.card_section():
                                        ui.label('Layer Functions').classes('text-h6 font-bold mb-3')
                                        
                                        with ui.row().classes('w-full items-end gap-2'):
                                            ui.button('Select Layer Functions', 
                                                     on_click=self.show_layer_method_selection, 
                                                     icon='functions').classes('bg-orange-700 hover:bg-orange-600 text-white')
                                            ui.button('Help', on_click=self.show_activation_help, icon='help').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                            ui.button('Clear', on_click=self.clear_layer_functions, icon='clear').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                        
                                        with ui.row().classes('w-full items-end gap-2 mt-2'):
                                            self.layer_functions_input = ui.input(
                                                label='Layer Functions List', 
                                                placeholder='Function codes (6,2,6,1,6,6,3,4)',
                                                on_change=self.on_layer_functions_change
                                            ).classes('flex-1')
                                            self.layer_functions_count = ui.label('(0)').classes('text-sm text-gray-600')
                        
                        # Network Architecture - Most Important Section
                        with ui.expansion('Network Architecture', icon='account_tree', value=True).classes('w-full mb-2 bg-red-100 border border-red-300'):
                            with ui.column().classes('p-4 gap-4'):
                                ui.label('Design your neural network layers:').classes('text-subtitle1 mb-2')
                                
                                # Quick presets
                                with ui.row().classes('gap-2 mb-4'):
                                    ui.button('Simple (3 layers)', 
                                             on_click=lambda: self.apply_preset([784, 128, 10])).props('dense').classes('bg-orange-700 hover:bg-orange-600 text-white')
                                    ui.button('Deep (5 layers)', 
                                             on_click=lambda: self.apply_preset([784, 256, 128, 64, 10])).props('dense').classes('bg-orange-700 hover:bg-orange-600 text-white')
                                    ui.button('Complex (7 layers)', 
                                             on_click=lambda: self.apply_preset([784, 512, 256, 128, 64, 32, 10])).props('dense').classes('bg-orange-700 hover:bg-orange-600 text-white')
                                
                                # Layer configuration
                                self.layer_config_container = ui.column().classes('w-full gap-2')
                                self.update_layer_designer()
                                
                                # Add layer button
                                ui.button('Add Layer', 
                                         on_click=self.add_layer,
                                         icon='add').props('unelevated').classes('w-full bg-orange-800 hover:bg-orange-700 text-white')
                        
                        # Training Parameters
                        with ui.expansion('Training Parameters', icon='tune').classes('w-full mb-2 bg-red-100 border border-red-300'):
                            with ui.column().classes('p-4 gap-4'):
                                # Learning Rate and Epochs
                                with ui.row().classes('w-full gap-4'):
                                    self.learning_rate_input = ui.number(
                                        label='Learning Rate',
                                        value=0.001,
                                        min=0.00001,
                                        max=1.0,
                                        step=0.00001,
                                        format='%.6f',
                                        on_change=self.on_learning_rate_change
                                    ).classes('flex-1')
                                    
                                    self.epochs_input = ui.number(
                                        label='Epochs',
                                        value=100,
                                        min=1,
                                        max=10000,
                                        step=1,
                                        on_change=self.on_epochs_change
                                    ).classes('flex-1')
                                
                                # Optimizer Configuration
                                with ui.row().classes('w-full items-end gap-2'):
                                    self.optimizer_select = ui.select(
                                        self.get_optimizer_options(),
                                        label='Optimizer Type',
                                        value='Stochastic Gradient Descent',
                                        on_change=self.on_optimizer_change
                                    ).classes('flex-1')
                                    
                                    self.optimizer_args_input = ui.input(
                                        label='Optimizer Args',
                                        placeholder='none',
                                        value='none',
                                        on_change=self.on_optimizer_args_change
                                    ).classes('flex-1')
                                
                                # Loss Method Configuration
                                with ui.row().classes('w-full items-end gap-2'):
                                    self.loss_method_select = ui.select(
                                        self.get_loss_method_options(),
                                        label='Loss Method',
                                        value='Mean Squared Error',
                                        on_change=self.on_loss_method_change
                                    ).classes('flex-1')
                                    
                                    self.loss_args_input = ui.input(
                                        label='Loss Args',
                                        placeholder='none',
                                        value='',
                                        on_change=self.on_loss_args_change
                                    ).classes('flex-1')
                        
                        # Infrastructure Configuration
                        with ui.expansion('Infrastructure Configuration', icon='engineering').classes('w-full mb-2 bg-red-100 border border-red-300'):
                            with ui.column().classes('p-4 gap-4'):
                                # Infrastructure Type
                                with ui.row().classes('w-full items-center gap-2'):
                                    ui.label('Infrastructure Type:').classes('font-bold')
                                    ui.button('Help', on_click=self.show_infra_help, icon='help').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                
                                self.infra_type_select = ui.select(
                                    self.get_infra_type_options(),
                                    label='Select Infrastructure',
                                    value='OpenNN',
                                    on_change=self.on_infra_type_change
                                ).classes('w-full')
                                
                                # Distributed System Configuration
                                with ui.row().classes('w-full items-center gap-2'):
                                    ui.label('Distributed System:').classes('font-bold')
                                    ui.button('Help', on_click=self.show_distributed_help, icon='help').classes('bg-orange-600 hover:bg-orange-500 text-white')
                                
                                self.distributed_type_select = ui.select(
                                    self.get_distributed_system_options(),
                                    label='Select Distributed System Type',
                                    value='None',
                                    on_change=self.on_distributed_type_change
                                ).classes('w-full')
                                
                                # Distributed System Token and Args
                                with ui.row().classes('w-full gap-2'):
                                    self.distributed_token_input = ui.input(
                                        label='Distributed System Token',
                                        placeholder='none',
                                        value='none',
                                        on_change=self.on_distributed_token_change
                                    ).classes('flex-1')
                                    
                                    ui.button('Auto Generate', 
                                             on_click=self.auto_generate_token, 
                                             icon='refresh').classes('bg-orange-700 hover:bg-orange-600 text-white')
                                
                                self.distributed_args_input = ui.input(
                                    label='Distributed System Args',
                                    placeholder='none',
                                    value='none',
                                    on_change=self.on_distributed_args_change
                                ).classes('w-full')
                        
                        # File Operations
                        with ui.expansion('File Operations', icon='folder').classes('w-full mb-2 bg-red-100 border border-red-300'):
                            with ui.column().classes('p-4 gap-4'):
                                # Export configuration
                                with ui.row().classes('w-full gap-2'):
                                    self.filename_input = ui.input(
                                        label='Export Filename',
                                        placeholder='worker_config.json',
                                        value='worker_config.json'
                                    ).classes('flex-1')
                                    
                                    self.with_documentation_checkbox = ui.checkbox(
                                        'With Documentation', 
                                        value=True
                                    ).classes('mt-6')
                                
                                with ui.row().classes('w-full gap-2'):
                                    ui.button('Export Worker Config', 
                                             on_click=self.export_worker_config, 
                                             icon='download').classes('bg-orange-800 hover:bg-orange-700 text-white flex-1')
                                    ui.button('Load From JSON', 
                                             on_click=self.load_worker_config, 
                                             icon='upload').classes('bg-orange-700 hover:bg-orange-600 text-white flex-1')
                                    ui.button('Reset Config', 
                                             on_click=self.reset_config, 
                                             icon='refresh').classes('bg-orange-600 hover:bg-orange-500 text-white flex-1')
            
            # Right panel - Interactive Network Designer
            with splitter.after:
                with ui.column().classes('w-full h-full p-4 bg-gray-50'):
                    # Header with tools
                    with ui.row().classes('w-full items-center mb-4 p-3 bg-gray-800 rounded-lg'):
                        ui.icon('account_tree', size='md').classes('text-white')
                        ui.label('Visual Network Designer').classes('text-h6 font-bold text-white')
                        ui.space()
                        with ui.button_group():
                            ui.button('Add Layer', on_click=self.show_add_layer_dialog, icon='add').classes('bg-orange-700 hover:bg-orange-600 text-white')
                            ui.button('Auto Layout', on_click=self.auto_layout_network, icon='auto_fix_high').classes('bg-orange-600 hover:bg-orange-500 text-white')
                            ui.button('Zoom Fit', on_click=self.zoom_to_fit, icon='zoom_out_map').classes('bg-orange-500 hover:bg-orange-400 text-white')
                    
                    # Interactive Network Canvas
                    with ui.card().classes('w-full flex-1 border border-gray-300'):
                        with ui.card_section().classes('p-0'):
                            # Canvas container with scroll
                            with ui.scroll_area().classes('w-full h-96'):
                                self.network_canvas = ui.column().classes('w-full min-h-96 p-4 bg-gray-900 relative')
                                self.render_network_diagram()
                    
                    # Quick Layer Tools
                    with ui.card().classes('w-full mt-4 border border-gray-300'):
                        with ui.card_section():
                            ui.label('Quick Add Layers').classes('text-subtitle1 font-bold mb-2')
                            with ui.grid(columns=4).classes('gap-2 w-full'):
                                # Common layer types as quick buttons
                                ui.button('Input', on_click=lambda: self.quick_add_layer('input')).classes('bg-blue-600 hover:bg-blue-500 text-white text-xs')
                                ui.button('Dense', on_click=lambda: self.quick_add_layer('dense')).classes('bg-green-600 hover:bg-green-500 text-white text-xs')
                                ui.button('Conv2D', on_click=lambda: self.quick_add_layer('conv2d')).classes('bg-purple-600 hover:bg-purple-500 text-white text-xs')
                                ui.button('MaxPool', on_click=lambda: self.quick_add_layer('maxpool')).classes('bg-red-600 hover:bg-red-500 text-white text-xs')
                                ui.button('Dropout', on_click=lambda: self.quick_add_layer('dropout')).classes('bg-yellow-600 hover:bg-yellow-500 text-white text-xs')
                                ui.button('BatchNorm', on_click=lambda: self.quick_add_layer('batchnorm')).classes('bg-indigo-600 hover:bg-indigo-500 text-white text-xs')
                                ui.button('Flatten', on_click=lambda: self.quick_add_layer('flatten')).classes('bg-pink-600 hover:bg-pink-500 text-white text-xs')
                                ui.button('Output', on_click=lambda: self.quick_add_layer('output')).classes('bg-gray-600 hover:bg-gray-500 text-white text-xs')
                    
                    # Layer Properties Panel
                    with ui.card().classes('w-full mt-4 border border-gray-300'):
                        with ui.card_section():
                            ui.label('Layer Properties').classes('text-subtitle1 font-bold mb-2')
                            self.layer_properties_container = ui.column().classes('w-full')
                            with self.layer_properties_container:
                                ui.label('Select a layer to edit its properties').classes('text-gray-600 text-center py-4')
    
    def update_layer_config(self):
        """Update the layer configuration UI based on current layer sizes"""
        self.layer_config_container.clear()
        
        with self.layer_config_container:
            for i, size in enumerate(self.model.layer_sizes):
                with ui.card().classes('w-full mb-2'):
                    with ui.card_section():
                        ui.label(f'Layer {i + 1} ({size} neurons)').classes('text-subtitle1')
                        
                        with ui.row().classes('gap-4 w-full'):
                            # Layer Type
                            layer_type_select = ui.select(
                                {lt.value: lt.name.title() for lt in LayerType},
                                label='Layer Type',
                                value=self.model.layer_types[i].value if i < len(self.model.layer_types) else LayerType.PERCEPTRON.value,
                                on_change=lambda e, idx=i: self.on_layer_type_change(idx, e)
                            )
                            
                            # Activation Function
                            activation_select = ui.select(
                                {af.value: af.name.title() for af in ActivationFunction},
                                label='Activation Function',
                                value=self.model.layer_functions[i].value if i < len(self.model.layer_functions) else ActivationFunction.RELU.value,
                                on_change=lambda e, idx=i: self.on_activation_change(idx, e)
                            )
    
    def on_model_type_change(self, e):
        """Handle model type change"""
        self.model.model_type = ModelType(e.value)
        ui.notify(f'Model type changed to {self.model.model_type.name}', type='info')
    
    def on_infra_type_change(self, e):
        """Handle infrastructure type change"""
        self.model.infra_type = InfraType(e.value)
        ui.notify(f'Infrastructure changed to {self.model.infra_type.name}', type='info')
    
    def on_layer_sizes_change(self, e):
        """Handle layer sizes change"""
        try:
            sizes = [int(x.strip()) for x in e.value.split(',') if x.strip()]
            if all(s > 0 for s in sizes):
                self.model.layer_sizes = sizes
                # Reset layer types and functions to match new size
                self.model.layer_types = [LayerType.SCALING] + [LayerType.PROBABILISTIC] * (len(sizes) - 2) + [LayerType.SCALING]
                self.model.layer_functions = [ActivationFunction.LINEAR] + [ActivationFunction.RELU] * (len(sizes) - 2) + [ActivationFunction.LINEAR]
                self.update_layer_config()
                ui.notify('Layer sizes updated', type='positive')
            else:
                ui.notify('All layer sizes must be positive integers', type='negative')
        except ValueError:
            ui.notify('Invalid layer sizes format. Use comma-separated integers.', type='negative')
    
    def on_layer_type_change(self, layer_idx: int, e):
        """Handle layer type change for specific layer"""
        if layer_idx < len(self.model.layer_types):
            self.model.layer_types[layer_idx] = LayerType(e.value)
            ui.notify(f'Layer {layer_idx + 1} type updated', type='info')
    
    def on_activation_change(self, layer_idx: int, e):
        """Handle activation function change for specific layer"""
        if layer_idx < len(self.model.layer_functions):
            self.model.layer_functions[layer_idx] = ActivationFunction(e.value)
            ui.notify(f'Layer {layer_idx + 1} activation updated', type='info')
    
    def on_loss_method_change(self, e):
        """Handle loss method change"""
        self.model.loss_method = LossMethod(e.value)
        ui.notify(f'Loss method changed to {self.model.loss_method.name}', type='info')
    
    def on_learning_rate_change(self, e):
        """Handle learning rate change"""
        self.model.learning_rate = e.value
    
    def on_epochs_change(self, e):
        """Handle epochs change"""
        self.model.epochs = int(e.value)
    
    def on_optimizer_change(self, e):
        """Handle optimizer change"""
        self.model.optimizer = OptimizerType(e.value)
        ui.notify(f'Optimizer changed to {self.model.optimizer.name}', type='info')
    
    def on_loss_args_change(self, e):
        """Handle loss arguments change"""
        self.model.loss_args = e.value
    
    def on_optimizer_args_change(self, e):
        """Handle optimizer arguments change"""
        self.model.optimizer_args = e.value
    
    def on_dist_type_change(self, e):
        """Handle distributed system type change"""
        self.model.distributed_system_type = DistributedSystemType(e.value)
        ui.notify(f'Distributed system type changed to {self.model.distributed_system_type.name}', type='info')
    
    def on_dist_args_change(self, e):
        """Handle distributed system arguments change"""
        self.model.distributed_system_args = e.value
    
    def reset_model(self):
        """Reset the model to default values"""
        self.model.reset()
        # Update all UI elements
        self.ui_elements['model_type'].value = self.model.model_type.value
        self.ui_elements['infra_type'].value = self.model.infra_type.value
        self.ui_elements['layer_sizes'].value = ','.join(map(str, self.model.layer_sizes))
        self.ui_elements['loss_method'].value = self.model.loss_method.value
        self.ui_elements['learning_rate'].value = self.model.learning_rate
        self.ui_elements['epochs'].value = self.model.epochs
        self.ui_elements['optimizer'].value = self.model.optimizer.value
        self.ui_elements['loss_args'].value = self.model.loss_args
        self.ui_elements['optimizer_args'].value = self.model.optimizer_args
        self.ui_elements['dist_type'].value = self.model.distributed_system_type.value
        self.ui_elements['dist_args'].value = self.model.distributed_system_args
        self.update_layer_config()
        ui.notify('Model reset to defaults', type='info')
    
    def validate_config(self):
        """Validate the current configuration"""
        try:
            # Try to create the JSON to validate
            json_data = self.model.to_dict()
            ui.notify('Configuration is valid!', type='positive')
        except Exception as e:
            ui.notify(f'Configuration error: {str(e)}', type='negative')
    
    def preview_json(self):
        """Show a preview of the generated JSON"""
        try:
            import json
            json_data = self.model.to_dict()
            formatted_json = json.dumps(json_data, indent=2)
            
            with ui.dialog() as dialog, ui.card().classes('w-96 max-w-full'):
                ui.label('Worker Configuration JSON').classes('text-h6')
                with ui.scroll_area().classes('w-full h-96'):
                    ui.code(formatted_json, language='json').classes('w-full')
                ui.button('Close', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-700 text-white')
            dialog.open()
        except Exception as e:
            ui.notify(f'Error generating JSON: {str(e)}', type='negative')
    
    # New methods for improved UI
    def get_model_type_options(self) -> dict:
        """Get model type options"""
        try:
            return {mt.value: mt.name.replace('_', ' ').title() for mt in ModelType}
        except:
            return {'neural_network': 'Neural Network', 'cnn': 'CNN', 'rnn': 'RNN'}
    
    def get_infra_type_options(self) -> dict:
        """Get infrastructure type options"""
        try:
            return {it.value: it.name.title() for it in InfraType}
        except:
            return {'sequential': 'Sequential', 'parallel': 'Parallel'}
    
    def get_loss_method_options(self) -> dict:
        """Get loss method options"""
        try:
            return {lm.value: lm.name.replace('_', ' ').title() for lm in LossMethod}
        except:
            return {'mean_squared_error': 'Mean Squared Error', 'binary_crossentropy': 'Binary Crossentropy'}
    
    def get_optimizer_options(self) -> dict:
        """Get optimizer options"""
        try:
            return {opt.value: opt.name.title() for opt in OptimizerType}
        except:
            return {'adam': 'Adam', 'sgd': 'SGD', 'rmsprop': 'RMSprop'}
    
    def get_distributed_options(self) -> dict:
        """Get distributed system options"""
        try:
            return {dst.value: dst.name.replace('_', ' ').title() for dst in DistributedSystemType}
        except:
            return {'federated': 'Federated Learning', 'centralized': 'Centralized'}
    
    def show_quick_setup(self):
        """Show quick setup dialog"""
        with ui.dialog() as dialog, ui.card():
            ui.label('🎯 Quick Neural Network Setup').classes('text-h6 mb-4')
            
            with ui.column().classes('gap-4'):
                ui.label('Choose a common configuration:')
                
                ui.button('Image Classification (MNIST)', 
                         on_click=lambda: self.apply_quick_config('mnist', dialog)).classes('w-full bg-orange-800 hover:bg-orange-700 text-white mb-2')
                ui.button('Regression Problem', 
                         on_click=lambda: self.apply_quick_config('regression', dialog)).classes('w-full bg-orange-800 hover:bg-orange-700 text-white mb-2')
                ui.button('Text Classification', 
                         on_click=lambda: self.apply_quick_config('text', dialog)).classes('w-full bg-orange-800 hover:bg-orange-700 text-white mb-2')
                ui.button('Time Series Prediction', 
                         on_click=lambda: self.apply_quick_config('timeseries', dialog)).classes('w-full bg-orange-800 hover:bg-orange-700 text-white mb-2')
                
                ui.button('Cancel', on_click=dialog.close).props('flat').classes('bg-gray-600 hover:bg-gray-700 text-white')
        
        dialog.open()
    
    def apply_quick_config(self, config_type: str, dialog):
        """Apply quick configuration preset"""
        configs = {
            'mnist': {'layers': [784, 128, 64, 10], 'loss': 'categorical_crossentropy', 'optimizer': 'adam'},
            'regression': {'layers': [10, 32, 16, 1], 'loss': 'mean_squared_error', 'optimizer': 'adam'},
            'text': {'layers': [1000, 256, 128, 2], 'loss': 'binary_crossentropy', 'optimizer': 'adam'},
            'timeseries': {'layers': [50, 64, 32, 1], 'loss': 'mean_squared_error', 'optimizer': 'rmsprop'}
        }
        
        if config_type in configs:
            config = configs[config_type]
            self.apply_preset(config['layers'])
            # Apply other settings if UI elements exist
            if 'loss_method' in self.ui_elements:
                self.ui_elements['loss_method'].value = config['loss']
            if 'optimizer' in self.ui_elements:
                self.ui_elements['optimizer'].value = config['optimizer']
            
            ui.notify(f'Applied {config_type} configuration!', type='positive')
        
        dialog.close()
    
    def apply_preset(self, layers: list):
        """Apply layer preset"""
        self.current_layers = layers
        self.update_layer_designer()
        ui.notify(f'Applied preset: {layers}', type='positive')
    
    def add_layer(self):
        """Add a new layer"""
        if not hasattr(self, 'current_layers'):
            self.current_layers = [784, 128, 10]  # Default
        
        # Add layer with reasonable size
        new_size = max(1, self.current_layers[-1] // 2) if self.current_layers else 32
        self.current_layers.append(new_size)
        self.update_layer_designer()
        ui.notify('Layer added!', type='positive')
    
    def update_layer_designer(self):
        """Update the layer designer UI"""
        if not hasattr(self, 'current_layers'):
            self.current_layers = [784, 128, 10]
        
        # Clear existing content
        self.layer_config_container.clear()
        
        # Create layer controls
        with self.layer_config_container:
            for i, layer_size in enumerate(self.current_layers):
                with ui.card().classes('w-full mb-2'):
                    with ui.card_section():
                        with ui.row().classes('w-full items-center gap-2'):
                            # Layer info
                            if i == 0:
                                ui.icon('input').classes('text-blue')
                                ui.label(f'Input Layer').classes('font-bold')
                            elif i == len(self.current_layers) - 1:
                                ui.icon('output').classes('text-green')
                                ui.label(f'Output Layer').classes('font-bold')
                            else:
                                ui.icon('layers').classes('text-purple')
                                ui.label(f'Hidden Layer {i}').classes('font-bold')
                            
                            ui.space()
                            
                            # Size input
                            size_input = ui.number(
                                value=layer_size,
                                min=1,
                                max=10000,
                                on_change=lambda e, idx=i: self.update_layer_size(idx, int(e.value) if e.value else 1)
                            ).classes('w-24')
                            
                            # Remove button (not for input/output)
                            if len(self.current_layers) > 2 and 0 < i < len(self.current_layers) - 1:
                                ui.button(icon='delete', 
                                         on_click=lambda idx=i: self.remove_layer(idx)).props('flat dense color=negative')
    
    def update_layer_size(self, index: int, new_size: int):
        """Update size of a specific layer"""
        if 0 <= index < len(self.current_layers):
            self.current_layers[index] = new_size
            self.update_network_visualization()
    
    def remove_layer(self, index: int):
        """Remove a layer"""
        if 0 < index < len(self.current_layers) - 1:  # Don't remove input/output
            self.current_layers.pop(index)
            self.update_layer_designer()
            ui.notify('Layer removed!', type='positive')
    
    def update_network_visualization(self):
        """Update the network visualization"""
        if not hasattr(self, 'network_viz_container'):
            return
        
        self.network_viz_container.clear()
        
        with self.network_viz_container:
            if not hasattr(self, 'current_layers'):
                ui.label('No layers configured').classes('text-grey text-center')
                return
            
            # Simple ASCII-style visualization
            ui.label('Network Structure:').classes('text-subtitle2 mb-2')
            
            for i, layer_size in enumerate(self.current_layers):
                with ui.row().classes('items-center gap-2 mb-1'):
                    if i == 0:
                        ui.icon('input', color='blue')
                        ui.label(f'Input: {layer_size} nodes')
                    elif i == len(self.current_layers) - 1:
                        ui.icon('output', color='green') 
                        ui.label(f'Output: {layer_size} nodes')
                    else:
                        ui.icon('circle', color='purple')
                        ui.label(f'Hidden: {layer_size} nodes')
                
                # Connection arrow (except for last layer)
                if i < len(self.current_layers) - 1:
                    ui.icon('arrow_downward', color='grey').classes('mx-auto')
    
    def update_summary(self):
        """Update configuration summary"""
        if not hasattr(self, 'summary_container'):
            return
        
        self.summary_container.clear()
        
        with self.summary_container:
            # Layer summary
            if hasattr(self, 'current_layers'):
                total_params = sum(
                    self.current_layers[i] * self.current_layers[i+1] + self.current_layers[i+1]
                    for i in range(len(self.current_layers)-1)
                )
                
                ui.label(f'🔢 Total Layers: {len(self.current_layers)}').classes('text-sm')
                ui.label(f'📊 Parameters: ~{total_params:,}').classes('text-sm')
                
                # Model info
                model_type = 'Neural Network'
                if 'model_type' in self.ui_elements and hasattr(self.ui_elements['model_type'], 'value'):
                    model_type = self.ui_elements['model_type'].value
                ui.label(f'🏗️ Type: {model_type}').classes('text-sm')
                
                loss_method = 'MSE'
                if 'loss_method' in self.ui_elements and hasattr(self.ui_elements['loss_method'], 'value'):
                    loss_method = self.ui_elements['loss_method'].value
                ui.label(f'Loss: {loss_method}').classes('text-sm')
    
    def on_layer_sizes_change(self, e):
        """Handle layer sizes input change"""
        if hasattr(e, 'value') and e.value:
            try:
                # Parse comma-separated sizes
                sizes = [int(x.strip()) for x in e.value.split(',') if x.strip()]
                if sizes:
                    self.current_layers = sizes
                    self.update_network_visualization()
                    self.update_summary()
            except ValueError:
                pass  # Invalid input, ignore
    
    def show_layer_help(self):
        """Show help for layer sizes"""
        ui.notify('Enter the number of neurons in each layer, separated by commas. Example: 784,256,128,64,10', 
                 type='info', timeout=8000)
    
    def get_layer_type_options(self):
        """Get available layer types"""
        return {
            'Dense/Fully Connected': '0',
            'Convolutional': '1', 
            'Pooling': '2',
            'Perceptron': '3',
            'Recurrent': '4',
            'LSTM': '5',
            'GRU': '6'
        }
    
    def show_layer_method_selection(self):
        """Show layer method selection dialog"""
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 800px; max-width: 90vw'):
                ui.label('Select Layer Function').classes('text-h6 mb-4')
                
                with ui.grid(columns=3).classes('gap-4 w-full'):
                    # Activation Functions
                    with ui.column():
                        ui.label('Activation Functions').classes('font-bold mb-2')
                        activation_list = ui.select(
                            self.get_activation_functions(),
                            label='Activation'
                        ).classes('w-full')
                    
                    # Pooling Methods  
                    with ui.column():
                        ui.label('Pooling Methods').classes('font-bold mb-2')
                        pooling_list = ui.select(
                            self.get_pooling_methods(),
                            label='Pooling'
                        ).classes('w-full')
                    
                    # Other Methods
                    with ui.column():
                        ui.label('Other Methods').classes('font-bold mb-2')
                        other_list = ui.select(
                            self.get_other_methods(),
                            label='Other'
                        ).classes('w-full')
                
                with ui.row().classes('w-full justify-end gap-2 mt-4'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Add Selected', on_click=lambda: self.add_selected_function(
                        activation_list, pooling_list, other_list, dialog
                    )).classes('bg-orange-800 hover:bg-orange-700 text-white')
        
        dialog.open()
    
    def get_activation_functions(self):
        """Get activation function options"""
        return {
            'Linear': '0', 'Threshold': '1', 'Sigmoid': '2', 'Hard Sigmoid': '3',
            'Hyperbolic Tangent': '4', 'ReLU': '5', 'Leaky ReLU': '6', 'ELU': '7',
            'Softmax': '8', 'Softplus': '9', 'Softsign': '10', 'SELU': '11'
        }
    
    def get_pooling_methods(self):
        """Get pooling method options"""
        return {
            'Max Pooling': '20', 'Average Pooling': '21', 'Global Max Pooling': '22',
            'Global Average Pooling': '23'
        }
    
    def get_other_methods(self):
        """Get other method options"""
        return {
            'Batch Normalization': '40', 'Dropout': '41', 'Flatten': '50',
            'Min-Max Scaler': '30', 'Standard Scaler': '31'
        }
    
    def add_selected_function(self, activation_list, pooling_list, other_list, dialog):
        """Add selected function to the layer functions"""
        selected_code = None
        selected_name = None
        
        if hasattr(activation_list, 'value') and activation_list.value:
            selected_code = self.get_activation_functions()[activation_list.value]
            selected_name = activation_list.value
        elif hasattr(pooling_list, 'value') and pooling_list.value:
            selected_code = self.get_pooling_methods()[pooling_list.value]
            selected_name = pooling_list.value
        elif hasattr(other_list, 'value') and other_list.value:
            selected_code = self.get_other_methods()[other_list.value]
            selected_name = other_list.value
        
        if selected_code:
            current_functions = self.layer_functions_input.value or ''
            if current_functions:
                current_functions += ',' + selected_code
            else:
                current_functions = selected_code
            
            self.layer_functions_input.value = current_functions
            self.current_config['layers_functions'] = current_functions
            self.update_layer_function_count()
            ui.notify(f'Added {selected_name} (code: {selected_code})', type='positive')
        else:
            ui.notify('Please select a function first', type='warning')
        
        dialog.close()

    # All the configuration change handlers
    def on_model_type_change(self, e):
        """Handle model type change"""
        if hasattr(e, 'value') and e.value:
            # Convert display name to code
            self.current_config['modelType'] = self.get_model_type_options()[e.value]
    
    def on_model_args_change(self, e):
        """Handle model args change"""
        if hasattr(e, 'value'):
            self.current_config['modelArgs'] = e.value
            count = len(e.value.split(',')) if e.value else 0
            self.model_args_count.text = f'({count})'
    
    def on_layers_sizes_change(self, e):
        """Handle layers sizes change"""
        if hasattr(e, 'value'):
            self.current_config['layersSizes'] = e.value
            count = len(e.value.split(',')) if e.value else 0
            self.layers_count.text = f'({count})'
    
    def on_layer_types_change(self, e):
        """Handle layer types change"""
        if hasattr(e, 'value'):
            self.current_config['layerTypesList'] = e.value
            self.update_layer_type_count()
    
    def on_layer_functions_change(self, e):
        """Handle layer functions change"""
        if hasattr(e, 'value'):
            self.current_config['layers_functions'] = e.value
            self.update_layer_function_count()
    
    def on_learning_rate_change(self, e):
        """Handle learning rate change"""
        if hasattr(e, 'value'):
            self.current_config['lr'] = str(e.value)
    
    def on_epochs_change(self, e):
        """Handle epochs change"""
        if hasattr(e, 'value'):
            self.current_config['epochs'] = str(e.value)
    
    def on_optimizer_change(self, e):
        """Handle optimizer change"""
        if hasattr(e, 'value') and e.value:
            # Convert display name to code
            self.current_config['optimizer'] = self.get_optimizer_options()[e.value]
    
    def on_optimizer_args_change(self, e):
        """Handle optimizer args change"""
        if hasattr(e, 'value'):
            self.current_config['optimizerArgs'] = e.value
    
    def on_loss_method_change(self, e):
        """Handle loss method change"""
        if hasattr(e, 'value') and e.value:
            # Convert display name to code
            self.current_config['lossMethod'] = self.get_loss_method_options()[e.value]
    
    def on_loss_args_change(self, e):
        """Handle loss args change"""
        if hasattr(e, 'value'):
            self.current_config['lossArgs'] = e.value
    
    def on_infra_type_change(self, e):
        """Handle infrastructure type change"""
        if hasattr(e, 'value') and e.value:
            # Convert display name to code
            self.current_config['infraType'] = self.get_infra_type_options()[e.value]
    
    def on_distributed_type_change(self, e):
        """Handle distributed type change"""
        if hasattr(e, 'value') and e.value:
            # Convert display name to code
            code = self.get_distributed_system_options()[e.value]
            self.current_config['distributedSystemType'] = code
            # Reset token if type is none
            if code == '0':
                self.distributed_token_input.value = 'none'
                self.current_config['distributedSystemToken'] = 'none'
    
    def on_distributed_token_change(self, e):
        """Handle distributed token change"""
        if hasattr(e, 'value'):
            self.current_config['distributedSystemToken'] = e.value
    
    def on_distributed_args_change(self, e):
        """Handle distributed args change"""
        if hasattr(e, 'value'):
            self.current_config['distributedSystemArgs'] = e.value

    # Helper methods for dropdowns
    def get_model_type_options(self):
        """Get model type options"""
        return {
            'Neural Network': '0',
            'Approximation': '1', 
            'Classification': '2',
            'Forecasting': '3',
            'Image Classification': '4',
            'Text Classification': '5',
            'Text Generation': '6',
            'Auto Association': '7',
            'Autoencoder': '8',
            'AE Classifier': '9'
        }
    
    def get_layer_type_options(self):
        """Get layer type options for internal use"""
        return {
            'Default': '0',
            'Scaling': '1',
            'Conv': '2',
            'Perceptron': '3',
            'Pooling': '4',
            'Probabilistic': '5',
            'LSTM': '6',
            'Recurrent': '7',
            'Unscaling': '8',
            'Flatten': '9',
            'Bounding': '10'
        }
    
    def get_layer_type_options_for_display(self):
        """Get layer type options with ID and name for display"""
        return [
            '0 - Default',
            '1 - Scaling', 
            '2 - Conv',
            '3 - Perceptron',
            '4 - Pooling',
            '5 - Probabilistic',
            '6 - LSTM',
            '7 - Recurrent',
            '8 - Unscaling',
            '9 - Flatten',
            '10 - Bounding'
        ]

    def extract_layer_type_id(self, display_value):
        """Extract the numeric ID from display format 'ID - Name'"""
        if display_value and ' - ' in display_value:
            return display_value.split(' - ')[0]
        return display_value

    def extract_layer_type_name(self, display_value):
        """Extract the name from display format 'ID - Name'"""
        if display_value and ' - ' in display_value:
            return display_value.split(' - ')[1]
        return display_value
    
    def get_optimizer_options(self):
        """Get optimizer options"""
        return {
            'Gradient Descent': '0',
            'Conjugate Gradient': '1',
            'Stochastic Gradient Descent': '2',
            'Quasi-Newton': '3',
            'Levenberg-Marquardt': '4',
            'ADAM': '5'
        }
    
    def get_loss_method_options(self):
        """Get loss method options"""
        return {
            'Sum Squared Error': '1',
            'Mean Squared Error': '2',
            'Normalized Squared Error': '3',
            'Minkowski Error': '4',
            'Weighted Squared Error': '5',
            'Cross Entropy Error': '6'
        }
    
    def get_infra_type_options(self):
        """Get infrastructure type options"""
        return {
            'OpenNN': '0',
            'Wolfram Engine': '1'
        }
    
    def get_distributed_system_options(self):
        """Get distributed system options"""
        return {
            'None': '0',
            'Fed Client Avg': '1',
            'Fed Server Avg': '2',
            'Fed Client Weighted Avg Classification': '3',
            'Fed Server Weighted Avg Classification': '4',
            'Fed Client AE': '5',
            'Fed Server AE': '6',
            'Tiles': '7'
        }

    # Layer management methods
    def add_layer_type(self):
        """Add selected layer type to the list"""
        if hasattr(self, 'layer_type_select') and hasattr(self.layer_type_select, 'value') and self.layer_type_select.value:
            current_types = self.layer_types_input.value or ''
            new_type = self.get_layer_type_options_for_display()[self.layer_type_select.value]
            
            if current_types:
                current_types += ',' + new_type
            else:
                current_types = new_type
                
            self.layer_types_input.value = current_types
            self.current_config['layerTypesList'] = current_types
            self.update_layer_type_count()
    
    def clear_layer_types(self):
        """Clear all layer types"""
        self.layer_types_input.value = ''
        self.current_config['layerTypesList'] = ''
        self.update_layer_type_count()
    
    def clear_layer_functions(self):
        """Clear all layer functions"""
        self.layer_functions_input.value = ''
        self.current_config['layers_functions'] = ''
        self.update_layer_function_count()
    
    def update_layer_type_count(self):
        """Update layer type count display"""
        count = len(self.layer_types_input.value.split(',')) if self.layer_types_input.value else 0
        self.layer_types_count.text = f'({count} types)'
    
    def update_layer_function_count(self):
        """Update layer function count display"""
        count = len(self.layer_functions_input.value.split(',')) if self.layer_functions_input.value else 0
        self.layer_functions_count.text = f'({count} functions)'
    
    def update_layer_sizes_count(self):
        """Update layer sizes count display"""
        if hasattr(self, 'layers_sizes_input') and self.layers_sizes_input.value:
            count = len(self.layers_sizes_input.value.split(',')) if self.layers_sizes_input.value else 0
            if hasattr(self, 'layers_count'):
                self.layers_count.text = f'({count})'
    
    def auto_generate_token(self):
        """Generate a random distributed system token"""
        import random
        import string
        # Generate 4 random digits + 1 random letter
        digits = ''.join(random.choices(string.digits, k=4))
        letter = random.choice(string.ascii_letters)
        token = digits + letter
        self.distributed_token_input.value = token
        self.current_config['distributedSystemToken'] = token
        ui.notify(f'Generated token: {token}', type='positive')
    
    # Help methods
    def show_model_type_help(self):
        """Show model type help"""
        help_text = """Model Types:
        • Neural Network (0): General purpose neural network
        • Approximation (1): Function approximation
        • Classification (2): Classification tasks
        • Forecasting (3): Time series forecasting
        • Image Classification (4): Image recognition
        • Text Classification (5): Text categorization
        • Text Generation (6): Text generation tasks
        • Auto Association (7): Pattern completion
        • Autoencoder (8): Data compression/reconstruction
        • AE Classifier (9): Autoencoder for classification"""
        ui.notify(help_text, type='info', timeout=10000)
    
    def show_layers_help(self):
        """Show layers help"""
        help_text = """Layer Sizes Format:
        
        For CNN layers: 28x28x1k5x5x1x8p0s1t1
        - 28x28x1: Input dimensions (height x width x channels)
        - k5x5x1x8: Kernel 5x5, input channels 1, output channels 8
        - p0: Padding 0
        - s1: Stride 1
        - t1: Activation type
        
        For Dense layers: 256,128,64,10
        - Simple comma-separated neuron counts
        
        Mixed example: 28x28x1k5x5x1x8p0s1t1,256,128,10"""
        ui.notify(help_text, type='info', timeout=12000)
    
    def show_infra_help(self):
        """Show infrastructure help"""
        ui.notify('Infrastructure Types:\n• OpenNN (0): C++ neural network library\n• Wolfram Engine (1): Wolfram Language backend', type='info', timeout=6000)
    
    def show_distributed_help(self):
        """Show distributed system help"""
        help_text = """Distributed System Types:
        • None (0): No distributed training
        • Fed Client/Server Avg (1,2): Federated averaging
        • Fed Weighted Avg (3,4): Weighted federated averaging
        • Fed AE (5,6): Federated autoencoder
        • Tiles (7): Tile-based distributed training"""
        ui.notify(help_text, type='info', timeout=8000)
    
    # File operations
    def load_configuration(self):
        """Load configuration from drag & drop or file selector"""
        ui.notify('Drop a JSON configuration file onto the designated areas, or use the distributed config section', type='info', timeout=6000)
    
    def load_worker_config(self):
        """Load worker configuration from JSON"""
        ui.notify('Use the drag & drop area in the main interface to load JSON files', type='info', timeout=4000)
    
    def export_worker_config(self):
        """Export current configuration as JSON"""
        try:
            # Build the complete worker configuration
            config = self.build_worker_json()
            
            # Get filename
            filename = self.filename_input.value or 'worker_config.json'
            if not filename.endswith('.json'):
                filename += '.json'
            
            # Create JSON string
            import json
            json_str = json.dumps(config, indent=2)
            
            # Trigger download
            ui.download(json_str.encode(), filename=filename)
            ui.notify(f'Configuration exported as {filename}', type='positive')
        except Exception as e:
            ui.notify(f'Export failed: {str(e)}', type='negative')
    
    def build_worker_json(self):
        """Build complete worker JSON configuration"""
        # Generate a model SHA (simplified for demo)
        import hashlib
        config_str = str(self.current_config)
        model_sha = hashlib.sha256(config_str.encode()).hexdigest()
        
        # Build the model configuration
        model_config = {
            "modelType": self.current_config.get('modelType', '0'),
            "_doc_modelType": " nn:0 | approximation:1 | classification:2 | forecasting:3 | image_classification:4 | text_classification:5 | text_generation:6 | auto_association:7 | autoencoder:8 | ae_classifier:9 |",
            "modelArgs": self.current_config.get('modelArgs', ''),
            "_doc_modelArgs": "Extra arguments to model",
            "layersSizes": self.current_config.get('layersSizes', ''),
            "_doc_layersSizes": "List of layer sizes or CNN format (28x28x1k5x5x1x8p0s1t1)",
            "layerTypesList": self.current_config.get('layerTypesList', ''),
            "_doc_LayerTypes": " Default:0 | Scaling:1 | Conv:2 | Perceptron:3 | Pooling:4 | Probabilistic:5 | LSTM:6 | Reccurrent:7 | Unscaling:8 | Flatten:9 | Bounding:10 |",
            "layers_functions": self.current_config.get('layers_functions', ''),
            "_doc_layers_functions_activation": " Threshold:1 | Sign:2 | Logistic:3 | Tanh:4 | Linear:5 | ReLU:6 | eLU:7 | SeLU:8 | Soft-plus:9 | Soft-sign:10 | Hard-sigmoid:11 |",
            "lossMethod": self.current_config.get('lossMethod', '2'),
            "lossArgs": self.current_config.get('lossArgs', ''),
            "_doc_lossMethod": " SSE:1 | MSE:2 | NSE:3 | MinkowskiE:4 | WSE:5 | CEE:6 |",
            "lr": self.current_config.get('lr', '0.001'),
            "_doc_lr": "Positive float",
            "epochs": self.current_config.get('epochs', '1'),
            "_doc_epochs": "Positive Integer",
            "optimizer": self.current_config.get('optimizer', '2'),
            "_doc_optimizer": " GD:0 | CGD:1 | SGD:2 | QuasiNewton:3 | LVM:4 | ADAM:5 |",
            "optimizerArgs": self.current_config.get('optimizerArgs', 'none'),
            "_doc_optimizerArgs": "String",
            "infraType": self.current_config.get('infraType', '0'),
            "_doc_infraType": " opennn:0 | wolfengine:1 |",
            "distributedSystemType": self.current_config.get('distributedSystemType', '0'),
            "_doc_distributedSystemType": " none:0 | FedClientAvg:1 | FedServerAvg:2 | FedClientWeightedAvgClassification:3 | FedServerWeightedAvgClassification:4 | FedClientAE:5 | FedServerAE:6 | tiles:7 |",
            "distributedSystemArgs": self.current_config.get('distributedSystemArgs', 'none'),
            "_doc_distributedSystemArgs": "String",
            "distributedSystemToken": self.current_config.get('distributedSystemToken', 'none'),
            "_doc_distributedSystemToken": "Token that associates distributed group of workers and parameter-server"
        }
        
        # Include documentation if requested
        if not self.with_documentation_checkbox.value:
            # Remove documentation fields
            model_config = {k: v for k, v in model_config.items() if not k.startswith('_doc_')}
        
        # Build complete configuration
        return {
            "workers": [
                {
                    "name": "w1", 
                    "model_sha": model_sha
                }
            ],
            "model_sha": {
                model_sha: model_config
            }
        }
    
    def load_from_json(self, json_data):
        """Load configuration from JSON data"""
        try:
            # Extract model configuration from the JSON structure
            model_config = None
            
            # Handle different JSON structures
            if 'model_sha' in json_data and json_data['model_sha']:
                # Get the first model configuration
                sha_key = list(json_data['model_sha'].keys())[0]
                model_config = json_data['model_sha'][sha_key]
            elif 'modelType' in json_data:
                # Direct model configuration
                model_config = json_data
            
            if model_config:
                # Update UI elements with loaded data
                self.current_config.update({
                    'modelType': model_config.get('modelType', '0'),
                    'modelArgs': model_config.get('modelArgs', ''),
                    'layersSizes': model_config.get('layersSizes', ''),
                    'layerTypesList': model_config.get('layerTypesList', ''),
                    'layers_functions': model_config.get('layers_functions', ''),
                    'lossMethod': model_config.get('lossMethod', '2'),
                    'lossArgs': model_config.get('lossArgs', ''),
                    'lr': model_config.get('lr', '0.001'),
                    'epochs': model_config.get('epochs', '1'),
                    'optimizer': model_config.get('optimizer', '2'),
                    'optimizerArgs': model_config.get('optimizerArgs', 'none'),
                    'infraType': model_config.get('infraType', '0'),
                    'distributedSystemType': model_config.get('distributedSystemType', '0'),
                    'distributedSystemArgs': model_config.get('distributedSystemArgs', 'none'),
                    'distributedSystemToken': model_config.get('distributedSystemToken', 'none')
                })
                
                # Update all UI elements
                self.update_ui_from_config()
                
                # Convert to visual layers if we have layer sizes
                if 'layersSizes' in model_config and model_config['layersSizes']:
                    self.config_to_visual_layers(model_config['layersSizes'])
                    self.render_network_diagram()
                
                ui.notify('Configuration loaded successfully!', type='positive')
                return True
            else:
                ui.notify('No valid model configuration found in JSON', type='warning')
                return False
                
        except Exception as e:
            ui.notify(f'Error loading configuration: {str(e)}', type='negative')
            return False
    
    def update_ui_from_config(self):
        """Update UI elements from current configuration"""
        try:
            # Helper function to find display name by code
            def find_key_by_value(options_dict, code):
                for key, value in options_dict.items():
                    if value == code:
                        return key
                return list(options_dict.keys())[0] if options_dict else None
            
            # Update all input fields
            if hasattr(self, 'model_type_select'):
                code = self.current_config.get('modelType', '0')
                display_name = find_key_by_value(self.get_model_type_options(), code)
                if display_name:
                    self.model_type_select.value = display_name
            
            if hasattr(self, 'model_args_input'):
                self.model_args_input.value = self.current_config.get('modelArgs', '')
            if hasattr(self, 'layers_sizes_input'):
                self.layers_sizes_input.value = self.current_config.get('layersSizes', '')
            if hasattr(self, 'layer_types_input'):
                self.layer_types_input.value = self.current_config.get('layerTypesList', '')
            if hasattr(self, 'layer_functions_input'):
                self.layer_functions_input.value = self.current_config.get('layers_functions', '')
            if hasattr(self, 'learning_rate_input'):
                self.learning_rate_input.value = float(self.current_config.get('lr', '0.001'))
            if hasattr(self, 'epochs_input'):
                self.epochs_input.value = int(self.current_config.get('epochs', '1'))
            
            if hasattr(self, 'optimizer_select'):
                code = self.current_config.get('optimizer', '2')
                display_name = find_key_by_value(self.get_optimizer_options(), code)
                if display_name:
                    self.optimizer_select.value = display_name
                    
            if hasattr(self, 'optimizer_args_input'):
                self.optimizer_args_input.value = self.current_config.get('optimizerArgs', 'none')
            
            if hasattr(self, 'loss_method_select'):
                code = self.current_config.get('lossMethod', '2')
                display_name = find_key_by_value(self.get_loss_method_options(), code)
                if display_name:
                    self.loss_method_select.value = display_name
                    
            if hasattr(self, 'loss_args_input'):
                self.loss_args_input.value = self.current_config.get('lossArgs', '')
            
            if hasattr(self, 'infra_type_select'):
                code = self.current_config.get('infraType', '0')
                display_name = find_key_by_value(self.get_infra_type_options(), code)
                if display_name:
                    self.infra_type_select.value = display_name
            
            if hasattr(self, 'distributed_type_select'):
                code = self.current_config.get('distributedSystemType', '0')
                display_name = find_key_by_value(self.get_distributed_system_options(), code)
                if display_name:
                    self.distributed_type_select.value = display_name
            
            if hasattr(self, 'distributed_token_input'):
                self.distributed_token_input.value = self.current_config.get('distributedSystemToken', 'none')
            if hasattr(self, 'distributed_args_input'):
                self.distributed_args_input.value = self.current_config.get('distributedSystemArgs', 'none')
                
            # Update counters
            self.update_layer_type_count()
            self.update_layer_function_count()
            
        except Exception as e:
            print(f"Warning: Could not update all UI elements: {e}")

    def show_layer_type_help(self):
        """Show help for layer types"""
        help_text = """Layer Types:
        • Default (0): Standard neural network layer
        • Scaling (1): Input normalization layer
        • CNN/Conv (2): Convolutional layer for image processing
        • Perceptron (3): Single neuron layer
        • Pooling (4): Downsampling layer  
        • Probabilistic (5): Probabilistic activation
        • LSTM (6): Long Short-Term Memory
        • Recurrent (7): Recurrent neural network
        • Unscaling (8): Output denormalization
        • Flatten (9): Reshape to 1D
        • Bounding (10): Output constraint layer"""
        ui.notify(help_text, type='info', timeout=10000)

    def show_activation_help(self):
        """Show help for activation functions"""
        help_text = """Layer Functions:
        
        Activation Functions:
        • Linear (5): No activation
        • Sigmoid (3): S-shaped curve, outputs 0-1
        • ReLU (6): Rectified Linear Unit
        • Tanh (4): Hyperbolic tangent
        
        Pooling Methods:
        • Max Pooling (20): Takes maximum value
        • Average Pooling (21): Takes average value
        
        Other Methods:
        • Dropout (41): Prevents overfitting
        • Batch Normalization (40): Normalizes inputs
        • Flatten (50): Converts to 1D"""
        ui.notify(help_text, type='info', timeout=12000)

    def show_add_layer_dialog(self):
        """Show dialog to add a new layer"""
        print("DEBUG: Opening add layer dialog")
        
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 600px; max-width: 90vw'):
                ui.label('Add New Layer').classes('text-h6 mb-4')
                
                # Layer Type Selection
                options = self.get_layer_type_options_for_display()
                print(f"DEBUG: Using options: {options}")
                
                layer_type_select = ui.select(
                    options,
                    label='Layer Type'
                ).classes('w-full mb-4')
                
                # Layer Parameters (will be shown based on type)
                params_container = ui.column().classes('w-full mb-4')
                
                # Function to update parameter fields based on layer type
                def update_params():
                    params_container.clear()
                    if not layer_type_select.value:
                        return
                    
                    layer_type_id = self.extract_layer_type_id(layer_type_select.value)
                    
                    with params_container:
                        if layer_type_id == '2':  # CNN
                            ui.label('CNN Layer Configuration (Format: WxHxDkKxKxFxOp*s*t*)').classes('text-sm font-semibold mb-2')
                            
                            # Input dimensions
                            with ui.row().classes('w-full gap-2'):
                                self.param_width = ui.number('Width', value=28, min=1).classes('flex-1')
                                self.param_height = ui.number('Height', value=28, min=1).classes('flex-1')
                                self.param_depth = ui.number('Input Channels', value=1, min=1).classes('flex-1')
                            
                            # Kernel and output
                            with ui.row().classes('w-full gap-2'):
                                self.param_kernel_w = ui.number('Kernel Width', value=5, min=1).classes('flex-1')
                                self.param_kernel_h = ui.number('Kernel Height', value=5, min=1).classes('flex-1')
                                self.param_filters = ui.number('Output Filters', value=8, min=1).classes('flex-1')
                            
                            # CNN specific parameters
                            with ui.row().classes('w-full gap-2'):
                                self.param_padding = ui.number('Padding', value=0, min=0).classes('flex-1')
                                self.param_stride = ui.number('Stride', value=1, min=1).classes('flex-1')
                                self.param_type = ui.number('Type', value=1, min=0).classes('flex-1')
                                
                            # Preview the generated format
                            ui.label('Preview: 28x28x1k5x5x1x8p0s1t1').classes('text-xs text-gray-600 mt-2')
                            
                        elif layer_type_id == '3':  # Dense/Perceptron
                            ui.label('Dense/Perceptron Layer').classes('text-sm font-semibold mb-2')
                            self.param_neurons = ui.number('Number of Neurons', value=128, min=1).classes('w-full mb-2')
                            
                        elif layer_type_id == '4':  # Pooling
                            ui.label('Pooling Layer Configuration').classes('text-sm font-semibold mb-2')
                            with ui.row().classes('w-full gap-2'):
                                self.param_pool_w = ui.number('Pool Width', value=2, min=1).classes('flex-1')
                                self.param_pool_h = ui.number('Pool Height', value=2, min=1).classes('flex-1')
                            with ui.row().classes('w-full gap-2'):
                                self.param_pool_padding = ui.number('Padding', value=0, min=0).classes('flex-1')
                                self.param_pool_stride = ui.number('Stride', value=2, min=1).classes('flex-1')
                            
                        elif layer_type_id == '1':  # Scaling
                            ui.label('Scaling Layer').classes('text-sm font-semibold mb-2')
                            self.param_scale_factor = ui.number('Scale Factor', value=1.0, step=0.1).classes('w-full mb-2')
                            
                        elif layer_type_id == '9':  # Flatten
                            ui.label('Flatten Layer (no parameters needed)').classes('text-sm font-semibold mb-2')
                            
                        else:  # Default and other types
                            ui.label('Basic Layer').classes('text-sm font-semibold mb-2')
                            self.param_size = ui.number('Layer Size', value=128, min=1).classes('w-full mb-2')
                
                layer_type_select.on('change', lambda: update_params())
                
                # Activation Function
                activation_select = ui.select(
                    self.get_activation_functions(),
                    label='Activation Function',
                    value='ReLU'
                ).classes('w-full mb-4')
                
                # Buttons
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Add Layer', on_click=lambda: self.add_visual_layer(
                        layer_type_select, activation_select, dialog
                    )).classes('bg-orange-800 hover:bg-orange-700 text-white')
        
        dialog.open()
    
    def add_visual_layer(self, layer_type_select, activation_select, dialog):
        """Add a visual layer to the network"""
        if not layer_type_select.value:
            ui.notify('Please select a layer type', type='warning')
            return
        
        layer_type_code = self.extract_layer_type_id(layer_type_select.value)
        activation_code = self.get_activation_functions().get(activation_select.value, '6')
        
        # Generate layer size based on type
        layer_size = ""
        
        if layer_type_code == '2':  # CNN
            # Generate CNN format: WxHxDkKWxKHxFxOp*s*t*
            w = int(getattr(self, 'param_width', ui.number('', value=28)).value)
            h = int(getattr(self, 'param_height', ui.number('', value=28)).value)
            d = int(getattr(self, 'param_depth', ui.number('', value=1)).value)
            kw = int(getattr(self, 'param_kernel_w', ui.number('', value=5)).value)
            kh = int(getattr(self, 'param_kernel_h', ui.number('', value=5)).value)
            f = int(getattr(self, 'param_filters', ui.number('', value=8)).value)
            p = int(getattr(self, 'param_padding', ui.number('', value=0)).value)
            s = int(getattr(self, 'param_stride', ui.number('', value=1)).value)
            t = int(getattr(self, 'param_type', ui.number('', value=1)).value)
            
            layer_size = f"{w}x{h}x{d}k{kw}x{kh}x1x{f}p{p}s{s}t{t}"
            
        elif layer_type_code == '4':  # Pooling
            # Generate pooling format: WxHp*s*
            pw = int(getattr(self, 'param_pool_w', ui.number('', value=2)).value)
            ph = int(getattr(self, 'param_pool_h', ui.number('', value=2)).value)
            pp = int(getattr(self, 'param_pool_padding', ui.number('', value=0)).value)
            ps = int(getattr(self, 'param_pool_stride', ui.number('', value=2)).value)
            
            layer_size = f"{pw}x{ph}p{pp}s{ps}"
            
        elif layer_type_code == '3':  # Dense/Perceptron
            neurons = int(getattr(self, 'param_neurons', ui.number('', value=128)).value)
            layer_size = str(neurons)
            
        elif layer_type_code == '1':  # Scaling
            scale = float(getattr(self, 'param_scale_factor', ui.number('', value=1.0)).value)
            layer_size = str(scale)
            
        else:  # Default and other types
            size = int(getattr(self, 'param_size', ui.number('', value=128)).value)
            layer_size = str(size)
        
        # Add to model data structures
        if not hasattr(self.model, 'layer_sizes_raw'):
            self.model.layer_sizes_raw = []
        if not hasattr(self.model, 'layer_types_raw'):
            self.model.layer_types_raw = []
        if not hasattr(self.model, 'layer_functions_raw'):
            self.model.layer_functions_raw = []
        
        self.model.layer_sizes_raw.append(layer_size)
        self.model.layer_types_raw.append(layer_type_code)
        self.model.layer_functions_raw.append(activation_code)
        
        # Update UI elements
        if hasattr(self, 'layers_sizes_input'):
            self.layers_sizes_input.value = ','.join(self.model.layer_sizes_raw)
        if hasattr(self, 'layer_types_input'):
            self.layer_types_input.value = ','.join(self.model.layer_types_raw)
        if hasattr(self, 'layer_functions_input'):
            self.layer_functions_input.value = ','.join(self.model.layer_functions_raw)
        
        # Update config
        self.current_config['layersSizes'] = ','.join(self.model.layer_sizes_raw)
        self.current_config['layerTypesList'] = ','.join(self.model.layer_types_raw)
        self.current_config['layers_functions'] = ','.join(self.model.layer_functions_raw)
        
        # Re-render network diagram
        print(f"DEBUG: Added layer - sizes: {self.model.layer_sizes_raw}, types: {self.model.layer_types_raw}")
        self.render_network_diagram()
        
        ui.notify(f'Added {layer_type_select.value} layer: {layer_size}', type='positive')
        dialog.close()

    def update_visual_network(self):
        """Update the visual network display"""
        if not hasattr(self, 'network_canvas'):
            return
        
        self.network_canvas.clear()
        
        with self.network_canvas:
            if not self.visual_layers:
                ui.label('No layers configured. Add layers to start designing.').classes('text-center text-gray-500 py-8')
                return
            
            for i, layer in enumerate(self.visual_layers):
                self.create_visual_layer_card(layer, i)
                
                # Add connection arrow (except for last layer)
                if i < len(self.visual_layers) - 1:
                    with ui.row().classes('w-full justify-center py-2'):
                        ui.icon('arrow_downward', size='lg').classes('text-gray-400')
    
    def create_visual_layer_card(self, layer, index):
        """Create a visual card for a layer"""
        with ui.card().classes('w-full max-w-sm mx-auto mb-4 border-2 border-orange-200 hover:border-orange-400'):
            with ui.card_section():
                # Layer header
                with ui.row().classes('w-full items-center justify-between mb-2'):
                    ui.label(f"Layer {index}").classes('text-h6 font-bold')
                    ui.label(layer['type']).classes('text-sm bg-orange-100 px-2 py-1 rounded')
                
                # Layer details
                with ui.column().classes('w-full gap-1'):
                    ui.label(f"Type: {layer['type']}").classes('text-sm')
                    ui.label(f"Activation: {layer['activation']}").classes('text-sm')
                    if 'neurons' in layer:
                        ui.label(f"Neurons: {layer['neurons']}").classes('text-sm')
                    if 'input_shape' in layer:
                        ui.label(f"Input: {layer['input_shape']}").classes('text-sm text-gray-600')
                    if 'output_shape' in layer:
                        ui.label(f"Output: {layer['output_shape']}").classes('text-sm text-gray-600')
                
                # Layer controls
                with ui.row().classes('w-full justify-end gap-1 mt-2'):
                    ui.button('Edit', on_click=lambda idx=index: self.edit_visual_layer(idx), 
                             icon='edit').props('size=sm').classes('bg-orange-600 hover:bg-orange-500 text-white')
                    ui.button('Delete', on_click=lambda idx=index: self.delete_visual_layer(idx), 
                             icon='delete').props('size=sm').classes('bg-red-600 hover:bg-red-500 text-white')
    
    def edit_visual_layer(self, index):
        """Edit a visual layer"""
        if index >= len(self.visual_layers):
            return
        
        layer = self.visual_layers[index]
        
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 500px; max-width: 90vw'):
                ui.label(f'Edit Layer {index}').classes('text-h6 mb-4')
                
                # Layer type (read-only)
                ui.input('Layer Type', value=layer['type']).props('readonly').classes('w-full mb-2')
                
                # Editable properties
                activation_select = ui.select(
                    self.get_activation_functions(),
                    label='Activation Function',
                    value=layer['activation']
                ).classes('w-full mb-2')
                
                neurons_input = ui.number(
                    'Number of Neurons',
                    value=layer.get('neurons', 128),
                    min=1, max=10000
                ).classes('w-full mb-4')
                
                # Buttons
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Save', on_click=lambda: self.save_layer_edit(
                        index, activation_select, neurons_input, dialog
                    )).classes('bg-orange-800 hover:bg-orange-700 text-white')
        
        dialog.open()
    
    def save_layer_edit(self, index, activation_select, neurons_input, dialog):
        """Save layer edits"""
        if index >= len(self.visual_layers):
            return
        
        # Update layer properties
        self.visual_layers[index]['activation'] = activation_select.value
        self.visual_layers[index]['activation_code'] = self.get_activation_functions().get(activation_select.value, '6')
        self.visual_layers[index]['neurons'] = int(neurons_input.value)
        
        # Refresh display
        self.update_visual_network()
        self.sync_visual_to_config()
        
        ui.notify('Layer updated successfully', type='positive')
        dialog.close()
    
    def delete_visual_layer(self, index):
        """Delete a visual layer"""
        if index >= len(self.visual_layers):
            return
        
        # Remove layer
        removed_layer = self.visual_layers.pop(index)
        
        # Refresh display
        self.update_visual_network()
        self.sync_visual_to_config()
        
        ui.notify(f'Deleted {removed_layer["type"]} layer', type='positive')
    
    def sync_visual_to_config(self):
        """Synchronize visual layers to configuration strings"""
        if not self.visual_layers:
            return
        
        # Build layer types list
        layer_types = [layer['type_code'] for layer in self.visual_layers]
        self.layer_types_input.value = ','.join(layer_types)
        self.current_config['layerTypesList'] = ','.join(layer_types)
        
        # Build layer functions list
        layer_functions = [layer['activation_code'] for layer in self.visual_layers]
        self.layer_functions_input.value = ','.join(layer_functions)
        self.current_config['layers_functions'] = ','.join(layer_functions)
        
        # Build layer sizes (simplified for now)
        layer_sizes = [str(layer.get('neurons', 128)) for layer in self.visual_layers if layer.get('neurons')]
        if layer_sizes:
            self.layers_sizes_input.value = ','.join(layer_sizes)
            self.current_config['layersSizes'] = ','.join(layer_sizes)
        
        # Update counters
        self.update_layer_type_count()
        self.update_layer_function_count()

    def copy_config(self):
        """Copy configuration to clipboard"""
        try:
            import json
            config = self.get_current_config()
            json_str = json.dumps(config, indent=2)
            # Note: Actual clipboard copy would need additional JS/browser API
            ui.notify('📋 Configuration copied! (JSON in console)', type='positive')
            print("Worker Configuration JSON:")
            print(json_str)
        except Exception as e:
            ui.notify(f'❌ Copy failed: {str(e)}', type='negative')
    
    def save_config(self):
        """Save configuration"""
        try:
            import json
            config = self.get_current_config()
            json_str = json.dumps(config, indent=2)
            
            # Trigger download
            ui.download(json_str.encode(), filename='worker_config.json')
            ui.notify('💾 Configuration saved!', type='positive')
        except Exception as e:
            ui.notify(f'❌ Save failed: {str(e)}', type='negative')
    
    def reset_config(self):
        """Reset configuration to defaults"""
        self.current_layers = [784, 128, 10]
        self.update_layer_designer()
        self.update_network_visualization()
        self.update_summary()
        ui.notify('🔄 Configuration reset to defaults', type='positive')
    
    def get_current_config(self) -> dict:
        """Get current configuration as dictionary"""
        def get_ui_value(key, default):
            if key in self.ui_elements and hasattr(self.ui_elements[key], 'value'):
                return self.ui_elements[key].value
            return default
            
        return {
            'model_type': get_ui_value('model_type', 'neural_network'),
            'infra_type': get_ui_value('infra_type', 'sequential'),
            'layer_sizes': getattr(self, 'current_layers', [784, 128, 10]),
            'loss_method': get_ui_value('loss_method', 'mean_squared_error'),
            'optimizer_type': get_ui_value('optimizer', 'adam'),
            'learning_rate': get_ui_value('learning_rate', 0.001),
            'batch_size': get_ui_value('batch_size', 32),
            'dropout_rate': get_ui_value('dropout_rate', 0.2),
            'distributed_system_type': get_ui_value('distributed_system', 'federated')
        }
    
    def render_network_diagram(self):
        """Render the visual network diagram"""
        print(f"DEBUG: render_network_diagram called")
        print(f"DEBUG: has network_canvas: {hasattr(self, 'network_canvas')}")
        if hasattr(self.model, 'layer_sizes_raw'):
            print(f"DEBUG: layer_sizes_raw: {self.model.layer_sizes_raw}")
        if hasattr(self.model, 'layer_types_raw'):
            print(f"DEBUG: layer_types_raw: {self.model.layer_types_raw}")
        
        if not hasattr(self, 'network_canvas'):
            print("DEBUG: network_canvas not found, skipping render")
            return
            
        self.network_canvas.clear()
        with self.network_canvas:
            # Add network visualization header
            ui.label('Network Architecture').classes('text-lg font-bold mb-2')
            
            # Create scrollable network canvas
            with ui.column().classes('w-full border border-gray-300 rounded-lg p-4 bg-gray-900 max-h-80 overflow-y-auto') as canvas:
                # Check if we have layers to display
                has_layers = False
                layer_data = []
                
                # Get layer information from raw data
                if (hasattr(self.model, 'layer_sizes_raw') and self.model.layer_sizes_raw and
                    hasattr(self.model, 'layer_types_raw') and self.model.layer_types_raw):
                    
                    for i, (size, layer_type) in enumerate(zip(self.model.layer_sizes_raw, self.model.layer_types_raw)):
                        layer_info = {
                            'index': i,
                            'size': size,
                            'type': layer_type,
                            'type_name': self.get_layer_type_name(layer_type),
                            'activation': self.model.layer_functions_raw[i] if i < len(self.model.layer_functions_raw) else '6'
                        }
                        layer_data.append(layer_info)
                        has_layers = True
                
                print(f"DEBUG: has_layers = {has_layers}, layer_data length = {len(layer_data)}")
                
                if not has_layers:
                    print("DEBUG: No layers found, showing empty message")
                    with ui.row().classes('justify-center p-8'):
                        ui.label('No layers added. Use the "Add Layer" button to build your network.').classes('text-gray-400 italic')
                else:
                    # Render each layer
                    for i, layer in enumerate(layer_data):
                        with ui.card().classes('w-full mb-2 bg-gray-800 border border-gray-600'):
                            with ui.card_section().classes('p-3'):
                                with ui.row().classes('w-full items-center justify-between'):
                                    with ui.column().classes('flex-1'):
                                        ui.label(f'Layer {i + 1}: {layer["type_name"]}').classes('text-white font-bold')
                                        if layer['type'] == '2':  # CNN layer
                                            ui.label(f'CNN: {layer["size"]}').classes('text-sm text-blue-300')
                                        elif layer['type'] == '4':  # Pooling layer
                                            ui.label(f'Pooling: {layer["size"]}').classes('text-sm text-green-300')
                                        else:
                                            ui.label(f'Size: {layer["size"]}').classes('text-sm text-gray-300')
                                        
                                        # Show activation function
                                        activation_name = self.get_activation_name(layer['activation'])
                                        ui.label(f'Activation: {activation_name}').classes('text-xs text-yellow-300')
                                    
                                    with ui.row().classes('gap-2'):
                                        ui.button('Edit', icon='edit', 
                                                on_click=lambda idx=i: self.edit_layer(idx)).classes('bg-blue-600 hover:bg-blue-500 text-white text-xs px-2 py-1')
                                        ui.button('Remove', icon='delete',
                                                on_click=lambda idx=i: self.remove_layer(idx)).classes('bg-red-600 hover:bg-red-500 text-white text-xs px-2 py-1')
                                
                                # Visual connection line (except for last layer)
                                if i < len(layer_data) - 1:
                                    with ui.row().classes('justify-center mt-2'):
                                        ui.icon('arrow_downward', size='sm').classes('text-gray-400')
                    
                    # Quick configuration display
                    with ui.card().classes('w-full mt-4 p-3 bg-blue-50 border-l-4 border-blue-400'):
                        ui.label('Current Configuration:').classes('font-semibold text-blue-800')
                        
                        # Show current layer configuration
                        if hasattr(self.model, 'layer_sizes_raw') and self.model.layer_sizes_raw:
                            ui.label(f'Layer Sizes: {",".join(self.model.layer_sizes_raw)}').classes('font-mono text-sm text-blue-700')
                        if hasattr(self.model, 'layer_types_raw') and self.model.layer_types_raw:
                            ui.label(f'Layer Types: {",".join(self.model.layer_types_raw)}').classes('font-mono text-sm text-blue-700')
                        if hasattr(self.model, 'layer_functions_raw') and self.model.layer_functions_raw:
                            ui.label(f'Activations: {",".join(self.model.layer_functions_raw)}').classes('font-mono text-sm text-blue-700')
            
            # Layer management controls
            self.render_layer_controls()
    
    def render_layer_card(self, layer, index):
        """Render an individual layer card"""
        with ui.card().classes('w-full mb-2 p-3 border border-gray-200 hover:border-orange-300 transition-colors'):
            with ui.row().classes('w-full justify-between items-center'):
                # Layer info
                with ui.column().classes('flex-grow'):
                    layer_title = f"Layer {index + 1}: {layer['type']}"
                    if layer['type'] == 'Conv2D':
                        layer_title += f" ({layer['filters']} filters, {layer['kernel_size']}x{layer['kernel_size']})"
                    elif layer['type'] == 'Dense':
                        layer_title += f" ({layer['units']} units)"
                    elif layer['type'] in ['MaxPool2D', 'AvgPool2D']:
                        layer_title += f" ({layer['pool_size']}x{layer['pool_size']})"
                    
                    ui.label(layer_title).classes('font-semibold text-gray-800')
                    
                    # Layer details
                    details = []
                    if layer['type'] == 'Conv2D':
                        details.append(f"Activation: {layer.get('activation', 'relu')}")
                        if layer.get('padding') != 'valid':
                            details.append(f"Padding: {layer['padding']}")
                        if layer.get('strides') != 1:
                            details.append(f"Stride: {layer['strides']}")
                    elif layer['type'] == 'Dense':
                        details.append(f"Activation: {layer.get('activation', 'relu')}")
                    elif layer['type'] == 'Dropout':
                        details.append(f"Rate: {layer.get('rate', 0.2)}")
                    
                    if details:
                        ui.label(' • '.join(details)).classes('text-sm text-gray-600')
                
                # Layer controls
                with ui.row().classes('gap-1'):
                    ui.button('Edit', on_click=lambda idx=index: self.edit_layer(idx)).classes('bg-blue-500 text-white px-2 py-1 text-xs rounded hover:bg-blue-600')
                    ui.button('↑', on_click=lambda idx=index: self.move_layer(idx, -1)).classes('bg-gray-400 text-white px-2 py-1 text-xs rounded hover:bg-gray-500').props('disabled' if index == 0 else '')
                    ui.button('↓', on_click=lambda idx=index: self.move_layer(idx, 1)).classes('bg-gray-400 text-white px-2 py-1 text-xs rounded hover:bg-gray-500').props('disabled' if index == len(self.visual_layers) - 1 else '')
                    ui.button('×', on_click=lambda idx=index: self.remove_layer(idx)).classes('bg-red-500 text-white px-2 py-1 text-xs rounded hover:bg-red-600')
    
    def render_layer_controls(self):
        """Render layer management controls"""
        with ui.card().classes('w-full mt-4 p-4 bg-orange-50 border border-orange-200'):
            ui.label('Add Layer').classes('text-lg font-semibold mb-3 text-orange-800')
            
            with ui.row().classes('w-full gap-2 flex-wrap'):
                # Quick add buttons for common layers
                ui.button('+ Input Layer', on_click=lambda: self.quick_add_layer('Input')).classes('bg-green-500 text-white px-3 py-2 rounded hover:bg-green-600')
                ui.button('+ Conv2D', on_click=lambda: self.quick_add_layer('Conv2D')).classes('bg-blue-500 text-white px-3 py-2 rounded hover:bg-blue-600')
                ui.button('+ MaxPool2D', on_click=lambda: self.quick_add_layer('MaxPool2D')).classes('bg-purple-500 text-white px-3 py-2 rounded hover:bg-purple-600')
                ui.button('+ Dense', on_click=lambda: self.quick_add_layer('Dense')).classes('bg-orange-500 text-white px-3 py-2 rounded hover:bg-orange-600')
                ui.button('+ Dropout', on_click=lambda: self.quick_add_layer('Dropout')).classes('bg-yellow-500 text-white px-3 py-2 rounded hover:bg-yellow-600')
                ui.button('+ Flatten', on_click=lambda: self.quick_add_layer('Flatten')).classes('bg-gray-500 text-white px-3 py-2 rounded hover:bg-gray-600')
                ui.button('+ Output', on_click=lambda: self.quick_add_layer('Output')).classes('bg-red-500 text-white px-3 py-2 rounded hover:bg-red-600')
    
    def quick_add_layer(self, layer_type):
        """Quick add a layer with default parameters"""
        # Map quick layer types to NerlNet format
        layer_mappings = {
            'input': {'size': '784', 'type_code': '0', 'activation_code': '6'},  # Default input
            'dense': {'size': '128', 'type_code': '3', 'activation_code': '6'},  # Perceptron/Dense 
            'conv2d': {'size': '28x28x1k3x3x1x32p1s1t1', 'type_code': '2', 'activation_code': '6'},  # CNN
            'maxpool': {'size': '2x2p0s2', 'type_code': '4', 'activation_code': '6'},  # Pooling
            'dropout': {'size': '0.2', 'type_code': '5', 'activation_code': '6'},  # Probabilistic
            'batchnorm': {'size': '1', 'type_code': '1', 'activation_code': '6'},  # Scaling  
            'flatten': {'size': '1', 'type_code': '9', 'activation_code': '6'},  # Flatten
            'output': {'size': '10', 'type_code': '3', 'activation_code': '7'}  # Dense with softmax
        }
        
        layer_key = layer_type.lower()
        if layer_key in layer_mappings:
            mapping = layer_mappings[layer_key]
            
            # Initialize model arrays if needed
            if not hasattr(self.model, 'layer_sizes_raw'):
                self.model.layer_sizes_raw = []
            if not hasattr(self.model, 'layer_types_raw'):
                self.model.layer_types_raw = []
            if not hasattr(self.model, 'layer_functions_raw'):
                self.model.layer_functions_raw = []
            
            # Add layer to model
            self.model.layer_sizes_raw.append(mapping['size'])
            self.model.layer_types_raw.append(mapping['type_code'])
            self.model.layer_functions_raw.append(mapping['activation_code'])
            
            # Update UI elements
            if hasattr(self, 'layers_sizes_input'):
                self.layers_sizes_input.value = ','.join(self.model.layer_sizes_raw)
            if hasattr(self, 'layer_types_input'):
                self.layer_types_input.value = ','.join(self.model.layer_types_raw)
            if hasattr(self, 'layer_functions_input'):
                self.layer_functions_input.value = ','.join(self.model.layer_functions_raw)
            
            # Update config
            self.current_config['layersSizes'] = ','.join(self.model.layer_sizes_raw)
            self.current_config['layerTypesList'] = ','.join(self.model.layer_types_raw)
            self.current_config['layers_functions'] = ','.join(self.model.layer_functions_raw)
            
            # Refresh network diagram
            self.render_network_diagram()
            
            ui.notify(f'Added {layer_type} layer: {mapping["size"]}', type='positive')
    
    def edit_layer(self, layer_index):
        """Open layer editing dialog"""
        if (not hasattr(self.model, 'layer_sizes_raw') or 
            layer_index >= len(self.model.layer_sizes_raw)):
            ui.notify('Layer not found', type='warning')
            return
            
        current_size = self.model.layer_sizes_raw[layer_index]
        current_type = self.model.layer_types_raw[layer_index] if layer_index < len(self.model.layer_types_raw) else '0'
        current_activation = self.model.layer_functions_raw[layer_index] if layer_index < len(self.model.layer_functions_raw) else '6'
        
        with ui.dialog().props('persistent') as dialog:
            with ui.card().style('width: 500px; max-width: 90vw'):
                ui.label(f'Edit Layer {layer_index + 1}').classes('text-h6 mb-4')
                
                # Layer type selector
                layer_type_select = ui.select(
                    self.get_layer_type_options_for_display(),
                    label='Layer Type',
                    value=self.get_display_key_for_layer_type(current_type)
                ).classes('w-full mb-3')
                
                # Layer size input
                layer_size_input = ui.input(
                    'Layer Size/Configuration', 
                    value=current_size,
                    placeholder='e.g., 128 or 28x28x1k5x5x1x8p0s1t1'
                ).classes('w-full mb-3')
                
                # Activation function
                activation_select = ui.select(
                    self.get_activation_functions(),
                    label='Activation Function',
                    value=self.get_activation_name_by_code(current_activation)
                ).classes('w-full mb-4')
                
                # Help text
                ui.label('For CNN layers use format: WxHxDkKWxKHxFxOp*s*t* (e.g., 28x28x1k5x5x1x8p0s1t1)').classes('text-xs text-gray-600 mb-4')
                
                # Buttons
                with ui.row().classes('w-full justify-end gap-2'):
                    ui.button('Cancel', on_click=dialog.close).classes('bg-gray-500 hover:bg-gray-400 text-white')
                    ui.button('Save Changes', on_click=lambda: self.save_layer_edit(
                        layer_index, layer_type_select, layer_size_input, activation_select, dialog
                    )).classes('bg-orange-800 hover:bg-orange-700 text-white')
        
        dialog.open()
    
    def render_layer_params(self, layer_type, current_layer):
        """Render parameter inputs for the selected layer type"""
        if layer_type == 'Input':
            ui.label('Input Shape (H x W x C):').classes('font-semibold')
            shape = current_layer.get('shape', [28, 28, 1])
            with ui.row().classes('gap-2'):
                self.edit_height = ui.number(label='Height', value=shape[0]).classes('flex-1')
                self.edit_width = ui.number(label='Width', value=shape[1]).classes('flex-1')
                self.edit_channels = ui.number(label='Channels', value=shape[2]).classes('flex-1')
        
        elif layer_type == 'Conv2D':
            self.edit_filters = ui.number(label='Number of Filters', value=current_layer.get('filters', 32), min=1)
            self.edit_kernel_size = ui.number(label='Kernel Size', value=current_layer.get('kernel_size', 3), min=1)
            self.edit_strides = ui.number(label='Strides', value=current_layer.get('strides', 1), min=1)
            self.edit_padding = ui.select(
                options={'same': 'Same', 'valid': 'Valid'},
                label='Padding',
                value=current_layer.get('padding', 'same')
            )
            self.edit_activation = ui.select(
                options={'relu': 'ReLU', 'sigmoid': 'Sigmoid', 'tanh': 'Tanh', 'linear': 'Linear'},
                label='Activation',
                value=current_layer.get('activation', 'relu')
            )
        
        elif layer_type in ['MaxPool2D', 'AvgPool2D']:
            self.edit_pool_size = ui.number(label='Pool Size', value=current_layer.get('pool_size', 2), min=1)
            self.edit_strides = ui.number(label='Strides', value=current_layer.get('strides', 2), min=1)
            self.edit_padding = ui.select(
                options={'same': 'Same', 'valid': 'Valid'},
                label='Padding',
                value=current_layer.get('padding', 'valid')
            )
        
        elif layer_type == 'Dense':
            self.edit_units = ui.number(label='Number of Units', value=current_layer.get('units', 128), min=1)
            self.edit_activation = ui.select(
                options={'relu': 'ReLU', 'sigmoid': 'Sigmoid', 'tanh': 'Tanh', 'softmax': 'Softmax', 'linear': 'Linear'},
                label='Activation',
                value=current_layer.get('activation', 'relu')
            )
        
        elif layer_type == 'Dropout':
            self.edit_rate = ui.number(label='Dropout Rate', value=current_layer.get('rate', 0.2), min=0.0, max=1.0, step=0.1)
        
        # BatchNormalization and Flatten don't have parameters to edit
    
    def save_layer_edit(self, layer_index, layer_type, params_container, dialog):
        """Save the edited layer parameters"""
        new_layer = {'type': layer_type}
        
        if layer_type == 'Input' and hasattr(self, 'edit_height'):
            new_layer['shape'] = [int(self.edit_height.value), int(self.edit_width.value), int(self.edit_channels.value)]
        elif layer_type == 'Conv2D' and hasattr(self, 'edit_filters'):
            new_layer.update({
                'filters': int(self.edit_filters.value),
                'kernel_size': int(self.edit_kernel_size.value),
                'strides': int(self.edit_strides.value),
                'padding': self.edit_padding.value,
                'activation': self.edit_activation.value
            })
        elif layer_type in ['MaxPool2D', 'AvgPool2D'] and hasattr(self, 'edit_pool_size'):
            new_layer.update({
                'pool_size': int(self.edit_pool_size.value),
                'strides': int(self.edit_strides.value),
                'padding': self.edit_padding.value
            })
        elif layer_type == 'Dense' and hasattr(self, 'edit_units'):
            new_layer.update({
                'units': int(self.edit_units.value),
                'activation': self.edit_activation.value
            })
        elif layer_type == 'Dropout' and hasattr(self, 'edit_rate'):
            new_layer['rate'] = float(self.edit_rate.value)
        
        self.visual_layers[layer_index] = new_layer
        self.update_config_from_visual()
        self.render_network_diagram()
        dialog.close()
    
    def move_layer(self, layer_index, direction):
        """Move layer up or down in the network"""
        if not hasattr(self, 'visual_layers') or layer_index < 0 or layer_index >= len(self.visual_layers):
            return
        
        new_index = layer_index + direction
        if new_index < 0 or new_index >= len(self.visual_layers):
            return
        
        # Swap layers
        self.visual_layers[layer_index], self.visual_layers[new_index] = \
            self.visual_layers[new_index], self.visual_layers[layer_index]
        
        self.update_config_from_visual()
        self.render_network_diagram()
    
    def remove_layer(self, layer_index):
        """Remove layer from the network"""
        if not hasattr(self, 'visual_layers') or layer_index < 0 or layer_index >= len(self.visual_layers):
            return
        
        self.visual_layers.pop(layer_index)
        self.update_config_from_visual()
        self.render_network_diagram()
    
    def visual_layers_to_config_string(self):
        """Convert visual layers to configuration string"""
        if not hasattr(self, 'visual_layers') or not self.visual_layers:
            return "784,128,10"
        
        config_parts = []
        for layer in self.visual_layers:
            if layer['type'] == 'Input':
                shape = layer.get('shape', [28, 28, 1])
                config_parts.append(f"{shape[0]}x{shape[1]}x{shape[2]}")
            elif layer['type'] == 'Conv2D':
                filters = layer.get('filters', 32)
                kernel = layer.get('kernel_size', 3)
                strides = layer.get('strides', 1)
                padding = 1 if layer.get('padding', 'same') == 'same' else 0
                config_parts.append(f"k{kernel}x{kernel}x1x{filters}p{padding}s{strides}t1")
            elif layer['type'] in ['MaxPool2D', 'AvgPool2D']:
                pool_size = layer.get('pool_size', 2)
                strides = layer.get('strides', 2)
                padding = 1 if layer.get('padding', 'same') == 'same' else 0
                config_parts.append(f"k{pool_size}x{pool_size}p{padding}s{strides}")
            elif layer['type'] == 'Dense':
                units = layer.get('units', 128)
                config_parts.append(str(units))
            elif layer['type'] == 'Flatten':
                config_parts.append("flatten")
            elif layer['type'] == 'Dropout':
                rate = layer.get('rate', 0.2)
                config_parts.append(f"dropout{rate}")
        
        return ','.join(config_parts) if config_parts else "784,128,10"
    
    def update_config_from_visual(self):
        """Update the configuration from visual layers"""
        config_string = self.visual_layers_to_config_string()
        self.current_layers = self.parse_layer_config(config_string)
        
        # Update UI elements if they exist
        if hasattr(self, 'ui_elements') and 'layers' in self.ui_elements:
            self.ui_elements['layers'].set_value(config_string)
    
    def create_default_network(self):
        """Create a default network configuration"""
        if not hasattr(self, 'visual_layers'):
            self.visual_layers = []
        
        # Default CNN network for MNIST-like data
        default_layers = [
            {'type': 'Input', 'shape': [28, 28, 1]},
            {'type': 'Conv2D', 'filters': 32, 'kernel_size': 3, 'activation': 'relu', 'padding': 'same'},
            {'type': 'MaxPool2D', 'pool_size': 2, 'strides': 2},
            {'type': 'Conv2D', 'filters': 64, 'kernel_size': 3, 'activation': 'relu', 'padding': 'same'},
            {'type': 'MaxPool2D', 'pool_size': 2, 'strides': 2},
            {'type': 'Flatten'},
            {'type': 'Dense', 'units': 128, 'activation': 'relu'},
            {'type': 'Dropout', 'rate': 0.2},
            {'type': 'Dense', 'units': 10, 'activation': 'softmax'}
        ]
        
        self.visual_layers = default_layers
        self.update_config_from_visual()
        self.render_network_diagram()
    
    def config_to_visual_layers(self, config_string):
        """Convert configuration string to visual layers"""
        if not config_string:
            return
        
        self.visual_layers = []
        parts = config_string.split(',')
        
        for i, part in enumerate(parts):
            part = part.strip()
            if not part:
                continue
            
            # Handle different layer formats
            if 'x' in part and 'k' in part:
                # Convolutional layer format: 28x28x1k5x5x1x8p0s1t1
                layer = self.parse_conv_layer(part)
                if layer:
                    self.visual_layers.append(layer)
            elif 'x' in part and part.replace('x', '').replace('.', '').isdigit():
                # Input dimension format: 28x28x1
                dims = part.split('x')
                if len(dims) == 3:
                    self.visual_layers.append({
                        'type': 'Input',
                        'shape': [int(dims[0]), int(dims[1]), int(dims[2])]
                    })
            elif part.isdigit():
                # Dense layer
                units = int(part)
                activation = 'softmax' if i == len(parts) - 1 else 'relu'  # Last layer usually softmax
                self.visual_layers.append({
                    'type': 'Dense',
                    'units': units,
                    'activation': activation
                })
            elif part.lower() == 'flatten':
                self.visual_layers.append({'type': 'Flatten'})
            elif part.lower().startswith('dropout'):
                rate_str = part[7:]  # Remove 'dropout' prefix
                rate = float(rate_str) if rate_str else 0.2
                self.visual_layers.append({
                    'type': 'Dropout',
                    'rate': rate
                })
    
    def parse_conv_layer(self, layer_str):
        """Parse convolutional layer string format"""
        try:
            # Example: 28x28x1k5x5x1x8p0s1t1
            if 'k' in layer_str:
                input_part, conv_part = layer_str.split('k', 1)
                
                # Parse input dimensions (optional)
                if 'x' in input_part:
                    input_dims = input_part.split('x')
                    if len(input_dims) == 3:
                        # This is an input + conv layer, split it
                        return {
                            'type': 'Conv2D',
                            'filters': 32,  # Default
                            'kernel_size': 3,  # Default
                            'activation': 'relu',
                            'padding': 'same'
                        }
                
                # Parse convolution parameters
                # Format: kHxWxInxOutpPsSt (e.g., k5x5x1x8p0s1t1)
                import re
                
                # Extract filters (output channels)
                filters_match = re.search(r'x(\d+)(?:p|$)', conv_part)
                filters = int(filters_match.group(1)) if filters_match else 32
                
                # Extract kernel size
                kernel_match = re.search(r'^(\d+)x(\d+)', conv_part)
                kernel_size = int(kernel_match.group(1)) if kernel_match else 3
                
                # Extract padding
                padding_match = re.search(r'p(\d+)', conv_part)
                padding = 'same' if padding_match and int(padding_match.group(1)) > 0 else 'valid'
                
                # Extract stride
                stride_match = re.search(r's(\d+)', conv_part)
                strides = int(stride_match.group(1)) if stride_match else 1
                
                return {
                    'type': 'Conv2D',
                    'filters': filters,
                    'kernel_size': kernel_size,
                    'strides': strides,
                    'padding': padding,
                    'activation': 'relu'
                }
        except Exception as e:
            print(f"Error parsing conv layer {layer_str}: {e}")
            return None
    
    # Missing callback methods
    def load_configuration(self):
        """Load a configuration file"""
        ui.notify('Load configuration functionality coming soon!', color='info')
    
    def show_model_type_help(self):
        """Show help for model types"""
        with ui.dialog() as dialog, ui.card():
            ui.html('<h3>Model Type Help</h3>', sanitize=False)
            ui.html('''
            <p><strong>Neural Network:</strong> Standard fully connected neural network</p>
            <p><strong>CNN:</strong> Convolutional Neural Network for image processing</p>
            <p><strong>RNN:</strong> Recurrent Neural Network for sequential data</p>
            <p><strong>Classification:</strong> Network optimized for classification tasks</p>
            <p><strong>Regression:</strong> Network optimized for regression tasks</p>
            ''', sanitize=False)
            ui.button('Close', on_click=dialog.close).classes('bg-orange-600 hover:bg-orange-500 text-white')
        dialog.open()
    
    def show_layers_help(self):
        """Show help for layer configuration"""
        with ui.dialog() as dialog, ui.card():
            ui.html('<h3>Layer Configuration Help</h3>', sanitize=False)
            ui.html('''
            <p><strong>Layer Sizes:</strong> Number of neurons in each layer (comma-separated)</p>
            <p>Example: 784,128,64,10</p>
            <p>First layer should match input size, last should match output size</p>
            ''', sanitize=False)
            ui.button('Close', on_click=dialog.close).classes('bg-orange-600 hover:bg-orange-500 text-white')
        dialog.open()
    
    def add_layer_type(self):
        """Add a new layer type"""
        ui.notify('Add layer type functionality coming soon!', color='info')
    
    def show_layer_type_help(self):
        """Show help for layer types"""
        with ui.dialog() as dialog, ui.card():
            ui.html('<h3>Layer Type Help</h3>', sanitize=False)
            ui.html('''
            <p><strong>Dense:</strong> Fully connected layer</p>
            <p><strong>Conv2D:</strong> Convolutional layer for 2D data</p>
            <p><strong>MaxPooling2D:</strong> Max pooling layer</p>
            <p><strong>Dropout:</strong> Dropout layer for regularization</p>
            <p><strong>Flatten:</strong> Flatten layer to convert 2D to 1D</p>
            ''', sanitize=False)
            ui.button('Close', on_click=dialog.close).classes('bg-orange-600 hover:bg-orange-500 text-white')
        dialog.open()
    
    def clear_layer_types(self):
        """Clear all layer types"""
        self.model.layer_types = []
        ui.notify('Layer types cleared', color='positive')
    
    def show_layer_method_selection(self):
        """Show layer method selection dialog"""
        ui.notify('Layer method selection functionality coming soon!', color='info')
    
    def show_activation_help(self):
        """Show help for activation functions"""
        with ui.dialog() as dialog, ui.card():
            ui.html('<h3>Activation Functions Help</h3>', sanitize=False)
            ui.html('''
            <p><strong>ReLU:</strong> Rectified Linear Unit - fast and efficient</p>
            <p><strong>Sigmoid:</strong> Output between 0 and 1</p>
            <p><strong>Tanh:</strong> Output between -1 and 1</p>
            <p><strong>Softmax:</strong> Used for multi-class classification output</p>
            <p><strong>Linear:</strong> No activation, output as-is</p>
            ''', sanitize=False)
            ui.button('Close', on_click=dialog.close).classes('bg-orange-600 hover:bg-orange-500 text-white')
        dialog.open()
    
    def clear_layer_functions(self):
        """Clear all layer functions"""
        self.model.layer_functions = []
        ui.notify('Layer functions cleared', color='positive')
    
    def add_layer(self):
        """Add a new layer to the network"""
        ui.notify('Add layer functionality coming soon!', color='info')
    
    def show_infra_help(self):
        """Show infrastructure help"""
        with ui.dialog() as dialog, ui.card():
            ui.html('<h3>Infrastructure Help</h3>', sanitize=False)
            ui.html('''
            <p><strong>Sequential:</strong> Process layers one after another</p>
            <p><strong>Parallel:</strong> Process layers in parallel where possible</p>
            <p><strong>Distributed:</strong> Distribute processing across multiple nodes</p>
            ''', sanitize=False)
            ui.button('Close', on_click=dialog.close).classes('bg-orange-600 hover:bg-orange-500 text-white')
        dialog.open()
    
    def show_distributed_help(self):
        """Show distributed computing help"""
        with ui.dialog() as dialog, ui.card():
            ui.html('<h3>Distributed Computing Help</h3>', sanitize=False)
            ui.html('''
            <p><strong>Federation:</strong> Federated learning across multiple devices</p>
            <p><strong>Data Parallel:</strong> Split data across multiple workers</p>
            <p><strong>Model Parallel:</strong> Split model across multiple workers</p>
            ''', sanitize=False)
            ui.button('Close', on_click=dialog.close).classes('bg-orange-600 hover:bg-orange-500 text-white')
        dialog.open()
    
    def auto_generate_token(self):
        """Auto-generate a unique token"""
        import uuid
        token = str(uuid.uuid4())[:8]
        self.model.unique_token = token
        ui.notify(f'Generated token: {token}', color='positive')
    
    def export_worker_config(self):
        """Export worker configuration to JSON"""
        try:
            config = self.build_worker_json()
            ui.download(
                content=json.dumps(config, indent=2),
                filename=f'worker_config_{self.model.unique_token or "default"}.json'
            )
            ui.notify('Configuration exported successfully!', color='positive')
        except Exception as e:
            ui.notify(f'Export failed: {str(e)}', color='negative')
    
    def load_worker_config(self):
        """Load worker configuration from JSON"""
        ui.notify('Load worker config functionality coming soon!', color='info')
    
    def reset_config(self):
        """Reset configuration to defaults"""
        self.model = WorkerModel()
        ui.notify('Configuration reset to defaults', color='positive')
    
    def show_add_layer_dialog(self):
        """Show dialog to add a new layer with comprehensive layer types"""
        with ui.dialog() as dialog, ui.card().classes('w-full max-w-4xl'):
            ui.html('<h3>Add New Layer</h3>', sanitize=False)
            
            # Use the proper display method for layer types
            layer_type_options = self.get_layer_type_options_for_display()
            print(f"DEBUG Add Layer Dialog: Using display options: {layer_type_options}")
            
            layer_type = ui.select(
                options=layer_type_options,
                label='Layer Type',
                value='3 - Perceptron'  # Default to Perceptron with display format
            ).classes('w-full')
            
            # Layer configuration container
            config_container = ui.column().classes('w-full gap-4 mt-4')
            
            def update_layer_config():
                """Update layer configuration fields based on selected type"""
                config_container.clear()
                selected_type_display = layer_type.value
                selected_type = self.extract_layer_type_name(selected_type_display)
                
                with config_container:
                    if selected_type == 'Conv':
                        # CNN Layer Configuration
                        ui.label('CNN Layer Configuration').classes('text-h6 font-bold')
                        ui.html('''
                        <p><strong>Format:</strong> &lt;W&gt;x&lt;H&gt;x&lt;D&gt;k&lt;K&gt;s&lt;S&gt;p&lt;P&gt;&lt;T&gt;</p>
                        <p><strong>Example:</strong> 28x28x1k5x5x1x8p0s1t1 (28x28 input, 5x5 kernel, 8 filters, padding 0, stride 1, type 1)</p>
                        ''', sanitize=False).classes('text-sm text-gray-600 mb-4')
                        
                        with ui.row().classes('w-full gap-4'):
                            with ui.column().classes('flex-1'):
                                ui.label('Input Dimensions').classes('font-bold')
                                width = ui.number('Width', value=28, min=1).classes('w-full')
                                height = ui.number('Height', value=28, min=1).classes('w-full')
                                depth = ui.number('Depth', value=1, min=1).classes('w-full')
                            
                            with ui.column().classes('flex-1'):
                                ui.label('Kernel Configuration').classes('font-bold')
                                kernel_w = ui.number('Kernel Width', value=5, min=1).classes('w-full')
                                kernel_h = ui.number('Kernel Height', value=5, min=1).classes('w-full')
                                kernel_d = ui.number('Kernel Depth', value=1, min=1).classes('w-full')
                                filters = ui.number('Filters', value=8, min=1).classes('w-full')
                            
                            with ui.column().classes('flex-1'):
                                ui.label('Layer Parameters').classes('font-bold')
                                padding = ui.number('Padding', value=0, min=0).classes('w-full')
                                stride = ui.number('Stride', value=1, min=1).classes('w-full')
                                conv_type = ui.select(['1', '0'], label='Conv Type (1=valid, 0=same)', value='1').classes('w-full')
                        
                        # Store references for CNN config
                        config_container.cnn_config = {
                            'width': width, 'height': height, 'depth': depth,
                            'kernel_w': kernel_w, 'kernel_h': kernel_h, 'kernel_d': kernel_d, 'filters': filters,
                            'padding': padding, 'stride': stride, 'conv_type': conv_type
                        }
                    
                    elif selected_type == 'Pooling':
                        # Pooling Layer Configuration
                        ui.label('Pooling Layer Configuration').classes('text-h6 font-bold')
                        ui.html('''
                        <p><strong>Format:</strong> &lt;N&gt;x&lt;N&gt;p&lt;P&gt;x&lt;P&gt;s&lt;S&gt; or &lt;N&gt;p&lt;P&gt;s&lt;S&gt;</p>
                        <p><strong>Examples:</strong> 8x8p2x2s1x1 or 10p2s1</p>
                        ''', sanitize=False).classes('text-sm text-gray-600 mb-4')
                        
                        with ui.row().classes('w-full gap-4'):
                            layer_width = ui.number('Layer Width', value=8, min=1).classes('w-full')
                            layer_height = ui.number('Layer Height (optional)', value=8, min=0).classes('w-full')
                            pool_width = ui.number('Pool Width', value=2, min=1).classes('w-full')
                            pool_height = ui.number('Pool Height', value=2, min=1).classes('w-full')
                            stride_x = ui.number('Stride X', value=1, min=1).classes('w-full')
                            stride_y = ui.number('Stride Y', value=1, min=1).classes('w-full')
                        
                        config_container.pooling_config = {
                            'layer_width': layer_width, 'layer_height': layer_height,
                            'pool_width': pool_width, 'pool_height': pool_height,
                            'stride_x': stride_x, 'stride_y': stride_y
                        }
                    
                    elif selected_type == 'Perceptron':
                        # Dense/Perceptron Layer
                        ui.label('Perceptron Layer Configuration').classes('text-h6 font-bold')
                        neurons = ui.number('Number of Neurons', value=64, min=1).classes('w-full')
                        config_container.perceptron_config = {'neurons': neurons}
                        
                    elif selected_type in ['Scaling', 'Unscaling']:
                        # Scaling Layer Configuration
                        ui.label(f'{selected_type} Layer Configuration').classes('text-h6 font-bold')
                        ui.html('<p><strong>Format:</strong> &lt;N&gt;x&lt;N&gt; or &lt;N&gt;</p>', sanitize=False).classes('text-sm text-gray-600 mb-4')
                        
                        with ui.row().classes('w-full gap-4'):
                            scale_width = ui.number('Width', value=100, min=1).classes('w-full')
                            scale_height = ui.number('Height (optional)', value=0, min=0).classes('w-full')
                        
                        config_container.scaling_config = {
                            'width': scale_width, 'height': scale_height
                        }
                    
                    else:
                        # Simple layer types (Flatten, Bounding, etc.)
                        ui.label(f'{selected_type} Layer Configuration').classes('text-h6 font-bold')
                        if selected_type == 'Flatten':
                            ui.label('Flatten layers don\'t require additional configuration').classes('text-gray-600')
                        elif selected_type == 'Bounding':
                            ui.label('Bounding layers use default configuration').classes('text-gray-600')
                        else:
                            size = ui.number('Layer Size', value=1, min=1).classes('w-full')
                            config_container.simple_config = {'size': size}
            
            # Initial config setup
            layer_type.on('update:model-value', lambda: update_layer_config())
            update_layer_config()
            
            def add_layer_to_config():
                """Add the configured layer to the worker model"""
                try:
                    selected_type_display = layer_type.value
                    selected_type_name = self.extract_layer_type_name(selected_type_display)
                    selected_type_id = self.extract_layer_type_id(selected_type_display)
                    layer_size_str = ""
                    
                    print(f"DEBUG Add Layer: display='{selected_type_display}', name='{selected_type_name}', id='{selected_type_id}'")
                    
                    if selected_type_name == 'Conv' and hasattr(config_container, 'cnn_config'):
                        cfg = config_container.cnn_config
                        layer_size_str = f"{cfg['width'].value}x{cfg['height'].value}x{cfg['depth'].value}k{cfg['kernel_w'].value}x{cfg['kernel_h'].value}x{cfg['kernel_d'].value}x{cfg['filters'].value}p{cfg['padding'].value}s{cfg['stride'].value}t{cfg['conv_type'].value}"
                    
                    elif selected_type_name == 'Pooling' and hasattr(config_container, 'pooling_config'):
                        cfg = config_container.pooling_config
                        if cfg['layer_height'].value > 0:
                            layer_size_str = f"{cfg['layer_width'].value}x{cfg['layer_height'].value}p{cfg['pool_width'].value}x{cfg['pool_height'].value}s{cfg['stride_x'].value}x{cfg['stride_y'].value}"
                        else:
                            layer_size_str = f"{cfg['layer_width'].value}p{cfg['pool_width'].value}s{cfg['stride_x'].value}"
                    
                    elif selected_type_name == 'Perceptron' and hasattr(config_container, 'perceptron_config'):
                        layer_size_str = str(config_container.perceptron_config['neurons'].value)
                    
                    elif selected_type_name in ['Scaling', 'Unscaling'] and hasattr(config_container, 'scaling_config'):
                        cfg = config_container.scaling_config
                        if cfg['height'].value > 0:
                            layer_size_str = f"{cfg['width'].value}x{cfg['height'].value}"
                        else:
                            layer_size_str = str(cfg['width'].value)
                    
                    elif selected_type_name == 'Flatten':
                        layer_size_str = "1"
                    
                    elif hasattr(config_container, 'simple_config'):
                        layer_size_str = str(config_container.simple_config['size'].value)
                    else:
                        layer_size_str = "1"
                    
                    # Add to worker model
                    if not hasattr(self.model, 'layer_sizes_raw'):
                        self.model.layer_sizes_raw = []
                    if not hasattr(self.model, 'layer_types_raw'):
                        self.model.layer_types_raw = []
                    if not hasattr(self.model, 'layer_functions_raw'):
                        self.model.layer_functions_raw = []
                    
                    self.model.layer_sizes_raw.append(layer_size_str)
                    self.model.layer_types_raw.append(selected_type_id)
                    
                    # Add default activation function based on layer type
                    if selected_type_name == 'Conv':
                        self.model.layer_functions_raw.append('6')  # ReLU
                    elif selected_type_name == 'Pooling':
                        self.model.layer_functions_raw.append('2')  # Max pooling
                    elif selected_type_name == 'Probabilistic':
                        self.model.layer_functions_raw.append('4')  # Softmax
                    elif selected_type_name == 'Scaling':
                        self.model.layer_functions_raw.append('2')  # MinMax
                    else:
                        self.model.layer_functions_raw.append('6')  # ReLU default
                    
                    # Update the configuration strings
                    if hasattr(self, 'layers_sizes_input'):
                        self.layers_sizes_input.value = ','.join(self.model.layer_sizes_raw)
                    if hasattr(self, 'layer_types_input'):
                        self.layer_types_input.value = ','.join(self.model.layer_types_raw)
                    if hasattr(self, 'layer_functions_input'):
                        self.layer_functions_input.value = ','.join(self.model.layer_functions_raw)
                    
                    # Update the current config
                    self.current_config['layersSizes'] = ','.join(self.model.layer_sizes_raw)
                    self.current_config['layerTypesList'] = ','.join(self.model.layer_types_raw)
                    self.current_config['layers_functions'] = ','.join(self.model.layer_functions_raw)
                    
                    # Update counters and render network
                    self.update_layer_sizes_count()
                    self.update_layer_type_count()
                    self.update_layer_function_count()
                    self.render_network_diagram()
                    
                    ui.notify(f'Added {selected_type_name} layer: {layer_size_str}', color='positive')
                    dialog.close()
                    
                except Exception as e:
                    ui.notify(f'Error adding layer: {str(e)}', color='negative')
            
            # Dialog buttons
            with ui.row().classes('w-full justify-end gap-2 mt-4'):
                ui.button('Add Layer', on_click=add_layer_to_config).classes('bg-orange-700 hover:bg-orange-600 text-white')
                ui.button('Cancel', on_click=dialog.close).classes('bg-gray-600 hover:bg-gray-500 text-white')
        
        dialog.open()
    
    def auto_layout_network(self):
        """Auto-layout the network visualization"""
        ui.notify('Auto-layout network functionality coming soon!', color='info')
        # This would implement automatic positioning of network nodes
    
    def zoom_to_fit(self):
        """Zoom to fit the entire network"""
        ui.notify('Zoom to fit functionality coming soon!', color='info')
        # This would adjust the zoom level to show the entire network
    
    def get_layer_type_name(self, type_code: str) -> str:
        """Get human-readable layer type name from code"""
        type_map = {
            '0': 'Default', '1': 'Scaling', '2': 'Conv', '3': 'Perceptron',
            '4': 'Pooling', '5': 'Probabilistic', '6': 'LSTM', '7': 'Recurrent',
            '8': 'Unscaling', '9': 'Flatten', '10': 'Bounding'
        }
        return type_map.get(type_code, f'Type {type_code}')
    
    def get_activation_name(self, activation_code: str) -> str:
        """Get human-readable activation function name from code"""
        activation_map = {
            '1': 'Threshold', '2': 'Sign', '3': 'Logistic', '4': 'Tanh',
            '5': 'Linear', '6': 'ReLU', '7': 'eLU', '8': 'SeLU',
            '9': 'Soft-plus', '10': 'Soft-sign', '11': 'Hard-sigmoid'
        }
        return activation_map.get(activation_code, f'Function {activation_code}')
    
    def edit_layer_simple(self, layer_index: int):
        """Edit a specific layer (simple notification)"""
        ui.notify(f'Edit layer {layer_index + 1} functionality coming soon!', color='info')
    
    def remove_layer(self, layer_index: int):
        """Remove a specific layer"""
        try:
            if (hasattr(self.model, 'layer_sizes_raw') and 
                layer_index < len(self.model.layer_sizes_raw)):
                
                # Remove from all arrays
                self.model.layer_sizes_raw.pop(layer_index)
                if hasattr(self.model, 'layer_types_raw') and layer_index < len(self.model.layer_types_raw):
                    self.model.layer_types_raw.pop(layer_index)
                if hasattr(self.model, 'layer_functions_raw') and layer_index < len(self.model.layer_functions_raw):
                    self.model.layer_functions_raw.pop(layer_index)
                
                # Update UI
                if hasattr(self, 'layers_sizes_input'):
                    self.layers_sizes_input.value = ','.join(self.model.layer_sizes_raw)
                if hasattr(self, 'layer_types_input'):
                    self.layer_types_input.value = ','.join(self.model.layer_types_raw)
                if hasattr(self, 'layer_functions_input'):
                    self.layer_functions_input.value = ','.join(self.model.layer_functions_raw)
                
                # Update config and render
                self.current_config['layersSizes'] = ','.join(self.model.layer_sizes_raw)
                self.current_config['layerTypesList'] = ','.join(self.model.layer_types_raw)
                self.current_config['layers_functions'] = ','.join(self.model.layer_functions_raw)
                
                self.update_layer_sizes_count()
                self.update_layer_type_count()
                self.update_layer_function_count()
                self.render_network_diagram()
                
                ui.notify(f'Removed layer {layer_index + 1}', color='positive')
        except Exception as e:
            ui.notify(f'Error removing layer: {str(e)}', color='negative')
    
    def get_layer_type_name_by_code(self, type_code: str) -> str:
        """Get layer type name from code"""
        reverse_map = {v: k for k, v in self.get_layer_type_options().items()}
        return reverse_map.get(type_code, 'Default')
    
    def get_display_key_for_layer_type(self, type_code: str) -> str:
        """Get display key for layer type code"""
        display_reverse_map = {v: k for k, v in self.get_layer_type_options_for_display().items()}
        return display_reverse_map.get(type_code, '0 - Default')
    
    def get_activation_name_by_code(self, activation_code: str) -> str:
        """Get activation function name from code for UI selection"""
        reverse_map = {v: k for k, v in self.get_activation_functions().items()}
        return reverse_map.get(activation_code, 'ReLU')
    
    def save_layer_edit(self, layer_index, layer_type_select, layer_size_input, activation_select, dialog):
        """Save layer edits to the model"""
        try:
            # Get new values
            new_type_code = self.get_layer_type_options_for_display()[layer_type_select.value]
            new_size = layer_size_input.value.strip()
            new_activation_code = self.get_activation_functions()[activation_select.value]
            
            # Update model data
            if layer_index < len(self.model.layer_types_raw):
                self.model.layer_types_raw[layer_index] = new_type_code
            if layer_index < len(self.model.layer_sizes_raw):
                self.model.layer_sizes_raw[layer_index] = new_size
            if layer_index < len(self.model.layer_functions_raw):
                self.model.layer_functions_raw[layer_index] = new_activation_code
            
            # Update UI elements
            if hasattr(self, 'layers_sizes_input'):
                self.layers_sizes_input.value = ','.join(self.model.layer_sizes_raw)
            if hasattr(self, 'layer_types_input'):
                self.layer_types_input.value = ','.join(self.model.layer_types_raw)
            if hasattr(self, 'layer_functions_input'):
                self.layer_functions_input.value = ','.join(self.model.layer_functions_raw)
            
            # Update config
            self.current_config['layersSizes'] = ','.join(self.model.layer_sizes_raw)
            self.current_config['layerTypesList'] = ','.join(self.model.layer_types_raw)
            self.current_config['layers_functions'] = ','.join(self.model.layer_functions_raw)
            
            # Refresh network diagram
            self.render_network_diagram()
            
            ui.notify(f'Layer {layer_index + 1} updated successfully', type='positive')
            dialog.close()
            
        except Exception as e:
            ui.notify(f'Error saving layer: {str(e)}', type='negative')
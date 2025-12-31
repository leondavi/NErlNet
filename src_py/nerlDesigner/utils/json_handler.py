"""
JSON Handler - Utility for handling NerlNet JSON configurations
"""

import json
from pathlib import Path
from typing import Dict, Any, List, Optional, Union
import logging

logger = logging.getLogger(__name__)

class JsonHandler:
    """Utility class for handling NerlNet JSON configurations"""
    
    @staticmethod
    def load_json(file_path: Union[str, Path]) -> Dict[str, Any]:
        """Load JSON data from a file"""
        file_path = Path(file_path)
        
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
        
        if not file_path.suffix.lower() == '.json':
            raise ValueError(f"File must be a JSON file: {file_path}")
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)
            logger.info(f"Successfully loaded JSON from {file_path}")
            return data
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON format in {file_path}: {str(e)}")
        except Exception as e:
            raise IOError(f"Error reading file {file_path}: {str(e)}")
    
    @staticmethod
    def save_json(data: Dict[str, Any], file_path: Union[str, Path], indent: int = 2) -> None:
        """Save JSON data to a file"""
        file_path = Path(file_path)
        
        # Create directory if it doesn't exist
        file_path.parent.mkdir(parents=True, exist_ok=True)
        
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=indent, ensure_ascii=False)
            logger.info(f"Successfully saved JSON to {file_path}")
        except Exception as e:
            raise IOError(f"Error writing file {file_path}: {str(e)}")
    
    @staticmethod
    def validate_worker_json(data: Dict[str, Any]) -> List[str]:
        """Validate worker JSON structure and return list of issues"""
        issues = []
        
        # Required fields for worker configuration
        required_fields = [
            'modelType', 'layersSizes', 'layerTypesList', 'layers_functions',
            'lossMethod', 'lr', 'epochs', 'optimizer', 'infraType',
            'distributedSystemType'
        ]
        
        for field in required_fields:
            if field not in data:
                issues.append(f"Missing required field: {field}")
        
        # Validate specific field formats
        if 'layersSizes' in data:
            try:
                sizes = [int(x.strip()) for x in data['layersSizes'].split(',')]
                if any(s <= 0 for s in sizes):
                    issues.append("All layer sizes must be positive integers")
            except ValueError:
                issues.append("Invalid layersSizes format - should be comma-separated integers")
        
        if 'lr' in data:
            try:
                lr = float(data['lr'])
                if lr <= 0:
                    issues.append("Learning rate must be positive")
            except ValueError:
                issues.append("Learning rate must be a number")
        
        if 'epochs' in data:
            try:
                epochs = int(data['epochs'])
                if epochs <= 0:
                    issues.append("Epochs must be positive integer")
            except ValueError:
                issues.append("Epochs must be an integer")
        
        return issues
    
    @staticmethod
    def validate_connection_json(data: Dict[str, Any]) -> List[str]:
        """Validate connection map JSON structure and return list of issues"""
        issues = []
        
        if 'devices' not in data:
            issues.append("Missing 'devices' field")
        elif not isinstance(data['devices'], list):
            issues.append("'devices' must be a list")
        else:
            for i, device in enumerate(data['devices']):
                if not isinstance(device, dict):
                    issues.append(f"Device {i} must be an object")
                    continue
                
                required_device_fields = ['name', 'ipv4', 'entities']
                for field in required_device_fields:
                    if field not in device:
                        issues.append(f"Device {i} missing required field: {field}")
        
        if 'connections' in data:
            if not isinstance(data['connections'], list):
                issues.append("'connections' must be a list")
            else:
                for i, connection in enumerate(data['connections']):
                    if not isinstance(connection, dict):
                        issues.append(f"Connection {i} must be an object")
                        continue
                    
                    if 'from' not in connection or 'to' not in connection:
                        issues.append(f"Connection {i} missing 'from' or 'to' field")
        
        return issues
    
    @staticmethod
    def validate_distributed_config_json(data: Dict[str, Any]) -> List[str]:
        """Validate distributed config JSON structure and return list of issues"""
        issues = []
        
        # Check main structure
        if 'nerlnetSettings' not in data:
            issues.append("Missing 'nerlnetSettings' field")
        elif isinstance(data['nerlnetSettings'], dict):
            settings = data['nerlnetSettings']
            if 'frequency' not in settings:
                issues.append("Missing 'frequency' in nerlnetSettings")
            if 'batchSize' not in settings:
                issues.append("Missing 'batchSize' in nerlnetSettings")
        
        if 'devices' not in data:
            issues.append("Missing 'devices' field")
        elif not isinstance(data['devices'], list):
            issues.append("'devices' must be a list")
        
        # Validate server configurations
        for server_type in ['mainServer', 'apiServer']:
            if server_type in data:
                server = data[server_type]
                if not isinstance(server, dict):
                    issues.append(f"'{server_type}' must be an object")
                elif 'port' not in server:
                    issues.append(f"'{server_type}' missing 'port' field")
        
        return issues
    
    @staticmethod
    def detect_json_type(data: Dict[str, Any]) -> Optional[str]:
        """Detect the type of NerlNet JSON configuration"""
        if 'modelType' in data and 'layersSizes' in data:
            return 'worker'
        elif 'nerlnetSettings' in data and 'devices' in data:
            return 'distributed_config'
        elif 'devices' in data and 'connections' in data:
            return 'connection_map'
        elif 'experiment' in data or 'experimentName' in data:
            return 'experiment'
        else:
            return None
    
    @staticmethod
    def format_json_string(data: Dict[str, Any], compact: bool = False) -> str:
        """Format JSON data as a string"""
        if compact:
            return json.dumps(data, separators=(',', ':'))
        else:
            return json.dumps(data, indent=2, ensure_ascii=False)
    
    @staticmethod
    def merge_configurations(base_config: Dict[str, Any], 
                           override_config: Dict[str, Any]) -> Dict[str, Any]:
        """Merge two configuration dictionaries"""
        merged = base_config.copy()
        
        def deep_merge(base_dict: Dict[str, Any], override_dict: Dict[str, Any]) -> Dict[str, Any]:
            for key, value in override_dict.items():
                if (key in base_dict and 
                    isinstance(base_dict[key], dict) and 
                    isinstance(value, dict)):
                    base_dict[key] = deep_merge(base_dict[key], value)
                else:
                    base_dict[key] = value
            return base_dict
        
        return deep_merge(merged, override_config)
    
    @staticmethod
    def extract_entities_from_devices(devices: List[Dict[str, Any]]) -> List[str]:
        """Extract all entity names from device configurations"""
        entities = []
        for device in devices:
            if 'entities' in device:
                device_entities = device['entities'].split(',')
                entities.extend([e.strip() for e in device_entities if e.strip()])
        return list(set(entities))  # Remove duplicates
    
    @staticmethod
    def generate_sample_worker_config() -> Dict[str, Any]:
        """Generate a sample worker configuration"""
        return {
            "modelType": "0",
            "_doc_modelType": " nn:0 | approximation:1 | classification:2 | forecasting:3 | image-classification:4 | text-classification:5 | text-generation:6 | auto-association:7 | autoencoder:8 | ae-classifier:9 |",
            "layersSizes": "100,80,60,40,20,1",
            "_doc_layersSizes": "List of positive integers [L0, L1, ..., LN]",
            "layerTypesList": "1,5,5,5,5,1",
            "_doc_LayerTypes": " Default:0 | Scaling:1 | CNN:2 | Perceptron:3 | Pooling:4 | Probabilistic:5 | LSTM:6 | Recurrent:7 | Unscaling:8 | Bounding:9 |",
            "layers_functions": "1,6,6,6,6,1",
            "_doc_layers_functions_activation": " Threshold:1 | Sign:2 | Logistic:3 | Tanh:4 | Linear:5 | ReLU:6 | eLU:7 | SeLU:8 | Soft-plus:9 | Soft-sign:10 | Hard-sigmoid:11 |",
            "lossMethod": "2",
            "_doc_lossMethod": " SSE:1 | MSE:2 | NSE:3 | MinkowskiE:4 | WSE:5 | CEE:6 |",
            "lossArgs:": "",
            "_doc_lossArgs": "Arguments to loss function. Regularization: reg=L2, reg=L1, reg=NoRegularization (can be also empty)",
            "lr": "0.01",
            "_doc_lr": "Positive float",
            "epochs": "100",
            "_doc_epochs": "Positive Integer",
            "optimizer": "2",
            "_doc_optimizer": " GD:0 | CGD:1 | SGD:2 | QuasiNeuton:3 | LVM:4 | ADAM:5 |",
            "optimizerArgs": "",
            "_doc_optimizerArgs": "String",
            "infraType": "0",
            "_doc_infraType": " opennn:0 | wolfengine:1 |",
            "distributedSystemType": "0",
            "_doc_distributedSystemType": " none:0 | fedClientAvg:1 | fedServerAvg:2 |",
            "distributedSystemArgs": "none",
            "_doc_distributedSystemArgs": "String"
        }
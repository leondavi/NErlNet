"""
Worker Model - Data model for neural network worker configuration
"""

from typing import Dict, List, Optional, Any
from pydantic import BaseModel, Field, validator
from enum import Enum

class ModelType(Enum):
    NEURAL_NETWORK = "0"
    APPROXIMATION = "1" 
    CLASSIFICATION = "2"
    FORECASTING = "3"
    IMAGE_CLASSIFICATION = "4"
    TEXT_CLASSIFICATION = "5"
    TEXT_GENERATION = "6"
    AUTO_ASSOCIATION = "7"
    AUTOENCODER = "8"
    AE_CLASSIFIER = "9"

class LayerType(Enum):
    DEFAULT = "0"
    SCALING = "1"
    CNN = "2"
    PERCEPTRON = "3"
    POOLING = "4"
    PROBABILISTIC = "5"
    LSTM = "6"
    RECURRENT = "7"
    UNSCALING = "8"
    FLATTEN = "9"
    BOUNDING = "10"

class ActivationFunction(Enum):
    THRESHOLD = "1"
    SIGN = "2"
    LOGISTIC = "3"
    TANH = "4"
    LINEAR = "5"
    RELU = "6"
    ELU = "7"
    SELU = "8"
    SOFT_PLUS = "9"
    SOFT_SIGN = "10"
    HARD_SIGMOID = "11"

class PoolingMethod(Enum):
    NONE = "1"
    MAX = "2"
    AVG = "3"

class ScalingMethod(Enum):
    NONE = "1"
    MINMAX = "2"
    MEANSTD = "3"
    STD = "4"
    LOG = "5"

class ProbabilisticFunction(Enum):
    BINARY = "1"
    LOGISTIC = "2"
    COMPETITIVE = "3"
    SOFTMAX = "4"

class BoundingMethod(Enum):
    NONE = "1"
    BOUNDING = "2"

class FlattenMethod(Enum):
    FLATTEN = "0"

class LossMethod(Enum):
    SSE = "1"  # Sum Squared Error
    MSE = "2"  # Mean Squared Error
    NSE = "3"  # Normalized Squared Error
    MINKOWSKI = "4"  # Minkowski Error
    WSE = "5"  # Weighted Squared Error
    CEE = "6"  # Cross Entropy Error

class OptimizerType(Enum):
    GD = "0"          # Gradient Descent
    CGD = "1"         # Conjugate Gradient Descent
    SGD = "2"         # Stochastic Gradient Descent
    QUASI_NEWTON = "3" # Quasi-Newton
    LVM = "4"         # Levenberg-Marquardt
    ADAM = "5"        # Adam

class InfraType(Enum):
    OPENNN = "0"
    WOLFRAM = "1"

class DistributedSystemType(Enum):
    NONE = "0"
    FED_CLIENT_AVG = "1"
    FED_SERVER_AVG = "2"

class WorkerModel(BaseModel):
    """Pydantic model for worker configuration"""
    
    # Model Configuration
    model_type: ModelType = Field(default=ModelType.NEURAL_NETWORK, description="Type of neural network model")
    model_args: str = Field(default="", description="Additional model arguments")
    layer_sizes: List[int] = Field(default=[100, 80, 60, 40, 20, 1], description="Number of neurons in each layer")
    layer_types: List[LayerType] = Field(default=[], description="Type of each layer")
    layer_functions: List[ActivationFunction] = Field(default=[], description="Activation function for each layer")
    
    # Raw layer data for complex formats (CNN, Pooling, etc.)
    layer_sizes_raw: List[str] = Field(default=[], description="Raw layer size definitions including CNN format")
    layer_types_raw: List[str] = Field(default=[], description="Raw layer type codes")
    layer_functions_raw: List[str] = Field(default=[], description="Raw layer function codes")
    
    # Training Configuration
    loss_method: LossMethod = Field(default=LossMethod.MSE, description="Loss function")
    loss_args: str = Field(default="", description="Additional loss function arguments")
    learning_rate: float = Field(default=0.01, gt=0, description="Learning rate")
    epochs: int = Field(default=1, gt=0, description="Number of training epochs")
    
    # Optimizer Configuration
    optimizer: OptimizerType = Field(default=OptimizerType.SGD, description="Optimization algorithm")
    optimizer_args: str = Field(default="", description="Additional optimizer arguments")
    
    # Infrastructure Configuration
    infra_type: InfraType = Field(default=InfraType.OPENNN, description="Neural network infrastructure")
    
    # Distributed System Configuration
    distributed_system_type: DistributedSystemType = Field(default=DistributedSystemType.NONE)
    distributed_system_token: str = Field(default="", description="Distributed system token")
    distributed_system_args: str = Field(default="none", description="Distributed system arguments")
    
    class Config:
        use_enum_values = True
        
    @validator('layer_types', always=True)
    def validate_layer_types(cls, v, values):
        """Ensure layer_types matches layer_sizes length"""
        if 'layer_sizes' in values:
            layer_count = len(values['layer_sizes'])
            if not v:
                # Default to scaling, perceptron layers, probabilistic
                return [LayerType.SCALING] + [LayerType.PROBABILISTIC] * (layer_count - 2) + [LayerType.SCALING]
            elif len(v) != layer_count:
                raise ValueError(f"layer_types length ({len(v)}) must match layer_sizes length ({layer_count})")
        return v
    
    @validator('layer_functions', always=True)
    def validate_layer_functions(cls, v, values):
        """Ensure layer_functions matches layer_sizes length"""
        if 'layer_sizes' in values:
            layer_count = len(values['layer_sizes'])
            if not v:
                # Default to linear, relu functions, linear
                return [ActivationFunction.LINEAR] + [ActivationFunction.RELU] * (layer_count - 2) + [ActivationFunction.LINEAR]
            elif len(v) != layer_count:
                raise ValueError(f"layer_functions length ({len(v)}) must match layer_sizes length ({layer_count})")
        return v
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to NerlNet JSON format"""
        return {
            "modelType": self.model_type.value,
            "_doc_modelType": " nn:0 | approximation:1 | classification:2 | forecasting:3 | image-classification:4 | text-classification:5 | text-generation:6 | auto-association:7 | autoencoder:8 | ae-classifier:9 |",
            "layersSizes": ",".join(map(str, self.layer_sizes)),
            "_doc_layersSizes": "List of positive integers [L0, L1, ..., LN]",
            "layerTypesList": ",".join([lt.value for lt in self.layer_types]),
            "_doc_LayerTypes": " Default:0 | Scaling:1 | CNN:2 | Perceptron:3 | Pooling:4 | Probabilistic:5 | LSTM:6 | Recurrent:7 | Unscaling:8 | Bounding:9 |",
            "layers_functions": ",".join([lf.value for lf in self.layer_functions]),
            "_doc_layers_functions_activation": " Threshold:1 | Sign:2 | Logistic:3 | Tanh:4 | Linear:5 | ReLU:6 | eLU:7 | SeLU:8 | Soft-plus:9 | Soft-sign:10 | Hard-sigmoid:11 |",
            "_doc_layer_functions_pooling": " none:1 | Max:2 | Avg:3 |",
            "_doc_layer_functions_probabilistic": " Binary:1 | Logistic:2 | Competitive:3 | Softmax:4 |",
            "_doc_layer_functions_scaler": " none:1 | MinMax:2 | MeanStd:3 | STD:4 | Log:5 |",
            "lossMethod": self.loss_method.value,
            "_doc_lossMethod": " SSE:1 | MSE:2 | NSE:3 | MinkowskiE:4 | WSE:5 | CEE:6 |",
            "lossArgs:": self.loss_args,
            "_doc_lossArgs": "Arguments to loss function. Regularization: reg=L2, reg=L1, reg=NoRegularization (can be also empty)",
            "lr": str(self.learning_rate),
            "_doc_lr": "Positive float",
            "epochs": str(self.epochs),
            "_doc_epochs": "Positive Integer",
            "optimizer": self.optimizer.value,
            "_doc_optimizer": " GD:0 | CGD:1 | SGD:2 | QuasiNeuton:3 | LVM:4 | ADAM:5 |",
            "optimizerArgs": self.optimizer_args,
            "_doc_optimizerArgs": "String",
            "infraType": self.infra_type.value,
            "_doc_infraType": " opennn:0 | wolfengine:1 |",
            "distributedSystemType": self.distributed_system_type.value,
            "_doc_distributedSystemType": " none:0 | fedClientAvg:1 | fedServerAvg:2 |",
            "distributedSystemArgs": self.distributed_system_args,
            "_doc_distributedSystemArgs": "String",
        }
    
    def from_dict(self, data: Dict[str, Any]):
        """Load from NerlNet JSON format"""
        if "modelType" in data:
            self.model_type = ModelType(data["modelType"])
        if "layersSizes" in data:
            self.layer_sizes = [int(x.strip()) for x in data["layersSizes"].split(",")]
        if "layerTypesList" in data:
            self.layer_types = [LayerType(x.strip()) for x in data["layerTypesList"].split(",")]
        if "layers_functions" in data:
            self.layer_functions = [ActivationFunction(x.strip()) for x in data["layers_functions"].split(",")]
        if "lossMethod" in data:
            self.loss_method = LossMethod(data["lossMethod"])
        if "lossArgs:" in data:
            self.loss_args = data["lossArgs:"]
        if "lr" in data:
            self.learning_rate = float(data["lr"])
        if "epochs" in data:
            self.epochs = int(data["epochs"])
        if "optimizer" in data:
            self.optimizer = OptimizerType(data["optimizer"])
        if "optimizerArgs" in data:
            self.optimizer_args = data["optimizerArgs"]
        if "infraType" in data:
            self.infra_type = InfraType(data["infraType"])
        if "distributedSystemType" in data:
            self.distributed_system_type = DistributedSystemType(data["distributedSystemType"])
        if "distributedSystemArgs" in data:
            self.distributed_system_args = data["distributedSystemArgs"]
    
    def reset(self):
        """Reset to default values"""
        self.__init__()
"""
Distributed Config Model - Data model for distributed system configuration
"""

from typing import Dict, List, Optional, Any
from pydantic import BaseModel, Field

class NerlnetSettings(BaseModel):
    """NerlNet system settings"""
    frequency: str = Field(default="1", description="System frequency")
    batch_size: str = Field(default="50", description="Batch size for processing")
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "frequency": self.frequency,
            "batchSize": self.batch_size
        }
    
    def from_dict(self, data: Dict[str, Any]):
        self.frequency = data.get("frequency", "1")
        self.batch_size = data.get("batchSize", "50")

class ServerConfig(BaseModel):
    """Server configuration"""
    port: str = Field(default="8080", description="Server port")
    args: str = Field(default="", description="Server arguments")
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "port": self.port,
            "args": self.args
        }
    
    def from_dict(self, data: Dict[str, Any]):
        self.port = data.get("port", "8080")
        self.args = data.get("args", "")

class DistributedDevice(BaseModel):
    """Device in distributed system"""
    name: str = Field(description="Device name")
    ipv4: str = Field(description="IPv4 address")
    entities: str = Field(description="Comma-separated list of entities")
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "ipv4": self.ipv4,
            "entities": self.entities
        }
    
    def from_dict(self, data: Dict[str, Any]):
        self.name = data.get("name", "")
        self.ipv4 = data.get("ipv4", "")
        self.entities = data.get("entities", "")

class WorkerDefinition(BaseModel):
    """Worker definition in distributed config"""
    worker_id: str = Field(description="Worker identifier")
    worker_file: str = Field(description="Path to worker configuration file")
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "workerId": self.worker_id,
            "workerFile": self.worker_file
        }
    
    def from_dict(self, data: Dict[str, Any]):
        self.worker_id = data.get("workerId", "")
        self.worker_file = data.get("workerFile", "")

class DistributedConfigModel(BaseModel):
    """Model for distributed system configuration"""
    
    nerlnet_settings: NerlnetSettings = Field(default_factory=NerlnetSettings)
    main_server: ServerConfig = Field(default_factory=lambda: ServerConfig(port="8080"))
    api_server: ServerConfig = Field(default_factory=lambda: ServerConfig(port="8081"))
    devices: List[DistributedDevice] = Field(default=[])
    workers: List[WorkerDefinition] = Field(default=[])
    sources: List[Dict[str, Any]] = Field(default=[])
    connections_file: str = Field(default="", description="Path to connections file")
    
    def add_device(self, name: str, ipv4: str, entities: str):
        """Add a new device"""
        device = DistributedDevice(name=name, ipv4=ipv4, entities=entities)
        self.devices.append(device)
    
    def remove_device(self, name: str):
        """Remove a device by name"""
        self.devices = [d for d in self.devices if d.name != name]
    
    def add_worker(self, worker_id: str, worker_file: str):
        """Add a worker definition"""
        worker = WorkerDefinition(worker_id=worker_id, worker_file=worker_file)
        self.workers.append(worker)
    
    def remove_worker(self, worker_id: str):
        """Remove a worker by ID"""
        self.workers = [w for w in self.workers if w.worker_id != worker_id]
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to distributed config JSON format"""
        result = {
            "nerlnetSettings": self.nerlnet_settings.to_dict(),
            "mainServer": self.main_server.to_dict(),
            "apiServer": self.api_server.to_dict(),
            "devices": [device.to_dict() for device in self.devices]
        }
        
        if self.workers:
            result["workers"] = [worker.to_dict() for worker in self.workers]
        
        if self.sources:
            result["sources"] = self.sources
        
        if self.connections_file:
            result["connectionsFile"] = self.connections_file
        
        return result
    
    def from_dict(self, data: Dict[str, Any]):
        """Load from distributed config JSON format"""
        if "nerlnetSettings" in data:
            self.nerlnet_settings = NerlnetSettings()
            self.nerlnet_settings.from_dict(data["nerlnetSettings"])
        
        if "mainServer" in data:
            self.main_server = ServerConfig()
            self.main_server.from_dict(data["mainServer"])
        
        if "apiServer" in data:
            self.api_server = ServerConfig()
            self.api_server.from_dict(data["apiServer"])
        
        self.devices = []
        if "devices" in data:
            for device_data in data["devices"]:
                device = DistributedDevice(name="", ipv4="", entities="")
                device.from_dict(device_data)
                self.devices.append(device)
        
        self.workers = []
        if "workers" in data:
            for worker_data in data["workers"]:
                worker = WorkerDefinition(worker_id="", worker_file="")
                worker.from_dict(worker_data)
                self.workers.append(worker)
        
        self.sources = data.get("sources", [])
        self.connections_file = data.get("connectionsFile", "")
    
    def reset(self):
        """Reset to default values"""
        self.nerlnet_settings = NerlnetSettings()
        self.main_server = ServerConfig(port="8080")
        self.api_server = ServerConfig(port="8081")
        self.devices = []
        self.workers = []
        self.sources = []
        self.connections_file = ""
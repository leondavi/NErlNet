"""
Connection Model - Data model for network connection maps
"""

from typing import Dict, List, Optional, Any
from pydantic import BaseModel, Field

class Device(BaseModel):
    """Represents a network device"""
    name: str = Field(description="Device name")
    ipv4: str = Field(description="IPv4 address")
    entities: List[str] = Field(default=[], description="List of entities on this device")
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "ipv4": self.ipv4,
            "entities": ",".join(self.entities)
        }
    
    def from_dict(self, data: Dict[str, Any]):
        self.name = data.get("name", "")
        self.ipv4 = data.get("ipv4", "")
        if "entities" in data:
            self.entities = [e.strip() for e in data["entities"].split(",") if e.strip()]

class Connection(BaseModel):
    """Represents a connection between entities"""
    from_entity: str = Field(description="Source entity")
    to_entity: str = Field(description="Destination entity")
    connection_type: str = Field(default="data", description="Type of connection")
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "from": self.from_entity,
            "to": self.to_entity,
            "type": self.connection_type
        }
    
    def from_dict(self, data: Dict[str, Any]):
        self.from_entity = data.get("from", "")
        self.to_entity = data.get("to", "")
        self.connection_type = data.get("type", "data")

class ConnectionModel(BaseModel):
    """Model for managing network connections and topology"""
    
    devices: List[Device] = Field(default=[], description="List of network devices")
    connections: List[Connection] = Field(default=[], description="List of connections")
    
    def add_device(self, name: str, ipv4: str, entities: List[str] = None):
        """Add a new device"""
        device = Device(name=name, ipv4=ipv4, entities=entities or [])
        self.devices.append(device)
    
    def remove_device(self, name: str):
        """Remove a device by name"""
        self.devices = [d for d in self.devices if d.name != name]
    
    def add_connection(self, from_entity: str, to_entity: str, connection_type: str = "data"):
        """Add a new connection"""
        connection = Connection(from_entity=from_entity, to_entity=to_entity, connection_type=connection_type)
        self.connections.append(connection)
    
    def remove_connection(self, from_entity: str, to_entity: str):
        """Remove a connection"""
        self.connections = [c for c in self.connections if not (c.from_entity == from_entity and c.to_entity == to_entity)]
    
    def get_all_entities(self) -> List[str]:
        """Get all entities across all devices"""
        entities = []
        for device in self.devices:
            entities.extend(device.entities)
        return entities
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to connection map JSON format"""
        return {
            "devices": [device.to_dict() for device in self.devices],
            "connections": [connection.to_dict() for connection in self.connections]
        }
    
    def from_dict(self, data: Dict[str, Any]):
        """Load from connection map JSON format"""
        self.devices = []
        self.connections = []
        
        print(f"DEBUG: ConnectionModel.from_dict called with keys: {list(data.keys())}")
        
        # Handle standard format with devices and connections arrays
        if "devices" in data:
            for device_data in data["devices"]:
                device = Device(name="", ipv4="")
                device.from_dict(device_data)
                self.devices.append(device)
        
        if "connections" in data:
            for conn_data in data["connections"]:
                connection = Connection(from_entity="", to_entity="")
                connection.from_dict(conn_data)
                self.connections.append(connection)
        
        # Handle connectionsMap format
        if "connectionsMap" in data:
            print(f"DEBUG: Processing connectionsMap format")
            connections_map = data["connectionsMap"]
            print(f"DEBUG: connectionsMap type: {type(connections_map)}, value: {connections_map}")
            
            # Convert connectionsMap to connections list
            if isinstance(connections_map, dict):
                for from_entity, to_entities in connections_map.items():
                    print(f"DEBUG: Processing {from_entity} -> {to_entities}, type: {type(to_entities)}")
                    if isinstance(to_entities, list):
                        for to_entity in to_entities:
                            connection = Connection(from_entity=from_entity, to_entity=to_entity)
                            self.connections.append(connection)
                    else:
                        print(f"DEBUG: Warning - to_entities is not a list: {type(to_entities)}")
            else:
                print(f"DEBUG: Warning - connectionsMap is not a dict: {type(connections_map)}")
    
    def reset(self):
        """Reset to default values"""
        self.devices = []
        self.connections = []
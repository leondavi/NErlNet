# Nerlnet Planner
Nerlplanner is a NErlNEt tool that assist users generating json files that of a distributed Nerlnet network.
There are 3 kinds of json files: 
- Distributed Configuration json file (DC json file with prefix dc e.g., dc_\<any name\>.json)
- Communication network. (json file with prefix comm e.g., comm_\<any name|>.json)
- Experiment. (json file with prefix exp e.g., exp_\<any name\>.json)
A distributed nerlnet network consists of different entities and their connectivity.  
### Entities
Entities are Routers, Sources and Clients:  
- Router allows the connection between entities (Router<->Router, Router<->Sources, Router<->Client).
- Source streams data through Nerlnet network toward edge workers.
- Client is a communication envelope that hosts workers.

### Special Entities
Main Server is a special entities that controls a Nerlnet cluster.
It creates and initializes entities based on loaded json files.
API Server is a controling API allow user to control the Nerlnet cluster through  
a pythonic API. It communicates with the main server through HTTP protocol.  

### Workers
Workers are ML models FSM. FSM of worker has mainly train and predict states. 
A worker has an ML model that perform train/predict according to its FSM state. 
It receives data from sources through its client.

### Devices
A device hosts entities. Each device should have a unique ip address.  
A single device can be defined and it can host alone all entities under its memory/cpu constraints.

## Installation steps:  

1. ```sudo apt install python3-pydot graphviz```  
2. Use virtualenv to create a new pythong environment.
3. Install requirements.txt
![Screenshot 2023-09-03 at 15 11 10](https://github.com/leondavi/NErlNet/assets/18975070/de31d840-7151-4a51-93ff-37f52442a687)
![Screenshot 2023-09-03 at 15 10 47](https://github.com/leondavi/NErlNet/assets/18975070/c81811e8-b3f4-448d-9ea0-3855074cc549)

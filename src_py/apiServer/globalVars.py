import multiprocessing 
import socket
import os
from networkComponents import NetworkComponents
import json
from IPython import get_ipython
from experiment import *
from csvResult import *
from workerResult import *

localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

pendingAcks = 0

multiProcQueue = multiprocessing.Queue() # Create an instance of the queue

# Get the components of the current system:
ARCHITECTURE_INDEX = 4
GRAPH_INDEX = 5


username = os.getlogin()
#jsonPathLocation = '/home/{}/workspace/NErlNet/jsonPath'.format(username) # Use this if NerlnetInstall.sh does not work
jsonPathLocation = '/usr/local/lib/nerlnet-lib/NErlNet/jsonPath' 
jsonPath = open(jsonPathLocation)
content = jsonPath.readlines()
# Get the components of the current system:
componentsPath = content[ARCHITECTURE_INDEX][:-1]
components = NetworkComponents(componentsPath)
# Get the flow of the current experiment:
expFlowPath = content[GRAPH_INDEX][:-1]
file = open(expFlowPath)
expFlow = json.load(file)

# Dict with {worker : csv}:
workerCsv = {}

# Check if we are running on Jupyter Notebook, to disable logging prompts:
ipythonPlatform = str(type(get_ipython()))

if 'zmqshell' in ipythonPlatform: # Check if runnnig on Jupyter Notebook.
    jupyterFlag =  True 

else: 
    jupyterFlag = False

# Prepare to get results from the receiver:
expResults = Experiment()

if __name__ == "__main__":
    components.printComponents()
    print(content[0])
    print(content[1])

# Addresses for future development:
'''
trainingListReq = [('http://127.0.0.1:8080/updateCSV', "s1,w1,RunOrWalkTrain_splitted"),
                ('http://127.0.0.1:8080/clientsTraining', "")]

CastingListReq = [('http://127.0.0.1:8080/startCasting', "s1")]
'''

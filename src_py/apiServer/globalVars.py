import multiprocessing 
import socket
import os
from networkComponents import NetworkComponents
import json
from IPython import get_ipython

#from IPython import get_ipython
localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

pendingAcks = 0

multiProcQueue = multiprocessing.Queue() # Create an instance of the queue

# Creating lists to store results
trainResults = []
predictResults = []

# Get the components of the current system:
ARCHITECTURE_INDEX = 4
GRAPH_INDEX = 5

username = os.getlogin()
jsonPathLocation = '/home/{}/workspace/NErlNet/jsonPath'.format(username)
#jsonPathLocation = '/usr/local/lib/nerlnet-lib/NErlNet' #TODO: Check if working after using NerlnetInstall.sh, and delete previous line
jsonPath = open(jsonPathLocation)
content = jsonPath.readlines()
componentsPath = content[ARCHITECTURE_INDEX][:-1]
components = NetworkComponents(componentsPath)
# Get the topology of the current system:
expFlowPath = content[GRAPH_INDEX][:-1]
file = open(expFlowPath)
expFlow = json.load(file)

# Check the platform we are running on:
ipythonPlatform = str(type(get_ipython()))

if 'zmqshell' in ipythonPlatform:
    jupyterFlag =  1 #Return 1 for Jupyter Notebook.
elif 'terminal' in ipythonPlatform:
    jupyterFlag =  2 #Return 2 for IPython Shell.
else:
    jupyterFlag =  0 #Return 0 for Terminal.

if __name__ == "__main__":
    components.printComponents()
    print(content[0])
    print(content[1])

'''
trainingListReq = [('http://127.0.0.1:8080/updateCSV', "s1,w1,RunOrWalkTrain_splitted"),
                ('http://127.0.0.1:8080/clientsTraining', "")]

CastingListReq = [('http://127.0.0.1:8080/startCasting', "s1")]
'''

'''
mainServerIP = 'http://127.0.0.1' 
mainServerPort = '8080'
mainServerAddress = mainServerIP + ':' + mainServerPort
'''
import multiprocessing 
import socket
from networkMap import NetworkMap
import json
from IPython import get_ipython

localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

pendingAcks = 0

multiProcQueue = multiprocessing.Queue() # Create instance of queue

lossMaps = []

mapPath = '1s1wMap.json'  
map = NetworkMap(mapPath)

expFlowPath = 'expFlowJson.json'
file = open(expFlowPath)
expFlow = json.load(file)


def checkPlatform():
        ipy_str = str(type(get_ipython()))
        if 'zmqshell' in ipy_str:
            return 1 #Return 1 for Jupyter Notebook.
        elif 'terminal' in ipy_str:
            return 2 #Return 2 for IPython Shell.
        else:
            return 0 #Return 0 for Terminal.

jupyterFlag = checkPlatform()


if __name__ == "__main__":
    #print(localIp)
    #print(checkPlatform())
    list = expFlow["Training"]["s1"]
    str = ", ".join(list)
    print(str)
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
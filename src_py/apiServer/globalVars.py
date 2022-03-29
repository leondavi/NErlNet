import multiprocessing 
import socket
from networkMap import NetworkMap
from IPython import get_ipython

localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

pendingAcks = 0

multiProcQueue = multiprocessing.Queue() # Create instance of queue

lossMaps = []

path = 'map.json'  
map = NetworkMap(path)

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
    print(localIp)
    print(checkPlatform())

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
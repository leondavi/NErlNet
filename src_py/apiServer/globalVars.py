import multiprocessing 
import socket
from networkMap import NetworkMap

localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

pendingAcks = 0

multiProcQueue = multiprocessing.Queue() # Create instance of queue

lossMaps = []

path = './map.json'  
map = NetworkMap(path)

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
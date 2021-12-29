import multiprocessing 
import socket

hostName = socket.gethostname()
localIp = socket.gethostbyname(hostName)

pendingAcks = 0

mainServerIP = 'http://127.0.0.1' 
mainServerPort = '8080'
mainServerAddress = mainServerIP + ':' + mainServerPort

multiProcQueue = multiprocessing.Queue() # Create instance of queue

lossMap = {}

'''
trainingListReq = [('http://127.0.0.1:8080/updateCSV', "s1,w1,RunOrWalkTrain_splitted"),
                ('http://127.0.0.1:8080/clientsTraining', "")]

CastingListReq = [('http://127.0.0.1:8080/startCasting', "s1")]
'''
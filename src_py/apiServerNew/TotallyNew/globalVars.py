pendingAcks = 0

mainServerIP = 'http://127.0.0.1' 
mainServerPort = '8095'
mainServerAddress = mainServerIP + ':' + mainServerPort

trainingListReq = [('http://127.0.0.1:8080/updateCSV', "s1,w1,RunOrWalkTrain_splitted"),
                ('http://127.0.0.1:8080/clientsTraining', "")]

CastingListReq = [('http://127.0.0.1:8080/startCasting', "s1")]
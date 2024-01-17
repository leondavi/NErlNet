################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
from os import replace
from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import globalVars as globe
from workerResult import *
from decoderHttpMainServer import decode_main_server_ets_str

# debug flask with receiver.logger.info("message") instead of print("message
# import logging # To debug flask
# logging.basicConfig(level=logging.ERROR) # to debug flask


WORKER_NON_RESULT = -1
ACK_DEBUG = False

receiver = Flask(__name__)
api = Api(receiver)

#ackPost = reqparse.RequestParser()
#ackPost.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')
#lossArgs = reqparse.RequestParser()
#lossArgs.add_argument('lossFunction', type='str', help='Receiver Error - Please send lossFunction')

#Disable logging messages (Must be disabled in Jupyter):
logging.getLogger('werkzeug').disabled = True

def initReceiver(receiverHost, receiverPort, event):
        try:
            receiver.run(threaded = True, host = receiverHost, port = receiverPort) 

        except:
            event.set()
            return

def processResult(resData, currentPhase):
        if (currentPhase == "Training"):
            # Parse the Result: [w#, float]
            worker = resData[0]
            result = float(resData[1].replace(' ',''))
            #print(result)
            if (int(result) == -1):
                print(f"Received loss=-1 from worker {worker}. The NN's weights have been reset.")

            ## result is set by worker to be -1 when it had a problem working on the data
            if (int(result) != WORKER_NON_RESULT): 
                for csvRes in globe.experiment_focused_on.trainingResList:
                    if worker in csvRes.workers:
                        for workerRes in csvRes.workersResList:
                            if (workerRes.name == worker):
                                workerRes.addResult(result)

        elif (currentPhase == "Prediction"):
            # Parsing is done by the PredictBatch class:
            # experiment has reslist => of csvResult has workerResList => of WorkerResult has resList => of PredictBatch
            # resData = [w#, batchID, csvName, batchSize]
            newPredictBatch = PredictBatch(resData) 

            for csvRes in globe.experiment_focused_on.predictionResList:
                if newPredictBatch.worker in csvRes.workers:
                    for workerRes in csvRes.workersResList:
                        if (workerRes.name == newPredictBatch.worker):
                            newPredictBatch.fixOffset(csvRes.indexOffset)
                            workerRes.addResult(newPredictBatch)

class shutdown(Resource):
    def get(self):
        # https://stackoverflow.com/questions/15562446/how-to-stop-flask-application-without-using-ctrl-c
        # https://stackoverflow.com/questions/37004983/what-exactly-is-werkzeug
        shut = request.environ.get('werkzeug.server.shutdown')
        if shut is None:
            raise RuntimeError('Shudown error: not running with the Werkzeug Server')
        shut()

class test(Resource):
    def post(self):
        #multiProcQueue.put("new message @@@")
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class ack(Resource):
    def post(self):
        resData = request.form
        globe.pendingAcks -= 1
        if ACK_DEBUG:
            receiver.logger.info(f"Ack received {resData} pending acks (after): {globe.pendingAcks}")
        

class trainRes(Resource):
    def post(self):
        # receiver.logger.info("Training result received")
        # Result preprocessing:
        # Receiving from Erlang: "worker#loss"
        resData = request.form
        resData = list(resData)
        resData = resData[0].split('#') # From a list with only one string -> to a string. split by delimiter
        # Consider what to do
        # if globe.jupyterFlag == False:
        #     print(resData)
        processResult(resData, "Training")
        
        
#http_request(RouterHost,RouterPort,"predictRes",ListOfResults++"#"++BatchID++"#"++CSVName++"#"++BatchSize)
class predictRes(Resource):
    def post(self):
        # Result preprocessing:
        # Receiving from Erlang: Result++"#"++integer_to_list(BatchID)++"#"++CSVName++"#"++integer_to_list(BatchSize)
        resData = request.form
        resData = list(resData)
        resData = resData[0].split('#') # From a list with only one string -> to a string. split by delimiter:
        
        # This prints every batch - Consider what to do with this part!
        # if globe.jupyterFlag == False:
        #     print(resData)

        processResult(resData, "Prediction")

class statistics(Resource):
    def post(self):
        resData = request.get_data()

        # TODO 
        # list_of_entities_names_dicts = decode_main_server_ets_str(resData)
        # receiver.logger.info(f"Received statistics from main server: {res_dict}")
        # entity_type = globe.components.map_name_to_type[entity_name] #
        # for entity_name, entity_dict in list_of_entities_names_dicts:
        #     if entity_type == TYPE_CLIENT:
        #         nerlcom_db.update_client_stats(entity_name, entity_dict)
        # print(resData)
        # format: entity:stats,...|entity:stats,....
        # input is: "c1:c1=1903,w1=5,w2=2,w3=4,w4=4,w5=5,w6=1,w1=0.878,w2=0.953,w3=0.899,w4=0.269,w5=0.667,w6=0.945|c2:c2=638,w7=0,w8=2,w7=1.403,w8=1.192|r1:2507|r2:632|s1:207"
        statDict = {"workers": {}}
        for items in str(resData).split('|'):
            key, val = items.split(':')
            if '=' in val:      # workers stats
                for worker in val.split(','):
                    workerName, time = worker.split('=')
                    statDict["workers"][workerName] = time
            else:               # other entity
                statDict[key] = val
        # print(statDict)
        for key, val in statDict.items():
            if isinstance(val, dict):
                print(key, '--')
                for key2, val2 in val.items():
                    print("\t", key2, ' : ', val2)
            else:
                print(key, ' : ', val)
        globe.pendingAcks = 0

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(ack, "/ackPy")
api.add_resource(shutdown, "/shutdown")
api.add_resource(trainRes, "/trainRes")
api.add_resource(predictRes, "/predRes")
api.add_resource(statistics, "/statistics")

"""
def findDictForCsv(resList, csvWorked):
    for idx, resDict in enumerate(resList):
        if resDict['CSV path'] == csvWorked:
            return idx
    
    raise RuntimeError(f"ERROR(Receiver): Dictionary for {csvWorked} was not created.")

def processResult(resData, resList, currentPhase):
    worker = resData[0]
    result = float(resData[1].replace(' ',''))
    csvWorked = globe.workerCsv[worker]

    if currentPhase == "Training":
        dictIdx = findDictForCsv(globe.trainResults, csvWorked)
    elif currentPhase == "Predict":
        dictIdx = findDictForCsv(globe.predictResults, csvWorked)

    if not worker in resList[dictIdx]:
        resList[dictIdx][worker] = [result]
    else:
        resList[dictIdx][worker].append(result)
"""

"""
while True:
                print("1) Use the default address (http://127.0.0.1:8095).")
                print("2) Enter an address manually.")
                print("\nPlease choose an option:", end = ' ')

                option = input()

                try:
                    option = int(option)
                except ValueError:
                    print("\nIllegal Input") 
                    continue

                if (option > 0 and option <= 2):
                    break

                else:
                    print("\nIllegal Input") 

            if (option == 1):
                print("\nUsing the default address to initialize the receiver.")
                receiverHost = '127.0.0.1'
                receiverPort = '8095'

            elif (option == 2):
                print("\nPlease enter the host IP for the receiver:", end = ' ')
                receiverHost = input()
                print("\nPlease enter the port for the receiver:", end = ' ')
                receiverPort = input()       
"""
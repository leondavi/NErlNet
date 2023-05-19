###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
from os import replace
from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import globalVars as globe
import logging
from workerResult import *

WORKER_NON_RESULT = -1

receiver = Flask(__name__)
api = Api(receiver)

#ackPost = reqparse.RequestParser()
#ackPost.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')
#lossArgs = reqparse.RequestParser()
#lossArgs.add_argument('lossFunction', type='str', help='Receiver Error - Please send lossFunction')

#If in Jupyter Notebook: Disable logging messages:
if globe.jupyterFlag == True: 
    logging.getLogger('werkzeug').disabled = True

# Prepare to get results from the receiver:
experiment_flow_global = Experiment()

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
            ## second condition will be tested but respected for now
            if (int(result) != WORKER_NON_RESULT and int(result != 0)): 
                for csvRes in globe.experiment_flow_global.trainingResList:
                    if worker in csvRes.workers:
                        for workerRes in csvRes.workersResList:
                            if (workerRes.name == worker):
                                workerRes.addResult(result)

        elif (currentPhase == "Prediction"):
            # Parsing is done by the PredictBatch class:
            # experiment has reslist => of csvResult has workerResList => of WorkerResult has resList => of PredictBatch
            # resData = [w#, batchID, csvName, batchSize]
            newPredictBatch = PredictBatch(resData) 

            for csvRes in globe.experiment_flow_global.predictionResList:
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
        multiProcQueue.put("new message @@@")
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class ack(Resource):
    def post(self):
        globe.pendingAcks -= 1
        print(request.form['ack'] + 'Ack Received!')
        if globe.jupyterFlag == False:
            resData = request.form['ack']
            print(resData + 'Ack Received!')
            print(globe.pendingAcks)

class trainRes(Resource):
    def post(self):
        # Result preprocessing:
        # Receiving from Erlang: "worker#loss"
        resData = request.form
        resData = list(resData)
        resData = resData[0].split('#') # From a list with only one string -> to a string. split by delimiter
        if globe.jupyterFlag == False:
            print(resData)

        processResult(resData, "Training")
        
        
#http_request(RouterHost,RouterPort,"predictRes",ListOfResults++"#"++BatchID++"#"++CSVName++"#"++BatchSize)
class predictRes(Resource):
    def post(self):
        # Result preprocessing:
        # Receiving from Erlang: Result++"#"++integer_to_list(BatchID)++"#"++CSVName++"#"++integer_to_list(BatchSize)
        resData = request.form
        resData = list(resData)
        resData = resData[0].split('#') # From a list with only one string -> to a string. split by delimiter:
        if globe.jupyterFlag == False:
            print(resData)

        processResult(resData, "Prediction")

class statistics(Resource):
    def post(self):
        print(request)
        resData = request.form
        print(resData)

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(ack, "/ackP")
api.add_resource(shutdown, "/shutdown")
api.add_resource(trainRes, "/lossFunc") # TODO: Change to "/trainRes", both here and in erl
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
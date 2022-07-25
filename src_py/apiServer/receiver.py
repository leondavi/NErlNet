from os import replace
from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import globalVars as globe
import multiprocessing
import logging
from predictBatch import *

receiver = Flask(__name__)
api = Api(receiver)

#ackPost = reqparse.RequestParser()
#ackPost.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')
#lossArgs = reqparse.RequestParser()
#lossArgs.add_argument('lossFunction', type='str', help='Receiver Error - Please send lossFunction')

if globe.jupyterFlag == 1: #If in Jupyter Notebook: Disable logging messages
    logging.getLogger('werkzeug').disabled = True

def initReceiver():
    receiver.run(threaded=True, host='127.0.0.1', port=8095)

def processResult(resData, currentPhase):
        if (currentPhase == "Training"):
            # Parse the Result:
            worker = resData[0]
            result = float(resData[1].replace(' ',''))
            #print(result)
            if (int(result) == -1):
                print("Received loss=-1. The NN's weights have been reset.")
            if (int(result) != -1 and int(result != 0)):
                for csvRes in globe.expResults.trainingResList:
                    if worker in csvRes.workers:
                        for workerRes in csvRes.workersResList:
                            if (workerRes.name == worker):
                                workerRes.addResult(result)

        elif (currentPhase == "Prediction"):
            # Parsing is done by the PredictBatch class:
            newPredictBatch = PredictBatch(resData) 

            for csvRes in globe.expResults.predictionResList:
                if newPredictBatch.worker in csvRes.workers:
                    for workerRes in csvRes.workersResList:
                        if (workerRes.name == newPredictBatch.worker):
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
        if globe.jupyterFlag == 0:
            resData = request.form['ack']
            print(resData + 'Ack Received!')
            print(globe.pendingAcks)

class trainRes(Resource):
    def post(self):
        # Result preprocessing:
        resData = request.form
        resData = list(resData)
        resData = resData[0].split('#') # From a list with only one string -> to a string. split by delimiter
        if globe.jupyterFlag == 0:
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
        if globe.jupyterFlag == 0:
            print(resData)

        processResult(resData, "Prediction")

class statistics(Resource):
    def post(self):
        print('hi')
        # TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

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
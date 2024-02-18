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
from decoderHttpMainServer import *

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

def initReceiver(receiverHost, receiverPort, event, apiserver_event_sync):
        try:
            receiver.config['API_SERVER_EVENT_SYNC'] = apiserver_event_sync #TODO Ohad&Noa check that this works otherwise use glboal
            receiver.run(threaded = True, host = receiverHost, port = receiverPort) 

        except:
            event.set()
            return

def processResult(resData, currentPhase):
        if (currentPhase == "Training"):
            # Parse the Result: [w#, float, float...]
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

class terminate(Resource):
    def post(self):
        resData = request.form
        shutdown = request.environ.get('werkzeug.server.shutdown')
        if shutdown is None:
            raise RuntimeError('Shudown error: not running with the Werkzeug Server')
        api_server_event_sync_inst = receiver.config['API_SERVER_EVENT_SYNC']
        api_server_event_sync_inst.set_event_done(api_server_event_sync_inst.TERMINATE)
        shutdown()

class test(Resource):
    def post(self):
        #multiProcQueue.put("new message @@@")
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class ack(Resource):  # request from Guy state related message as ack
    def post(self):
        body = request.get_data().decode('utf-8')
        print(f"Got ack from Main Server: {body}")  #TOdo remove print
        events_sync_inst = globe.experiment_focused_on.get_events_sync()
        event_done = events_sync_inst.get_event_done(body)
        events_sync_inst.set_event_wait(event_done)
        events_sync_inst.set_event_done(event_done)

class trainRes(Resource):
    def post(self):
        # receiver.logger.info("Training result received")
        # Result preprocessing:
        # Receiving from Erlang: "worker#loss" 
        # TODO example "w1#source_name|batch_id|loss_value|duration"
        # TODO GUY - Add all attributes of nerl_db (batch_id etc.)
        source_csv_dict = {}
        resData = request.get_data().decode('utf-8')
        print(f"Got {resData} from MainServer")
        resData = decode_main_server_str_train(resData) #list of strings
        #source_csv_dict[resData[0]] = None #TODO Ohad&Noa Continue
        print(f"Received training result {resData}")
        globe.experiment_focused_on.get_current_experiment_phase() #TODO Ohad&Noa Continue
        #TRAINING_STR
        # TODO OHAD - Add the parsed data to the nerl_db
        # Consider what to do
        # if globe.jupyterFlag == False:
        #processResult(resData, "Training")
        
        
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

class jsons_sent(Resource): #TODO guy implement on erlang side
    def post(self):
        # pay attention that there are two kinds of syncs one for experiment phase events and one for api-server events
        apiserver_events_sync_inst =  api_server_event_sync_inst = receiver.config['API_SERVER_EVENT_SYNC']
        apiserver_events_sync_inst.set_event_done(apiserver_events_sync_inst.SEND_JSONS)

class statistics(Resource):
    def post(self) -> None:
        resData = request.get_data().decode('utf-8')
        print("Got statistics from main server")
        entity_com_dicts = decode_main_server_ets_str(resData) # dict of dicts 
        globe.experiment_focused_on.nerl_comm_db.update_entities_stats(entity_com_dicts)        
        globe.pendingAcks = 0

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(ack, "/ackPy")
api.add_resource(trainRes, "/trainRes")
api.add_resource(predictRes, "/predRes")
api.add_resource(statistics, "/statistics")
api.add_resource(terminate, "/terminate")
api.add_resource(jsons_sent, "/jsonsSent") #TODO guy implement on erlang side

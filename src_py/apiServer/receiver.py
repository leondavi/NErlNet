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

from time import sleep # TODO remove

# debug flask with receiver.logger.info("message") instead of print("message
#import logging # To debug flask
#logging.basicConfig(level=logging.INFO) # to debug flask


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

def initReceiver(globe_components, transmitter, event, apiserver_event_sync):
        try:
            receiver.config['GLOBE_COMPONNETNS'] = globe_components
            receiver.config['TRANSMITTER'] = transmitter
            receiver.config['API_SERVER_EVENT_SYNC'] = apiserver_event_sync #TODO Ohad&Noa check that this works otherwise use glboal
            receiver.run(threaded = True, host = globe_components.receiverIp, port = globe_components.receiverPort) 

        except:
            event.set()
            return

class terminate(Resource):
    def post(self):
        resData = request.form
        shutdown = request.environ.get('werkzeug.server.shutdown')
        if shutdown is None:
            raise RuntimeError('Shudown error: not running with the Werkzeug Server')
        api_server_event_sync_inst = receiver.config['API_SERVER_EVENT_SYNC']
        api_server_event_sync_inst.set_event_done(api_server_event_sync_inst.TERMINATE)
        shutdown()

class ack(Resource):  # request from Guy state related message as ack
    def post(self):
        event_str = request.get_data().decode('utf-8')
        #receiver.logger.info(f"received event_str {event_str}")
        api_server_events_sync_inst = receiver.config['API_SERVER_EVENT_SYNC']
        enum_api_server_event_done = api_server_events_sync_inst.get_event_done(event_str)
        event_status = api_server_events_sync_inst.get_event_status(enum_api_server_event_done)
        #receiver.logger.info(f"event_str {event_str} event_status {event_status}")
        if event_status == api_server_events_sync_inst.WAIT:
            api_server_events_sync_inst.set_event_done(enum_api_server_event_done)

        events_sync_inst = globe.experiment_focused_on.get_events_sync()
        enum_event_done = events_sync_inst.get_event_done(event_str)
        event_status = events_sync_inst.get_event_status(enum_event_done)
        if event_status == events_sync_inst.WAIT:
            events_sync_inst.set_event_done(enum_event_done)
        
        return "OK", 200

class trainRes(Resource):
    def post(self):
        resDataDict = request.get_json()
        current_experiment_phase = globe.experiment_focused_on.get_current_experiment_phase() 
        raw_data_buffer = current_experiment_phase.get_raw_data_buffer()
        raw_data_buffer.append(resDataDict)
        return "OK", 200

#http_request(RouterHost,RouterPort,"predictRes",ListOfResults++"#"++BatchID++"#"++CSVName++"#"++BatchSize)
class predictRes(Resource):
    def post(self):
        resDataDict = request.get_json()
        current_experiment_phase = globe.experiment_focused_on.get_current_experiment_phase() 
        raw_data_buffer = current_experiment_phase.get_raw_data_buffer()
        raw_data_buffer.append(resDataDict)
        return "OK", 200 

class statistics(Resource):
    def post(self) -> None:
        resData = request.get_data().decode('utf-8')
        entity_com_dicts, entity_perf_dicts = decode_main_server_ets_str(resData) # dict of dicts 
        current_experiment_flow = globe.experiment_focused_on
        event_sync_inst = current_experiment_flow.get_events_sync()
        current_experiment_phase = current_experiment_flow.get_current_experiment_phase()
        current_experiment_phase.get_nerl_comm_db().update_entities_stats(entity_com_dicts)    
        current_experiment_phase.get_nerl_perf_db().update_entities_stats(entity_perf_dicts) 
        event_sync_inst.set_event_done(event_sync_inst.COMMUNICATION_STATS)

        transmitter = receiver.config['TRANSMITTER']
        transmitter.send_ack_validation()
        
        return "OK", 200


#Listener Server list of resources: 
api.add_resource(ack, "/ackPy")
api.add_resource(trainRes, "/trainRes")
api.add_resource(predictRes, "/predRes")
api.add_resource(statistics, "/statistics")
api.add_resource(terminate, "/terminate")


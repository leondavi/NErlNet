from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import multiprocessing

SERVER_BUSY = 0
SERVER_DONE = 1
multiProcQueue = multiprocessing.Queue()

receiver = Flask(__name__)
api = Api(receiver)

ackArgs = reqparse.RequestParser()
ackArgs.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')

def initReceiver():
    receiver.run(threaded=True, port=8095)
    
class shutdown(Resource):
    def get(self):
        pass
        #shut = request.environ.get('werkzeug.server.shutdown')

        #if shut is None:
        #    raise RuntimeError('Shudown error: not running with the Werkzeug Server')

        #shut()

class test(Resource):
    def post(self):
        multiProcQueue.put("new message @@@")
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class train(Resource):
    def post(self): 
        pass

class ack(Resource):
    def post(self):
        reqData = request.form['ack']
        print(reqData + 'Ack Received!')
        #globe.pendingAcks -= 1
        #print(globe.pendingAcks)

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(train, "/train")
api.add_resource(ack, "/ack")
api.add_resource(shutdown, "/shutdown")

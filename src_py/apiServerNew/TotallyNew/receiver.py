from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import globalVars as globe 

SERVER_BUSY = 0
SERVER_DONE = 1

receiver = Flask(__name__)
api = Api(receiver)

ackArgs = reqparse.RequestParser()
ackArgs.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')

def initReceiver():
    receiver.run(debug=True, threaded=True, port=8095)

class test(Resource):
    def post(self):
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class train(Resource):
    def post(self): 
        managerQueue.put(SERVER_BUSY)

        #while numPages < TotalPages
        #    managerQueue.put(mainServerData)
        #    numPages += 1 

        managerQueue.put(SERVER_DONE) 

        return None 

class ack(Resource):
    def post(self):
        reqData = request.form['ack']
        print(reqData + 'Ack Received!')

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(train, "/train")
api.add_resource(ack, "/ack")

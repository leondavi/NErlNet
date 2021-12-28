from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import multiprocessing
import globalVars as globe

SERVER_BUSY = 0
SERVER_DONE = 1
multiProcQueue = multiprocessing.Queue() # Create instance of queue

receiver = Flask(__name__)
api = Api(receiver)

ackArgs = reqparse.RequestParser()
ackArgs.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')

def initReceiver():
    receiver.run(threaded=True, port=8095)

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

class train(Resource):
    def post(self): 
        pass

class predict(Resource):
    def post(self):
        pass

class ack(Resource):
    def post(self):
        reqData = request.form['ack']
        print(reqData + 'Ack Received!')
        globe.pendingAcks -= 1
        print(globe.pendingAcks)

class testglobe(Resource):
    def get(self):
        return globe.pendingAcks

class lossFunction(Resource):
    def post(self):
        reqData = request.form
        reqData = list(reqData)
        # From a list with only one string -> to a string
        reqData = reqData[0].split('#')
        print(reqData[2])


#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(train, "/train")
api.add_resource(ack, "/ack")
api.add_resource(shutdown, "/shutdown")
api.add_resource(predict, "/predict")
api.add_resource(testglobe, "/testglobe")
api.add_resource(lossFunction, "/lossFunction")

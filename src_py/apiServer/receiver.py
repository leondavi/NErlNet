from flask import Flask, request, jsonify
from flask_restful import Api, Resource, reqparse
from globalVars import *
import globalVars as globe
import multiprocessing


#SERVER_BUSY = 0
#SERVER_DONE = 1

receiver = Flask(__name__)
api = Api(receiver)

#ackPost = reqparse.RequestParser()
#ackPost.add_argument('ack', type='str', help='Receiver Error - Please send Acknowledgment')
#lossArgs = reqparse.RequestParser()
#lossArgs.add_argument('lossFunction', type='str', help='Receiver Error - Please send lossFunction')

def initReceiver():
    receiver.run(threaded=True, host='127.0.0.1', port=8095)

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
        reqData = request.form['ack']
        print(reqData + 'Ack Received!')
        globe.pendingAcks -= 1
        print(globe.pendingAcks)

        # After receivng the final ACK, we conclude that the operation has finished.
        if globe.pendingAcks == 0:
            # So, we are able to insert the loss map that was created to the queue:
            globe.multiProcQueue.put(lossMaps[-1])

class lossFunction(Resource):
    def post(self):
        # Receive string 'worker#loss' -> []
        reqData = request.form
        reqData = list(reqData)
        # From a list with only one string -> to a string. split by delimiter:
        reqData = reqData[0].split('#')

        worker = reqData[0]
        loss = float(reqData[1])

        currentLossMap = globe.lossMaps[-1]

        if not worker in currentLossMap:
            currentLossMap[worker] = [loss]
        else:
            currentLossMap[worker].append(loss)
        
        # After receiving the entire map, we wait for the final ACK.
        # Then we insert the map to the queue (See lines 43-46).

class statistics(Resource):
    def post(self):
        # Receive string 'worker#loss' -> []
        reqData = request.form
        print(reqData)

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(ack, "/ack")
api.add_resource(shutdown, "/shutdown")
api.add_resource(lossFunction, "/lossFunction")
api.add_resource(statistics, "/statistics")
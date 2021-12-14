from flask import Flask
from flask_restful import Api, Resource
import globalVars2 as globe
import redis
from rq import Queue
import queue

receiver = Flask(__name__)
api = Api(receiver)

def runReceiver():
    print("Starting receiver server...")
    receiver.run(debug=True, threaded=True, port=8095)

class test(Resource):
    def post(self):
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class testQueue(Resource):
    def post(self):
        return {'Test' : 'Passed!'} 

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(testQueue, "/testQueue")

if __name__ == "__main__":
    runReceiver()
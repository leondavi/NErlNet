from flask import Flask
from flask_restful import Api, Resource
import globalVars as globe
import queue

reciever = Flask(__name__)
api = Api(reciever)

def runReciever():
    reciever.run(debug=True)

class test(Resource):
    def post(self):
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class testQueue(Resource):
    def post(self):
        testHandeled = globe.ackQueue.get()
        str = 'Test' + testHandeled
        return {str : 'Passed!'} 


#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(testQueue, "/testQueue")

if __name__ == "__main__":
    runReciever()
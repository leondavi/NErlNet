from flask import Flask
from flask_restful import Api, Resource
from globalVars2 import *
from flask import Flask, Api

SERVER_BUSY = 0
SERVER_DONE = 1

receiver = Flask(__name__)
api = Api(receiver)

class test(Resource):
    def post(self):
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class train(Resource):
    def post(self):
        # data pre processing  
        managerQueue.put(SERVER_BUSY)

        while numPages < TotalPages
            managerQueue.put(mainServerData)
            numPages += 1 

        managerQueue.put(SERVER_DONE)


class testQueue(Resource):
    def post(self):
        return {'Test' : 'Passed!'} 

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(testQueue, "/testQueue")
api.add_resource(train, "/train")

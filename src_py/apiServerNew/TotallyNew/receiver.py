from flask import Flask
from flask_restful import Api, Resource
from globalVars2 import *
from flask import Flask, Api

receiver = Flask(__name__)
api = Api(receiver)

class test(Resource):
    def post(self):
        return {'Test' : 'Passed!'} #Returns the response in JSON format

class testQueue(Resource):
    def post(self):
        return {'Test' : 'Passed!'} 

#Listener Server list of resources: 
api.add_resource(test, "/test")
api.add_resource(testQueue, "/testQueue")


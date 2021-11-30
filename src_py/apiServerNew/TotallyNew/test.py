from flask import Flask
from flask_restful import Api, Resource
import transmitter as trans
import recieverServer as reciever
import globalVars as globe

defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseRecieverAddress = 'http://127.0.0.1:5000'

trans.testPost(baseRecieverAddress + '/test', 0)

trans.testQueue(baseRecieverAddress + '/testQueue')
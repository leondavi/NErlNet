from flask import Flask
from flask_restful import Api, Resource
import transmitter as trans
import receiverServer as receiver
import globalVars as globe
from threading import Thread
import time

defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:8095'


receiver.runReceiver()
time.sleep(2)
print("server strated")

result = trans.testQueue(baseReceiverAddress + '/testQueue')
time.sleep(2)
print(result)
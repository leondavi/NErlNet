from flask import Flask
from flask_restful import Api, Resource
import transmitter as trans
import receiverServer as receiver
import globalVars as globe
from multiprocessing import Process

defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:5000'

if __name__ == "__main__":
    trans.testPost(baseReceiverAddress + '/test', 0)

    trans.testQueue(baseReceiverAddress + '/testQueue')
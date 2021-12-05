from flask import Flask
from flask_restful import Api, Resource
import transmitter as trans
import receiverServer as receiver
import globalVars as globe
import time

defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:8095'

if __name__ == "__main__":
    result1 = trans.testPost(baseReceiverAddress + '/test', 0)
    print(result1)
    
    result2 = trans.testQueue(baseReceiverAddress + '/testQueue')
    time.sleep(2)
    print(result2)

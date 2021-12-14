from flask import Flask
from flask_restful import Api, Resource
import transmitter as trans
import receiverServer as receiver
import globalVars2 as globe
from multiprocessing import Process, Manager
import time
import queue

defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:8095'

#Creating a seperate process for the receiver, in the following block:
procs = []

p1 = Process(target=receiver.runReceiver, args=())
procs.append(p1)

p1.start()
#It takes some time for the server to fully initiate
time.sleep(0.5)

print("Receiver server started!")
##########################################################################

trans.testPost(baseReceiverAddress + '/test', 0)

result2 = trans.testQueue(baseReceiverAddress + '/testQueue')
print(result2)





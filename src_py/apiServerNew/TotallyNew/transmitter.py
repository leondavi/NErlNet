import requests
import globalVars2 as globe
import queue
from threading import Thread
import redis
from rq import Queue
import time
import queue

#DEFAULT_PORT = 8095

def testPost(address, payloadNum):
    payload = {'test' : payloadNum}
    response = requests.post(address,data = payload)
    print(response.ok, response.status_code, response.json())
    #Return true, if received: HTTP status code < 400
    #Return the HTTP status code for the response
    #Return the reponse in JSON format
    return(response.ok, response.status_code, response.json())

def testQueue(address):
    for i in range(0, 10):
        globe.q.put(testPost(address, i+1))

    empty = globe.q.empty()

    return empty
def wait():
    while not globe.ackQueue.empty(): #While the queue is NOT empty
        pass

if __name__ == "__main__":
    print('transmitter')
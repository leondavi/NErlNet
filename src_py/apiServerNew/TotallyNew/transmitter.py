import requests
import globalVars as globe
import queue
from threading import Thread

DEFAULT_PORT = 8095

def testPost(address, payloadNum):
    payload = {'test' : payloadNum}
    response = requests.post(address,data = payload) 
    print(response.ok) #Return true, if recived: HTTP status code < 400
    print(response.status_code) #Print the HTTP status code for the response
    print(response.json()) #Print the reponse in JSON format

def testQueue(address):
    for i in range(0, 10):
        globe.ackQueue.put(i+1)   #Add the request to the queue
        testPost(address, i+1)
        wait()

def wait():
    while not globe.ackQueue.empty(): #While the queue is NOT empty
        pass


if __name__ == "__main__":
    print('transmitter')
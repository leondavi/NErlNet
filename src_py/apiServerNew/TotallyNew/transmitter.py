import requests
import globalVars2 as globe
#import time

#DEFAULT_PORT = 8095

class Transmitter():

    def __init__(self,mainServerAddress):
        self.mainServerAddress = mainServerAddress

    def testPost(self,payloadNum):
        payload = {'test' : payloadNum}
        response = requests.post(self.mainServerAddress ,data = payload)
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
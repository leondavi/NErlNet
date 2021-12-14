import sys
import os
import glob
import numpy as np
import requests
import time
import matplotlib.pyplot as plt
import NerlnetPyAPI.settings
from NerlnetPyAPI import run, JsonCreator, __init__
from threading import Thread

DEFAULT_PORT = 8095

def startServer(port = DEFAULT_PORT):
    thread = Thread(target=run, args=(port, )) #Define a new Thread object
    thread.start() #Start a new thread
    time.sleep(2) #Wait 2 seconds, to allow proper functioning
    print("Server started working")

def testServerPost():
    payLoad = 'test'
    r = requests.post('https://httpbin.org/post',data = payLoad) #httpbin is a website aimed for these tests
    print(r.ok) #Return true, if recived: HTTP status code < 400

if __name__ == "__main__":
    NerlnetPyAPI.settings.lenOfRequests = 6
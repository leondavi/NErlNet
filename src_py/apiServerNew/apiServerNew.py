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
    thread = Thread(target=run, args=(port,))
    thread.start()
    time.sleep(2)
    print("Server started working")

def testServerPost():
    pload = 'test'
    r = requests.post('https://httpbin.org/post',data = pload)
    print(r.ok) #Return true, if recived: HTTP status code < 400

if __name__ == "__main__":
    NerlnetPyAPI.settings.lenOfRequests = 6
import sys
import os
import glob
import numpy as np
import requests
import time
import matplotlib.pyplot as plt
import NerlnetPyAPI.settings
from NerlnetPyAPI import run, creatJson, init
from threading import Thread
from expiremntFLow import *

sys.path.append(os.getcwd() + '/src_py')

DEFAULT_PORT = 8095


def server(port=DEFAULT_PORT):
    thread = Thread(target=run, args=(port,))
    thread.start()
    time.sleep(2)
    print("server running in background...")

def hello():
    r = requests.post('https://httpbin.org/post',data = pload)

if __name__ == "__main__":
    NerlnetPyAPI.settings.lenOfRequests = 6
    port = DEFAULT_PORT
    server(port)



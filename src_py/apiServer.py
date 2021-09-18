import sys
import os
import numpy as np

sys.path.append(os.getcwd() + '/src_py')

from NerlnetPyAPI import run, creatJson, init
import matplotlib.pyplot as plt
import NerlnetPyAPI.settings
from threading import Thread
from expiremntFLow import *
import requests
import time

DEFAULT_PORT = 8095

def server(port):
    thread = Thread(target=run, args=(port,))
    thread.start()
    time.sleep(2)
    print("server running in background...")


def analyze(arr):
    # x-axis values
    x = np.array([0] * len(arr))
    for i in range(1, len(arr) + 1):
        x[i - 1] = i;
    y = arr  # this for example you could extract it from external file

    # plotting points as a scatter plot
    plt.plot(x, y)

    # x-axis label
    plt.xlabel('x - axis')
    # frequency label
    plt.ylabel('y - axis')
    # plot title
    plt.title('My scatter plot!')
    # showing legend
    plt.legend()

    # function to show the plot
    plt.show()


def readfile():
    solution_path = "w1.txt"

    sequence_array = []
    with open(solution_path, 'r') as f:
        for line in f.readlines():
            sequence_array.append(float(line))
    # print(map(float, sequence_array))

    print(sequence_array)
    analyze(sequence_array)


def startPredict():
    print('Nerlnet initiating predict..')
    listOfRequests = initPredict()
    NerlnetPyAPI.settings.x = len(listOfRequests)
    for request in listOfRequests:
        r = requests.post(request[0], data=request[1])
        print(r.text)
    while NerlnetPyAPI.settings.x != 0:
        time.sleep(0.2)
        print('Nerlnet finished initiating predict..')


def startInit():
    print('Nerlnet initiating training..')
    trainRequests = initTrain()
    NerlnetPyAPI.settings.x = len(trainRequests)
    for trainRequest in trainRequests:
        r = requests.post(trainRequest[0], data=trainRequest[1])
        print(r.text)
    while NerlnetPyAPI.settings.x != 0:
        time.sleep(0.2)
    print('finish initiating start training..')


def startCAting():
    print('startCasting..')
    listOfRequests = startCasting()
    NerlnetPyAPI.settings.x = len(listOfRequests)
    for request in listOfRequests:
        r = requests.post(request[0], data=request[1])
        print(r.text)
    while NerlnetPyAPI.settings.x != 0:
        time.sleep(0.2)
    print('finished training!..')


def init(jsonPath='src_py/architectures.json', inputPort=DEFAULT_PORT):
    NerlnetPyAPI.settings.x = 6
    port = inputPort
    server(port)

    # if creatJson():
    #   jsonPath = 'src_py/architectures.json'

    startInit()

    startCAting()

    startPredict()

    startCAting()

    # analyze()
    # init(jsonPath)


if __name__ == "__main__":
    from sys import argv

    init()
    # analyze()

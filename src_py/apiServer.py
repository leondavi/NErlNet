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
    NerlnetPyAPI.settings.lenOfRequests = len(listOfRequests)
    for request in listOfRequests:
        r = requests.post(request[0], data=request[1])
        print(r.text)
    while NerlnetPyAPI.settings.lenOfRequests != 0:
        time.sleep(0.2)
        print('Nerlnet finished initiating predict..')


def startCasting(numberOfbatches='1000'):  # X i
    print('startCasting..')
    listOfRequests = startCastingStack()
    NerlnetPyAPI.settings.lenOfRequests = len(listOfRequests)
    for request in listOfRequests:
        r = requests.post(request[0], data=request[1] + ',' + numberOfbatches)
        print(r.text)
    while NerlnetPyAPI.settings.lenOfRequests != 0:
        time.sleep(0.2)
    print('finished training!..')


def initTrain(jsonPath='src_py/architectures.json', inputPort=DEFAULT_PORT):  # X1

    # if creatJson():
    #   jsonPath = 'src_py/architectures.json'
    print('Nerlnet initiating training..')
    trainRequests = initTrainStack()
    NerlnetPyAPI.settings.lenOfRequests = len(trainRequests)
    for trainRequest in trainRequests:
        r = requests.post(trainRequest[0], data=trainRequest[1])
        print(r.text)
    while NerlnetPyAPI.settings.lenOfRequests != 0:
        time.sleep(0.2)
    print('finish initiating start training..')
    # init(jsonPath)


def getLoosVector():
    pass


def getDistanceVector():
    pass


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
    solution_path = "../src_erl/Communication_Layer/http_Nerlserver/output/w1"

    sequence_array = []
    with open(solution_path, 'r') as f:
        for line in f.readlines():
            sequence_array.append(float(line))
    analyze(sequence_array)


def startPredict():
    print('Nerlnet initiating predict..')
    listOfRequests = initPredict()
    NerlnetPyAPI.settings.lenOfRequests = len(listOfRequests)
    for request in listOfRequests:
        r = requests.post(request[0], data=request[1])
        print(r.text)
    while NerlnetPyAPI.settings.lenOfRequests != 0:
        time.sleep(0.2)
        print('Nerlnet finished initiating predict..')


def startCasting(numberOfbatches='1000'):  # X i
    print('startCasting..')
    listOfRequests = startCastingStack()
    NerlnetPyAPI.settings.lenOfRequests = len(listOfRequests)
    for request in listOfRequests:
        r = requests.post(request[0], data=request[1] + ',' + numberOfbatches)
        print(r.text)
    while NerlnetPyAPI.settings.lenOfRequests != 0:
        time.sleep(0.2)
    print('finished training!..')


def initTrain(jsonPath='src_py/architectures.json', inputPort=DEFAULT_PORT):  # X1

    # if creatJson():
    #   jsonPath = 'src_py/architectures.json'
    print('Nerlnet initiating training..')
    trainRequests = initTrainStack()
    NerlnetPyAPI.settings.lenOfRequests = len(trainRequests)
    for trainRequest in trainRequests:
        r = requests.post(trainRequest[0], data=trainRequest[1])
        print(r.text)
    while NerlnetPyAPI.settings.lenOfRequests != 0:
        time.sleep(0.2)
    print('finish initiating start training..')
    # init(jsonPath)


def getLoosVector():
    pass


def getDistanceVector():
    pass

def calculateAccuracy(resultPath, inputPath):
    resFile = open(resultPath, "r")
    inputFile = open(inputPath, "r")
    roundNumbersFile = open(resultPath + "round", "a")

    for y in resFile:
        ResLineWithIndex1 = y.split()
        ResLine1 = ResLineWithIndex1[1]
        numbers = ResLine1.split(",")
        numberslen = len(numbers) - 1
        roundNumbersFile.write(ResLineWithIndex1[0] + " ")
        for number, item in enumerate(numbers):
            if number == numberslen:
                roundedNumber = round(float(item))
                roundNumbersFile.write(str(roundedNumber))
            else:
                roundedNumber = round(float(item))
                roundNumbersFile.write(str(roundedNumber) + ",")

        roundNumbersFile.write("\n")

    roundNumbersFile.close()
    resFile.close()
    resRoundFile = open(resultPath + "round", "r")
    Lines = inputFile.readlines()
    total = 0
    correct = 0
    for x in resRoundFile:
        splitted = x.split()
        Result = splitted[1]
        index = int(splitted[0]) - 1
        inputRes = Lines[index].rstrip()
        if Result == inputRes:
            # print("correct!")
            total += 1
            correct += 1
        else:
            # print("wrong!")
            total += 1

    print("Accuracy:", str(correct) + "/" + str(total), "=", str((correct / total) * 100), "%")
    inputFile.close()
    resRoundFile.close()

def removeRedundantFiles():
    files = glob.glob('../src_erl/Communication_Layer/http_Nerlserver/output/*')
    for f in files:
        os.remove(f)


if __name__ == "__main__":
    from sys import argv

    removeRedundantFiles()

    NerlnetPyAPI.settings.lenOfRequests = 6
    port = DEFAULT_PORT
    server(port)
    initTrain()
    # analyze()
    startCasting()
    startPredict()
    startCasting()
    time.sleep(1)
    readfile()
    calculateAccuracy("../src_erl/Communication_Layer/http_Nerlserver/output/predictRunOrWalkPredictNolabels_splitted",
                      "../inputDataFiles/RunOrWalkPredictResults.csv")

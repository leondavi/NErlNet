import sys
import os

sys.path.append(os.getcwd()+'/src_py')

from NerlnetPyAPI import run, creatJson, init
import matplotlib.pyplot as plt
import NerlnetPyAPI.settings
from threading import Thread
from expiremntFLow import *
import requests
import time


def server(port):
    thread = Thread(target=run, args=(port,))
    thread.start()
    time.sleep(2)
    print("server running in background...")


def analyze():
  # x-axis values
  x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  # this for example you could extract it from external file
  # y-axis values
  y = [2, 4, 5, 7, 6, 8, 9, 11, 12, 12]  # this for example you could extract it from external file

  # plotting points as a scatter plot
  plt.scatter(x, y, label="stars", color="green",
              marker="*", s=30)

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

DEFAULT_PORT = 8095
def init(jsonPath = 'src_py/architectures.json',inputPort = DEFAULT_PORT):
  
  NerlnetPyAPI.settings.x = 6
  port = inputPort
  server(port)

  # if creatJson():
  #   jsonPath = 'src_py/architectures.json'
  print('Nerlnet initiating training..')
  trainRequests = initTrain()
  NerlnetPyAPI.settings.x = len(trainRequests)
  for trainRequest in trainRequests:
    r = requests.post(trainRequest[0], data=trainRequest[1])
    print(r.text)

  while NerlnetPyAPI.settings.x != 0:
    time.sleep(0.2)

  print('finish initiating start training..')

  print('startCasting..')
  listOfRequests = startCasting()
  NerlnetPyAPI.settings.x = len(listOfRequests)
  for request in listOfRequests:
    r = requests.post(request[0], data=request[1])
    print(r.text)

  while NerlnetPyAPI.settings.x != 0:
    time.sleep(0.2)

  print('Finished training!..')

  print('Nerlnet initiating predict..')
  listOfRequests = initPredict()
  NerlnetPyAPI.settings.x = len(listOfRequests)
  for request in listOfRequests:
    r = requests.post(request[0], data=request[1])
    print(r.text)

  while NerlnetPyAPI.settings.x != 0:
    time.sleep(0.2)
    print('Nerlnet finished initiating predict..')

  print('startCasting..')
  listOfRequests = startCasting()
  NerlnetPyAPI.settings.x = len(listOfRequests)
  for request in listOfRequests:
    r = requests.post(request[0], data=request[1])
    print(r.text)

  while NerlnetPyAPI.settings.x != 0:
    time.sleep(0.2)

  print('Finished predicting!..')

  # analyze()
  # init(jsonPath)

if __name__ == "__main__":
    from sys import argv
    init()

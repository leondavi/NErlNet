from flask import Flask
from flask_restful import Api, Resource
import transmitter as trans
import receiverServer as receiver
import globalVars as globe
import time
import signal

'''
defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:8095'

if __name__ == "__main__":
    result1 = trans.testPost(baseReceiverAddress + '/test', 0)
    print(result1)
    
    result2 = trans.testQueue(baseReceiverAddress + '/testQueue')
    time.sleep(2)
    print(result2)
'''

def handler(signum, frame):
    msg = "Ctrl-c was pressed. Do you really want to exit? y/n "
    print(msg, end="", flush=True)
    res = readchar.readchar()
    if res == 'y':
        print("")
        exit(1)
    else:
        print("", end="\r", flush=True)
        print(" " * len(msg), end="", flush=True) # clear the printed line
        print("    ", end="\r", flush=True)
 
 
signal.signal(signal.SIGINT, handler)
 
count = 0
while True:
    print(f"{count}", end="\r", flush=True)
    count += 1
    time.sleep(0.1)

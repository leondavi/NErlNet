lenOfRequests = 0
ackQueue = []
loosFunQueue = []
statistics = []
losDict = {}


def glob():
    global ackQueue
    ackQueue = []
    global loosFunQueue
    loosFunQueue = []
    global statistics
    statistics = []
    global losDict
    losDict = {}
    global lenOfRequests
    lenOfRequests = 0

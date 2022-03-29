def initTrainStack():
    new_list = [('http://192.168.0.102:8080/updateCSV', "s1,w1,RunOrWalkTrain_splitted"),
                ('http://192.168.0.102:8080/clientsTraining', "")]
    return new_list

def startCastingStack():
    # startCasting bode: "source_name1,source_2,....,numberOfSamplesToSend"
    new_list = [('http://192.168.0.102:8080/startCasting', "s1")]
    return new_list

def initPredictStack():
    new_list = [('http://192.168.0.102:8080/updateCSV', "s1,w1,RunOrWalkPredictNolabels_splitted"),
                ('http://192.168.0.102:8080/clientsPredict', "")]
    return new_list

def statistics():
    new_list = [('http://192.168.0.102:8080/statistics', "getStatistics")]
    return new_list
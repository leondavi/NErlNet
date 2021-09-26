def initTrain():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,./input/RunOrWalkTrain_splitted/"),
                ('http://127.0.0.1:8080/clientsTraining', "")]
    return new_list

def startCasting():
    # startCasting bode: "source_name1,source_2,....,numberOfSamplesToSend"
    new_list = [('http://127.0.0.1:8080/startCasting', "s1,100000")]
    return new_list

def initPredict():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,./input/RunOrWalkPredictNolabels_splitted/"),
                ('http://127.0.0.1:8080/clientsPredict', "")]
    return new_list

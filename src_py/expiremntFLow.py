def initTrain():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,./input/traininputShuffledNormalized.csv"),
                ('http://127.0.0.1:8080/clientsTraining', "")]
    return new_list

def startCasting():
    new_list = [('http://127.0.0.1:8080/startCasting', "s1")]
    return new_list

def initPredict():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,./input/predict6labelsNoLabels.csv"),
                ('http://127.0.0.1:8080/clientsPredict', "")]
    return new_list

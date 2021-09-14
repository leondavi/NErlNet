def initTrain():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,/home/david/workspace/data/sensor1/train6labels.csv"),
                ('http://127.0.0.1:8080/updateCSV', "s2,w2,/home/david/workspace/data/sensor2/train6labels.csv"),
                ('http://127.0.0.1:8080/clientsTraining', "")]
    return new_list

def startCasting():
    new_list = [('http://127.0.0.1:8080/startCasting', "s1,s2")]
    return new_list

def initPredict():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,/home/david/workspace/data/sensor1/predict6labelsNoLabels.csv"),
                ('http://127.0.0.1:8080/updateCSV', "s2,w2,/home/david/workspace/data/sensor2/predict6labelsNoLabels.csv"),
                ('http://127.0.0.1:8080/clientsPredict', "")]
    return new_list

def initTrain():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,./input/inputShort.csv"),
                ('http://127.0.0.1:8080/updateCSV', "s2,w2,./input/inputShort2.csv"),
                ('http://127.0.0.1:8080/clientsTraining', "")]
    return new_list

def startCasting():
    new_list = [('http://127.0.0.1:8080/startCasting', "s1,s2")]
    return new_list

def initPredict():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,./input/input9p.csv"),
                ('http://127.0.0.1:8080/updateCSV', "s2,w2,./input/input9p.csv"),
                ('http://127.0.0.1:8080/clientsPredict', "")]
    return new_list

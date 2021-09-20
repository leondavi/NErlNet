def initTrain():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,w2,./input/inputShort_splitted/"),
                ('http://127.0.0.1:8080/clientsTraining', "")]
    return new_list

def startCasting():
    # startCasting bode: "source_name1,source_2,....,numberOfSamplesToSend"
    new_list = [('http://127.0.0.1:8080/startCasting', "s1,2")]
    return new_list

def initPredict():
    new_list = [('http://127.0.0.1:8080/updateCSV', "s1,w1,w2,./input/input9p_splitted/"),
                ('http://127.0.0.1:8080/clientsPredict', "")]
    return new_list

import requests


def updateCSVtrain():
	r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/shuffled-input1.csv")
	# r = requests.post('http://192.168.0.107:8080/updateCSV',data = "s1,w1,w2rr,./input/shuffled-input99.csv")
	# r2 = requests.post('http://192.168.0.107:8080/updateCSV',data = "source2,worker5,worker7, ... ,./input/input4.csv")...
	print(r.text)

def updateCSVpredict():
	r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,w2,./input/input9p.csv")
	# r = requests.post('http://192.168.0.107:8080/updateCSV',data = "s1,w1,w2,./input/shuffled-input99.csv")
	# r2 = requests.post('http://192.168.0.107:8080/updateCSV',data = "source2,worker5,worker7, ... ,./input/input4.csv")...
	print(r.text)

def clientsTraining():
	r = requests.post('http://127.0.0.1:8080/clientsTraining',data = "")
	print(r.text)


def  clientsPredict():
	r = requests.post('http://127.0.0.1:8080/clientsPredict',data = "")
	print(r.text)

def startCasting():
	r = requests.post('http://127.0.0.1:8080/startCasting',data = "s1")
	print(r.text)

def stopCasting():
	r = requests.post('http://127.0.0.1:8080/stopCasting',data = "s1")
	print(r.text)




while 1:
	action = input("insert action: ")
	if action == "init1":
		updateCSVtrain()	
	elif action == "init2":
		updateCSVpredict()
	elif action == "train":
		clientsTraining()

	elif action == "predict":
		clientsPredict()

	elif action == "start":
		startCasting()

	elif action == "stop":
		stopCasting()






import requests


def updateCSVtrain():
	# r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,w2,./input/inputShort.csv")
	r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/traininputShuffled.csv")
	#r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/traininput.csv")
	#r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/train70percent.csv")
	#r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/train70percentShuffled.csv")
	
	# r = requests.post('http://192.168.0.107:8080/updateCSV',data = "s1,w1,w2rr,./input/shuffled-input99.csv")
	# r2 = requests.post('http://192.168.0.107:8080/updateCSV',data = "source2,worker5,worker7, ... ,./input/input4.csv")...
	print(r.text)

def updateCSVpredict():
	r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/predict10noLabels.csv")
	#r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/predict30percentNoLabels.csv")
	# r = requests.post('http://192.168.0.107:8080/updateCSV',data = "s1,w1,w2,./input/shuffled-input99.csv")
	# r2 = requests.post('http://192.168.0.107:8080/updateCSV',data = "source2,worker5,worker7, ... ,./input/input4.csv")...
	print(r.text)

def updateCSVpredict1():
	#r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/predict10noLabels.csv")
	r = requests.post('http://127.0.0.1:8080/updateCSV',data = "s1,w1,./input/predict30percentNoLabels.csv")
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

def statistics():
	r = requests.post('http://127.0.0.1:8080/statistics',data = "getStatistics")
	print(r.text)




while 1:
	action = input("insert action: ")
	if action == "init1":
		updateCSVtrain()	
	elif action == "init2":
		updateCSVpredict()
	elif action == "init3":
		updateCSVpredict1()
	elif action == "train":
		clientsTraining()

	elif action == "predict":
		clientsPredict()

	elif action == "start":
		startCasting()

	elif action == "stop":
		stopCasting()

	elif action == "stats":
		statistics()






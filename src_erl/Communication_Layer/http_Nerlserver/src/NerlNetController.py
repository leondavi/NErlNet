import requests


def updateCSV():
	r = requests.post('http://192.168.0.112:8080/updateCSV',data = "source1,worker1,worker2,./input/shuffled-input99.csv")
	# r2 = requests.post('http://192.168.0.112:8080/updateCSV',data = "source2,worker5,worker7, ... ,./input/input4.csv")...
	print(r.text)

def clientsTraining():
	r = requests.post('http://192.168.0.112:8080/clientsTraining',data = "")
	print(r.text)


def  clientsPredict():
	r = requests.post('http://192.168.0.112:8080/clientsPredict',data = "")
	print(r.text)

def startCasting():
	r = requests.post('http://192.168.0.112:8080/startCasting',data = "source1")
	print(r.text)

def stopCasting():
	r = requests.post('http://192.168.0.112:8080/stopCasting',data = "source1")
	print(r.text)




while 1:
	action = input("insert action: ")
	if action == "init":
		updateCSV()
	elif action == "training":
		clientsTraining()

	elif action == "predict":
		clientsPredict()

	elif action == "start Casting":
		startCasting()

	elif action == "stop Casting":
		stopCasting()






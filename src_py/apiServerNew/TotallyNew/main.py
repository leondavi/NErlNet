import transmitter as trans
from apiServer import ApiServer


defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:5000'

ApiServer()



#if __name__ == "__main__":
#    trans.testPost(baseReceiverAddress + '/test', 0)
#    trans.testQueue(baseReceiverAddress + '/testQueue')
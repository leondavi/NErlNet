from transmitter import Transmitter


defAddress = 'https://httpbin.org/post' #httpbin is a website aimed for testing HTTP requests
baseReceiverAddress = 'http://127.0.0.1:8095'

trans = Transmitter()

trans.testPost(baseReceiverAddress + '/test', 0)

result2 = trans.testQueue(baseReceiverAddress + '/testQueue')
print(result2)





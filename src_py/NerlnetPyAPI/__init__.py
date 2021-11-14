"""
if you want to test it if its working, you could use postman
   just need to specify the correct url and the methode calling [post, get]
"""
import sys
import threading
import time
from socketserver import ThreadingMixIn
import requests
import json
import NerlnetPyAPI.settings

in1 = ''
in2 = ''
in3 = ''
in4 = ''

serveQueue = []
globvarN = 0
port = 0

if sys.version_info[0] < 3:
    # python 2 import
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
else:
    # python 3 import
    from http.server import BaseHTTPRequestHandler, HTTPServer


class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
    pass


class BaseServer(BaseHTTPRequestHandler):

    def _set_headers(self): #Generate a map of header: value
        self.send_response(200) #Send status code 200, for success
        self.send_header('Content-type', 'text/html') #Specify the type of header, and its value
        self.end_headers() #End headers list

    def do_GET(self): #Defining the response to a get request
        message = threading.currentThread().getName() #The message is the current thread's name
        self.wfile.write(message) #Display the message (thread's name)
        self.wfile.write('\n') 

    def do_HEAD(self):
        self._set_headers()

    def do_POST(self):
        contentLength = int(self.headers['Content-Length']) #The length of our text
        postData = self.rfile.read(contentLength)
        self._set_headers()
        self.wfile.write("<html><body><p>POST</p><p>%s</p></body></html>"
                         .encode('utf-8') % postData) #Write "Post", and than the contentLength
        # print(NerlnetPyAPI.settings.lenOfRequests)
        # print(postData.decode())
        if postData.decode() == 'ack':
            print("ack received")
            NerlnetPyAPI.settings.ackQueue.append(str(postData.decode()))
            NerlnetPyAPI.settings.lenOfRequests = NerlnetPyAPI.settings.lenOfRequests - 1

        else:
            strSplited = splitStr(postData.decode())
            if strSplited[0] == 'statistics':
                NerlnetPyAPI.settings.statistics.append(strSplited[1])
            elif not NerlnetPyAPI.settings.losDict:
                splitedStr = splitStr(postData.decode())
                NerlnetPyAPI.settings.losDict.update({splitedStr[1]: [splitedStr[1]]})
            else:
                splitedStr = splitStr(postData.decode())
                curr = NerlnetPyAPI.settings.losDict.get(splitedStr[0])
                NerlnetPyAPI.settings.losDict.update({splitedStr[1]: [curr] + [splitedStr[1]]})

        print(postData.decode())
        print(NerlnetPyAPI.settings.lenOfRequests)
        print(NerlnetPyAPI.settings.ackQueue)
        if postData == b'exit':
            serverRun(True, ThreadedHTTPServer(('localhost', port), BaseServer))
        else:
            pass


def run(port=80, interrupted=False):
    server = ThreadedHTTPServer(('localhost', port), BaseServer)
    print('Starting server, use <Ctrl-C> to stop')
    print('HTTP server running on port %s' % port)
    serverRun(interrupted, server)


def splitStr(word):
    splitedArr = word.split('#')
    stripSplitedSTR = []
    for str in splitedArr:
        stripSplitedSTR.append(str.strip())
    return stripSplitedSTR


def serverRun(interrupted, server):
    if not interrupted:
        server.serve_forever()
        serveQueue.append(server)
    else:
        for _ in serveQueue:
            ThreadedHTTPServer().shutdown()


def creatJson():
    creat()


def initData():
    print('/***/ data initialization started..')
    s = ""
    while True:
        exit = input('Do you want to add another source (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            source = input('please enter source name: ')
            s = s + source + ','
    while True:
        exit = input('Do you want to add another worker (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            worker = input('please enter worker name: ')
            s = s + worker + ','
    return s


def init(jasonPath): pass


# data = initData()  # ask tal about the accurate data
# data = initData()  # ask tal about the accurate data
# new_list = [requests.post('http://127.0.0.1:8080/updateCSV', data="s1,w1,w2,./input/shuffled-input9.csv"),
#             requests.post('http://127.0.0.1:8080/updateCSV', data="s2,w3,w4,./input/shuffled-input9.csv"),
#             requests.post('http://127.0.0.1:8080/updateCSV', data="s3,w5,w6,./input/shuffled-input9.csv"),
#             requests.post('http://127.0.0.1:8080/updateCSV', data="s4,w7,w8,./input/shuffled-input9.csv")]
# for request in new_list:
#     print(request.text)
# NerlnetPyAPI.settings.x = len(new_list)
# print(NerlnetPyAPI.settings.x)


def clientsTraining():
    r = requests.post('http://192.168.0.107:8080/clientsTraining', data="")
    print(r.text)


def creat():
    def get_connectionsMap_client_headr():
        print("ConnectionsMap CLIENTS")
        global in1
        in1 = input("Enter header client name:")
        return in1

    def get_connectionsMap_server_headr():
        print("ConnectionsMap SERVERS")
        global in2
        in2 = input("Enter server name:")
        return in2

    def get_connectionsMap_mainServer_headr():
        print("ConnectionsMap MAIN SERVER")
        global in3
        in3 = input("Enter mainServer Name:")
        return in3

    def get_connectionsMap_serverApi_headr():
        print("ConnectionsMap SERVER API")
        global in4
        in4 = input("Enter server api Name:")
        return in4

    def get_devices_inputs():
        d = {'host': input("Enter host:"), 'entities': input('Enter entities list:')}
        return d

    def get_serverAPI_inputs():
        c = {'host': input('Host:'), 'port': input('Port:'), 'args': input('Enter args list:')}
        return c

    def get_mainServer_inputs():
        g = {'host': input('Host:'), 'port': input('Port:'), 'args': input('Enter args list:')}
        return g

    def get_worker_inputs():
        f = {'name': input("Enter worker (cppSANNStatem) name:"), 'args': input('Enter args list:')}
        return f

    def get_client_inputs():
        t = {'name': input("Enter client name:"), 'port': input('Port:'), 'workers': input('Enter workers List:')}
        return t

    def get_source_inputs():
        l = {'name': input("Enter source name:"), 'port': input('Port:')}
        return l

    def get_router_inputs():
        s = {'name': input("Enter Machine Name:"), 'host': input('Host:'), 'port': input('Port:')}
        return s

    def get_connectionsMap_client():
        print("ConnectionsMap CLIENTS arguments")
        x1 = {'mainServer': input("Enter mainServer name:")}
        return x1

    def get_connectionsMap_server():
        print("ConnectionsMap SERVERS arguments")
        x2 = {in3: input("Enter mainServer name:"), in1: input("Enter client name:")}
        return x2

    def get_connectionsMap_mainServer():
        print("ConnectionsMap MAIN SERVER arguments")
        x3 = {in4: input("Enter mainServer name:"),
              in1: input("Enter server name:"),
              in2: input("Enter client name:")}
        return x3

    def get_connectionsMap_serverApi():
        print("ConnectionsMap SERVER API arguments")
        x4 = {in3: input("Enter mainServer name:")}
        return x4

    out = {"devices": [],
           "serverAPI": [],
           "mainServer": [],
           "cppSANNStatem": [],
           "clients": [],
           "sources": [],
           "routers": [],
           "connectionsMap": [{in1: {}}, {in2: {}}, {in3: {}}, {in4: {}}]
           }

    while True:
        exit = input('Do you want to add another device (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            d = get_devices_inputs()
            out["devices"].append(d)

    while True:
        exit = input('Do you want to add another serverAPI (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            c = get_serverAPI_inputs()
            out["serverAPI"].append(c)

    while True:
        exit = input('Do you want to add another mainServer (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            g = get_mainServer_inputs()
            out["mainServer"].append(g)

    while True:
        exit = input('Do you want to add another worker (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            f = get_worker_inputs()
            out["cppSANNStatem"].append(f)

    while True:
        exit = input('Do you want to add another client (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            t = get_client_inputs()
            out["clients"].append(t)

    while True:
        exit = input('Do you want to add another source (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            l = get_source_inputs()
            out["sources"].append(l)

    while True:
        exit = input('Do you want to add another router (y/n)? ')
        if exit.lower() == 'n':
            break
        else:
            s = get_router_inputs()
            out["routers"].append(s)

    exit = input('Please add you connectionMap')
    get_connectionsMap_client_headr()
    get_connectionsMap_server_headr()
    get_connectionsMap_mainServer_headr()
    get_connectionsMap_serverApi_headr()

    y1 = get_connectionsMap_client()
    out['connectionsMap'][0][in1] = out['connectionsMap'][0]['']
    del out['connectionsMap'][0]['']
    out["connectionsMap"][0][in1] = y1

    y2 = get_connectionsMap_server()
    out['connectionsMap'][1][in2] = out['connectionsMap'][1]['']
    del out['connectionsMap'][1]['']
    out["connectionsMap"][1][in2] = y2

    y3 = get_connectionsMap_mainServer()
    out['connectionsMap'][2][in3] = out['connectionsMap'][2]['']
    del out['connectionsMap'][2]['']
    out["connectionsMap"][2][in3] = y3

    y4 = get_connectionsMap_serverApi()
    out['connectionsMap'][3][in4] = out['connectionsMap'][3]['']
    del out['connectionsMap'][3]['']
    out["connectionsMap"][3][in4] = y4

    with open('architectures.json', 'w') as f:
        json.dump(out, f, indent=4)
        created = True
        return created


g_c = 0


class Queue():

    def glob(val):
        global g_c
        g_c = val
        print(g_c)

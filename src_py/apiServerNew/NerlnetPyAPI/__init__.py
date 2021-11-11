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
import NerlnetPyAPI.globalObjects

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

    def _set_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()

    def do_GET(self):
        print("do")
        time.sleep(5)
        message = threading.currentThread().getName()
        self.wfile.write(message)
        self.wfile.write('\n')

    def do_HEAD(self):
        self._set_headers()

    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)
        self._set_headers()
        self.wfile.write("<html><body><p>POST!</p><p>%s</p></body></html>"
                         .encode('utf-8') % post_data)
        print(post_data.decode())
        if post_data == b'exit':
            serverRun(True, ThreadedHTTPServer(('localhost', port), BaseServer))
        else:
            pass


def run(port=8080, interrupted=False):
    server = ThreadedHTTPServer(('localhost', port), BaseServer)
    print('Starting server, use <Ctrl-C> to stop')
    print('HTTP server running on port %s' % port)
    serverRun(interrupted, server)

def serverRun(interrupted, server):
    if not interrupted:
        server.serve_forever()
        serveQueue.append(server)
    else:
        for _ in serveQueue:
            ThreadedHTTPServer().shutdown()

g_c = 0


class Queue():

    def glob(val):
        global g_c
        g_c = val
        print(g_c)

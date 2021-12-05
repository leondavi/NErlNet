import queue
from multiprocessing import Process, Manager

manager = Manager()
q = manager.Queue()
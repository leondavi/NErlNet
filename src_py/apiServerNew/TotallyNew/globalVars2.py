from multiprocessing import Process, Manager

manager = Manager()
managerQueue = manager.Queue()
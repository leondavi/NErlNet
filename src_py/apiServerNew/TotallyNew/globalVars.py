#import queue
import redis
from rq import Queue

#ackQueue = queue.Queue()
r = redis.Redis()
queue = Queue(connection=r)


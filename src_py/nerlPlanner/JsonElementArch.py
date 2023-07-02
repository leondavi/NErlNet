from JsonElements import JsonElement
from JsonElementsDefinitions import *
import json
from collections import OrderedDict

class JsonElementArchitecture(JsonElement):
    def __init__(self, name):
        super(JsonElementArchitecture, self).__init__(name, WORKER_TYPE)
        pass

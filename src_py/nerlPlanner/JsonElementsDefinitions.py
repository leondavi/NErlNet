from collections import OrderedDict

ARGS_TYPE = 0
VALUE_TYPE = 1
IP_TYPE = 2
PORT_TYPE = 3
POLICY_TYPE = 4
DEVICE_TYPE = 5
ROUTER_TYPE = 6
CLIENT_TYPE = 7
SOURCE_TYPE = 8
WORKER_TYPE = 9
MAIN_SERVER_TYPE = 10
API_SERVER_TYPE = 11
SPECIAL_ENTITY_TYPE = 12
NONE_TYPE = -1

def comm_entity_type(in_type) -> bool:
    '''
    returns true if the input type is a communication supported type
    '''
    comm_types_list = [ROUTER_TYPE, CLIENT_TYPE, SOURCE_TYPE, MAIN_SERVER_TYPE, API_SERVER_TYPE]
    return True if in_type in comm_types_list else False

RouterPolicyDict = {
    "Shortest-Path":0,
    "Routing-Table":1
}

SourcePolicyDict = {
    "Casting":0,
    "Round-Robin":1
}

SOURCE_TYPE_DICT_DEFAULT_SOURCE_TYPE = "csv"
SourceTypeDict = {
    "csv" : 0,
    "cam-dummy" : 1
}

def get_inv_dict(in_dict):
    Keys = list(in_dict.keys())
    Values = list(in_dict.values())
    newDict = []
    for value, key in zip(Values,Keys):
        newDict.append((value,key))
    return dict(newDict)

FIELD_NAME_SOURCE_TYPE = 'type'
FIELD_NAME_ENTITIES = 'entities'
FIELD_NAME_WORKERS = 'workers'

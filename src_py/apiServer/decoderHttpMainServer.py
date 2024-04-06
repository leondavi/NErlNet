
import numpy as np
from decoderHttpMainServerDefs import *


# | seperates entities
# & start entities stats
# # seperates (stat_name-stat_value-type) -> triplets for entity without workers
# : seperates stat_name , stat_value and type
# ^ seperates workers belong to client 
#  Only for workers:
# @ seperates (stat_name, stat_value and type) -> worker_triplets for worker
# $ seperates stat_name, stat_value and type

def split_results_to_entities_chunks(string_to_convert : str) -> list:
    return [x for x in string_to_convert.split(SEP_PHASE_ENTITY_DATA) if x]

# Todo: fix all examples in the function
def decode_main_server_ets_str(string_to_convert: str):
    result_dict = {} # {entity_name: ets_dict,...}

    entity_with_stats_list = string_to_convert.split(SEP_ENTITY_OR_STATS)[:-1]
    for entity_with_stats in entity_with_stats_list:
        # Example: entity_with_stats = c2&bytes_sent:0:int#messages_sent:8015:int#bad_messages:0:int#batches_sent:0:int...
        if SEP_ENTITY_AND_STATS in entity_with_stats:
            # Example: entity_with_stats = c2&bytes_sent:0:int#messages_sent:8015:int#bad_messages:0:int#batches_sent:0:int...
            entity_name = entity_with_stats.split(SEP_ENTITY_AND_STATS)[0] # Example: c2
            entity_stats = entity_with_stats.split(SEP_ENTITY_AND_STATS)[1] # Example: bytes_sent:0:int#messages_sent:8015:int#bad_messages:0:int#batches_sent:0:int...
            triplets = entity_stats.split(SEP_ENTITY_HASH_STATS)[:-1]  # Example: [bytes_sent:0:int, messages_sent:8015:int, bad_messages:0:int, batches_sent:0:int, ...]
            entity_dict = {}
            
            for triplet in triplets:
                key, value, value_type = triplet.split(SEP_ENTITY_COLON_STATS) # Example: [bytes_sent,0,int]
                if value_type == 'string':
                    value = value
                else:
                    value = float(value) if value_type == 'float' else int(value)
                entity_dict[key] = value  # Key is always a string
                
            result_dict[entity_name] = entity_dict
            
        if  SEP_ENTITY_XOR_STATS in entity_with_stats: # Belongs only to workers
            # Example: entity_with_stats = w4^bytes_sent$0$int@empty_batches$0$int@...
            worker_name = entity_with_stats.split(SEP_ENTITY_XOR_STATS)[0]
            worker_stats = entity_with_stats.split(SEP_ENTITY_XOR_STATS)[1]
            worker_triplets = worker_stats.split(SEP_ENTITY_AT_STATS)[:-1] # Example: [bytes_sent$0$int, empty_batches$0$int, ...]
            worker_dict = {}
            
            for worker_triplet in worker_triplets:
                key, value, value_type = worker_triplet.split(SEP_ENTITY_DOLLAR_STATS) # Example: [bytes_sent,0,int]
                if value_type == 'string':
                    value = value
                else:
                    value = float(value) if value_type == 'float' else int(value)
                worker_dict[key] = value  # Key is always a string
                
            result_dict[worker_name] = worker_dict

    return result_dict

def parse_key_string(key_string: str) -> tuple:
    WORKER_NAME_IDX = 0
    SOURCE_NAME_IDX = 1
    BATCH_ID_IDX = 2
    BATCH_TS_IDX = 3
    DURATION_IDX = 4 # TimeNIF
    NERLTENSOR_TYPE_IDX = 5

    definitions_list = key_string.split(SEP_ENTITY_HASH_STATS)
    worker_name = definitions_list[WORKER_NAME_IDX]
    source_name = definitions_list[SOURCE_NAME_IDX]
    batch_id = definitions_list[BATCH_ID_IDX]
    batch_ts = definitions_list[BATCH_TS_IDX]
    duration = definitions_list[DURATION_IDX]
    nerltensor_type = definitions_list[NERLTENSOR_TYPE_IDX]

    return worker_name, source_name, batch_id, batch_ts, duration, nerltensor_type


def decode_phase_result_data_json_from_main_server(input_json_dict : dict) -> list:
    decoded_data = []
    DIMS_LENGTH = 3
    for key_string, nerltensor in input_json_dict.items():
        worker_name, source_name, batch_id, batch_ts, duration, nerltensor_type = parse_key_string(key_string)
        duration = int(float(duration)) # from here duration is int in micro seconds

        # nerltensor to numpy tensor conversion
        np_tensor = None
        nerltensor_as_bytes = bytes(nerltensor)
        if nerltensor_type == 'float':
            np_tensor = np.frombuffer(nerltensor_as_bytes, dtype=np.float32)
        elif nerltensor_type == 'int':
            np_tensor = np.frombuffer(nerltensor_as_bytes, dtype=np.int32)
        elif nerltensor_type == 'double':
            np_tensor = np.frombuffer(nerltensor_as_bytes, dtype=np.float64)

        dims = np_tensor[:DIMS_LENGTH].astype(int)
        np_tensor = np_tensor[DIMS_LENGTH:]
        np_tensor = np_tensor.reshape(dims) # reshaped

        decoded_data.append((worker_name, source_name, duration, batch_id, batch_ts, np_tensor))
    return decoded_data
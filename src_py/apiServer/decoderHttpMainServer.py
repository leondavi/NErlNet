
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

def decode_main_server_str_train(string_to_convert: str) -> list:  #change to tuple of 5 elements
    result_list = []
    worker_name = string_to_convert.split(SEP_ENTITY_HASH_STATS)[0]
    result_list.append(worker_name)
    train_result = string_to_convert.split(SEP_ENTITY_HASH_STATS)[1]
    source_and_stats = train_result.split(SEP_ENTITY_OR_STATS)
    source_name = source_and_stats[0]
    result_list.append(source_name)
    loss_val = source_and_stats[1]
    tensor_data = np.float32(loss_val)
    result_list.append(tensor_data)
    duration = source_and_stats[2]
    result_list.append(duration)
    batch_id = source_and_stats[3]
    result_list.append(batch_id)
    return result_list

def test():
    string_to_convert = "c2&bytes_sent:54036:int#messages_sent:8016:int#bad_messages:0:int#batches_sent:0:int#messages_received:8016:int#batches_received:0:int#bytes_received:40022:int#batches_dropped:0:int#messages_dropped:0:int#|w4^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@w3^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@|s1&bytes_sent:0:int#messages_sent:4:int#bad_messages:0:int#batches_sent:8004:int#messages_received:7:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|c1&bytes_sent:54036:int#messages_sent:8016:int#bad_messages:0:int#batches_sent:0:int#messages_received:8016:int#batches_received:0:int#bytes_received:40022:int#batches_dropped:0:int#messages_dropped:0:int#|w2^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@w1^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@|r4&bytes_sent:0:int#messages_sent:8014:int#bad_messages:0:int#batches_sent:0:int#messages_received:8014:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|r3&bytes_sent:0:int#messages_sent:8015:int#bad_messages:0:int#batches_sent:0:int#messages_received:8015:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|r2&bytes_sent:0:int#messages_sent:12025:int#bad_messages:0:int#batches_sent:0:int#messages_received:12024:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|r1&bytes_sent:0:int#messages_sent:20043:int#bad_messages:0:int#batches_sent:0:int#messages_received:20038:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|mainServer&bytes_sent:0:int#messages_sent:8017:int#bad_messages:0:int#batches_sent:0:int#messages_received:8035:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|"
    print(decode_main_server_ets_str(string_to_convert))
    
if __name__ == "__main__":
    test()
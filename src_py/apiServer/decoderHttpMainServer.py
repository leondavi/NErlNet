
from decoderHttpMainServerDefs import *

# ? Example: "name:mainServer:string#bytes_sent:0.5:float#....|name:c1:string#bytes_sent:0.5:float#....#w1&average_time_training:10.5:float#...|w2&...|name:c2:string#...#w3&...|"
# | seperates entities
# & start worker stats under his client
# # seperates stat_name-stat_value-type triplets
# : seperates stat_name , stat_value and type

# Todo: fix all examples in the function
def decode_main_server_ets_str(string_to_convert: str):
    result_dict = {} # {entity_name: ets_dict,...}

    # Split the input string into individual key-value pairs
    entity_stats_list = string_to_convert.split("|")[:-1]
    for entity_stats in entity_stats_list:
        # Todo check why it is not working - print 3 times
        if "name" in entity_stats:
            # Example: entity_stats = name:mainServer:string#bytes_sent:0.5:float#....
            triplets = entity_stats.split('#')  # Remove the last #
            entity_dict = {}
            entity_name = triplets[0].split(":")[1] # example: name:mainServer:string -> mainServer
            triplets = triplets[1:-1] # without entity_name 
            # Example: triplets = [name:mainServer:string, ...]
            for triplet in triplets:
                if "&" in triplet:
                    # Example: triplet = w1&average_time_training$10.5$float@average_time_predict$11.1$float@...
                    worker_name = triplet.split('&')[0] # worker name
                    worker_stats = triplet.split('&')[1] # worker stats , Example: average_time_training$10.5$float@average_time_predict$11.1$float@...
                    worker_triplets = worker_stats.split('@') # Example: [average_time_training$10.5$float, average_time_predict$11.1$float, ...]
                    worker_dict = {}
                    for worker_triplet in worker_triplets:
                        key, value, value_type = worker_triplet.split('$') # Example: [average_time_training,10.5,float] 
                        if value_type == 'string':
                            value = value
                        else:
                            value = float(value) if value_type == 'float' else int(value)
                        worker_dict[key] = value  # Key is always a string
                    result_dict[worker_name] = worker_dict
                else:
                    key, value, value_type = triplet.split(':')
                    if value_type == 'string':
                        value = value
                    else:
                        value = float(value) if value_type == 'float' else int(value)
                    entity_dict[key] = value  # Key is always a string
                    result_dict[entity_name] = entity_dict

    return result_dict


def test():
    string_to_convert = "name:mainServer:string#bytes_sent:0.5:float#bad_messages:0:int#batches_sent:0:int#batches_received:0:int#bytes_received:0:int#message_received:0:int#batches_dropped:0:int#message_dropped:0:int#message_sent:0:int#|name:c1:string#bytes_sent:0.5:float#bad_messages:0:int#batches_sent:0:int#batches_received:0:int#bytes_received:0:int#message_received:0:int#batches_dropped:0:int#message_dropped:0:int#message_sent:0:int#|name:c2:string#bytes_sent:0.5:float#bad_messages:0:int#batches_sent:0:int#batches_received:0:int#bytes_received:0:int#message_received:0:int#batches_dropped:0:int#message_dropped:0:int#message_sent:0:int#w1&average_time_training$10.5$float@average_time_predict$11.1$float#w2&average_time_training$10.5$float@average_time_predict$11.1$float#w3&average_time_training$10.5$float@average_time_predict$11.1$float#|"
    print(decode_main_server_ets_str(string_to_convert))
    
if __name__ == "__main__":
    test()
    #print(decode_main_server_ets_str("name:c1:string#bytes_sent:5.2:float#bad_messages:0:int#batches_sent:0:int#batches_received:0:int#bytes_received:0:int#message_received:0:int#batches_dropped:0:int#message_dropped:0:int#message_sent:0:int#"))
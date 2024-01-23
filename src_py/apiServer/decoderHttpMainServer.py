
from decoderHttpMainServerDefs import *

# ? Example: "name:mainServer:string#bytes_sent:0.5:float#....|name:c1:string#bytes_sent:0.5:float#....#w1&average_time_training:10.5:float#...|w2&...|name:c2:string#...#w3&...|"
# | seperates entities
# & start worker stats under his client
# # seperates stat_name-stat_value-type triplets
# : seperates stat_name , stat_value and type

def decode_main_server_ets_str(string_to_convert: str):
    result_list = [] # [(entity_name, ets_dict), ...)]

    # Split the input string into individual key-value pairs
    pairs = string_to_convert.split('#')  # Remove the last #
    for pair in pairs:
        if pair:
            key, value, value_type = pair.split(':')
            if value_type == 'string':
                value = value
            else:
                value = float(value) if value_type == 'float' else int(value)
            result_list.append((key, value))

    return result_list

if __name__ == "__main__":
    print(decode_main_server_ets_str("name:c1:string#bytes_sent:5.2:float#bad_messages:0:int#batches_sent:0:int#batches_received:0:int#bytes_received:0:int#message_received:0:int#batches_dropped:0:int#message_dropped:0:int#message_sent:0:int#"))
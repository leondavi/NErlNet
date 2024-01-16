
from decoderHttpMainServerDefs import *

def decode_main_server_ets_str(string_to_convert: str):
    result_list = []

    # Split the input string into individual key-value pairs
    pairs = string_to_convert.split('#')  # Remove the last #
    for pair in pairs:
        if pair:
            key, value, value_type = pair.split(':')
            value = float(value) if value_type == 'float' else int(value)
            result_list.append((key, value))

    return dict(result_list)

if __name__ == "__main__":
    print(decode_main_server_ets_str("bytes_sent:5.2:float#bad_messages:0:int#batches_sent:0:int#batches_received:0:int#bytes_received:0:int#message_received:0:int#batches_dropped:0:int#message_dropped:0:int#message_sent:0:int#"))
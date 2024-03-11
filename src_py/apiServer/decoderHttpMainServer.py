
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

def decode_main_server_str_train(string_to_convert: str) -> tuple:  #change to tuple of 5 elements
    worker_name = string_to_convert.split(SEP_ENTITY_HASH_STATS)[0]
    train_result = string_to_convert.split(SEP_ENTITY_HASH_STATS)[1]
    source_and_stats = train_result.split(SEP_ENTITY_OR_STATS)
    source_name = source_and_stats[0]
    tensor_to_convert = source_and_stats[1]
    tensor_to_convert = tensor_to_convert.split(SEP_ENTITY_LEFT_BRACKETS_STATS)[1]
    tensor_to_convert = tensor_to_convert.split(SEP_ENTITY_RIGHT_BRACKETS_STATS)[0]
    tensor_dim = tensor_to_convert.split(SEP_ENTITY_COMMA_STATS)[:3]
    tensor_dim = list(map(int, map(float, tensor_dim))) # convert arguments to int
    tensor_data = np.float32(tensor_to_convert.split(SEP_ENTITY_COMMA_STATS)[3:])
    tensor_data = tensor_data.reshape(tensor_dim)
    duration = int(float(source_and_stats[2])) # micro seconds
    batch_id = source_and_stats[3]
    batch_timestamp = source_and_stats[4]
    return source_name, tensor_data, duration, batch_id, worker_name, batch_timestamp


def decode_main_server_str_predict(string_to_convert: str) -> tuple:  
    worker_name = string_to_convert.split(SEP_ENTITY_HASH_STATS)[0]
    string_to_convert = string_to_convert.split(SEP_ENTITY_HASH_STATS)[1]
    source_name = string_to_convert.split(SEP_ENTITY_OR_STATS)[0]
    tensor_list = string_to_convert.split(SEP_ENTITY_OR_STATS)[2:]
    string_to_convert = string_to_convert.split(SEP_ENTITY_OR_STATS)[1]
    string_to_convert = string_to_convert.split(SEP_ENTITY_LEFT_BRACKETS_STATS)[1]
    tensor_to_convert = string_to_convert.split(SEP_ENTITY_OR_STATS)[0]
    tensor_to_convert = tensor_to_convert.split(SEP_ENTITY_RIGHT_BRACKETS_STATS)[0]
    tensor_dim = tensor_to_convert.split(SEP_ENTITY_COMMA_STATS)[:3]
    tensor_dim = list(map(int, map(float, tensor_dim))) # convert arguments to int
    tensor_data = np.float32(tensor_to_convert.split(SEP_ENTITY_COMMA_STATS)[3:])
    tensor_data = tensor_data.reshape(tensor_dim)
    duration = int(float(tensor_list[0])) 
    batch_id = tensor_list[1]
    batch_timestamp = tensor_list[2]
    return worker_name, source_name, tensor_data, duration, batch_id, batch_timestamp

def test():
    string_to_convert = "c2&bytes_sent:54036:int#messages_sent:8016:int#bad_messages:0:int#batches_sent:0:int#messages_received:8016:int#batches_received:0:int#bytes_received:40022:int#batches_dropped:0:int#messages_dropped:0:int#|w4^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@w3^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@|s1&bytes_sent:0:int#messages_sent:4:int#bad_messages:0:int#batches_sent:8004:int#messages_received:7:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|c1&bytes_sent:54036:int#messages_sent:8016:int#bad_messages:0:int#batches_sent:0:int#messages_received:8016:int#batches_received:0:int#bytes_received:40022:int#batches_dropped:0:int#messages_dropped:0:int#|w2^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@w1^bytes_sent$0$int@empty_batches$0$int@bad_messages$0$int@batches_dropped_predict$0$int@batches_sent_predict$0$int@nan_loss_count$0$int@batches_received_predict$1001$int@batches_dropped_train$0$int@acc_time_training$0$int@bytes_received$0$int@average_time_prediction$0$int@batches_received_train$1000$int@average_time_training$0$int@acc_time_prediction$0$int@batches_sent_train$0$int@|r4&bytes_sent:0:int#messages_sent:8014:int#bad_messages:0:int#batches_sent:0:int#messages_received:8014:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|r3&bytes_sent:0:int#messages_sent:8015:int#bad_messages:0:int#batches_sent:0:int#messages_received:8015:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|r2&bytes_sent:0:int#messages_sent:12025:int#bad_messages:0:int#batches_sent:0:int#messages_received:12024:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|r1&bytes_sent:0:int#messages_sent:20043:int#bad_messages:0:int#batches_sent:0:int#messages_received:20038:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|mainServer&bytes_sent:0:int#messages_sent:8017:int#bad_messages:0:int#batches_sent:0:int#messages_received:8035:int#batches_received:0:int#bytes_received:0:int#batches_dropped:0:int#messages_dropped:0:int#|"
    print(decode_main_server_ets_str(string_to_convert))

def predict_test():
    string_to_convert = "w4#s1|[50.0,3.0,1.0,0.5004591941833496,0.5055416822433472,0.5376569628715515,0.49915972352027893,0.5077085494995117,0.5408108234405518,0.498697429895401,0.5071896314620972,0.5403850078582764,0.49878209829330444,0.507079541683197,0.5403009057044983,0.5090339779853821,0.5019631385803223,0.5317938327789307,0.4991932511329651,0.5058079361915588,0.5380713939666748,0.5102575421333313,0.502470076084137,0.5314392447471619,0.4987733066082001,0.506700336933136,0.539694607257843,0.4988332986831665,0.5068778395652771,0.5397170782089233,0.5115110278129578,0.5001749396324158,0.5299235582351685,0.5100616216659546,0.5036847591400146,0.5322853922843933,0.4996839165687561,0.5060247182846069,0.5393354892730713,0.4990982115268707,0.5066022872924805,0.5398479104042053,0.4989696741104126,0.5065380334854126,0.5395784378051758,0.5010527968406677,0.5054836273193359,0.5372504591941833,0.4985913932323456,0.5069376826286316,0.54018235206604,0.5012227296829224,0.505314826965332,0.5369112491607666,0.5080367922782898,0.5025777220726013,0.5326507091522217,0.4991578459739685,0.50767582654953,0.5407848954200745,0.4985300898551941,0.5068618059158325,0.5397419333457947,0.499387264251709,0.5063247084617615,0.5396109819412231,0.4990617036819458,0.5060428977012634,0.5384480357170105,0.5068861246109009,0.5039123296737671,0.5336389541625977,0.5120653510093689,0.4997715950012207,0.5292671322822571,0.5106130838394165,0.5020105838775635,0.5310877561569214,0.5007823705673218,0.5053133368492126,0.5370851755142212,0.4985859990119934,0.5071172714233398,0.5403240323066711,0.4983019530773163,0.5070235133171082,0.5399211645126343,0.4998704493045807,0.5056422352790833,0.5386389493942261,0.4985085129737854,0.5069769620895386,0.5402105450630188,0.5019485950469971,0.5052040815353394,0.5362134575843811,0.4986557066440582,0.5067465305328369,0.5396547913551331,0.5023921132087708,0.5049285888671875,0.5357553958892822,0.49849677085876465,0.5068165063858032,0.5398951172828674,0.49871230125427246,0.5071614384651184,0.5403632521629333,0.4995129406452179,0.5061236023902893,0.5393246412277222,0.509457528591156,0.4999198019504547,0.5306822061538696,0.5007544755935669,0.5036685466766357,0.5361377000808716,0.5095479488372803,0.5030366778373718,0.5320889949798584,0.5009932518005371,0.5052581429481506,0.5367897152900696,0.4990951716899872,0.5064340233802795,0.5395112037658691,0.5006503462791443,0.5056132078170776,0.5375162363052368,0.5101680755615234,0.5024511814117432,0.5315715670585632,0.49863865971565247,0.5067603588104248,0.5396633148193359,0.5009318590164185,0.5054137110710144,0.537749171257019,0.4986039102077484,0.5069242715835571,0.5401721000671387,0.5114684700965881,0.5006999373435974,0.5299604535102844,0.5010262131690979,0.5053548216819763,0.5366659164428711,0.5009362101554871,0.5054230093955994,0.537044882774353,0.5086351037025452,0.502194881439209,0.5321375131607056]|89|4|2"
    print(decode_main_server_str_predict(string_to_convert))

def train_test():
    string_to_convert = "w1#s1|[1.0,1.0,1.0,0.24699141085147858]|7.19000000000000000000e+02|1|1708807553182354940"
    print(decode_main_server_str_train(string_to_convert))

if __name__ == "__main__":
    train_test()
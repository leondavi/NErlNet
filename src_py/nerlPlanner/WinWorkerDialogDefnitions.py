

# Maps are based on src_cpp/opennnBridge/definitionsNN.h

LayerTypeMap = {
    "Default" : "0",
    "Scaling none" : "1-1",
    "Scaling MinMax" : "1-2",
    "Scaling MeanStd" : "1-3",
    "Scaling STD" : "1-4",
    "Scaling Log" : "1-5",
    "CNN" : "2",
    "Perceptron" : "3",
    "Pooling none" : "4-1",
    "Pooling max" : "4-2",
    "Pooling avg" : "4-3",
    "Probabilistic" : "5",
    "LSTM" : "6",
    "RNN" : "7",
    "Unscaling" : "8",
    "Bounding" : "9"
}

ActivationFunctionsMap = {
    "Threshold" : 1,
    "Sign" : 2,
    "Logistic" : 3,
    "Tanh" : 4,
    "Linear" : 5,
    "ReLU" : 6,
    "eLU" : 7,
    "SeLU" : 8,
    "Soft-plus" : 9,
    "Soft-sign" : 10,
    "Hard-sigmoid" : 11,
}

ModelTypeMapping = {
    "approximation" : 1,
    "classification" : 2,
    "forecasting" : 3,
    "encoder_decoder" : 4,
    "nn" : 5,
    "autoencoder" : 6,
    "ae-classifier" : 7,
    "fed-client": 8,
    "fed-server": 9
}


# Action Keys

KEY_MODEL_TYPE_LIST_BOX = '-MODEL-TYPE-LIST-BOX-'

KEY_LAYER_SIZES_INPUT = '-LAYERS-SIZES-INPUT-'
KEY_NUM_OF_LAYERS_SIZES = '-NUM-OF-LAYERS-SIZES-'
KEY_LAYER_TYPE_SELECTION = '-LAYER-TYPE-SELECTION-'
KEY_LAYER_TYPE_SELECTION_ADD = '-LAYER-TYPE-SELECTION-ADD-'
KEY_LAYER_TYPE_HELP = '-LAYER-TYPE-HELP-'
KEY_LAYER_TYPE_SELECTION_CLEAR = '-LAYER-TYPE-SELECTION-CLEAR-'
KEY_LAYER_TYPE_CODES_INPUT = '-LAYER-TYPE-CODES-INPUT-'
KEY_NUM_OF_LAYERS_TYPES = '-NUM-OF-LAYERS-TYPES-'

KEY_ACTIVATION_LAYER_SELECTION = '-ACTIVATION-LAYER-SELECTION-'
KEY_ACTIVATION_LAYER_SELECTION_ADD = '-ACTIVATION-LAYER-SELECTION-ADD-'
KEY_ACTIVATION_LAYER_HELP = '-ACTIVATION-LAYER-HELP-'
KEY_ACTIVATION_LAYER_SELECTION_CLEAR = '-ACTIVATION-LAYER-SELECTION-CLEAR-'
KEY_ACTIVATION_CODES_INPUT = '-ACTIVATION-CODES-INPUT-'
KEY_ACTIVATION_NUMOF_LAYERS = 'ACTIVATION-NUMOF-LAYERS'

KEY_JSON_FILE_CHOSEN_DIR = '-JSON-FILE-CHOSEN-DIRECTORY'
KEY_JSON_FILE_NAME = '-JSON-FILE-NAME-'

KEY_GENERATE_WORKER = '-GENERATE-WORKER-'
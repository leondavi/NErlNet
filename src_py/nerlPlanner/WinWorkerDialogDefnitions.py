
# Action Keys

KEY_MODEL_TYPE_LIST_BOX = '-MODEL-TYPE-LIST-BOX-'
KEY_MODEL_ARGS_INPUT = '-MODEL-ARGS-INPUT-'

KEY_NUM_OF_MODEL_ARGS = '-NUM-OF-MODEL-ARGS-'
KEY_LAYER_SIZES_INPUT = '-LAYERS-SIZES-INPUT-'
KEY_NUM_OF_LAYERS_SIZES = '-NUM-OF-LAYERS-SIZES-'
KEY_LAYER_TYPE_SELECTION = '-LAYER-TYPE-SELECTION-'
KEY_LAYER_TYPE_SELECTION_ADD = '-LAYER-TYPE-SELECTION-ADD-'
KEY_LAYER_SIZES_HELP_BUTTON = '-KEY-LAYER-SIZES-HELP-BUTTON-'
KEY_LAYER_TYPE_HELP = '-LAYER-TYPE-HELP-'
KEY_LAYER_TYPE_SELECTION_CLEAR = '-LAYER-TYPE-SELECTION-CLEAR-'
KEY_LAYER_TYPE_CODES_INPUT = '-LAYER-TYPE-CODES-INPUT-'
KEY_NUM_OF_LAYERS_TYPES = '-NUM-OF-LAYERS-TYPES-'

KEY_LAYER_METHODS_BUTTON_SELECT = '-LAYER-METHODS-BUTTON-SELECT-'

KEY_ACTIVATION_LAYER_SELECTION = '-ACTIVATION-LAYER-SELECTION-'
KEY_ACTIVATION_LAYER_SELECTION_ADD = '-ACTIVATION-LAYER-SELECTION-ADD-'
KEY_ACTIVATION_LAYER_HELP = '-ACTIVATION-LAYER-HELP-'
KEY_LAYER_FUNCTIONS_SELECTION_CLEAR = '-ACTIVATION-LAYER-SELECTION-CLEAR-'
KEY_LAYER_FUNCTIONS_CODES_INPUT = '-ACTIVATION-CODES-INPUT-'
KEY_LAYERS_FUNCTIONS_CODES = '-KEY-LAYERS-FUNCTIONS-CODES-'

# optimizer keys
KEY_LEARNING_RATE_INPUT = '-LEARNING-RATE-INPUT-'
KEY_OPTIMIZER_TYPE_LIST_BOX = '-OPTIMIZER-TYPE-LIST-BOX-'
KEY_OPTIMIZER_ARGS_INPUT = '-OPTIMIZER-ARGS-INPUT-'
KEY_LOSS_METHOD_LIST_BOX = '-LOSS-METHOD-LIST-BOX-'
KEY_LOSS_ARGS_INPUT = '-LOSS-ARGS-INPUT-'
KEY_EPOCHS_INPUT = '-WORKER-EPOCHS-INPUT-'

#  Distributed System and Infra keys
KEY_INFRA_TYPE_LIST_BOX = '-INFRA-TYPE-LIST-BOX-'
KEY_DISTRIBUTED_SYSTEM_TYPE_LIST_BOX = '-DISTRIBUTED-SYSTEM-TYPE-LIST-BOX-'
KEY_DISTRIBUTED_SYSTEM_TOKEN_AUTOGENERATE_BUTTON = '-DISTRIBUTED-SYSTEM-TOKEN-AUTOGENERATE-BUTTON-'
KEY_DISTRIBUTED_SYSTEM_TOKEN_INPUT = '-DISTRIBUTED-SYSTEM-TOKEN-INPUT-'
KEY_DISTRIBUTED_SYSTEM_TOKEN_HELP_BUTTON = '-DISTRIBUTED-SYSTEM-TOKEN-HELP-BUTTON-'
KEY_DISTRIBUTED_SYSTEM_ARGS_INPUT = 'KEY-DISTRIBUTED-SYSTEM-ARGS-INPUT'

KEY_JSON_FILE_CHOSEN_DIR = '-JSON-FILE-CHOSEN-DIRECTORY'
KEY_JSON_LOAD_FILE_BUTTON_EVENT = '-JSON-FILE-LOAD-BUTTON-EVENT-'
KEY_JSON_FILE_NAME = '-JSON-FILE-NAME-'
KEY_JSON_LOAD_FILE_BROWSE_EVENT = '-JSON_LOAD_FILE_BROWSE_EVENT-'

KEY_BUTTON_EXPORT_WORKER = '-BUTTON-EXPORT-WORKER-'
KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION = 'KEY-CHECKBOX-WORKER-WITH-DOCUMENTATION-'


# Layer Method Selection Dialog

KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_ACTIVATION = '-LAYER-METHOD-SELECTION-DIALOG-LISTBOX-ACTIVATION-'
KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_POOLING = '-LAYER-METHOD-SELECTION-DIALOG-LISTBOX-POOLING-'
KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_SCALER = '-LAYER-METHOD-SELECTION-DIALOG-LISTBOX-SCALER-'
KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_FLATTEN = '-LAYER-METHOD-SELECTION-DIALOG-LISTBOX-FLATTEN-'
KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_BOUNDING = '-LAYER-METHOD-SELECTION-DIALOG-LISTBOX-BOUNDING-'
KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_PROBABILISTIC = '-LAYER-METHOD-SELECTION-DIALOG-LISTBOX-PROBABILISTIC-'
KEY_LAYER_METHOD_SELECTION_TEXT = '-LAYER-METHOD-SELECTION-TEXT-'
KEY_LAYER_METHOD_SELECTION_BUTTON = '-LAYER-METHOD-SELECTION-BUTTON-'

LAYER_SIZES_HELP_POPUP_STR =  'CNN Layer definition: <W>x<H>x<D>k<K>s<S>p<P><T>\n\nShould be defined only with CNN layer type code\nW - Width\nH - Height\nD - Depth\nF - Filters\nK - Kernel size <W>x<H>x<D>x<F>\nS - Stride\nP - Padding\nT - Convolution Type (v-valid, s-same)\n\nExample:\n128x128k3x3x1x16s2p1 represents a CNN layer\ninput size is 128x128\nkernel size 3x3x1x16 and stride of 2 with padding 1\nconvolution type is valid.\n\nPooling:\nPooling of single or multiple dimension:\n<N>x<N>p<P>x<P>s<S>\nor\n<N>p<P>s<S>\nExamples:\n8x8p2x2s1x1 means size of layer is 8x8\nand pooling is by two rows and tow columns\nand stride of 1 in each direction\n10p2s1 means pool size of 2 and stride of 1\n\nScaling\n<N>x<N>\nor\n<N>'
import subprocess
from logger import *

VERSION = "1.0.4"
NERLNET_VERSION_TESTED_WITH = "1.5.2"
NERLNET_TMP_PATH = "/tmp/nerlnet"
NERLNET_GRAPHVIZ_OUTPUT_DIR = f"{NERLNET_TMP_PATH}/nerlplanner"
NERLNET_GLOBAL_PATH = "/usr/local/lib/nerlnet-lib/NErlNet"
NERL_PLANNER_PATH = NERLNET_GLOBAL_PATH+"/src_py/nerlPlanner"
NERLNET_LOGO_PATH = NERL_PLANNER_PATH+"/NerlnetIcon.png"
NERLNET_SPLASH_LOGO_PATH = NERL_PLANNER_PATH+"/Nerlnet_splash_logo.png"
WINDOW_TITLE = "NerlPlanner"
WINDOW_FIXED_WIDTH = 1500
WINDOW_MAX_SUPPORTED_HEIGHT = 1080 # smaller than 1080 is better
WINDOW_HEIGHT_MULTIPLICATION_FACTOR =  5/6
PYSIMPLEGUI_5_LICENSE = 'eby6JRM5a8WmNGlnbWnUN2l0VVHilawWZaSoIK6hIUkdRGpPcG3YR7ygaLWlJ015dxG7lUvfbRijI0s6IIkfxbplYD2XVhuCc220VFJIR7CUIy68MXTAcxyBMRDUIYxKONTYATwXM3S7w5iQT4GWlMjaZUWV5pzfZbUjR1lcc6GIxOvWe8Wm1elIbvn3RUWjZDXRJtzHa4Wo9PuEIVjgokiENISn47wvI0iowVieTlm5F4tDZOUFZapUcenANX0AImjSoni1RPGtFO2taaW7QGidLXCwJ7OgYuWz1zloTkG5FWzVddCWIw6UItkwxIl0bV2o4liLLfCGJvDiby2z1fwkYYWW5u5fIGjsoRiZTemkVXy1bHGS5kljdRChIRsdIqkJNL1rcG3rRHvHbcWAVgyUS5UgQkihO5i0IDy1OEDDkF4uMzCtISszIukTRhhqdCG5VSJUcz3LNN1vZTWoQYi4OrifISyoMuDAIF02LGTBA736LMTMAu2sI4icwSidRxGmFF0cZ5UMVG4ucMGclkyOZQXPMXiLOBijIJytMjDyI410L8TkAL39L8TnAL2zISiQwdimRqWS1ghFa2WJxiBtZBGkR4yzZrXmN3zWInjGofijZUGfRIsPZaWS9iuOQxGs9E16dDGbxuvybq28sKudYx2w8wula2WRwaiELTCMJTJuUKEUFqkvZHHSJclDc53CMsi1OqinI60bNLi543yKMcTTAcukMVT1IhuqMYjPQsxXIPnD02=R13b28056153da511d4134b1f82c2bcd0679524dbf9a1d1c3802e9405fe49caa270a248235f2a9d13c77dee452bd2e7f4e6928f222a9f5e659be68400f463fed960ab75142668d5be303ba1fee27349967dd7eb64890f549dd7f8a224d2f2cdcd1146da869d39ad02121707ab3cefaed98e20eb4197c2a36cf64211ea77ee2442537e0d910ae843691a0107ae7a4dc922d1e091f655675d6cabaad0096b863eec55bfb7a758b8f140b2c31595978ce000d75696b3616b76a3a6ee8daab6002029e9b9d8954a0dbcd2a5f332d84cfa370799a58a8f3e0e2cd4ba3ba5f1f76b88c97ee6d05033f40180d808eb158a5ed30695c50edc33a67895ec2755775882a0996fb59afdef8951daef8f7b9501bb9771ef326248f1ff58df2a8ecb9051d1827904d4061cd5b4de6eb0a6ff9277af1206eb8223fce66e58635927791fa96f09aee64b11ec0c88da6d303389edef162b4e13dc3ed286b2f2c28cdc315677c50e547d071245d7393a5085b9869d52858566021396e3aeb2b6de3c47631c40b3cc667845064aa70ef3a14884a802c9136d27a7f2f4c91a28edf6f6c3a7d5b611d98ea5db9036cb9d69f642ebee1c7d186d99263e81cd45c7ca2b02d74256aeb6ac7cac4483e0b39088d7be8e7531906bc4ee6919e5ae345897d6afe293c54a2bc45913158ed366df603c71deb73601bcd17626a78d08aa47f0e91f82364cc91ea424'

WIN_EXPERIMENT_FLOW_DIALOG_EVENT_KEY = 'WIN_EXPERIMENT_FLOW_DIALOG'
WIN_COMMUNICATION_MAP_DIALOG_EVENT_KEY = 'WIN_COMMUNICATION_MAP_DIALOG'
WIN_WORKER_DIALOG_EVENT_KEY = 'WIN_WORKER_DIALOG'


KEY_SETTINGS_FREQUENCY_INPUT = '-KEY-SETTINGS-FREQUENCY-INPUT-'
KEY_SETTINGS_BATCH_SIZE_INPUT = '-KEY-SETTINGS-BATCH-SIZE-INPUT-'
KEY_SETTINGS_MAINSERVER_PORT_INPUT = '-KEY-SETTINGS-MAINSERVER-PORT-INPUT-'
KEY_SETTINGS_MAINSERVER_ARGS_INPUT = '-KEY-SETTINGS-MAINSERVER-ARGS-INPUT-'
KEY_SETTINGS_APISERVER_PORT_INPUT = '-KEY-SETTINGS-APISERVER-PORT-INPUT-'
KEY_SETTINGS_APISERVER_ARGS_INPUT = '-KEY-SETTINGS-APISERVER-ARGS-INPUT-' 
KEY_SETTINGS_SAVE_BUTTON = '-KEY-SETTINGS-SAVE-BUTTON-'
KEY_SETTINGS_CLEAR_BUTTON = '-KEY-SETTINGS-CLEAR-BUTTON-'
KEY_SETTINGS_STATUS_BAR = '-KEY-SETTINGS-STATUS-BAR-'
KEY_SETTINGS_MAIN_SERVER_STATUS_BAR = '-KEY-SETTINGS-MAIN-SERVER-STATUS-BAR-'
KEY_SETTINGS_API_SERVER_STATUS_BAR = '-KEY-SETTINGS-API-SERVER-STATUS-BAR-'

KEY_SETTINGS_SPECIAL_ENTITIES_SAVE = '-KEY-SETTINGS-SPECIAL-ENTITIES-SAVE-'
KEY_SETTINGS_SPECIAL_ENTITIES_CLEAR = '-KEY-SETTINGS-SPECIAL-ENTITIES-CLEAR-'

KEY_WORKERS_INPUT_LOAD_WORKER_PATH = '-KEY-WORKERS-INPUT-LOAD-WORKER-PATH-'
KEY_WORKERS_LOAD_FROM_LIST_WORKER_BUTTON = '-KEY-WORKERS-LOAD-FROM-LIST-WORKER-BUTTON-'
KEY_WORKERS_SHOW_WORKER_BUTTON = '-KEY-WORKERS-SHOW-WORKER-BUTTON-'
KEY_WORKERS_NAME_INPUT = '-KEY-WORKERS-NAME-INPUT-'
KEY_WORKERS_BUTTON_ADD = '-KEY-WORKERS-BUTTON-ADD-'
KEY_WORKERS_BUTTON_VIEW = '-KEY-WORKERS-BUTTON-VIEW-'
KEY_WORKERS_BUTTON_REMOVE = '-KEY-WORKERS-BUTTON-REMOVE-'
KEY_WORKERS_LIST_BOX = '-KEY-WORKERS-LIST-BOX-'
KEY_WORKERS_INFO_BAR = '-KEY-WORKERS-INFO-BAR-'

KEY_DEVICES_SCANNER_BUTTON = '-KEY-DEVICES-SCANNER-BUTTON-'
KEY_DEVICES_ONLINE_LIST_COMBO_BOX = '-KEY-DEVICES-ONLINE-LIST-COMBO-BOX-'
KEY_DEVICES_BUTTON_ADD = '-KEY-DEVICES-BUTTON-ADD-'
KEY_DEVICES_BUTTON_LOAD = '-KEY-DEVICES-BUTTON-LOAD-'
KEY_DEVICES_BUTTON_SAVE = '-KEY-DEVICES-BUTTON-SAVE-'
KEY_DEVICES_BUTTON_REMOVE = '-KEY-DEVICES-BUTTON-REMOVE-'
KEY_DEVICES_SCANNER_INPUT_LAN_MASK = '-KEY-DEVICES-SCANNER-INPUT-LAN-MASK-'
KEY_DEVICES_IP_INPUT = '-KEY-DEVICES-IP-INPUT-'
KEY_DEVICES_NAME_INPUT = '-KEY-DEVICES-NAME-'
KEY_DEVICES_LIST_BOX_DEVICES = '-KEY-DEVICES-LIST-BOX-DEVICES-'
KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES = '-KEY-DEVICES-LIST-BOX-DEVICE-ENTITIES-'

KEY_DEVICES_SELECTED_ENTITY_COMBO = '-KEY-DEVICES-SELECTED-ENTITY-COMBO-'
KEY_DEVICES_SELECTED_DEVICE_TEXT = '-KEY-DEVICES-SELECTED-DEVICE-TEXT-'
KEY_DEVICES_ADD_ENTITY_TO_DEVICE = '-KEY-DEVICES-ADD-ENTITY-TO-DEVICE-'
KEY_DEVICES_REMOVE_ENTITY_FROM_DEVICE = '-KEY-DEVICES-REMOVE-ENTITY-FROM-DEVICE-'

KEY_CLIENTS_WORKERS_LIST_COMBO_BOX = '-KEY-CLIENTS-WORKERS-LIST-COMBO-BOX-'
KEY_CLIENTS_BUTTON_ADD = '-KEY-CLIENTS-BUTTON-ADD-'
KEY_CLIENTS_BUTTON_LOAD = '-KEY-CLIENTS-BUTTON-LOAD-'
KEY_CLIENTS_BUTTON_SAVE = '-KEY-CLIENTS-BUTTON-SAVE-'
KEY_CLIENTS_BUTTON_REMOVE = '-KEY-CLIENTS-BUTTON-REMOVE-'
KEY_CLIENTS_NAME_INPUT = '-KEY-CLIENTS-NAME-INPUT-'
KEY_CLIENTS_PORT_INPUT = '-KEY-CLIENTS-PORT-INPUT-'
KEY_CLIENTS_WORKERS_LIST_ADD_WORKER = '-KEY-CLIENTS-WORKERS-LIST-ADD-WORKER-'
KEY_CLIENTS_WORKERS_LIST_REMOVE_WORKER = '-KEY-CLIENTS-WORKERS-LIST-REMOVE-WORKER-'
KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS = '-KEY-CLIENTS-WORKERS-LIST-BOX-CLIENT-FOCUS-'


KEY_ROUTERS_BUTTON_ADD = '-KEY-ROUTERS-BUTTON-ADD-'
KEY_ROUTERS_BUTTON_LOAD = '-KEY-ROUTERS-BUTTON-LOAD-'
KEY_ROUTERS_BUTTON_SAVE = '-KEY-ROUTERS-BUTTON-LOAD-'
KEY_ROUTERS_BUTTON_REMOVE = '-KEY-ROUTERS-BUTTON-REMOVE-'
KEY_ROUTERS_NAME_INPUT = '-KEY-ROUTERS-NAME-INPUT-'
KEY_ROUTERS_PORT_INPUT = '-KEY-ROUTERS-PORT-INPUT-'
KEY_ROUTERS_POLICY_COMBO_BOX = '-KEY-ROUTERS-POLICY-COMBO-BOX-'

KEY_SOURCES_BUTTON_ADD = '-KEY-SOURCES-BUTTON-ADD-'
KEY_SOURCES_BUTTON_LOAD = '-KEY-SOURCES-BUTTON-LOAD-'
KEY_SOURCES_BUTTON_SAVE = '-KEY-SOURCES-BUTTON-SAVE-'
KEY_SOURCES_BUTTON_REMOVE = '-KEY-SOURCES-BUTTON-REMOVE-'
KEY_SOURCES_NAME_INPUT = '-KEY-SOURCES-NAME-INPUT-'
KEY_SOURCES_PORT_INPUT = '-KEY-SOURCES-PORT-INPUT-'
KEY_SOURCES_FREQUENCY_INPUT = '-KEY-SOURCES-FREQUENCY-INPUT-'
KEY_SOURCES_FREQUENCY_DEFAULT_CHECKBOX = '-KEY-SOURCES-FREQUENCY-DEFAULT-CHECKBOX-'
KEY_SOURCES_EPOCHS_INPUT = '-KEY-SOURCES-EPOCHS-INPUT-'
KEY_SOURCES_POLICY_COMBO_BOX = '-KEY-SOURCES-POLICY-COMBO-BOX-'
KEY_SOURCES_TYPE_COMBO_BOX = '-KEY-SOURCES-TYPE-COMBO-BOX-'

KEY_ENTITIES_CLIENTS_LISTBOX = '-KEY-ENTITIES-CLIENTS-LISTBOX-'
KEY_ENTITIES_ROUTERS_LISTBOX = '-KEY-ENTITIES-ROUTERS-LISTBOX-'
KEY_ENTITIES_SOURCES_LISTBOX = '-KEY-ENTITIES-SOURCES-LISTBOX-'
KEY_CLIENTS_STATUS_BAR = '-KEY-CLIENTS-STATUS-BAR-'

KEY_DC_JSON_IMPORT_BROWSE_BUTTON = '-KEY-DC-JSON-IMPORT-BROWSE-BUTTON-' # not in use
KEY_DC_JSON_IMPORT_INPUT = '-KEY-DC-JSON-IMPORT-BROWSE-BUTTON-'
KEY_DC_JSON_IMPORT_BUTTON = '-KEY-DC-JSON-IMPORT-BUTTON-'
KEY_DC_JSON_EXPORT_TO_BROWSE_BUTTON_DIR = '-KEY-DC-JSON-EXPORT-TO-BROWSE-BUTTON-DIR-' # not in use
KEY_DC_JSON_EXPORT_TO_INPUT_DIR = '-KEY-DC-JSON-EXPORT-TO-INPUT-DIR-'
KEY_DC_JSON_EXPORT_TO_INPUT_FILENAME = '-KEY-DC-JSON-EXPORT-TO-INPUT-FILENAME-'
KEY_DC_JSON_EXPORT_BUTTON= '-KEY-DC-JSON-EXPORT-BUTTON-'
KEY_DC_JSON_CLEAR_BUTTON= '-KEY-DC-JSON-CLEAR-BUTTON-'

def settings_freq_batch_str(freq : str = 'xxx', batch : str = 'xxx'):
    return f"{freq}Hz {batch} Samples"

def pretty_print_dict(d):#define d
    pretty_dict = ''  #take empty string
    for k, v in d.items():#get items for dict
        pretty_dict += f'{k}: {str(v)}\n'
    return pretty_dict#return result

def print_banner():
    print("\n d8b   db d88888b d8888b. db      d8b   db d88888b d888888b\n \
888o  88 88'     88  `8D 88      888o  88 88'     `~~88~~'\n \
88V8o 88 88ooooo 88oobY' 88      88V8o 88 88ooooo    88   \n \
88 V8o88 88~~~~~ 88`8b   88      88 V8o88 88~~~~~    88   \n \
88  V888 88.     88 `88. 88booo. 88  V888 88.        88   \n \
VP   V8P Y88888P 88   YD Y88888P VP   V8P Y88888P    YP   \n \
                                                          \n \
                                                          \n \
d8888b. db       .d8b.  d8b   db d8b   db d88888b d8888b. \n \
88  `8D 88      d8' `8b 888o  88 888o  88 88'     88  `8D \n \
88oodD' 88      88ooo88 88V8o 88 88V8o 88 88ooooo 88oobY' \n \
88~~~   88      88~~~88 88 V8o88 88 V8o88 88~~~~~ 88`8b   \n \
88      88booo. 88   88 88  V888 88  V888 88.     88 `88. \n \
88      Y88888P YP   YP VP   V8P VP   V8P Y88888P 88   YD \n \
                                                          \n \
                                                          \n ")
    LOG_INFO(f"Nerlnet Planner version {VERSION} is given without any warranty.")
    LOG_INFO(f"There is no commitiment or responsibility for results, damage, loss that can be caused by using this tool.")
    LOG_INFO(f"Please review the license of Nerlnet on Github repository:")
    LOG_INFO(f"www.github.com/leondavi/NErlNet")
    LOG_INFO(f"You must cite Nerlnet if you use any of its tools for academic/commercial/any purpose.")
    LOG_INFO(f"Tested with Nerlnet version {NERLNET_VERSION_TESTED_WITH}")


def get_screen_resolution():
    cmd = ["xrandr | grep '*'"]
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
    resolution_string, junk = p.communicate()
    resolution = resolution_string.split()[0].decode('utf-8')
    width, height = resolution.split('x')
    return width, height
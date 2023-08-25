from collections import OrderedDict

# instances
batch_size = None
main_server_inst = None
api_server_inst = None
nerl_gui_inst = None

# workers
workers_load_worker_path = None
workers_new_worker = None
workers_new_worker_name = None
workers_new_worker_dict = None
worker_name_selection = None

# devices
devices_name = None
devices_ip_str = None
devices_devices_list_box_selection = None # no mistake in name there is also an entities listbox
devices_entity_selection = None

# clients
clients_combo_box_worker_selection = None
clients_this_client_name = None
clients_this_client_port = None
clients_this_client = None
clients_this_client_workers_dict = OrderedDict()

# routers
routers_this_router = None
routers_this_router_name = None
routers_this_router_port = None
routers_this_router_policy = None

# sources
sources_this_source = None
sources_this_source_name = None
sources_this_source_port = None
sources_this_source_frequency = None
sources_this_source_epochs = None
sources_this_source_policy = None
sources_this_source_type = None

# entities
last_selected_entity = []
entities_clients_names_list = []
entities_routers_names_list = []
entities_sources_names_list = []
last_entities_list_state = []
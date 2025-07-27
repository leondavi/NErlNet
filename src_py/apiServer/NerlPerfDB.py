from networkComponents import MAIN_SERVER_STR , NetworkComponents
from abc import ABC

class EntityPerfDB(ABC): # Abstract Class
    def __init__(self):
        self.time_train_active = 0
        self.time_train_total = 0
        self.time_predict_active = 0
        self.time_predict_total = 0
        self.average_gpu_usage_train = 0
        self.average_gpu_memory_usage_predict = 0
        self.memory_train_ema_usage = 0
        self.memory_predict_ema_usage = 0
        self.memory_train_peak_usage = 0
        self.memory_predict_peak_usage = 0
        self.num_of_cores = 0
        self.cpu_train_util_per_core = {}  # {core_index: utilization}
        self.cpu_predict_util_per_core = {}  # {core_index: utilization}
        
        
class ClientPerfDB(EntityPerfDB):
    def __init__(self):
        super().__init__()
        
    def update_performance_stats(self, performance_stats: dict):
        # Update the performance stats for the client
        self.time_train_active = performance_stats.get('time_train_active', 0)
        self.time_train_total = performance_stats.get('time_train_total', 0)
        self.time_predict_active = performance_stats.get('time_predict_active', 0)
        self.time_predict_total = performance_stats.get('time_predict_total', 0)
        self.average_gpu_usage_train = performance_stats.get('average_gpu_usage_train', 0)
        self.average_gpu_memory_usage_predict = performance_stats.get('average_gpu_memory_usage_predict', 0)
        self.memory_train_ema_usage = performance_stats.get('memory_train_ema_usage', 0)
        self.memory_predict_ema_usage = performance_stats.get('memory_predict_ema_usage', 0)
        self.memory_train_peak_usage = performance_stats.get('memory_train_peak_usage', 0)
        self.memory_predict_peak_usage = performance_stats.get('memory_predict_peak_usage', 0)
        self.num_of_cores = performance_stats.get('num_of_cores', 0)
        for core_index in range(self.num_of_cores):
            self.cpu_train_util_per_core[core_index] = performance_stats.get(f'cpu_train_util_core_{core_index}', 0)
            self.cpu_predict_util_per_core[core_index] = performance_stats.get(f'cpu_predict_util_core_{core_index}', 0)
            
    def get_as_dict(self):
        return {
            "time_train_active": self.time_train_active,
            "time_train_total": self.time_train_total,
            "time_predict_active": self.time_predict_active,
            "time_predict_total": self.time_predict_total,
            "average_gpu_usage_train": self.average_gpu_usage_train,
            "average_gpu_memory_usage_predict": self.average_gpu_memory_usage_predict,
            "memory_train_ema_usage": self.memory_train_ema_usage,
            "memory_predict_ema_usage": self.memory_predict_ema_usage,
            "memory_train_peak_usage": self.memory_train_peak_usage,
            "memory_predict_peak_usage": self.memory_predict_peak_usage,
            "num_of_cores": self.num_of_cores,
            "cpu_train_util_per_core": self.cpu_train_util_per_core,
            "cpu_predict_util_per_core": self.cpu_predict_util_per_core
        }

        
class NerlPerfDB:
    def __init__(self, network_components: NetworkComponents):
        self.network_components = network_components
        self.clients = {}  # {client_name: ClientPerfDB}
        self.sources = {}  # {source_name: SourcePerfDB} - TODO
        self.workers = {}  # {worker_name: WorkerPerfDB} - TODO
        self.routers = {}  # {router_name: RouterPerfDB} - TODO
        self.main_server = {} # {MAIN_SERVER_STR: MainServerPerfDB} - TODO
        self.build_dicts()

    def get_clients(self):
        return self.clients

    def get_sources(self):
        return self.sources

    def get_workers(self):
        return self.workers

    def get_routers(self):
        return self.routers

    def get_main_server(self):
        return self.main_server
    
    def build_dicts(self):
        for client_name in self.network_components.clients:
            self.clients[client_name] = ClientPerfDB()
            
        # TODO (implement sources, workers, routers, main server)
    
        
    def update_entities_stats(self, entity_perf_dicts: dict):
        for entity_name in entity_perf_dicts:
            entity_stats_dict = entity_perf_dicts[entity_name]
            if entity_name in self.routers: # TODO
                assert 0, "This is not implemented yet"
            elif entity_name in self.clients:
                self.clients[entity_name].update_performance_stats(entity_stats_dict)
            elif entity_name in self.sources: # TODO
                assert 0, "This is not implemented yet"
            elif entity_name in self.workers: # TODO
                assert 0, "This is not implemented yet"
            elif entity_name == MAIN_SERVER_STR: # TODO
                assert 0, "This is not implemented yet"
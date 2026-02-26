################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################

import sys
import os
import hashlib
from definitions import *

sys.path.insert(0, f'{NERLNET_SRC_PY_PATH}/nerlPlanner')
sys.path.insert(0, f'{NERLNET_SRC_PY_PATH}') # keep both paths for vscode intelisense

from logger import *
from nerlPlanner.JsonDistributedConfigDefs import *
from nerlPlanner.JsonElements import GetFields

#import globalVars as globe
API_SERVER_STR = GetFields.get_api_server_field_name()
MAIN_SERVER_STR = GetFields.get_main_server_field_name()

# types
TYPE_CLIENT = "client"
TYPE_WORKER = "worker"
TYPE_SOURCE = "source"
TYPE_ROUTER = "router"
TYPE_WORKER = "worker"
TYPE_MAIN_SERVER = "mainServer"
TYPE_SUPER_NODE = "superNode"

class NetworkComponents():

    def __init__(self, dc_json: dict):
        # Loading the data in JSON format:
        self.jsonData = dc_json

        # Initializing lists for all the relevant components names:
        self.devicesIp = []
        self.clients = []
        self.workers = []
        self.sources = []
        self.sourcesPolicies = []
        self.sourceEpochs = {}
        self.routers = []
        self.sources_policy_dict = {}
        self.super_nodes = []
        self.super_node_to_clients = {}
        self.client_to_super_node = {}
        self.super_node_ports = {}
        self.super_node_heartbeat_ms = {}
        self.super_node_max_inflight_microbatches = {}
        self.worker_parallel_map = {}
        self.model_tp_plan_map = {}
        

        # Initializing maps
        self.map_worker_to_client = {}
        self.map_entity_to_device = {}
        self.map_device_to_ip = {}
        self.map_name_to_type = {}
        self.worker_to_model_sha = {}
        self.model_sha_map = self.jsonData.get(KEY_MODEL_SHA, {})
        self.torch_model_assets = {}

        # Getting the desired batch size:
        self.batchSize = int(self.jsonData[KEY_NERLNET_SETTINGS][KEY_BATCH_SIZE])
        self.frequency = int(self.jsonData[KEY_NERLNET_SETTINGS][KEY_FREQUENCY])

        # Getting the names of all the devices:
        devicesJsons = self.jsonData[GetFields.get_devices_field_name()]

        for device in devicesJsons:
            self.devicesIp.append(device[GetFields.get_ipv4_field_name()])
            for entity_name in device[GetFields.get_entities_field_name()].split(','):
                self.map_entity_to_device[entity_name] = device[GetFields.get_name_field_name()]
                self.map_device_to_ip[device[GetFields.get_name_field_name()]] = device[GetFields.get_ipv4_field_name()]

        # Getting the address of the main server:
        self.mainServerIp, self.mainServerPort = self.get_main_server_ip_port()
        self.apiServerIp, self.apiServerPort = self.get_api_server_ip_port()

        # Getting the address for the receiver:
        self.receiverIp = self.apiServerIp
        self.receiverPort =  self.apiServerPort

        # Getting the names of all the clients and workers:
        clientsJsons = self.jsonData[GetFields.get_clients_field_name()]

        for client_dict in clientsJsons:
            client_name = client_dict[GetFields.get_name_field_name()]
            self.clients.append(client_name)
            subWorkers = client_dict[GetFields.get_workers_field_name()].split(',') # list
            for worker_name in subWorkers:
                self.map_worker_to_client[worker_name] = client_name # map worker name to client name
            # Add every sub-worker of this client, to the general workers list:
            self.workers.extend(subWorkers)
            self.map_name_to_type[client_name] = TYPE_CLIENT
            client_super = str(client_dict.get(SUPERNODE_FIELD, "")).strip()
            if client_super:
                self.client_to_super_node[client_name] = client_super

        workers_section = self.jsonData.get(GetFields.get_workers_field_name(), [])
        for worker_def in workers_section:
            worker_name = worker_def.get(GetFields.get_name_field_name())
            worker_sha = worker_def.get(WORKER_MODEL_SHA_FIELD)
            if worker_name and worker_sha:
                self.worker_to_model_sha[worker_name] = worker_sha
            worker_parallel = worker_def.get(PARALLEL_FIELD)
            if worker_name and worker_parallel:
                self.worker_parallel_map[worker_name] = self._normalize_worker_parallel(worker_name, worker_parallel)

        # Getting the names of all the sources:
        sourcesJsons = self.jsonData[GetFields.get_sources_field_name()]
        for source in sourcesJsons:
            self.sources.append(source[GetFields.get_name_field_name()])
            self.sourcesPolicies.append(source[GetFields.get_policy_field_name()])
            self.sourceEpochs[source[GetFields.get_name_field_name()]] = source[GetFields.get_epochs_field_name()]
            self.map_name_to_type[source[GetFields.get_name_field_name()]] = TYPE_SOURCE
            self.sources_policy_dict[source[GetFields.get_name_field_name()]] = source[GetFields.get_policy_field_name()]

        # Getting the names of all the routers:
        routersJsons = self.jsonData[GetFields.get_routers_field_name()]
        for router in routersJsons:
            self.routers.append(router[GetFields.get_name_field_name()])
            self.map_name_to_type[router[GetFields.get_name_field_name()]] = TYPE_ROUTER

        self._extract_super_nodes()
        self._extract_model_tp_plans()
        self._validate_super_nodes()
        self._validate_worker_parallel()
        self._validate_pipeline_tp_compatibility()
        self._validate_model_tp_plans()
        self.torch_model_assets = self._extract_torch_assets()


    def get_map_worker_to_client(self):
        return self.map_worker_to_client
    
    def get_source_epochs_dict(self):
        return self.sourceEpochs
    
    def get_client_name_by_worker_name(self, worker_name):
        return self.map_worker_to_client[worker_name]

    def get_main_server_ip_port(self):
        main_server_port = self.jsonData[MAIN_SERVER_STR][GetFields.get_port_field_name()]
        main_server_ip = self.map_device_to_ip[self.map_entity_to_device[MAIN_SERVER_STR]]
        return main_server_ip, main_server_port
    
    def get_api_server_ip_port(self):
        api_server_port = self.jsonData[API_SERVER_STR][GetFields.get_port_field_name()]
        api_server_ip = self.map_device_to_ip[self.map_entity_to_device[API_SERVER_STR]]
        return api_server_ip, api_server_port
    
    def get_freq(self):
        return self.frequency
    
    def get_batch_size(self):
        return self.batchSize
    
    def get_num_of_sources(self):
        return len(self.sources)
    
    def get_workers_list(self):
        return self.workers

    def printComponents(self):
        LOG_INFO(f"\nNetwork components:\n \
                Receiver's Address: http://{self.receiverIp}:{self.receiverPort}\n \
                Frequency: {self.frequency} [batches/sec]\n \
                Batchsize: {self.batchSize} [samples]\n \
                devicesIp: {self.devicesIp}\n \
                mainServerIp: {self.mainServerIp}\n \
                mainServerPort: {self.mainServerPort}\n \
                apiServerIp: {self.apiServerIp}\n \
                apiServerPort: {self.apiServerPort}\n \
                Clients: {self.clients}\n \
                Workers: {self.workers}\n \
                Sources: {self.sources}\n \
                Routers: {self.routers}\n \
                SuperNodes: {self.super_nodes}")

    def has_super_nodes(self):
        return len(self.super_nodes) > 0

    def get_super_nodes_list(self):
        return list(self.super_nodes)

    def get_super_node_to_clients_map(self):
        return dict(self.super_node_to_clients)

    def get_client_to_super_node_map(self):
        return dict(self.client_to_super_node)

    def get_super_node_ports(self):
        return dict(self.super_node_ports)

    def get_super_node_heartbeat_ms(self):
        return dict(self.super_node_heartbeat_ms)

    def get_super_node_max_inflight_microbatches(self):
        return dict(self.super_node_max_inflight_microbatches)

    def get_client_super_node(self, client_name: str):
        return self.client_to_super_node.get(client_name)

    def get_worker_parallel_map(self):
        return dict(self.worker_parallel_map)

    def get_worker_parallel(self, worker_name: str):
        return dict(self.worker_parallel_map.get(worker_name, {}))

    def has_pipeline_workers(self):
        return any(PIPELINE_STAGE_FIELD in cfg for cfg in self.worker_parallel_map.values())

    def has_tp_workers(self):
        return any(bool(cfg.get(TP_GROUP_FIELD, "")) for cfg in self.worker_parallel_map.values())

    def get_pipeline_stage_world_size(self):
        max_stage = -1
        max_world = 0
        for cfg in self.worker_parallel_map.values():
            stage = cfg.get(PIPELINE_STAGE_FIELD)
            world = cfg.get(PIPELINE_WORLD_SIZE_FIELD)
            if isinstance(stage, int):
                max_stage = max(max_stage, stage)
            if isinstance(world, int):
                max_world = max(max_world, world)
        return max(1, max_world, max_stage + 1)

    def get_model_tp_plan_map(self):
        return dict(self.model_tp_plan_map)

    def get_runtime_graph_entities(self):
        entities = [MAIN_SERVER_STR, API_SERVER_STR]
        entities.extend(self.routers)
        entities.extend(self.sources)
        entities.extend(self.clients)
        entities.extend(self.super_nodes)
        return list(dict.fromkeys(entities))

    def validate_connection_map(self, connections_map: dict):
        if not isinstance(connections_map, dict):
            raise ValueError("Connection map must include a 'connectionsMap' object")

        runtime_entities = self.get_runtime_graph_entities()
        adjacency = {entity_name: set() for entity_name in runtime_entities}
        runtime_entity_set = set(runtime_entities)
        referenced_entities = set()

        for raw_src, raw_neighbors in connections_map.items():
            src = str(raw_src).strip()
            if not src:
                raise ValueError("Connection map contains an empty source entity")
            if src not in runtime_entity_set:
                raise ValueError(f"Connection map references unknown source entity '{src}'")
            if not isinstance(raw_neighbors, list):
                raise ValueError(f"Connection map neighbors for '{src}' must be a list")

            for raw_neighbor in raw_neighbors:
                neighbor = str(raw_neighbor).strip()
                if not neighbor:
                    continue
                if neighbor not in runtime_entity_set:
                    raise ValueError(
                        f"Connection map references unknown neighbor '{neighbor}' from '{src}'"
                    )
                adjacency[src].add(neighbor)
                adjacency[neighbor].add(src)
                referenced_entities.add(src)
                referenced_entities.add(neighbor)

                # Erlang runtime automatically aliases router->mainServer into router->apiServer.
                if neighbor == MAIN_SERVER_STR:
                    adjacency[src].add(API_SERVER_STR)
                    adjacency[API_SERVER_STR].add(src)
                    referenced_entities.add(API_SERVER_STR)

        if runtime_entities:
            root = MAIN_SERVER_STR if MAIN_SERVER_STR in adjacency else runtime_entities[0]
            stack = [root]
            visited = set()
            while stack:
                node = stack.pop()
                if node in visited:
                    continue
                visited.add(node)
                stack.extend(neighbor for neighbor in adjacency[node] if neighbor not in visited)

            disconnected = sorted(name for name in runtime_entities if name not in visited)
            if disconnected:
                raise ValueError(
                    "Connection map must produce one fully connected runtime graph after "
                    f"bidirectional completion. Disconnected entities: {disconnected}"
                )

        for super_name in self.super_nodes:
            if super_name not in referenced_entities:
                raise ValueError(
                    f"Super node '{super_name}' must appear in the connection map adjacency"
                )

    def _extract_super_nodes(self):
        super_nodes_json = self.jsonData.get(KEY_SUPER_NODES, [])
        if super_nodes_json is None:
            super_nodes_json = []
        if not isinstance(super_nodes_json, list):
            raise ValueError(f"'{KEY_SUPER_NODES}' must be a list when provided")

        for super_node in super_nodes_json:
            if not isinstance(super_node, dict):
                raise ValueError("Each super node entry must be an object")
            name = str(super_node.get(NAME_FIELD, "")).strip()
            if not name:
                raise ValueError("Super node must include a non-empty name")
            if name in self.map_name_to_type:
                raise ValueError(f"Duplicate entity name found for super node: {name}")
            self.super_nodes.append(name)
            self.map_name_to_type[name] = TYPE_SUPER_NODE

            port_value = super_node.get(PORT_FIELD, None)
            try:
                port = int(port_value)
            except (TypeError, ValueError) as exc:
                raise ValueError(f"Super node '{name}' has invalid port '{port_value}'") from exc
            if port < 1 or port > 65535:
                raise ValueError(f"Super node '{name}' port must be in range [1, 65535]")
            self.super_node_ports[name] = port

            heartbeat_ms_value = super_node.get(HEARTBEAT_MS_FIELD, 1000)
            max_inflight_value = super_node.get(MAX_INFLIGHT_MICROBATCHES_FIELD, 1)
            try:
                heartbeat_ms = int(heartbeat_ms_value)
                max_inflight = int(max_inflight_value)
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    f"Super node '{name}' heartbeat/maxInflight must be integers"
                ) from exc
            if heartbeat_ms < 1:
                raise ValueError(f"Super node '{name}' heartbeatMs must be >= 1")
            if max_inflight < 1:
                raise ValueError(f"Super node '{name}' maxInflightMicrobatches must be >= 1")
            self.super_node_heartbeat_ms[name] = heartbeat_ms
            self.super_node_max_inflight_microbatches[name] = max_inflight

            managed_clients = super_node.get(MANAGED_CLIENTS_FIELD, [])
            if isinstance(managed_clients, str):
                managed_clients = [entry.strip() for entry in managed_clients.split(",") if entry.strip()]
            if not isinstance(managed_clients, list):
                raise ValueError(f"Super node '{name}' has invalid '{MANAGED_CLIENTS_FIELD}' field")

            normalized_clients = [str(client_name).strip() for client_name in managed_clients if str(client_name).strip()]
            self.super_node_to_clients[name] = normalized_clients

            for client_name in normalized_clients:
                owner = self.client_to_super_node.get(client_name)
                if owner and owner != name:
                    raise ValueError(
                        f"Client '{client_name}' is managed by multiple super nodes: '{owner}' and '{name}'"
                    )
                self.client_to_super_node[client_name] = name

    def _extract_model_tp_plans(self):
        for model_sha, model_payload in self.model_sha_map.items():
            tp_plan = model_payload.get(TP_PLAN_FIELD, None)
            if tp_plan is None:
                continue
            if not isinstance(tp_plan, list):
                raise ValueError(f"Model '{model_sha}' has invalid '{TP_PLAN_FIELD}' (must be list)")
            normalized_plan = []
            for idx, layer_plan in enumerate(tp_plan):
                if not isinstance(layer_plan, dict):
                    raise ValueError(f"Model '{model_sha}' tpPlan entry #{idx} must be an object")
                layer_name = str(layer_plan.get("layer", "")).strip()
                mode = str(layer_plan.get("mode", "")).strip().lower()
                group = str(layer_plan.get("group", "")).strip()
                shard_axis_raw = layer_plan.get("shardAxis", 0)
                try:
                    shard_axis = int(shard_axis_raw)
                except (TypeError, ValueError) as exc:
                    raise ValueError(
                        f"Model '{model_sha}' tpPlan entry #{idx} has invalid shardAxis '{shard_axis_raw}'"
                    ) from exc
                normalized_plan.append(
                    {
                        "layer": layer_name,
                        "mode": mode,
                        "group": group,
                        "shardAxis": shard_axis,
                    }
                )
            self.model_tp_plan_map[model_sha] = normalized_plan

    def _normalize_worker_parallel(self, worker_name: str, worker_parallel: dict):
        if not isinstance(worker_parallel, dict):
            raise ValueError(f"Worker '{worker_name}' field '{PARALLEL_FIELD}' must be an object")

        normalized = {}

        def _coerce_int(field_name: str, min_value=None):
            if field_name not in worker_parallel:
                return
            raw_value = worker_parallel.get(field_name)
            try:
                value = int(raw_value)
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    f"Worker '{worker_name}' parallel field '{field_name}' must be an integer"
                ) from exc
            if min_value is not None and value < min_value:
                raise ValueError(
                    f"Worker '{worker_name}' parallel field '{field_name}' must be >= {min_value}"
                )
            normalized[field_name] = value

        _coerce_int(PIPELINE_STAGE_FIELD, min_value=0)
        _coerce_int(PIPELINE_WORLD_SIZE_FIELD, min_value=1)
        _coerce_int(TP_RANK_FIELD, min_value=0)
        _coerce_int(TP_WORLD_SIZE_FIELD, min_value=1)

        for key in (TP_GROUP_FIELD,):
            value = worker_parallel.get(key)
            if value is None:
                continue
            normalized[key] = str(value).strip()

        return normalized

    def _validate_super_nodes(self):
        for super_name in self.super_nodes:
            if super_name not in self.map_entity_to_device:
                raise ValueError(f"Super node '{super_name}' must be assigned to exactly one device")
            managed_clients = self.super_node_to_clients.get(super_name, [])
            if len(managed_clients) == 0:
                raise ValueError(f"Super node '{super_name}' must manage at least one client")
            for client_name in managed_clients:
                if client_name not in self.clients:
                    raise ValueError(
                        f"Super node '{super_name}' references unknown client '{client_name}'"
                    )

        for client_name, super_name in self.client_to_super_node.items():
            if client_name not in self.clients:
                raise ValueError(f"Unknown client '{client_name}' declared under super node '{super_name}'")
            if super_name not in self.super_nodes:
                raise ValueError(f"Client '{client_name}' references unknown super node '{super_name}'")
            managed_clients = self.super_node_to_clients.get(super_name, [])
            if client_name not in managed_clients:
                raise ValueError(
                    f"Client '{client_name}' is assigned to super node '{super_name}' but is missing "
                    f"from '{MANAGED_CLIENTS_FIELD}'"
                )

    def _validate_worker_parallel(self):
        tp_groups = {}
        for worker_name, parallel_cfg in self.worker_parallel_map.items():
            if worker_name not in self.workers:
                raise ValueError(f"Parallel configuration references unknown worker '{worker_name}'")

            pipeline_stage = parallel_cfg.get(PIPELINE_STAGE_FIELD)
            pipeline_world_size = parallel_cfg.get(PIPELINE_WORLD_SIZE_FIELD)
            tp_rank = parallel_cfg.get(TP_RANK_FIELD)
            tp_world_size = parallel_cfg.get(TP_WORLD_SIZE_FIELD)
            tp_group = parallel_cfg.get(TP_GROUP_FIELD, "")

            if (pipeline_stage is None) != (pipeline_world_size is None):
                raise ValueError(
                    f"Worker '{worker_name}' must define both {PIPELINE_STAGE_FIELD} and "
                    f"{PIPELINE_WORLD_SIZE_FIELD} together"
                )

            if pipeline_stage is not None and pipeline_world_size is not None and pipeline_stage >= pipeline_world_size:
                raise ValueError(
                    f"Worker '{worker_name}' has invalid pipeline stage {pipeline_stage} for world size {pipeline_world_size}"
                )

            if tp_rank is not None and tp_world_size is not None and tp_rank >= tp_world_size:
                raise ValueError(
                    f"Worker '{worker_name}' has invalid TP rank {tp_rank} for world size {tp_world_size}"
                )

            if tp_group and (tp_rank is None or tp_world_size is None):
                raise ValueError(
                    f"Worker '{worker_name}' TP config must include both {TP_RANK_FIELD} and "
                    f"{TP_WORLD_SIZE_FIELD} when {TP_GROUP_FIELD} is set"
                )
            if (tp_rank is not None or tp_world_size is not None) and not tp_group:
                raise ValueError(
                    f"Worker '{worker_name}' TP rank/world size requires non-empty {TP_GROUP_FIELD}"
                )

            if tp_group:
                group = tp_groups.setdefault(tp_group, {"world_size": tp_world_size, "ranks": set()})
                if group["world_size"] is None:
                    group["world_size"] = tp_world_size
                elif tp_world_size is not None and group["world_size"] != tp_world_size:
                    raise ValueError(
                        f"TP group '{tp_group}' has inconsistent world size between workers"
                    )
                if tp_rank is not None:
                    if tp_rank in group["ranks"]:
                        raise ValueError(
                            f"TP group '{tp_group}' contains duplicate rank {tp_rank}"
                        )
                    group["ranks"].add(tp_rank)

        for group_name, info in tp_groups.items():
            world_size = info["world_size"]
            ranks = info["ranks"]
            if world_size is not None and len(ranks) not in (0, world_size):
                raise ValueError(
                    f"TP group '{group_name}' expected {world_size} ranks but found {len(ranks)}"
                )

    def _validate_pipeline_tp_compatibility(self):
        pipeline_workers = {
            worker_name: cfg
            for worker_name, cfg in self.worker_parallel_map.items()
            if PIPELINE_STAGE_FIELD in cfg
        }
        if pipeline_workers:
            world_sizes = {
                cfg.get(PIPELINE_WORLD_SIZE_FIELD)
                for cfg in pipeline_workers.values()
                if cfg.get(PIPELINE_WORLD_SIZE_FIELD) is not None
            }
            if len(world_sizes) > 1:
                raise ValueError("Pipeline workers must share the same pipelineWorldSize")
            pipeline_world_size = next(iter(world_sizes)) if world_sizes else 1
            stages_present = sorted(
                {
                    cfg.get(PIPELINE_STAGE_FIELD)
                    for cfg in pipeline_workers.values()
                    if cfg.get(PIPELINE_STAGE_FIELD) is not None
                }
            )
            expected_stages = list(range(pipeline_world_size))
            if stages_present != expected_stages:
                raise ValueError(
                    f"Pipeline stage coverage mismatch. expected={expected_stages} got={stages_present}"
                )

        tp_group_to_stage = {}
        for worker_name, cfg in self.worker_parallel_map.items():
            tp_group = cfg.get(TP_GROUP_FIELD, "")
            if not tp_group:
                continue
            stage = cfg.get(PIPELINE_STAGE_FIELD)
            if stage is not None:
                existing_stage = tp_group_to_stage.get(tp_group)
                if existing_stage is None:
                    tp_group_to_stage[tp_group] = stage
                elif existing_stage != stage:
                    raise ValueError(
                        f"TP group '{tp_group}' spans multiple pipeline stages "
                        f"({existing_stage}, {stage}) via worker '{worker_name}'"
                    )

    def _validate_model_tp_plans(self):
        valid_modes = {"column", "row"}
        for model_sha, tp_plan in self.model_tp_plan_map.items():
            for idx, plan_entry in enumerate(tp_plan):
                if not plan_entry["layer"]:
                    raise ValueError(f"Model '{model_sha}' tpPlan entry #{idx} is missing layer name")
                if plan_entry["mode"] not in valid_modes:
                    raise ValueError(
                        f"Model '{model_sha}' tpPlan entry #{idx} has invalid mode '{plan_entry['mode']}'"
                    )
                if not plan_entry["group"]:
                    raise ValueError(
                        f"Model '{model_sha}' tpPlan entry #{idx} must include a non-empty TP group"
                    )

    def has_torch_models(self) -> bool:
        return bool(self.torch_model_assets)

    def get_torch_model_assets(self):
        return dict(self.torch_model_assets)

    def _extract_torch_assets(self):
        assets = {}
        if not self.model_sha_map:
            return assets

        for model_sha, model_payload in self.model_sha_map.items():
            infra_type = str(model_payload.get('infraType', '')).lower()
            if infra_type != 'torch':
                continue

            pt_path = model_payload.get('pt_path')
            if not pt_path:
                raise ValueError(f"Torch model {model_sha} is missing 'pt_path' in distributed config")

            resolved_path = self._resolve_asset_path(pt_path)
            if not os.path.isfile(resolved_path):
                raise FileNotFoundError(f"Torch model file not found for {model_sha}: {resolved_path}")

            remote_path = build_torch_remote_model_path(model_sha, os.path.basename(resolved_path))
            actual_checksum = self._calculate_sha256(resolved_path)
            expected_checksum = model_payload.get('pt_checksum')
            if expected_checksum and expected_checksum.lower() not in ('placeholder', 'none'):
                if actual_checksum.lower() != expected_checksum.lower():
                    raise ValueError(f"Checksum mismatch for Torch model {model_sha}")

            assets[model_sha] = {
                'sha': model_sha,
                'local_path': resolved_path,
                'remote_path': remote_path,
                'format': model_payload.get('pt_format', 'torchscript'),
                'description': model_payload.get('pt_description', ''),
                'checksum': actual_checksum,
            }

        return assets

    def _resolve_asset_path(self, asset_path: str) -> str:
        normalized = asset_path.strip()
        candidate_paths = []
        if os.path.isabs(normalized):
            candidate_paths.append(normalized)
        else:
            candidate_paths.append(os.path.abspath(normalized))
            candidate_paths.append(os.path.abspath(os.path.join(NERLNET_PATH, normalized)))

        for candidate in candidate_paths:
            if os.path.isfile(candidate):
                return candidate

        raise FileNotFoundError(f"Unable to resolve Torch model path: {asset_path}")

    def _calculate_sha256(self, file_path: str) -> str:
        sha256_hash = hashlib.sha256()
        with open(file_path, 'rb') as file_obj:
            for chunk in iter(lambda: file_obj.read(1024 * 1024), b''):
                sha256_hash.update(chunk)
        return sha256_hash.hexdigest()

         
    def toString(self, char): #Prints the contents of any of the components' lists (e.g. "routers")
        if char == 'd':
            return ','.join(self.devicesIp)
        elif char == 'c':
            return ','.join(self.clients)
        elif char == 'w':
            return ','.join(self.workers)
        elif char == 's':
            return ','.join(self.sources)
        elif char == 'r':
            return ','.join(self.routers)
        else:
            raise ValueError('Not a valid char!\n \
Please enter a valid char as input:\n \
d - devices Ip\n \
c - clients\n \
w - workers\n \
s - sources\n \
r - routers')

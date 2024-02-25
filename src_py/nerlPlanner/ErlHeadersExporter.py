import argparse
import os
from ErlHeadersExporterDefs import *
from JsonDistributedConfigDefs import *
from JsonElementWorkerDefinitions import *
from JsonElementsDefinitions import *
from Definitions import VERSION as NERLPLANNER_VERSION

EMPTY_LINE = '\n'
DEBUG = False

def gen_erlang_exporter_logger(message : str):
    if DEBUG:
        print(f'[NERLPLANNER][AUTO_HEADER_GENERATOR][DEBUG] {message}')

def path_validator(path : str):
    if os.path.dirname(path):
        os.makedirs(os.path.dirname(path), exist_ok=True)

def gen_worker_fields_hrl(header_path : str, debug : bool = False):
    global DEBUG
    DEBUG = debug

    auto_generated_header = AutoGeneratedHeader()
    gen_erlang_exporter_logger(auto_generated_header.generate_code())

    nerlplanner_version = Comment(f'Worker Fields Generated by Nerlplanner version: {NERLPLANNER_VERSION}')
    gen_erlang_exporter_logger(nerlplanner_version.generate_code())

    fields_list_vals = [KEY_MODEL_TYPE, KEY_LAYER_SIZES_LIST,
                   KEY_LAYER_TYPES_LIST, KEY_LAYERS_FUNCTIONS,
                   KEY_LOSS_METHOD, KEY_LEARNING_RATE,
                   KEY_EPOCHS, KEY_OPTIMIZER_TYPE, KEY_OPTIMIZER_ARGS, KEY_INFRA_TYPE,
                   KEY_DISTRIBUTED_SYSTEM_TYPE, KEY_DISTRIBUTED_SYSTEM_TOKEN, KEY_DISTRIBUTED_SYSTEM_ARGS]
    fields_list_strs = ['KEY_MODEL_TYPE', 'KEY_LAYER_SIZES_LIST',
                   'KEY_LAYER_TYPES_LIST', 'KEY_LAYERS_FUNCTIONS',
                   'KEY_LOSS_METHOD', 'KEY_LEARNING_RATE',
                   'KEY_EPOCHS', 'KEY_OPTIMIZER_TYPE', 'KEY_OPTIMIZER_ARGS', 'KEY_INFRA_TYPE',
                   'KEY_DISTRIBUTED_SYSTEM_TYPE', 'KEY_DISTRIBUTED_SYSTEM_TOKEN', 'KEY_DISTRIBUTED_SYSTEM_ARGS']
    fields_list_strs = [f'WORKER_FIELD_{x}' for x in fields_list_strs]

    fields_list_defs = [ Definition(fields_list_strs[idx], f'{Definition.assert_not_atom(fields_list_vals[idx])}') for idx in range(len(fields_list_vals))]
    [gen_erlang_exporter_logger(x.generate_code()) for x in fields_list_defs]

    fields_list_defs_str_bins = [ Definition(f'{fields_list_strs[idx]}_BIN', f'<<"{fields_list_vals[idx]}">>') for idx in range(len(fields_list_strs))]
    [gen_erlang_exporter_logger(x.generate_code()) for x in fields_list_defs_str_bins]
    

    distributed_system_type_definition_key_atom_list = []
    distributed_system_type_definition_idx_str_list = []
    distributed_system_type_definition_idx_list = []

    for key, val in DistributedSystemTypeMapping.items():
        distributed_system_type_definition_idx_str = Definition(f'DC_DISTRIBUTED_SYSTEM_TYPE_{key.upper()}_IDX_STR', f'"{val}"')
        gen_erlang_exporter_logger(distributed_system_type_definition_idx_str.generate_code())
        distributed_system_type_definition_key_atom = Definition(f'DC_DISTRIBUTED_SYSTEM_TYPE_{key.upper()}_KEY_ATOM', f'{Definition.force_atom_value(key)}')
        gen_erlang_exporter_logger(distributed_system_type_definition_key_atom.generate_code())
        distributed_system_type_definition_idx = Definition(f'DC_DISTRIBUTED_SYSTEM_TYPE_{key.upper()}_IDX', f'{val}')
        gen_erlang_exporter_logger(distributed_system_type_definition_idx.generate_code())
        distributed_system_type_definition_idx_str_list.append(distributed_system_type_definition_idx_str)
        distributed_system_type_definition_key_atom_list.append(distributed_system_type_definition_key_atom)
        distributed_system_type_definition_idx_list.append(distributed_system_type_definition_idx)

    infra_type_definition_key_atom_list = []
    infra_type_definition_idx_str_list = []
    infra_type_definition_idx_list = []

    for key, val in InfraTypeMapping.items():
        infra_type_definition_idx_str = Definition(f'DC_INFRA_TYPE_{key.upper()}_IDX_STR', f'"{val}"')
        gen_erlang_exporter_logger(infra_type_definition_idx_str.generate_code())
        infra_type_definition_key_atom = Definition(f'DC_INFRA_TYPE_{key.upper()}_KEY_ATOM', f'{Definition.force_atom_value(key)}')
        gen_erlang_exporter_logger(infra_type_definition_key_atom.generate_code())
        infra_type_definition_idx = Definition(f'DC_INFRA_TYPE_{key.upper()}_IDX', f'{val}')
        gen_erlang_exporter_logger(infra_type_definition_idx.generate_code())
        infra_type_definition_idx_str_list.append(infra_type_definition_idx_str)
        infra_type_definition_key_atom_list.append(infra_type_definition_key_atom)
        infra_type_definition_idx_list.append(infra_type_definition_idx)

    path_validator(header_path)

    with open(header_path, 'w') as f:
        f.write(auto_generated_header.generate_code())
        f.write(nerlplanner_version.generate_code())
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in fields_list_defs]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in fields_list_defs_str_bins]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in distributed_system_type_definition_key_atom_list]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in distributed_system_type_definition_idx_str_list]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in distributed_system_type_definition_idx_list]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in infra_type_definition_key_atom_list]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in infra_type_definition_idx_str_list]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in infra_type_definition_idx_list]


def gen_source_fields_hrl(header_path : str, debug : bool = False):
    global DEBUG
    DEBUG = debug

    auto_generated_header = AutoGeneratedHeader()
    gen_erlang_exporter_logger(auto_generated_header.generate_code())

    nerlplanner_version = Comment(f'Source Fields Generated by Nerlplanner version: {NERLPLANNER_VERSION}')
    gen_erlang_exporter_logger(nerlplanner_version.generate_code())

    source_policy_definitions_indexes = []
    source_policy_definitions_atoms = []
    
    source_type_definitions_indexes = []
    source_type_definitions_atoms = []

    for key, val in SourcePolicyDict.items():
        source_policy_definition = Definition(f'SOURCE_POLICY_{key.upper()}_IDX', f'"{val}"')
        gen_erlang_exporter_logger(source_policy_definition.generate_code())
        source_policy_definitions_indexes.append(source_policy_definition)
        source_policy_definition = Definition(f'SOURCE_POLICY_{key.upper()}_ATOM', f'{Definition.force_atom_value(key)}')
        source_policy_definitions_atoms.append(source_policy_definition)
        
    for key , val in SourceTypeDict.items():
        source_type_definition = Definition(f'SOURCE_TYPE_{key.upper()}_IDX', f'"{val}"')
        gen_erlang_exporter_logger(source_type_definition.generate_code())
        source_type_definitions_indexes.append(source_type_definition)
        source_type_definition = Definition(f'SOURCE_TYPE_{key.upper()}_ATOM', f'{Definition.force_atom_value(key)}')
        source_type_definitions_atoms.append(source_type_definition)
    
    path_validator(header_path)

    with open(header_path, 'w') as f:
        f.write(auto_generated_header.generate_code())
        f.write(nerlplanner_version.generate_code())
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in source_policy_definitions_indexes]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in source_policy_definitions_atoms]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in source_type_definitions_indexes]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in source_type_definitions_atoms]

def gen_router_fields_hrl(header_path : str, debug : bool = False):
    global DEBUG
    DEBUG = debug

    auto_generated_header = AutoGeneratedHeader()
    gen_erlang_exporter_logger(auto_generated_header.generate_code())

    nerlplanner_version = Comment(f'Source Fields Generated by Nerlplanner version: {NERLPLANNER_VERSION}')
    gen_erlang_exporter_logger(nerlplanner_version.generate_code())

    router_policy_definitions_indexes = []
    router_policy_definitions_atoms = []

    for key, val in RouterPolicyDict.items():
        router_policy_definition = Definition(f'ROUTER_POLICY_{key.upper()}_IDX', f'"{val}"')
        gen_erlang_exporter_logger(router_policy_definition.generate_code())
        router_policy_definitions_indexes.append(router_policy_definition)
        router_policy_definition = Definition(f'ROUTER_POLICY_{key.upper()}_ATOM', f'{Definition.force_atom_value(key)}')
        router_policy_definitions_atoms.append(router_policy_definition)
    
    path_validator(header_path)

    with open(header_path, 'w') as f:
        f.write(auto_generated_header.generate_code())
        f.write(nerlplanner_version.generate_code())
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in router_policy_definitions_indexes]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in router_policy_definitions_atoms]



def gen_dc_fields_hrl(header_path : str, debug : bool = False):
    global DEBUG
    DEBUG = debug

    auto_generated_header = AutoGeneratedHeader()
    gen_erlang_exporter_logger(auto_generated_header.generate_code())

    nerlplanner_version = Comment(f'DC Fields Generated by Nerlplanner version: {NERLPLANNER_VERSION}')
    gen_erlang_exporter_logger(nerlplanner_version.generate_code())

    fields_list_vals_atoms = [KEY_NERLNET_SETTINGS, KEY_FREQUENCY, KEY_BATCH_SIZE,
                        KEY_DEVICES, KEY_CLIENTS, KEY_WORKERS, KEY_MODEL_SHA,
                        KEY_SOURCES, KEY_ROUTERS, NAME_FIELD, WORKER_MODEL_SHA_FIELD,
                        IPV4_FIELD, PORT_FIELD, ARGS_FIELD, ENTITIES_FIELD,
                        POLICY_FIELD, EPOCHS_FIELD, TYPE_FIELD, FREQUENCY_FIELD,
                        WORKERS_FIELD]
    fields_list_vals_strs = [f'"{x}"' for x in fields_list_vals_atoms]
    fields_list_strs = ['KEY_NERLNET_SETTINGS', 'KEY_FREQUENCY', 'KEY_BATCH_SIZE',
                        'KEY_DEVICES', 'KEY_CLIENTS', 'KEY_WORKERS', 'KEY_MODEL_SHA',
                        'KEY_SOURCES', 'KEY_ROUTERS', 'NAME_FIELD', 'WORKER_MODEL_SHA_FIELD',
                        'IPV4_FIELD', 'PORT_FIELD', 'ARGS_FIELD', 'ENTITIES_FIELD',
                        'POLICY_FIELD', 'EPOCHS_FIELD', 'TYPE_FIELD', 'FREQUENCY_FIELD',
                        'WORKERS_FIELD']
    fields_list_strs_atom = [f'DC_{x}_ATOM' for x in fields_list_strs]
    fields_list_strs_string = [f'DC_{x}_STR' for x in fields_list_strs]
    fields_list_strs_bin = [f'DC_{x}_STR_BIN' for x in fields_list_strs]

    fields_list_defs_atoms = [ Definition(fields_list_strs_atom[idx], f'{fields_list_vals_atoms[idx]}') for idx in range(len(fields_list_strs))]
    [gen_erlang_exporter_logger(x.generate_code()) for x in fields_list_defs_atoms]

    fields_list_defs_strings = [ Definition(fields_list_strs_string[idx], f'{fields_list_vals_strs[idx]}') for idx in range(len(fields_list_strs))]
    [gen_erlang_exporter_logger(x.generate_code()) for x in fields_list_defs_strings]

    fields_list_defs_str_bins = [ Definition(fields_list_strs_bin[idx], f'<<{fields_list_vals_strs[idx]}>>') for idx in range(len(fields_list_strs))]
    [gen_erlang_exporter_logger(x.generate_code()) for x in fields_list_defs_str_bins]

    path_validator(header_path)

    with open(header_path, 'w') as f:
        f.write(auto_generated_header.generate_code())
        f.write(nerlplanner_version.generate_code())
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in fields_list_defs_atoms]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in fields_list_defs_strings]
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in fields_list_defs_str_bins]

def gen_layers_type_hrl(header_path : str, debug : bool = False):
    global DEBUG
    DEBUG = debug


    auto_generated_header = AutoGeneratedHeader()
    gen_erlang_exporter_logger(auto_generated_header.generate_code())

    nerlplanner_version = Comment(f'DC Fields Generated by Nerlplanner version: {NERLPLANNER_VERSION}')
    gen_erlang_exporter_logger(nerlplanner_version.generate_code())

    layers_type_index_defs_list = []
    for key,val in LayerTypeMap.items():
        layer_type_definition = Definition(f'LAYERS_TYPE_{key.upper()}_IDX', f'"{val}"')
        layers_type_index_defs_list.append(layer_type_definition)
        gen_erlang_exporter_logger(layer_type_definition.generate_code())

    path_validator(header_path)

    with open(header_path, 'w') as f:
        f.write(auto_generated_header.generate_code())
        f.write(nerlplanner_version.generate_code())
        f.write(EMPTY_LINE)
        [f.write(x.generate_code()) for x in layers_type_index_defs_list]
    
def main():
    parser = argparse.ArgumentParser(description='Generate C++ header file for nerlPlanner')
    parser.add_argument('-o', '--output', help='output header file path', required=True)
    parser.add_argument('-d', '--debug', help='debug mode', action='store_true')
    parser.add_argument('--gen_worker_fields_hrl', help='debug mode', action='store_true')
    parser.add_argument('--gen_dc_fields_hrl', help='debug mode', action='store_true')
    parser.add_argument('--gen_source_fields_hrl', help='debug mode', action='store_true')
    parser.add_argument('--gen_router_fields_hrl', help='debug mode', action='store_true')
    parser.add_argument('--gen_layers_type_hrl', help='debug mode', action='store_true')

    args = parser.parse_args()
    if args.gen_worker_fields_hrl:
        gen_worker_fields_hrl(args.output, args.debug)
    if args.gen_dc_fields_hrl:
        gen_dc_fields_hrl(args.output, args.debug)
    if args.gen_source_fields_hrl:
        gen_source_fields_hrl(args.output, args.debug)
    if args.gen_router_fields_hrl:
        gen_router_fields_hrl(args.output, args.debug)
    if args.gen_layers_type_hrl:
        gen_layers_type_hrl(args.output, args.debug)

if __name__=="__main__":
    main()


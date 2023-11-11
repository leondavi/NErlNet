import argparse
import os
from CppHeadersExporterDefs import *
from JsonElementWorkerDefinitions import *
from Definitions import VERSION as NERLPLANNER_VERSION

DEBUG = False

def gen_header_exporter_logger(message : str):
    if DEBUG:
        print(f'[NERLPLANNER][AUTO_HEADER_GENERATOR][DEBUG] {message}')

def gen_header_worker_parameters_definitions(header_path : str, debug : bool = False):
    global DEBUG
    DEBUG = debug

    empty_line = '\n'
    pragma_once = PragmaOnce()
    gen_header_exporter_logger(pragma_once.generate_code())
    auto_generated_header = AutoGeneratedHeader()
    gen_header_exporter_logger(auto_generated_header.generate_code())
    nerlplanner_version = Comment(f'Generated by Nerlplanner version: {NERLPLANNER_VERSION}')

    layer_type_enums = EnumType('LayerTypeEnum', LayerTypeMap, True, 'LAYER_TYPE')
    gen_header_exporter_logger(layer_type_enums.generate_code())
    probabilistic_activation_enums = EnumType('ProbabilisticActivationEnum', ProbabilisticActivationFunctionMap, True, 'PROBABILISTIC_ACTIVATION')
    gen_header_exporter_logger(probabilistic_activation_enums.generate_code())
    scaling_enums = EnumType('ScalingEnum', ScalingMethodMap, True, 'SCALING')
    gen_header_exporter_logger(scaling_enums.generate_code())
    unscaling_enums = EnumType('UnscalingEnum', UnScalingMethodMap, True, 'UNSCALING')
    gen_header_exporter_logger(unscaling_enums.generate_code())
    pooling_enums = EnumType('PoolingEnum', PoolingMethodMap, True, 'POOLING')
    gen_header_exporter_logger(pooling_enums.generate_code())
    activation_enums = EnumType('ActivationEnum', ActivationFunctionsMap, True, 'ACTIVATION')
    gen_header_exporter_logger(activation_enums.generate_code())
    layer_type_enums = EnumType('LayerTypeEnum', LayerTypeMap, True, 'LAYER_TYPE')
    gen_header_exporter_logger(layer_type_enums.generate_code())
    model_type_enums = EnumType('ModelTypeEnum', ModelTypeMapping, True, 'MODEL_TYPE')
    gen_header_exporter_logger(model_type_enums.generate_code())
    optimizer_enums = EnumType('OptimizerEnum', OptimizerTypeMapping, True, 'OPTIMIZER')
    gen_header_exporter_logger(optimizer_enums.generate_code())
    loss_method_enums = EnumType('LossMethodEnum', LossMethodMapping, True, 'LOSS_METHOD')
    gen_header_exporter_logger(loss_method_enums.generate_code())

    if os.path.dirname(header_path):
        os.makedirs(os.path.dirname(header_path), exist_ok=True)

    with open(header_path, 'w') as f:
        f.write(pragma_once.generate_code())
        f.write(empty_line)
        f.write(auto_generated_header.generate_code())
        f.write(nerlplanner_version.generate_code())
        f.write(empty_line)
        f.write(layer_type_enums.generate_code())
        f.write(probabilistic_activation_enums.generate_code())
        f.write(scaling_enums.generate_code())
        f.write(unscaling_enums.generate_code())
        f.write(pooling_enums.generate_code())
        f.write(activation_enums.generate_code())
        f.write(layer_type_enums.generate_code())
        f.write(model_type_enums.generate_code())
        f.write(optimizer_enums.generate_code())
        f.write(loss_method_enums.generate_code())




def main():
    parser = argparse.ArgumentParser(description='Generate C++ header file for nerlPlanner')
    parser.add_argument('-o', '--output', help='output header file path', required=True)
    parser.add_argument('-d', '--debug', help='debug mode', action='store_true')
    args = parser.parse_args()
    gen_header_worker_parameters_definitions(args.output, args.debug)

if __name__=="__main__":
    main()


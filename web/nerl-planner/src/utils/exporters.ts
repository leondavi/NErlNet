import {
  activationFunctionOptions,
  boundingMethodOptions,
  distributedSystemOptions,
  layerTypeOptions,
  lossMethodOptions,
  modelTypeOptions,
  optimizerOptions,
  poolingMethodOptions,
  probabilisticFunctionOptions,
  scalingMethodOptions,
  unscalingMethodOptions
} from '../data/mappings';
import {
  ConnectionEdge,
  ExperimentFlow,
  PlannerState,
  WorkerModel
} from '../data/types';
import { sha256String } from './sha';
import { stableStringify } from './serialize';

const docStringFromOptions = (options: { label: string; value: string; docLabel?: string }[]) =>
  options.map((option) => ` ${(option.docLabel ?? option.label)}:${option.value} |`).join('');

const MODEL_DOCS = {
  modelType: docStringFromOptions(modelTypeOptions),
  layerTypes: docStringFromOptions(layerTypeOptions),
  lossMethod: docStringFromOptions(lossMethodOptions),
  optimizer: docStringFromOptions(optimizerOptions),
  infraType: ' opennn:0 | wolfengine:1 |',
  distributedSystem: docStringFromOptions(distributedSystemOptions),
  activation: docStringFromOptions(activationFunctionOptions),
  pooling: docStringFromOptions(poolingMethodOptions),
  probabilistic: docStringFromOptions(probabilisticFunctionOptions),
  scaling: docStringFromOptions(scalingMethodOptions),
  unscaling: docStringFromOptions(unscalingMethodOptions),
  bounding: docStringFromOptions(boundingMethodOptions)
};

const TorchDoc = {
  infraType: ' opennn:0 | wolfengine:1 | torch:2 |',
  distributedSystem: MODEL_DOCS.distributedSystem
};

export async function buildDistributedConfig(
  state: PlannerState,
  includeDocs: boolean
): Promise<Record<string, unknown>> {
  const modelPayloads: Record<string, Record<string, unknown>> = {};
  const workerEntries: { name: string; model_sha: string }[] = [];

  for (const worker of state.workers) {
    const model = state.models.find((entry) => entry.id === worker.modelId);
    if (!model) {
      continue;
    }

    const payload = buildModelPayload(model, includeDocs);
    const sha = await sha256String(stableStringify(payload));

    if (!modelPayloads[sha]) {
      modelPayloads[sha] = payload;
    }

    workerEntries.push({ name: worker.name, model_sha: sha });
  }

  return {
    nerlnetSettings: {
      frequency: state.settings.frequency,
      batchSize: state.settings.batchSize
    },
    mainServer: {
      port: state.servers.mainServer.port,
      args: state.servers.mainServer.args
    },
    apiServer: {
      port: state.servers.apiServer.port,
      args: state.servers.apiServer.args
    },
    devices: state.devices.map((device) => ({
      name: device.name,
      ipv4: device.ipv4,
      entities: device.entities.join(',')
    })),
    routers: state.routers.map((router) => ({
      name: router.name,
      port: router.port,
      policy: router.policy
    })),
    sources: state.sources.map((source) => ({
      name: source.name,
      port: source.port,
      frequency: source.frequency,
      policy: source.policy,
      epochs: source.epochs,
      type: source.type
    })),
    clients: state.clients.map((client) => ({
      name: client.name,
      port: client.port,
      workers: client.workers.join(',')
    })),
    workers: workerEntries,
    model_sha: modelPayloads
  };
}

export function buildConnectionMap(connections: ConnectionEdge[]): Record<string, unknown> {
  const map: Record<string, string[]> = {};
  for (const edge of connections) {
    if (!map[edge.from]) {
      map[edge.from] = [];
    }
    if (!map[edge.from].includes(edge.to)) {
      map[edge.from].push(edge.to);
    }
  }
  return { connectionsMap: map };
}

export function buildExperimentFlow(experimentFlow: ExperimentFlow): Record<string, unknown> {
  return {
    experimentName: experimentFlow.experimentName,
    experimentType: experimentFlow.experimentType,
    batchSize: toNumberOrString(experimentFlow.batchSize),
    csvFilePath: experimentFlow.csvFilePath,
    numOfFeatures: experimentFlow.numOfFeatures,
    numOfLabels: experimentFlow.numOfLabels,
    headersNames: experimentFlow.headersNames,
    Phases: experimentFlow.phases.map((phase) => ({
      phaseName: phase.phaseName,
      phaseType: phase.phaseType,
      sourcePieces: phase.sourcePieces.map((piece) => ({
        sourceName: piece.sourceName,
        startingSample: piece.startingSample,
        numOfBatches: piece.numOfBatches,
        workers: piece.workers.join(','),
        nerltensorType: piece.nerltensorType
      }))
    }))
  };
}

export function buildModelPayload(model: WorkerModel, includeDocs: boolean): Record<string, unknown> {
  if (model.infraType === 'torch') {
    const layers = model.layers ?? [];
    const layerPayload =
      layers.length > 0
        ? {
            layersSizes: layers.map((layer) => layer.size).join(','),
            layerTypesList: layers.map((layer) => layer.type).join(','),
            layers_functions: layers.map((layer) => layer.functionCode).join(',')
          }
        : {};
    const base: Record<string, unknown> = {
      infraType: model.infraType,
      distributedSystemType: model.distributedSystemType,
      distributedSystemArgs: model.distributedSystemArgs,
      distributedSystemToken: model.distributedSystemToken,
      pt_path: model.ptPath,
      pt_format: model.ptFormat,
      pt_checksum: model.ptChecksum,
      pt_description: model.ptDescription,
      train_params: {
        lr: model.trainParams.lr,
        epochs: model.trainParams.epochs,
        optimizer: model.trainParams.optimizer,
        batch_size: model.trainParams.batchSize,
        input_tensor_shape: model.trainParams.inputTensorShape,
        labels_offset: model.trainParams.labelsOffset,
        labels_shape: model.trainParams.labelsShape,
        w_init_rand: model.trainParams.wInitRand
      },
      ...layerPayload
    };

    if (!includeDocs) {
      return base;
    }

    return {
      ...base,
      _doc_infraType: TorchDoc.infraType,
      _doc_distributedSystemType: TorchDoc.distributedSystem,
      _doc_distributedSystemArgs: 'String',
      _doc_distributedSystemToken: 'Token that associates distributed group of workers and parameter-server',
      ...(layers.length > 0
        ? {
            _doc_layersSizes: 'List of postive integers [L0, L1, ..., LN]',
            _doc_LayerTypes: MODEL_DOCS.layerTypes,
            _doc_layers_functions_activation: MODEL_DOCS.activation,
            _doc_layer_functions_pooling: MODEL_DOCS.pooling,
            _doc_layer_functions_probabilistic: MODEL_DOCS.probabilistic,
            _doc_layer_functions_scaler: MODEL_DOCS.scaling
          }
        : {})
    };
  }

  const layerSizes = model.layers.map((layer) => layer.size).join(',');
  const layerTypes = model.layers.map((layer) => layer.type).join(',');
  const layerFunctions = model.layers.map((layer) => layer.functionCode).join(',');

  const base: Record<string, unknown> = {
    modelType: model.modelType,
    modelArgs: model.modelArgs,
    layersSizes: layerSizes,
    layerTypesList: layerTypes,
    layers_functions: layerFunctions,
    lossMethod: model.lossMethod,
    lossArgs: model.lossArgs,
    lr: model.learningRate,
    epochs: model.epochs,
    optimizer: model.optimizer,
    optimizerArgs: model.optimizerArgs,
    infraType: model.infraType,
    distributedSystemType: model.distributedSystemType,
    distributedSystemArgs: model.distributedSystemArgs,
    distributedSystemToken: model.distributedSystemToken
  };

  if (!includeDocs) {
    return base;
  }

  return {
    ...base,
    _doc_modelType: MODEL_DOCS.modelType,
    _doc_modelArgs: 'Extra arguments to model',
    _doc_layersSizes: 'List of postive integers [L0, L1, ..., LN]',
    _doc_LayerTypes: MODEL_DOCS.layerTypes,
    _doc_layers_functions_activation: MODEL_DOCS.activation,
    _doc_layer_functions_pooling: MODEL_DOCS.pooling,
    _doc_layer_functions_probabilistic: MODEL_DOCS.probabilistic,
    _doc_layer_functions_scaler: MODEL_DOCS.scaling,
    _doc_lossMethod: MODEL_DOCS.lossMethod,
    _doc_lossArgs: 'reg=L2, reg=L1, reg=NoRegularization (can be also empty)',
    _doc_lr: 'Positve float',
    _doc_epochs: 'Positve Integer',
    _doc_optimizer: MODEL_DOCS.optimizer,
    _doc_optimizerArgs: 'String',
    _doc_infraType: MODEL_DOCS.infraType,
    _doc_distributedSystemType: MODEL_DOCS.distributedSystem,
    _doc_distributedSystemArgs: 'String',
    _doc_distributedSystemToken: 'Token that associates distributed group of workers and parameter-server'
  };
}

function toNumberOrString(value: string): number | string {
  const numeric = Number(value);
  if (!Number.isNaN(numeric) && value.trim() !== '') {
    return numeric;
  }
  return value;
}

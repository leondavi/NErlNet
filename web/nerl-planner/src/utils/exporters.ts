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
  infraType: ' opennn:0 | wolfengine:1 | torch:torch |',
  distributedSystem: docStringFromOptions(distributedSystemOptions),
  activation: docStringFromOptions(activationFunctionOptions),
  pooling: docStringFromOptions(poolingMethodOptions),
  probabilistic: docStringFromOptions(probabilisticFunctionOptions),
  scaling: docStringFromOptions(scalingMethodOptions),
  unscaling: docStringFromOptions(unscalingMethodOptions),
  bounding: docStringFromOptions(boundingMethodOptions)
};

const TorchDoc = {
  infraType: MODEL_DOCS.infraType,
  distributedSystem: MODEL_DOCS.distributedSystem
};

export async function buildDistributedConfig(
  state: PlannerState,
  includeDocs: boolean
): Promise<Record<string, unknown>> {
  const modelPayloads: Record<string, Record<string, unknown>> = {};
  const workerEntries: Record<string, unknown>[] = [];

  // Compute all SHA hashes in parallel for better performance
  const workerShaPromises = state.workers.map(async (worker) => {
    const model = state.models.find((entry) => entry.id === worker.modelId);
    if (!model) {
      return null;
    }
    const basePayload = buildModelPayload(model, false);
    const sha = await sha256String(stableStringify(basePayload));
    const payload = includeDocs ? buildModelPayload(model, true) : basePayload;
    return { worker, model, sha, payload };
  });

  const results = await Promise.all(workerShaPromises);

  for (const result of results) {
    if (!result) {
      continue;
    }
    const { worker, sha, payload } = result;

    if (!modelPayloads[sha]) {
      modelPayloads[sha] = payload;
    }

    const workerEntry: Record<string, unknown> = { name: worker.name, model_sha: sha };
    if (worker.parallel) {
      const parallel: Record<string, number | string> = {};
      if (worker.parallel.pipelineStage.trim()) {
        parallel.pipelineStage = toNumberOrString(worker.parallel.pipelineStage);
      }
      if (worker.parallel.pipelineWorldSize.trim()) {
        parallel.pipelineWorldSize = toNumberOrString(worker.parallel.pipelineWorldSize);
      }
      if (worker.parallel.tpGroup.trim()) {
        parallel.tpGroup = worker.parallel.tpGroup.trim();
      }
      if (worker.parallel.tpRank.trim()) {
        parallel.tpRank = toNumberOrString(worker.parallel.tpRank);
      }
      if (worker.parallel.tpWorldSize.trim()) {
        parallel.tpWorldSize = toNumberOrString(worker.parallel.tpWorldSize);
      }
      if (Object.keys(parallel).length > 0) {
        workerEntry.parallel = parallel;
      }
    }
    workerEntries.push(workerEntry);
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
    superNodes: state.superNodes.map((superNode) => ({
      name: superNode.name,
      port: superNode.port,
      managedClients: superNode.managedClients,
      heartbeatMs: toNumberOrString(superNode.heartbeatMs),
      maxInflightMicrobatches: toNumberOrString(superNode.maxInflightMicrobatches)
    })),
    clients: state.clients.map((client) => {
      const entry: Record<string, unknown> = {
        name: client.name,
        port: client.port,
        workers: client.workers.join(',')
      };
      if (client.superNode?.trim()) {
        entry.superNode = client.superNode.trim();
      }
      return entry;
    }),
    workers: workerEntries,
    model_sha: modelPayloads
  };
}

export function buildConnectionMap(connections: ConnectionEdge[]): Record<string, unknown> {
  const map: Record<string, string[]> = {};
  const addLink = (from: string, to: string) => {
    if (!from || !to || from === to) {
      return;
    }
    if (!map[from]) {
      map[from] = [];
    }
    if (!map[from].includes(to)) {
      map[from].push(to);
    }
  };
  for (const edge of connections) {
    addLink(edge.from, edge.to);
    addLink(edge.to, edge.from);
  }
  return { connectionsMap: map };
}

export function buildExperimentFlow(experimentFlow: ExperimentFlow): Record<string, unknown> {
  return {
    experimentName: experimentFlow.experimentName,
    experimentType: experimentFlow.experimentType,
    batchSize: toNumberOrString(experimentFlow.batchSize),
    csvFilePath: experimentFlow.csvFilePath,
    numOfFeatures: toNumberOrString(experimentFlow.numOfFeatures),
    numOfLabels: toNumberOrString(experimentFlow.numOfLabels),
    headersNames: experimentFlow.headersNames,
    Phases: experimentFlow.phases.map((phase) => {
      const phasePayload: Record<string, unknown> = {
        phaseName: phase.phaseName,
        phaseType: phase.phaseType,
        sourcePieces: phase.sourcePieces.map((piece) => ({
        sourceName: piece.sourceName,
        startingSample: toNumberOrString(piece.startingSample),
        numOfBatches: toNumberOrString(piece.numOfBatches),
        workers: piece.workers.join(','),
        nerltensorType: piece.nerltensorType
        }))
      };
      if (phase.parallelExecution) {
        phasePayload.parallelExecution = {
          mode: phase.parallelExecution.mode,
          superNode: phase.parallelExecution.superNode,
          scheduler: phase.parallelExecution.scheduler,
          microBatchSize: toNumberOrString(phase.parallelExecution.microBatchSize),
          numMicroBatches: toNumberOrString(phase.parallelExecution.numMicroBatches),
          virtualStages: toNumberOrString(phase.parallelExecution.virtualStages)
        };
      }
      return phasePayload;
    })
  };
}

export function buildModelPayload(model: WorkerModel, includeDocs: boolean): Record<string, unknown> {
  if (model.infraType === 'torch') {
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
        loss: model.trainParams.loss,
        batch_size: model.trainParams.batchSize,
        input_tensor_shape: model.trainParams.inputTensorShape,
        labels_offset: model.trainParams.labelsOffset,
        labels_shape: model.trainParams.labelsShape,
        w_init_rand: model.trainParams.wInitRand
      }
    };
    if (model.tpPlan && model.tpPlan.length > 0) {
      base.tpPlan = model.tpPlan.map((entry) => ({
        layer: entry.layer,
        mode: entry.mode,
        shardAxis: toNumberOrString(entry.shardAxis),
        group: entry.group
      }));
    }

    if (!includeDocs) {
      return base;
    }

    return {
      ...base,
      _doc_infraType: TorchDoc.infraType,
      _doc_distributedSystemType: TorchDoc.distributedSystem,
      _doc_distributedSystemArgs: 'String',
      _doc_distributedSystemToken: 'Token that associates distributed group of workers and parameter-server'
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
  if (model.tpPlan && model.tpPlan.length > 0) {
    base.tpPlan = model.tpPlan.map((entry) => ({
      layer: entry.layer,
      mode: entry.mode,
      shardAxis: toNumberOrString(entry.shardAxis),
      group: entry.group
    }));
  }

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

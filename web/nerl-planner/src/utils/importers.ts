import {
  PlannerState,
  WorkerModel,
  Layer,
  ConnectionEdge,
  ExperimentFlow,
  OpenNNModel,
  TorchModel
} from '../data/types';
import { defaultLayerFunctionByType } from '../data/mappings';
import { createTorchModel } from '../data/defaults';

export function importDistributedConfig(
  data: Record<string, unknown>,
  state: PlannerState
): PlannerState {
  const settings = data.nerlnetSettings as Record<string, string> | undefined;
  const mainServer = data.mainServer as Record<string, string> | undefined;
  const apiServer = data.apiServer as Record<string, string> | undefined;

  const devices = Array.isArray(data.devices)
    ? data.devices.map((device: Record<string, string>) => ({
        name: device.name ?? '',
        ipv4: device.ipv4 ?? '',
        entities: device.entities ? device.entities.split(',').map((e) => e.trim()).filter(Boolean) : []
      }))
    : [];

  const routers = Array.isArray(data.routers)
    ? data.routers.map((router: Record<string, string>) => ({
        name: router.name ?? '',
        port: router.port ?? '',
        policy: router.policy ?? '0'
      }))
    : [];

  const sources = Array.isArray(data.sources)
    ? data.sources.map((source: Record<string, string>) => ({
        name: source.name ?? '',
        port: source.port ?? '',
        frequency: source.frequency ?? '',
        policy: source.policy ?? '0',
        epochs: source.epochs ?? '1',
        type: source.type ?? '0'
      }))
    : [];

  const clients = Array.isArray(data.clients)
    ? data.clients.map((client: Record<string, string>) => ({
        name: client.name ?? '',
        port: client.port ?? '',
        workers: client.workers ? client.workers.split(',').map((w) => w.trim()).filter(Boolean) : [],
        superNode: client.superNode ? String(client.superNode) : ''
      }))
    : [];

  const superNodes = Array.isArray(data.superNodes)
    ? data.superNodes.map((superNode: Record<string, unknown>) => ({
        name: String(superNode.name ?? ''),
        port: String(superNode.port ?? ''),
        managedClients: Array.isArray(superNode.managedClients)
          ? (superNode.managedClients as unknown[]).map((entry) => String(entry).trim()).filter(Boolean)
          : String(superNode.managedClients ?? '')
              .split(',')
              .map((entry) => entry.trim())
              .filter(Boolean),
        heartbeatMs: String(superNode.heartbeatMs ?? '1000'),
        maxInflightMicrobatches: String(superNode.maxInflightMicrobatches ?? '1')
      }))
    : [];

  const modelSha = (data.model_sha as Record<string, Record<string, unknown>>) ?? {};
  const models = Object.entries(modelSha).map(([sha, payload]) => parseModelPayload(sha, payload));

  const workers = Array.isArray(data.workers)
    ? data.workers.map((worker: Record<string, unknown>) => ({
        name: String(worker.name ?? ''),
        modelId: String(worker.model_sha ?? ''),
        parallel: parseWorkerParallel(
          (typeof worker.parallel === 'object' ? worker.parallel : undefined) as
            | Record<string, unknown>
            | undefined
        )
      }))
    : [];

  return {
    ...state,
    settings: {
      frequency: settings?.frequency ?? state.settings.frequency,
      batchSize: settings?.batchSize ?? state.settings.batchSize
    },
    servers: {
      mainServer: {
        port: mainServer?.port ?? state.servers.mainServer.port,
        args: mainServer?.args ?? state.servers.mainServer.args
      },
      apiServer: {
        port: apiServer?.port ?? state.servers.apiServer.port,
        args: apiServer?.args ?? state.servers.apiServer.args
      }
    },
    devices,
    routers,
    sources,
    clients,
    superNodes,
    workers,
    models
  };
}

export function importConnectionMap(data: Record<string, unknown>): ConnectionEdge[] {
  const edges: ConnectionEdge[] = [];
  const connectionsMap = data.connectionsMap as Record<string, string[]> | undefined;
  if (connectionsMap) {
    Object.entries(connectionsMap).forEach(([from, toList]) => {
      if (Array.isArray(toList)) {
        toList.forEach((to) => {
          edges.push({
            id: `${from}-${to}`,
            from,
            to,
            type: 'data'
          });
        });
      }
    });
  }

  const connections = data.connections as Record<string, string>[] | undefined;
  if (Array.isArray(connections)) {
    connections.forEach((conn) => {
      if (conn.from && conn.to) {
        edges.push({
          id: `${conn.from}-${conn.to}`,
          from: conn.from,
          to: conn.to,
          type: conn.type ?? 'data'
        });
      }
    });
  }

  return dedupeEdges(edges);
}

export function importExperimentFlow(data: Record<string, unknown>): ExperimentFlow {
  const phasesRaw = (data.Phases ?? data.phases) as Record<string, unknown>[] | undefined;
  const phases = Array.isArray(phasesRaw)
    ? phasesRaw.map((phase) => ({
        id: crypto.randomUUID(),
        phaseName: String(phase.phaseName ?? ''),
        phaseType: String(phase.phaseType ?? 'training'),
        sourcePieces: Array.isArray(phase.sourcePieces)
          ? (phase.sourcePieces as Record<string, unknown>[]).map((piece) => ({
              id: crypto.randomUUID(),
              sourceName: String(piece.sourceName ?? ''),
              startingSample: String(piece.startingSample ?? ''),
              numOfBatches: String(piece.numOfBatches ?? ''),
              workers: String(piece.workers ?? '')
                .split(',')
                .map((worker) => worker.trim())
                .filter(Boolean),
              nerltensorType: normalizeNerltensorType(String(piece.nerltensorType ?? 'float'))
            }))
          : [],
        parallelExecution: parseParallelExecution(phase.parallelExecution as Record<string, unknown> | undefined)
      }))
    : [];

  return {
    experimentName: String(data.experimentName ?? ''),
    experimentType: String(data.experimentType ?? ''),
    batchSize: String(data.batchSize ?? ''),
    csvFilePath: String(data.csvFilePath ?? ''),
    numOfFeatures: String(data.numOfFeatures ?? ''),
    numOfLabels: String(data.numOfLabels ?? ''),
    headersNames: String(data.headersNames ?? ''),
    phases
  };
}

const normalizeNerltensorType = (value: string): string => {
  const normalized = value.trim().toLowerCase();
  if (normalized === 'int') {
    return 'int32';
  }
  if (normalized === 'float32') {
    return 'float';
  }
  if (normalized === 'float64') {
    return 'double';
  }
  return normalized;
};

const parseWorkerParallel = (parallel: Record<string, unknown> | undefined) => {
  if (!parallel || typeof parallel !== 'object') {
    return undefined;
  }
  return {
    pipelineStage: String(parallel.pipelineStage ?? ''),
    pipelineWorldSize: String(parallel.pipelineWorldSize ?? ''),
    tpGroup: String(parallel.tpGroup ?? ''),
    tpRank: String(parallel.tpRank ?? ''),
    tpWorldSize: String(parallel.tpWorldSize ?? '')
  };
};

const parseParallelExecution = (parallel: Record<string, unknown> | undefined) => {
  if (!parallel || typeof parallel !== 'object') {
    return undefined;
  }
  const modeRaw = String(parallel.mode ?? 'legacy').toLowerCase();
  const schedulerRaw = String(parallel.scheduler ?? 'gpipe').toLowerCase();
  const mode = (
    modeRaw === 'pipeline' || modeRaw === 'tensor' || modeRaw === 'pipeline_tensor'
      ? modeRaw
      : 'legacy'
  ) as 'legacy' | 'pipeline' | 'tensor' | 'pipeline_tensor';
  const scheduler = (
    schedulerRaw === '1f1b' || schedulerRaw === 'interleaved'
      ? schedulerRaw
      : 'gpipe'
  ) as 'gpipe' | '1f1b' | 'interleaved';
  return {
    mode,
    superNode: String(parallel.superNode ?? ''),
    scheduler,
    microBatchSize: String(parallel.microBatchSize ?? ''),
    numMicroBatches: String(parallel.numMicroBatches ?? ''),
    virtualStages: String(parallel.virtualStages ?? '')
  };
};

const parseTpPlan = (value: unknown) =>
  Array.isArray(value)
    ? value
        .map((entry) => ({
          modeRaw: String((entry as Record<string, unknown>).mode ?? 'column').toLowerCase(),
          layer: String((entry as Record<string, unknown>).layer ?? ''),
          shardAxis: String((entry as Record<string, unknown>).shardAxis ?? '0'),
          group: String((entry as Record<string, unknown>).group ?? '')
        }))
        .map((entry) => ({
          layer: entry.layer,
          mode: (entry.modeRaw === 'row' ? 'row' : 'column') as 'column' | 'row',
          shardAxis: entry.shardAxis,
          group: entry.group
        }))
        .filter((entry) => entry.layer && entry.group)
    : [];

const parseLayers = (payload: Record<string, unknown>): Layer[] => {
  const layersSizesRaw = String(payload.layersSizes ?? '').trim();
  if (!layersSizesRaw) {
    return [];
  }
  const layersSizes = layersSizesRaw.split(',');
  const layerTypes = String(payload.layerTypesList ?? '').split(',');
  const layerFunctions = String(payload.layers_functions ?? '').split(',');

  return layersSizes
    .map((size, index) => {
      const trimmedSize = size.trim();
      const type = (layerTypes[index] ?? '3').trim();
      const functionCode =
        (layerFunctions[index] ?? defaultLayerFunctionByType[type] ?? '1').trim();
      return {
        id: `layer-${index}`,
        size: trimmedSize,
        type,
        functionCode
      };
    })
    .filter((layer) => layer.size !== '');
};

function parseModelPayload(sha: string, payload: Record<string, unknown>): WorkerModel {
  const layers = parseLayers(payload);
  if (payload.infraType === 'torch' || payload.pt_path) {
    const base = createTorchModel(`Torch Model ${sha.slice(0, 6)}`);
    const trainParams = payload.train_params as Record<string, unknown> | undefined;
    const inputTensorShape = String(trainParams?.input_tensor_shape ?? base.trainParams.inputTensorShape);
    const graphInputShape =
      inputTensorShape && inputTensorShape.startsWith('[')
        ? inputTensorShape.replace(/^\[(\d+)/, '[N')
        : base.graph.inputShape;
    const torchModel: TorchModel = {
      ...base,
      id: sha,
      name: `Torch Model ${sha.slice(0, 6)}`,
      ptPath: String(payload.pt_path ?? ''),
      ptFormat: String(payload.pt_format ?? 'torchscript'),
      ptChecksum: String(payload.pt_checksum ?? 'placeholder'),
      ptDescription: String(payload.pt_description ?? ''),
      graph: {
        ...base.graph,
        inputShape: graphInputShape
      },
      trainParams: {
        ...base.trainParams,
        lr: String(trainParams?.lr ?? base.trainParams.lr),
        epochs: String(trainParams?.epochs ?? base.trainParams.epochs),
        optimizer: String(trainParams?.optimizer ?? base.trainParams.optimizer),
        loss: String(trainParams?.loss ?? base.trainParams.loss),
        batchSize: String(trainParams?.batch_size ?? base.trainParams.batchSize),
        inputTensorShape,
        labelsOffset: String(trainParams?.labels_offset ?? base.trainParams.labelsOffset),
        labelsShape: String(trainParams?.labels_shape ?? base.trainParams.labelsShape),
        wInitRand: String(trainParams?.w_init_rand ?? base.trainParams.wInitRand)
      },
      distributedSystemType: String(payload.distributedSystemType ?? '0'),
      distributedSystemArgs: String(payload.distributedSystemArgs ?? ''),
      distributedSystemToken: String(payload.distributedSystemToken ?? 'none'),
      tpPlan: parseTpPlan(payload.tpPlan)
    };
    return torchModel;
  }

  const model: OpenNNModel = {
    id: sha,
    name: `Worker Model ${sha.slice(0, 6)}`,
    infraType: (String(payload.infraType ?? '0') as '0' | '1') ?? '0',
    modelType: String(payload.modelType ?? '0'),
    modelArgs: String(payload.modelArgs ?? ''),
    layers,
    lossMethod: String(payload.lossMethod ?? '2'),
    lossArgs: String(payload.lossArgs ?? ''),
    learningRate: String(payload.lr ?? '0.001'),
    epochs: String(payload.epochs ?? '1'),
    optimizer: String(payload.optimizer ?? '5'),
    optimizerArgs: String(payload.optimizerArgs ?? ''),
    distributedSystemType: String(payload.distributedSystemType ?? '0'),
    distributedSystemArgs: String(payload.distributedSystemArgs ?? ''),
    distributedSystemToken: String(payload.distributedSystemToken ?? 'none'),
    tpPlan: parseTpPlan(payload.tpPlan)
  };

  return model;
}

function dedupeEdges(edges: ConnectionEdge[]): ConnectionEdge[] {
  const seen = new Set<string>();
  return edges.filter((edge) => {
    const key = [edge.from, edge.to].sort().join('--');
    if (seen.has(key)) {
      return false;
    }
    seen.add(key);
    return true;
  });
}

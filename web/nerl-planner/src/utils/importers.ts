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
        workers: client.workers ? client.workers.split(',').map((w) => w.trim()).filter(Boolean) : []
      }))
    : [];

  const modelSha = (data.model_sha as Record<string, Record<string, unknown>>) ?? {};
  const models = Object.entries(modelSha).map(([sha, payload]) => parseModelPayload(sha, payload));

  const workers = Array.isArray(data.workers)
    ? data.workers.map((worker: Record<string, string>) => ({
        name: worker.name ?? '',
        modelId: worker.model_sha ?? ''
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
        phaseName: String(phase.phaseName ?? ''),
        phaseType: String(phase.phaseType ?? 'training'),
        sourcePieces: Array.isArray(phase.sourcePieces)
          ? (phase.sourcePieces as Record<string, unknown>[]).map((piece) => ({
              sourceName: String(piece.sourceName ?? ''),
              startingSample: String(piece.startingSample ?? ''),
              numOfBatches: String(piece.numOfBatches ?? ''),
              workers: String(piece.workers ?? '')
                .split(',')
                .map((worker) => worker.trim())
                .filter(Boolean),
              nerltensorType: String(piece.nerltensorType ?? 'float')
            }))
          : []
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
    const torchModel: TorchModel = {
      id: sha,
      name: `Torch Model ${sha.slice(0, 6)}`,
      infraType: 'torch',
      ptPath: String(payload.pt_path ?? ''),
      ptFormat: String(payload.pt_format ?? 'torchscript'),
      ptChecksum: String(payload.pt_checksum ?? 'placeholder'),
      ptDescription: String(payload.pt_description ?? ''),
      layers,
      trainParams: {
        lr: String(payload.train_params ? (payload.train_params as Record<string, unknown>).lr ?? '' : ''),
        epochs: String(payload.train_params ? (payload.train_params as Record<string, unknown>).epochs ?? '' : ''),
        optimizer: String(payload.train_params ? (payload.train_params as Record<string, unknown>).optimizer ?? '' : ''),
        batchSize: String(payload.train_params ? (payload.train_params as Record<string, unknown>).batch_size ?? '' : ''),
        inputTensorShape: String(payload.train_params ? (payload.train_params as Record<string, unknown>).input_tensor_shape ?? '' : ''),
        labelsOffset: String(payload.train_params ? (payload.train_params as Record<string, unknown>).labels_offset ?? '' : ''),
        labelsShape: String(payload.train_params ? (payload.train_params as Record<string, unknown>).labels_shape ?? '' : ''),
        wInitRand: String(payload.train_params ? (payload.train_params as Record<string, unknown>).w_init_rand ?? '' : '')
      },
      distributedSystemType: String(payload.distributedSystemType ?? '0'),
      distributedSystemArgs: String(payload.distributedSystemArgs ?? ''),
      distributedSystemToken: String(payload.distributedSystemToken ?? 'none')
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
    distributedSystemToken: String(payload.distributedSystemToken ?? 'none')
  };

  return model;
}

function dedupeEdges(edges: ConnectionEdge[]): ConnectionEdge[] {
  const seen = new Set<string>();
  return edges.filter((edge) => {
    const key = `${edge.from}->${edge.to}`;
    if (seen.has(key)) {
      return false;
    }
    seen.add(key);
    return true;
  });
}

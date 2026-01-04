export type Layer = {
  id: string;
  size: string;
  type: string;
  functionCode: string;
};

export type OpenNNModel = {
  id: string;
  name: string;
  infraType: '0' | '1';
  modelType: string;
  modelArgs: string;
  layers: Layer[];
  lossMethod: string;
  lossArgs: string;
  learningRate: string;
  epochs: string;
  optimizer: string;
  optimizerArgs: string;
  distributedSystemType: string;
  distributedSystemArgs: string;
  distributedSystemToken: string;
};

export type TorchModel = {
  id: string;
  name: string;
  infraType: 'torch';
  ptPath: string;
  ptFormat: string;
  ptChecksum: string;
  ptDescription: string;
  layers: Layer[];
  trainParams: {
    lr: string;
    epochs: string;
    optimizer: string;
    batchSize: string;
    inputTensorShape: string;
    labelsOffset: string;
    labelsShape: string;
    wInitRand: string;
  };
  distributedSystemType: string;
  distributedSystemArgs: string;
  distributedSystemToken: string;
};

export type WorkerModel = OpenNNModel | TorchModel;

export type WorkerEntity = {
  name: string;
  modelId: string;
};

export type Client = {
  name: string;
  port: string;
  workers: string[];
};

export type Source = {
  name: string;
  port: string;
  frequency: string;
  policy: string;
  epochs: string;
  type: string;
};

export type Router = {
  name: string;
  port: string;
  policy: string;
};

export type Device = {
  name: string;
  ipv4: string;
  entities: string[];
};

export type ConnectionEdge = {
  id: string;
  from: string;
  to: string;
  type: string;
};

export type ExperimentSourcePiece = {
  sourceName: string;
  startingSample: string;
  numOfBatches: string;
  workers: string[];
  nerltensorType: string;
};

export type ExperimentPhase = {
  phaseName: string;
  phaseType: string;
  sourcePieces: ExperimentSourcePiece[];
};

export type ExperimentFlow = {
  experimentName: string;
  experimentType: string;
  batchSize: string;
  csvFilePath: string;
  numOfFeatures: string;
  numOfLabels: string;
  headersNames: string;
  phases: ExperimentPhase[];
};

export type PlannerState = {
  settings: {
    frequency: string;
    batchSize: string;
  };
  servers: {
    mainServer: { port: string; args: string };
    apiServer: { port: string; args: string };
  };
  devices: Device[];
  routers: Router[];
  sources: Source[];
  clients: Client[];
  workers: WorkerEntity[];
  models: WorkerModel[];
  connections: ConnectionEdge[];
  experimentFlow: ExperimentFlow;
};

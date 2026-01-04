import { defaultLayerFunctionByType } from './mappings';
import { Layer, OpenNNModel, PlannerState, TorchModel } from './types';

export const createLayer = (index: number): Layer => ({
  id: `layer-${index}-${crypto.randomUUID()}`,
  size: index === 0 ? '5' : '16',
  type: index === 0 ? '1' : '3',
  functionCode: index === 0 ? defaultLayerFunctionByType['1'] : defaultLayerFunctionByType['3']
});

const createDefaultLayers = (): Layer[] => [createLayer(0), createLayer(1), createLayer(2)];

export const createOpenNNModel = (
  name = 'New OpenNN Model',
  layers?: Layer[]
): OpenNNModel => {
  const resolvedLayers = layers && layers.length > 0 ? layers : createDefaultLayers();
  return {
    id: crypto.randomUUID(),
    name,
    infraType: '0',
    modelType: '2',
    modelArgs: '',
    layers: resolvedLayers,
    lossMethod: '2',
    lossArgs: '',
    learningRate: '0.001',
    epochs: '1',
    optimizer: '5',
    optimizerArgs: '',
    distributedSystemType: '0',
    distributedSystemArgs: '',
    distributedSystemToken: 'none'
  };
};

export const createTorchModel = (name = 'New Torch Model', layers: Layer[] = []): TorchModel => ({
  id: crypto.randomUUID(),
  name,
  infraType: 'torch',
  ptPath: 'tests/inputTorchJsonsFiles/models/placeholder_perceptron.pt',
  ptFormat: 'torchscript',
  ptChecksum: 'placeholder',
  ptDescription: 'TorchScript placeholder',
  layers,
  trainParams: {
    lr: '0.001',
    epochs: '1',
    optimizer: 'adam',
    batchSize: '50',
    inputTensorShape: '[50, 5]',
    labelsOffset: 'default',
    labelsShape: '[50, 3]',
    wInitRand: 'True'
  },
  distributedSystemType: '0',
  distributedSystemArgs: '',
  distributedSystemToken: 'none'
});

export const createDefaultState = (): PlannerState => ({
  settings: {
    frequency: '60',
    batchSize: '50'
  },
  servers: {
    mainServer: { port: '8081', args: '' },
    apiServer: { port: '8082', args: '' }
  },
  devices: [],
  routers: [],
  sources: [],
  clients: [],
  workers: [],
  models: [],
  connections: [],
  experimentFlow: {
    experimentName: 'synthetic_experiment',
    experimentType: 'classification',
    batchSize: '50',
    csvFilePath: '/tmp/nerlnet/data/example.csv',
    numOfFeatures: '5',
    numOfLabels: '3',
    headersNames: 'feature1,feature2,feature3',
    phases: []
  }
});

import { TorchLayerParams, TorchLayerType } from './types';

export type TorchLayerParamSpec = {
  key: keyof TorchLayerParams;
  label: string;
  type: 'number' | 'select';
  options?: { label: string; value: number }[];
  step?: number;
  min?: number;
};

export type TorchLayerDefinition = {
  type: TorchLayerType;
  label: string;
  description: string;
  category: string;
  params: TorchLayerParamSpec[];
};

export const torchLayerCatalog: TorchLayerDefinition[] = [
  {
    type: 'conv2d',
    label: 'Conv2d',
    description: '2D convolution with learnable kernels.',
    category: 'Convolution',
    params: [
      { key: 'outChannels', label: 'Out Channels', type: 'number', min: 1, step: 1 },
      { key: 'kernel', label: 'Kernel (K)', type: 'number', min: 1, step: 1 },
      { key: 'stride', label: 'Stride (S)', type: 'number', min: 1, step: 1 },
      { key: 'padding', label: 'Padding (P)', type: 'number', min: 0, step: 1 }
    ]
  },
  {
    type: 'maxpool2d',
    label: 'MaxPool2d',
    description: 'Downsample spatial features with max pooling.',
    category: 'Convolution',
    params: [
      { key: 'kernel', label: 'Kernel (K)', type: 'number', min: 1, step: 1 },
      { key: 'stride', label: 'Stride (S)', type: 'number', min: 1, step: 1 },
      { key: 'padding', label: 'Padding (P)', type: 'number', min: 0, step: 1 }
    ]
  },
  {
    type: 'conv1d',
    label: 'Conv1d',
    description: '1D convolution for temporal signals.',
    category: 'Convolution',
    params: [
      { key: 'outChannels', label: 'Out Channels', type: 'number', min: 1, step: 1 },
      { key: 'kernel', label: 'Kernel (K)', type: 'number', min: 1, step: 1 },
      { key: 'stride', label: 'Stride (S)', type: 'number', min: 1, step: 1 },
      { key: 'padding', label: 'Padding (P)', type: 'number', min: 0, step: 1 }
    ]
  },
  {
    type: 'maxpool1d',
    label: 'MaxPool1d',
    description: 'Downsample sequences with max pooling.',
    category: 'Convolution',
    params: [
      { key: 'kernel', label: 'Kernel (K)', type: 'number', min: 1, step: 1 },
      { key: 'stride', label: 'Stride (S)', type: 'number', min: 1, step: 1 },
      { key: 'padding', label: 'Padding (P)', type: 'number', min: 0, step: 1 }
    ]
  },
  {
    type: 'flatten',
    label: 'Flatten',
    description: 'Flatten feature maps into a single dimension.',
    category: 'Shape',
    params: []
  },
  {
    type: 'relu',
    label: 'ReLU',
    description: 'Rectified linear activation.',
    category: 'Activation',
    params: []
  },
  {
    type: 'sigmoid',
    label: 'Sigmoid',
    description: 'Sigmoid activation for probabilities.',
    category: 'Activation',
    params: []
  },
  {
    type: 'softmax',
    label: 'Softmax',
    description: 'Softmax along a chosen dimension.',
    category: 'Activation',
    params: [
      {
        key: 'dim',
        label: 'Dim',
        type: 'select',
        options: [
          { label: 'Last (-1)', value: -1 },
          { label: 'Channel (1)', value: 1 }
        ]
      }
    ]
  },
  {
    type: 'batchnorm2d',
    label: 'BatchNorm2d',
    description: 'Batch normalization for channels.',
    category: 'Normalization',
    params: []
  },
  {
    type: 'layernorm',
    label: 'LayerNorm',
    description: 'Normalize across the last dimension.',
    category: 'Normalization',
    params: [{ key: 'normalizedShape', label: 'Norm Size', type: 'number', min: 1, step: 1 }]
  },
  {
    type: 'dropout',
    label: 'Dropout',
    description: 'Randomly drop activations.',
    category: 'Regularization',
    params: [{ key: 'dropout', label: 'Dropout P', type: 'number', min: 0, step: 0.05 }]
  },
  {
    type: 'linear',
    label: 'Linear',
    description: 'Fully connected projection.',
    category: 'Dense',
    params: [{ key: 'outFeatures', label: 'Out Features', type: 'number', min: 1, step: 1 }]
  },
  {
    type: 'transformer',
    label: 'Transformer Layer',
    description: 'Encoder-style transformer block.',
    category: 'Sequence',
    params: [
      { key: 'dModel', label: 'd_model', type: 'number', min: 1, step: 1 },
      { key: 'nHead', label: 'n_head', type: 'number', min: 1, step: 1 },
      { key: 'dimFeedforward', label: 'Feedforward', type: 'number', min: 1, step: 1 },
      { key: 'dropout', label: 'Dropout P', type: 'number', min: 0, step: 0.05 }
    ]
  },
  {
    type: 'residual',
    label: 'Residual Add',
    description: 'Add a skip branch to the main path.',
    category: 'Merge',
    params: []
  }
];

export const torchLayerDefaults: Record<TorchLayerType, TorchLayerParams> = {
  conv2d: { outChannels: 16, kernel: 3, stride: 1, padding: 1 },
  maxpool2d: { kernel: 2, stride: 2, padding: 0 },
  conv1d: { outChannels: 16, kernel: 3, stride: 1, padding: 1 },
  maxpool1d: { kernel: 2, stride: 2, padding: 0 },
  relu: {},
  sigmoid: {},
  softmax: { dim: -1 },
  batchnorm2d: {},
  layernorm: { normalizedShape: 128 },
  dropout: { dropout: 0.25 },
  linear: { outFeatures: 10 },
  flatten: {},
  transformer: { dModel: 128, nHead: 4, dimFeedforward: 256, dropout: 0.1 },
  residual: {}
};

export const torchOptimizerOptions = [
  { label: 'Adam', value: 'adam' },
  { label: 'SGD', value: 'sgd' }
];

export const torchLossOptions = [
  { label: 'Mean Squared Error', value: 'mse' }
];

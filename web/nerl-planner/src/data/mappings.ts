export type Option = { label: string; value: string; docLabel?: string };

export const modelTypeOptions: Option[] = [
  { label: 'Neural Network', value: '0', docLabel: 'nn' },
  { label: 'Approximation', value: '1', docLabel: 'approximation' },
  { label: 'Classification', value: '2', docLabel: 'classification' },
  { label: 'Forecasting', value: '3', docLabel: 'forecasting' },
  { label: 'Image Classification', value: '4', docLabel: 'image_classification' },
  { label: 'Text Classification', value: '5', docLabel: 'text_classification' },
  { label: 'Text Generation', value: '6', docLabel: 'text_generation' },
  { label: 'Auto Association', value: '7', docLabel: 'auto_association' },
  { label: 'Autoencoder', value: '8', docLabel: 'autoencoder' },
  { label: 'AE Classifier', value: '9', docLabel: 'ae_classifier' }
];

export const layerTypeOptions: Option[] = [
  { label: 'Default', value: '0' },
  { label: 'Scaling', value: '1' },
  { label: 'Conv', value: '2' },
  { label: 'Perceptron', value: '3' },
  { label: 'Pooling', value: '4' },
  { label: 'Probabilistic', value: '5' },
  { label: 'LSTM', value: '6' },
  { label: 'Recurrent', value: '7', docLabel: 'Reccurrent' },
  { label: 'Unscaling', value: '8' },
  { label: 'Flatten', value: '9' },
  { label: 'Bounding', value: '10' }
];

export const activationFunctionOptions: Option[] = [
  { label: 'Threshold', value: '1' },
  { label: 'Sign', value: '2' },
  { label: 'Logistic', value: '3' },
  { label: 'Tanh', value: '4' },
  { label: 'Linear', value: '5' },
  { label: 'ReLU', value: '6' },
  { label: 'eLU', value: '7' },
  { label: 'SeLU', value: '8' },
  { label: 'Soft-plus', value: '9' },
  { label: 'Soft-sign', value: '10' },
  { label: 'Hard-sigmoid', value: '11' }
];

export const scalingMethodOptions: Option[] = [
  { label: 'None', value: '1' },
  { label: 'MinMax', value: '2' },
  { label: 'MeanStd', value: '3' },
  { label: 'STD', value: '4' },
  { label: 'Log', value: '5' }
];

export const unscalingMethodOptions: Option[] = [
  { label: 'None', value: '1' },
  { label: 'MinMax', value: '2' },
  { label: 'MeanStd', value: '3' },
  { label: 'STD', value: '4' },
  { label: 'Log', value: '5' }
];

export const poolingMethodOptions: Option[] = [
  { label: 'None', value: '1' },
  { label: 'Max', value: '2' },
  { label: 'Avg', value: '3' }
];

export const probabilisticFunctionOptions: Option[] = [
  { label: 'Binary', value: '1' },
  { label: 'Logistic', value: '2' },
  { label: 'Competitive', value: '3' },
  { label: 'Softmax', value: '4' }
];

export const flattenMethodOptions: Option[] = [
  { label: 'Flatten', value: '0' }
];

export const boundingMethodOptions: Option[] = [
  { label: 'None', value: '1' },
  { label: 'Bounding', value: '2' }
];

export const optimizerOptions: Option[] = [
  { label: 'Gradient Descent', value: '0', docLabel: 'GD' },
  { label: 'Conjugate Gradient', value: '1', docLabel: 'CGD' },
  { label: 'Stochastic Gradient', value: '2', docLabel: 'SGD' },
  { label: 'Quasi-Newton', value: '3', docLabel: 'QuasiNeuton' },
  { label: 'Levenberg-Marquardt', value: '4', docLabel: 'LVM' },
  { label: 'Adam', value: '5', docLabel: 'ADAM' }
];

export const lossMethodOptions: Option[] = [
  { label: 'SSE', value: '1', docLabel: 'SSE' },
  { label: 'MSE', value: '2', docLabel: 'MSE' },
  { label: 'NSE', value: '3', docLabel: 'NSE' },
  { label: 'Minkowski', value: '4', docLabel: 'MinkowskiE' },
  { label: 'WSE', value: '5', docLabel: 'WSE' },
  { label: 'CEE', value: '6', docLabel: 'CEE' }
];

export const distributedSystemOptions: Option[] = [
  { label: 'None', value: '0', docLabel: 'none' },
  { label: 'Fed Client Avg', value: '1', docLabel: 'FedClientAvg' },
  { label: 'Fed Server Avg', value: '2', docLabel: 'FedServerAvg' },
  { label: 'Fed Client Weighted Avg Classification', value: '3', docLabel: 'FedClientWeightedAvgClassification' },
  { label: 'Fed Server Weighted Avg Classification', value: '4', docLabel: 'FedServerWeightedAvgClassification' },
  { label: 'Fed Client AE', value: '5', docLabel: 'FedClientAE' },
  { label: 'Fed Server AE', value: '6', docLabel: 'FedServerAE' },
  { label: 'Tiles', value: '7', docLabel: 'tiles' }
];

export const infraTypeOptions: Option[] = [
  { label: 'OpenNN', value: '0', docLabel: 'opennn' },
  { label: 'Wolfram Engine', value: '1', docLabel: 'wolfengine' },
  { label: 'Torch', value: 'torch', docLabel: 'torch' }
];

export const sourcePolicyOptions: Option[] = [
  { label: 'Casting', value: '0' },
  { label: 'Round Robin', value: '1' },
  { label: 'Random', value: '2' }
];

export const routerPolicyOptions: Option[] = [
  { label: 'Routing Table', value: '0' }
];

export const sourceTypeOptions: Option[] = [
  { label: 'CSV', value: '0' },
  { label: 'Camera Dummy', value: '1' }
];

export const phaseTypeOptions: Option[] = [
  { label: 'Training', value: 'training' },
  { label: 'Prediction', value: 'prediction' }
];

export const nerltensorTypeOptions: Option[] = [
  { label: 'Float', value: 'float' },
  { label: 'Double', value: 'double' },
  { label: 'Int', value: 'int' }
];

export const functionOptionsByLayerType: Record<string, Option[]> = {
  '0': activationFunctionOptions,
  '1': scalingMethodOptions,
  '2': activationFunctionOptions,
  '3': activationFunctionOptions,
  '4': poolingMethodOptions,
  '5': probabilisticFunctionOptions,
  '6': activationFunctionOptions,
  '7': activationFunctionOptions,
  '8': unscalingMethodOptions,
  '9': flattenMethodOptions,
  '10': boundingMethodOptions
};

export const defaultLayerFunctionByType: Record<string, string> = {
  '0': activationFunctionOptions[0].value,
  '1': scalingMethodOptions[0].value,
  '2': activationFunctionOptions[5].value,
  '3': activationFunctionOptions[5].value,
  '4': poolingMethodOptions[1].value,
  '5': probabilisticFunctionOptions[3].value,
  '6': activationFunctionOptions[5].value,
  '7': activationFunctionOptions[5].value,
  '8': unscalingMethodOptions[0].value,
  '9': flattenMethodOptions[0].value,
  '10': boundingMethodOptions[0].value
};

export const layerTypeLabelByValue: Record<string, string> = Object.fromEntries(
  layerTypeOptions.map((option) => [option.value, option.label])
);

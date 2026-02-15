import { TorchGraph, TorchLayerNode, TorchLayerParams } from '../data/types';

export type TorchDim = number | 'N';
export type TorchShape = TorchDim[];

export type TorchGraphWarning = {
  nodeId: string;
  message: string;
  severity: 'warning' | 'error';
};

export const parseShape = (value: string): TorchShape | null => {
  const trimmed = value.trim();
  if (!trimmed) {
    return null;
  }
  const normalized = trimmed.replace(/^\[|\]$/g, '');
  const parts = normalized
    .split(/[,\s]+/)
    .map((part) => part.trim())
    .filter(Boolean);
  if (parts.length === 0) {
    return null;
  }
  const dims: TorchShape = [];
  for (const part of parts) {
    if (part.toUpperCase() === 'N') {
      dims.push('N');
      continue;
    }
    const numeric = Number(part);
    if (Number.isNaN(numeric) || numeric <= 0) {
      return null;
    }
    dims.push(numeric);
  }
  return dims;
};

export const formatShape = (shape: TorchShape | null): string => {
  if (!shape) {
    return '--';
  }
  return `[${shape.map((dim) => (typeof dim === 'number' ? dim : 'N')).join(', ')}]`;
};

const toPair = (value?: number | [number, number]): [number, number] | null => {
  if (value === undefined) {
    return null;
  }
  if (Array.isArray(value)) {
    return [value[0], value[1]];
  }
  return [value, value];
};

const toNumber = (value?: number): number | null => {
  if (value === undefined || Number.isNaN(value)) {
    return null;
  }
  return value;
};

const shapeEquals = (left: TorchShape, right: TorchShape) => {
  if (left.length !== right.length) {
    return false;
  }
  return left.every((dim, index) => dim === right[index]);
};

const convOut = (size: number, kernel: number, stride: number, padding: number) =>
  Math.floor((size + 2 * padding - kernel) / stride + 1);

const inferConv2d = (input: TorchShape, params: TorchLayerParams): TorchShape | null => {
  if (input.length !== 4) {
    return null;
  }
  const [batch, channels, height, width] = input;
  if (typeof channels !== 'number' || typeof height !== 'number' || typeof width !== 'number') {
    return null;
  }
  const outChannels = toNumber(params.outChannels) ?? 1;
  const kernel = toPair(params.kernel) ?? [3, 3];
  const stride = toPair(params.stride) ?? [1, 1];
  const padding = toPair(params.padding) ?? [0, 0];
  const outH = convOut(height, kernel[0], stride[0], padding[0]);
  const outW = convOut(width, kernel[1], stride[1], padding[1]);
  if (outH <= 0 || outW <= 0) {
    return null;
  }
  return [batch, outChannels, outH, outW];
};

const inferConv1d = (input: TorchShape, params: TorchLayerParams): TorchShape | null => {
  if (input.length !== 3) {
    return null;
  }
  const [batch, channels, length] = input;
  if (typeof channels !== 'number' || typeof length !== 'number') {
    return null;
  }
  const outChannels = toNumber(params.outChannels) ?? 1;
  const kernel = toNumber(params.kernel as number) ?? 3;
  const stride = toNumber(params.stride as number) ?? 1;
  const padding = toNumber(params.padding as number) ?? 0;
  const outL = convOut(length, kernel, stride, padding);
  if (outL <= 0) {
    return null;
  }
  return [batch, outChannels, outL];
};

const inferPool2d = (input: TorchShape, params: TorchLayerParams): TorchShape | null => {
  if (input.length !== 4) {
    return null;
  }
  const [batch, channels, height, width] = input;
  if (typeof channels !== 'number' || typeof height !== 'number' || typeof width !== 'number') {
    return null;
  }
  const kernel = toPair(params.kernel) ?? [2, 2];
  const stride = toPair(params.stride) ?? kernel;
  const padding = toPair(params.padding) ?? [0, 0];
  const outH = convOut(height, kernel[0], stride[0], padding[0]);
  const outW = convOut(width, kernel[1], stride[1], padding[1]);
  if (outH <= 0 || outW <= 0) {
    return null;
  }
  return [batch, channels, outH, outW];
};

const inferPool1d = (input: TorchShape, params: TorchLayerParams): TorchShape | null => {
  if (input.length !== 3) {
    return null;
  }
  const [batch, channels, length] = input;
  if (typeof channels !== 'number' || typeof length !== 'number') {
    return null;
  }
  const kernel = toNumber(params.kernel as number) ?? 2;
  const stride = toNumber(params.stride as number) ?? kernel;
  const padding = toNumber(params.padding as number) ?? 0;
  const outL = convOut(length, kernel, stride, padding);
  if (outL <= 0) {
    return null;
  }
  return [batch, channels, outL];
};

const inferLinear = (input: TorchShape, params: TorchLayerParams): TorchShape | null => {
  if (input.length < 2) {
    return null;
  }
  const batch = input[0];
  const outFeatures = toNumber(params.outFeatures) ?? 1;
  return [batch, outFeatures];
};

const inferFlatten = (input: TorchShape): TorchShape | null => {
  if (input.length < 2) {
    return null;
  }
  const batch = input[0];
  let features = 1;
  for (const dim of input.slice(1)) {
    if (typeof dim !== 'number') {
      return null;
    }
    features *= dim;
  }
  return [batch, features];
};

const inferTransformer = (input: TorchShape, params: TorchLayerParams): TorchShape | null => {
  if (input.length !== 3) {
    return null;
  }
  const dModel = toNumber(params.dModel) ?? 1;
  const [batch, seq, features] = input;
  if (typeof features === 'number' && features !== dModel) {
    return null;
  }
  return [batch, seq, dModel];
};

const inferLayer = (node: TorchLayerNode, input: TorchShape): TorchShape | null => {
  switch (node.type) {
    case 'conv2d':
      return inferConv2d(input, node.params);
    case 'conv1d':
      return inferConv1d(input, node.params);
    case 'maxpool2d':
      return inferPool2d(input, node.params);
    case 'maxpool1d':
      return inferPool1d(input, node.params);
    case 'linear':
      return inferLinear(input, node.params);
    case 'flatten':
      return inferFlatten(input);
    case 'transformer':
      return inferTransformer(input, node.params);
    case 'batchnorm2d':
    case 'relu':
    case 'dropout':
    case 'softmax':
    case 'sigmoid':
    case 'layernorm':
      return input;
    default:
      return input;
  }
};

const topologicalSort = (graph: TorchGraph) => {
  const nodes = graph.nodes.map((node) => node.id);
  const incoming = new Map<string, number>(nodes.map((id) => [id, 0]));
  graph.edges.forEach((edge) => {
    incoming.set(edge.to, (incoming.get(edge.to) ?? 0) + 1);
  });
  const queue = nodes.filter((id) => (incoming.get(id) ?? 0) === 0);
  const order: string[] = [];
  while (queue.length > 0) {
    const current = queue.shift();
    if (!current) {
      continue;
    }
    order.push(current);
    graph.edges
      .filter((edge) => edge.from === current)
      .forEach((edge) => {
        const next = (incoming.get(edge.to) ?? 0) - 1;
        incoming.set(edge.to, next);
        if (next === 0) {
          queue.push(edge.to);
        }
      });
  }
  const isComplete = order.length === nodes.length;
  return { order: isComplete ? order : nodes, isComplete };
};

export const inferTorchGraph = (graph: TorchGraph) => {
  const warnings: TorchGraphWarning[] = [];
  const shapeMap = new Map<string, TorchShape | null>();
  const nodeById = new Map(graph.nodes.map((node) => [node.id, node]));
  const inputShape = parseShape(graph.inputShape);
  if (!inputShape) {
    return { shapes: shapeMap, warnings: [{ nodeId: 'input', message: 'Invalid input shape.', severity: 'error' }] };
  }

  const incoming = new Map<string, string[]>();
  graph.edges.forEach((edge) => {
    if (!incoming.has(edge.to)) {
      incoming.set(edge.to, []);
    }
    incoming.get(edge.to)?.push(edge.from);
  });

  const { order, isComplete } = topologicalSort(graph);
  if (!isComplete && graph.nodes.length > 0) {
    warnings.push({
      nodeId: 'graph',
      message: 'Graph has cycles or unresolved dependencies. Remove loops before export.',
      severity: 'error'
    });
  }
  order.forEach((nodeId) => {
    const node = nodeById.get(nodeId);
    if (!node) {
      return;
    }
    const sources = incoming.get(nodeId) ?? [];
    const sourceShapes = sources.map((source) => shapeMap.get(source)).filter(Boolean) as TorchShape[];
    if (node.type === 'residual') {
      if (sourceShapes.length < 2) {
        warnings.push({
          nodeId,
          message: 'Residual merge expects two inputs.',
          severity: 'warning'
        });
        shapeMap.set(nodeId, sourceShapes[0] ?? inputShape);
        return;
      }
      const [left, right] = sourceShapes;
      if (!shapeEquals(left, right)) {
        warnings.push({
          nodeId,
          message: `Residual inputs mismatch ${formatShape(left)} vs ${formatShape(right)}.`,
          severity: 'warning'
        });
        shapeMap.set(nodeId, null);
        return;
      }
      shapeMap.set(nodeId, left);
      return;
    }

    const baseShape = sourceShapes[0] ?? inputShape;
    if (sourceShapes.length > 1) {
      warnings.push({
        nodeId,
        message: 'Layer has multiple inputs. Using the first input.',
        severity: 'warning'
      });
    }
    const output = inferLayer(node, baseShape);
    if (!output) {
      warnings.push({
        nodeId,
        message: `Shape error on ${node.type}. Check parameters.`,
        severity: 'warning'
      });
    }
    shapeMap.set(nodeId, output);
  });

  return { shapes: shapeMap, warnings };
};

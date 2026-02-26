import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import {
  addEdge,
  applyEdgeChanges,
  Background,
  BackgroundVariant,
  Connection,
  Controls,
  Edge,
  Node,
  Position,
  ReactFlow,
  ReactFlowInstance,
  useEdgesState,
  useNodesState
} from '@xyflow/react';
import {
  activationFunctionOptions,
  distributedSystemOptions,
  functionOptionsByLayerType,
  infraTypeOptions,
  layerTypeOptions,
  layerTypeLabelByValue,
  modelTypeOptions,
  lossMethodOptions,
  optimizerOptions
} from '../data/mappings';
import { createOpenNNModel, createTorchModel } from '../data/defaults';
import { torchLayerCatalog, torchLayerDefaults, torchLossOptions, torchOptimizerOptions } from '../data/torchCatalog';
import {
  Layer,
  OpenNNModel,
  PlannerState,
  TorchLayerNode,
  TorchModel,
  TpPlanEntry,
  WorkerModel
} from '../data/types';
import { defaultLayerFunctionByType } from '../data/mappings';
import WorkerPreview from './WorkerPreview';
import { formatShape, inferTorchGraph, parseShape, TorchShape } from '../utils/torchGraph';
import ModelGraphNode, { ModelGraphNodeData } from './ModelGraphNode';
import ValidationPanel from './ValidationPanel';
import { validatePlannerState } from '../utils/validation';

const getNodeSize = (node: Node): { width: number; height: number } => {
  const width =
    typeof node.style?.width === 'number'
      ? node.style.width
      : typeof node.width === 'number'
        ? node.width
        : 180;
  const height =
    typeof node.style?.height === 'number'
      ? node.style.height
      : typeof node.height === 'number'
        ? node.height
        : 90;
  return { width, height };
};

const getTorchNodeDimensions = (shape: TorchShape | null): { width: number; height: number } => {
  const widthBase =
    shape && typeof shape[1] === 'number'
      ? Math.min(260, Math.max(140, 100 + shape[1] * 2))
      : 180;
  const heightBase =
    shape && typeof shape[2] === 'number'
      ? Math.min(160, Math.max(60, 40 + shape[2]))
      : 90;
  return { width: widthBase, height: heightBase };
};

const HANDLE_SIZE = 8;

const getNodeHandles = (
  layout: 'horizontal' | 'vertical' | 'free',
  width: number,
  height: number
) => {
  const isVertical = layout === 'vertical';
  if (isVertical) {
    const centerX = Math.max(0, width / 2 - HANDLE_SIZE / 2);
    return [
      {
        id: 'target',
        type: 'target' as const,
        position: Position.Top,
        x: centerX,
        y: 0,
        width: HANDLE_SIZE,
        height: HANDLE_SIZE
      },
      {
        id: 'source',
        type: 'source' as const,
        position: Position.Bottom,
        x: centerX,
        y: Math.max(0, height - HANDLE_SIZE),
        width: HANDLE_SIZE,
        height: HANDLE_SIZE
      }
    ];
  }
  const centerY = Math.max(0, height / 2 - HANDLE_SIZE / 2);
  return [
    {
      id: 'target',
      type: 'target' as const,
      position: Position.Left,
      x: 0,
      y: centerY,
      width: HANDLE_SIZE,
      height: HANDLE_SIZE
    },
    {
      id: 'source',
      type: 'source' as const,
      position: Position.Right,
      x: Math.max(0, width - HANDLE_SIZE),
      y: centerY,
      width: HANDLE_SIZE,
      height: HANDLE_SIZE
    }
  ];
};

const getBounds = (nodes: Node[]) => {
  if (nodes.length === 0) {
    return { minX: 0, minY: 0, maxX: 0, maxY: 0, width: 0, height: 0 };
  }
  let minX = Number.POSITIVE_INFINITY;
  let minY = Number.POSITIVE_INFINITY;
  let maxX = Number.NEGATIVE_INFINITY;
  let maxY = Number.NEGATIVE_INFINITY;
  nodes.forEach((node) => {
    const { width, height } = getNodeSize(node);
    const x = node.position.x ?? 0;
    const y = node.position.y ?? 0;
    minX = Math.min(minX, x);
    minY = Math.min(minY, y);
    maxX = Math.max(maxX, x + width);
    maxY = Math.max(maxY, y + height);
  });
  return { minX, minY, maxX, maxY, width: maxX - minX, height: maxY - minY };
};

const AUTO_LAYOUT_MIN_READABLE_ZOOM = 0.62;
const AUTO_LAYOUT_MARGIN = 84;
const AUTO_LAYOUT_GAP = 90;

type LayoutSpec = {
  id: string;
  width: number;
  height: number;
};

type LayoutPlan = {
  positions: Record<string, { x: number; y: number }>;
  bounds: ReturnType<typeof getBounds>;
  estimatedZoom: number;
};

const estimateViewportZoom = (
  bounds: ReturnType<typeof getBounds>,
  frame: { width: number; height: number }
) => {
  if (bounds.width <= 0 || bounds.height <= 0 || frame.width <= 0 || frame.height <= 0) {
    return 1;
  }
  const freeWidth = Math.max(1, frame.width - AUTO_LAYOUT_MARGIN * 2);
  const freeHeight = Math.max(1, frame.height - AUTO_LAYOUT_MARGIN * 2);
  return Math.min(freeWidth / bounds.width, freeHeight / bounds.height, 1);
};

const buildLayoutForMaxTrack = (
  layout: 'horizontal' | 'vertical',
  specs: LayoutSpec[],
  maxTrackCount: number
): LayoutPlan => {
  const positions: Record<string, { x: number; y: number }> = {};
  const safeTrackCount = Math.max(1, maxTrackCount);
  const startX = AUTO_LAYOUT_MARGIN;
  const startY = AUTO_LAYOUT_MARGIN;

  if (layout === 'horizontal') {
    let x = startX;
    let y = startY;
    let countInTrack = 0;
    let trackMaxHeight = 0;
    for (const spec of specs) {
      if (countInTrack >= safeTrackCount) {
        x = startX;
        y += trackMaxHeight + AUTO_LAYOUT_GAP;
        countInTrack = 0;
        trackMaxHeight = 0;
      }
      positions[spec.id] = { x, y };
      x += spec.width + AUTO_LAYOUT_GAP;
      trackMaxHeight = Math.max(trackMaxHeight, spec.height);
      countInTrack += 1;
    }
  } else {
    let x = startX;
    let y = startY;
    let countInTrack = 0;
    let trackMaxWidth = 0;
    for (const spec of specs) {
      if (countInTrack >= safeTrackCount) {
        y = startY;
        x += trackMaxWidth + AUTO_LAYOUT_GAP;
        countInTrack = 0;
        trackMaxWidth = 0;
      }
      positions[spec.id] = { x, y };
      y += spec.height + AUTO_LAYOUT_GAP;
      trackMaxWidth = Math.max(trackMaxWidth, spec.width);
      countInTrack += 1;
    }
  }

  const nodesForBounds: Node[] = specs.map((spec) => ({
    id: spec.id,
    position: positions[spec.id] ?? { x: 0, y: 0 },
    data: {},
    style: { width: spec.width, height: spec.height }
  }));
  return {
    positions,
    bounds: getBounds(nodesForBounds),
    estimatedZoom: 1
  };
};

const chooseWrappedAutoLayout = (
  layout: 'horizontal' | 'vertical',
  specs: LayoutSpec[],
  frame: { width: number; height: number }
) => {
  if (specs.length === 0) {
    return { positions: {} };
  }

  let bestPlan = buildLayoutForMaxTrack(layout, specs, specs.length);
  bestPlan = {
    ...bestPlan,
    estimatedZoom: estimateViewportZoom(bestPlan.bounds, frame)
  };

  for (let maxTrack = specs.length - 1; maxTrack >= 1; maxTrack -= 1) {
    const candidateBase = buildLayoutForMaxTrack(layout, specs, maxTrack);
    const candidate = {
      ...candidateBase,
      estimatedZoom: estimateViewportZoom(candidateBase.bounds, frame)
    };
    if (candidate.estimatedZoom >= AUTO_LAYOUT_MIN_READABLE_ZOOM) {
      return { positions: candidate.positions };
    }
    if (candidate.estimatedZoom > bestPlan.estimatedZoom) {
      bestPlan = candidate;
    }
  }

  return { positions: bestPlan.positions };
};

const selectPreferredTorchOutputNode = (graph: TorchModel['graph']): TorchLayerNode | null => {
  if (!graph.nodes || graph.nodes.length === 0) {
    return null;
  }

  const nodeIds = graph.nodes.map((node) => node.id);
  const nodeIndex = new Map(nodeIds.map((id, index) => [id, index]));
  const outDegree = new Map(nodeIds.map((id) => [id, 0]));
  const inDegree = new Map(nodeIds.map((id) => [id, 0]));
  const adjacency = new Map(nodeIds.map((id) => [id, [] as string[]]));

  graph.edges.forEach((edge) => {
    if (!nodeIndex.has(edge.from) || !nodeIndex.has(edge.to)) {
      return;
    }
    outDegree.set(edge.from, (outDegree.get(edge.from) ?? 0) + 1);
    inDegree.set(edge.to, (inDegree.get(edge.to) ?? 0) + 1);
    adjacency.get(edge.from)?.push(edge.to);
  });

  const sinkIds = nodeIds.filter((id) => (outDegree.get(id) ?? 0) === 0);
  if (sinkIds.length === 0) {
    return graph.nodes[graph.nodes.length - 1] ?? null;
  }
  if (sinkIds.length === 1) {
    return graph.nodes[nodeIndex.get(sinkIds[0]) ?? 0] ?? null;
  }

  const queue = nodeIds.filter((id) => (inDegree.get(id) ?? 0) === 0);
  const depth = new Map(nodeIds.map((id) => [id, 0]));
  let processed = 0;

  while (queue.length > 0) {
    const current = queue.shift();
    if (!current) {
      continue;
    }
    processed += 1;
    const currentDepth = depth.get(current) ?? 0;
    const neighbors = adjacency.get(current) ?? [];
    neighbors.forEach((next) => {
      const nextDepth = Math.max(depth.get(next) ?? 0, currentDepth + 1);
      depth.set(next, nextDepth);
      const nextIn = (inDegree.get(next) ?? 0) - 1;
      inDegree.set(next, nextIn);
      if (nextIn === 0) {
        queue.push(next);
      }
    });
  }

  if (processed !== nodeIds.length) {
    const fallbackSink = sinkIds[sinkIds.length - 1];
    return graph.nodes[nodeIndex.get(fallbackSink) ?? graph.nodes.length - 1] ?? null;
  }

  let bestSink = sinkIds[0];
  for (const candidate of sinkIds.slice(1)) {
    const bestDepth = depth.get(bestSink) ?? 0;
    const candidateDepth = depth.get(candidate) ?? 0;
    const bestIdx = nodeIndex.get(bestSink) ?? -1;
    const candidateIdx = nodeIndex.get(candidate) ?? -1;
    if (candidateDepth > bestDepth || (candidateDepth === bestDepth && candidateIdx > bestIdx)) {
      bestSink = candidate;
    }
  }

  return graph.nodes[nodeIndex.get(bestSink) ?? graph.nodes.length - 1] ?? null;
};

const layerDescriptions: Record<string, string> = {
  '0': 'Baseline passthrough for generic layers.',
  '1': 'Scaling layer for normalization strategies.',
  '2': 'Convolutional layer with kernel patterns.',
  '3': 'Dense perceptron with activation.',
  '4': 'Pooling layer for downsampling.',
  '5': 'Probabilistic outputs for classification.',
  '6': 'LSTM sequence memory.',
  '7': 'Recurrent layer for temporal flow.',
  '8': 'Unscaling layer to restore ranges.',
  '9': 'Flatten for tensor shaping.',
  '10': 'Bounding layer for clipping.'
};

type FlowMouseEvent = MouseEvent | React.MouseEvent<Element, MouseEvent>;

const ModelLabView = ({
  state,
  onChange
}: {
  state: PlannerState;
  onChange: (next: PlannerState) => void;
}) => {
  const [selectedModelId, setSelectedModelId] = useState<string | null>(
    () => state.models[0]?.id ?? null
  );
  const normalizeModel = (model?: WorkerModel | null): WorkerModel => {
    if (!model || typeof model !== 'object') {
      return createTorchModel();
    }
    const infraType = (model as { infraType?: string }).infraType;
    if (infraType === 'torch' || infraType === '2') {
      const fallback = createTorchModel(model.name);
      const candidateGraph = (model as TorchModel).graph;
      const resolvedGraph =
        candidateGraph &&
        Array.isArray(candidateGraph.nodes) &&
        Array.isArray(candidateGraph.edges)
          ? candidateGraph
          : fallback.graph;
      return {
        ...fallback,
        ...model,
        infraType: 'torch',
        graph: resolvedGraph,
        trainParams: {
          ...fallback.trainParams,
          ...((model as TorchModel).trainParams ?? {})
        }
      };
    }
    if (infraType === '0' || infraType === '1') {
      const fallback = createOpenNNModel(model.name);
      const layers = Array.isArray((model as OpenNNModel).layers)
        ? (model as OpenNNModel).layers
        : fallback.layers;
      return {
        ...fallback,
        ...model,
        infraType,
        layers
      };
    }
    const fallback = createTorchModel((model as { name?: string }).name ?? 'New Torch Model');
    return fallback;
  };

  const [modelDraft, setModelDraft] = useState<WorkerModel>(() => normalizeModel(state.models[0]));
  const [selectedLayerIndex, setSelectedLayerIndex] = useState(0);
  const [selectedTorchNodeId, setSelectedTorchNodeId] = useState<string | null>(null);
  const [linkingFrom, setLinkingFrom] = useState<string | null>(null);
  const [graphNodes, setGraphNodes, onGraphNodesChange] = useNodesState<Node<ModelGraphNodeData>>(
    []
  );
  const [graphEdges, setGraphEdges] = useEdgesState<Edge>([]);
  const [graphFlowInstance, setGraphFlowInstance] = useState<
    ReactFlowInstance<Node<ModelGraphNodeData>, Edge> | null
  >(null);
  const [graphMenu, setGraphMenu] = useState<{
    x: number;
    y: number;
    flowX: number;
    flowY: number;
  } | null>(null);
  const [graphPalette, setGraphPalette] = useState<{
    x: number;
    y: number;
    flowX: number;
    flowY: number;
  } | null>(null);
  const [nodeMenu, setNodeMenu] = useState<{
    x: number;
    y: number;
    nodeId: string;
  } | null>(null);
  const [edgeMenu, setEdgeMenu] = useState<{
    x: number;
    y: number;
    flowX: number;
    flowY: number;
    source: string;
    target: string;
  } | null>(null);
  const [layerInspectorOpen, setLayerInspectorOpen] = useState(false);
  const [stackExpanded, setStackExpanded] = useState(false);
  const manualPanStateRef = useRef<{
    pointerId: number;
    startX: number;
    startY: number;
    lastX: number;
    lastY: number;
    moved: boolean;
  } | null>(null);
  const pendingCenterNodeIdRef = useRef<string | null>(null);
  const previousLayoutRef = useRef<'free' | 'horizontal' | 'vertical'>('horizontal');
  const suppressNextPaneClickRef = useRef(false);
  const [openLayerPositions, setOpenLayerPositions] = useState<
    Record<string, { x: number; y: number }>
  >(() => state.ui?.openLayerPositions ?? {});
  const graphFrameRef = useRef<HTMLDivElement | null>(null);
  const isTorch = modelDraft.infraType === 'torch';
  const fallbackTorchGraph = useMemo(() => createTorchModel().graph, []);
  const torchGraph = isTorch ? modelDraft.graph ?? fallbackTorchGraph : null;
  const torchDraft = modelDraft.infraType === 'torch' ? modelDraft : null;
  const [graphLayout, setGraphLayout] = useState<'free' | 'horizontal' | 'vertical'>('horizontal');
  const [graphFrameSize, setGraphFrameSize] = useState({ width: 0, height: 0 });
  const torchTrainParams = isTorch ? modelDraft.trainParams : null;
  const openLayers = useMemo<Layer[]>(
    () => (modelDraft.infraType === 'torch' ? [] : modelDraft.layers),
    [modelDraft]
  );
  const openLayerCount = openLayers.length;
  const tpPlanEntries = modelDraft.tpPlan ?? [];
  const availableTpLayers = useMemo(
    () =>
      modelDraft.infraType === 'torch'
        ? (torchGraph?.nodes.map((node) => node.name || node.id) ?? [])
        : openLayers.map((layer) => layer.id),
    [modelDraft.infraType, openLayers, torchGraph]
  );
  const updateTpPlan = (updater: (entries: TpPlanEntry[]) => TpPlanEntry[]) => {
    setModelDraft((current) => ({
      ...current,
      tpPlan: updater(current.tpPlan ?? [])
    }));
  };
  const addTpPlanEntry = () => {
    const fallbackLayer = availableTpLayers[0] ?? '';
    updateTpPlan((entries) => [
      ...entries,
      {
        layer: fallbackLayer,
        mode: 'column',
        shardAxis: '0',
        group: ''
      }
    ]);
  };
  const updateTpPlanEntry = (index: number, patch: Partial<TpPlanEntry>) => {
    updateTpPlan((entries) =>
      entries.map((entry, entryIndex) =>
        entryIndex === index ? { ...entry, ...patch } : entry
      )
    );
  };
  const removeTpPlanEntry = (index: number) => {
    updateTpPlan((entries) => entries.filter((_, entryIndex) => entryIndex !== index));
  };
  const graphNodeTypes = useMemo(() => ({ model: ModelGraphNode }), []);
  const torchOptimizerChoices = useMemo(() => {
    if (!isTorch) {
      return torchOptimizerOptions;
    }
    const current = torchTrainParams?.optimizer?.trim();
    if (current && !torchOptimizerOptions.some((option) => option.value === current)) {
      return [{ label: `Unsupported (${current})`, value: current }, ...torchOptimizerOptions];
    }
    return torchOptimizerOptions;
  }, [isTorch, torchTrainParams?.optimizer]);
  const torchLossChoices = useMemo(() => {
    if (!isTorch) {
      return torchLossOptions;
    }
    const current = torchTrainParams?.loss?.trim();
    if (current && !torchLossOptions.some((option) => option.value === current)) {
      return [{ label: `Unsupported (${current})`, value: current }, ...torchLossOptions];
    }
    return torchLossOptions;
  }, [isTorch, torchTrainParams?.loss]);
  const graphLayerOptions = useMemo(() => {
    if (isTorch) {
      return torchLayerCatalog.map((entry) => ({
        key: entry.type,
        label: entry.label,
        description: entry.description
      }));
    }
    return layerTypeOptions.map((option) => ({
      key: option.value,
      label: option.label,
      description: layerDescriptions[option.value] ?? 'Layer component'
    }));
  }, [isTorch]);
  const [torchExportStatus, setTorchExportStatus] = useState<'idle' | 'exporting' | 'done' | 'error'>(
    'idle'
  );
  const [torchExportError, setTorchExportError] = useState<string | null>(null);
  const [importableTorchModels, setImportableTorchModels] = useState<
    { name: string; label: string; configPath: string; ptPath: string }[]
  >([]);

  // Only sync modelDraft from state.models when selectedModelId changes (not on every state.models reference change)
  const prevSelectedModelIdRef = useRef<string | null>(null);
  useEffect(() => {
    // Skip if selectedModelId hasn't actually changed
    if (prevSelectedModelIdRef.current === selectedModelId) {
      return;
    }
    prevSelectedModelIdRef.current = selectedModelId;
    
    if (selectedModelId) {
      const selected = state.models.find((model) => model.id === selectedModelId);
      if (selected) {
        const normalized = normalizeModel(selected);
        setModelDraft(normalized);
        setSelectedTorchNodeId(
          normalized.infraType === 'torch' ? normalized.graph.nodes[0]?.id ?? null : null
        );
        setSelectedLayerIndex(0);
      }
    }
  }, [selectedModelId, state.models]);

  useEffect(() => {
    setTorchExportStatus('idle');
    setTorchExportError(null);
  }, [selectedModelId]);

  useEffect(() => {
    setGraphMenu(null);
    setGraphPalette(null);
    setNodeMenu(null);
    setEdgeMenu(null);
    setLinkingFrom(null);
    setLayerInspectorOpen(false);
  }, [modelDraft.infraType]);

  useEffect(() => {
    const frame = graphFrameRef.current;
    if (!frame) {
      return;
    }
    const updateSize = () => {
      setGraphFrameSize({ width: frame.clientWidth, height: frame.clientHeight });
    };
    updateSize();
    const observer = new ResizeObserver(updateSize);
    observer.observe(frame);
    return () => observer.disconnect();
  }, []);

  useEffect(() => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    if (openLayerCount === 0) {
      setSelectedLayerIndex(0);
    } else if (selectedLayerIndex >= openLayerCount) {
      setSelectedLayerIndex(openLayerCount - 1);
    }
  }, [modelDraft.infraType, openLayerCount, selectedLayerIndex]);

  // Persist OpenNN layer positions to parent state
  const syncOpenLayerPositionsRef = useRef(openLayerPositions);
  syncOpenLayerPositionsRef.current = openLayerPositions;

  useEffect(() => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    const positions = syncOpenLayerPositionsRef.current;
    if (Object.keys(positions).length === 0) {
      return;
    }
    const currentPositions = state.ui?.openLayerPositions ?? {};
    const hasChanges = Object.keys(positions).some(
      (id) =>
        !currentPositions[id] ||
        currentPositions[id].x !== positions[id].x ||
        currentPositions[id].y !== positions[id].y
    );
    if (hasChanges) {
      onChange({
        ...state,
        ui: {
          ...state.ui,
          nodePositions: state.ui?.nodePositions ?? {},
          openLayerPositions: positions
        }
      });
    }
  }, [modelDraft.infraType, onChange, openLayerPositions, state]);

  const updateOpenNN = (patch: Partial<OpenNNModel>) => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    setModelDraft({ ...modelDraft, ...patch });
  };

  const updateTorch = (patch: Partial<TorchModel>) => {
    if (modelDraft.infraType !== 'torch') {
      return;
    }
    setModelDraft({ ...modelDraft, ...patch });
  };

  const updateLayer = (index: number, patch: Partial<Layer>) => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    const nextLayers = [...openLayers];
    if (!nextLayers[index]) {
      return;
    }
    nextLayers[index] = { ...nextLayers[index], ...patch };
    setModelDraft({ ...modelDraft, layers: nextLayers });
  };

  const addLayerByType = (
    type: string,
    insertIndex?: number,
    position?: { x: number; y: number }
  ) => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    const size = openLayers.length === 0 ? '5' : '16';
    const functionCode = defaultLayerFunctionByType[type] ?? activationFunctionOptions[0].value;
    const nextLayer: Layer = {
      id: `layer-${crypto.randomUUID()}`,
      size,
      type,
      functionCode
    };
    const nextLayers = [...openLayers];
    const targetIndex =
      insertIndex === undefined || insertIndex < 0 || insertIndex > nextLayers.length
        ? nextLayers.length
        : insertIndex;
    const defaultLayoutPosition =
      graphLayout === 'free'
        ? undefined
        : graphLayout === 'horizontal'
          ? { x: 160 + targetIndex * (180 + 90), y: 140 - 90 / 2 }
          : { x: 160 - 180 / 2, y: 140 + targetIndex * (90 + 90) };
    nextLayers.splice(targetIndex, 0, nextLayer);
    pendingCenterNodeIdRef.current = nextLayer.id;
    setModelDraft({ ...modelDraft, layers: nextLayers });
    setSelectedLayerIndex(targetIndex);
    const resolvedPosition =
      graphLayout === 'free' ? position ?? defaultLayoutPosition : defaultLayoutPosition;
    if (resolvedPosition) {
      setOpenLayerPositions((prev) => ({ ...prev, [nextLayer.id]: resolvedPosition }));
    }
  };

  const removeLayer = (index: number) => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    const nextLayers = openLayers.filter((_, idx) => idx !== index);
    setModelDraft({ ...modelDraft, layers: nextLayers });
    setSelectedLayerIndex(Math.max(0, index - 1));
    const removed = openLayers[index];
    if (removed) {
      setOpenLayerPositions((prev) => {
        const next = { ...prev };
        delete next[removed.id];
        return next;
      });
    }
  };

  const moveLayer = (index: number, direction: number) => {
    if (modelDraft.infraType === 'torch') {
      return;
    }
    const target = index + direction;
    if (target < 0 || target >= openLayers.length) {
      return;
    }
    const nextLayers = [...openLayers];
    const [item] = nextLayers.splice(index, 1);
    nextLayers.splice(target, 0, item);
    setModelDraft({ ...modelDraft, layers: nextLayers });
    setSelectedLayerIndex(target);
  };

  const openLayerIndexById = useMemo(() => {
    if (modelDraft.infraType === 'torch') {
      return new Map<string, number>();
    }
    return new Map(openLayers.map((layer, index) => [layer.id, index]));
  }, [modelDraft.infraType, openLayers]);

  const updateTorchGraph = useCallback(
    (updater: (graph: TorchModel['graph']) => TorchModel['graph']) => {
      setModelDraft((current) => {
        if (current.infraType !== 'torch') {
          return current;
        }
        const baseGraph = current.graph ?? fallbackTorchGraph;
        const nextGraph = updater(baseGraph);
        if (!nextGraph || !Array.isArray(nextGraph.nodes) || !Array.isArray(nextGraph.edges)) {
          return current;
        }
        const validNodeIds = new Set(nextGraph.nodes.map((node) => node.id));
        const filteredEdges = nextGraph.edges.filter(
          (edge) => validNodeIds.has(edge.from) && validNodeIds.has(edge.to)
        );
        if (filteredEdges.length !== nextGraph.edges.length) {
          return { ...current, graph: { ...nextGraph, edges: filteredEdges } };
        }
        return { ...current, graph: nextGraph };
      });
    },
    [fallbackTorchGraph]
  );

  const addTorchLayer = useCallback(
    (
      type: TorchLayerNode['type'],
      position?: { x: number; y: number },
      connectFromId?: string
    ) => {
      const nodeId = `torch-${crypto.randomUUID()}`;
      updateTorchGraph((graph) => {
        const definition = torchLayerCatalog.find((entry) => entry.type === type);
        const { width, height } = getTorchNodeDimensions(null);
        const defaultLayoutPosition =
          graphLayout === 'free'
            ? undefined
            : graphLayout === 'horizontal'
              ? { x: 160 + graph.nodes.length * (width + 90), y: 140 - height / 2 }
              : { x: 160 - width / 2, y: 140 + graph.nodes.length * (height + 90) };
        const node: TorchLayerNode = {
          id: nodeId,
          name: definition?.label ?? type,
          type,
          params: { ...torchLayerDefaults[type] },
          position:
            graphLayout === 'free'
              ? position ?? defaultLayoutPosition ?? { x: 120 + graph.nodes.length * 220, y: 140 }
              : defaultLayoutPosition ?? { x: 120 + graph.nodes.length * 220, y: 140 }
        };
        const lastNode = connectFromId
          ? graph.nodes.find((entry) => entry.id === connectFromId)
          : selectedTorchNodeId
            ? graph.nodes.find((entry) => entry.id === selectedTorchNodeId)
            : graph.nodes[graph.nodes.length - 1];
        const nextEdges = lastNode
          ? [
              ...graph.edges,
              {
                id: `${lastNode.id}-${node.id}`,
                from: lastNode.id,
                to: node.id
              }
            ]
          : graph.edges;
        return {
          ...graph,
          nodes: [...graph.nodes, node],
          edges: nextEdges
        };
      });
      pendingCenterNodeIdRef.current = nodeId;
      setSelectedTorchNodeId(nodeId);
      setGraphMenu(null);
    },
    [graphLayout, selectedTorchNodeId, updateTorchGraph]
  );

  const insertTorchLayerBetween = useCallback(
    (
      sourceId: string,
      targetId: string,
      type: TorchLayerNode['type'],
      position?: { x: number; y: number }
    ) => {
      const nodeId = `torch-${crypto.randomUUID()}`;
      updateTorchGraph((graph) => {
        const definition = torchLayerCatalog.find((entry) => entry.type === type);
        const { width, height } = getTorchNodeDimensions(null);
        const defaultLayoutPosition =
          graphLayout === 'free'
            ? undefined
            : graphLayout === 'horizontal'
              ? { x: 160 + graph.nodes.length * (width + 90), y: 140 - height / 2 }
              : { x: 160 - width / 2, y: 140 + graph.nodes.length * (height + 90) };
        const node: TorchLayerNode = {
          id: nodeId,
          name: definition?.label ?? type,
          type,
          params: { ...torchLayerDefaults[type] },
          position:
            graphLayout === 'free'
              ? position ?? defaultLayoutPosition ?? { x: 120 + graph.nodes.length * 220, y: 140 }
              : defaultLayoutPosition ?? { x: 120 + graph.nodes.length * 220, y: 140 }
        };
        const prunedEdges = graph.edges.filter(
          (edge) => !(edge.from === sourceId && edge.to === targetId)
        );
        const nextEdges = [
          ...prunedEdges,
          { id: `${sourceId}-${nodeId}`, from: sourceId, to: nodeId },
          { id: `${nodeId}-${targetId}`, from: nodeId, to: targetId }
        ];
        return {
          ...graph,
          nodes: [...graph.nodes, node],
          edges: nextEdges
        };
      });
      pendingCenterNodeIdRef.current = nodeId;
      setSelectedTorchNodeId(nodeId);
    },
    [graphLayout, updateTorchGraph]
  );

  const updateTorchNode = (id: string, patch: Partial<TorchLayerNode>) => {
    updateTorchGraph((graph) => ({
      ...graph,
      nodes: graph.nodes.map((node) =>
        node.id === id ? { ...node, ...patch, params: { ...node.params, ...patch.params } } : node
      )
    }));
  };

  const removeTorchNode = (id: string) => {
    updateTorchGraph((graph) => {
      const incoming = graph.edges.filter((edge) => edge.to === id).map((edge) => edge.from);
      const outgoing = graph.edges.filter((edge) => edge.from === id).map((edge) => edge.to);
      const remainingEdges = graph.edges.filter((edge) => edge.from !== id && edge.to !== id);
      const rewired = [...remainingEdges];
      incoming.forEach((from) => {
        outgoing.forEach((to) => {
          if (from === to) {
            return;
          }
          if (!rewired.some((edge) => edge.from === from && edge.to === to)) {
            rewired.push({ id: `${from}-${to}`, from, to });
          }
        });
      });
      return {
        ...graph,
        nodes: graph.nodes.filter((node) => node.id !== id),
        edges: rewired
      };
    });
    if (selectedTorchNodeId === id) {
      setSelectedTorchNodeId(null);
    }
    if (linkingFrom === id) {
      setLinkingFrom(null);
    }
  };

  const syncTorchEdges = useCallback(
    (edgeList: Edge[]) => {
      updateTorchGraph((graph) => ({
        ...graph,
        edges: edgeList.map((edge) => ({
          id: edge.id,
          from: String(edge.source),
          to: String(edge.target)
        }))
      }));
    },
    [updateTorchGraph]
  );

  const removeOpenLayerById = useCallback(
    (id: string) => {
      const index = openLayerIndexById.get(id);
      if (index === undefined) {
        return;
      }
      removeLayer(index);
    },
    [openLayerIndexById, removeLayer]
  );

  const insertOpenLayerBetween = useCallback(
    (sourceId: string, targetId: string, type: string, position?: { x: number; y: number }) => {
      const sourceIndex = openLayerIndexById.get(sourceId) ?? -1;
      const targetIndex = openLayerIndexById.get(targetId) ?? -1;
      let insertIndex = targetIndex >= 0 ? targetIndex : sourceIndex + 1;
      if (insertIndex < 0) {
        insertIndex = openLayerCount;
      }
      addLayerByType(type, insertIndex, position);
    },
    [addLayerByType, openLayerCount, openLayerIndexById]
  );

  const graphNodeMap = useMemo(
    () => new Map(graphNodes.map((node) => [node.id, node])),
    [graphNodes]
  );
  const graphBounds = useMemo(() => getBounds(graphNodes), [graphNodes]);
  const graphTranslateExtent = useMemo<
    [[number, number], [number, number]] | undefined
  >(() => {
    if (graphLayout === 'free') {
      return undefined;
    }
    if (graphNodes.length === 0) {
      return [
        [-1200, -1200],
        [1200, 1200]
      ];
    }
    const pad = 520;
    return [
      [graphBounds.minX - pad, graphBounds.minY - pad],
      [graphBounds.maxX + pad, graphBounds.maxY + pad]
    ];
  }, [graphBounds.maxX, graphBounds.maxY, graphBounds.minX, graphBounds.minY, graphLayout, graphNodes.length]);
  const edgePresentationForNodes = useCallback(
    (sourceId: string, targetId: string, nodeMap: Map<string, Node>, bounds: ReturnType<typeof getBounds>) => {
      const source = nodeMap.get(sourceId);
      const target = nodeMap.get(targetId);
      const base = {
        style: { stroke: 'var(--ink-soft)', strokeWidth: 2 },
        interactionWidth: 32
      };
      if (!source || !target) {
        return { ...base, type: 'straight' as const };
      }
      if (graphLayout === 'vertical' || graphLayout === 'horizontal') {
        return { ...base, type: 'straight' as const };
      }
      const dx = Math.abs((target.position?.x ?? 0) - (source.position?.x ?? 0));
      const dy = Math.abs((target.position?.y ?? 0) - (source.position?.y ?? 0));
      const widthNearLimit =
        graphFrameSize.width > 0 && bounds.width >= graphFrameSize.width * 0.9;
      const heightNearLimit =
        graphFrameSize.height > 0 && bounds.height >= graphFrameSize.height * 0.9;
      const useStep = (widthNearLimit && dx >= dy) || (heightNearLimit && dy > dx);
      return { ...base, type: useStep ? ('step' as const) : ('straight' as const) };
    },
    [graphFrameSize, graphLayout]
  );

  const handleGraphEdgesChange = useCallback(
    (changes: Parameters<typeof applyEdgeChanges>[0]) => {
      setGraphEdges((current) => {
        if (
          isTorch &&
          current.length > 0 &&
          changes.length === current.length &&
          changes.every((change) => change.type === 'remove')
        ) {
          return current;
        }
        const nextEdges = applyEdgeChanges(changes, current);
        if (
          isTorch &&
          changes.some(
            (change) => change.type === 'remove' || change.type === 'add' || change.type === 'replace'
          )
        ) {
          syncTorchEdges(nextEdges);
        }
        return nextEdges;
      });
    },
    [isTorch, setGraphEdges, syncTorchEdges]
  );

  const handleGraphConnect = useCallback(
    (connection: Connection) => {
      if (!isTorch) {
        return;
      }
      setGraphEdges((current) => {
        const sourceId = String(connection.source ?? '');
        const targetId = String(connection.target ?? '');
        const edgePresentation = edgePresentationForNodes(
          sourceId,
          targetId,
          graphNodeMap,
          graphBounds
        );
        const nextEdges = addEdge(
          { ...connection, ...edgePresentation, animated: true },
          current
        );
        syncTorchEdges(nextEdges);
        return nextEdges;
      });
    },
    [edgePresentationForNodes, graphBounds, graphNodeMap, isTorch, setGraphEdges, syncTorchEdges]
  );

  const addTorchLink = useCallback(
    (sourceId: string, targetId: string) => {
      if (!isTorch || !sourceId || !targetId || sourceId === targetId) {
        return false;
      }
      if (
        graphEdges.some((edge) => String(edge.source) === sourceId && String(edge.target) === targetId)
      ) {
        return false;
      }
      const edgePresentation = edgePresentationForNodes(sourceId, targetId, graphNodeMap, graphBounds);
      const nextEdges = [
        ...graphEdges,
        {
          id: `${sourceId}-${targetId}`,
          source: sourceId,
          target: targetId,
          ...edgePresentation,
          animated: true
        }
      ];
      setGraphEdges(nextEdges);
      syncTorchEdges(nextEdges);
      return true;
    },
    [
      edgePresentationForNodes,
      graphBounds,
      graphEdges,
      graphNodeMap,
      isTorch,
      setGraphEdges,
      syncTorchEdges
    ]
  );

  const handleGraphNodeClick = useCallback(
    (_: unknown, node: Node) => {
      if (suppressNextPaneClickRef.current) {
        suppressNextPaneClickRef.current = false;
        return;
      }
      setNodeMenu(null);
      setEdgeMenu(null);
      setGraphMenu(null);
      if (isTorch) {
        setSelectedTorchNodeId(node.id);
        if (linkingFrom && node.id !== linkingFrom) {
          addTorchLink(linkingFrom, node.id);
          setLinkingFrom(null);
        }
        return;
      }
      const index = openLayerIndexById.get(node.id);
      if (index !== undefined) {
        setSelectedLayerIndex(index);
      }
    },
    [addTorchLink, isTorch, linkingFrom, openLayerIndexById]
  );

  const handleGraphNodeContextMenu = useCallback(
    (event: React.MouseEvent, node: Node) => {
      event.preventDefault();
      setGraphMenu(null);
      setGraphPalette(null);
      setEdgeMenu(null);
      setNodeMenu({
        x: event.clientX,
        y: event.clientY,
        nodeId: node.id
      });
    },
    []
  );

  const handleGraphEdgeContextMenu = useCallback(
    (event: React.MouseEvent, edge: Edge) => {
      event.preventDefault();
      const flowPos = graphFlowInstance?.screenToFlowPosition({
        x: event.clientX,
        y: event.clientY
      });
      setGraphMenu(null);
      setGraphPalette(null);
      setNodeMenu(null);
      setEdgeMenu({
        x: event.clientX,
        y: event.clientY,
        flowX: flowPos?.x ?? event.clientX,
        flowY: flowPos?.y ?? event.clientY,
        source: String(edge.source),
        target: String(edge.target)
      });
    },
    [graphFlowInstance]
  );

  const handleGraphEdgeDoubleClick = useCallback(
    (event: React.MouseEvent, edge: Edge) => {
      handleGraphEdgeContextMenu(event, edge);
    },
    [handleGraphEdgeContextMenu]
  );

  const handleGraphNodesChange = useCallback(
    (changes: Parameters<typeof onGraphNodesChange>[0]) => {
      onGraphNodesChange(changes);
      const updates: Record<string, { x: number; y: number }> = {};
      changes.forEach((change) => {
        if (change.type === 'position') {
          const nextPosition = change.position ?? change.positionAbsolute;
          if (nextPosition) {
            updates[change.id] = nextPosition;
          }
        }
      });
      if (Object.keys(updates).length === 0) {
        return;
      }
      if (isTorch) {
        updateTorchGraph((graph) => ({
          ...graph,
          nodes: graph.nodes.map((node) =>
            updates[node.id] ? { ...node, position: updates[node.id] } : node
          )
        }));
        return;
      }
      setOpenLayerPositions((prev) => ({ ...prev, ...updates }));
    },
    [isTorch, onGraphNodesChange, updateTorchGraph]
  );

  const openGraphMenu = useCallback(
    (event: FlowMouseEvent) => {
      const flowPos = graphFlowInstance?.screenToFlowPosition({
        x: event.clientX,
        y: event.clientY
      });
      setGraphMenu({
        x: event.clientX,
        y: event.clientY,
        flowX: flowPos?.x ?? 0,
        flowY: flowPos?.y ?? 0
      });
      setGraphPalette(null);
    },
    [graphFlowInstance]
  );

  const openGraphPalette = useCallback(
    (menu: { x: number; y: number; flowX: number; flowY: number }) => {
      const frame = graphFrameRef.current?.getBoundingClientRect();
      const clamp = (value: number, min: number, max: number) =>
        Math.min(Math.max(value, min), max);
      if (!frame) {
        setGraphPalette({ x: menu.x, y: menu.y, flowX: menu.flowX, flowY: menu.flowY });
        return;
      }
      const pickerWidth = 260;
      const pickerHeight = 320;
      const left = clamp(menu.x - frame.left, 12, Math.max(12, frame.width - pickerWidth - 12));
      const top = clamp(menu.y - frame.top, 12, Math.max(12, frame.height - pickerHeight - 12));
      setGraphPalette({ x: left, y: top, flowX: menu.flowX, flowY: menu.flowY });
    },
    []
  );

  const handleGraphPaneClick = useCallback(
    (event: FlowMouseEvent) => {
      if (suppressNextPaneClickRef.current) {
        suppressNextPaneClickRef.current = false;
        return;
      }
      if (event.detail > 1) {
        event.preventDefault();
        if (linkingFrom) {
          setLinkingFrom(null);
        }
        setNodeMenu(null);
        setEdgeMenu(null);
        openGraphMenu(event);
        return;
      }
      if (linkingFrom) {
        setLinkingFrom(null);
      }
      setGraphMenu(null);
      setGraphPalette(null);
      setNodeMenu(null);
      setEdgeMenu(null);
    },
    [linkingFrom, openGraphMenu]
  );

  const shouldBlockManualPan = useCallback((target: EventTarget | null) => {
    const element = target instanceof Element ? target : null;
    if (!element) {
      return false;
    }
    return Boolean(
      element.closest(
        'button, input, select, textarea, a, label, .graph-layer-picker, .context-menu, .floating-panel, .react-flow__controls, .react-flow__handle, .react-flow__edge'
      )
    );
  }, []);

  const handleGraphFramePointerDownCapture = useCallback(
    (event: React.PointerEvent<HTMLDivElement>) => {
      if (event.button !== 0 || !graphFlowInstance || shouldBlockManualPan(event.target)) {
        return;
      }
      manualPanStateRef.current = {
        pointerId: event.pointerId,
        startX: event.clientX,
        startY: event.clientY,
        lastX: event.clientX,
        lastY: event.clientY,
        moved: false
      };
      event.currentTarget.setPointerCapture(event.pointerId);
    },
    [graphFlowInstance, shouldBlockManualPan]
  );

  const handleGraphFramePointerMoveCapture = useCallback(
    (event: React.PointerEvent<HTMLDivElement>) => {
      const state = manualPanStateRef.current;
      if (!state || state.pointerId !== event.pointerId || !graphFlowInstance) {
        return;
      }
      const deltaX = event.clientX - state.lastX;
      const deltaY = event.clientY - state.lastY;
      const totalMove = Math.hypot(event.clientX - state.startX, event.clientY - state.startY);
      if (!state.moved && totalMove > 2) {
        state.moved = true;
      }
      state.lastX = event.clientX;
      state.lastY = event.clientY;
      if (!state.moved || (deltaX === 0 && deltaY === 0)) {
        return;
      }
      const viewport = graphFlowInstance.getViewport();
      void graphFlowInstance.setViewport({
        x: viewport.x + deltaX,
        y: viewport.y + deltaY,
        zoom: viewport.zoom
      });
      suppressNextPaneClickRef.current = true;
      event.preventDefault();
      event.stopPropagation();
    },
    [graphFlowInstance]
  );

  const handleGraphFramePointerUpCapture = useCallback(
    (event: React.PointerEvent<HTMLDivElement>) => {
      const state = manualPanStateRef.current;
      if (!state || state.pointerId !== event.pointerId) {
        return;
      }
      if (state.moved) {
        suppressNextPaneClickRef.current = true;
      }
      manualPanStateRef.current = null;
      if (event.currentTarget.hasPointerCapture(event.pointerId)) {
        event.currentTarget.releasePointerCapture(event.pointerId);
      }
    },
    []
  );

  const handleGraphPaneContextMenu = useCallback(
    (event: FlowMouseEvent) => {
      event.preventDefault();
      if (linkingFrom) {
        setLinkingFrom(null);
      }
      setNodeMenu(null);
      setEdgeMenu(null);
      openGraphMenu(event);
    },
    [linkingFrom, openGraphMenu]
  );

  const torchInference = useMemo(() => {
    if (!isTorch || !torchGraph) {
      return null;
    }
    return inferTorchGraph(torchGraph);
  }, [isTorch, torchGraph]);

  const openLayerInspector = useCallback(
    (nodeId: string) => {
      if (isTorch) {
        setSelectedTorchNodeId(nodeId);
      } else {
        const index = openLayerIndexById.get(nodeId);
        if (index !== undefined) {
          setSelectedLayerIndex(index);
        }
      }
      setLayerInspectorOpen(true);
    },
    [isTorch, openLayerIndexById]
  );

  const loadImportableTorchModels = useCallback(async () => {
    try {
      const response = await fetch('/api/torch/models');
      if (!response.ok) {
        return;
      }
      const data = (await response.json()) as {
        models?: { name: string; label: string; configPath: string; ptPath: string }[];
      };
      setImportableTorchModels(data.models ?? []);
    } catch {
      setImportableTorchModels([]);
    }
  }, []);

  useEffect(() => {
    if (isTorch) {
      loadImportableTorchModels();
    }
  }, [isTorch, loadImportableTorchModels]);

  const handleTorchExport = useCallback(async () => {
    if (!isTorch || !torchGraph) {
      return;
    }
    if (torchGraph.nodes.length === 0) {
      setTorchExportStatus('error');
      setTorchExportError('Add at least one layer before exporting.');
      return;
    }
    const parsedInput = parseShape(torchGraph.inputShape);
    if (!parsedInput) {
      setTorchExportStatus('error');
      setTorchExportError('Invalid input shape. Use [N, C, H, W] or [N, C, L].');
      return;
    }
    setTorchExportStatus('exporting');
    setTorchExportError(null);
    try {
      const payload = {
        name: modelDraft.name,
        graph: torchGraph,
        batchSize: modelDraft.trainParams.batchSize,
        trainParams: modelDraft.trainParams,
        ptDescription: modelDraft.ptDescription
      };
      const response = await fetch('/api/torch/export', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(payload)
      });
      if (!response.ok) {
        let message = `Export failed (${response.status})`;
        try {
          const failure = (await response.json()) as { error?: string };
          if (failure.error && failure.error.trim().length > 0) {
            message = failure.error.trim();
          }
        } catch {
          // fallback to generic HTTP status message
        }
        throw new Error(message);
      }
      const data = (await response.json()) as {
        ptPath: string;
        ptChecksum: string;
        ptFormat?: string;
        ptDescription?: string;
      };
      updateTorch({
        ptPath: data.ptPath,
        ptChecksum: data.ptChecksum,
        ptFormat: data.ptFormat ?? 'torchscript',
        ptDescription: data.ptDescription ?? modelDraft.ptDescription
      });
      setTorchExportStatus('done');
      loadImportableTorchModels();
    } catch (error) {
      setTorchExportStatus('error');
      setTorchExportError(error instanceof Error ? error.message : 'Export failed');
    }
  }, [isTorch, loadImportableTorchModels, modelDraft, torchGraph, updateTorch]);

  const handleTorchImport = useCallback(
    async (modelName: string) => {
      if (!modelName || !torchDraft) {
        return;
      }
      try {
        const response = await fetch(`/api/torch/import?name=${encodeURIComponent(modelName)}`);
        if (!response.ok) {
          throw new Error(`Import failed (${response.status})`);
        }
        const data = (await response.json()) as {
          graph?: TorchModel['graph'];
          ptPath?: string;
          ptChecksum?: string;
          ptFormat?: string;
          ptDescription?: string;
          trainParams?: TorchModel['trainParams'];
        };
        if (!data.graph) {
          throw new Error('No graph metadata found.');
        }
        updateTorch({
          graph: data.graph,
          ptPath: data.ptPath ?? torchDraft.ptPath,
          ptChecksum: data.ptChecksum ?? torchDraft.ptChecksum,
          ptFormat: data.ptFormat ?? torchDraft.ptFormat,
          ptDescription: data.ptDescription ?? torchDraft.ptDescription,
          trainParams: data.trainParams
            ? { ...torchDraft.trainParams, ...data.trainParams }
            : torchDraft.trainParams
        });
      } catch (error) {
        setTorchExportStatus('error');
        setTorchExportError(error instanceof Error ? error.message : 'Import failed');
      }
    },
    [torchDraft, updateTorch]
  );

  useEffect(() => {
    if (isTorch) {
      const graph = torchGraph;
      if (!graph) {
        return;
      }
      const warningsByNode = new Map<string, string>();
      (torchInference?.warnings ?? []).forEach((warning) => {
        warningsByNode.set(warning.nodeId, warning.message);
      });
      const incoming = new Map<string, string[]>();
      graph.edges.forEach((edge) => {
        if (!incoming.has(edge.to)) {
          incoming.set(edge.to, []);
        }
        incoming.get(edge.to)?.push(edge.from);
      });
      const parsedInput = parseShape(graph.inputShape);
      const fallbackInput = parsedInput;
      const nodes: Node<ModelGraphNodeData>[] = graph.nodes.map((node, index) => {
        const sources = incoming.get(node.id) ?? [];
        const inputShape =
          sources.length > 0
            ? torchInference?.shapes.get(sources[0]) ?? null
            : fallbackInput;
        const outputShape = torchInference?.shapes.get(node.id) ?? null;
        const { width: widthBase, height: heightBase } = getTorchNodeDimensions(outputShape);
        const position =
          node.position ?? {
            x: 120 + (index % 3) * 240,
            y: 120 + Math.floor(index / 3) * 160
          };
        return {
          id: node.id,
          type: 'model',
          position,
          initialWidth: widthBase,
          initialHeight: heightBase,
          handles: getNodeHandles(graphLayout, widthBase, heightBase),
          data: {
            label: node.name,
            subtitle: node.type,
            info: [
              `in ${formatShape(inputShape ?? null)}`,
              `out ${formatShape(outputShape ?? null)}`
            ],
            warning: warningsByNode.get(node.id),
            tone: toneForTorch(node.type),
            layout: graphLayout
          },
          style: { width: widthBase, height: heightBase }
        };
      });
      const nodeMap = new Map(nodes.map((node) => [node.id, node]));
      const bounds = getBounds(nodes);
      const edges: Edge[] = graph.edges.map((edge) => ({
        id: edge.id,
        source: edge.from,
        target: edge.to,
        ...edgePresentationForNodes(edge.from, edge.to, nodeMap, bounds),
        animated: true
      }));
      setGraphNodes(nodes);
      setGraphEdges(edges);
      return;
    }

    const layers = openLayers;
    const nodes: Node<ModelGraphNodeData>[] = layers.map((layer, index) => {
      const position =
        openLayerPositions[layer.id] ?? {
          x: 120 + (index % 4) * 220,
          y: 120 + Math.floor(index / 4) * 160
        };
      const functionOptions = functionOptionsByLayerType[layer.type] ?? activationFunctionOptions;
      const functionLabel =
        functionOptions.find((option) => option.value === layer.functionCode)?.label ??
        layer.functionCode;
      return {
        id: layer.id,
        type: 'model',
        position,
        initialWidth: 180,
        initialHeight: 90,
        handles: getNodeHandles(graphLayout, 180, 90),
        data: {
          label: layerTypeLabelByValue[layer.type] ?? 'Layer',
          subtitle: `Layer ${index + 1}`,
          info: [`size ${layer.size}`, `fn ${functionLabel}`],
          tone: toneForOpenLayer(layer.type),
          layout: graphLayout
        },
        style: { width: 180, height: 90 }
      };
    });
    const nodeMap = new Map(nodes.map((node) => [node.id, node]));
    const bounds = getBounds(nodes);
    const edges: Edge[] = layers.slice(0, -1).map((layer, index) => ({
      id: `${layer.id}-${layers[index + 1].id}`,
      source: layer.id,
      target: layers[index + 1].id,
      ...edgePresentationForNodes(layer.id, layers[index + 1].id, nodeMap, bounds),
      animated: true
    }));
    setGraphNodes(nodes);
    setGraphEdges(edges);
  }, [
    isTorch,
    edgePresentationForNodes,
    graphLayout,
    openLayerPositions,
    openLayers,
    setGraphEdges,
    setGraphNodes,
    torchGraph,
    torchInference
  ]);

  useEffect(() => {
    if (!isTorch) {
      return;
    }
    const graph = torchGraph;
    if (!graph) {
      return;
    }
    if (graph.nodes.length === 0) {
      setSelectedTorchNodeId(null);
      return;
    }
    if (!selectedTorchNodeId || !graph.nodes.some((node) => node.id === selectedTorchNodeId)) {
      setSelectedTorchNodeId(graph.nodes[0].id);
    }
  }, [isTorch, selectedTorchNodeId, torchGraph]);

  const validation = useMemo(() => validatePlannerState(state), [state]);
  const modelIssues = useMemo(
    () => validation.issues.filter((issue) => issue.scope.includes('models')),
    [validation]
  );

  useEffect(() => {
    if (!isTorch) {
      return;
    }
    if (!torchTrainParams) {
      return;
    }
    const graph = torchGraph;
    if (!graph) {
      return;
    }
    const parsedInput = parseShape(graph.inputShape);
    if (!parsedInput) {
      return;
    }
    const batchSize = torchTrainParams.batchSize.trim() || '1';
    const resolvedInput = `[${parsedInput
      .map((dim) => (dim === 'N' ? batchSize : dim))
      .join(', ')}]`;
    const sinkNode = selectPreferredTorchOutputNode(graph);
    const outputShape = sinkNode ? torchInference?.shapes.get(sinkNode.id) ?? null : parsedInput;
    const resolvedOutput = outputShape
      ? `[${outputShape.map((dim) => (dim === 'N' ? batchSize : dim)).join(', ')}]`
      : torchTrainParams.labelsShape;
    setModelDraft((current) => {
      if (current.infraType !== 'torch') {
        return current;
      }
      if (
        current.trainParams.inputTensorShape === resolvedInput &&
        current.trainParams.labelsShape === resolvedOutput
      ) {
        return current;
      }
      return {
        ...current,
        trainParams: {
          ...current.trainParams,
          inputTensorShape: resolvedInput,
          labelsShape: resolvedOutput
        }
      };
    });
  }, [isTorch, torchGraph, torchTrainParams?.batchSize, torchTrainParams?.labelsShape, torchInference]);

  const torchNodeCount = torchGraph?.nodes.length ?? 0;
  useEffect(() => {
    if (graphLayout === 'free') {
      return;
    }
    if (isTorch) {
      updateTorchGraph((graph) => {
        const layoutSpecs = graph.nodes.map((node) => {
          const shape = torchInference?.shapes.get(node.id) ?? null;
          const { width, height } = getTorchNodeDimensions(shape);
          return { id: node.id, width, height };
        });
        const { positions } = chooseWrappedAutoLayout(graphLayout, layoutSpecs, graphFrameSize);
        let changed = false;
        const nextNodes = graph.nodes.map((node) => {
          const nextPosition = positions[node.id];
          if (!nextPosition) {
            return node;
          }
          const currentPosition = node.position;
          if (
            !currentPosition ||
            Math.abs(currentPosition.x - nextPosition.x) > 0.5 ||
            Math.abs(currentPosition.y - nextPosition.y) > 0.5
          ) {
            changed = true;
            return { ...node, position: nextPosition };
          }
          return node;
        });
        if (!changed) {
          return graph;
        }
        return { ...graph, nodes: nextNodes };
      });
      return;
    }
    const layoutSpecs = openLayers.map((layer) => ({ id: layer.id, width: 180, height: 90 }));
    const { positions } = chooseWrappedAutoLayout(graphLayout, layoutSpecs, graphFrameSize);
    const changed = openLayers.some((layer) => {
      const prevPos = openLayerPositions[layer.id];
      const nextPos = positions[layer.id];
      if (!nextPos || !prevPos) {
        return true;
      }
      return Math.abs(prevPos.x - nextPos.x) > 0.5 || Math.abs(prevPos.y - nextPos.y) > 0.5;
    });
    if (changed || Object.keys(openLayerPositions).length !== Object.keys(positions).length) {
      setOpenLayerPositions(positions);
    }
  }, [
    graphLayout,
    graphFrameSize,
    isTorch,
    openLayerCount,
    openLayerPositions,
    openLayers,
    torchInference,
    torchNodeCount,
    updateTorchGraph
  ]);

  useEffect(() => {
    if (graphLayout === 'free' || !isTorch || !torchGraph) {
      return;
    }
    if (torchGraph.edges.length === 0 && torchGraph.nodes.length > 1) {
      updateTorchGraph((graph) => {
        const edges: TorchModel['graph']['edges'] = [];
        for (let i = 0; i < graph.nodes.length - 1; i += 1) {
          edges.push({
            id: `${graph.nodes[i].id}-${graph.nodes[i + 1].id}`,
            from: graph.nodes[i].id,
            to: graph.nodes[i + 1].id
          });
        }
        return { ...graph, edges };
      });
    }
  }, [graphLayout, isTorch, torchGraph, updateTorchGraph]);

  const arrangedGeometrySignature = useMemo(
    () =>
      graphLayout === 'free'
        ? ''
        : graphNodes
            .map((node) => {
              const { width, height } = getNodeSize(node);
              return `${node.id}:${Math.round(node.position.x)}:${Math.round(node.position.y)}:${Math.round(width)}:${Math.round(height)}`;
            })
            .join('|'),
    [graphLayout, graphNodes]
  );

  useEffect(() => {
    if (!graphFlowInstance || graphNodes.length === 0) {
      return;
    }
    const pendingNodeId = pendingCenterNodeIdRef.current;
    if (!pendingNodeId) {
      return;
    }
    const targetNode = graphNodes.find((node) => node.id === pendingNodeId);
    if (!targetNode) {
      return;
    }
    const { width, height } = getNodeSize(targetNode);
    const centerX = (targetNode.position?.x ?? 0) + width / 2;
    const centerY = (targetNode.position?.y ?? 0) + height / 2;
    const zoom = graphFlowInstance.getViewport().zoom;

    const id = setTimeout(() => {
      void graphFlowInstance.setCenter(centerX, centerY, {
        zoom,
        duration: 260
      });
      if (pendingCenterNodeIdRef.current === pendingNodeId) {
        pendingCenterNodeIdRef.current = null;
      }
    }, 110);
    return () => clearTimeout(id);
  }, [
    arrangedGeometrySignature,
    graphNodes,
    graphFlowInstance,
    graphNodes.length
  ]);

  useEffect(() => {
    if (
      !graphFlowInstance ||
      graphNodes.length === 0 ||
      previousLayoutRef.current === graphLayout
    ) {
      previousLayoutRef.current = graphLayout;
      return;
    }
    previousLayoutRef.current = graphLayout;
    const id = setTimeout(() => {
      graphFlowInstance.fitView({ padding: 0.2, duration: 250, maxZoom: 1.2, minZoom: 0.1 });
    }, 80);
    return () => clearTimeout(id);
  }, [graphFlowInstance, graphLayout, graphNodes.length]);

  const saveModel = () => {
    const existingIndex = state.models.findIndex((model) => model.id === modelDraft.id);
    const nextModels = [...state.models];
    if (existingIndex >= 0) {
      nextModels[existingIndex] = modelDraft;
    } else {
      nextModels.push(modelDraft);
    }
    setSelectedModelId(modelDraft.id);
    onChange({ ...state, models: nextModels });
  };

  const deleteModel = () => {
    const nextModels = state.models.filter((model) => model.id !== modelDraft.id);
    const nextWorkers = state.workers.filter((worker) => worker.modelId !== modelDraft.id);
    onChange({ ...state, models: nextModels, workers: nextWorkers });
    if (nextModels.length > 0) {
      setSelectedModelId(nextModels[0].id);
    } else {
      const fresh = createTorchModel();
      setModelDraft(fresh);
      setSelectedModelId(null);
    }
  };

  const createNewModel = () => {
    const fresh = createTorchModel('New Torch Model');
    setModelDraft(fresh);
    setSelectedModelId(null);
    setSelectedLayerIndex(0);
    setSelectedTorchNodeId(fresh.infraType === 'torch' ? fresh.graph.nodes[0]?.id ?? null : null);
  };

  const selectedLayer =
    modelDraft.infraType === 'torch' ? null : openLayers[selectedLayerIndex];
  const layerFunctionOptions = selectedLayer
    ? functionOptionsByLayerType[selectedLayer.type] ?? activationFunctionOptions
    : activationFunctionOptions;
  const selectedTorchNode =
    modelDraft.infraType === 'torch' && torchGraph
      ? torchGraph.nodes.find((node) => node.id === selectedTorchNodeId) ?? null
      : null;
  const torchWarnings = torchInference?.warnings ?? [];

  const toneForTorch = (type: TorchLayerNode['type']) => {
    if (type === 'conv1d' || type === 'conv2d') {
      return 'tone-conv';
    }
    if (type === 'maxpool1d' || type === 'maxpool2d') {
      return 'tone-pool';
    }
    if (type === 'linear' || type === 'transformer') {
      return 'tone-linear';
    }
    if (type === 'residual') {
      return 'tone-residual';
    }
    if (type === 'flatten') {
      return 'tone-flatten';
    }
    if (type === 'relu' || type === 'sigmoid' || type === 'softmax') {
      return 'tone-activation';
    }
    return 'tone-utility';
  };

  const toneForOpenLayer = (type: string) => {
    if (type === '2') {
      return 'tone-conv';
    }
    if (type === '4') {
      return 'tone-pool';
    }
    if (type === '3' || type === '6' || type === '7') {
      return 'tone-linear';
    }
    if (type === '9') {
      return 'tone-flatten';
    }
    if (type === '5') {
      return 'tone-activation';
    }
    if (type === '1' || type === '8' || type === '10') {
      return 'tone-utility';
    }
    return 'tone-utility';
  };
  const torchIncoming = useMemo(() => {
    if (!isTorch) {
      return new Map<string, string[]>();
    }
    const incoming = new Map<string, string[]>();
    torchGraph?.edges.forEach((edge) => {
      if (!incoming.has(edge.to)) {
        incoming.set(edge.to, []);
      }
      incoming.get(edge.to)?.push(edge.from);
    });
    return incoming;
  }, [isTorch, torchGraph]);
  const torchInputShape = isTorch ? torchGraph?.inputShape ?? '' : '';
  const resolveTorchInputShape = useCallback(
    (nodeId: string) => {
      if (!isTorch) {
        return null;
      }
      const sourceIds = torchIncoming.get(nodeId) ?? [];
      if (sourceIds.length === 0) {
        return parseShape(torchInputShape);
      }
      return torchInference?.shapes.get(sourceIds[0]) ?? null;
    },
    [isTorch, torchInputShape, torchIncoming, torchInference]
  );
  const layerInspectorContent =
    modelDraft.infraType === 'torch' ? (
      selectedTorchNode ? (
        <>
          <label className="field">
            <span>Name</span>
            <input
              type="text"
              value={selectedTorchNode.name}
              onChange={(event) =>
                updateTorchNode(selectedTorchNode.id, { name: event.target.value })
              }
            />
          </label>
          <div className="inline-fields">
            <label className="field">
              <span>Type</span>
              <select
                value={selectedTorchNode.type}
                onChange={(event) => {
                  const nextType = event.target.value as TorchLayerNode['type'];
                  const definition = torchLayerCatalog.find((entry) => entry.type === nextType);
                  updateTorchNode(selectedTorchNode.id, {
                    type: nextType,
                    name: definition?.label ?? nextType,
                    params: torchLayerDefaults[nextType]
                  });
                }}
              >
                {torchLayerCatalog.map((entry) => (
                  <option key={entry.type} value={entry.type}>
                    {entry.label}
                  </option>
                ))}
              </select>
            </label>
            <label className="field">
              <span>Input</span>
              <input
                type="text"
                value={formatShape(resolveTorchInputShape(selectedTorchNode.id))}
                readOnly
              />
            </label>
            <label className="field">
              <span>Output</span>
              <input
                type="text"
                value={formatShape(torchInference?.shapes.get(selectedTorchNode.id) ?? null)}
                readOnly
              />
            </label>
          </div>
          <div className="torch-params">
            {torchLayerCatalog
              .find((entry) => entry.type === selectedTorchNode.type)
              ?.params?.map((param) => {
                const value = selectedTorchNode.params[param.key] ?? '';
                if (param.type === 'select') {
                  return (
                    <label key={param.key} className="field">
                      <span>{param.label}</span>
                      <select
                        value={typeof value === 'number' ? value : 0}
                        onChange={(event) =>
                          updateTorchNode(selectedTorchNode.id, {
                            params: { [param.key]: Number(event.target.value) }
                          })
                        }
                      >
                        {(param.options ?? []).map((option) => (
                          <option key={option.value} value={option.value}>
                            {option.label}
                          </option>
                        ))}
                      </select>
                    </label>
                  );
                }
                return (
                  <label key={param.key} className="field">
                    <span>{param.label}</span>
                    <input
                      type="number"
                      value={typeof value === 'number' ? value : ''}
                      min={param.min}
                      step={param.step}
                      onChange={(event) => {
                        const parsed = Number(event.target.value);
                        if (!Number.isNaN(parsed)) {
                          updateTorchNode(selectedTorchNode.id, {
                            params: { [param.key]: parsed }
                          });
                        }
                      }}
                    />
                  </label>
                );
              })}
          </div>
          <div className="panel-actions align-right">
            <button
              type="button"
              className="ghost danger"
              onClick={() => removeTorchNode(selectedTorchNode.id)}
            >
              Remove Layer
            </button>
          </div>
        </>
      ) : (
        <p className="muted">Select a layer to edit parameters.</p>
      )
    ) : selectedLayer ? (
      <>
        <div className="inline-fields">
          <label className="field">
            <span>Size</span>
            <input
              type="text"
              value={selectedLayer.size}
              onChange={(event) => updateLayer(selectedLayerIndex, { size: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Type</span>
            <select
              value={selectedLayer.type}
              onChange={(event) =>
                updateLayer(selectedLayerIndex, {
                  type: event.target.value,
                  functionCode:
                    defaultLayerFunctionByType[event.target.value] ?? selectedLayer.functionCode
                })
              }
            >
              {layerTypeOptions.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <label className="field">
            <span>Function</span>
            <select
              value={selectedLayer.functionCode}
              onChange={(event) =>
                updateLayer(selectedLayerIndex, { functionCode: event.target.value })
              }
            >
              {layerFunctionOptions.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
        </div>
        <div className="panel-actions align-right">
          <button
            type="button"
            className="ghost"
            onClick={() => moveLayer(selectedLayerIndex, -1)}
            disabled={selectedLayerIndex === 0}
          >
            Move Up
          </button>
          <button
            type="button"
            className="ghost"
            onClick={() => moveLayer(selectedLayerIndex, 1)}
            disabled={selectedLayerIndex >= openLayerCount - 1}
          >
            Move Down
          </button>
          <button
            type="button"
            className="ghost danger"
            onClick={() => removeLayer(selectedLayerIndex)}
          >
            Remove Layer
          </button>
        </div>
      </>
    ) : (
      <p className="muted">Select a layer to edit parameters.</p>
    );
  const getModelLayerCount = useCallback((model: WorkerModel) => {
    if (model.infraType === 'torch') {
      return model.graph.nodes.length;
    }
    return model.layers.length;
  }, []);

  return (
    <div className="view model-lab-view">
      <div className="hero compact">
        <div>
          <p className="eyebrow">Worker Model Lab</p>
          <h1>Shape model payloads with an intuitive layer studio.</h1>
          <p>Build and reuse worker models, then assign them to clients in the sandbox.</p>
        </div>
        <div className="hero-badge">
          <div>
            <span>Models</span>
            <strong>{state.models.length}</strong>
          </div>
          <div>
              <span>Layers</span>
              <strong>
                {modelDraft.infraType === 'torch'
                  ? torchGraph?.nodes.length ?? 0
                  : openLayerCount}
              </strong>
            </div>
        </div>
      </div>

      <div className="model-lab-layout">
        <div className="panel model-library">
          <div className="panel-header">
            <div>
              <p className="panel-title">Model Library</p>
              <p className="panel-subtitle">Select a model to edit or start fresh.</p>
            </div>
            <div className="panel-actions">
              <button type="button" className="ghost" onClick={createNewModel}>
                New Model
              </button>
            </div>
          </div>
          <div className="library-list">
            {state.models.map((entry) => (
              <button
                key={entry.id}
                className={`library-item ${entry.id === selectedModelId ? 'active' : ''}`}
                type="button"
                onClick={() => setSelectedModelId(entry.id)}
              >
                <div>
                  <strong>{entry.name}</strong>
                  <span className="muted">
                    {infraTypeOptions.find((option) => option.value === entry.infraType)?.label ??
                      ((entry as { infraType?: string }).infraType === '2' ? 'Torch' : 'Unknown')}
                  </span>
                </div>
                <span className="muted">{getModelLayerCount(entry)} layers</span>
              </button>
            ))}
            {state.models.length === 0 && <p className="muted">No models saved yet.</p>}
          </div>
        </div>

        <div className="model-graph-panel">
          <div className="model-graph-header">
            <div>
              <p className="panel-title">Model Graph</p>
              <p className="panel-subtitle">
                Right-click or double-click to add layers. Press-hold drag to pan the grid.{' '}
                {isTorch
                  ? 'Right-click a layer and choose Start Link to connect nodes.'
                  : 'OpenNN layers connect sequentially.'}
              </p>
            </div>
            <div className="panel-actions">
              <div className="graph-layout-toggle">
                <button
                  type="button"
                  className={graphLayout === 'free' ? 'active' : ''}
                  onClick={() => setGraphLayout('free')}
                >
                  Free
                </button>
                <button
                  type="button"
                  className={graphLayout === 'horizontal' ? 'active' : ''}
                  onClick={() => setGraphLayout('horizontal')}
                >
                  Horizontal
                </button>
                <button
                  type="button"
                  className={graphLayout === 'vertical' ? 'active' : ''}
                  onClick={() => setGraphLayout('vertical')}
                >
                  Vertical
                </button>
              </div>
            </div>
          </div>
          {isTorch && torchWarnings.length > 0 && (
            <div className="graph-warnings">
              {torchWarnings.map((warning, index) => (
                <p key={`${warning.nodeId}-${index}`}>{warning.message}</p>
              ))}
            </div>
          )}
          <div
            className="model-graph-frame"
            ref={graphFrameRef}
            onPointerDownCapture={handleGraphFramePointerDownCapture}
            onPointerMoveCapture={handleGraphFramePointerMoveCapture}
            onPointerUpCapture={handleGraphFramePointerUpCapture}
            onPointerCancelCapture={handleGraphFramePointerUpCapture}
          >
            <ReactFlow<Node<ModelGraphNodeData>, Edge>
              nodes={graphNodes}
              edges={graphEdges}
              nodeTypes={graphNodeTypes}
              defaultEdgeOptions={{ interactionWidth: 32 }}
              onInit={setGraphFlowInstance}
              onNodesChange={handleGraphNodesChange}
              onEdgesChange={handleGraphEdgesChange}
              onNodeClick={handleGraphNodeClick}
              onNodeContextMenu={handleGraphNodeContextMenu}
              onNodeDoubleClick={handleGraphNodeContextMenu}
              onEdgeContextMenu={handleGraphEdgeContextMenu}
              onEdgeDoubleClick={handleGraphEdgeDoubleClick}
              onConnect={handleGraphConnect}
              onPaneClick={handleGraphPaneClick}
              onPaneContextMenu={handleGraphPaneContextMenu}
              panOnDrag={false}
              zoomOnScroll
              zoomOnPinch
              nodesDraggable={false}
              selectionOnDrag={false}
              zoomOnDoubleClick={false}
              translateExtent={graphTranslateExtent}
              fitView
            >
              <Background color="#d1c8b8" gap={18} variant={BackgroundVariant.Dots} />
              <Controls />
            </ReactFlow>
            {graphPalette && (
              <div
                className="graph-layer-picker"
                style={{ left: graphPalette.x, top: graphPalette.y }}
              >
                <div className="picker-header">
                  <span>Add New Layer</span>
                  <button type="button" className="picker-close" onClick={() => setGraphPalette(null)}>
                    ✕
                  </button>
                </div>
                <div className="picker-list">
                  {graphLayerOptions.map((option) => (
                    <button
                      key={option.key}
                      type="button"
                      className="picker-option"
                      onClick={() => {
                        if (isTorch) {
                          addTorchLayer(
                            option.key as TorchLayerNode['type'],
                            graphLayout === 'free'
                              ? { x: graphPalette.flowX, y: graphPalette.flowY }
                              : undefined
                          );
                        } else {
                          addLayerByType(option.key, undefined, {
                            x: graphPalette.flowX,
                            y: graphPalette.flowY
                          });
                        }
                        setGraphPalette(null);
                      }}
                    >
                      <strong>{option.label}</strong>
                      <span>{option.description}</span>
                    </button>
                  ))}
                </div>
              </div>
            )}
            {layerInspectorOpen && (
              <div className="floating-panel">
                <div className="floating-panel-header">
                  <div>
                    <p className="panel-title">Layer Inspector</p>
                    <p className="panel-subtitle">Adjust layer details and parameters.</p>
                  </div>
                  <button type="button" className="ghost" onClick={() => setLayerInspectorOpen(false)}>
                    Close
                  </button>
                </div>
                <div className="floating-panel-body">{layerInspectorContent}</div>
              </div>
            )}
            {graphNodes.length === 0 && (
              <div className="model-graph-empty">Right-click or double-click to add your first layer.</div>
            )}
          </div>

          {graphMenu && (
            <div className="context-menu" style={{ top: graphMenu.y, left: graphMenu.x }}>
              <button
                type="button"
                onClick={() => {
                  openGraphPalette(graphMenu);
                  setGraphMenu(null);
                }}
              >
                Add New Layer
              </button>
            </div>
          )}

          {nodeMenu && (
            <div className="context-menu" style={{ top: nodeMenu.y, left: nodeMenu.x }}>
              {isTorch && (
                <>
                  {!linkingFrom && (
                    <button
                      type="button"
                      onClick={() => {
                        setLinkingFrom(nodeMenu.nodeId);
                        setSelectedTorchNodeId(nodeMenu.nodeId);
                        setNodeMenu(null);
                      }}
                    >
                      Start Link
                    </button>
                  )}
                  {linkingFrom && linkingFrom !== nodeMenu.nodeId && (
                    <button
                      type="button"
                      onClick={() => {
                        addTorchLink(linkingFrom, nodeMenu.nodeId);
                        setSelectedTorchNodeId(nodeMenu.nodeId);
                        setLinkingFrom(null);
                        setNodeMenu(null);
                      }}
                    >
                      Link Here
                    </button>
                  )}
                  {linkingFrom === nodeMenu.nodeId && (
                    <button
                      type="button"
                      onClick={() => {
                        setLinkingFrom(null);
                        setNodeMenu(null);
                      }}
                    >
                      Cancel Link Start
                    </button>
                  )}
                </>
              )}
              <button
                type="button"
                onClick={() => {
                  openLayerInspector(nodeMenu.nodeId);
                  setNodeMenu(null);
                }}
              >
                Configure
              </button>
              <button
                type="button"
                className="danger"
                onClick={() => {
                  if (isTorch) {
                    removeTorchNode(nodeMenu.nodeId);
                  } else {
                    removeOpenLayerById(nodeMenu.nodeId);
                  }
                  setNodeMenu(null);
                }}
              >
                Remove Layer
              </button>
            </div>
          )}

          {edgeMenu && (
            <div className="context-menu" style={{ top: edgeMenu.y, left: edgeMenu.x }}>
              <span className="menu-title">Insert Layer</span>
              {graphLayerOptions.map((option) => (
                <button
                  key={option.key}
                  type="button"
                  onClick={() => {
                    if (isTorch) {
                      insertTorchLayerBetween(
                        edgeMenu.source,
                        edgeMenu.target,
                        option.key as TorchLayerNode['type'],
                        { x: edgeMenu.flowX, y: edgeMenu.flowY }
                      );
                    } else {
                      insertOpenLayerBetween(
                        edgeMenu.source,
                        edgeMenu.target,
                        option.key,
                        { x: edgeMenu.flowX, y: edgeMenu.flowY }
                      );
                    }
                    setEdgeMenu(null);
                  }}
                >
                  Insert {option.label}
                </button>
              ))}
              {isTorch && (
                <>
                  <div className="menu-divider" />
                  <button
                    type="button"
                    className="danger"
                    onClick={() => {
                      const nextEdges = graphEdges.filter(
                        (edge) =>
                          !(edge.source === edgeMenu.source && edge.target === edgeMenu.target)
                      );
                      setGraphEdges(nextEdges);
                      syncTorchEdges(nextEdges);
                      setEdgeMenu(null);
                    }}
                  >
                    Remove Link
                  </button>
                </>
              )}
            </div>
          )}

        </div>

        <div className="model-detail-grid">
          <div className={`panel model-settings ${modelDraft.infraType === 'torch' ? 'torch' : ''}`}>
            <div className="panel-header">
              <div>
                <p className="panel-title">Model Settings</p>
                <p className="panel-subtitle">Core identity and training configuration.</p>
              </div>
            </div>
            <label className="field">
              <span>Model Name</span>
              <input
                type="text"
                value={modelDraft.name}
                onChange={(event) => setModelDraft({ ...modelDraft, name: event.target.value })}
              />
            </label>
            <div className="inline-fields">
              <label className="field">
                <span>Infra</span>
                <select
                  value={modelDraft.infraType}
                  onChange={(event) => {
                    if (event.target.value === 'torch') {
                      const next = createTorchModel(modelDraft.name);
                      setModelDraft({
                        ...next,
                        distributedSystemType: modelDraft.distributedSystemType,
                        distributedSystemArgs: modelDraft.distributedSystemArgs,
                        distributedSystemToken: modelDraft.distributedSystemToken
                      });
                    } else if (modelDraft.infraType === 'torch') {
                      setModelDraft({
                        ...createOpenNNModel(modelDraft.name),
                        infraType: event.target.value as '0' | '1'
                      });
                    } else {
                      setModelDraft({
                        ...createOpenNNModel(modelDraft.name, openLayers),
                        infraType: event.target.value as '0' | '1'
                      });
                    }
                  }}
                >
                  {infraTypeOptions.map((option) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </select>
              </label>
              <label className="field">
                <span>Distributed System</span>
                <select
                  value={modelDraft.distributedSystemType}
                  onChange={(event) => {
                    if (modelDraft.infraType === 'torch') {
                      updateTorch({ distributedSystemType: event.target.value });
                    } else {
                      updateOpenNN({ distributedSystemType: event.target.value });
                    }
                  }}
                >
                  {distributedSystemOptions.map((option) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </select>
              </label>
            </div>

            {modelDraft.infraType !== 'torch' && (
              <>
                <label className="field">
                  <span>Model Type</span>
                  <select
                    value={modelDraft.modelType}
                    onChange={(event) => updateOpenNN({ modelType: event.target.value })}
                  >
                    {modelTypeOptions.map((option) => (
                      <option key={option.value} value={option.value}>
                        {option.label}
                      </option>
                    ))}
                  </select>
                </label>
                <label className="field">
                  <span>Model Args</span>
                  <input
                    type="text"
                    value={modelDraft.modelArgs}
                    onChange={(event) => updateOpenNN({ modelArgs: event.target.value })}
                  />
                </label>
                <div className="inline-fields">
                  <label className="field">
                    <span>Learning Rate</span>
                    <input
                      type="text"
                      value={modelDraft.learningRate}
                      onChange={(event) => updateOpenNN({ learningRate: event.target.value })}
                    />
                  </label>
                  <label className="field">
                    <span>Epochs</span>
                    <input
                      type="text"
                      value={modelDraft.epochs}
                      onChange={(event) => updateOpenNN({ epochs: event.target.value })}
                    />
                  </label>
                </div>
                <div className="inline-fields">
                  <label className="field">
                    <span>Optimizer</span>
                    <select
                      value={modelDraft.optimizer}
                      onChange={(event) => updateOpenNN({ optimizer: event.target.value })}
                    >
                      {optimizerOptions.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                  <label className="field">
                    <span>Optimizer Args</span>
                    <input
                      type="text"
                      value={modelDraft.optimizerArgs}
                      onChange={(event) => updateOpenNN({ optimizerArgs: event.target.value })}
                    />
                  </label>
                </div>
                <div className="inline-fields">
                  <label className="field">
                    <span>Loss Method</span>
                    <select
                      value={modelDraft.lossMethod}
                      onChange={(event) => updateOpenNN({ lossMethod: event.target.value })}
                    >
                      {lossMethodOptions.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                  <label className="field">
                    <span>Loss Args</span>
                    <input
                      type="text"
                      value={modelDraft.lossArgs}
                      onChange={(event) => updateOpenNN({ lossArgs: event.target.value })}
                    />
                  </label>
                </div>
              </>
            )}

            {modelDraft.infraType === 'torch' && (
              <div className="torch-settings-grid">
                <div className="torch-settings-col">
                  <label className="field">
                      <span>Input Shape (N, C, H, W)</span>
                      <input
                        type="text"
                        value={torchInputShape}
                        onChange={(event) =>
                          updateTorchGraph((graph) => ({ ...graph, inputShape: event.target.value }))
                        }
                      />
                    </label>
                  <p className="muted">Legend: N = batch size set below.</p>
                  <div className="inline-fields">
                    <label className="field">
                      <span>Batch Size (N)</span>
                      <input
                        type="text"
                        value={modelDraft.trainParams.batchSize}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, batchSize: event.target.value }
                          })
                        }
                      />
                    </label>
                    <label className="field">
                      <span>Learning Rate</span>
                      <input
                        type="text"
                        value={modelDraft.trainParams.lr}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, lr: event.target.value }
                          })
                        }
                      />
                    </label>
                    <label className="field">
                      <span>Epochs</span>
                      <input
                        type="text"
                        value={modelDraft.trainParams.epochs}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, epochs: event.target.value }
                          })
                        }
                      />
                    </label>
                  </div>
                  <div className="inline-fields">
                    <label className="field">
                      <span>Optimizer</span>
                      <select
                        value={modelDraft.trainParams.optimizer}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, optimizer: event.target.value }
                          })
                        }
                      >
                        {torchOptimizerChoices.map((option) => (
                          <option key={option.value} value={option.value}>
                            {option.label}
                          </option>
                        ))}
                      </select>
                    </label>
                    <label className="field">
                      <span>Loss</span>
                      <select
                        value={modelDraft.trainParams.loss}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, loss: event.target.value }
                          })
                        }
                      >
                        {torchLossChoices.map((option) => (
                          <option key={option.value} value={option.value}>
                            {option.label}
                          </option>
                        ))}
                      </select>
                    </label>
                  </div>
                  <div className="inline-fields">
                    <label className="field">
                      <span>Labels Offset</span>
                      <input
                        type="text"
                        value={modelDraft.trainParams.labelsOffset}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, labelsOffset: event.target.value }
                          })
                        }
                      />
                    </label>
                    <label className="field">
                      <span>Weight Init Random</span>
                      <input
                        type="text"
                        value={modelDraft.trainParams.wInitRand}
                        onChange={(event) =>
                          updateTorch({
                            trainParams: { ...modelDraft.trainParams, wInitRand: event.target.value }
                          })
                        }
                      />
                    </label>
                  </div>
                </div>

                <div className="torch-settings-col">
                  <div className="inline-fields">
                    <label className="field">
                      <span>Input Tensor Shape</span>
                      <input type="text" value={modelDraft.trainParams.inputTensorShape} readOnly />
                    </label>
                    <label className="field">
                      <span>Labels Shape</span>
                      <input type="text" value={modelDraft.trainParams.labelsShape} readOnly />
                    </label>
                  </div>
                  <div className="panel-section">
                    <p className="section-title">TorchScript Artifact</p>
                    <label className="field">
                      <span>Path</span>
                      <input type="text" value={modelDraft.ptPath} readOnly />
                    </label>
                    <div className="inline-fields">
                      <label className="field">
                        <span>Format</span>
                        <input type="text" value={modelDraft.ptFormat} readOnly />
                      </label>
                      <label className="field">
                        <span>Checksum</span>
                        <input type="text" value={modelDraft.ptChecksum} readOnly />
                      </label>
                    </div>
                    <label className="field">
                      <span>Description</span>
                      <input
                        type="text"
                        value={modelDraft.ptDescription}
                        onChange={(event) => updateTorch({ ptDescription: event.target.value })}
                      />
                    </label>
                    <div className="panel-actions align-right">
                      <button
                        type="button"
                        className="primary"
                        onClick={handleTorchExport}
                        disabled={torchExportStatus === 'exporting'}
                      >
                        {torchExportStatus === 'exporting' ? 'Exporting...' : 'Export TorchScript'}
                      </button>
                    </div>
                    {torchExportError && <p className="muted">{torchExportError}</p>}
                    {torchExportStatus === 'done' && (
                      <p className="muted">TorchScript saved to {modelDraft.ptPath}</p>
                    )}
                  </div>
                  <div className="panel-section">
                    <p className="section-title">Import Saved Model</p>
                    <div className="inline-fields">
                      <label className="field">
                        <span>Saved Models</span>
                        <select onChange={(event) => handleTorchImport(event.target.value)}>
                          <option value="">Select export</option>
                          {importableTorchModels.map((entry) => (
                            <option key={entry.name} value={entry.name}>
                              {entry.label}
                            </option>
                          ))}
                        </select>
                      </label>
                    </div>
                  </div>
                </div>
              </div>
            )}

            <label className="field">
              <span>Distributed System Args</span>
              <input
                type="text"
                value={modelDraft.distributedSystemArgs}
                onChange={(event) =>
                  modelDraft.infraType === 'torch'
                    ? updateTorch({ distributedSystemArgs: event.target.value })
                    : updateOpenNN({ distributedSystemArgs: event.target.value })
                }
              />
            </label>
            <label className="field">
              <span>Distributed System Token</span>
              <input
                type="text"
                value={modelDraft.distributedSystemToken}
                onChange={(event) =>
                  modelDraft.infraType === 'torch'
                    ? updateTorch({ distributedSystemToken: event.target.value })
                    : updateOpenNN({ distributedSystemToken: event.target.value })
                }
              />
            </label>

            <div className="panel-section">
              <p className="section-title">Tensor Parallel Plan</p>
              <p className="muted">Explicit layer shards used by TP orchestration.</p>
              <div className="library-list compact">
                {tpPlanEntries.map((entry, index) => (
                  <div key={`${entry.layer}-${index}`} className="library-item">
                    <div className="inline-fields">
                      <label className="field">
                        <span>Layer</span>
                        <input
                          type="text"
                          value={entry.layer}
                          onChange={(event) =>
                            updateTpPlanEntry(index, { layer: event.target.value })
                          }
                          list="tp-plan-layers"
                        />
                      </label>
                      <label className="field">
                        <span>Mode</span>
                        <select
                          value={entry.mode}
                          onChange={(event) =>
                            updateTpPlanEntry(index, {
                              mode: event.target.value as TpPlanEntry['mode']
                            })
                          }
                        >
                          <option value="column">column</option>
                          <option value="row">row</option>
                        </select>
                      </label>
                      <label className="field">
                        <span>Shard Axis</span>
                        <input
                          type="text"
                          value={entry.shardAxis}
                          onChange={(event) =>
                            updateTpPlanEntry(index, { shardAxis: event.target.value })
                          }
                        />
                      </label>
                      <label className="field">
                        <span>TP Group</span>
                        <input
                          type="text"
                          value={entry.group}
                          onChange={(event) =>
                            updateTpPlanEntry(index, { group: event.target.value })
                          }
                        />
                      </label>
                    </div>
                    <button
                      type="button"
                      className="ghost danger"
                      onClick={() => removeTpPlanEntry(index)}
                    >
                      Remove
                    </button>
                  </div>
                ))}
                {tpPlanEntries.length === 0 && <p className="muted">No TP entries yet.</p>}
              </div>
              <div className="panel-actions align-right">
                <button type="button" className="ghost" onClick={addTpPlanEntry}>
                  Add TP Entry
                </button>
              </div>
              <datalist id="tp-plan-layers">
                {availableTpLayers.map((layer) => (
                  <option key={layer} value={layer} />
                ))}
              </datalist>
            </div>

            <div className="panel-actions align-right">
              <button type="button" className="ghost danger" onClick={deleteModel} disabled={!modelDraft.id}>
                Delete Model
              </button>
              <button type="button" className="primary" onClick={saveModel}>
                Save Model
              </button>
            </div>
          </div>

          <div className="panel model-inspector">
            <div className="panel-header">
              <div>
                <p className="panel-title">Layer Stack</p>
                <p className="panel-subtitle">Keep the stack tidy as it grows.</p>
              </div>
              <button
                type="button"
                className="stack-toggle"
                onClick={() => setStackExpanded((prev) => !prev)}
              >
                {stackExpanded ? 'Hide Stack' : 'Show Stack'}
              </button>
            </div>
            {stackExpanded && (
              <div className="layer-stack-grid">
                {modelDraft.infraType === 'torch' &&
                  (torchGraph?.nodes ?? []).map((node) => (
                    <div
                      key={node.id}
                      className={`layer-stack-item ${
                        node.id === selectedTorchNodeId ? 'active' : ''
                      }`}
                    >
                      <button type="button" onClick={() => setSelectedTorchNodeId(node.id)}>
                        {node.name}
                      </button>
                      <span className="layer-stack-meta">{node.type}</span>
                    </div>
                  ))}
                {modelDraft.infraType !== 'torch' &&
                  openLayers.map((layer, index) => (
                    <div
                      key={layer.id}
                      className={`layer-stack-item ${
                        index === selectedLayerIndex ? 'active' : ''
                      }`}
                    >
                      <button type="button" onClick={() => setSelectedLayerIndex(index)}>
                        {layerTypeLabelByValue[layer.type] ?? 'Layer'}
                      </button>
                      <span className="layer-stack-meta">size {layer.size}</span>
                    </div>
                  ))}
                {modelDraft.infraType === 'torch' && (torchGraph?.nodes.length ?? 0) === 0 && (
                  <span className="muted">No layers yet.</span>
                )}
                {modelDraft.infraType !== 'torch' && openLayerCount === 0 && (
                  <span className="muted">No layers yet.</span>
                )}
              </div>
            )}

            <div className="panel-section">
              <p className="section-title">Layer Inspector</p>
              <p className="muted">Right-click a layer in the graph and choose Configure.</p>
            </div>

            {modelDraft.infraType !== 'torch' && (
              <div className="panel-section">
                <p className="section-title">Preview</p>
                <WorkerPreview layers={openLayers} />
              </div>
            )}
          </div>
        </div>
      </div>

      <ValidationPanel
        issues={modelIssues}
        title="Model validation"
        subtitle="Review model configuration issues before export."
      />
    </div>
  );
};

export default ModelLabView;

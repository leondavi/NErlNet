import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import {
  addEdge,
  applyEdgeChanges,
  Background,
  BackgroundVariant,
  Connection,
  Controls,
  Edge,
  MiniMap,
  Node,
  ReactFlow,
  ReactFlowInstance,
  useEdgesState,
  useNodesState
} from '@xyflow/react';
import {
  routerPolicyOptions,
  sourcePolicyOptions,
  sourceTypeOptions
} from '../data/mappings';
import { PlannerState } from '../data/types';
import EntityNode, { EntityNodeData } from './EntityNode';
import ValidationPanel from './ValidationPanel';
import { validatePlannerState } from '../utils/validation';

type EntityKind = 'router' | 'source' | 'client' | 'server';

const TYPE_COLORS: Record<EntityKind, string> = {
  router: '#e57a44',
  source: '#2e8a7a',
  client: '#1c4561',
  server: '#8a5a2b'
};

const GRID_COLUMNS = 4;

const SandboxView = ({
  state,
  onChange
}: {
  state: PlannerState;
  onChange: (next: PlannerState) => void;
}) => {
  const [reactFlowInstance, setReactFlowInstance] = useState<ReactFlowInstance | null>(null);
  const hasFitView = useRef(false);
  const [nodes, setNodes, onNodesChange] = useNodesState<Node>([]);
  const [edges, setEdges] = useEdgesState<Edge>([]);
  const [selectedEntityId, setSelectedEntityId] = useState<string | null>(null);
  const [contextMenu, setContextMenu] = useState<{
    x: number;
    y: number;
    flowX: number;
    flowY: number;
  } | null>(null);
  const [entityMenu, setEntityMenu] = useState<{
    x: number;
    y: number;
    entityId: string;
  } | null>(null);
  const [edgeMenu, setEdgeMenu] = useState<{
    x: number;
    y: number;
    edgeId: string;
  } | null>(null);
  const [panelState, setPanelState] = useState<{
    type: 'config' | 'worker';
    entityId: string;
  } | null>(null);
  const [linkingFrom, setLinkingFrom] = useState<string | null>(null);
  const [scanStatus, setScanStatus] = useState<'idle' | 'scanning' | 'done' | 'error'>(
    'idle'
  );
  const [scanResults, setScanResults] = useState<string[]>([]);
  const [scanError, setScanError] = useState<string | null>(null);
  const [deviceDraft, setDeviceDraft] = useState({ name: '', ipv4: '' });
  const [workerDraft, setWorkerDraft] = useState({ name: '', modelId: '' });
  const [nameDraft, setNameDraft] = useState('');
  const isEditingNameRef = useRef(false);

  const nodeTypes = useMemo(() => ({ entity: EntityNode }), []);

  const entityDescriptors = useMemo(() => {
    return [
      { id: 'mainServer', kind: 'server' as EntityKind, role: 'main' as const },
      { id: 'apiServer', kind: 'server' as EntityKind, role: 'api' as const },
      ...state.routers.map((router) => ({ id: router.name, kind: 'router' as const })),
      ...state.sources.map((source) => ({ id: source.name, kind: 'source' as const })),
      ...state.clients.map((client) => ({ id: client.name, kind: 'client' as const }))
    ].filter((entry) => entry.id);
  }, [state.clients, state.routers, state.sources]);

  const deviceByEntity = useMemo(() => {
    const map = new Map<string, { name: string; ipv4: string }>();
    state.devices.forEach((device) => {
      device.entities.forEach((entity) => {
        map.set(entity, { name: device.name, ipv4: device.ipv4 });
      });
    });
    return map;
  }, [state.devices]);

  const workersByClient = useMemo(() => {
    return new Map(state.clients.map((client) => [client.name, client.workers]));
  }, [state.clients]);

  const modelNameById = useMemo(() => {
    return new Map(state.models.map((model) => [model.id, model.name]));
  }, [state.models]);

  const entityIdSet = useMemo(() => {
    return new Set(entityDescriptors.map((entity) => entity.id));
  }, [entityDescriptors]);
  const entityIds = useMemo(() => Array.from(entityIdSet), [entityIdSet]);
  const undirectedAdjacency = useMemo(() => {
    const adjacency = new Map<string, Set<string>>();
    entityIdSet.forEach((id) => {
      adjacency.set(id, new Set());
    });
    state.connections.forEach((edge) => {
      if (!entityIdSet.has(edge.from) || !entityIdSet.has(edge.to)) {
        return;
      }
      adjacency.get(edge.from)?.add(edge.to);
      adjacency.get(edge.to)?.add(edge.from);
    });
    return adjacency;
  }, [entityIdSet, state.connections]);

  const unreachableEntities = useMemo(() => {
    const sources = state.sources.map((source) => source.name).filter(Boolean);
    if (sources.length === 0) {
      return new Set<string>();
    }
    const reachable = new Set<string>();
    const queue = [...new Set(sources)];
    while (queue.length > 0) {
      const current = queue.shift();
      if (!current || reachable.has(current)) {
        continue;
      }
      reachable.add(current);
      Array.from(undirectedAdjacency.get(current) ?? []).forEach((next) => {
        if (!reachable.has(next)) {
          queue.push(next);
        }
      });
    }
    return new Set(entityIds.filter((id) => !reachable.has(id)));
  }, [entityIds, state.sources, undirectedAdjacency]);

  const fullyConnected = useMemo(() => {
    if (entityIds.length <= 1) {
      return true;
    }
    const visited = new Set<string>();
    const queue = [entityIds[0]];
    while (queue.length > 0) {
      const current = queue.shift();
      if (!current || visited.has(current)) {
        continue;
      }
      visited.add(current);
      Array.from(undirectedAdjacency.get(current) ?? []).forEach((next) => {
        if (!visited.has(next)) {
          queue.push(next);
        }
      });
    }
    return visited.size === entityIds.length;
  }, [entityIds, undirectedAdjacency]);

  const validation = useMemo(() => validatePlannerState(state), [state]);
  const sandboxIssues = useMemo(
    () => validation.issues.filter((issue) => issue.scope.includes('sandbox')),
    [validation]
  );

  const warnings = useMemo(() => {
    const entries: string[] = [];
    if (!entityIdSet.has('mainServer') || !entityIdSet.has('apiServer')) {
      entries.push('Main Server or API Server missing from the topology.');
    }
    if (state.sources.length === 0) {
      entries.push('No sources defined yet. Add at least one source to validate connectivity.');
    }
    if (unreachableEntities.size > 0 && state.sources.length > 0) {
      entries.push(
        `Unreachable from sources: ${Array.from(unreachableEntities).join(', ')}.`
      );
    }
    if (!fullyConnected && entityDescriptors.length > 1) {
      entries.push('Topology is not fully connected. Add links so every entity can reach the rest.');
    }
    return entries;
  }, [entityDescriptors.length, entityIdSet, fullyConnected, state.sources.length, unreachableEntities]);

  useEffect(() => {
    setNodes((prevNodes) => {
      const prevMap = new Map(prevNodes.map((node) => [node.id, node]));
      return entityDescriptors.map((entity, index) => {
        const previous = prevMap.get(entity.id);
        const storedPosition = state.ui?.nodePositions?.[entity.id];
        const column = index % GRID_COLUMNS;
        const row = Math.floor(index / GRID_COLUMNS);
        const fallbackPosition = { x: 120 + column * 240, y: 120 + row * 170 };
        const position = previous?.position ?? storedPosition ?? fallbackPosition;
        const device = deviceByEntity.get(entity.id);
        const workers = entity.kind === 'client' ? workersByClient.get(entity.id) ?? [] : [];
        const data: EntityNodeData = {
          name: entity.id,
          kind: entity.kind,
          role: entity.kind === 'server' ? entity.role : undefined,
          deviceName: device?.name,
          deviceIp: device?.ipv4,
          workers,
          warning: unreachableEntities.has(entity.id) ? 'Unreachable' : undefined
        };
        return {
          id: entity.id,
          type: 'entity',
          position,
          selected: entity.id === selectedEntityId,
          data,
          className: `entity-node entity-${entity.kind}${
            unreachableEntities.has(entity.id) ? ' entity-unreachable' : ''
          }`
        };
      });
    });
  }, [
    deviceByEntity,
    entityDescriptors,
    selectedEntityId,
    setNodes,
    state.ui,
    unreachableEntities,
    workersByClient
  ]);

  useEffect(() => {
    if (nodes.length === 0) {
      hasFitView.current = false;
      return;
    }
    if (!reactFlowInstance || hasFitView.current) {
      return;
    }
    reactFlowInstance.fitView({ padding: 0.2 });
    hasFitView.current = true;
  }, [nodes.length, reactFlowInstance]);

  useEffect(() => {
    const nextEdges = new Map<string, Edge>();
    state.connections.forEach((connection) => {
      const key = connectionKey(connection.from, connection.to);
      if (!nextEdges.has(key)) {
        nextEdges.set(key, {
          id: key,
          source: connection.from,
          target: connection.to,
          type: 'smoothstep',
          animated: true,
          style: { stroke: 'var(--ink-soft)' }
        });
      }
    });
    setEdges(Array.from(nextEdges.values()));
  }, [setEdges, state.connections]);

  useEffect(() => {
    setWorkerDraft((prev) => ({
      ...prev,
      modelId: state.models[0]?.id ?? ''
    }));
  }, [state.models]);

  useEffect(() => {
    if (linkingFrom && !entityIdSet.has(linkingFrom)) {
      setLinkingFrom(null);
    }
  }, [entityIdSet, linkingFrom]);

  useEffect(() => {
    // Only reset nameDraft if user is not actively editing
    if (isEditingNameRef.current) {
      return;
    }
    const baseId = panelState?.entityId ?? selectedEntityId;
    if (baseId) {
      setNameDraft(baseId);
    } else {
      setNameDraft('');
    }
  }, [panelState?.entityId, selectedEntityId]);

  useEffect(() => {
    if (selectedEntityId && !entityIdSet.has(selectedEntityId)) {
      setSelectedEntityId(null);
    }
  }, [entityIdSet, selectedEntityId]);

  useEffect(() => {
    if (panelState && !entityIdSet.has(panelState.entityId)) {
      setPanelState(null);
    }
  }, [entityIdSet, panelState]);

  const syncConnections = useCallback(
    (edgeList: Edge[]) => {
      const nextConnections = new Map<string, { id: string; from: string; to: string; type: string }>();
      edgeList.forEach((edge) => {
        const from = String(edge.source);
        const to = String(edge.target);
        const key = connectionKey(from, to);
        if (!nextConnections.has(key)) {
          nextConnections.set(key, { id: key, from, to, type: 'data' });
        }
      });
      onChange({ ...state, connections: Array.from(nextConnections.values()) });
    },
    [onChange, state]
  );

  const handleEdgesChange = useCallback(
    (changes: Parameters<typeof applyEdgeChanges>[0]) => {
      setEdges((current) => {
        const nextEdges = applyEdgeChanges(changes, current);
        syncConnections(nextEdges);
        return nextEdges;
      });
    },
    [setEdges, syncConnections]
  );

  const handleConnect = useCallback(
    (connection: Connection) => {
      setEdges((current) => {
        if (!connection.source || !connection.target) {
          return current;
        }
        const key = connectionKey(String(connection.source), String(connection.target));
        if (current.some((edge) => connectionKey(String(edge.source), String(edge.target)) === key)) {
          return current;
        }
        const nextEdges = addEdge(
          {
            ...connection,
            id: key,
            type: 'smoothstep',
            animated: true,
            style: { stroke: 'var(--ink-soft)' }
          },
          current
        );
        syncConnections(nextEdges);
        return nextEdges;
      });
    },
    [setEdges, syncConnections]
  );

  const handleNodeClick = useCallback(
    (_: React.MouseEvent, node: Node) => {
      if (linkingFrom && node.id !== linkingFrom) {
        const connectionId = connectionKey(linkingFrom, node.id);
        if (
          !edges.some(
            (edge) => connectionKey(String(edge.source), String(edge.target)) === connectionId
          )
        ) {
          const nextEdges = [
            ...edges,
            {
              id: connectionId,
              source: linkingFrom,
              target: node.id,
              type: 'smoothstep',
              animated: true,
              style: { stroke: 'var(--ink-soft)' }
            }
          ];
          setEdges(nextEdges);
          syncConnections(nextEdges);
        }
        setLinkingFrom(null);
      }
      setSelectedEntityId(node.id);
      setContextMenu(null);
      setEntityMenu(null);
      setEdgeMenu(null);
    },
    [edges, linkingFrom, setEdges, syncConnections]
  );

  const openEntityMenu = useCallback((event: React.MouseEvent, node: Node) => {
    event.preventDefault();
    setSelectedEntityId(node.id);
    setEntityMenu({
      x: event.clientX,
      y: event.clientY,
      entityId: node.id
    });
    setContextMenu(null);
    setEdgeMenu(null);
  }, []);

  const handleNodeContextMenu = useCallback(
    (event: React.MouseEvent, node: Node) => {
      openEntityMenu(event, node);
    },
    [openEntityMenu]
  );

  const handleNodeDoubleClick = useCallback(
    (event: React.MouseEvent, node: Node) => {
      openEntityMenu(event, node);
    },
    [openEntityMenu]
  );

  const handleEdgeContextMenu = useCallback((event: React.MouseEvent, edge: Edge) => {
    event.preventDefault();
    setEdgeMenu({
      x: event.clientX,
      y: event.clientY,
      edgeId: edge.id
    });
    setEntityMenu(null);
    setContextMenu(null);
  }, []);

  const openAddMenu = useCallback(
    (event: React.MouseEvent) => {
      const flowPos = reactFlowInstance?.screenToFlowPosition({
        x: event.clientX,
        y: event.clientY
      });
      setContextMenu({
        x: event.clientX,
        y: event.clientY,
        flowX: flowPos?.x ?? event.clientX,
        flowY: flowPos?.y ?? event.clientY
      });
      setEntityMenu(null);
      setEdgeMenu(null);
      setSelectedEntityId(null);
    },
    [reactFlowInstance]
  );

  const handlePaneClick = useCallback(
    (event: React.MouseEvent) => {
      if (linkingFrom) {
        setLinkingFrom(null);
        return;
      }
      openAddMenu(event);
    },
    [linkingFrom, openAddMenu]
  );

  const handlePaneContextMenu = useCallback(
    (event: React.MouseEvent) => {
      event.preventDefault();
      if (linkingFrom) {
        setLinkingFrom(null);
        return;
      }
      openAddMenu(event);
    },
    [linkingFrom, openAddMenu]
  );

  const addEntity = (kind: EntityKind, position?: { x: number; y: number }) => {
    const existingNames = new Set(entityDescriptors.map((entity) => entity.id));
    const baseName = kind === 'router' ? 'router' : kind === 'source' ? 'source' : 'client';
    let nextName = `${baseName}-1`;
    let counter = 1;
    while (existingNames.has(nextName)) {
      counter += 1;
      nextName = `${baseName}-${counter}`;
    }
    const nextState = { ...state };
    if (kind === 'router') {
      nextState.routers = [
        ...state.routers,
        { name: nextName, port: '', policy: '0' }
      ];
    }
    if (kind === 'source') {
      nextState.sources = [
        ...state.sources,
        {
          name: nextName,
          port: '',
          frequency: state.settings.frequency,
          policy: '0',
          epochs: '1',
          type: '0'
        }
      ];
    }
    if (kind === 'client') {
      nextState.clients = [...state.clients, { name: nextName, port: '', workers: [] }];
    }

    nextState.ui = {
      nodePositions: {
        ...(state.ui?.nodePositions ?? {}),
        ...(position ? { [nextName]: position } : {})
      }
    };
    onChange(nextState);
    setSelectedEntityId(nextName);
    setContextMenu(null);
    setEntityMenu(null);
    setEdgeMenu(null);
  };

  const updateNodePositions = (updates: Record<string, { x: number; y: number }>) => {
    onChange({
      ...state,
      ui: {
        nodePositions: {
          ...(state.ui?.nodePositions ?? {}),
          ...updates
        }
      }
    });
  };

  const handleNodesChange = useCallback(
    (changes: Parameters<typeof onNodesChange>[0]) => {
      onNodesChange(changes);
      const positionUpdates: Record<string, { x: number; y: number }> = {};
      changes.forEach((change) => {
        if (change.type === 'position' && change.position) {
          positionUpdates[change.id] = change.position;
        }
      });
      if (Object.keys(positionUpdates).length > 0) {
        updateNodePositions(positionUpdates);
      }
    },
    [onNodesChange, state]
  );

  const assignEntityToDevice = (entityName: string, deviceName: string) => {
    const nextDevices = state.devices.map((device) => {
      const filtered = device.entities.filter((entity) => entity !== entityName);
      if (device.name === deviceName) {
        return { ...device, entities: [...filtered, entityName] };
      }
      return { ...device, entities: filtered };
    });
    onChange({ ...state, devices: nextDevices });
  };

  const updateDevice = (index: number, field: 'name' | 'ipv4', value: string) => {
    const nextDevices = state.devices.map((device, idx) =>
      idx === index ? { ...device, [field]: value } : device
    );
    onChange({ ...state, devices: nextDevices });
  };

  const addDevice = () => {
    const name = deviceDraft.name.trim();
    const ipv4 = deviceDraft.ipv4.trim();
    if (!name || !ipv4) {
      return;
    }
    if (state.devices.some((device) => device.name === name)) {
      return;
    }
    onChange({
      ...state,
      devices: [...state.devices, { name, ipv4, entities: [] }]
    });
    setDeviceDraft({ name: '', ipv4: '' });
  };

  const removeDevice = (name: string) => {
    onChange({
      ...state,
      devices: state.devices.filter((device) => device.name !== name)
    });
  };

  const renameEntity = (kind: EntityKind, oldName: string, nextName: string) => {
    const trimmed = nextName.trim();
    if (!trimmed || trimmed === oldName) {
      return;
    }
    if (trimmed === 'mainServer' || trimmed === 'apiServer') {
      return;
    }
    if (entityIdSet.has(trimmed)) {
      return;
    }

    const replaceName = (value: string) => (value === oldName ? trimmed : value);

    const nextConnections = state.connections.map((edge) => {
      const from = replaceName(edge.from);
      const to = replaceName(edge.to);
      return { ...edge, from, to, id: connectionKey(from, to) };
    });

    const nextDevices = state.devices.map((device) => ({
      ...device,
      entities: device.entities.map(replaceName)
    }));

    const nextExperiment =
      kind === 'source'
        ? {
            ...state.experimentFlow,
            phases: state.experimentFlow.phases.map((phase) => ({
              ...phase,
              sourcePieces: phase.sourcePieces.map((piece) => ({
                ...piece,
                sourceName: replaceName(piece.sourceName)
              }))
            }))
          }
        : state.experimentFlow;

    const nextUiPositions = { ...(state.ui?.nodePositions ?? {}) };
    if (nextUiPositions[oldName]) {
      nextUiPositions[trimmed] = nextUiPositions[oldName];
      delete nextUiPositions[oldName];
    }

    onChange({
      ...state,
      routers:
        kind === 'router'
          ? state.routers.map((router) =>
              router.name === oldName ? { ...router, name: trimmed } : router
            )
          : state.routers,
      sources:
        kind === 'source'
          ? state.sources.map((source) =>
              source.name === oldName ? { ...source, name: trimmed } : source
            )
          : state.sources,
      clients:
        kind === 'client'
          ? state.clients.map((client) =>
              client.name === oldName ? { ...client, name: trimmed } : client
            )
          : state.clients,
      connections: nextConnections,
      devices: nextDevices,
      experimentFlow: nextExperiment,
      ui: { nodePositions: nextUiPositions }
    });
    setSelectedEntityId(trimmed);
  };

  const removeEntity = (kind: EntityKind, name: string) => {
    const nextConnections = state.connections.filter(
      (edge) => edge.from !== name && edge.to !== name
    );
    const nextDevices = state.devices.map((device) => ({
      ...device,
      entities: device.entities.filter((entity) => entity !== name)
    }));
    const nextUiPositions = { ...(state.ui?.nodePositions ?? {}) };
    delete nextUiPositions[name];

    let nextWorkers = state.workers;
    let nextClients = state.clients;
    let nextExperiment = state.experimentFlow;

    if (kind === 'client') {
      const removedWorkers = state.clients.find((client) => client.name === name)?.workers ?? [];
      nextWorkers = state.workers.filter((worker) => !removedWorkers.includes(worker.name));
      nextClients = state.clients.filter((client) => client.name !== name);
      nextExperiment = {
        ...state.experimentFlow,
        phases: state.experimentFlow.phases.map((phase) => ({
          ...phase,
          sourcePieces: phase.sourcePieces.map((piece) => ({
            ...piece,
            workers: piece.workers.filter((worker) => !removedWorkers.includes(worker))
          }))
        }))
      };
    }

    if (kind === 'source') {
      nextExperiment = {
        ...state.experimentFlow,
        phases: state.experimentFlow.phases.map((phase) => ({
          ...phase,
          sourcePieces: phase.sourcePieces.filter((piece) => piece.sourceName !== name)
        }))
      };
    }

    onChange({
      ...state,
      routers: kind === 'router' ? state.routers.filter((router) => router.name !== name) : state.routers,
      sources: kind === 'source' ? state.sources.filter((source) => source.name !== name) : state.sources,
      clients: kind === 'client' ? nextClients : state.clients,
      workers: nextWorkers,
      connections: nextConnections,
      devices: nextDevices,
      experimentFlow: nextExperiment,
      ui: { nodePositions: nextUiPositions }
    });
    setSelectedEntityId(null);
  };

  const resolveEntity = useCallback((entityId: string | null) => {
    if (!entityId) {
      return null;
    }
    if (entityId === 'mainServer') {
      return { kind: 'server' as const, role: 'main' as const, name: 'mainServer' };
    }
    if (entityId === 'apiServer') {
      return { kind: 'server' as const, role: 'api' as const, name: 'apiServer' };
    }
    const router = state.routers.find((entry) => entry.name === entityId);
    if (router) {
      return { kind: 'router' as const, data: router, name: router.name };
    }
    const source = state.sources.find((entry) => entry.name === entityId);
    if (source) {
      return { kind: 'source' as const, data: source, name: source.name };
    }
    const client = state.clients.find((entry) => entry.name === entityId);
    if (client) {
      return { kind: 'client' as const, data: client, name: client.name };
    }
    return null;
  }, [state.routers, state.sources, state.clients]);

  const selectedEntity = useMemo(
    () => resolveEntity(selectedEntityId),
    [selectedEntityId, resolveEntity]
  );

  const panelEntity = useMemo(
    () => resolveEntity(panelState?.entityId ?? null),
    [panelState?.entityId, resolveEntity]
  );

  const menuEntity = useMemo(
    () => resolveEntity(entityMenu?.entityId ?? null),
    [entityMenu?.entityId, resolveEntity]
  );

  const panelDeviceName = panelEntity
    ? state.devices.find((device) => device.entities.includes(panelEntity.name))?.name ?? ''
    : '';

  const openConfigPanel = (entityId: string) => {
    setPanelState({ type: 'config', entityId });
    setEntityMenu(null);
    setEdgeMenu(null);
    setContextMenu(null);
    setSelectedEntityId(entityId);
  };

  const openWorkerPanel = (entityId: string) => {
    setPanelState({ type: 'worker', entityId });
    setEntityMenu(null);
    setEdgeMenu(null);
    setContextMenu(null);
    setSelectedEntityId(entityId);
    setWorkerDraft({
      name: '',
      modelId: state.models[0]?.id ?? ''
    });
  };

  const closePanel = () => {
    setPanelState(null);
  };

  const applyNameChange = () => {
    if (panelEntity && panelEntity.kind !== 'server') {
      const nextName = nameDraft.trim();
      if (!nextName || nextName === panelEntity.name) {
        return;
      }
      renameEntity(panelEntity.kind, panelEntity.name, nextName);
      setPanelState((current) =>
        current ? { ...current, entityId: nextName } : current
      );
    }
  };

  const addWorkerToClient = (clientName: string) => {
    const name = workerDraft.name.trim();
    if (!name || !workerDraft.modelId) {
      return;
    }
    if (state.workers.some((worker) => worker.name === name)) {
      return;
    }
    const nextWorkers = [...state.workers, { name, modelId: workerDraft.modelId }];
    const nextClients = state.clients.map((client) =>
      client.name === clientName
        ? { ...client, workers: [...client.workers, name] }
        : client
    );
    onChange({ ...state, workers: nextWorkers, clients: nextClients });
    setWorkerDraft({ name: '', modelId: workerDraft.modelId });
  };

  const updateWorkerModel = (workerName: string, modelId: string) => {
    const nextWorkers = state.workers.map((worker) =>
      worker.name === workerName ? { ...worker, modelId } : worker
    );
    onChange({ ...state, workers: nextWorkers });
  };

  const removeWorker = (workerName: string) => {
    const nextWorkers = state.workers.filter((worker) => worker.name !== workerName);
    const nextClients = state.clients.map((client) => ({
      ...client,
      workers: client.workers.filter((worker) => worker !== workerName)
    }));
    const nextExperiment = {
      ...state.experimentFlow,
      phases: state.experimentFlow.phases.map((phase) => ({
        ...phase,
        sourcePieces: phase.sourcePieces.map((piece) => ({
          ...piece,
          workers: piece.workers.filter((worker) => worker !== workerName)
        }))
      }))
    };
    onChange({ ...state, workers: nextWorkers, clients: nextClients, experimentFlow: nextExperiment });
  };

  const removeConnection = (id: string) => {
    const nextEdges = edges.filter((edge) => edge.id !== id);
    setEdges(nextEdges);
    syncConnections(nextEdges);
  };

  const handleScan = async () => {
    setScanStatus('scanning');
    setScanError(null);
    try {
      const response = await fetch('/api/scan');
      if (!response.ok) {
        throw new Error(`Scan failed (${response.status})`);
      }
      const data = (await response.json()) as { ips?: string[] };
      setScanResults(data.ips ?? []);
      setScanStatus('done');
    } catch (error) {
      setScanStatus('error');
      setScanError(error instanceof Error ? error.message : 'Scan failed');
    }
  };

  const addDeviceFromScan = (ip: string, assignTo?: string) => {
    const baseName = 'device';
    let counter = state.devices.length + 1;
    let name = `${baseName}-${counter}`;
    while (state.devices.some((device) => device.name === name)) {
      counter += 1;
      name = `${baseName}-${counter}`;
    }
    const nextDevices = assignTo
      ? state.devices.map((device) => ({
          ...device,
          entities: device.entities.filter((entity) => entity !== assignTo)
        }))
      : state.devices;
    onChange({
      ...state,
      devices: [...nextDevices, { name, ipv4: ip, entities: assignTo ? [assignTo] : [] }]
    });
  };

  return (
    <div className="view sandbox-view">
      <div className="hero compact">
        <div>
          <p className="eyebrow">Topology Sandbox</p>
          <h1>Drop entities, wire links, and validate reachability in one space.</h1>
          <p>Click the canvas to add entities. Right-click or double-click a node to configure it, add links, or manage workers.</p>
        </div>
        <div className="hero-badge">
          <div>
            <span>Entities</span>
            <strong>{entityDescriptors.length}</strong>
          </div>
          <div>
            <span>Connections</span>
            <strong>{edges.length}</strong>
          </div>
        </div>
      </div>

      <div className="sandbox-layout">
        <div className="canvas-panel">
          {warnings.length > 0 && (
            <div className="warning-banner">
              {warnings.map((warning) => (
                <p key={warning}>{warning}</p>
              ))}
            </div>
          )}
          {linkingFrom && (
            <div className="linking-banner">
              <p>Linking from {linkingFrom}. Click another entity to complete the link.</p>
              <button type="button" className="ghost" onClick={() => setLinkingFrom(null)}>
                Cancel link
              </button>
            </div>
          )}
          <ReactFlow
            nodes={nodes}
            edges={edges}
            nodeTypes={nodeTypes}
            onInit={setReactFlowInstance}
            onNodesChange={handleNodesChange}
            onEdgesChange={handleEdgesChange}
            onNodeClick={handleNodeClick}
            onNodeContextMenu={handleNodeContextMenu}
            onNodeDoubleClick={handleNodeDoubleClick}
            onEdgeContextMenu={handleEdgeContextMenu}
            onConnect={handleConnect}
            onPaneClick={handlePaneClick}
            onPaneContextMenu={handlePaneContextMenu}
            zoomOnDoubleClick={false}
            fitView
          >
            <Background color="#d1c8b8" gap={18} variant={BackgroundVariant.Dots} />
            <MiniMap
              nodeColor={(node) => {
                if (node.className?.includes('entity-router')) return TYPE_COLORS.router;
                if (node.className?.includes('entity-source')) return TYPE_COLORS.source;
                if (node.className?.includes('entity-client')) return TYPE_COLORS.client;
                return TYPE_COLORS.server;
              }}
            />
            <Controls />
          </ReactFlow>
          {contextMenu && (
            <div className="context-menu" style={{ top: contextMenu.y, left: contextMenu.x }}>
              <button type="button" onClick={() => addEntity('router', { x: contextMenu.flowX, y: contextMenu.flowY })}>
                Add Router
              </button>
              <button type="button" onClick={() => addEntity('source', { x: contextMenu.flowX, y: contextMenu.flowY })}>
                Add Source
              </button>
              <button type="button" onClick={() => addEntity('client', { x: contextMenu.flowX, y: contextMenu.flowY })}>
                Add Client
              </button>
            </div>
          )}
          {entityMenu && menuEntity && (
            <div className="context-menu" style={{ top: entityMenu.y, left: entityMenu.x }}>
              <button type="button" onClick={() => openConfigPanel(entityMenu.entityId)}>
                Configure
              </button>
              <button
                type="button"
                onClick={() => {
                  setLinkingFrom(entityMenu.entityId);
                  setEntityMenu(null);
                }}
              >
                Add Link
              </button>
              {menuEntity.kind === 'client' && (
                <button type="button" onClick={() => openWorkerPanel(entityMenu.entityId)}>
                  Add Worker
                </button>
              )}
              {menuEntity.kind !== 'server' && (
                <button
                  type="button"
                  className="danger"
                  onClick={() => {
                    removeEntity(menuEntity.kind, menuEntity.name);
                    setEntityMenu(null);
                  }}
                >
                  Remove {menuEntity.kind}
                </button>
              )}
            </div>
          )}
          {edgeMenu && (
            <div className="context-menu" style={{ top: edgeMenu.y, left: edgeMenu.x }}>
              <button
                type="button"
                className="danger"
                onClick={() => {
                  removeConnection(edgeMenu.edgeId);
                  setEdgeMenu(null);
                }}
              >
                Remove Link
              </button>
            </div>
          )}
          {panelState && panelEntity && (
            <div className="floating-panel">
              <div className="floating-panel-header">
                <div>
                  <p className="panel-title">
                    {panelState.type === 'worker'
                      ? `Workers - ${panelEntity.name}`
                      : `Configure ${panelEntity.kind}`}
                  </p>
                  <p className="panel-subtitle">{panelEntity.name}</p>
                </div>
                <button type="button" className="ghost" onClick={closePanel}>
                  Close
                </button>
              </div>
              <div className="floating-panel-body">
                {panelState.type === 'config' && (
                  <>
                    {panelEntity.kind === 'server' && (
                      <div className="inspector-form">
                        <label className="field">
                          <span>Port</span>
                          <input
                            type="text"
                            value={
                              panelEntity.role === 'main'
                                ? state.servers.mainServer.port
                                : state.servers.apiServer.port
                            }
                            onChange={(event) =>
                              onChange({
                                ...state,
                                servers: {
                                  ...state.servers,
                                  [panelEntity.role === 'main' ? 'mainServer' : 'apiServer']: {
                                    ...state.servers[
                                      panelEntity.role === 'main' ? 'mainServer' : 'apiServer'
                                    ],
                                    port: event.target.value
                                  }
                                }
                              })
                            }
                          />
                        </label>
                        <label className="field">
                          <span>Args</span>
                          <input
                            type="text"
                            value={
                              panelEntity.role === 'main'
                                ? state.servers.mainServer.args
                                : state.servers.apiServer.args
                            }
                            onChange={(event) =>
                              onChange({
                                ...state,
                                servers: {
                                  ...state.servers,
                                  [panelEntity.role === 'main' ? 'mainServer' : 'apiServer']: {
                                    ...state.servers[
                                      panelEntity.role === 'main' ? 'mainServer' : 'apiServer'
                                    ],
                                    args: event.target.value
                                  }
                                }
                              })
                            }
                          />
                        </label>
                      </div>
                    )}

                    {panelEntity.kind === 'router' && (
                      <div className="inspector-form">
                        <label className="field">
                          <span>Name</span>
                          <input
                            type="text"
                            value={nameDraft}
                            onChange={(event) => setNameDraft(event.target.value)}
                            onFocus={() => { isEditingNameRef.current = true; }}
                            onBlur={() => { isEditingNameRef.current = false; }}
                            onKeyDown={(event) => {
                              if (event.key === 'Enter') {
                                applyNameChange();
                              }
                            }}
                          />
                        </label>
                        <label className="field">
                          <span>Port</span>
                          <input
                            type="text"
                            value={panelEntity.data.port}
                            onChange={(event) =>
                              onChange({
                                ...state,
                                routers: state.routers.map((router) =>
                                  router.name === panelEntity.name
                                    ? { ...router, port: event.target.value }
                                    : router
                                )
                              })
                            }
                          />
                        </label>
                        <label className="field">
                          <span>Policy</span>
                          <select
                            value={panelEntity.data.policy}
                            onChange={(event) =>
                              onChange({
                                ...state,
                                routers: state.routers.map((router) =>
                                  router.name === panelEntity.name
                                    ? { ...router, policy: event.target.value }
                                    : router
                                )
                              })
                            }
                          >
                            {routerPolicyOptions.map((option) => (
                              <option key={option.value} value={option.value}>
                                {option.label}
                              </option>
                            ))}
                          </select>
                        </label>
                      </div>
                    )}

                    {panelEntity.kind === 'source' && (
                      <div className="inspector-form">
                        <label className="field">
                          <span>Name</span>
                          <input
                            type="text"
                            value={nameDraft}
                            onChange={(event) => setNameDraft(event.target.value)}
                            onFocus={() => { isEditingNameRef.current = true; }}
                            onBlur={() => { isEditingNameRef.current = false; }}
                            onKeyDown={(event) => {
                              if (event.key === 'Enter') {
                                applyNameChange();
                              }
                            }}
                          />
                        </label>
                        <label className="field">
                          <span>Port</span>
                          <input
                            type="text"
                            value={panelEntity.data.port}
                            onChange={(event) =>
                              onChange({
                                ...state,
                                sources: state.sources.map((source) =>
                                  source.name === panelEntity.name
                                    ? { ...source, port: event.target.value }
                                    : source
                                )
                              })
                            }
                          />
                        </label>
                        <div className="inline-fields">
                          <label className="field">
                            <span>Frequency</span>
                            <input
                              type="text"
                              value={panelEntity.data.frequency}
                              onChange={(event) =>
                                onChange({
                                  ...state,
                                  sources: state.sources.map((source) =>
                                    source.name === panelEntity.name
                                      ? { ...source, frequency: event.target.value }
                                      : source
                                  )
                                })
                              }
                            />
                          </label>
                          <label className="field">
                            <span>Epochs</span>
                            <input
                              type="text"
                              value={panelEntity.data.epochs}
                              onChange={(event) =>
                                onChange({
                                  ...state,
                                  sources: state.sources.map((source) =>
                                    source.name === panelEntity.name
                                      ? { ...source, epochs: event.target.value }
                                      : source
                                  )
                                })
                              }
                            />
                          </label>
                        </div>
                        <div className="inline-fields">
                          <label className="field">
                            <span>Policy</span>
                            <select
                              value={panelEntity.data.policy}
                              onChange={(event) =>
                                onChange({
                                  ...state,
                                  sources: state.sources.map((source) =>
                                    source.name === panelEntity.name
                                      ? { ...source, policy: event.target.value }
                                      : source
                                  )
                                })
                              }
                            >
                              {sourcePolicyOptions.map((option) => (
                                <option key={option.value} value={option.value}>
                                  {option.label}
                                </option>
                              ))}
                            </select>
                          </label>
                          <label className="field">
                            <span>Source Type</span>
                            <select
                              value={panelEntity.data.type}
                              onChange={(event) =>
                                onChange({
                                  ...state,
                                  sources: state.sources.map((source) =>
                                    source.name === panelEntity.name
                                      ? { ...source, type: event.target.value }
                                      : source
                                  )
                                })
                              }
                            >
                              {sourceTypeOptions.map((option) => (
                                <option key={option.value} value={option.value}>
                                  {option.label}
                                </option>
                              ))}
                            </select>
                          </label>
        </div>
        <ValidationPanel
          issues={sandboxIssues}
          title="Topology validation"
          subtitle="Resolve blocking topology and device issues before export."
        />
      </div>
                    )}

                    {panelEntity.kind === 'client' && (
                      <div className="inspector-form">
                        <label className="field">
                          <span>Name</span>
                          <input
                            type="text"
                            value={nameDraft}
                            onChange={(event) => setNameDraft(event.target.value)}
                            onFocus={() => { isEditingNameRef.current = true; }}
                            onBlur={() => { isEditingNameRef.current = false; }}
                            onKeyDown={(event) => {
                              if (event.key === 'Enter') {
                                applyNameChange();
                              }
                            }}
                          />
                        </label>
                        <label className="field">
                          <span>Port</span>
                          <input
                            type="text"
                            value={panelEntity.data.port}
                            onChange={(event) =>
                              onChange({
                                ...state,
                                clients: state.clients.map((client) =>
                                  client.name === panelEntity.name
                                    ? { ...client, port: event.target.value }
                                    : client
                                )
                              })
                            }
                          />
                        </label>
                        <button type="button" className="ghost" onClick={() => openWorkerPanel(panelEntity.name)}>
                          Manage Workers
                        </button>
                      </div>
                    )}

                    <div className="panel-section">
                      <p className="section-title">Device Assignment</p>
                      <label className="field">
                        <span>Device</span>
                        <select
                          value={panelDeviceName}
                          onChange={(event) =>
                            assignEntityToDevice(panelEntity.name, event.target.value)
                          }
                        >
                          <option value="">Unassigned</option>
                          {state.devices.map((device) => (
                            <option key={device.name} value={device.name}>
                              {device.name} - {device.ipv4}
                            </option>
                          ))}
                        </select>
                      </label>
                    </div>
                  </>
                )}

                {panelState.type === 'worker' && panelEntity.kind === 'client' && (
                  <div className="inspector-form">
                    <div className="inline-fields">
                      <label className="field">
                        <span>Name</span>
                        <input
                          type="text"
                          value={workerDraft.name}
                          onChange={(event) =>
                            setWorkerDraft({ ...workerDraft, name: event.target.value })
                          }
                        />
                      </label>
                      <label className="field">
                        <span>Model</span>
                        <select
                          value={workerDraft.modelId}
                          onChange={(event) =>
                            setWorkerDraft({ ...workerDraft, modelId: event.target.value })
                          }
                        >
                          <option value="">Select model</option>
                          {state.models.map((model) => (
                            <option key={model.id} value={model.id}>
                              {model.name}
                            </option>
                          ))}
                        </select>
                      </label>
                    </div>
                    <button
                      type="button"
                      className="ghost"
                      onClick={() => addWorkerToClient(panelEntity.name)}
                    >
                      Add Worker
                    </button>
                    <div className="library-list compact">
                      {panelEntity.data.workers.map((workerName) => (
                        <div key={workerName} className="library-item">
                          <div>
                            <strong>{workerName}</strong>
                            <span className="muted">
                              {modelNameById.get(
                                state.workers.find((worker) => worker.name === workerName)?.modelId ??
                                  ''
                              ) ?? 'Unlinked'}
                            </span>
                          </div>
                          <select
                            value={
                              state.workers.find((worker) => worker.name === workerName)?.modelId ?? ''
                            }
                            onChange={(event) => updateWorkerModel(workerName, event.target.value)}
                          >
                            <option value="">Select model</option>
                            {state.models.map((model) => (
                              <option key={model.id} value={model.id}>
                                {model.name}
                              </option>
                            ))}
                          </select>
                          <button
                            type="button"
                            className="ghost danger"
                            onClick={() => removeWorker(workerName)}
                          >
                            Remove
                          </button>
                        </div>
                      ))}
                      {panelEntity.data.workers.length === 0 && (
                        <p className="muted">No workers yet.</p>
                      )}
                    </div>
                  </div>
                )}
              </div>
              {panelState.type === 'config' && panelEntity.kind !== 'server' && (
                <div className="panel-actions align-right">
                  <button type="button" className="ghost" onClick={applyNameChange}>
                    Save Name
                  </button>
                  <button type="button" className="primary" onClick={closePanel}>
                    Done
                  </button>
                </div>
              )}
              {panelState.type === 'config' && panelEntity.kind === 'server' && (
                <div className="panel-actions align-right">
                  <button type="button" className="primary" onClick={closePanel}>
                    Done
                  </button>
                </div>
              )}
              {panelState.type === 'worker' && (
                <div className="panel-actions align-right">
                  <button type="button" className="primary" onClick={closePanel}>
                    Done
                  </button>
                </div>
              )}
            </div>
          )}
        </div>

        <aside className="inspector">
          <div className="panel">
            <div className="panel-header">
              <div>
                <p className="panel-title">Cluster Settings</p>
                <p className="panel-subtitle">Global defaults for sources and batch handling.</p>
              </div>
            </div>
            <div className="inline-fields">
              <label className="field">
                <span>Frequency</span>
                <input
                  type="text"
                  value={state.settings.frequency}
                  onChange={(event) =>
                    onChange({
                      ...state,
                      settings: { ...state.settings, frequency: event.target.value }
                    })
                  }
                />
              </label>
              <label className="field">
                <span>Batch Size</span>
                <input
                  type="text"
                  value={state.settings.batchSize}
                  onChange={(event) =>
                    onChange({
                      ...state,
                      settings: { ...state.settings, batchSize: event.target.value }
                    })
                  }
                />
              </label>
            </div>
          </div>

          <div className="panel">
            <div className="panel-header">
              <div>
                <p className="panel-title">Devices</p>
                <p className="panel-subtitle">Scan reachable IPs and assign entities.</p>
              </div>
            </div>
            <div className="inline-fields">
              <label className="field">
                <span>Name</span>
                <input
                  type="text"
                  value={deviceDraft.name}
                  onChange={(event) => setDeviceDraft({ ...deviceDraft, name: event.target.value })}
                />
              </label>
              <label className="field">
                <span>IPv4</span>
                <input
                  type="text"
                  value={deviceDraft.ipv4}
                  onChange={(event) => setDeviceDraft({ ...deviceDraft, ipv4: event.target.value })}
                />
              </label>
            </div>
            <button type="button" className="ghost" onClick={addDevice}>
              Add Device
            </button>

            <div className="library-list compact">
              {state.devices.map((device, index) => (
                <div key={device.name} className="library-item device-item">
                  <div>
                    <strong>{device.name}</strong>
                    <span className="muted">{device.ipv4}</span>
                  </div>
                  <div className="device-fields">
                    <input
                      type="text"
                      value={device.name}
                      onChange={(event) => updateDevice(index, 'name', event.target.value)}
                    />
                    <input
                      type="text"
                      value={device.ipv4}
                      onChange={(event) => updateDevice(index, 'ipv4', event.target.value)}
                    />
                  </div>
                  <button
                    type="button"
                    className="ghost danger"
                    onClick={() => removeDevice(device.name)}
                  >
                    Remove
                  </button>
                </div>
              ))}
              {state.devices.length === 0 && <p className="muted">No devices yet.</p>}
            </div>

            <div className="panel-section">
              <div className="panel-actions">
                <button
                  type="button"
                  className="ghost"
                  onClick={handleScan}
                  disabled={scanStatus === 'scanning'}
                >
                  {scanStatus === 'scanning' ? 'Scanning...' : 'Scan Network'}
                </button>
                <span className="muted">
                  {scanStatus === 'done' && `${scanResults.length} hosts found`}
                </span>
              </div>
              {scanError && <p className="muted">{scanError}</p>}
              {scanResults.length > 0 && (
                <div className="scan-grid">
                  {scanResults.map((ip) => (
                    <div key={ip} className="scan-item">
                      <span>{ip}</span>
                      <div className="scan-actions">
                        <button type="button" className="ghost" onClick={() => addDeviceFromScan(ip)}>
                          Add Device
                        </button>
                        {selectedEntity && (
                          <button
                            type="button"
                            className="ghost"
                            onClick={() => addDeviceFromScan(ip, selectedEntity.name)}
                          >
                            Add & Assign
                          </button>
                        )}
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>
          </div>
        </aside>
      </div>
    </div>
  );
};

export default SandboxView;

const connectionKey = (from: string, to: string) => [from, to].sort().join('--');

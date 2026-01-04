import { useCallback, useEffect, useMemo, useState } from 'react';
import {
  addEdge,
  Background,
  applyEdgeChanges,
  Connection,
  Controls,
  Edge,
  MiniMap,
  Node,
  ReactFlow,
  useEdgesState,
  useNodesState
} from '@xyflow/react';
import { PlannerState, ConnectionEdge } from '../data/types';

const TYPE_COLORS: Record<string, string> = {
  router: '#e57a44',
  source: '#2e8a7a',
  client: '#1c4561',
  server: '#8a5a2b'
};

const ConnectionMapView = ({
  state,
  onChange
}: {
  state: PlannerState;
  onChange: (next: PlannerState) => void;
}) => {
  const entityNames = useMemo(() => {
    return [
      'mainServer',
      'apiServer',
      ...state.routers.map((router) => router.name),
      ...state.sources.map((source) => source.name),
      ...state.clients.map((client) => client.name)
    ].filter(Boolean);
  }, [state.routers, state.sources, state.clients]);

  const entityType = (name: string) => {
    if (name === 'mainServer' || name === 'apiServer') {
      return 'server';
    }
    if (state.routers.some((router) => router.name === name)) {
      return 'router';
    }
    if (state.sources.some((source) => source.name === name)) {
      return 'source';
    }
    if (state.clients.some((client) => client.name === name)) {
      return 'client';
    }
    return 'router';
  };

  const [nodes, setNodes, onNodesChange] = useNodesState<Node>([]);
  const [edges, setEdges] = useEdgesState<Edge>([]);
  const [connectionDraft, setConnectionDraft] = useState({ from: '', to: '' });

  useEffect(() => {
    setNodes((prevNodes) => {
      const prevMap = new Map(prevNodes.map((node) => [node.id, node]));
      const nextNodes: Node[] = [];
      entityNames.forEach((name, index) => {
        const existing = prevMap.get(name);
        if (existing) {
          nextNodes.push({ ...existing, data: { label: name } });
          return;
        }
        const groupIndex = index % 4;
        const row = Math.floor(index / 4);
        const x = 120 + groupIndex * 220;
        const y = 80 + row * 160;
        const type = entityType(name);
        nextNodes.push({
          id: name,
          position: { x, y },
          data: { label: name },
          style: {
            borderRadius: '18px',
            padding: '8px 14px',
            border: '1px solid var(--stroke)',
            background: 'var(--card)',
            color: 'var(--ink)',
            boxShadow: 'var(--shadow-soft)'
          },
          className: `node-${type}`
        });
      });
      return nextNodes;
    });
  }, [entityNames]);

  useEffect(() => {
    const edgeList = state.connections.map((connection) => ({
      id: connection.id,
      source: connection.from,
      target: connection.to,
      type: 'smoothstep',
      animated: true,
      style: { stroke: 'var(--ink-soft)' }
    }));
    setEdges(edgeList);
  }, [state.connections, setEdges]);

  const syncConnections = useCallback(
    (edgeList: Edge[]) => {
      const nextConnections: ConnectionEdge[] = edgeList.map((edge) => ({
        id: edge.id,
        from: String(edge.source),
        to: String(edge.target),
        type: 'data'
      }));
      onChange({ ...state, connections: nextConnections });
    },
    [onChange, state]
  );

  const handleEdgesChange = useCallback(
    (changes: Parameters<typeof applyEdgeChanges>[0]) => {
      setEdges((eds) => {
        const nextEdges = applyEdgeChanges(changes, eds);
        syncConnections(nextEdges);
        return nextEdges;
      });
    },
    [setEdges, syncConnections]
  );

  const onConnect = useCallback(
    (connection: Connection) => {
      setEdges((eds) => {
        const nextEdges = addEdge({
          ...connection,
          type: 'smoothstep',
          animated: true,
          style: { stroke: 'var(--ink-soft)' }
        }, eds);
        syncConnections(nextEdges);
        return nextEdges;
      });
    },
    [setEdges, syncConnections]
  );

  const addConnection = () => {
    if (!connectionDraft.from || !connectionDraft.to) {
      return;
    }
    const id = `${connectionDraft.from}-${connectionDraft.to}`;
    const next = {
      id,
      source: connectionDraft.from,
      target: connectionDraft.to,
      type: 'smoothstep',
      animated: true,
      style: { stroke: 'var(--ink-soft)' }
    };
    const nextEdges = [...edges, next];
    setEdges(nextEdges);
    syncConnections(nextEdges);
    setConnectionDraft({ from: '', to: '' });
  };

  const removeConnection = (id: string) => {
    const nextEdges = edges.filter((edge) => edge.id !== id);
    setEdges(nextEdges);
    syncConnections(nextEdges);
  };

  return (
    <div className="view">
      <div className="hero compact">
        <div>
          <p className="eyebrow">Topology Builder</p>
          <h1>Visualize entity topology and connection routes.</h1>
          <p>Drag nodes, connect routers to sources/clients, and validate names on canvas.</p>
        </div>
        <div className="hero-badge">
          <div>
            <span>Entities</span>
            <strong>{entityNames.length}</strong>
          </div>
          <div>
            <span>Connections</span>
            <strong>{edges.length}</strong>
          </div>
        </div>
      </div>

      <div className="grid two">
        <div className="panel map-panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Connection Map</p>
              <p className="panel-subtitle">Build routes between routers, sources, clients, and servers.</p>
            </div>
          </div>
          <div className="map-frame">
            <ReactFlow
              nodes={nodes}
              edges={edges}
              onNodesChange={onNodesChange}
              onEdgesChange={handleEdgesChange}
              onConnect={onConnect}
              fitView
            >
              <Background color="#d1c8b8" gap={18} />
              <MiniMap
                nodeColor={(node) => {
                  if (node.className?.includes('router')) return TYPE_COLORS.router;
                  if (node.className?.includes('source')) return TYPE_COLORS.source;
                  if (node.className?.includes('client')) return TYPE_COLORS.client;
                  return TYPE_COLORS.server;
                }}
              />
              <Controls />
            </ReactFlow>
          </div>
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Connections</p>
              <p className="panel-subtitle">Manual edits and policy-aware routing targets.</p>
            </div>
          </div>
          <div className="inline-fields">
            <label className="field">
              <span>From</span>
              <select
                value={connectionDraft.from}
                onChange={(event) => setConnectionDraft({ ...connectionDraft, from: event.target.value })}
              >
                <option value="">Select entity</option>
                {entityNames.map((name) => (
                  <option key={name} value={name}>
                    {name}
                  </option>
                ))}
              </select>
            </label>
            <label className="field">
              <span>To</span>
              <select
                value={connectionDraft.to}
                onChange={(event) => setConnectionDraft({ ...connectionDraft, to: event.target.value })}
              >
                <option value="">Select entity</option>
                {entityNames.map((name) => (
                  <option key={name} value={name}>
                    {name}
                  </option>
                ))}
              </select>
            </label>
          </div>
          <button className="ghost" type="button" onClick={addConnection}>
            Add Connection
          </button>

          <div className="library-list compact">
            {edges.map((edge) => (
              <div key={edge.id} className="library-item">
                <div>
                  <strong>{edge.source}</strong> {' -> '} <strong>{edge.target}</strong>
                </div>
                <button className="ghost danger" type="button" onClick={() => removeConnection(edge.id)}>
                  Remove
                </button>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ConnectionMapView;

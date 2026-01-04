import { useEffect, useMemo, useState } from 'react';
import {
  infraTypeOptions,
  routerPolicyOptions,
  sourcePolicyOptions,
  sourceTypeOptions
} from '../data/mappings';
import { createOpenNNModel } from '../data/defaults';
import { PlannerState, WorkerModel } from '../data/types';
import ModelBuilder from './ModelBuilder';

const PlannerView = ({
  state,
  onChange
}: {
  state: PlannerState;
  onChange: (next: PlannerState) => void;
}) => {
  const [selectedModelId, setSelectedModelId] = useState<string | null>(
    state.models[0]?.id ?? null
  );
  const [modelDraft, setModelDraft] = useState<WorkerModel>(
    state.models[0] ?? createOpenNNModel()
  );

  const [workerDraftName, setWorkerDraftName] = useState('');
  const [workerDraftModelId, setWorkerDraftModelId] = useState(
    state.models[0]?.id ?? modelDraft.id
  );

  const [clientDraft, setClientDraft] = useState({
    name: '',
    port: '',
    workers: [] as string[]
  });

  const [sourceDraft, setSourceDraft] = useState({
    name: '',
    port: '',
    frequency: state.settings.frequency,
    policy: '0',
    epochs: '1',
    type: '0'
  });

  const [routerDraft, setRouterDraft] = useState({
    name: '',
    port: '',
    policy: '0'
  });

  const [deviceDraft, setDeviceDraft] = useState({
    name: '',
    ipv4: '',
    entities: [] as string[]
  });

  const infraSummary = useMemo(() => {
    const used = new Set(state.models.map((model) => model.infraType));
    if (used.size === 0) {
      return 'None';
    }
    const labels = Array.from(used)
      .map((value) => infraTypeOptions.find((option) => option.value === value)?.label)
      .filter((label): label is string => Boolean(label));
    return labels.length > 0 ? labels.join(' + ') : 'None';
  }, [state.models]);

  useEffect(() => {
    if (selectedModelId) {
      const selected = state.models.find((model) => model.id === selectedModelId);
      if (selected) {
        setModelDraft(selected);
        setWorkerDraftModelId(selected.id);
      }
    }
  }, [selectedModelId, state.models]);

  const assignedWorkers = useMemo(() => {
    return new Set(state.clients.flatMap((client) => client.workers));
  }, [state.clients]);

  const availableWorkers = useMemo(() => {
    return state.workers.filter((worker) => !assignedWorkers.has(worker.name));
  }, [state.workers, assignedWorkers]);

  const entitiesList = useMemo(() => {
    return [
      'mainServer',
      'apiServer',
      ...state.routers.map((router) => router.name),
      ...state.sources.map((source) => source.name),
      ...state.clients.map((client) => client.name)
    ].filter(Boolean);
  }, [state.routers, state.sources, state.clients]);

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
    }
  };

  const addWorker = () => {
    if (!workerDraftName.trim() || !workerDraftModelId) {
      return;
    }
    if (state.workers.some((worker) => worker.name === workerDraftName.trim())) {
      return;
    }
    const nextWorkers = [
      ...state.workers,
      { name: workerDraftName.trim(), modelId: workerDraftModelId }
    ];
    onChange({ ...state, workers: nextWorkers });
    setWorkerDraftName('');
  };

  const removeWorker = (name: string) => {
    const nextWorkers = state.workers.filter((worker) => worker.name !== name);
    const nextClients = state.clients.map((client) => ({
      ...client,
      workers: client.workers.filter((worker) => worker !== name)
    }));
    onChange({ ...state, workers: nextWorkers, clients: nextClients });
  };

  const addClient = () => {
    if (!clientDraft.name.trim() || !clientDraft.port.trim()) {
      return;
    }
    const filteredWorkers = clientDraft.workers.filter(
      (worker) => !assignedWorkers.has(worker)
    );
    const nextClients = [
      ...state.clients,
      {
        ...clientDraft,
        name: clientDraft.name.trim(),
        workers: filteredWorkers
      }
    ];
    onChange({ ...state, clients: nextClients });
    setClientDraft({ name: '', port: '', workers: [] });
  };

  const removeClient = (name: string) => {
    const nextClients = state.clients.filter((client) => client.name !== name);
    const nextDevices = state.devices.map((device) => ({
      ...device,
      entities: device.entities.filter((entity) => entity !== name)
    }));
    onChange({ ...state, clients: nextClients, devices: nextDevices });
  };

  const addSource = () => {
    if (!sourceDraft.name.trim() || !sourceDraft.port.trim()) {
      return;
    }
    onChange({
      ...state,
      sources: [...state.sources, { ...sourceDraft, name: sourceDraft.name.trim() }]
    });
    setSourceDraft({
      name: '',
      port: '',
      frequency: state.settings.frequency,
      policy: '0',
      epochs: '1',
      type: '0'
    });
  };

  const removeSource = (name: string) => {
    const nextSources = state.sources.filter((source) => source.name !== name);
    const nextDevices = state.devices.map((device) => ({
      ...device,
      entities: device.entities.filter((entity) => entity !== name)
    }));
    onChange({ ...state, sources: nextSources, devices: nextDevices });
  };

  const addRouter = () => {
    if (!routerDraft.name.trim() || !routerDraft.port.trim()) {
      return;
    }
    onChange({
      ...state,
      routers: [...state.routers, { ...routerDraft, name: routerDraft.name.trim() }]
    });
    setRouterDraft({ name: '', port: '', policy: '0' });
  };

  const removeRouter = (name: string) => {
    const nextRouters = state.routers.filter((router) => router.name !== name);
    const nextDevices = state.devices.map((device) => ({
      ...device,
      entities: device.entities.filter((entity) => entity !== name)
    }));
    onChange({ ...state, routers: nextRouters, devices: nextDevices });
  };

  const addDevice = () => {
    if (!deviceDraft.name.trim() || !deviceDraft.ipv4.trim()) {
      return;
    }
    onChange({
      ...state,
      devices: [...state.devices, { ...deviceDraft, name: deviceDraft.name.trim() }]
    });
    setDeviceDraft({ name: '', ipv4: '', entities: [] });
  };

  const removeDevice = (name: string) => {
    onChange({
      ...state,
      devices: state.devices.filter((device) => device.name !== name)
    });
  };

  return (
    <div className="view">
      <div className="hero">
        <div>
          <p className="eyebrow">Nerlnet Planner</p>
          <h1>Design distributed experiments in one flow.</h1>
          <p>
            Build worker models, map entities to devices, and prepare JSON ready for deployment.
          </p>
        </div>
        <div className="hero-badge">
          <div>
            <span>Infra Targets</span>
            <strong>{infraSummary}</strong>
          </div>
          <div>
            <span>Policies</span>
            <strong>Source + Router</strong>
          </div>
        </div>
      </div>

      <div className="grid two">
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
              <p className="panel-title">Main Services</p>
              <p className="panel-subtitle">Ports and args for API + Main server.</p>
            </div>
          </div>
          <div className="inline-fields">
            <label className="field">
              <span>Main Server Port</span>
              <input
                type="text"
                value={state.servers.mainServer.port}
                onChange={(event) =>
                  onChange({
                    ...state,
                    servers: {
                      ...state.servers,
                      mainServer: {
                        ...state.servers.mainServer,
                        port: event.target.value
                      }
                    }
                  })
                }
              />
            </label>
            <label className="field">
              <span>API Server Port</span>
              <input
                type="text"
                value={state.servers.apiServer.port}
                onChange={(event) =>
                  onChange({
                    ...state,
                    servers: {
                      ...state.servers,
                      apiServer: { ...state.servers.apiServer, port: event.target.value }
                    }
                  })
                }
              />
            </label>
          </div>
          <div className="inline-fields">
            <label className="field">
              <span>Main Server Args</span>
              <input
                type="text"
                value={state.servers.mainServer.args}
                onChange={(event) =>
                  onChange({
                    ...state,
                    servers: {
                      ...state.servers,
                      mainServer: {
                        ...state.servers.mainServer,
                        args: event.target.value
                      }
                    }
                  })
                }
              />
            </label>
            <label className="field">
              <span>API Server Args</span>
              <input
                type="text"
                value={state.servers.apiServer.args}
                onChange={(event) =>
                  onChange({
                    ...state,
                    servers: {
                      ...state.servers,
                      apiServer: {
                        ...state.servers.apiServer,
                        args: event.target.value
                      }
                    }
                  })
                }
              />
            </label>
          </div>
        </div>
      </div>

      <ModelBuilder
        model={modelDraft}
        onChange={setModelDraft}
        onSave={saveModel}
        onDelete={state.models.length > 0 ? deleteModel : undefined}
      />

      <div className="grid two">
        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Model Library</p>
              <p className="panel-subtitle">Select or duplicate worker models.</p>
            </div>
          </div>
          <div className="library-list">
            {state.models.length === 0 && <p className="muted">No models yet. Save one to start.</p>}
            {state.models.map((entry) => (
              <button
                key={entry.id}
                className={`library-item ${entry.id === selectedModelId ? 'active' : ''}`}
                type="button"
                onClick={() => setSelectedModelId(entry.id)}
              >
                <div>
                  <strong>{entry.name}</strong>
                  <span className="chip">{infraTypeOptions.find((o) => o.value === entry.infraType)?.label}</span>
                </div>
                <span className="muted">{entry.id.slice(0, 8)}</span>
              </button>
            ))}
          </div>
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Workers</p>
              <p className="panel-subtitle">Assign models to worker instances.</p>
            </div>
          </div>
          <div className="inline-fields">
            <label className="field">
              <span>Worker Name</span>
              <input
                type="text"
                value={workerDraftName}
                onChange={(event) => setWorkerDraftName(event.target.value)}
              />
            </label>
            <label className="field">
              <span>Model</span>
              <select
                value={workerDraftModelId}
                onChange={(event) => setWorkerDraftModelId(event.target.value)}
              >
                {state.models.map((entry) => (
                  <option key={entry.id} value={entry.id}>
                    {entry.name}
                  </option>
                ))}
              </select>
            </label>
            <button className="ghost" type="button" onClick={addWorker}>
              Add Worker
            </button>
          </div>
          <div className="library-list compact">
            {state.workers.map((worker) => (
              <div key={worker.name} className="library-item">
                <div>
                  <strong>{worker.name}</strong>
                  <span className="muted">
                    {state.models.find((model) => model.id === worker.modelId)?.name ?? 'Unlinked'}
                  </span>
                </div>
                <button
                  className="ghost danger"
                  type="button"
                  onClick={() => removeWorker(worker.name)}
                >
                  Remove
                </button>
              </div>
            ))}
          </div>
        </div>
      </div>

      <div className="grid three">
        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Clients</p>
              <p className="panel-subtitle">Bind workers to edge clients.</p>
            </div>
          </div>
          <label className="field">
            <span>Client Name</span>
            <input
              type="text"
              value={clientDraft.name}
              onChange={(event) => setClientDraft({ ...clientDraft, name: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Client Port</span>
            <input
              type="text"
              value={clientDraft.port}
              onChange={(event) => setClientDraft({ ...clientDraft, port: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Workers</span>
            <select
              multiple
              value={clientDraft.workers}
              onChange={(event) =>
                setClientDraft({
                  ...clientDraft,
                  workers: Array.from(event.target.selectedOptions).map((option) => option.value)
                })
              }
            >
              {availableWorkers.map((worker) => (
                <option key={worker.name} value={worker.name}>
                  {worker.name}
                </option>
              ))}
            </select>
          </label>
          <button className="ghost" type="button" onClick={addClient}>
            Add Client
          </button>
          <div className="library-list compact">
            {state.clients.map((client) => (
              <div key={client.name} className="library-item">
                <div>
                  <strong>{client.name}</strong>
                  <span className="muted">{client.workers.join(', ') || 'No workers yet'}</span>
                </div>
                <button className="ghost danger" type="button" onClick={() => removeClient(client.name)}>
                  Remove
                </button>
              </div>
            ))}
          </div>
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Sources</p>
              <p className="panel-subtitle">Policies, epochs, and stream frequency.</p>
            </div>
          </div>
          <label className="field">
            <span>Source Name</span>
            <input
              type="text"
              value={sourceDraft.name}
              onChange={(event) => setSourceDraft({ ...sourceDraft, name: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Port</span>
            <input
              type="text"
              value={sourceDraft.port}
              onChange={(event) => setSourceDraft({ ...sourceDraft, port: event.target.value })}
            />
          </label>
          <div className="inline-fields">
            <label className="field">
              <span>Frequency</span>
              <input
                type="text"
                value={sourceDraft.frequency}
                onChange={(event) => setSourceDraft({ ...sourceDraft, frequency: event.target.value })}
              />
            </label>
            <label className="field">
              <span>Epochs</span>
              <input
                type="text"
                value={sourceDraft.epochs}
                onChange={(event) => setSourceDraft({ ...sourceDraft, epochs: event.target.value })}
              />
            </label>
          </div>
          <div className="inline-fields">
            <label className="field">
              <span>Policy</span>
              <select
                value={sourceDraft.policy}
                onChange={(event) => setSourceDraft({ ...sourceDraft, policy: event.target.value })}
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
                value={sourceDraft.type}
                onChange={(event) => setSourceDraft({ ...sourceDraft, type: event.target.value })}
              >
                {sourceTypeOptions.map((option) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </select>
            </label>
          </div>
          <button className="ghost" type="button" onClick={addSource}>
            Add Source
          </button>
          <div className="library-list compact">
            {state.sources.map((source) => (
              <div key={source.name} className="library-item">
                <div>
                  <strong>{source.name}</strong>
                  <span className="muted">policy {source.policy}</span>
                </div>
                <button className="ghost danger" type="button" onClick={() => removeSource(source.name)}>
                  Remove
                </button>
              </div>
            ))}
          </div>
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Routers</p>
              <p className="panel-subtitle">Routing policies and ports.</p>
            </div>
          </div>
          <label className="field">
            <span>Router Name</span>
            <input
              type="text"
              value={routerDraft.name}
              onChange={(event) => setRouterDraft({ ...routerDraft, name: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Port</span>
            <input
              type="text"
              value={routerDraft.port}
              onChange={(event) => setRouterDraft({ ...routerDraft, port: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Policy</span>
            <select
              value={routerDraft.policy}
              onChange={(event) => setRouterDraft({ ...routerDraft, policy: event.target.value })}
            >
              {routerPolicyOptions.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <button className="ghost" type="button" onClick={addRouter}>
            Add Router
          </button>
          <div className="library-list compact">
            {state.routers.map((router) => (
              <div key={router.name} className="library-item">
                <div>
                  <strong>{router.name}</strong>
                  <span className="muted">policy {router.policy}</span>
                </div>
                <button className="ghost danger" type="button" onClick={() => removeRouter(router.name)}>
                  Remove
                </button>
              </div>
            ))}
          </div>
        </div>
      </div>

      <div className="panel">
        <div className="panel-header">
          <div>
            <p className="panel-title">Devices</p>
            <p className="panel-subtitle">Bind entities to physical or virtual hosts.</p>
          </div>
        </div>
        <div className="grid three">
          <label className="field">
            <span>Device Name</span>
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
          <label className="field">
            <span>Entities</span>
            <select
              multiple
              value={deviceDraft.entities}
              onChange={(event) =>
                setDeviceDraft({
                  ...deviceDraft,
                  entities: Array.from(event.target.selectedOptions).map((option) => option.value)
                })
              }
            >
              {entitiesList.map((entity) => (
                <option key={entity} value={entity}>
                  {entity}
                </option>
              ))}
            </select>
          </label>
        </div>
        <button className="ghost" type="button" onClick={addDevice}>
          Add Device
        </button>
        <div className="library-list compact">
          {state.devices.map((device) => (
            <div key={device.name} className="library-item">
              <div>
                <strong>{device.name}</strong>
                <span className="muted">{device.ipv4}</span>
              </div>
              <div className="muted">{device.entities.join(', ')}</div>
              <button className="ghost danger" type="button" onClick={() => removeDevice(device.name)}>
                Remove
              </button>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

export default PlannerView;

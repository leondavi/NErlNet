import { useCallback, useEffect, useRef, useState } from 'react';
import ExperimentFlowView from './components/ExperimentFlowView';
import ExportView from './components/ExportView';
import ModelLabView from './components/ModelLabView';
import SandboxView from './components/SandboxView';
import { createDefaultState } from './data/defaults';
import { PlannerState } from './data/types';

const STORAGE_KEY = 'nerlnet-planner-state-v1';

type ViewKey = 'sandbox' | 'models' | 'experiment' | 'export';

const coerceArray = <T,>(value: unknown, fallback: T[]): T[] =>
  Array.isArray(value) ? (value as T[]) : fallback;

const coerceRecord = <T extends Record<string, unknown>>(value: unknown, fallback: T): T =>
  value && typeof value === 'object' && !Array.isArray(value) ? (value as T) : fallback;

const hydrateState = (stored?: Partial<PlannerState> | null): PlannerState => {
  const defaults = createDefaultState();
  if (!stored) {
    return defaults;
  }
  const storedExperiment = coerceRecord(stored.experimentFlow, {});
  const storedServers = coerceRecord(stored.servers, {});
  const storedUi = coerceRecord(stored.ui, {});
  return {
    ...defaults,
    ...stored,
    devices: coerceArray(stored.devices, defaults.devices),
    routers: coerceArray(stored.routers, defaults.routers),
    sources: coerceArray(stored.sources, defaults.sources),
    clients: coerceArray(stored.clients, defaults.clients),
    workers: coerceArray(stored.workers, defaults.workers),
    models: coerceArray(stored.models, defaults.models),
    connections: coerceArray(stored.connections, defaults.connections),
    settings: { ...defaults.settings, ...(stored.settings ?? {}) },
    servers: {
      mainServer: { ...defaults.servers.mainServer, ...(storedServers.mainServer as Record<string, string> ?? {}) },
      apiServer: { ...defaults.servers.apiServer, ...(storedServers.apiServer as Record<string, string> ?? {}) }
    },
    experimentFlow: {
      ...defaults.experimentFlow,
      ...storedExperiment,
      phases: coerceArray(
        (storedExperiment as { phases?: unknown }).phases,
        defaults.experimentFlow.phases
      ).map((phase: { id?: string; phaseName?: string; phaseType?: string; sourcePieces?: unknown[] }) => ({
        ...phase,
        id: phase.id ?? crypto.randomUUID(),
        sourcePieces: coerceArray(phase.sourcePieces, []).map(
          (piece: { id?: string }) => ({
            ...piece,
            id: piece.id ?? crypto.randomUUID()
          })
        )
      }))
    },
    ui: {
      nodePositions: {
        ...defaults.ui?.nodePositions,
        ...((storedUi.nodePositions as Record<string, { x: number; y: number }>) ?? {})
      }
    }
  };
};

const App = () => {
  const [state, setState] = useState<PlannerState>(() => {
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored) {
      try {
        return hydrateState(JSON.parse(stored) as PlannerState);
      } catch {
        return createDefaultState();
      }
    }
    return createDefaultState();
  });

  const [activeView, setActiveView] = useState<ViewKey>('sandbox');
  const [savedAt, setSavedAt] = useState<string | null>(null);
  const saveTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  const debouncedSave = useCallback((stateToSave: PlannerState) => {
    if (saveTimeoutRef.current) {
      clearTimeout(saveTimeoutRef.current);
    }
    saveTimeoutRef.current = setTimeout(() => {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(stateToSave));
      setSavedAt(new Date().toLocaleTimeString());
    }, 300);
  }, []);

  useEffect(() => {
    debouncedSave(state);
    return () => {
      if (saveTimeoutRef.current) {
        clearTimeout(saveTimeoutRef.current);
      }
    };
  }, [state, debouncedSave]);

  return (
    <div className="app">
      <header className="topbar">
        <div className="brand">
          <div className="logo">NN</div>
          <div>
            <p>Nerlnet Planner</p>
            <span>{savedAt ? `Autosaved ${savedAt}` : 'Autosave enabled'}</span>
          </div>
        </div>
        <nav className="nav">
          <button
            type="button"
            className={activeView === 'sandbox' ? 'active' : ''}
            onClick={() => setActiveView('sandbox')}
          >
            Sandbox
          </button>
          <button
            type="button"
            className={activeView === 'models' ? 'active' : ''}
            onClick={() => setActiveView('models')}
          >
            Worker Model Lab
          </button>
          <button
            type="button"
            className={activeView === 'experiment' ? 'active' : ''}
            onClick={() => setActiveView('experiment')}
          >
            Experiment Flow
          </button>
          <button
            type="button"
            className={activeView === 'export' ? 'active' : ''}
            onClick={() => setActiveView('export')}
          >
            Export
          </button>
        </nav>
        <div className="topbar-actions">
          <button
            type="button"
            className="ghost"
            onClick={() => {
              setState(createDefaultState());
              setActiveView('sandbox');
            }}
          >
            Reset Workspace
          </button>
        </div>
      </header>

      <main className="main">
        {activeView === 'sandbox' && <SandboxView state={state} onChange={setState} />}
        {activeView === 'models' && <ModelLabView state={state} onChange={setState} />}
        {activeView === 'experiment' && <ExperimentFlowView state={state} onChange={setState} />}
        {activeView === 'export' && <ExportView state={state} onChange={setState} />}
      </main>
    </div>
  );
};

export default App;

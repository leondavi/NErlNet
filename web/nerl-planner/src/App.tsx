import { useCallback, useEffect, useRef, useState } from 'react';
import ExperimentFlowView from './components/ExperimentFlowView';
import ExportView from './components/ExportView';
import ModelLabView from './components/ModelLabView';
import SandboxView from './components/SandboxView';
import { createDefaultState } from './data/defaults';
import { ExperimentPhase, ExperimentSourcePiece, PlannerState } from './data/types';

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
  const storedExperiment = coerceRecord<Partial<PlannerState['experimentFlow']>>(
    stored.experimentFlow,
    {}
  );
  const storedServers = coerceRecord<Partial<PlannerState['servers']>>(stored.servers, {});
  const storedUi = coerceRecord<Partial<NonNullable<PlannerState['ui']>>>(stored.ui, {});
  const storedHfDatasetCache = coerceRecord<Partial<NonNullable<NonNullable<PlannerState['ui']>['hfDatasetCache']>>>(
    storedUi.hfDatasetCache,
    {}
  );
  const defaultHfDatasetCache = defaults.ui?.hfDatasetCache ?? {
    datasets: [],
    selectedIdx: '',
    status: 'idle',
    message: '',
    hasLoaded: false
  };
  const hydratedPhases = coerceArray<Partial<ExperimentPhase>>(
    storedExperiment.phases,
    defaults.experimentFlow.phases
  ).map((phase, index) => ({
    id: phase.id ?? crypto.randomUUID(),
    phaseName: typeof phase.phaseName === 'string' ? phase.phaseName : `phase_${index + 1}`,
    phaseType: phase.phaseType === 'prediction' ? 'prediction' : 'training',
    sourcePieces: coerceArray<Partial<ExperimentSourcePiece>>(phase.sourcePieces, []).map(
      (piece) => ({
      id: piece.id ?? crypto.randomUUID(),
      sourceName: piece.sourceName ?? '',
      startingSample: piece.startingSample ?? '',
      numOfBatches: piece.numOfBatches ?? '',
      workers: coerceArray(piece.workers, []),
      nerltensorType: piece.nerltensorType ?? 'float'
      })
    ),
    parallelExecution: phase.parallelExecution
  }));
  return {
    ...defaults,
    ...stored,
    devices: coerceArray(stored.devices, defaults.devices),
    routers: coerceArray(stored.routers, defaults.routers),
    sources: coerceArray(stored.sources, defaults.sources),
    clients: coerceArray(stored.clients, defaults.clients),
    superNodes: coerceArray(stored.superNodes, defaults.superNodes),
    workers: coerceArray(stored.workers, defaults.workers),
    models: coerceArray(stored.models, defaults.models),
    connections: coerceArray(stored.connections, defaults.connections),
    settings: { ...defaults.settings, ...(stored.settings ?? {}) },
    servers: {
      mainServer: { ...defaults.servers.mainServer, ...(storedServers.mainServer ?? {}) },
      apiServer: { ...defaults.servers.apiServer, ...(storedServers.apiServer ?? {}) }
    },
    experimentFlow: {
      ...defaults.experimentFlow,
      ...storedExperiment,
      phases: hydratedPhases
    },
    ui: {
      nodePositions: {
        ...defaults.ui?.nodePositions,
        ...(storedUi.nodePositions ?? {})
      },
      openLayerPositions: {
        ...(defaults.ui?.openLayerPositions ?? {}),
        ...(storedUi.openLayerPositions ?? {})
      },
      hfDatasetCache: {
        ...defaultHfDatasetCache,
        ...storedHfDatasetCache,
        datasets: coerceArray(
          storedHfDatasetCache.datasets,
          defaultHfDatasetCache.datasets
        )
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

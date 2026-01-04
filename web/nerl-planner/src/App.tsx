import { useEffect, useState } from 'react';
import ConnectionMapView from './components/ConnectionMapView';
import ExperimentFlowView from './components/ExperimentFlowView';
import ExportView from './components/ExportView';
import PlannerView from './components/PlannerView';
import { createDefaultState } from './data/defaults';
import { PlannerState } from './data/types';

const STORAGE_KEY = 'nerlnet-planner-state-v1';

type ViewKey = 'planner' | 'connections' | 'experiment' | 'export';

const App = () => {
  const [state, setState] = useState<PlannerState>(() => {
    const stored = localStorage.getItem(STORAGE_KEY);
    if (stored) {
      try {
        return JSON.parse(stored) as PlannerState;
      } catch {
        return createDefaultState();
      }
    }
    return createDefaultState();
  });

  const [activeView, setActiveView] = useState<ViewKey>('planner');
  const [savedAt, setSavedAt] = useState<string | null>(null);

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    setSavedAt(new Date().toLocaleTimeString());
  }, [state]);

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
            className={activeView === 'planner' ? 'active' : ''}
            onClick={() => setActiveView('planner')}
          >
            Planner
          </button>
          <button
            type="button"
            className={activeView === 'connections' ? 'active' : ''}
            onClick={() => setActiveView('connections')}
          >
            Connection Map
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
              setActiveView('planner');
            }}
          >
            Reset Workspace
          </button>
        </div>
      </header>

      <main className="main">
        {activeView === 'planner' && <PlannerView state={state} onChange={setState} />}
        {activeView === 'connections' && <ConnectionMapView state={state} onChange={setState} />}
        {activeView === 'experiment' && <ExperimentFlowView state={state} onChange={setState} />}
        {activeView === 'export' && <ExportView state={state} onChange={setState} />}
      </main>
    </div>
  );
};

export default App;

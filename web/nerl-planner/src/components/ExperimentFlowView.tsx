import { useMemo, useState } from 'react';
import { nerltensorTypeOptions, phaseTypeOptions } from '../data/mappings';
import { PlannerState } from '../data/types';

const ExperimentFlowView = ({
  state,
  onChange
}: {
  state: PlannerState;
  onChange: (next: PlannerState) => void;
}) => {
  const [phaseDraft, setPhaseDraft] = useState({ phaseName: '', phaseType: 'training' });
  const [selectedPhaseIndex, setSelectedPhaseIndex] = useState(0);
  const [pieceDraft, setPieceDraft] = useState({
    sourceName: '',
    startingSample: '0',
    numOfBatches: '100',
    workers: [] as string[],
    nerltensorType: 'float'
  });

  const workerOptions = useMemo(
    () => state.workers.map((worker) => worker.name),
    [state.workers]
  );
  const sourceOptions = useMemo(
    () => state.sources.map((source) => source.name),
    [state.sources]
  );

  const updateExperimentField = (field: string, value: string) => {
    onChange({
      ...state,
      experimentFlow: { ...state.experimentFlow, [field]: value }
    });
  };

  const addPhase = () => {
    if (!phaseDraft.phaseName.trim()) {
      return;
    }
    const nextPhases = [
      ...state.experimentFlow.phases,
      { phaseName: phaseDraft.phaseName.trim(), phaseType: phaseDraft.phaseType, sourcePieces: [] }
    ];
    onChange({
      ...state,
      experimentFlow: { ...state.experimentFlow, phases: nextPhases }
    });
    setPhaseDraft({ phaseName: '', phaseType: 'training' });
    setSelectedPhaseIndex(nextPhases.length - 1);
  };

  const removePhase = (index: number) => {
    const nextPhases = state.experimentFlow.phases.filter((_, idx) => idx !== index);
    onChange({
      ...state,
      experimentFlow: { ...state.experimentFlow, phases: nextPhases }
    });
    setSelectedPhaseIndex(Math.max(0, index - 1));
  };

  const addSourcePiece = () => {
    const phase = state.experimentFlow.phases[selectedPhaseIndex];
    if (!phase || !pieceDraft.sourceName) {
      return;
    }
    const nextPhases = state.experimentFlow.phases.map((item, index) => {
      if (index !== selectedPhaseIndex) {
        return item;
      }
      return {
        ...item,
        sourcePieces: [...item.sourcePieces, { ...pieceDraft }]
      };
    });
    onChange({
      ...state,
      experimentFlow: { ...state.experimentFlow, phases: nextPhases }
    });
    setPieceDraft({
      sourceName: '',
      startingSample: '0',
      numOfBatches: '100',
      workers: [],
      nerltensorType: 'float'
    });
  };

  const removeSourcePiece = (phaseIndex: number, pieceIndex: number) => {
    const nextPhases = state.experimentFlow.phases.map((phase, index) => {
      if (index !== phaseIndex) {
        return phase;
      }
      return {
        ...phase,
        sourcePieces: phase.sourcePieces.filter((_, idx) => idx !== pieceIndex)
      };
    });
    onChange({
      ...state,
      experimentFlow: { ...state.experimentFlow, phases: nextPhases }
    });
  };

  const activePhase = state.experimentFlow.phases[selectedPhaseIndex];

  return (
    <div className="view">
      <div className="hero compact">
        <div>
          <p className="eyebrow">Experiment Flow</p>
          <h1>Design training and prediction phases.</h1>
          <p>Attach source pieces, define CSV slices, and orchestrate worker participation.</p>
        </div>
        <div className="hero-badge">
          <div>
            <span>Phases</span>
            <strong>{state.experimentFlow.phases.length}</strong>
          </div>
          <div>
            <span>Sources</span>
            <strong>{state.sources.length}</strong>
          </div>
        </div>
      </div>

      <div className="grid two">
        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Experiment Metadata</p>
              <p className="panel-subtitle">Dataset and label configuration.</p>
            </div>
          </div>
          <label className="field">
            <span>Experiment Name</span>
            <input
              type="text"
              value={state.experimentFlow.experimentName}
              onChange={(event) => updateExperimentField('experimentName', event.target.value)}
            />
          </label>
          <label className="field">
            <span>Experiment Type</span>
            <input
              type="text"
              value={state.experimentFlow.experimentType}
              onChange={(event) => updateExperimentField('experimentType', event.target.value)}
            />
          </label>
          <label className="field">
            <span>CSV File Path</span>
            <input
              type="text"
              value={state.experimentFlow.csvFilePath}
              onChange={(event) => updateExperimentField('csvFilePath', event.target.value)}
            />
          </label>
          <div className="inline-fields">
            <label className="field">
              <span>Batch Size</span>
              <input
                type="text"
                value={state.experimentFlow.batchSize}
                onChange={(event) => updateExperimentField('batchSize', event.target.value)}
              />
            </label>
            <label className="field">
              <span>Features</span>
              <input
                type="text"
                value={state.experimentFlow.numOfFeatures}
                onChange={(event) => updateExperimentField('numOfFeatures', event.target.value)}
              />
            </label>
            <label className="field">
              <span>Labels</span>
              <input
                type="text"
                value={state.experimentFlow.numOfLabels}
                onChange={(event) => updateExperimentField('numOfLabels', event.target.value)}
              />
            </label>
          </div>
          <label className="field">
            <span>Header Names</span>
            <input
              type="text"
              value={state.experimentFlow.headersNames}
              onChange={(event) => updateExperimentField('headersNames', event.target.value)}
            />
          </label>
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Add Phase</p>
              <p className="panel-subtitle">Training or prediction blocks with source pieces.</p>
            </div>
          </div>
          <label className="field">
            <span>Phase Name</span>
            <input
              type="text"
              value={phaseDraft.phaseName}
              onChange={(event) => setPhaseDraft({ ...phaseDraft, phaseName: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Phase Type</span>
            <select
              value={phaseDraft.phaseType}
              onChange={(event) => setPhaseDraft({ ...phaseDraft, phaseType: event.target.value })}
            >
              {phaseTypeOptions.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
          <button className="ghost" type="button" onClick={addPhase}>
            Add Phase
          </button>
          <div className="library-list compact">
            {state.experimentFlow.phases.map((phase, index) => (
              <button
                key={phase.phaseName}
                className={`library-item ${index === selectedPhaseIndex ? 'active' : ''}`}
                type="button"
                onClick={() => setSelectedPhaseIndex(index)}
              >
                <div>
                  <strong>{phase.phaseName}</strong>
                  <span className="muted">{phase.phaseType}</span>
                </div>
                <button
                  className="ghost danger"
                  type="button"
                  onClick={(event) => {
                    event.stopPropagation();
                    removePhase(index);
                  }}
                >
                  Remove
                </button>
              </button>
            ))}
          </div>
        </div>
      </div>

      <div className="panel">
        <div className="panel-header">
          <div>
            <p className="panel-title">Source Pieces</p>
            <p className="panel-subtitle">Attach dataset slices to {activePhase?.phaseName || 'a phase'}.</p>
          </div>
        </div>
        <div className="grid three">
          <label className="field">
            <span>Source</span>
            <select
              value={pieceDraft.sourceName}
              onChange={(event) => setPieceDraft({ ...pieceDraft, sourceName: event.target.value })}
            >
              <option value="">Select source</option>
              {sourceOptions.map((source) => (
                <option key={source} value={source}>
                  {source}
                </option>
              ))}
            </select>
          </label>
          <label className="field">
            <span>Start Sample</span>
            <input
              type="text"
              value={pieceDraft.startingSample}
              onChange={(event) => setPieceDraft({ ...pieceDraft, startingSample: event.target.value })}
            />
          </label>
          <label className="field">
            <span>Num of Batches</span>
            <input
              type="text"
              value={pieceDraft.numOfBatches}
              onChange={(event) => setPieceDraft({ ...pieceDraft, numOfBatches: event.target.value })}
            />
          </label>
        </div>
        <div className="grid two">
          <label className="field">
            <span>Workers</span>
            <select
              multiple
              value={pieceDraft.workers}
              onChange={(event) =>
                setPieceDraft({
                  ...pieceDraft,
                  workers: Array.from(event.target.selectedOptions).map((option) => option.value)
                })
              }
            >
              {workerOptions.map((worker) => (
                <option key={worker} value={worker}>
                  {worker}
                </option>
              ))}
            </select>
          </label>
          <label className="field">
            <span>Nerltensor Type</span>
            <select
              value={pieceDraft.nerltensorType}
              onChange={(event) => setPieceDraft({ ...pieceDraft, nerltensorType: event.target.value })}
            >
              {nerltensorTypeOptions.map((option) => (
                <option key={option.value} value={option.value}>
                  {option.label}
                </option>
              ))}
            </select>
          </label>
        </div>
        <button className="ghost" type="button" onClick={addSourcePiece}>
          Add Source Piece
        </button>

        {activePhase && (
          <div className="library-list compact">
            {activePhase.sourcePieces.map((piece, index) => (
              <div key={`${piece.sourceName}-${index}`} className="library-item">
                <div>
                  <strong>{piece.sourceName}</strong>
                  <span className="muted">
                    {piece.startingSample} / {piece.numOfBatches} batches
                  </span>
                </div>
                <span className="muted">{piece.workers.join(', ')}</span>
                <button
                  className="ghost danger"
                  type="button"
                  onClick={() => removeSourcePiece(selectedPhaseIndex, index)}
                >
                  Remove
                </button>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
};

export default ExperimentFlowView;

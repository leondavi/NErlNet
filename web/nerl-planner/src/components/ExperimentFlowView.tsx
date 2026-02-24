import { useEffect, useMemo, useState } from 'react';
import { nerltensorTypeOptions, phaseTypeOptions } from '../data/mappings';
import {
  HfDatasetCache,
  HfDatasetMeta,
  ParallelExecutionConfig,
  ParallelExecutionMode,
  ParallelScheduler,
  PlannerState,
  WorkerModel
} from '../data/types';
import ValidationPanel from './ValidationPanel';
import { ValidationIssue, validatePlannerState } from '../utils/validation';
import { parseShape } from '../utils/torchGraph';

const toPositiveNumber = (value: string) => {
  const num = Number(value);
  if (!Number.isFinite(num) || num <= 0) {
    return null;
  }
  return num;
};

const numericProduct = (values: number[]) => values.reduce((acc, value) => acc * value, 1);

const defaultHfDatasetCache: HfDatasetCache = {
  datasets: [],
  selectedIdx: '',
  status: 'idle',
  message: '',
  hasLoaded: false
};

const inferModelIoDimensions = (model: WorkerModel) => {
  if (model.infraType === 'torch') {
    const inputShape = parseShape(model.trainParams.inputTensorShape);
    const labelsShape = parseShape(model.trainParams.labelsShape);
    const inputDims = inputShape?.slice(1).filter((dim): dim is number => typeof dim === 'number') ?? [];
    const labelDims = labelsShape?.slice(1).filter((dim): dim is number => typeof dim === 'number') ?? [];
    return {
      input: inputDims.length > 0 ? numericProduct(inputDims) : null,
      output: labelDims.length > 0 ? numericProduct(labelDims) : null
    };
  }

  if (!('layers' in model) || model.layers.length === 0) {
    return { input: null, output: null };
  }

  const first = Number(model.layers[0]?.size);
  const last = Number(model.layers[model.layers.length - 1]?.size);
  return {
    input: Number.isFinite(first) && first > 0 ? first : null,
    output: Number.isFinite(last) && last > 0 ? last : null
  };
};

const parallelModeOptions: { value: ParallelExecutionMode; label: string }[] = [
  { value: 'legacy', label: 'Legacy (w2w)' },
  { value: 'pipeline', label: 'Pipeline' },
  { value: 'tensor', label: 'Tensor' },
  { value: 'pipeline_tensor', label: 'Pipeline + Tensor' }
];

const parallelSchedulerOptions: { value: ParallelScheduler; label: string }[] = [
  { value: 'gpipe', label: 'GPipe' },
  { value: '1f1b', label: '1F1B' },
  { value: 'interleaved', label: 'Interleaved' }
];

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
  const superNodeOptions = useMemo(
    () => state.superNodes.map((superNode) => superNode.name),
    [state.superNodes]
  );
  const validation = useMemo(() => validatePlannerState(state), [state]);
  const experimentIssues = useMemo(
    () => validation.issues.filter((issue) => issue.scope.includes('experiment')),
    [validation]
  );
  const hfDatasetCache = state.ui?.hfDatasetCache ?? defaultHfDatasetCache;
  const hfDatasets = hfDatasetCache.datasets;
  const selectedHfDatasetIdx = hfDatasetCache.selectedIdx;
  const hfDatasetStatus = hfDatasetCache.status;
  const hfDatasetMessage = hfDatasetCache.message;
  const selectedHfDataset = useMemo(
    () => hfDatasets.find((dataset) => String(dataset.idx) === selectedHfDatasetIdx),
    [hfDatasets, selectedHfDatasetIdx]
  );

  const updateHfDatasetCache = (
    updater: (cache: HfDatasetCache) => HfDatasetCache
  ) => {
    const currentCache = state.ui?.hfDatasetCache ?? defaultHfDatasetCache;
    const nextCache = updater(currentCache);
    onChange({
      ...state,
      ui: {
        nodePositions: state.ui?.nodePositions ?? {},
        openLayerPositions: state.ui?.openLayerPositions ?? {},
        hfDatasetCache: nextCache
      }
    });
  };

  const loadHfDatasets = async () => {
    updateHfDatasetCache((cache) => ({
      ...cache,
      status: 'loading',
      message: ''
    }));
    try {
      const response = await fetch('/api/hf/datasets');
      const data = (await response.json().catch(() => ({}))) as {
        datasets?: HfDatasetMeta[];
        error?: string;
      };
      if (!response.ok) {
        throw new Error(data.error || 'Failed to load HF dataset list');
      }

      const datasets = Array.isArray(data.datasets) ? data.datasets : [];
      updateHfDatasetCache((cache) => {
        const selectedIdx =
          cache.selectedIdx && datasets.some((dataset) => String(dataset.idx) === cache.selectedIdx)
            ? cache.selectedIdx
            : datasets[0]
              ? String(datasets[0].idx)
              : '';
        return {
          ...cache,
          datasets,
          selectedIdx,
          status: 'done',
          message: datasets.length > 0 ? `Loaded ${datasets.length} datasets.` : 'No datasets found.',
          hasLoaded: true
        };
      });
    } catch (error) {
      updateHfDatasetCache((cache) => ({
        ...cache,
        status: 'error',
        message: error instanceof Error ? error.message : 'Failed to load HF dataset list',
        hasLoaded: true
      }));
    }
  };

  const downloadSelectedHfDataset = async () => {
    const repoIdx = Number(selectedHfDatasetIdx);
    if (!Number.isInteger(repoIdx) || repoIdx < 0) {
      updateHfDatasetCache((cache) => ({
        ...cache,
        status: 'error',
        message: 'Select a dataset before downloading.'
      }));
      return;
    }

    updateHfDatasetCache((cache) => ({
      ...cache,
      status: 'downloading',
      message: ''
    }));
    try {
      const response = await fetch('/api/hf/datasets/download', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ repoIdx })
      });
      const data = (await response.json().catch(() => ({}))) as {
        error?: string;
        name?: string;
        datasetPath?: string;
        firstCsvPath?: string;
        csvFiles?: string[];
        sampleRows?: number | null;
        sampleColumns?: number | null;
      };
      if (!response.ok) {
        throw new Error(data.error || 'Failed to download dataset');
      }

      const nextCsvPath = data.firstCsvPath || data.datasetPath || '';
      if (nextCsvPath) {
        updateExperimentField('csvFilePath', nextCsvPath);
      }

      const csvCount = Array.isArray(data.csvFiles) ? data.csvFiles.length : 0;
      const datasetName = data.name || selectedHfDataset?.name || `idx ${repoIdx}`;
      updateHfDatasetCache((cache) => ({
        ...cache,
        datasets: cache.datasets.map((dataset) =>
          dataset.idx !== repoIdx
            ? dataset
            : {
                ...dataset,
                datasetPath: data.datasetPath ?? dataset.datasetPath,
                firstCsvPath: data.firstCsvPath ?? dataset.firstCsvPath,
                sampleRows:
                  data.sampleRows !== undefined ? data.sampleRows : dataset.sampleRows ?? null,
                sampleColumns:
                  data.sampleColumns !== undefined ? data.sampleColumns : dataset.sampleColumns ?? null
              }
        ),
        status: 'done',
        message: nextCsvPath
          ? `Downloaded ${datasetName}. CSV path updated to ${nextCsvPath}${
              csvCount > 0 ? ` (${csvCount} CSV files found).` : '.'
            }`
          : `Downloaded ${datasetName}, but no CSV file was found to auto-fill CSV path.`,
        hasLoaded: true
      }));
    } catch (error) {
      updateHfDatasetCache((cache) => ({
        ...cache,
        status: 'error',
        message: error instanceof Error ? error.message : 'Failed to download dataset'
      }));
    }
  };

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
      { id: crypto.randomUUID(), phaseName: phaseDraft.phaseName.trim(), phaseType: phaseDraft.phaseType, sourcePieces: [] }
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
        sourcePieces: [...item.sourcePieces, { id: crypto.randomUUID(), ...pieceDraft }]
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
  const defaultParallelExecution = useMemo<ParallelExecutionConfig>(
    () => ({
      mode: 'legacy',
      superNode: state.superNodes[0]?.name ?? '',
      scheduler: 'gpipe',
      microBatchSize: '8',
      numMicroBatches: '16',
      virtualStages: '2'
    }),
    [state.superNodes]
  );
  const activeParallelExecution = activePhase?.parallelExecution ?? defaultParallelExecution;

  const updateActivePhase = (updater: (phase: PlannerState['experimentFlow']['phases'][number]) => PlannerState['experimentFlow']['phases'][number]) => {
    if (!activePhase) {
      return;
    }
    const nextPhases = state.experimentFlow.phases.map((phase, index) =>
      index === selectedPhaseIndex ? updater(phase) : phase
    );
    onChange({
      ...state,
      experimentFlow: { ...state.experimentFlow, phases: nextPhases }
    });
  };

  const updateActiveParallelExecution = (patch: Partial<ParallelExecutionConfig>) => {
    updateActivePhase((phase) => ({
      ...phase,
      parallelExecution: {
        ...(phase.parallelExecution ?? defaultParallelExecution),
        ...patch
      }
    }));
  };

  const clearActiveParallelExecution = () => {
    updateActivePhase((phase) => ({
      ...phase,
      parallelExecution: undefined
    }));
  };

  useEffect(() => {
    if (!hfDatasetCache.hasLoaded && hfDatasetCache.status !== 'loading') {
      void loadHfDatasets();
    }
  }, [hfDatasetCache.hasLoaded, hfDatasetCache.status]);

  const hfDatasetIssues = useMemo<ValidationIssue[]>(() => {
    const issues: ValidationIssue[] = [];
    const expFeatures = toPositiveNumber(state.experimentFlow.numOfFeatures);
    const expLabels = toPositiveNumber(state.experimentFlow.numOfLabels);
    const expectedColumns =
      expFeatures !== null && expLabels !== null ? expFeatures + expLabels : null;

    if (
      selectedHfDataset &&
      (selectedHfDataset.sampleColumns === null || selectedHfDataset.sampleColumns === undefined)
    ) {
      issues.push({
        id: `hf-dims-unknown-${selectedHfDataset.idx}`,
        severity: 'warning',
        message: 'Selected dataset dimensions are unknown until a local CSV is available.',
        detail: 'Download this dataset to inspect row/column dimensions and run compatibility checks.',
        scope: ['experiment', 'export']
      });
    }

    if (
      selectedHfDataset &&
      typeof selectedHfDataset.sampleColumns === 'number' &&
      expectedColumns !== null &&
      selectedHfDataset.sampleColumns !== expectedColumns
    ) {
      issues.push({
        id: `hf-columns-${selectedHfDataset.idx}`,
        severity: 'error',
        message: `Selected dataset columns (${selectedHfDataset.sampleColumns}) do not match Features+Labels (${expectedColumns}).`,
        detail:
          'Adjust Experiment Flow feature/label counts or pick a dataset with matching CSV width.',
        scope: ['experiment', 'export']
      });
    }

    if (
      selectedHfDataset &&
      selectedHfDataset.firstCsvPath &&
      state.experimentFlow.csvFilePath &&
      state.experimentFlow.csvFilePath !== selectedHfDataset.firstCsvPath
    ) {
      issues.push({
        id: `hf-csv-path-${selectedHfDataset.idx}`,
        severity: 'warning',
        message: 'CSV File Path differs from the selected Hugging Face dataset file.',
        detail: `Selected dataset CSV: ${selectedHfDataset.firstCsvPath}`,
        scope: ['experiment', 'export']
      });
    }

    if (expFeatures === null || expLabels === null) {
      return issues;
    }

    const workersUsedByExperiment = new Set(
      state.experimentFlow.phases.flatMap((phase) =>
        phase.sourcePieces.flatMap((piece) => piece.workers)
      )
    );
    const workersToValidate =
      workersUsedByExperiment.size > 0
        ? state.workers.filter((worker) => workersUsedByExperiment.has(worker.name))
        : state.workers;
    const modelIds = Array.from(new Set(workersToValidate.map((worker) => worker.modelId).filter(Boolean)));

    modelIds.forEach((modelId) => {
      const model = state.models.find((entry) => entry.id === modelId);
      if (!model) {
        return;
      }
      const io = inferModelIoDimensions(model);
      if (io.input !== null && io.input !== expFeatures) {
        issues.push({
          id: `model-input-${model.id}`,
          severity: 'error',
          message: `Model ${model.name} input size (${io.input}) does not match Features (${expFeatures}).`,
          detail: 'Update model input tensor/layer sizes or change Experiment Flow feature count.',
          scope: ['experiment', 'models', 'export']
        });
      }
      if (io.output !== null && io.output !== expLabels) {
        issues.push({
          id: `model-output-${model.id}`,
          severity: 'error',
          message: `Model ${model.name} output size (${io.output}) does not match Labels (${expLabels}).`,
          detail: 'Update model output tensor/layer sizes or change Experiment Flow labels count.',
          scope: ['experiment', 'models', 'export']
        });
      }
      if (io.input === null || io.output === null) {
        issues.push({
          id: `model-shape-unknown-${model.id}`,
          severity: 'warning',
          message: `Model ${model.name} shape compatibility could not be fully inferred.`,
          detail: 'Verify input and label dimensions manually before running the experiment.',
          scope: ['experiment', 'models', 'export']
        });
      }
    });

    return issues;
  }, [selectedHfDataset, state.experimentFlow, state.models, state.workers]);

  const mergedExperimentIssues = useMemo(() => {
    return [...experimentIssues, ...hfDatasetIssues];
  }, [experimentIssues, hfDatasetIssues]);

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
              <p className="panel-subtitle">Dataset, labels, and batching configuration.</p>
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
          <div className="panel-section">
            <p className="section-title">Hugging Face Datasets (Nerlnet)</p>
            <label className="field">
              <span>Dataset</span>
              <select
                value={selectedHfDatasetIdx}
                onChange={(event) =>
                  updateHfDatasetCache((cache) => ({
                    ...cache,
                    selectedIdx: event.target.value
                  }))
                }
                disabled={hfDatasetStatus === 'loading' || hfDatasetStatus === 'downloading'}
              >
                <option value="">Select dataset</option>
                {hfDatasets.map((dataset) => (
                  <option key={dataset.idx} value={String(dataset.idx)}>
                    {dataset.name || dataset.id}
                    {typeof dataset.sampleColumns === 'number'
                      ? ` • cols ${dataset.sampleColumns}`
                      : ''}
                    {typeof dataset.sampleRows === 'number' ? ` • rows ${dataset.sampleRows}` : ''}
                    {` (${dataset.id})`}
                  </option>
                ))}
              </select>
            </label>
            <div className="panel-actions">
              <button
                type="button"
                className="ghost"
                onClick={loadHfDatasets}
                disabled={hfDatasetStatus === 'loading' || hfDatasetStatus === 'downloading'}
              >
                {hfDatasetStatus === 'loading' ? 'Refreshing...' : 'Refresh List'}
              </button>
              <button
                type="button"
                className="primary"
                onClick={downloadSelectedHfDataset}
                disabled={
                  !selectedHfDatasetIdx ||
                  hfDatasetStatus === 'loading' ||
                  hfDatasetStatus === 'downloading'
                }
              >
                {hfDatasetStatus === 'downloading' ? 'Downloading...' : 'Download Dataset'}
              </button>
            </div>
            {selectedHfDataset && (
              <p className="muted">
                {selectedHfDataset.description || selectedHfDataset.id}
                {selectedHfDataset.csvFiles.length > 0
                  ? ` • ${selectedHfDataset.csvFiles.length} CSV files available in repo`
                  : ''}
                {typeof selectedHfDataset.sampleColumns === 'number'
                  ? ` • columns ${selectedHfDataset.sampleColumns}`
                  : ' • columns unknown (download to inspect)'}
                {typeof selectedHfDataset.sampleRows === 'number'
                  ? ` • rows ${selectedHfDataset.sampleRows}`
                  : ''}
                {selectedHfDataset.error ? ` • ${selectedHfDataset.error}` : ''}
              </p>
            )}
            {hfDatasetMessage && <p className="muted">{hfDatasetMessage}</p>}
          </div>
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
              <p className="panel-title">Phases</p>
              <p className="panel-subtitle">Switch between training and prediction steps.</p>
            </div>
          </div>
          <div className="phase-tabs">
            {state.experimentFlow.phases.map((phase, index) => (
              <div
                key={phase.id}
                className={`phase-tab ${index === selectedPhaseIndex ? 'active' : ''}`}
              >
                <button type="button" className="phase-select" onClick={() => setSelectedPhaseIndex(index)}>
                  <strong>{phase.phaseName}</strong>
                  <span>{phase.phaseType}</span>
                </button>
                <button className="ghost danger" type="button" onClick={() => removePhase(index)}>
                  Remove
                </button>
              </div>
            ))}
            {state.experimentFlow.phases.length === 0 && (
              <p className="muted">No phases yet. Add one below.</p>
            )}
          </div>
          <div className="panel-section">
            <p className="section-title">Add Phase</p>
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
          </div>
        </div>
      </div>

      <div className="grid two">
        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Add Source Piece</p>
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
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Phase Pieces</p>
              <p className="panel-subtitle">Review pieces attached to {activePhase?.phaseName || 'a phase'}.</p>
            </div>
          </div>
          {activePhase && (
            <>
              <div className="library-list compact">
                {activePhase.sourcePieces.map((piece, index) => (
                  <div key={piece.id} className="library-item">
                    <div>
                      <strong>{piece.sourceName}</strong>
                      <span className="muted">
                        {piece.startingSample} / {piece.numOfBatches} batches
                      </span>
                    </div>
                    <span className="muted">{piece.workers.join(', ') || 'No workers selected'}</span>
                    <button
                      className="ghost danger"
                      type="button"
                      onClick={() => removeSourcePiece(selectedPhaseIndex, index)}
                    >
                      Remove
                    </button>
                  </div>
                ))}
                {activePhase.sourcePieces.length === 0 && <p className="muted">No pieces yet.</p>}
              </div>

              <div className="panel-section">
                <p className="section-title">Parallel Execution</p>
                <div className="inline-fields">
                  <label className="field">
                    <span>Mode</span>
                    <select
                      value={activeParallelExecution.mode}
                      onChange={(event) =>
                        updateActiveParallelExecution({
                          mode: event.target.value as ParallelExecutionMode
                        })
                      }
                    >
                      {parallelModeOptions.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                  <label className="field">
                    <span>Scheduler</span>
                    <select
                      value={activeParallelExecution.scheduler}
                      onChange={(event) =>
                        updateActiveParallelExecution({
                          scheduler: event.target.value as ParallelScheduler
                        })
                      }
                    >
                      {parallelSchedulerOptions.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                </div>
                <div className="inline-fields">
                  <label className="field">
                    <span>Super Node</span>
                    <select
                      value={activeParallelExecution.superNode}
                      onChange={(event) =>
                        updateActiveParallelExecution({ superNode: event.target.value })
                      }
                    >
                      <option value="">Select super node</option>
                      {superNodeOptions.map((superNode) => (
                        <option key={superNode} value={superNode}>
                          {superNode}
                        </option>
                      ))}
                    </select>
                  </label>
                  <label className="field">
                    <span>Micro Batch Size</span>
                    <input
                      type="text"
                      value={activeParallelExecution.microBatchSize}
                      onChange={(event) =>
                        updateActiveParallelExecution({ microBatchSize: event.target.value })
                      }
                    />
                  </label>
                  <label className="field">
                    <span>Num Micro Batches</span>
                    <input
                      type="text"
                      value={activeParallelExecution.numMicroBatches}
                      onChange={(event) =>
                        updateActiveParallelExecution({ numMicroBatches: event.target.value })
                      }
                    />
                  </label>
                </div>
                {activeParallelExecution.scheduler === 'interleaved' && (
                  <label className="field">
                    <span>Virtual Stages</span>
                    <input
                      type="text"
                      value={activeParallelExecution.virtualStages}
                      onChange={(event) =>
                        updateActiveParallelExecution({ virtualStages: event.target.value })
                      }
                    />
                  </label>
                )}
                <div className="panel-actions align-right">
                  <button type="button" className="ghost" onClick={clearActiveParallelExecution}>
                    Clear Parallel Settings
                  </button>
                </div>
              </div>
            </>
          )}
        </div>
      </div>

      <ValidationPanel
        issues={mergedExperimentIssues}
        title="Experiment validation"
        subtitle="Fix phase and batch configuration issues before export."
      />
    </div>
  );
};

export default ExperimentFlowView;

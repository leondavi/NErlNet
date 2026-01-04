import {
  activationFunctionOptions,
  distributedSystemOptions,
  functionOptionsByLayerType,
  infraTypeOptions,
  layerTypeOptions,
  lossMethodOptions,
  modelTypeOptions,
  optimizerOptions
} from '../data/mappings';
import { createLayer, createOpenNNModel, createTorchModel } from '../data/defaults';
import { Layer, OpenNNModel, TorchModel, WorkerModel } from '../data/types';
import WorkerPreview from './WorkerPreview';

const layerTypeLabel = (value: string) =>
  layerTypeOptions.find((option) => option.value === value)?.label ?? 'Layer';

const ModelBuilder = ({
  model,
  onChange,
  onSave,
  onDelete
}: {
  model: WorkerModel;
  onChange: (model: WorkerModel) => void;
  onSave: () => void;
  onDelete?: () => void;
}) => {
  const modelLayers = model.layers ?? [];

  const updateOpenNN = (patch: Partial<OpenNNModel>) => {
    if (model.infraType === 'torch') {
      return;
    }
    onChange({ ...model, ...patch });
  };

  const updateTorch = (patch: Partial<TorchModel>) => {
    if (model.infraType !== 'torch') {
      return;
    }
    onChange({ ...model, ...patch });
  };

  const updateLayer = (index: number, patch: Partial<Layer>) => {
    if (index < 0 || index >= modelLayers.length) {
      return;
    }
    const nextLayers = [...modelLayers];
    nextLayers[index] = { ...nextLayers[index], ...patch };
    onChange({ ...model, layers: nextLayers });
  };

  const addLayer = () => {
    const next = createLayer(modelLayers.length);
    onChange({ ...model, layers: [...modelLayers, next] });
  };

  const removeLayer = (index: number) => {
    const nextLayers = modelLayers.filter((_, idx) => idx !== index);
    onChange({ ...model, layers: nextLayers });
  };

  const moveLayer = (index: number, direction: number) => {
    const nextLayers = [...modelLayers];
    const target = index + direction;
    if (target < 0 || target >= nextLayers.length) {
      return;
    }
    const [item] = nextLayers.splice(index, 1);
    nextLayers.splice(target, 0, item);
    onChange({ ...model, layers: nextLayers });
  };

  const switchToOpenNN = () => onChange(createOpenNNModel(model.name, modelLayers));
  const switchToTorch = () => onChange(createTorchModel(model.name, modelLayers));

  return (
    <div className="panel">
      <div className="panel-header">
        <div>
          <p className="panel-title">Worker Model Lab</p>
          <p className="panel-subtitle">Shape model architecture and export-ready payloads.</p>
        </div>
        <div className="panel-actions">
          <button className="ghost" type="button" onClick={switchToOpenNN}>
            OpenNN
          </button>
          <button className="ghost" type="button" onClick={switchToTorch}>
            Torch
          </button>
        </div>
      </div>

      <div className="model-grid">
        <div className="model-form">
          <label className="field">
            <span>Model Name</span>
            <input
              type="text"
              value={model.name}
              onChange={(event) => onChange({ ...model, name: event.target.value })}
            />
          </label>

          <div className="inline-fields">
            <label className="field">
              <span>Infra</span>
              <select
                value={model.infraType}
                onChange={(event) => {
                  if (event.target.value === 'torch') {
                    onChange(createTorchModel(model.name, modelLayers));
                  } else {
                    onChange({
                      ...createOpenNNModel(model.name, modelLayers),
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
                value={model.distributedSystemType}
                onChange={(event) => {
                  if (model.infraType === 'torch') {
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

          {model.infraType !== 'torch' && (
            <>
              <label className="field">
                <span>Model Type</span>
                <select
                  value={model.modelType}
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
                  value={model.modelArgs}
                  onChange={(event) => updateOpenNN({ modelArgs: event.target.value })}
                />
              </label>

              <div className="inline-fields">
                <label className="field">
                  <span>Learning Rate</span>
                  <input
                    type="text"
                    value={model.learningRate}
                    onChange={(event) => updateOpenNN({ learningRate: event.target.value })}
                  />
                </label>
                <label className="field">
                  <span>Epochs</span>
                  <input
                    type="text"
                    value={model.epochs}
                    onChange={(event) => updateOpenNN({ epochs: event.target.value })}
                  />
                </label>
              </div>

              <div className="inline-fields">
                <label className="field">
                  <span>Optimizer</span>
                  <select
                    value={model.optimizer}
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
                    value={model.optimizerArgs}
                    onChange={(event) => updateOpenNN({ optimizerArgs: event.target.value })}
                  />
                </label>
              </div>

              <div className="inline-fields">
                <label className="field">
                  <span>Loss Method</span>
                  <select
                    value={model.lossMethod}
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
                    value={model.lossArgs}
                    onChange={(event) => updateOpenNN({ lossArgs: event.target.value })}
                  />
                </label>
              </div>

              <label className="field">
                <span>Distributed System Args</span>
                <input
                  type="text"
                  value={model.distributedSystemArgs}
                  onChange={(event) => updateOpenNN({ distributedSystemArgs: event.target.value })}
                />
              </label>
              <label className="field">
                <span>Distributed System Token</span>
                <input
                  type="text"
                  value={model.distributedSystemToken}
                  onChange={(event) => updateOpenNN({ distributedSystemToken: event.target.value })}
                />
              </label>
            </>
          )}

          {model.infraType === 'torch' && (
            <>
              <label className="field">
                <span>TorchScript Path</span>
                <input
                  type="text"
                  value={model.ptPath}
                  onChange={(event) => updateTorch({ ptPath: event.target.value })}
                />
              </label>
              <div className="inline-fields">
                <label className="field">
                  <span>Format</span>
                  <input
                    type="text"
                    value={model.ptFormat}
                    onChange={(event) => updateTorch({ ptFormat: event.target.value })}
                  />
                </label>
                <label className="field">
                  <span>Checksum</span>
                  <input
                    type="text"
                    value={model.ptChecksum}
                    onChange={(event) => updateTorch({ ptChecksum: event.target.value })}
                  />
                </label>
              </div>
              <label className="field">
                <span>Description</span>
                <input
                  type="text"
                  value={model.ptDescription}
                  onChange={(event) => updateTorch({ ptDescription: event.target.value })}
                />
              </label>
              <div className="inline-fields">
                <label className="field">
                  <span>Train LR</span>
                  <input
                    type="text"
                    value={model.trainParams.lr}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, lr: event.target.value }
                      })
                    }
                  />
                </label>
                <label className="field">
                  <span>Train Epochs</span>
                  <input
                    type="text"
                    value={model.trainParams.epochs}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, epochs: event.target.value }
                      })
                    }
                  />
                </label>
                <label className="field">
                  <span>Train Optimizer</span>
                  <input
                    type="text"
                    value={model.trainParams.optimizer}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, optimizer: event.target.value }
                      })
                    }
                  />
                </label>
              </div>
              <div className="inline-fields">
                <label className="field">
                  <span>Train Batch Size</span>
                  <input
                    type="text"
                    value={model.trainParams.batchSize}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, batchSize: event.target.value }
                      })
                    }
                  />
                </label>
                <label className="field">
                  <span>Input Tensor Shape</span>
                  <input
                    type="text"
                    value={model.trainParams.inputTensorShape}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: {
                          ...model.trainParams,
                          inputTensorShape: event.target.value
                        }
                      })
                    }
                  />
                </label>
                <label className="field">
                  <span>Labels Shape</span>
                  <input
                    type="text"
                    value={model.trainParams.labelsShape}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, labelsShape: event.target.value }
                      })
                    }
                  />
                </label>
              </div>
              <div className="inline-fields">
                <label className="field">
                  <span>Labels Offset</span>
                  <input
                    type="text"
                    value={model.trainParams.labelsOffset}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, labelsOffset: event.target.value }
                      })
                    }
                  />
                </label>
                <label className="field">
                  <span>Weight Init Random</span>
                  <input
                    type="text"
                    value={model.trainParams.wInitRand}
                    onChange={(event) =>
                      updateTorch({
                        trainParams: { ...model.trainParams, wInitRand: event.target.value }
                      })
                    }
                  />
                </label>
              </div>
              <label className="field">
                <span>Distributed System Args</span>
                <input
                  type="text"
                  value={model.distributedSystemArgs}
                  onChange={(event) => updateTorch({ distributedSystemArgs: event.target.value })}
                />
              </label>
              <label className="field">
                <span>Distributed System Token</span>
                <input
                  type="text"
                  value={model.distributedSystemToken}
                  onChange={(event) => updateTorch({ distributedSystemToken: event.target.value })}
                />
              </label>
            </>
          )}

          <div className="layers-header">
            <div>
              <h3>Layers</h3>
              <p>
                {model.infraType === 'torch'
                  ? 'Exported as OpenNN-compatible metadata for Torch models.'
                  : 'Use CNN syntax for complex layers. One row per layer.'}
              </p>
            </div>
            <button className="ghost" type="button" onClick={addLayer}>
              Add Layer
            </button>
          </div>

          <div className="layers-list">
            {modelLayers.map((layer, index) => {
              const functionOptions =
                functionOptionsByLayerType[layer.type] ?? activationFunctionOptions;

              return (
                <div key={layer.id} className="layer-row">
                  <div className="layer-index">{index + 1}</div>
                  <label className="field">
                    <span>Size</span>
                    <input
                      type="text"
                      value={layer.size}
                      onChange={(event) => updateLayer(index, { size: event.target.value })}
                    />
                  </label>
                  <label className="field">
                    <span>Type</span>
                    <select
                      value={layer.type}
                      onChange={(event) =>
                        updateLayer(index, {
                          type: event.target.value,
                          functionCode:
                            functionOptionsByLayerType[event.target.value]?.[0]?.value ??
                            layer.functionCode
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
                      value={layer.functionCode}
                      onChange={(event) => updateLayer(index, { functionCode: event.target.value })}
                    >
                      {functionOptions.map((option) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </select>
                  </label>
                  <div className="layer-actions">
                    <button
                      className="ghost"
                      type="button"
                      onClick={() => moveLayer(index, -1)}
                      aria-label={`Move ${layerTypeLabel(layer.type)} up`}
                    >
                      Up
                    </button>
                    <button
                      className="ghost"
                      type="button"
                      onClick={() => moveLayer(index, 1)}
                      aria-label={`Move ${layerTypeLabel(layer.type)} down`}
                    >
                      Down
                    </button>
                    <button
                      className="ghost danger"
                      type="button"
                      onClick={() => removeLayer(index)}
                      aria-label={`Remove ${layerTypeLabel(layer.type)}`}
                    >
                      Remove
                    </button>
                  </div>
                </div>
              );
            })}
          </div>

          <div className="panel-actions align-right">
            {onDelete && (
              <button className="ghost danger" type="button" onClick={onDelete}>
                Delete Model
              </button>
            )}
            <button className="primary" type="button" onClick={onSave}>
              Save Model
            </button>
          </div>
        </div>

        <div className="model-preview">
          <div className="preview-card">
            <div className="preview-header">
              <h3>Network Preview</h3>
              <p>
                {model.infraType === 'torch'
                  ? 'Torch pipelines render as metadata, with optional layer export.'
                  : 'Live layer graph per worker.'}
              </p>
            </div>
            {model.infraType === 'torch' ? (
              <div className="torch-preview">
                <div>
                  <span className="preview-label">TorchScript</span>
                  <strong>{model.ptPath || 'No model path yet'}</strong>
                </div>
                <div>
                  <span className="preview-label">Train Params</span>
                  <p>
                    lr {model.trainParams.lr} | epochs {model.trainParams.epochs} | opt {model.trainParams.optimizer}
                  </p>
                </div>
                <div>
                  <span className="preview-label">Shapes</span>
                  <p>
                    input {model.trainParams.inputTensorShape} | labels {model.trainParams.labelsShape}
                  </p>
                </div>
              </div>
            ) : (
              <WorkerPreview layers={modelLayers} />
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ModelBuilder;

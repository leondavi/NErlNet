import { useEffect, useMemo, useState } from 'react';
import { PlannerState } from '../data/types';
import { buildConnectionMap, buildDistributedConfig, buildExperimentFlow } from '../utils/exporters';
import { importConnectionMap, importDistributedConfig, importExperimentFlow } from '../utils/importers';

const ExportView = ({
  state,
  onChange
}: {
  state: PlannerState;
  onChange: (next: PlannerState) => void;
}) => {
  const [includeDocs, setIncludeDocs] = useState(true);
  const [dcPreview, setDcPreview] = useState('');
  const [connPreview, setConnPreview] = useState('');
  const [expPreview, setExpPreview] = useState('');

  const connJson = useMemo(() => buildConnectionMap(state.connections), [state.connections]);
  const expJson = useMemo(() => buildExperimentFlow(state.experimentFlow), [state.experimentFlow]);

  useEffect(() => {
    let active = true;
    (async () => {
      const dcJson = await buildDistributedConfig(state, includeDocs);
      if (active) {
        setDcPreview(JSON.stringify(dcJson, null, 2));
      }
    })();
    return () => {
      active = false;
    };
  }, [state, includeDocs]);

  useEffect(() => {
    setConnPreview(JSON.stringify(connJson, null, 2));
  }, [connJson]);

  useEffect(() => {
    setExpPreview(JSON.stringify(expJson, null, 2));
  }, [expJson]);

  const downloadJson = (content: string, filename: string) => {
    const blob = new Blob([content], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    link.remove();
    URL.revokeObjectURL(url);
  };

  const getExportBaseName = () => {
    const rawName = state.experimentFlow.experimentName?.trim() || 'nerlnet';
    return rawName.replace(/[^a-zA-Z0-9_-]+/g, '_');
  };

  const handleImport = async (
    event: React.ChangeEvent<HTMLInputElement>,
    type: 'dc' | 'conn' | 'exp'
  ) => {
    const file = event.target.files?.[0];
    if (!file) {
      return;
    }
    const text = await file.text();
    const json = JSON.parse(text) as Record<string, unknown>;
    if (type === 'dc') {
      onChange(importDistributedConfig(json, state));
    }
    if (type === 'conn') {
      onChange({ ...state, connections: importConnectionMap(json) });
    }
    if (type === 'exp') {
      onChange({ ...state, experimentFlow: importExperimentFlow(json) });
    }
    event.target.value = '';
  };

  return (
    <div className="view">
      <div className="hero compact">
        <div>
          <p className="eyebrow">Export</p>
          <h1>Export configs as clean JSON files.</h1>
          <p>Preview, download, and re-import Nerlnet configurations.</p>
        </div>
        <div className="hero-badge">
          <div>
            <span>Docs in DC</span>
            <strong>{includeDocs ? 'On' : 'Off'}</strong>
          </div>
          <div>
            <span>Artifacts</span>
            <strong>3</strong>
          </div>
        </div>
      </div>

      <div className="panel">
        <div className="panel-header">
          <div>
            <p className="panel-title">Distributed Config</p>
            <p className="panel-subtitle">Includes workers, devices, and model SHA mapping.</p>
          </div>
          <div className="panel-actions">
            <label className="toggle">
              <input
                type="checkbox"
                checked={includeDocs}
                onChange={(event) => setIncludeDocs(event.target.checked)}
              />
              <span>Include docs</span>
            </label>
            <label className="ghost">
              Import DC
              <input type="file" accept="application/json" onChange={(event) => handleImport(event, 'dc')} />
            </label>
            <button
              className="primary"
              type="button"
              onClick={() => downloadJson(dcPreview, `dc_${getExportBaseName()}.json`)}
            >
              Download DC
            </button>
          </div>
        </div>
        <textarea className="json-preview" value={dcPreview} readOnly />
      </div>

      <div className="grid two">
        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Connection Map</p>
              <p className="panel-subtitle">Router topology and entity routes.</p>
            </div>
            <div className="panel-actions">
              <label className="ghost">
                Import Conn
                <input type="file" accept="application/json" onChange={(event) => handleImport(event, 'conn')} />
              </label>
              <button
                className="primary"
                type="button"
                onClick={() => downloadJson(connPreview, `conn_${getExportBaseName()}.json`)}
              >
                Download Conn
              </button>
            </div>
          </div>
          <textarea className="json-preview" value={connPreview} readOnly />
        </div>

        <div className="panel">
          <div className="panel-header">
            <div>
              <p className="panel-title">Experiment Flow</p>
              <p className="panel-subtitle">Phases and source piece scheduling.</p>
            </div>
            <div className="panel-actions">
              <label className="ghost">
                Import Exp
                <input type="file" accept="application/json" onChange={(event) => handleImport(event, 'exp')} />
              </label>
              <button
                className="primary"
                type="button"
                onClick={() => downloadJson(expPreview, `exp_${getExportBaseName()}.json`)}
              >
                Download Exp
              </button>
            </div>
          </div>
          <textarea className="json-preview" value={expPreview} readOnly />
        </div>
      </div>
    </div>
  );
};

export default ExportView;

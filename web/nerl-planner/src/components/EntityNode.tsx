import { Handle, NodeProps, Position } from '@xyflow/react';

export type EntityNodeData = {
  name: string;
  kind: 'router' | 'source' | 'client' | 'server' | 'super';
  role?: 'main' | 'api';
  deviceName?: string;
  deviceIp?: string;
  workers?: string[];
  warning?: string;
};

const EntityNode = (props: NodeProps) => {
  const data = props.data as EntityNodeData;
  const deviceLabel = data.deviceName
    ? `${data.deviceName}${data.deviceIp ? ` - ${data.deviceIp}` : ''}`
    : 'Unassigned device';

  return (
    <div className="entity-card">
      <Handle type="target" position={Position.Left} />
      <div className="entity-header">
        <span className="entity-kind">
          {data.kind === 'server'
            ? data.role === 'main'
              ? 'Main Server'
              : 'API Server'
            : data.kind === 'super'
              ? 'Super Node'
            : data.kind.charAt(0).toUpperCase() + data.kind.slice(1)}
        </span>
        <strong className="entity-name">{data.name}</strong>
      </div>
      <div className="entity-meta">
        <span>{deviceLabel}</span>
      </div>
      {data.kind === 'client' && (
        <div className="entity-workers">
          <span>Workers</span>
          <p>{data.workers && data.workers.length > 0 ? data.workers.join(', ') : 'None'}</p>
        </div>
      )}
      {data.warning && <div className="entity-warning">{data.warning}</div>}
      <Handle type="source" position={Position.Right} />
    </div>
  );
};

export default EntityNode;

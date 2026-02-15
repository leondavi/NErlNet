import { Handle, NodeProps, Position } from '@xyflow/react';

export type ModelGraphNodeData = {
  label: string;
  subtitle?: string;
  info?: string[];
  warning?: string;
  tone?: string;
  layout?: 'horizontal' | 'vertical' | 'free';
};

const ModelGraphNode = ({ data }: NodeProps<ModelGraphNodeData>) => {
  const toneClass = data.tone ? ` ${data.tone}` : '';
  const isVertical = data.layout === 'vertical';
  const targetPosition = isVertical ? Position.Top : Position.Left;
  const sourcePosition = isVertical ? Position.Bottom : Position.Right;
  return (
    <div className={`model-node${toneClass}`}>
      <Handle type="target" position={targetPosition} />
      <div className="model-node-header">
        <strong>{data.label}</strong>
        {data.subtitle && <span>{data.subtitle}</span>}
      </div>
      {data.info && data.info.length > 0 && (
        <div className="model-node-info">
          {data.info.map((line, index) => (
            <span key={`${line}-${index}`}>{line}</span>
          ))}
        </div>
      )}
      {data.warning && <div className="model-node-warning">{data.warning}</div>}
      <Handle type="source" position={sourcePosition} />
    </div>
  );
};

export default ModelGraphNode;

import { Handle, NodeProps, Position } from '@xyflow/react';
import { TorchLayerType } from '../data/types';

export type TorchGraphNodeData = {
  label: string;
  type: TorchLayerType;
  inputShape: string;
  outputShape: string;
  warning?: string;
};

const TorchGraphNode = (props: NodeProps) => {
  const data = props.data as TorchGraphNodeData;
  return (
    <div className={`torch-node torch-${data.type}`}>
      <Handle type="target" position={Position.Left} />
      <div className="torch-node-header">
        <strong>{data.label}</strong>
        <span>{data.type}</span>
      </div>
      <div className="torch-node-shapes">
        <span>in {data.inputShape}</span>
        <span>out {data.outputShape}</span>
      </div>
      {data.warning && <div className="torch-node-warning">{data.warning}</div>}
      <Handle type="source" position={Position.Right} />
    </div>
  );
};

export default TorchGraphNode;

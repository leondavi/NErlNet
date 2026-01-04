import { Layer } from '../data/types';
import { layerTypeLabelByValue } from '../data/mappings';

const NODE_WIDTH = 220;
const NODE_HEIGHT = 60;
const NODE_GAP = 28;
const PADDING = 24;

const WorkerPreview = ({ layers }: { layers: Layer[] }) => {
  const height = layers.length * (NODE_HEIGHT + NODE_GAP) + PADDING * 2 - NODE_GAP;
  const width = NODE_WIDTH + PADDING * 2;

  return (
    <div className="worker-preview">
      <svg width="100%" height={Math.max(height, 160)} viewBox={`0 0 ${width} ${Math.max(height, 160)}`}>
        {layers.map((layer, index) => {
          const x = PADDING;
          const y = PADDING + index * (NODE_HEIGHT + NODE_GAP);
          const label = layerTypeLabelByValue[layer.type] ?? 'Layer';
          const size = layer.size || '...';

          return (
            <g key={layer.id}>
              {index > 0 && (
                <line
                  x1={x + NODE_WIDTH / 2}
                  y1={y - NODE_GAP + 6}
                  x2={x + NODE_WIDTH / 2}
                  y2={y}
                  stroke="var(--ink-soft)"
                  strokeWidth="2"
                  strokeLinecap="round"
                />
              )}
              <rect
                x={x}
                y={y}
                rx="14"
                ry="14"
                width={NODE_WIDTH}
                height={NODE_HEIGHT}
                fill="var(--card)"
                stroke="var(--stroke)"
                strokeWidth="1.4"
              />
              <text x={x + 16} y={y + 24} className="worker-preview-title">
                {label}
              </text>
              <text x={x + 16} y={y + 44} className="worker-preview-meta">
                Size: {size}
              </text>
            </g>
          );
        })}
      </svg>
    </div>
  );
};

export default WorkerPreview;

import { ValidationIssue } from '../utils/validation';

type ValidationPanelProps = {
  issues: ValidationIssue[];
  title?: string;
  subtitle?: string;
  showEmpty?: boolean;
  emptyMessage?: string;
};

const ValidationPanel = ({
  issues,
  title = 'Validation',
  subtitle,
  showEmpty = false,
  emptyMessage = 'No validation issues detected.'
}: ValidationPanelProps) => {
  const errors = issues.filter((issue) => issue.severity === 'error');
  const warnings = issues.filter((issue) => issue.severity === 'warning');
  const sortedIssues = [...errors, ...warnings];

  if (!showEmpty && sortedIssues.length === 0) {
    return null;
  }

  return (
    <div className="panel validation-panel">
      <div className="panel-header">
        <div>
          <p className="panel-title">{title}</p>
          <p className="panel-subtitle">
            {subtitle ??
              (sortedIssues.length === 0
                ? emptyMessage
                : 'Fix errors to unblock export. Warnings are advisory.')}
          </p>
        </div>
        <div className="validation-summary">
          <span className={`validation-pill ${errors.length > 0 ? 'error' : 'ok'}`}>
            Errors: {errors.length}
          </span>
          <span className={`validation-pill ${warnings.length > 0 ? 'warning' : 'ok'}`}>
            Warnings: {warnings.length}
          </span>
        </div>
      </div>
      {sortedIssues.length === 0 ? (
        <p className="validation-empty">{emptyMessage}</p>
      ) : (
        <div className="validation-list">
          {sortedIssues.map((issue) => (
            <div key={issue.id} className={`validation-item ${issue.severity}`}>
              <div>
                <strong>{issue.message}</strong>
                {issue.detail && <span className="validation-detail">{issue.detail}</span>}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default ValidationPanel;

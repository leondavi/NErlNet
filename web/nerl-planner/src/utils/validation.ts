import { PlannerState, TorchModel, WorkerModel } from '../data/types';
import { inferTorchGraph, parseShape } from './torchGraph';

export type ValidationScope = 'sandbox' | 'models' | 'experiment' | 'export';
export type ValidationSeverity = 'error' | 'warning';

export type ValidationIssue = {
  id: string;
  severity: ValidationSeverity;
  message: string;
  detail?: string;
  scope: ValidationScope[];
};

export type ValidationResult = {
  issues: ValidationIssue[];
  errors: ValidationIssue[];
  warnings: ValidationIssue[];
};

const ensureExportScope = (scopes: ValidationScope[]): ValidationScope[] =>
  Array.from(new Set<ValidationScope>([...scopes, 'export']));

const addIssue = (
  issues: ValidationIssue[],
  severity: ValidationSeverity,
  message: string,
  scope: ValidationScope[],
  detail?: string
) => {
  issues.push({
    id: `${severity}-${message}-${issues.length}`,
    severity,
    message,
    detail,
    scope: ensureExportScope(scope)
  });
};

const isNumeric = (value: string) => {
  if (value.trim() === '') {
    return false;
  }
  return !Number.isNaN(Number(value));
};

const toNumber = (value: string) => (isNumeric(value) ? Number(value) : null);

const isValidIPv4 = (ip: string): boolean => {
  const pattern = /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/;
  return pattern.test(ip.trim());
};

const validNerltensorTypes = new Set(['float', 'int16', 'int32', 'double', 'uint8']);

export const validatePlannerState = (state: PlannerState): ValidationResult => {
  const issues: ValidationIssue[] = [];

  const entityEntries = [
    { name: 'mainServer', kind: 'server', port: state.servers.mainServer.port },
    { name: 'apiServer', kind: 'server', port: state.servers.apiServer.port },
    ...state.routers.map((router) => ({ name: router.name, kind: 'router', port: router.port })),
    ...state.sources.map((source) => ({ name: source.name, kind: 'source', port: source.port })),
    ...state.clients.map((client) => ({ name: client.name, kind: 'client', port: client.port })),
    ...state.superNodes.map((superNode) => ({ name: superNode.name, kind: 'superNode', port: superNode.port }))
  ].filter((entry) => entry.name.trim() !== '');

  const entityNames = entityEntries.map((entry) => entry.name);
  const entityNameSet = new Set<string>();
  entityNames.forEach((name) => {
    if (entityNameSet.has(name)) {
      addIssue(
        issues,
        'error',
        `Duplicate entity name detected: ${name}.`,
        ['sandbox']
      );
    }
    entityNameSet.add(name);
  });

  if (entityNames.length === 0) {
    addIssue(issues, 'error', 'No entities defined in the cluster.', ['sandbox']);
  }

  if (state.sources.length === 0) {
    addIssue(
      issues,
      'error',
      'No sources defined. Add at least one source to feed data.',
      ['sandbox', 'experiment']
    );
  }

  const entityAssignments = new Map<string, string[]>();
  state.devices.forEach((device) => {
    device.entities.forEach((entity) => {
      if (!entityAssignments.has(entity)) {
        entityAssignments.set(entity, []);
      }
      entityAssignments.get(entity)?.push(device.name);
    });
  });

  const duplicateDeviceIps = new Set<string>();
  const deviceIpSet = new Set<string>();
  state.devices.forEach((device) => {
    if (!device.ipv4.trim()) {
      addIssue(
        issues,
        'error',
        `Device ${device.name || '(unnamed)'} is missing an IPv4 address.`,
        ['sandbox']
      );
      return;
    }
    if (!isValidIPv4(device.ipv4)) {
      addIssue(
        issues,
        'error',
        `Device ${device.name || '(unnamed)'} has an invalid IPv4 address: ${device.ipv4}.`,
        ['sandbox']
      );
      return;
    }
    if (deviceIpSet.has(device.ipv4)) {
      duplicateDeviceIps.add(device.ipv4);
    }
    deviceIpSet.add(device.ipv4);
  });
  if (duplicateDeviceIps.size > 0) {
    addIssue(
      issues,
      'warning',
      `Duplicate device IPv4 detected: ${Array.from(duplicateDeviceIps).join(', ')}.`,
      ['sandbox']
    );
  }

  entityEntries.forEach((entry) => {
    const assigned = entityAssignments.get(entry.name) ?? [];
    if (assigned.length === 0) {
      addIssue(
        issues,
        'error',
        `Entity ${entry.name} is not assigned to a device.`,
        ['sandbox']
      );
    }
    if (assigned.length > 1) {
      addIssue(
        issues,
        'error',
        `Entity ${entry.name} is assigned to multiple devices: ${assigned.join(', ')}.`,
        ['sandbox']
      );
    }
  });

  const entityPorts = new Map<string, number>();
  entityEntries.forEach((entry) => {
    if (!isNumeric(entry.port)) {
      addIssue(
        issues,
        'error',
        `Port for ${entry.kind} ${entry.name} is invalid.`,
        ['sandbox']
      );
      return;
    }
    const port = Number(entry.port);
    if (port < 1 || port > 65535) {
      addIssue(
        issues,
        'error',
        `Port for ${entry.kind} ${entry.name} must be between 1 and 65535.`,
        ['sandbox']
      );
      return;
    }
    entityPorts.set(entry.name, port);
  });

  state.devices.forEach((device) => {
    const ports = new Map<number, string[]>();
    device.entities.forEach((entity) => {
      const port = entityPorts.get(entity);
      if (port === undefined) {
        if (!entityNameSet.has(entity)) {
          addIssue(
            issues,
            'error',
            `Device ${device.name} references unknown entity ${entity}.`,
            ['sandbox']
          );
        }
        return;
      }
      if (!ports.has(port)) {
        ports.set(port, []);
      }
      ports.get(port)?.push(entity);
    });
    ports.forEach((entities, port) => {
      if (entities.length > 1) {
        addIssue(
          issues,
          'error',
          `Device ${device.name} has duplicate port ${port} used by ${entities.join(', ')}.`,
          ['sandbox']
        );
      }
    });
  });

  const adjacency = new Map<string, Set<string>>();
  entityNames.forEach((name) => adjacency.set(name, new Set()));
  if (state.connections.length === 0 && entityNames.length > 1) {
    addIssue(issues, 'error', 'No connections defined between entities.', ['sandbox']);
  }
  state.connections.forEach((edge) => {
    if (!edge.from || !edge.to) {
      return;
    }
    if (edge.from === edge.to) {
      addIssue(issues, 'warning', `Self-connection detected on ${edge.from}.`, ['sandbox']);
      return;
    }
    if (!entityNameSet.has(edge.from) || !entityNameSet.has(edge.to)) {
      addIssue(
        issues,
        'error',
        `Connection references unknown entities: ${edge.from} -> ${edge.to}.`,
        ['sandbox']
      );
      return;
    }
    adjacency.get(edge.from)?.add(edge.to);
    adjacency.get(edge.to)?.add(edge.from);
  });

  if (entityNames.length > 1) {
    const visited = new Set<string>();
    const queue = [entityNames[0]];
    while (queue.length > 0) {
      const current = queue.shift();
      if (!current || visited.has(current)) {
        continue;
      }
      visited.add(current);
      adjacency.get(current)?.forEach((next) => {
        if (!visited.has(next)) {
          queue.push(next);
        }
      });
    }
    if (visited.size !== entityNames.length) {
      const missing = entityNames.filter((name) => !visited.has(name));
      addIssue(
        issues,
        'error',
        `Topology is not fully connected. Missing links for: ${missing.join(', ')}.`,
        ['sandbox']
      );
    }
  }

  if (state.sources.length > 0) {
    const reachable = new Set<string>();
    const queue = state.sources.map((source) => source.name).filter(Boolean);
    while (queue.length > 0) {
      const current = queue.shift();
      if (!current || reachable.has(current)) {
        continue;
      }
      reachable.add(current);
      adjacency.get(current)?.forEach((next) => {
        if (!reachable.has(next)) {
          queue.push(next);
        }
      });
    }
    const unreachable = entityNames.filter((name) => !reachable.has(name));
    if (unreachable.length > 0) {
      addIssue(
        issues,
        'warning',
        `Entities unreachable from sources: ${unreachable.join(', ')}.`,
        ['sandbox']
      );
    }
  }

  const workerNames = new Set(state.workers.map((worker) => worker.name));
  const workerPipelineStages = new Map(
    state.workers.map((worker) => [worker.name, toNumber(worker.parallel.pipelineStage)])
  );
  const modelIds = new Set(state.models.map((model) => model.id));
  const assignedWorkers = new Set(state.clients.flatMap((client) => client.workers));
  const superNodeNames = new Set(state.superNodes.map((superNode) => superNode.name));

  state.clients.forEach((client) => {
    if (client.workers.length === 0) {
      addIssue(
        issues,
        'warning',
        `Client ${client.name} has no workers assigned.`,
        ['sandbox', 'models']
      );
    }
    client.workers.forEach((worker) => {
      if (!workerNames.has(worker)) {
        addIssue(
          issues,
          'error',
          `Client ${client.name} references unknown worker ${worker}.`,
          ['sandbox', 'models']
        );
      }
    });
    if (client.superNode && client.superNode.trim()) {
      if (!superNodeNames.has(client.superNode)) {
        addIssue(
          issues,
          'error',
          `Client ${client.name} references unknown super node ${client.superNode}.`,
          ['sandbox', 'experiment']
        );
      }
    }
  });

  state.superNodes.forEach((superNode) => {
    if (superNode.managedClients.length === 0) {
      addIssue(
        issues,
        'warning',
        `Super node ${superNode.name} has no managed clients.`,
        ['sandbox', 'experiment']
      );
    }
    superNode.managedClients.forEach((clientName) => {
      if (!state.clients.some((client) => client.name === clientName)) {
        addIssue(
          issues,
          'error',
          `Super node ${superNode.name} references unknown client ${clientName}.`,
          ['sandbox', 'experiment']
        );
      }
    });
    if (!isNumeric(superNode.heartbeatMs) || Number(superNode.heartbeatMs) < 1) {
      addIssue(
        issues,
        'error',
        `Super node ${superNode.name} heartbeat must be a positive number.`,
        ['sandbox']
      );
    }
    if (
      !isNumeric(superNode.maxInflightMicrobatches) ||
      Number(superNode.maxInflightMicrobatches) < 1
    ) {
      addIssue(
        issues,
        'error',
        `Super node ${superNode.name} max inflight microbatches must be positive.`,
        ['sandbox']
      );
    }
  });

  state.workers.forEach((worker) => {
    if (!modelIds.has(worker.modelId)) {
      addIssue(
        issues,
        'error',
        `Worker ${worker.name} is missing a valid model assignment.`,
        ['models']
      );
    }
    if (!assignedWorkers.has(worker.name)) {
      addIssue(
        issues,
        'warning',
        `Worker ${worker.name} is not assigned to any client.`,
        ['models']
      );
    }
    if (worker.parallel) {
      const stage = toNumber(worker.parallel.pipelineStage);
      const stageWorld = toNumber(worker.parallel.pipelineWorldSize);
      const tpRank = toNumber(worker.parallel.tpRank);
      const tpWorld = toNumber(worker.parallel.tpWorldSize);

      if (worker.parallel.pipelineStage && (stage === null || stage < 0)) {
        addIssue(
          issues,
          'error',
          `Worker ${worker.name} has invalid pipeline stage.`,
          ['models', 'experiment']
        );
      }
      if (worker.parallel.pipelineWorldSize && (stageWorld === null || stageWorld < 1)) {
        addIssue(
          issues,
          'error',
          `Worker ${worker.name} has invalid pipeline world size.`,
          ['models', 'experiment']
        );
      }
      if (stage !== null && stageWorld !== null && stage >= stageWorld) {
        addIssue(
          issues,
          'error',
          `Worker ${worker.name} pipeline stage must be lower than pipeline world size.`,
          ['models', 'experiment']
        );
      }

      if (worker.parallel.tpRank && (tpRank === null || tpRank < 0)) {
        addIssue(
          issues,
          'error',
          `Worker ${worker.name} has invalid TP rank.`,
          ['models', 'experiment']
        );
      }
      if (worker.parallel.tpWorldSize && (tpWorld === null || tpWorld < 1)) {
        addIssue(
          issues,
          'error',
          `Worker ${worker.name} has invalid TP world size.`,
          ['models', 'experiment']
        );
      }
      if (tpRank !== null && tpWorld !== null && tpRank >= tpWorld) {
        addIssue(
          issues,
          'error',
          `Worker ${worker.name} TP rank must be lower than TP world size.`,
          ['models', 'experiment']
        );
      }
    }
  });

  const usedModelIds = new Set(state.workers.map((worker) => worker.modelId).filter(Boolean));
  const usedModels = state.models.filter((model) => usedModelIds.has(model.id));

  usedModels.forEach((model) => {
    const distributedType = String(model.distributedSystemType ?? '0');
    if (distributedType !== '0') {
      const token = String(model.distributedSystemToken ?? '').trim();
      if (token.length !== 5 || token.toLowerCase() === 'none') {
        addIssue(
          issues,
          'error',
          `Distributed token for model ${model.name} must be 5 characters and not 'none' (got '${token || 'empty'}').`,
          ['models']
        );
      }
    }
    if (Array.isArray(model.tpPlan)) {
      model.tpPlan.forEach((entry, index) => {
        if (!entry.layer.trim()) {
          addIssue(
            issues,
            'error',
            `Model ${model.name} tpPlan entry #${index + 1} is missing layer.`,
            ['models']
          );
        }
        if (!['column', 'row'].includes(entry.mode)) {
          addIssue(
            issues,
            'error',
            `Model ${model.name} tpPlan entry #${index + 1} has invalid mode.`,
            ['models']
          );
        }
        if (!entry.group.trim()) {
          addIssue(
            issues,
            'error',
            `Model ${model.name} tpPlan entry #${index + 1} is missing TP group.`,
            ['models']
          );
        }
        if (!isNumeric(entry.shardAxis)) {
          addIssue(
            issues,
            'error',
            `Model ${model.name} tpPlan entry #${index + 1} has invalid shard axis.`,
            ['models']
          );
        }
      });
    }
  });

  const batchSizeNumeric = toNumber(state.settings.batchSize);
  if (batchSizeNumeric === null || batchSizeNumeric < 1) {
    addIssue(issues, 'error', 'Cluster batch size must be a positive number.', ['sandbox']);
  }

  if (!isNumeric(state.settings.frequency)) {
    addIssue(issues, 'error', 'Cluster frequency must be numeric.', ['sandbox']);
  }

  usedModels.forEach((model) => {
    if (model.infraType === 'torch') {
      const torchModel = model as TorchModel;
      if (!torchModel.graph || !Array.isArray(torchModel.graph.nodes)) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} graph data is missing.`,
          ['models']
        );
        return;
      }
      if (!torchModel.trainParams) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} training params are missing.`,
          ['models']
        );
        return;
      }
      if (!torchModel.ptPath.trim()) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} is missing a TorchScript path.`,
          ['models']
        );
      }
      if (torchModel.graph.nodes.length === 0) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} has no layers in the graph.`,
          ['models']
        );
      }
      const inferred = inferTorchGraph(torchModel.graph);
      inferred.warnings.forEach((warning, index) => {
        addIssue(
          issues,
          warning.severity === 'error' ? 'error' : 'warning',
          `Torch graph: ${warning.message}`,
          ['models'],
          `Node ${warning.nodeId} (#${index + 1})`
        );
      });

      if (!parseShape(torchModel.graph.inputShape)) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} has an invalid input shape.`,
          ['models']
        );
      }

      const inputShapeOk = parseShape(torchModel.trainParams.inputTensorShape);
      if (!inputShapeOk) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} input tensor shape is invalid.`,
          ['models']
        );
      }
      const labelShapeOk = parseShape(torchModel.trainParams.labelsShape);
      if (!labelShapeOk) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} labels shape is invalid.`,
          ['models']
        );
      }

      const optimizer = torchModel.trainParams.optimizer?.toLowerCase().trim();
      if (optimizer && !['adam', 'sgd'].includes(optimizer)) {
        addIssue(
          issues,
          'warning',
          `Torch optimizer '${torchModel.trainParams.optimizer}' is not supported (only Adam/SGD).`,
          ['models']
        );
      }

      const loss = torchModel.trainParams.loss?.toLowerCase().trim();
      if (loss && loss !== 'mse') {
        addIssue(
          issues,
          'warning',
          `Torch loss '${torchModel.trainParams.loss}' is ignored (runtime uses MSE).`,
          ['models']
        );
      }

      const torchBatchSize = toNumber(torchModel.trainParams.batchSize);
      if (torchBatchSize === null || torchBatchSize < 1) {
        addIssue(
          issues,
          'error',
          `Torch model ${model.name} batch size must be a positive number.`,
          ['models']
        );
      } else if (batchSizeNumeric !== null && torchBatchSize !== batchSizeNumeric) {
        addIssue(
          issues,
          'warning',
          `Torch model ${model.name} batch size (${torchBatchSize}) differs from cluster batch size (${batchSizeNumeric}).`,
          ['models']
        );
      }
    } else {
      const openModel = model as WorkerModel;
      if (!('layers' in openModel)) {
        return;
      }
      if (openModel.layers.length === 0) {
        addIssue(
          issues,
          'error',
          `OpenNN model ${model.name} has no layers.`,
          ['models']
        );
      }
      openModel.layers.forEach((layer, index) => {
        if (!isNumeric(layer.size) || Number(layer.size) <= 0) {
          addIssue(
            issues,
            'error',
            `OpenNN model ${model.name} layer ${index + 1} size is invalid.`,
            ['models']
          );
        }
      });
      if (!isNumeric(openModel.epochs) || Number(openModel.epochs) <= 0) {
        addIssue(
          issues,
          'error',
          `OpenNN model ${model.name} epochs must be positive.`,
          ['models']
        );
      }
      if (!isNumeric(openModel.learningRate) || Number(openModel.learningRate) <= 0) {
        addIssue(
          issues,
          'error',
          `OpenNN model ${model.name} learning rate must be positive.`,
          ['models']
        );
      }
    }
  });

  const exp = state.experimentFlow;
  if (!exp.experimentType.trim()) {
    addIssue(issues, 'error', 'Experiment type is required.', ['experiment']);
  }
  if (!exp.csvFilePath.trim()) {
    addIssue(issues, 'error', 'Experiment CSV file path is required.', ['experiment']);
  } else if (!exp.csvFilePath.trim().toLowerCase().endsWith('.csv')) {
    addIssue(issues, 'warning', 'Experiment CSV path should end with .csv.', ['experiment']);
  }

  if (!isNumeric(exp.numOfFeatures) || Number(exp.numOfFeatures) <= 0) {
    addIssue(issues, 'error', 'Number of features must be a positive number.', ['experiment']);
  }
  if (!isNumeric(exp.numOfLabels) || Number(exp.numOfLabels) <= 0) {
    addIssue(issues, 'error', 'Number of labels must be a positive number.', ['experiment']);
  }

  if (!exp.headersNames.trim()) {
    addIssue(issues, 'warning', 'Headers list is empty.', ['experiment']);
  }

  const expBatch = toNumber(exp.batchSize);
  if (expBatch === null || expBatch < 1) {
    addIssue(issues, 'error', 'Experiment batch size must be a positive number.', ['experiment']);
  } else if (batchSizeNumeric !== null && expBatch !== batchSizeNumeric) {
    addIssue(
      issues,
      'error',
      `Experiment batch size (${expBatch}) does not match cluster batch size (${batchSizeNumeric}).`,
      ['experiment']
    );
  }

  if (exp.phases.length === 0) {
    addIssue(issues, 'error', 'No experiment phases defined.', ['experiment']);
  }

  const phaseNames = new Set<string>();
  exp.phases.forEach((phase) => {
    const parallelMode = phase.parallelExecution?.mode ?? 'legacy';
    if (!phase.phaseName.trim()) {
      addIssue(issues, 'error', 'Phase name is missing.', ['experiment']);
    } else if (phaseNames.has(phase.phaseName)) {
      addIssue(
        issues,
        'error',
        `Duplicate phase name detected: ${phase.phaseName}.`,
        ['experiment']
      );
    } else {
      phaseNames.add(phase.phaseName);
    }
    if (!['training', 'prediction'].includes(phase.phaseType)) {
      addIssue(
        issues,
        'error',
        `Phase ${phase.phaseName} has invalid type ${phase.phaseType}.`,
        ['experiment']
      );
    }
    if (phase.parallelExecution) {
      const mode = phase.parallelExecution.mode;
      if (!['legacy', 'pipeline', 'tensor', 'pipeline_tensor'].includes(mode)) {
        addIssue(
          issues,
          'error',
          `Phase ${phase.phaseName} has unsupported parallel mode ${mode}.`,
          ['experiment']
        );
      }
      if (mode !== 'legacy') {
        if (!phase.parallelExecution.superNode.trim()) {
          addIssue(
            issues,
            'error',
            `Phase ${phase.phaseName} must define a super node for non-legacy parallel mode.`,
            ['experiment']
          );
        } else if (!superNodeNames.has(phase.parallelExecution.superNode)) {
          addIssue(
            issues,
            'error',
            `Phase ${phase.phaseName} references unknown super node ${phase.parallelExecution.superNode}.`,
            ['experiment']
          );
        }
      }
      if (['pipeline', 'pipeline_tensor'].includes(mode)) {
        if (!['gpipe', '1f1b', 'interleaved'].includes(phase.parallelExecution.scheduler)) {
          addIssue(
            issues,
            'error',
            `Phase ${phase.phaseName} has invalid parallel scheduler ${phase.parallelExecution.scheduler}.`,
            ['experiment']
          );
        }
        if (
          !isNumeric(phase.parallelExecution.microBatchSize) ||
          Number(phase.parallelExecution.microBatchSize) < 1
        ) {
          addIssue(
            issues,
            'error',
            `Phase ${phase.phaseName} microBatchSize must be a positive number.`,
            ['experiment']
          );
        }
        if (
          !isNumeric(phase.parallelExecution.numMicroBatches) ||
          Number(phase.parallelExecution.numMicroBatches) < 1
        ) {
          addIssue(
            issues,
            'error',
            `Phase ${phase.phaseName} numMicroBatches must be a positive number.`,
            ['experiment']
          );
        }
        if (phase.parallelExecution.scheduler === 'interleaved') {
          if (
            !isNumeric(phase.parallelExecution.virtualStages) ||
            Number(phase.parallelExecution.virtualStages) < 1
          ) {
            addIssue(
              issues,
              'error',
              `Phase ${phase.phaseName} virtualStages must be a positive number for interleaved scheduling.`,
              ['experiment']
            );
          }
        }
      }
    }
    if (phase.sourcePieces.length === 0) {
      addIssue(
        issues,
        'warning',
        `Phase ${phase.phaseName} has no source pieces.`,
        ['experiment']
      );
    }
    phase.sourcePieces.forEach((piece) => {
      if (!state.sources.some((source) => source.name === piece.sourceName)) {
        addIssue(
          issues,
          'error',
          `Source piece references unknown source ${piece.sourceName}.`,
          ['experiment']
        );
      }
      if (!isNumeric(piece.startingSample) || Number(piece.startingSample) < 0) {
        addIssue(
          issues,
          'error',
          `Source piece for ${piece.sourceName} has invalid starting sample.`,
          ['experiment']
        );
      }
      if (!isNumeric(piece.numOfBatches) || Number(piece.numOfBatches) < 0) {
        addIssue(
          issues,
          'error',
          `Source piece for ${piece.sourceName} has invalid number of batches.`,
          ['experiment']
        );
      }
      piece.workers.forEach((worker) => {
        if (!workerNames.has(worker)) {
          addIssue(
            issues,
            'error',
            `Source piece references unknown worker ${worker}.`,
            ['experiment']
          );
          return;
        }
        if (parallelMode === 'pipeline' || parallelMode === 'pipeline_tensor') {
          const stage = workerPipelineStages.get(worker);
          if (stage === null || stage === undefined) {
            addIssue(
              issues,
              'error',
              `Phase ${phase.phaseName} source ${piece.sourceName} targets worker ${worker} without pipeline stage metadata.`,
              ['experiment']
            );
          } else if (stage !== 0) {
            addIssue(
              issues,
              'error',
              `Phase ${phase.phaseName} source ${piece.sourceName} targets worker ${worker} at stage ${stage}; only stage 0 workers can receive source batches.`,
              ['experiment']
            );
          }
        }
      });
      if (piece.workers.length === 0) {
        addIssue(
          issues,
          'warning',
          `Source piece for ${piece.sourceName} has no workers assigned.`,
          ['experiment']
        );
      }
      if ((parallelMode === 'pipeline' || parallelMode === 'pipeline_tensor') && piece.workers.length === 0) {
        addIssue(
          issues,
          'error',
          `Phase ${phase.phaseName} source ${piece.sourceName} must target at least one stage 0 worker in ${parallelMode} mode.`,
          ['experiment']
        );
      }
      if (!validNerltensorTypes.has(piece.nerltensorType)) {
        addIssue(
          issues,
          'error',
          `Source piece for ${piece.sourceName} uses unsupported nerltensor type '${piece.nerltensorType}'.`,
          ['experiment']
        );
      }
    });
  });

  const errors = issues.filter((issue) => issue.severity === 'error');
  const warnings = issues.filter((issue) => issue.severity === 'warning');

  return { issues, errors, warnings };
};

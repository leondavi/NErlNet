import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { scanNetwork } from './scripts/networkScan';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { spawn } from 'child_process';

const networkScanPlugin = () => ({
  name: 'network-scan',
  configureServer(server: { middlewares: { use: Function } }) {
    server.middlewares.use('/api/scan', async (req: { method?: string }, res: any) => {
      if (req.method !== 'GET') {
        res.statusCode = 405;
        res.end();
        return;
      }
      try {
        const data = await scanNetwork();
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify(data));
      } catch (error) {
        res.statusCode = 500;
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'Scan failed' }));
      }
    });
  },
  configurePreviewServer(server: { middlewares: { use: Function } }) {
    server.middlewares.use('/api/scan', async (req: { method?: string }, res: any) => {
      if (req.method !== 'GET') {
        res.statusCode = 405;
        res.end();
        return;
      }
      try {
        const data = await scanNetwork();
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify(data));
      } catch (error) {
        res.statusCode = 500;
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'Scan failed' }));
      }
    });
  }
});

const torchModelPlugin = () => {
  const rootDir = path.resolve(__dirname, '..', '..');
  const modelsDir = path.join(rootDir, 'nerl_designer_models');
  const scriptPath = path.resolve(__dirname, 'scripts', 'torch_export.py');

  const ensureModelsDir = () => {
    fs.mkdirSync(modelsDir, { recursive: true });
  };

  const safeName = (value: string) => value.replace(/[^a-zA-Z0-9_-]+/g, '_');

  const listModels = () => {
    ensureModelsDir();
    if (!fs.existsSync(modelsDir)) {
      return [];
    }
    return fs
      .readdirSync(modelsDir, { withFileTypes: true })
      .filter((entry) => entry.isDirectory())
      .map((entry) => {
        const configPath = path.join(modelsDir, entry.name, 'model.json');
        if (!fs.existsSync(configPath)) {
          return null;
        }
        try {
          const config = JSON.parse(fs.readFileSync(configPath, 'utf-8')) as {
            name?: string;
            ptPath?: string;
          };
          return {
            name: entry.name,
            label: config.name ?? entry.name,
            configPath: path.relative(rootDir, configPath),
            ptPath: config.ptPath ?? ''
          };
        } catch {
          return null;
        }
      })
      .filter(Boolean);
  };

  const readBody = (req: any) =>
    new Promise<string>((resolve, reject) => {
      let data = '';
      req.on('data', (chunk: Buffer) => {
        data += chunk.toString('utf-8');
      });
      req.on('end', () => resolve(data));
      req.on('error', reject);
    });

  const handleExport = async (req: any, res: any) => {
    if (req.method !== 'POST') {
      res.statusCode = 405;
      res.end();
      return;
    }
    try {
      ensureModelsDir();
      const body = await readBody(req);
      const payload = JSON.parse(body) as Record<string, unknown>;
      const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'nerl-torch-'));
      const configPath = path.join(tmpDir, 'model.json');
      fs.writeFileSync(configPath, JSON.stringify(payload, null, 2));
      const python = process.env.PYTHON || 'python3';
      const child = spawn(python, [
        scriptPath,
        '--config',
        configPath,
        '--output-dir',
        modelsDir,
        '--base-dir',
        rootDir
      ]);
      let stdout = '';
      let stderr = '';
      child.stdout.on('data', (chunk) => {
        stdout += chunk.toString('utf-8');
      });
      child.stderr.on('data', (chunk) => {
        stderr += chunk.toString('utf-8');
      });
      child.on('close', (code) => {
        if (code !== 0) {
          const baseError = (stderr || 'Torch export failed').trim();
          const guidance = baseError.includes('Torch not available')
            ? '\nInstall CPU torch for the planner python interpreter (example: python3 -m pip install torch --index-url https://download.pytorch.org/whl/cpu), or run planner with PYTHON=/path/to/python-with-torch npm run dev.'
            : '';
          res.statusCode = 500;
          res.setHeader('Content-Type', 'application/json');
          res.end(JSON.stringify({ error: `${baseError}${guidance}` }));
          return;
        }
        try {
          const result = JSON.parse(stdout);
          res.setHeader('Content-Type', 'application/json');
          res.end(JSON.stringify(result));
        } catch {
          res.statusCode = 500;
          res.setHeader('Content-Type', 'application/json');
          res.end(JSON.stringify({ error: 'Invalid export response' }));
        }
      });
    } catch (error) {
      res.statusCode = 500;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'Export failed' }));
    }
  };

  const handleImport = (req: any, res: any) => {
    if (req.method !== 'GET') {
      res.statusCode = 405;
      res.end();
      return;
    }
    const url = new URL(req.url ?? '', 'http://localhost');
    const name = safeName(url.searchParams.get('name') ?? '');
    if (!name) {
      res.statusCode = 400;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: 'Missing model name' }));
      return;
    }
    try {
      const configPath = path.join(modelsDir, name, 'model.json');
      const config = JSON.parse(fs.readFileSync(configPath, 'utf-8'));
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify(config));
    } catch (error) {
      res.statusCode = 404;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'Model not found' }));
    }
  };

  const handleList = (_req: any, res: any) => {
    try {
      const models = listModels();
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ models }));
    } catch (error) {
      res.statusCode = 500;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'List failed' }));
    }
  };

  const registerRoutes = (server: { middlewares: { use: Function } }) => {
    server.middlewares.use('/api/torch/export', handleExport);
    server.middlewares.use('/api/torch/import', handleImport);
    server.middlewares.use('/api/torch/models', handleList);
  };

  return {
    name: 'torch-models',
    configureServer(server: { middlewares: { use: Function } }) {
      registerRoutes(server);
    },
    configurePreviewServer(server: { middlewares: { use: Function } }) {
      registerRoutes(server);
    }
  };
};

const hfDatasetsPlugin = () => {
  const rootDir = path.resolve(__dirname, '..', '..');
  const scriptPath = path.resolve(__dirname, 'scripts', 'hf_datasets.py');
  const repoIdsPath = path.resolve(rootDir, 'src_py', 'apiServer', 'hf_repo_ids.json');
  const defaultDownloadDir = '/tmp/nerlnet/data/NerlnetData-master/nerlnet';

  const readBody = (req: any) =>
    new Promise<string>((resolve, reject) => {
      let data = '';
      req.on('data', (chunk: Buffer) => {
        data += chunk.toString('utf-8');
      });
      req.on('end', () => resolve(data));
      req.on('error', reject);
    });

  const runScript = (args: string[]) =>
    new Promise<{ code: number | null; stdout: string; stderr: string }>((resolve) => {
      const python = process.env.PYTHON || 'python3';
      const child = spawn(python, [scriptPath, ...args]);
      let stdout = '';
      let stderr = '';
      child.stdout.on('data', (chunk) => {
        stdout += chunk.toString('utf-8');
      });
      child.stderr.on('data', (chunk) => {
        stderr += chunk.toString('utf-8');
      });
      child.on('close', (code) => resolve({ code, stdout, stderr }));
    });

  const explainFailure = (raw: string) => {
    const baseError = raw.trim() || 'HF dataset operation failed';
    if (baseError.includes('huggingface_hub not available')) {
      return `${baseError}\nInstall huggingface_hub for the planner python interpreter (example: python3 -m pip install huggingface_hub), or run planner with PYTHON=/path/to/python-with-huggingface_hub npm run dev.`;
    }
    return baseError;
  };

  const handleList = async (req: any, res: any) => {
    if (req.method !== 'GET') {
      res.statusCode = 405;
      res.end();
      return;
    }

    try {
      const { code, stdout, stderr } = await runScript([
        '--action',
        'list',
        '--repo-file',
        repoIdsPath
      ]);

      if (code !== 0) {
        res.statusCode = 500;
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify({ error: explainFailure(stderr || stdout) }));
        return;
      }

      const payload = JSON.parse(stdout || '{}');
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify(payload));
    } catch (error) {
      res.statusCode = 500;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'HF list failed' }));
    }
  };

  const handleDownload = async (req: any, res: any) => {
    if (req.method !== 'POST') {
      res.statusCode = 405;
      res.end();
      return;
    }

    try {
      const body = await readBody(req);
      const payload = JSON.parse(body || '{}') as { repoIdx?: number | string };
      const repoIdx = Number(payload.repoIdx);
      if (!Number.isInteger(repoIdx) || repoIdx < 0) {
        res.statusCode = 400;
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify({ error: 'repoIdx must be a non-negative integer' }));
        return;
      }

      const { code, stdout, stderr } = await runScript([
        '--action',
        'download',
        '--repo-file',
        repoIdsPath,
        '--repo-idx',
        String(repoIdx),
        '--download-dir',
        defaultDownloadDir
      ]);

      if (code !== 0) {
        res.statusCode = 500;
        res.setHeader('Content-Type', 'application/json');
        res.end(JSON.stringify({ error: explainFailure(stderr || stdout) }));
        return;
      }

      const responsePayload = JSON.parse(stdout || '{}');
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify(responsePayload));
    } catch (error) {
      res.statusCode = 500;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: error instanceof Error ? error.message : 'HF download failed' }));
    }
  };

  const registerRoutes = (server: { middlewares: { use: Function } }) => {
    server.middlewares.use('/api/hf/datasets/download', handleDownload);
    server.middlewares.use('/api/hf/datasets', handleList);
  };

  return {
    name: 'hf-datasets',
    configureServer(server: { middlewares: { use: Function } }) {
      registerRoutes(server);
    },
    configurePreviewServer(server: { middlewares: { use: Function } }) {
      registerRoutes(server);
    }
  };
};

export default defineConfig({
  plugins: [react(), networkScanPlugin(), torchModelPlugin(), hfDatasetsPlugin()],
  server: {
    port: 5173,
    open: false
  },
  build: {
    outDir: 'dist'
  }
});

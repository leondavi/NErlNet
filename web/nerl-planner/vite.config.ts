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
          res.statusCode = 500;
          res.setHeader('Content-Type', 'application/json');
          res.end(JSON.stringify({ error: stderr || 'Torch export failed' }));
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

export default defineConfig({
  plugins: [react(), networkScanPlugin(), torchModelPlugin()],
  server: {
    port: 5173,
    open: false
  },
  build: {
    outDir: 'dist'
  }
});

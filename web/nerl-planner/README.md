# Nerlnet Planner (Web)

Modern, web-based redesign of NerlPlanner built with React. Use it to build:

- Distributed configuration JSON (dc_*.json)
- Connection map JSON (conn_*.json)
- Experiment flow JSON (exp_*.json)

## Quick Start

```bash
cd web/nerl-planner
npm install
npm run dev
```

Then open the printed local URL.

## Notes
- The app autosaves to local storage.
- Export view downloads JSON in the formats used by Nerlnet scripts.
- Import supports the legacy `connectionsMap` format and experiment flow `Phases`.

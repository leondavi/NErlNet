# NerlDesigner - Modern Web-Based Designer for NerlNet

NerlDesigner is a modern, cross-platform web-based designer for NerlNet built with NiceGUI. It provides an intuitive graphical interface for creating, editing, and visualizing NerlNet configurations.

## Features

- **Cross-Platform Web Interface**: Works on any device with a web browser
- **JSON Configuration Generation**: Create Worker, Connection Map, and Distributed Config JSON files
- **Graph Visualization**: Interactive network topology visualization
- **Modern UI**: Clean, responsive interface built with NiceGUI
- **Real-time Validation**: Instant feedback on configuration validity
- **Import/Export**: Load existing configurations and export new ones

## Installation

```bash
pip install nicegui
```

## Usage

```bash
python3 src_py/nerlDesigner/main.py
```

Then open your browser to `http://localhost:8080`

## Architecture

- `main.py` - Main application entry point
- `components/` - Reusable UI components
- `models/` - Data models for JSON configurations
- `utils/` - Utility functions and helpers
- `static/` - Static assets (CSS, JS, images)

## JSON File Types Supported

1. **Worker Configuration** - Neural network and training parameters
2. **Connection Map** - Network topology and device connections
3. **Distributed Config** - Device allocation and experiment settings
4. **Experiment Flow** - Complete experiment configuration
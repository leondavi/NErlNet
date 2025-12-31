# NErlNet Autogen Library

This directory contains the auto-generation scripts and their dependencies that were moved from the `nerlPlanner` module to create a reusable autogen library.

## Files

### Main Exporters
- `CppHeadersExporter.py` - Generates C++ header files with auto-generated definitions
- `ErlHeadersExporter.py` - Generates Erlang header (.hrl) files with auto-generated definitions

### Dependencies
- `CppHeadersExporterDefs.py` - Definitions for C++ header generation
- `ErlHeadersExporterDefs.py` - Definitions for Erlang header generation
- `JsonDistributedConfigDefs.py` - Distributed configuration definitions
- `JsonElementWorkerDefinitions.py` - Worker element definitions
- `JsonElementsDefinitions.py` - General element definitions
- `Definitions.py` - Global definitions and constants
- `logger.py` - Logging utilities

## Usage

The scripts are used by the build system (`NerlnetBuild.sh`) to generate auto-generated header files for both C++ and Erlang components of the NErlNet system.

### Example usage:
```bash
# Generate C++ headers
python3 src_py/autogen/CppHeadersExporter.py --output path/to/output.h

# Generate Erlang headers
python3 src_py/autogen/ErlHeadersExporter.py --gen_worker_fields_hrl --output path/to/output.hrl
```
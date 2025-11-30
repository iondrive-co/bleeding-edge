# BleedingEdge

A peer-to-peer file synchronization protocol stack, including an example implementation created by Claude Code.

## Quick Start

Start BleedingEdge in interactive mode to sync a directory:

```bash
# If installed via package manager (.deb/.rpm/etc)
bleeding-edge sync /path/to/directory

# Or using the JAR directly
java -jar bleeding-edge.jar sync /path/to/directory
```

This will:
1. Monitor the specified directory for changes
2. Discover other BleedingEdge peers on the network
3. Synchronize file changes bidirectionally

### Configuration

BleedingEdge can be configured using HOCON format configuration files.

#### Configuration File Example

Create `.bleedingedge/config`:

```hocon
# BleedingEdge Configuration

# Network port for peer connections (0 = OS-assigned random port)
port = 0

# Peer discovery settings
discovery {
  enabled = true
  multicast-group = "230.0.0.1"
  multicast-port = 4446
}

# Scheduler configuration
scheduler {
  thread-pool-size = 4
}

# Daemon mode
daemon {
  enabled = false
}
```

#### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | Int | 8888 | Network port for peer connections (0 = OS-assigned) |
| `discovery.enabled` | Boolean | true | Enable automatic peer discovery |
| `discovery.multicast-group` | String | "230.0.0.1" | Multicast group address |
| `discovery.multicast-port` | Int | 4446 | Multicast port |
| `scheduler.thread-pool-size` | Int | 4 | Number of worker threads |
| `daemon.enabled` | Boolean | false | Enable daemon mode |

## Architecture

### Overview

- Designed for parallel processing by modeling each layer as stateless functions that can run independently when data becomes available.
- Real-time change detection and propagation
- Automatic peer discovery via multicast. Uses UDP multicast (default group: 230.0.0.1:4446) for automatic peer discovery
- Resilient network operations with circuit breakers and retry logic
- Configurable daemon mode for background operation
- Health monitoring and status checks

### Layer 0: Scheduler
- Manages thread pool and task execution
- Provides `ExecutionContext` for async operations
- Graceful shutdown with timeout handling

### Layer 1: Application (CLI)
- Command-line interface for user interaction
- Configuration management
- Daemon mode support

### Layer 2: Resource
- `FileSystemMonitor`: Watches directories for changes using Java NIO WatchService
- Detects file creation, modification, and deletion
- Non-blocking, event-driven design

### Layer 3: Transposition
- `StateTransformer`: Converts file system states to commands
- Generates diff between snapshots
- Produces commands for remote peers

### Layer 4: Codec
- `Serialization`: Encodes/decodes snapshots and commands
- Binary protocol for efficient transmission
- Tail-recursive for stack safety

### Layer 5: Network
- `NetworkManager`: Handles peer connections
- TCP-based peer-to-peer communication
- Automatic peer discovery via multicast
- Connection pooling and management

### Layer 6: Sync
- `SyncManager`: Orchestrates synchronization
- Coordinates resource monitoring, state transformation, and network communication
- Ensures consistency across peers

### Resilience Layer
- `RetryPolicy`: Exponential backoff with jitter
- `CircuitBreaker`: Three-state pattern (Closed/Open/HalfOpen)
- `HealthCheck`: System health monitoring

### Data Model

**Core Types:**
- `ResourceId`: Immutable file identifier
- `LocationState`: Immutable directory state with hash
- `Snapshot`: Point-in-time view of directory
- `Command`: File operations (Create, Update, Move, Delete)
- `NetworkMessage`: Protocol messages between peers
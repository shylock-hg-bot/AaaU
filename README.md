# AaaU - Agent-as-User Architecture

A secure PTY (pseudo-terminal) bridge for running AI agents under isolated system users on Linux. This implements the "Agent-as-User" architecture where each agent runs as a dedicated system user, providing kernel-level isolation through standard Unix permissions.

## Overview

```
┌─────────────┐            ┌─────────────┐
│   Human     │            │   Human     │
│  (Operator) │            │ (Observer)  │
└──────┬──────┘            └──────┬──────┘
       │                          │
       └──────────┬───────────────┘
                  │
       ┌──────────▼──────────┐
       │    aaau-server      │
       │   (Unix Socket)     │
       └──────────┬──────────┘
                  │
       ┌──────────▼──────────┐
       │   agent-session     │
       │   (PTY bridge)      │
       └──────────┬──────────┘
                  │
       ┌──────────▼──────────┐
       │   agent process     │
       │  (runs as separate  │
       │    system user)     │
       └─────────────────────┘
```

## Features

- **Process Isolation**: Each agent runs as a dedicated system user
- **Resource Limits**: Leverages cgroups via systemd for resource control
- **File Isolation**: Each agent has its own `$HOME` directory
- **Audit Logging**: Complete session recording in JSON format
- **Multi-Client Support**: Multiple humans can connect to observe/interact
- **Permission Levels**: Read-only, Interactive, and Admin access levels
- **Unix Domain Sockets**: Fast, secure local communication

## Architecture

This system implements the Agent-as-User (AaaU) security model:

| Feature | Implementation |
|---------|---------------|
| User Creation | `useradd` / `systemd-sysusers` |
| Process Isolation | `setuid()` / dedicated users |
| Resource Limits | cgroups v2 (systemd) |
| File Isolation | Per-user `$HOME` directories |
| IPC Control | Unix socket permissions |
| Audit | JSONL format logs |

## Building

### Requirements

- OCaml >= 4.14
- opam
- Linux (for PTY and user isolation features)

### Install Dependencies

```bash
opam install -y dune lwt lwt_ppx logs fmt cmdliner yojson uuidm mtime cstruct
```

### Build

```bash
dune build
```

### Install

```bash
sudo cp _build/install/default/bin/aaau-server /usr/local/bin/
sudo cp _build/install/default/bin/aaau-client /usr/local/bin/
```

## Setup

### 1. Create Agent User

```bash
# Create dedicated user for running agents
sudo useradd -r -s /bin/false -d /var/lib/agents/default agent
```

### 2. Create Shared Group

```bash
# Create group for authorized human users
sudo groupadd agent-shared
sudo usermod -aG agent-shared $USER
```

### 3. Create Directories

```bash
sudo mkdir -p /var/run/aaau
sudo mkdir -p /var/log/aaau
sudo chown root:agent-shared /var/run/aaau
sudo chmod 775 /var/run/aaau
```

## Usage

### Start the Server

```bash
# Run in foreground
sudo aaau-server -s /var/run/aaau/server.sock -g agent-shared

# Or as daemon
sudo aaau-server -d -s /var/run/aaau/server.sock -g agent-shared
```

Options:
- `-s, --socket`: Unix socket path (default: `/var/run/claude-bridge.sock`)
- `-g, --group`: Authorized group name (default: `claude-shared`)
- `-u, --user`: Agent system user (default: `claude-agent`)
- `-l, --log-dir`: Audit log directory (default: `/var/log/claude-bridge`)
- `-d, --daemon`: Run as daemon

### Connect with Client

```bash
# Create new session
aaau-client -s /var/run/aaau/server.sock

# Join existing session
aaau-client -s /var/run/aaau/server.sock -n <session-id>

# Read-only mode (observe only)
aaau-client -s /var/run/aaau/server.sock -n <session-id> -r
```

## Protocol

The client-server protocol uses a simple text-based format:

### Client to Server

| Message | Description |
|---------|-------------|
| `<text>` | Regular terminal input |
| `\x01RESIZE:<rows>,<cols>` | Terminal resize |
| `\x01PING` | Keepalive ping |
| `\x01GET_STATUS` | Query session status |
| `\x01FORCE_KILL` | Admin: terminate session |

### Server to Client

| Message | Description |
|---------|-------------|
| `<text>` | Terminal output |
| `\x01PONG` | Ping response |
| `\x01STATUS:<json>` | Session status |
| `\x01ERROR:<msg>` | Error message |
| `\x01CONTROL:<msg>` | Control notification |

## API Usage

### Creating a Session

```ocaml
open Lwt.Syntax

let () =
  let audit = AaaU.Audit.create ~log_dir:"/var/log/aaau" in
  let* result = AaaU.Session.create 
    ~session_id:"sess-123"
    ~creator:"operator" 
    ~audit 
  in
  match result with
  | Ok session -> 
      Printf.printf "Session created: %s\n" (AaaU.Session.get_id session)
  | Error e -> 
      Printf.eprintf "Error: %s\n" e
```

### Running the Server

```ocaml
let server = AaaU.Bridge.create
  ~socket_path:"/var/run/aaau/server.sock"
  ~shared_group:"agent-shared"
  ~agent_user:"agent"
  ~log_dir:"/var/log/aaau"
in

Lwt_main.run (AaaU.Bridge.start server)
```

## Security Model

### Permission Levels

| Level | Permissions |
|-------|------------|
| `ReadOnly` | View output only |
| `Interactive` | View + send input |
| `Admin` | Full control + force kill |

### Authentication

Authentication is based on Unix socket credentials:
- Client's UID/GID verified via `SO_PEERCRED`
- User must be member of configured shared group
- Root users (UID < 1000) get Admin permissions

### Audit Trail

All actions are logged in JSON Lines format:

```json
{"timestamp": 1711523456.789, "source": "human", "user": "alice", "session_id": "sess-123", "command_type": "input", "content": "ls -la", "metadata": {}}
{"timestamp": 1711523457.012, "source": "system", "user": "system", "session_id": "sess-123", "command_type": "session_start", "content": "Agent PID 12345", "metadata": {"pty": "/dev/pts/5"}}
```

## Project Structure

```
lib/
├── pty.mli/ml       # PTY operations
├── protocol.mli/ml  # Communication protocol
├── audit.mli/ml     # Audit logging
├── auth.mli/ml      # Authentication/authorization
├── session.mli/ml   # Session management
└── bridge.mli/ml    # Main server

bin/
├── server.ml        # aaau-server executable
└── client.ml        # aaau-client executable
```

## Comparison with Alternatives

| Approach | Isolation | Overhead | Startup |
|----------|-----------|----------|---------|
| Docker/Podman | Namespace | High | 50-500ms |
| Firecracker VM | Hardware | Very High | ~150ms |
| gVisor | Syscall intercept | Medium | 10-20ms |
| **AaaU** | **User/UID** | **Minimal** | **~1ms** |

## Limitations

- Linux only (relies on Unix sockets, PTYs, and user isolation)
- Requires root/sudo for user switching
- Some ioctl operations require C bindings (currently simplified)
- GPU/graphics access requires additional setup

## License

MIT License - See LICENSE file

## Contributing

Contributions welcome! Please ensure:
- Code follows OCaml conventions
- Tests pass (`dune test`)
- Documentation is updated

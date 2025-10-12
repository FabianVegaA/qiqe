# Qiqe Architecture

## Overview

Qiqe is a web-based playground for a functional programming language. The system uses a modern microservices architecture with nginx as a reverse proxy.

## System Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│                 │    │                 │    │                 │
│   Client        │    │   nginx         │    │   React         │
│   Browser       │◄──►│   Reverse       │◄──►│   Frontend      │
│                 │    │   Proxy         │    │   Service       │
│                 │    │                 │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                │
                                │
                       ┌─────────────────┐    ┌─────────────────┐
                       │                 │    │                 │
                       │   Haskell       │    │   PostgreSQL    │
                       │   Interpreter   │    │   Database      │
                       │   Service       │    │                 │
                       │                 │    │                 │
                       └─────────────────┘    └─────────────────┘
```

## Components

### nginx Reverse Proxy
- **Port**: 80 (production), 8080 (development)
- **Purpose**: Routes requests to appropriate backend services
- **Routes**:
  - `/` → React Frontend Service
  - `/codegen` → Interpreter Service (JSON API)
  - `/lib` → Interpreter Service (JSON API)
  - `/file/*` → Static file serving

### React Frontend Service
- **Port**: 3000 (internal)
- **Purpose**: Serves the web-based code editor and user interface
- **Features**:
  - Code editor with syntax highlighting
  - Real-time code execution
  - Library import functionality
  - Responsive design

### Haskell Interpreter Service
- **Port**: 8000
- **Purpose**: Compiles and executes Qiqe code
- **API Endpoints**:
  - `POST /codegen` - Execute code and return results
  - `POST /lib` - Import and execute library files
- **Features**:
  - JSON REST API (no gRPC)
  - Built-in error handling
  - Library file processing

### PostgreSQL Database
- **Port**: 5432 (internal)
- **Purpose**: Data persistence (future use)
- **Configuration**: Optimized for development and production environments

## API Interfaces

### Code Execution API

**Endpoint**: `POST /codegen`

**Request**:
```json
{
  "code": "string"
}
```

**Response**:
```json
{
  "id": 1,
  "result": "string",
  "status": true,
  "error": "",
  "createdAt": "2024-01-01T00:00:00Z"
}
```

### Library Import API

**Endpoint**: `POST /lib`

**Request**:
```json
{
  "filename": "string"
}
```

**Response**:
```json
{
  "target_code": "string",
  "status": true,
  "error": ""
}
```

## Development Environment

### Prerequisites
- Docker
- Docker Compose

### Starting Development Environment
```bash
./scripts/start-dev.sh
```

This starts:
- nginx on port 8080
- React frontend with hot reloading
- Haskell interpreter service
- PostgreSQL database

### Production Deployment
```bash
./scripts/start-prod.sh
```

This starts the optimized production environment on port 80.

## File Structure

```
qiqe/
├── service/
│   ├── nginx/           # nginx configuration
│   ├── frontend/        # React application
│   ├── Interpreter/     # Haskell interpreter service
│   └── postgres/        # Database configuration
├── qiqe/
│   ├── library/         # Qiqe standard library
│   └── doc/            # Documentation and examples
├── scripts/            # Deployment and utility scripts
├── docker-compose.yml  # Production configuration
└── docker-compose.dev.yml # Development configuration
```

## Migration Notes

This architecture replaces the previous Rust-based proxy service with nginx for improved performance and simplified deployment. The system now uses direct HTTP communication instead of gRPC, making it easier to develop and maintain.

Key changes:
- Removed Rust proxy service
- Removed gRPC communication layer
- Simplified to nginx + JSON REST API
- Removed Nix build system dependencies
- Streamlined Docker-based deployment
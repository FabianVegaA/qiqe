# nginx Proxy Configuration

This directory contains the nginx configuration files for replacing the Rust-based proxy service.

## Files

- `nginx.conf` - Production nginx configuration
- `nginx.dev.conf` - Development nginx configuration with enhanced debugging
- `conf.d/logging.conf` - Additional logging configurations
- `conf.d/monitoring.conf` - Health check and monitoring endpoints

## Configuration Overview

### Upstream Services

The configuration defines two upstream services:
- `api_service` - The Node.js API service (port 8080)
- `frontend_service` - The React frontend service (port 3000)

### Location Blocks

#### API Endpoints (`/codegen`, `/lib`)
- Proxied to the API service
- Rate limited (10 requests/second with burst of 20)
- CORS headers configured
- Request/response logging with structured format
- 30-second timeouts

#### Static File Serving (`/file/{filename}`)
- Serves files from `/app/qiqe/library/` directory
- Security measures against path traversal attacks
- Content-type headers for `.qq` files
- Custom 404 error handling
- 1-hour cache expiration

#### Frontend (`/`)
- Proxied to the frontend service
- Support for React Router (SPA routing)
- CORS headers configured
- Rate limited (50 requests/second with burst of 100)

### Security Features

- Path traversal protection for static files
- Rate limiting on all endpoints
- Security headers (X-Frame-Options, X-Content-Type-Options, X-XSS-Protection)
- Request ID generation for tracing
- Restricted access to monitoring endpoints

### Logging

#### Production Logging
- JSON-formatted access logs with structured data
- Error logs at warn level
- Separate health check logs
- Performance metrics in logs

#### Development Logging
- Debug-level error logging
- Additional debug access log
- Security event logging
- 404 error logging

### Monitoring

#### Health Check Endpoints
- `/health` - Basic health check (port 80)
- `/health/detailed` - Detailed health status with JSON response (port 8081)
- `/nginx_status` - nginx status page (port 8081, restricted access)
- `/metrics` - Basic metrics endpoint (port 8081, restricted access)

## Usage

### Production
```bash
nginx -c /path/to/nginx.conf
```

### Development
```bash
nginx -c /path/to/nginx.dev.conf
```

### Testing Configuration
```bash
nginx -t -c /path/to/nginx.conf
```

## Environment Variables

The configuration supports different environments through upstream server definitions:
- Production: Uses service names (api:8080, frontend:3000)
- Development: Uses host.docker.internal for local development

## Log Files

- `/var/log/nginx/access.log` - Main access log
- `/var/log/nginx/error.log` - Error log
- `/var/log/nginx/health.log` - Health check requests
- `/var/log/nginx/debug.log` - Debug information (development only)
- `/var/log/nginx/security.log` - Security events (development only)
- `/var/log/nginx/404.log` - 404 errors (development only)

## Rate Limiting

### Production
- API endpoints: 10 requests/second (burst: 20)
- General endpoints: 50 requests/second (burst: 100)
- File serving: 50 requests/second (burst: 100)

### Development
- API endpoints: 100 requests/second (burst: 50)
- General endpoints: 200 requests/second (burst: 200)
- File serving: 200 requests/second (burst: 200)

## CORS Configuration

CORS is configured to allow:
- All origins (`*`)
- Common HTTP methods (GET, POST, OPTIONS)
- Standard headers including Authorization
- Preflight request handling

## Requirements Addressed

This configuration addresses the following requirements:
- 1.1, 1.2, 1.3: nginx as primary reverse proxy with maintained routing
- 2.3, 2.4, 2.5: API endpoint proxying with headers and CORS
- 3.1, 3.2, 3.3, 3.4: Static file serving with security measures
- 7.1, 7.2, 7.3, 7.4: Comprehensive logging and monitoring
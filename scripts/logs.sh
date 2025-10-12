#!/bin/bash

# View logs script
set -e

SERVICE=${1:-""}

if [ -z "$SERVICE" ]; then
    echo "📋 Showing logs for all services..."
    docker-compose logs -f
else
    echo "📋 Showing logs for service: $SERVICE"
    docker-compose logs -f "$SERVICE"
fi

echo ""
echo "Available services: nginx, api, frontend, interpreter"
echo "Usage: ./scripts/logs.sh [service_name]"
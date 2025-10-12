#!/bin/bash

# Stop all services script
set -e

echo "🛑 Stopping nginx proxy replacement services..."

# Stop and remove containers
docker-compose -f docker-compose.yml -f docker-compose.dev.yml down

echo "✅ All services stopped!"
echo "🧹 To clean up volumes and images, run: docker-compose down -v --rmi all"
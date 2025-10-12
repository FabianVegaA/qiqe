#!/bin/bash

# Production startup script
set -e

echo "🚀 Starting nginx proxy replacement in production mode..."

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

# Check if .env.production exists, create from example if not
if [ ! -f .env.production ]; then
    echo "📝 Creating .env.production from template..."
    cp .env.example .env.production
fi

# Build and start services
echo "🔨 Building and starting services..."
docker-compose --env-file .env.production up --build -d

echo "✅ Production environment started!"
echo "🌐 Access the application at: http://localhost"
echo "📊 Check service status: docker-compose ps"
echo "📋 View logs: docker-compose logs -f"
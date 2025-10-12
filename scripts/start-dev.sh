#!/bin/bash

# Development startup script
set -e

echo "🚀 Starting nginx proxy replacement in development mode..."

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

# Check if .env.development exists, create from example if not
if [ ! -f .env.development ]; then
    echo "📝 Creating .env.development from template..."
    cp .env.example .env.development
fi

# Build and start services
echo "🔨 Building and starting services..."
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up --build

echo "✅ Development environment started!"
echo "🌐 Access the application at: http://localhost:8080"
echo "🔧 Code execution endpoints available at: http://localhost:8080/codegen and http://localhost:8080/lib"
echo "⚛️  Frontend development server: http://localhost:3000"
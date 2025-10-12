#!/bin/bash

# Docker Configuration Validation Script
# This script validates that all Docker configurations are correct

set -e

echo "🐳 Validating Docker Configurations..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print status
print_status() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $2"
    else
        echo -e "${RED}✗${NC} $2"
        exit 1
    fi
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_info() {
    echo -e "ℹ️  $1"
}

# Check if Docker is running
print_info "Checking Docker daemon..."
if ! docker info > /dev/null 2>&1; then
    echo -e "${RED}✗${NC} Docker daemon is not running. Please start Docker first."
    exit 1
fi
print_status 0 "Docker daemon is running"

# Check if Docker Compose is available
print_info "Checking Docker Compose..."
if ! command -v docker-compose > /dev/null 2>&1; then
    echo -e "${RED}✗${NC} Docker Compose is not installed or not in PATH"
    exit 1
fi
print_status 0 "Docker Compose is available"

# Validate production docker-compose.yml
print_info "Validating production configuration..."
docker-compose config > /dev/null 2>&1
print_status $? "Production docker-compose.yml is valid"

# Validate development docker-compose.dev.yml
print_info "Validating development configuration..."
docker-compose -f docker-compose.dev.yml config > /dev/null 2>&1
print_status $? "Development docker-compose.dev.yml is valid"

# Check if required directories exist
print_info "Checking required directories and files..."

required_dirs=(
    "service/nginx"
    "service/frontend"
    "service/Interpreter"
    "service/postgres"
    "qiqe/library"
)

for dir in "${required_dirs[@]}"; do
    if [ -d "$dir" ]; then
        print_status 0 "Directory $dir exists"
    else
        print_status 1 "Directory $dir is missing"
    fi
done

# Check if required Dockerfiles exist
required_dockerfiles=(
    "service/frontend/Dockerfile"
    "service/frontend/Dockerfile.dev"
    "service/Interpreter/Dockerfile"
    "service/postgres/Dockerfile"
)

for dockerfile in "${required_dockerfiles[@]}"; do
    if [ -f "$dockerfile" ]; then
        print_status 0 "Dockerfile $dockerfile exists"
    else
        print_status 1 "Dockerfile $dockerfile is missing"
    fi
done

# Check if nginx configuration files exist
nginx_configs=(
    "service/nginx/nginx.conf"
    "service/nginx/nginx.dev.conf"
)

for config in "${nginx_configs[@]}"; do
    if [ -f "$config" ]; then
        print_status 0 "nginx config $config exists"
    else
        print_status 1 "nginx config $config is missing"
    fi
done

# Check if postgres configuration files exist
postgres_configs=(
    "service/postgres/postgresql.conf"
    "service/postgres/pg_hba.conf"
    "service/postgres/init-scripts/01-init-database.sql"
)

for config in "${postgres_configs[@]}"; do
    if [ -f "$config" ]; then
        print_status 0 "PostgreSQL config $config exists"
    else
        print_status 1 "PostgreSQL config $config is missing"
    fi
done

# Check for port conflicts
print_info "Checking for potential port conflicts..."

ports_to_check=(80 3000 5432 8080 50051)
for port in "${ports_to_check[@]}"; do
    if lsof -i :$port > /dev/null 2>&1; then
        print_warning "Port $port is currently in use"
    else
        print_status 0 "Port $port is available"
    fi
done

# Test building images (optional - can be slow)
if [ "$1" = "--build-test" ]; then
    print_info "Testing image builds (this may take a while)..."
    
    # Test postgres build
    print_info "Building PostgreSQL image..."
    docker build -t qiqe-postgres-test service/postgres/ > /dev/null 2>&1
    print_status $? "PostgreSQL image builds successfully"
    
    # Test interpreter build
    print_info "Building interpreter image..."
    docker build -t qiqe-interpreter-test service/Interpreter/ > /dev/null 2>&1
    print_status $? "Interpreter image builds successfully"
    
    # Test frontend build
    print_info "Building frontend image..."
    docker build -t qiqe-frontend-test service/frontend/ > /dev/null 2>&1
    print_status $? "Frontend image builds successfully"
    
    # Clean up test images
    docker rmi qiqe-postgres-test qiqe-interpreter-test qiqe-frontend-test > /dev/null 2>&1 || true
fi

echo ""
echo -e "${GREEN}🎉 All Docker configurations are valid!${NC}"
echo ""
echo "Next steps:"
echo "  • For development: docker-compose -f docker-compose.dev.yml up -d"
echo "  • For production:  docker-compose up -d"
echo "  • To build test:   $0 --build-test"
echo ""
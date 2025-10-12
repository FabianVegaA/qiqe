# Production Dockerfile for PostgreSQL database
FROM postgres:15-alpine

# Install additional tools
RUN apk add --no-cache \
    curl \
    postgresql-contrib

# Set environment variables
ENV POSTGRES_DB=qiqe_db
ENV POSTGRES_USER=qiqe_user
ENV POSTGRES_PASSWORD=qiqe_password
ENV PGDATA=/var/lib/postgresql/data/pgdata

# Create directories for initialization scripts
RUN mkdir -p /docker-entrypoint-initdb.d

# Copy initialization scripts
COPY init-scripts/ /docker-entrypoint-initdb.d/

# Copy custom postgresql configuration
COPY postgresql.conf /etc/postgresql/postgresql.conf
COPY pg_hba.conf /etc/postgresql/pg_hba.conf

# Set proper permissions
RUN chown -R postgres:postgres /etc/postgresql/

# Expose PostgreSQL port
EXPOSE 5432

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD pg_isready -U $POSTGRES_USER -d $POSTGRES_DB || exit 1

# Use the default postgres entrypoint
# The entrypoint will automatically run scripts in /docker-entrypoint-initdb.d/
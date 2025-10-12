# PostgreSQL Database Service

This directory contains the PostgreSQL database configuration for the Qiqe application.

## Structure

```
service/postgres/
├── Dockerfile              # Production PostgreSQL container
├── postgresql.conf         # PostgreSQL server configuration
├── pg_hba.conf            # Client authentication configuration
├── init-scripts/          # Database initialization scripts
│   └── 01-init-database.sql
└── README.md              # This file
```

## Configuration

### Environment Variables

- `POSTGRES_DB`: Database name (default: `qiqe_db`)
- `POSTGRES_USER`: Database user (default: `qiqe_user`)
- `POSTGRES_PASSWORD`: Database password (default: `qiqe_password`)
- `PGDATA`: PostgreSQL data directory (default: `/var/lib/postgresql/data/pgdata`)

### Database Schema

The initialization script creates:

- **Extensions**: `uuid-ossp`, `pg_trgm`
- **Schemas**: `qiqe`, `logs`
- **Tables**:
  - `qiqe.users`: User management
  - `qiqe.code_executions`: Code execution history
  - `qiqe.library_imports`: Library import tracking

### Security Features

- Non-root user execution
- Scram-SHA-256 password encryption
- Network-based authentication rules
- Proper file permissions

## Usage

### Production
```bash
docker-compose up postgres
```

### Development
```bash
docker-compose -f docker-compose.dev.yml up postgres
```

The development configuration exposes port 5432 for direct database access.

### Database Connection

**Production:**
```
Host: postgres
Port: 5432
Database: qiqe_db
User: qiqe_user
Password: qiqe_password
```

**Development:**
```
Host: localhost
Port: 5432
Database: qiqe_db_dev
User: qiqe_user
Password: qiqe_password_dev
```

## Health Checks

The container includes health checks using `pg_isready` to ensure the database is accepting connections.

## Volumes

- `postgres_data`: Persistent database storage
- `postgres_logs`: Database log files

## Customization

To add custom initialization scripts:

1. Place SQL files in `init-scripts/` directory
2. Files are executed in alphabetical order
3. Scripts run only on first container startup

To modify PostgreSQL configuration:

1. Edit `postgresql.conf` for server settings
2. Edit `pg_hba.conf` for authentication rules
3. Rebuild the container
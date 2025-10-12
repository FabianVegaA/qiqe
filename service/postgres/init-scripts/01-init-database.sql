-- Initialize the qiqe database schema
-- This script runs automatically when the container starts for the first time

-- Create extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";

-- Create schemas
CREATE SCHEMA IF NOT EXISTS qiqe;
CREATE SCHEMA IF NOT EXISTS logs;

-- Set default search path
ALTER DATABASE qiqe_db SET search_path TO qiqe, public;

-- Create basic tables (example structure)
CREATE TABLE IF NOT EXISTS qiqe.users (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    username VARCHAR(255) UNIQUE NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS qiqe.code_executions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id UUID REFERENCES qiqe.users(id),
    code TEXT NOT NULL,
    result TEXT,
    status BOOLEAN NOT NULL,
    error_message TEXT,
    execution_time_ms INTEGER,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS qiqe.library_imports (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id UUID REFERENCES qiqe.users(id),
    filename VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    imported_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create indexes for better performance
CREATE INDEX IF NOT EXISTS idx_users_username ON qiqe.users(username);
CREATE INDEX IF NOT EXISTS idx_users_email ON qiqe.users(email);
CREATE INDEX IF NOT EXISTS idx_code_executions_user_id ON qiqe.code_executions(user_id);
CREATE INDEX IF NOT EXISTS idx_code_executions_created_at ON qiqe.code_executions(created_at);
CREATE INDEX IF NOT EXISTS idx_library_imports_user_id ON qiqe.library_imports(user_id);
CREATE INDEX IF NOT EXISTS idx_library_imports_filename ON qiqe.library_imports(filename);

-- Create updated_at trigger function
CREATE OR REPLACE FUNCTION qiqe.update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Apply updated_at trigger to users table
CREATE TRIGGER update_users_updated_at 
    BEFORE UPDATE ON qiqe.users 
    FOR EACH ROW 
    EXECUTE FUNCTION qiqe.update_updated_at_column();

-- Grant permissions
GRANT USAGE ON SCHEMA qiqe TO qiqe_user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA qiqe TO qiqe_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA qiqe TO qiqe_user;
GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA qiqe TO qiqe_user;
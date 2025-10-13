FROM haskell:9.10.2-slim-bullseye AS builder

WORKDIR /opt/build

COPY . /opt/build

RUN set -eux \
    && apt-get update && apt-get install -y \
    && stack build --system-ghc \
    && mkdir -p /app/bin \
    && cp $(stack path --local-install-root)/bin/WebQiqe /app/bin/WebQiqe

# Production stage
FROM debian:bookworm-slim AS production

RUN mkdir -p /opt/Interpreter

# Install runtime dependencies
RUN apt-get update && apt-get install -y

# Create app directory and user
WORKDIR /app
RUN groupadd -r haskell && useradd -r -g haskell haskell

# Copy built binary from builder stage
COPY --from=builder /app/bin/WebQiqe /app/bin/WebQiqe

# Change ownership to haskell user
RUN chown -R haskell:haskell /app
USER haskell

# Expose HTTP JSON API port
EXPOSE 8000

# Health check using netcat
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD nc -z localhost 8000 || exit 1

# Start the interpreter service
CMD ["/app/bin/WebQiqe"]

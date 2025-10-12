FROM debian:bookworm-slim AS builder-stack

# Install system dependencies for HTTP server and protobuf
RUN apt-get update && apt-get install -y \
    && apt-get install -y \
    curl

WORKDIR /opt/build

COPY . /opt/build

RUN curl -sSL https://get.haskellstack.org/ | sh \
    && stack install

FROM builder-stack AS builder

RUN stack build --system-ghc \
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
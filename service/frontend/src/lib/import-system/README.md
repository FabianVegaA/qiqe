# Import System

A robust module import system for the qiqe programming language with dependency resolution, intelligent caching, and namespace management.

## Features

- **Dependency Resolution**: Automatically resolves module dependencies with circular dependency detection
- **Intelligent Caching**: Multi-level caching (memory and disk) with smart invalidation
- **Selective Imports**: Import specific functions from libraries
- **Namespace Management**: Avoid naming conflicts with namespace isolation and aliases
- **Path Resolution**: Support for relative, absolute, and standard library paths
- **Error Handling**: Comprehensive error reporting with detailed diagnostics

## Components

### ImportManager

Handles module imports and dependency resolution.

```typescript
import { ImportManager, ImportSpec } from "./index";

const importManager = new ImportManager();

const imports: ImportSpec[] = [
  {
    path: "std.qq",
    type: "standard",
  },
  {
    path: "./utils.qq",
    type: "relative",
    selective: ["helper1", "helper2"],
  },
];

const resolved = await importManager.resolveImports(imports);
```

### ModuleCache

Intelligent caching system for compiled modules.

```typescript
import { ModuleCache } from "./index";

const cache = new ModuleCache({
  maxMemorySize: 50 * 1024 * 1024, // 50MB
  maxEntries: 1000,
  ttl: 30 * 60 * 1000, // 30 minutes
});

// Cache a module
cache.set("module-key", compiledModule);

// Retrieve from cache
const module = cache.get("module-key");

// Get cache statistics
const stats = cache.getStats();
```

### NamespaceManager

Manages selective imports and namespace isolation.

```typescript
import { NamespaceManager } from "./index";

const namespaceManager = new NamespaceManager();

const result = namespaceManager.processSelectiveImports(library, {
  path: "math.qq",
  type: "standard",
  selective: ["add", "multiply"],
  alias: "MathLib",
});
```

### ImportSystem (Facade)

Main interface that combines all components.

```typescript
import { ImportSystem } from "./index";

const importSystem = new ImportSystem();

const result = await importSystem.processImports([
  {
    path: "std.qq",
    type: "standard",
    selective: ["print", "input"],
  },
]);

// Clean up
importSystem.destroy();
```

## Import Types

### Standard Library Imports

```typescript
{
  path: 'std.qq',
  type: 'standard'
}
```

### Relative Imports

```typescript
{
  path: './utils.qq',
  type: 'relative'
}
```

### Absolute Imports

```typescript
{
  path: '/qiqe/library/math.qq',
  type: 'absolute'
}
```

### Selective Imports

```typescript
{
  path: 'math.qq',
  type: 'standard',
  selective: ['add', 'subtract', 'multiply']
}
```

### Aliased Imports

```typescript
{
  path: 'math.qq',
  type: 'standard',
  alias: 'MathLib'
}
```

## Error Handling

The system provides comprehensive error handling:

- **CIRCULAR_DEPENDENCY**: Circular dependency detected
- **MODULE_NOT_FOUND**: Module file not found
- **INVALID_PATH**: Invalid import path format
- **PARSE_ERROR**: Error parsing module content

```typescript
try {
  const result = await importSystem.processImports(imports);
} catch (error) {
  if (error.type === "CIRCULAR_DEPENDENCY") {
    console.error("Circular dependency:", error.dependencyChain);
  }
}
```

## Cache Management

The cache system provides intelligent invalidation:

```typescript
// Invalidate by modification time
cache.invalidateByModificationTime("/path/to/module.qq", new Date());

// Invalidate dependents
cache.invalidateDependents("/path/to/dependency.qq");

// Manual cleanup
cache.cleanup();

// Get cache statistics
const stats = cache.getStats();
console.log(`Hit rate: ${stats.hitRate}, Memory usage: ${stats.memoryUsage}`);
```

## Namespace Management

Handle naming conflicts and organize imports:

```typescript
// Create namespace
const namespace = namespaceManager.createOrGetNamespace("MyNamespace");

// Resolve conflicts automatically
const conflicts = namespaceManager.resolveConflicts(namespace, "auto");

// Check for function existence
const hasFunction = namespaceManager.hasFunction("MyNamespace", "myFunction");

// Get namespace documentation
const docs = namespaceManager.generateNamespaceDocumentation("MyNamespace");
```

## Performance Considerations

- **Memory Management**: Automatic cleanup of expired entries
- **LRU Eviction**: Least recently used entries are evicted first
- **Lazy Loading**: Modules are loaded only when needed
- **Dependency Optimization**: Shared dependencies are cached and reused

## Testing

Run the test suite:

```typescript
import {
  testImportSystem,
  testModuleCache,
  testNamespaceManager,
} from "./test-import-system";

testImportSystem();
testModuleCache();
testNamespaceManager();
```

## Requirements Satisfied

This implementation satisfies the following requirements:

- **2.1**: Support for relative and absolute import paths
- **2.2**: Dependency resolution in correct order
- **2.3**: Circular dependency detection and reporting
- **2.4**: Module caching for performance
- **2.5**: Change detection and recompilation
- **2.6**: Selective imports (importing specific functions)
- **4.4**: Namespace management to avoid naming conflicts

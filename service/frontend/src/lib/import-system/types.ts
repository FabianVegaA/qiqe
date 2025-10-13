/**
 * Type definitions for the import system
 */

export interface ImportSpec {
  path: string;
  type: "relative" | "absolute" | "standard";
  selective?: string[]; // For selective imports
  alias?: string;
}

export interface ResolvedImport {
  spec: ImportSpec;
  library: Library;
  dependencies: ResolvedImport[];
  compiledCode: string;
}

export interface Library {
  name: string;
  version: string;
  path: string;
  exports: ExportedFunction[];
  dependencies: Dependency[];
  metadata: LibraryMetadata;
  compiledCode: string;
}

export interface ExportedFunction {
  name: string;
  signature: string;
  documentation: string;
  examples: string[];
  deprecated?: boolean;
  visibility: "public" | "private";
}

export interface Dependency {
  name: string;
  version: string;
  path: string;
  required: boolean;
}

export interface LibraryMetadata {
  author: string;
  description: string;
  tags: string[];
  lastModified: Date;
  size: number;
  checksum: string;
}

export interface DependencyGraph {
  nodes: DependencyNode[];
  edges: DependencyEdge[];
  circularDependencies: string[][];
}

export interface DependencyNode {
  id: string;
  path: string;
  library: Library;
  visited: boolean;
  inStack: boolean;
}

export interface DependencyEdge {
  from: string;
  to: string;
  type: "direct" | "transitive";
}

export interface CachedModule {
  compiledCode: string;
  metadata: ModuleMetadata;
  dependencies: string[];
  lastModified: Date;
  accessCount: number;
  size: number;
}

export interface ModuleMetadata {
  path: string;
  checksum: string;
  version: string;
  exports: string[];
  imports: string[];
}

export interface CacheStats {
  totalEntries: number;
  memoryUsage: number;
  hitRate: number;
  missRate: number;
  evictionCount: number;
}

export interface ImportError extends Error {
  type:
    | "CIRCULAR_DEPENDENCY"
    | "MODULE_NOT_FOUND"
    | "INVALID_PATH"
    | "PARSE_ERROR";
  path: string;
  dependencyChain?: string[];
}

export interface ImportValidationResult {
  valid: boolean;
  errors: ImportError[];
  warnings: string[];
}

/**
 * Import System - Robust module import system with dependency resolution
 */

export { ImportManager } from "./ImportManager";
export { ModuleCache } from "./ModuleCache";
export { NamespaceManager } from "./NamespaceManager";

export type {
  ImportSpec,
  ResolvedImport,
  Library,
  ExportedFunction,
  Dependency,
  LibraryMetadata,
  DependencyGraph,
  DependencyNode,
  DependencyEdge,
  CachedModule,
  ModuleMetadata,
  CacheStats,
  ImportError,
  ImportValidationResult,
} from "./types";

/**
 * Main import system facade that combines all components
 */
import { ImportManager } from "./ImportManager";
import { ModuleCache } from "./ModuleCache";
import { NamespaceManager } from "./NamespaceManager";
import { ImportSpec, ResolvedImport, ImportValidationResult } from "./types";

export class ImportSystem {
  private importManager: ImportManager;
  private moduleCache: ModuleCache;
  private namespaceManager: NamespaceManager;

  constructor() {
    this.moduleCache = new ModuleCache();
    this.importManager = new ImportManager();
    this.namespaceManager = new NamespaceManager();
  }

  /**
   * Process imports with full pipeline
   */
  async processImports(imports: ImportSpec[]): Promise<{
    resolved: ResolvedImport[];
    validation: ImportValidationResult;
    namespaces: string[];
  }> {
    // Validate imports
    const validation = this.importManager.validateImports(imports);
    if (!validation.valid) {
      return {
        resolved: [],
        validation,
        namespaces: [],
      };
    }

    // Resolve imports
    const resolved = await this.importManager.resolveImports(imports);

    // Process selective imports and namespaces
    const namespaces: string[] = [];
    for (const resolvedImport of resolved) {
      if (resolvedImport.spec.selective || resolvedImport.spec.alias) {
        const result = this.namespaceManager.processSelectiveImports(
          resolvedImport.library,
          resolvedImport.spec,
        );

        namespaces.push(result.namespace.name);

        // Update compiled code with filtered version
        resolvedImport.compiledCode = result.filteredCode;
      }
    }

    return {
      resolved,
      validation,
      namespaces,
    };
  }

  /**
   * Get import manager
   */
  getImportManager(): ImportManager {
    return this.importManager;
  }

  /**
   * Get module cache
   */
  getModuleCache(): ModuleCache {
    return this.moduleCache;
  }

  /**
   * Get namespace manager
   */
  getNamespaceManager(): NamespaceManager {
    return this.namespaceManager;
  }

  /**
   * Clear all caches and namespaces
   */
  clear(): void {
    this.moduleCache.clear();
    this.namespaceManager.clearNamespaces();
  }

  /**
   * Destroy and cleanup resources
   */
  destroy(): void {
    this.moduleCache.destroy();
    this.clear();
  }
}

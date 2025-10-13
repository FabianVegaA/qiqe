/**
 * Standard Library Loader
 *
 * This module handles automatic loading of the standard library and provides
 * mechanisms for preloading and lazy loading of libraries with dependency resolution.
 */

import { QiqeLibrary } from "./LibraryManager";
import { ImportManager } from "../import-system/ImportManager";
import { ModuleCache } from "../import-system/ModuleCache";
import { LibraryMetadata } from "../import-system/types";
// Note: These imports are available for future use in dependency resolution
// import { Library, ImportSpec, ResolvedImport } from '../import-system/types';

/**
 * Configuration for library loading
 */
export interface LibraryLoaderConfig {
  autoLoadStandardLibrary: boolean;
  preloadLibraries: string[];
  lazyLoadThreshold: number; // Size threshold for lazy loading
  cacheEnabled: boolean;
  standardLibraryPath: string;
}

/**
 * Default configuration for the library loader
 */
export const DEFAULT_LOADER_CONFIG: LibraryLoaderConfig = {
  autoLoadStandardLibrary: true,
  preloadLibraries: ["std"],
  lazyLoadThreshold: 1024 * 100, // 100KB
  cacheEnabled: true,
  standardLibraryPath: "/qiqe/library/std.qq",
};

/**
 * Library loading result
 */
export interface LibraryLoadResult {
  library: QiqeLibrary;
  loadTime: number;
  fromCache: boolean;
  dependencies: LibraryLoadResult[];
}

/**
 * Standard Library Loader class
 */
export class StandardLibraryLoader {
  private config: LibraryLoaderConfig;
  private importManager: ImportManager;
  private moduleCache: ModuleCache;
  private loadedLibraries: Map<string, QiqeLibrary> = new Map();
  private loadingPromises: Map<string, Promise<LibraryLoadResult>> = new Map();
  private standardLibrary: QiqeLibrary | null = null;
  private initialized: boolean = false;

  constructor(
    config: Partial<LibraryLoaderConfig> = {},
    importManager?: ImportManager,
    moduleCache?: ModuleCache,
  ) {
    this.config = { ...DEFAULT_LOADER_CONFIG, ...config };
    this.importManager = importManager || new ImportManager();
    this.moduleCache = moduleCache || new ModuleCache();
  }

  /**
   * Initialize the library loader and optionally load the standard library
   */
  public async initialize(): Promise<void> {
    if (this.initialized) return;

    try {
      // Load standard library if auto-loading is enabled
      if (this.config.autoLoadStandardLibrary) {
        await this.loadStandardLibrary();
      }

      // Preload specified libraries
      if (this.config.preloadLibraries.length > 0) {
        await this.preloadLibraries(this.config.preloadLibraries);
      }

      this.initialized = true;
    } catch (error) {
      console.error("Failed to initialize StandardLibraryLoader:", error);
      throw new Error(
        `Library loader initialization failed: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Load the standard library (std.qq)
   */
  public async loadStandardLibrary(): Promise<LibraryLoadResult> {
    const startTime = Date.now();

    try {
      // Check if already loaded
      if (this.standardLibrary) {
        return {
          library: this.standardLibrary,
          loadTime: 0,
          fromCache: true,
          dependencies: [],
        };
      }

      // Check cache first
      const cachedModule = this.moduleCache.get("std");
      if (cachedModule && this.config.cacheEnabled) {
        this.standardLibrary = new QiqeLibrary(
          "std",
          this.config.standardLibraryPath,
          cachedModule.compiledCode,
          cachedModule.metadata.version,
        );

        this.loadedLibraries.set("std", this.standardLibrary);

        return {
          library: this.standardLibrary,
          loadTime: Date.now() - startTime,
          fromCache: true,
          dependencies: [],
        };
      }

      // Load from source
      const sourceCode = await this.fetchLibrarySource(
        this.config.standardLibraryPath,
      );
      this.standardLibrary = new QiqeLibrary(
        "std",
        this.config.standardLibraryPath,
        sourceCode,
      );

      // Cache the library
      if (this.config.cacheEnabled) {
        this.moduleCache.set("std", {
          compiledCode: sourceCode,
          metadata: {
            path: this.config.standardLibraryPath,
            checksum: this.standardLibrary.metadata.checksum,
            version: this.standardLibrary.version,
            exports: this.standardLibrary.exports.map((f) => f.name),
            imports: [],
          },
          dependencies: [],
          lastModified: new Date(),
          accessCount: 1,
          size: sourceCode.length,
        });
      }

      this.loadedLibraries.set("std", this.standardLibrary);

      return {
        library: this.standardLibrary,
        loadTime: Date.now() - startTime,
        fromCache: false,
        dependencies: [],
      };
    } catch (error) {
      console.error("Failed to load standard library:", error);
      throw new Error(
        `Standard library loading failed: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Preload specified libraries
   */
  public async preloadLibraries(
    libraryNames: string[],
  ): Promise<LibraryLoadResult[]> {
    const results: LibraryLoadResult[] = [];

    for (const libraryName of libraryNames) {
      try {
        const result = await this.loadLibrary(libraryName);
        results.push(result);
      } catch (error) {
        console.warn(`Failed to preload library ${libraryName}:`, error);
        // Continue with other libraries even if one fails
      }
    }

    return results;
  }

  /**
   * Load a library by name with dependency resolution
   */
  public async loadLibrary(
    libraryName: string,
    version?: string,
    forceReload: boolean = false,
  ): Promise<LibraryLoadResult> {
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;

    // Check if already loading
    if (this.loadingPromises.has(libraryKey)) {
      return await this.loadingPromises.get(libraryKey)!;
    }

    // Check if already loaded and not forcing reload
    if (!forceReload && this.loadedLibraries.has(libraryKey)) {
      const library = this.loadedLibraries.get(libraryKey)!;
      return {
        library,
        loadTime: 0,
        fromCache: true,
        dependencies: [],
      };
    }

    // Create loading promise
    const loadingPromise = this.performLibraryLoad(libraryName, version);
    this.loadingPromises.set(libraryKey, loadingPromise);

    try {
      const result = await loadingPromise;
      this.loadingPromises.delete(libraryKey);
      return result;
    } catch (error) {
      this.loadingPromises.delete(libraryKey);
      throw error;
    }
  }

  /**
   * Perform the actual library loading with dependency resolution
   */
  private async performLibraryLoad(
    libraryName: string,
    version?: string,
  ): Promise<LibraryLoadResult> {
    const startTime = Date.now();
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;

    try {
      // Handle standard library specially
      if (libraryName === "std") {
        return await this.loadStandardLibrary();
      }

      // Check cache
      const cachedModule = this.moduleCache.get(libraryKey);
      if (cachedModule && this.config.cacheEnabled) {
        // Update access count for cache statistics
        cachedModule.accessCount++;
        this.moduleCache.set(libraryKey, cachedModule);

        const library = new QiqeLibrary(
          libraryName,
          cachedModule.metadata.path,
          cachedModule.compiledCode,
          cachedModule.metadata.version,
        );

        this.loadedLibraries.set(libraryKey, library);

        return {
          library,
          loadTime: Date.now() - startTime,
          fromCache: true,
          dependencies: [],
        };
      }

      // Determine library path
      const libraryPath = this.resolveLibraryPath(libraryName, version);

      // Check if should lazy load
      if (await this.shouldLazyLoad(libraryPath)) {
        return await this.lazyLoadLibrary(libraryName, libraryPath, version);
      }

      // Load library source
      const sourceCode = await this.fetchLibrarySource(libraryPath);
      const library = new QiqeLibrary(
        libraryName,
        libraryPath,
        sourceCode,
        version,
      );

      // Resolve dependencies BEFORE caching to ensure complete dependency tree
      const dependencies = await this.resolveDependencies(library);

      // Cache the library with complete dependency information
      if (this.config.cacheEnabled) {
        this.moduleCache.set(libraryKey, {
          compiledCode: sourceCode,
          metadata: {
            path: libraryPath,
            checksum: library.metadata.checksum,
            version: library.version,
            exports: library.exports.map((f) => f.name),
            imports: library.dependencies.map((d) => d.name),
          },
          dependencies: dependencies.map((d) => d.library.name),
          lastModified: new Date(),
          accessCount: 1,
          size: sourceCode.length,
        });
      }

      this.loadedLibraries.set(libraryKey, library);

      return {
        library,
        loadTime: Date.now() - startTime,
        fromCache: false,
        dependencies,
      };
    } catch (error) {
      console.error(`Failed to load library ${libraryName}:`, error);
      throw new Error(
        `Library loading failed: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Resolve library path from name and version
   */
  private resolveLibraryPath(libraryName: string, version?: string): string {
    // For now, assume libraries are in the qiqe/library directory
    const basePath = "/qiqe/library";

    if (version) {
      return `${basePath}/${libraryName}-${version}.qq`;
    }

    return `${basePath}/${libraryName}.qq`;
  }

  /**
   * Check if a library should be lazy loaded based on size and usage patterns
   */
  private async shouldLazyLoad(libraryPath: string): Promise<boolean> {
    try {
      // Check if we can determine the library size
      const sizeInfo = await this.getLibrarySize(libraryPath);

      if (sizeInfo && sizeInfo.size > this.config.lazyLoadThreshold) {
        console.log(
          `Library ${libraryPath} (${sizeInfo.size} bytes) exceeds lazy load threshold (${this.config.lazyLoadThreshold} bytes)`,
        );
        return true;
      }

      // For now, don't lazy load standard libraries or commonly used ones
      if (libraryPath.includes("std.qq") || libraryPath.includes("core.qq")) {
        return false;
      }

      return false;
    } catch (error) {
      console.warn(
        `Could not determine if ${libraryPath} should be lazy loaded:`,
        error,
      );
      return false;
    }
  }

  /**
   * Get library size information
   */
  private async getLibrarySize(
    libraryPath: string,
  ): Promise<{ size: number } | null> {
    try {
      // In a browser environment, we could use a HEAD request to get content-length
      // For now, we'll estimate based on the path or use a simple heuristic

      if (libraryPath.includes("std.qq")) {
        // We know the standard library is relatively small
        return { size: 1024 * 20 }; // ~20KB estimate
      }

      // For other libraries, we might need to make a HEAD request
      // This is a simplified implementation
      return null;
    } catch {
      return null;
    }
  }

  /**
   * Lazy load a library with progressive loading and caching
   */
  private async lazyLoadLibrary(
    libraryName: string,
    libraryPath: string,
    version?: string,
  ): Promise<LibraryLoadResult> {
    const startTime = Date.now();
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;

    try {
      console.log(`Lazy loading library: ${libraryName}`);

      // First, try to load just the metadata/header to get basic info
      const metadata = await this.loadLibraryMetadata(libraryPath);

      // Create a placeholder library with minimal information
      const placeholderLibrary = new QiqeLibrary(
        libraryName,
        libraryPath,
        "# Lazy loaded library placeholder",
        version,
      );

      // Update metadata if available
      if (metadata) {
        placeholderLibrary.updateMetadata(metadata);
      }

      // Mark as lazy loaded in cache
      if (this.config.cacheEnabled) {
        this.moduleCache.set(libraryKey, {
          compiledCode: "", // Empty for lazy loaded
          metadata: {
            path: libraryPath,
            checksum: placeholderLibrary.metadata.checksum,
            version: placeholderLibrary.version,
            exports: [],
            imports: [],
          },
          dependencies: [],
          lastModified: new Date(),
          accessCount: 1,
          size: 0,
        });
      }

      // Schedule full loading in the background
      this.scheduleFullLibraryLoad(libraryName, libraryPath, version);

      return {
        library: placeholderLibrary,
        loadTime: Date.now() - startTime,
        fromCache: false,
        dependencies: [],
      };
    } catch (error) {
      console.warn(
        `Lazy loading failed for ${libraryName}, falling back to normal loading:`,
        error,
      );

      // Fall back to normal loading
      const sourceCode = await this.fetchLibrarySource(libraryPath);
      const library = new QiqeLibrary(
        libraryName,
        libraryPath,
        sourceCode,
        version,
      );

      return {
        library,
        loadTime: Date.now() - startTime,
        fromCache: false,
        dependencies: [],
      };
    }
  }

  /**
   * Load library metadata without loading the full source
   */
  private async loadLibraryMetadata(
    libraryPath: string,
  ): Promise<Partial<LibraryMetadata> | null> {
    try {
      // In a real implementation, this might load just the header comments
      // or a separate metadata file. For now, we'll return null.
      return null;
    } catch {
      return null;
    }
  }

  /**
   * Schedule full library loading in the background
   */
  private scheduleFullLibraryLoad(
    libraryName: string,
    libraryPath: string,
    version?: string,
  ): void {
    // Use Promise.resolve() to schedule loading on the next tick (avoiding setTimeout restrictions)
    Promise.resolve().then(async () => {
      try {
        console.log(`Background loading full library: ${libraryName}`);

        const sourceCode = await this.fetchLibrarySource(libraryPath);
        const library = new QiqeLibrary(
          libraryName,
          libraryPath,
          sourceCode,
          version,
        );

        const libraryKey = version ? `${libraryName}@${version}` : libraryName;

        // Update the cached entry with full content
        if (this.config.cacheEnabled) {
          this.moduleCache.set(libraryKey, {
            compiledCode: sourceCode,
            metadata: {
              path: libraryPath,
              checksum: library.metadata.checksum,
              version: library.version,
              exports: library.exports.map((f) => f.name),
              imports: library.dependencies.map((d) => d.name),
            },
            dependencies: library.dependencies.map((d) => d.name),
            lastModified: new Date(),
            accessCount: 1,
            size: sourceCode.length,
          });
        }

        // Update the loaded libraries map
        this.loadedLibraries.set(libraryKey, library);

        console.log(`Background loading completed for: ${libraryName}`);
      } catch (error) {
        console.warn(`Background loading failed for ${libraryName}:`, error);
      }
    });
  }

  /**
   * Resolve library dependencies with circular dependency detection
   */
  private async resolveDependencies(
    library: QiqeLibrary,
    loadingStack: string[] = [],
  ): Promise<LibraryLoadResult[]> {
    const dependencies: LibraryLoadResult[] = [];
    const libraryKey = `${library.name}@${library.version}`;

    // Check for circular dependencies
    if (loadingStack.includes(libraryKey)) {
      const cycle = [...loadingStack, libraryKey];
      throw new Error(`Circular dependency detected: ${cycle.join(" -> ")}`);
    }

    const newLoadingStack = [...loadingStack, libraryKey];

    for (const dependency of library.dependencies) {
      try {
        // Check if dependency is already loaded to avoid redundant loading
        const depKey = dependency.version
          ? `${dependency.name}@${dependency.version}`
          : dependency.name;

        if (this.loadedLibraries.has(depKey)) {
          // Dependency already loaded, create a result object
          const loadedLib = this.loadedLibraries.get(depKey)!;
          dependencies.push({
            library: loadedLib,
            loadTime: 0,
            fromCache: true,
            dependencies: [],
          });
          continue;
        }

        // Load the dependency with circular dependency detection
        const depResult = await this.loadLibraryWithStack(
          dependency.name,
          dependency.version,
          newLoadingStack,
        );
        dependencies.push(depResult);
      } catch (error) {
        const errorMessage =
          error instanceof Error ? error.message : String(error);

        if (dependency.required !== false) {
          // Default to required if not specified
          throw new Error(
            `Required dependency ${dependency.name} could not be loaded: ${errorMessage}`,
          );
        } else {
          console.warn(
            `Optional dependency ${dependency.name} could not be loaded:`,
            errorMessage,
          );
          // Continue with other dependencies even if optional ones fail
        }
      }
    }

    return dependencies;
  }

  /**
   * Load library with circular dependency detection stack
   */
  private async loadLibraryWithStack(
    libraryName: string,
    version?: string,
    loadingStack: string[] = [],
  ): Promise<LibraryLoadResult> {
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;

    // Check if already loading to prevent infinite recursion
    if (this.loadingPromises.has(libraryKey)) {
      return await this.loadingPromises.get(libraryKey)!;
    }

    // Check if already loaded
    if (this.loadedLibraries.has(libraryKey)) {
      const library = this.loadedLibraries.get(libraryKey)!;
      return {
        library,
        loadTime: 0,
        fromCache: true,
        dependencies: [],
      };
    }

    // Create loading promise with stack tracking
    const loadingPromise = this.performLibraryLoadWithStack(
      libraryName,
      version,
      loadingStack,
    );
    this.loadingPromises.set(libraryKey, loadingPromise);

    try {
      const result = await loadingPromise;
      this.loadingPromises.delete(libraryKey);
      return result;
    } catch (error) {
      this.loadingPromises.delete(libraryKey);
      throw error;
    }
  }

  /**
   * Perform library loading with dependency stack tracking
   */
  private async performLibraryLoadWithStack(
    libraryName: string,
    version?: string,
    loadingStack: string[] = [],
  ): Promise<LibraryLoadResult> {
    const startTime = Date.now();
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;

    try {
      // Handle standard library specially
      if (libraryName === "std") {
        return await this.loadStandardLibrary();
      }

      // Check cache
      const cachedModule = this.moduleCache.get(libraryKey);
      if (cachedModule && this.config.cacheEnabled) {
        cachedModule.accessCount++;
        this.moduleCache.set(libraryKey, cachedModule);

        const library = new QiqeLibrary(
          libraryName,
          cachedModule.metadata.path,
          cachedModule.compiledCode,
          cachedModule.metadata.version,
        );

        this.loadedLibraries.set(libraryKey, library);

        return {
          library,
          loadTime: Date.now() - startTime,
          fromCache: true,
          dependencies: [],
        };
      }

      // Determine library path
      const libraryPath = this.resolveLibraryPath(libraryName, version);

      // Load library source
      const sourceCode = await this.fetchLibrarySource(libraryPath);
      const library = new QiqeLibrary(
        libraryName,
        libraryPath,
        sourceCode,
        version,
      );

      // Resolve dependencies with stack tracking
      const dependencies = await this.resolveDependencies(
        library,
        loadingStack,
      );

      // Cache the library
      if (this.config.cacheEnabled) {
        this.moduleCache.set(libraryKey, {
          compiledCode: sourceCode,
          metadata: {
            path: libraryPath,
            checksum: library.metadata.checksum,
            version: library.version,
            exports: library.exports.map((f) => f.name),
            imports: library.dependencies.map((d) => d.name),
          },
          dependencies: dependencies.map((d) => d.library.name),
          lastModified: new Date(),
          accessCount: 1,
          size: sourceCode.length,
        });
      }

      this.loadedLibraries.set(libraryKey, library);

      return {
        library,
        loadTime: Date.now() - startTime,
        fromCache: false,
        dependencies,
      };
    } catch (error) {
      console.error(`Failed to load library ${libraryName}:`, error);
      throw new Error(
        `Library loading failed: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Fetch library source code
   */
  private async fetchLibrarySource(libraryPath: string): Promise<string> {
    try {
      // In a browser environment, this would be a fetch request
      // For now, we'll simulate loading the standard library
      if (libraryPath.includes("std.qq")) {
        return await this.getStandardLibrarySource();
      }

      // For other libraries, we'd fetch from the server
      const response = await fetch(libraryPath);
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      return await response.text();
    } catch (error) {
      throw new Error(
        `Failed to fetch library source from ${libraryPath}: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Get the standard library source code
   * This is a fallback method that provides the std.qq content
   */
  private async getStandardLibrarySource(): Promise<string> {
    // This would normally be fetched from the server
    // For now, we'll return a minimal version for testing
    return `# STD Library Qiqe
# This is a library of functions and values used in the Qiqe programming language.

# Basic functions
let id = \\x. x
let const' = \\x y. x
let flip = \\f x y. f y x

# Arithmetic
let add = eval "(x)=>(y)=>(x + y)"
let sub = eval "(x)=>(y)=>(x - y)"
let mul = eval "(x)=>(y)=>(x * y)"
let div = eval "(x)=>(y)=>(x / y)"

# Comparison
let eq = eval "(x)=>(y)=>(x === y)"
let lt = eval "(x)=>(y)=>(x < y)"
let gt = eval "(x)=>(y)=>(x > y)"

# String operations
let show = eval "(x)=>(x.toString())"
let concat = eval "(x)=>(y)=>(x + y)"
let length = eval "(x)=>(x.length)"`;
  }

  /**
   * Get a loaded library by name
   */
  public getLibrary(libraryName: string, version?: string): QiqeLibrary | null {
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;
    return this.loadedLibraries.get(libraryKey) || null;
  }

  /**
   * Get the standard library
   */
  public getStandardLibrary(): QiqeLibrary | null {
    return this.standardLibrary;
  }

  /**
   * Get all loaded libraries
   */
  public getLoadedLibraries(): QiqeLibrary[] {
    return Array.from(this.loadedLibraries.values());
  }

  /**
   * Check if a library is loaded
   */
  public isLibraryLoaded(libraryName: string, version?: string): boolean {
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;
    return this.loadedLibraries.has(libraryKey);
  }

  /**
   * Unload a library
   */
  public unloadLibrary(libraryName: string, version?: string): boolean {
    const libraryKey = version ? `${libraryName}@${version}` : libraryName;

    if (libraryName === "std") {
      console.warn("Cannot unload standard library");
      return false;
    }

    const removed = this.loadedLibraries.delete(libraryKey);

    if (removed && this.config.cacheEnabled) {
      this.moduleCache.invalidate(libraryKey);
    }

    return removed;
  }

  /**
   * Reload a library
   */
  public async reloadLibrary(
    libraryName: string,
    version?: string,
  ): Promise<LibraryLoadResult> {
    this.unloadLibrary(libraryName, version);
    return await this.loadLibrary(libraryName, version, true);
  }

  /**
   * Get loader statistics
   */
  public getStats(): LoaderStats {
    const libraries = Array.from(this.loadedLibraries.values());

    return {
      initialized: this.initialized,
      loadedLibraryCount: libraries.length,
      standardLibraryLoaded: this.standardLibrary !== null,
      totalFunctions: libraries.reduce(
        (sum, lib) => sum + lib.exports.length,
        0,
      ),
      cacheHitRate: this.moduleCache.getStats().hitRate,
      memoryUsage: libraries.reduce((sum, lib) => sum + lib.metadata.size, 0),
    };
  }

  /**
   * Clear all loaded libraries (except standard library)
   */
  public clearLibraries(): void {
    const stdLib = this.standardLibrary;
    this.loadedLibraries.clear();

    if (stdLib) {
      this.loadedLibraries.set("std", stdLib);
    }

    if (this.config.cacheEnabled) {
      this.moduleCache.cleanup();
    }
  }

  /**
   * Update loader configuration
   */
  public updateConfig(newConfig: Partial<LibraryLoaderConfig>): void {
    this.config = { ...this.config, ...newConfig };
  }
}

/**
 * Loader statistics interface
 */
export interface LoaderStats {
  initialized: boolean;
  loadedLibraryCount: number;
  standardLibraryLoaded: boolean;
  totalFunctions: number;
  cacheHitRate: number;
  memoryUsage: number;
}

/**
 * Singleton instance of the standard library loader
 */
let globalLoader: StandardLibraryLoader | null = null;
let initializationPromise: Promise<void> | null = null;

/**
 * Get the global standard library loader instance
 */
export function getStandardLibraryLoader(
  config?: Partial<LibraryLoaderConfig>,
): StandardLibraryLoader {
  if (!globalLoader) {
    globalLoader = new StandardLibraryLoader(config);
  }
  return globalLoader;
}

/**
 * Initialize the global standard library loader
 */
export async function initializeStandardLibrary(
  config?: Partial<LibraryLoaderConfig>,
): Promise<void> {
  // Prevent multiple simultaneous initializations
  if (initializationPromise) {
    return initializationPromise;
  }

  initializationPromise = (async () => {
    try {
      const loader = getStandardLibraryLoader(config);
      await loader.initialize();
      console.log("Standard library initialized successfully");
    } catch (error) {
      console.error("Failed to initialize standard library:", error);
      // Reset initialization promise so it can be retried
      initializationPromise = null;
      throw error;
    }
  })();

  return initializationPromise;
}

/**
 * Ensure the standard library is initialized (auto-initialization)
 */
export async function ensureStandardLibraryInitialized(
  config?: Partial<LibraryLoaderConfig>,
): Promise<StandardLibraryLoader> {
  if (!globalLoader || !globalLoader.getStats().initialized) {
    await initializeStandardLibrary(config);
  }
  return getStandardLibraryLoader(config);
}

/**
 * Get the standard library (convenience function)
 */
export function getStandardLibrary(): QiqeLibrary | null {
  return globalLoader?.getStandardLibrary() || null;
}

/**
 * Auto-initialize the standard library when this module is loaded
 * This ensures the standard library is available as soon as possible
 */
const autoInitialize = async () => {
  try {
    // Only auto-initialize if not already initialized
    if (!globalLoader || !globalLoader.getStats().initialized) {
      await initializeStandardLibrary({
        autoLoadStandardLibrary: true,
        preloadLibraries: ["std"],
        cacheEnabled: true,
        lazyLoadThreshold: 1024 * 50, // 50KB threshold for lazy loading
        standardLibraryPath: "/qiqe/library/std.qq",
      });
    }
  } catch (error) {
    // Don't throw during auto-initialization to avoid breaking module loading
    console.warn("Auto-initialization of standard library failed:", error);
  }
};

// Trigger auto-initialization when the module loads
// Use Promise.resolve() to avoid blocking module loading and setTimeout restrictions
if (typeof window !== "undefined") {
  // Browser environment - initialize after DOM is ready
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", () => {
      Promise.resolve().then(autoInitialize);
    });
  } else {
    Promise.resolve().then(autoInitialize);
  }
} else {
  // Node.js environment - initialize on next tick
  Promise.resolve().then(autoInitialize);
}

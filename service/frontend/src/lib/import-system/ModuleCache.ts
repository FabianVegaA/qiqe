/**
 * ModuleCache - Intelligent caching system for compiled modules
 */

import { CachedModule, ModuleMetadata, CacheStats } from "./types";

interface CacheEntry {
  module: CachedModule;
  lastAccessed: Date;
  accessCount: number;
  memorySize: number;
}

interface CacheConfig {
  maxMemorySize: number; // Maximum memory usage in bytes
  maxEntries: number; // Maximum number of cached entries
  ttl: number; // Time to live in milliseconds
  checkInterval: number; // Cleanup check interval in milliseconds
}

export class ModuleCache {
  private memoryCache = new Map<string, CacheEntry>();
  private diskCache = new Map<string, string>(); // Simulated disk cache
  private config: CacheConfig;
  private stats: CacheStats;
  private cleanupTimer: NodeJS.Timeout | null = null;

  constructor(config: Partial<CacheConfig> = {}) {
    this.config = {
      maxMemorySize: config.maxMemorySize || 50 * 1024 * 1024, // 50MB default
      maxEntries: config.maxEntries || 1000,
      ttl: config.ttl || 30 * 60 * 1000, // 30 minutes default
      checkInterval: config.checkInterval || 5 * 60 * 1000, // 5 minutes default
    };

    this.stats = {
      totalEntries: 0,
      memoryUsage: 0,
      hitRate: 0,
      missRate: 0,
      evictionCount: 0,
    };

    this.startCleanupTimer();
  }

  /**
   * Get cached module
   */
  get(key: string): CachedModule | null {
    // Try memory cache first
    const memoryEntry = this.memoryCache.get(key);
    if (memoryEntry) {
      // Update access statistics
      memoryEntry.lastAccessed = new Date();
      memoryEntry.accessCount++;
      this.updateHitRate(true);

      // Check if module is still valid
      if (this.isModuleValid(memoryEntry.module)) {
        return memoryEntry.module;
      } else {
        // Module is stale, remove from cache
        this.invalidate(key);
      }
    }

    // Try disk cache
    const diskEntry = this.diskCache.get(key);
    if (diskEntry) {
      try {
        const module = JSON.parse(diskEntry) as CachedModule;
        if (this.isModuleValid(module)) {
          // Promote to memory cache
          this.setInMemory(key, module);
          this.updateHitRate(true);
          return module;
        } else {
          // Remove stale entry from disk cache
          this.diskCache.delete(key);
        }
      } catch (error) {
        // Invalid JSON, remove from disk cache
        this.diskCache.delete(key);
      }
    }

    this.updateHitRate(false);
    return null;
  }

  /**
   * Set cached module
   */
  set(key: string, module: CachedModule): void {
    // Set in memory cache
    this.setInMemory(key, module);

    // Also set in disk cache for persistence
    this.setInDisk(key, module);

    // Trigger cleanup if needed
    this.checkAndCleanup();
  }

  /**
   * Set module in memory cache
   */
  private setInMemory(key: string, module: CachedModule): void {
    const memorySize = this.calculateMemorySize(module);

    // Check if we need to evict entries to make space
    this.ensureMemorySpace(memorySize);

    const entry: CacheEntry = {
      module,
      lastAccessed: new Date(),
      accessCount: 1,
      memorySize,
    };

    // Remove existing entry if present
    if (this.memoryCache.has(key)) {
      const oldEntry = this.memoryCache.get(key)!;
      this.stats.memoryUsage -= oldEntry.memorySize;
    } else {
      this.stats.totalEntries++;
    }

    this.memoryCache.set(key, entry);
    this.stats.memoryUsage += memorySize;
  }

  /**
   * Set module in disk cache
   */
  private setInDisk(key: string, module: CachedModule): void {
    try {
      const serialized = JSON.stringify(module);
      this.diskCache.set(key, serialized);
    } catch (error) {
      console.warn(`Failed to serialize module for disk cache: ${key}`, error);
    }
  }

  /**
   * Invalidate cached module
   */
  invalidate(key: string): void {
    const memoryEntry = this.memoryCache.get(key);
    if (memoryEntry) {
      this.stats.memoryUsage -= memoryEntry.memorySize;
      this.stats.totalEntries--;
      this.memoryCache.delete(key);
    }

    this.diskCache.delete(key);
  }

  /**
   * Invalidate modules based on file modification times
   */
  invalidateByModificationTime(filePath: string, modificationTime: Date): void {
    const keysToInvalidate: string[] = [];

    // Check memory cache
    for (const [key, entry] of Array.from(this.memoryCache.entries())) {
      if (
        entry.module.metadata.path === filePath &&
        entry.module.lastModified < modificationTime
      ) {
        keysToInvalidate.push(key);
      }
    }

    // Check disk cache
    for (const [key, serializedModule] of Array.from(
      this.diskCache.entries(),
    )) {
      try {
        const module = JSON.parse(serializedModule) as CachedModule;
        if (
          module.metadata.path === filePath &&
          module.lastModified < modificationTime
        ) {
          keysToInvalidate.push(key);
        }
      } catch (error) {
        // Invalid JSON, mark for removal
        keysToInvalidate.push(key);
      }
    }

    // Invalidate all identified keys
    for (const key of keysToInvalidate) {
      this.invalidate(key);
    }
  }

  /**
   * Invalidate modules that depend on a specific module
   */
  invalidateDependents(modulePath: string): void {
    const keysToInvalidate: string[] = [];

    // Check memory cache for dependents
    for (const [key, entry] of Array.from(this.memoryCache.entries())) {
      if (entry.module.dependencies.includes(modulePath)) {
        keysToInvalidate.push(key);
      }
    }

    // Check disk cache for dependents
    for (const [key, serializedModule] of Array.from(
      this.diskCache.entries(),
    )) {
      try {
        const module = JSON.parse(serializedModule) as CachedModule;
        if (module.dependencies.includes(modulePath)) {
          keysToInvalidate.push(key);
        }
      } catch (error) {
        // Invalid JSON, mark for removal
        keysToInvalidate.push(key);
      }
    }

    // Invalidate all dependents
    for (const key of keysToInvalidate) {
      this.invalidate(key);
    }
  }

  /**
   * Cleanup expired and least recently used entries
   */
  cleanup(): void {
    const now = new Date();
    const keysToEvict: string[] = [];

    // Find expired entries
    for (const [key, entry] of Array.from(this.memoryCache.entries())) {
      const age = now.getTime() - entry.lastAccessed.getTime();
      if (age > this.config.ttl) {
        keysToEvict.push(key);
      }
    }

    // Evict expired entries
    for (const key of keysToEvict) {
      this.evictEntry(key);
    }

    // If still over limits, evict LRU entries
    this.evictLRUEntries();

    // Clean up disk cache
    this.cleanupDiskCache();
  }

  /**
   * Evict least recently used entries if over limits
   */
  private evictLRUEntries(): void {
    // Check memory limit
    while (
      this.stats.memoryUsage > this.config.maxMemorySize &&
      this.memoryCache.size > 0
    ) {
      const lruKey = this.findLRUKey();
      if (lruKey) {
        this.evictEntry(lruKey);
      } else {
        break;
      }
    }

    // Check entry count limit
    while (this.memoryCache.size > this.config.maxEntries) {
      const lruKey = this.findLRUKey();
      if (lruKey) {
        this.evictEntry(lruKey);
      } else {
        break;
      }
    }
  }

  /**
   * Find least recently used key
   */
  private findLRUKey(): string | null {
    let lruKey: string | null = null;
    let oldestAccess = new Date();

    for (const [key, entry] of Array.from(this.memoryCache.entries())) {
      if (entry.lastAccessed < oldestAccess) {
        oldestAccess = entry.lastAccessed;
        lruKey = key;
      }
    }

    return lruKey;
  }

  /**
   * Evict a specific entry
   */
  private evictEntry(key: string): void {
    const entry = this.memoryCache.get(key);
    if (entry) {
      this.stats.memoryUsage -= entry.memorySize;
      this.stats.totalEntries--;
      this.stats.evictionCount++;
      this.memoryCache.delete(key);
    }
  }

  /**
   * Clean up disk cache
   */
  private cleanupDiskCache(): void {
    const now = new Date();
    const keysToRemove: string[] = [];

    for (const [key, serializedModule] of Array.from(
      this.diskCache.entries(),
    )) {
      try {
        const module = JSON.parse(serializedModule) as CachedModule;
        const age = now.getTime() - module.lastModified.getTime();

        if (age > this.config.ttl * 2) {
          // Keep disk cache longer than memory
          keysToRemove.push(key);
        }
      } catch (error) {
        // Invalid JSON, remove
        keysToRemove.push(key);
      }
    }

    for (const key of Array.from(keysToRemove)) {
      this.diskCache.delete(key);
    }
  }

  /**
   * Check if module is still valid
   */
  private isModuleValid(module: CachedModule): boolean {
    // Check if module has expired
    const now = new Date();
    const age = now.getTime() - module.lastModified.getTime();

    if (age > this.config.ttl) {
      return false;
    }

    // In a real implementation, we would check file modification times
    // For now, assume modules are valid if not expired
    return true;
  }

  /**
   * Calculate memory size of a module
   */
  private calculateMemorySize(module: CachedModule): number {
    // Rough estimation of memory usage
    const jsonString = JSON.stringify(module);
    return jsonString.length * 2; // Approximate UTF-16 encoding
  }

  /**
   * Ensure there's enough memory space for a new entry
   */
  private ensureMemorySpace(requiredSize: number): void {
    while (
      this.stats.memoryUsage + requiredSize > this.config.maxMemorySize &&
      this.memoryCache.size > 0
    ) {
      const lruKey = this.findLRUKey();
      if (lruKey) {
        this.evictEntry(lruKey);
      } else {
        break;
      }
    }
  }

  /**
   * Check and cleanup if needed
   */
  private checkAndCleanup(): void {
    if (
      this.stats.memoryUsage > this.config.maxMemorySize * 0.9 ||
      this.memoryCache.size > this.config.maxEntries * 0.9
    ) {
      this.cleanup();
    }
  }

  /**
   * Update hit rate statistics
   */
  private updateHitRate(hit: boolean): void {
    const totalRequests = this.stats.hitRate + this.stats.missRate + 1;

    if (hit) {
      this.stats.hitRate = (this.stats.hitRate + 1) / totalRequests;
      this.stats.missRate = this.stats.missRate / totalRequests;
    } else {
      this.stats.hitRate = this.stats.hitRate / totalRequests;
      this.stats.missRate = (this.stats.missRate + 1) / totalRequests;
    }
  }

  /**
   * Start cleanup timer (disabled to avoid runtime restrictions)
   */
  private startCleanupTimer(): void {
    // Disable automatic cleanup to avoid setInterval restrictions in qiqe runtime
    // Manual cleanup can be triggered by calling cleanup() directly
    console.log("Automatic cleanup disabled to avoid runtime restrictions");
  }

  /**
   * Stop cleanup timer
   */
  private stopCleanupTimer(): void {
    if (this.cleanupTimer) {
      clearInterval(this.cleanupTimer);
      this.cleanupTimer = null;
    }
  }

  /**
   * Get cache statistics
   */
  getStats(): CacheStats {
    return { ...this.stats };
  }

  /**
   * Clear all cached entries
   */
  clear(): void {
    this.memoryCache.clear();
    this.diskCache.clear();
    this.stats = {
      totalEntries: 0,
      memoryUsage: 0,
      hitRate: 0,
      missRate: 0,
      evictionCount: 0,
    };
  }

  /**
   * Get all cached keys
   */
  getKeys(): string[] {
    const memoryKeys = Array.from(this.memoryCache.keys());
    const diskKeys = Array.from(this.diskCache.keys());
    const allKeys = new Set([...memoryKeys, ...diskKeys]);
    return Array.from(allKeys);
  }

  /**
   * Check if key exists in cache
   */
  has(key: string): boolean {
    return this.memoryCache.has(key) || this.diskCache.has(key);
  }

  /**
   * Get cache size information
   */
  getSize(): { memory: number; disk: number; total: number } {
    return {
      memory: this.memoryCache.size,
      disk: this.diskCache.size,
      total: this.stats.totalEntries,
    };
  }

  /**
   * Destroy cache and cleanup resources
   */
  destroy(): void {
    this.stopCleanupTimer();
    this.clear();
  }
}

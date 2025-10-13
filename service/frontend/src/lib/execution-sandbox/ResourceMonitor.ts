/**
 * Resource monitoring system for execution sandbox
 */

import { ResourceUsage, ResourceLimits, SecurityViolation } from "./types";
import { ErrorType } from "../error-handling/types";

export class ResourceMonitor {
  private monitoringIntervals: Map<string, NodeJS.Timeout> = new Map();
  private resourceUsage: Map<string, ResourceUsage> = new Map();
  private violations: Map<string, SecurityViolation[]> = new Map();
  private readonly MONITORING_INTERVAL_MS = 100;

  /**
   * Start monitoring resources for a specific execution context
   */
  startMonitoring(
    contextId: string,
    limits: ResourceLimits,
    onViolation: (violation: SecurityViolation) => void,
  ): void {
    // Initialize resource usage tracking
    this.resourceUsage.set(contextId, {
      memoryUsageMB: 0,
      executionTimeMs: 0,
      callStackDepth: 0,
      outputLength: 0,
      lastUpdated: new Date(),
    });

    this.violations.set(contextId, []);
  }

  /**
   * Stop monitoring for a specific context
   */
  stopMonitoring(contextId: string): void {
    const interval = this.monitoringIntervals.get(contextId);
    if (interval) {
      clearInterval(interval);
      this.monitoringIntervals.delete(contextId);
    }

    // Clean up data
    this.resourceUsage.delete(contextId);
    this.violations.delete(contextId);
  }

  /**
   * Get current resource usage for a context
   */
  getResourceUsage(contextId: string): ResourceUsage | null {
    return this.resourceUsage.get(contextId) || null;
  }

  /**
   * Update resource usage manually
   */
  updateResourceUsage(contextId: string, usage: Partial<ResourceUsage>): void {
    const current = this.resourceUsage.get(contextId);
    if (current) {
      this.resourceUsage.set(contextId, {
        ...current,
        ...usage,
        lastUpdated: new Date(),
      });
    }
  }

  /**
   * Get security violations for a context
   */
  getViolations(contextId: string): SecurityViolation[] {
    return this.violations.get(contextId) || [];
  }

  /**
   * Record a security violation
   */
  recordViolation(contextId: string, violation: SecurityViolation): void {
    const violations = this.violations.get(contextId) || [];
    violations.push(violation);
    this.violations.set(contextId, violations);
  }

  /**
   * Check if resource usage exceeds limits
   */
  private checkResourceUsage(
    contextId: string,
    limits: ResourceLimits,
    onViolation: (violation: SecurityViolation) => void,
  ): void {
    const usage = this.resourceUsage.get(contextId);
    if (!usage) return;

    // Update execution time
    const startTime = this.getContextStartTime(contextId);
    if (startTime) {
      usage.executionTimeMs = Date.now() - startTime.getTime();
    }

    // Estimate memory usage (browser limitation - approximate)
    usage.memoryUsageMB = this.estimateMemoryUsage();

    // Check memory limit
    if (usage.memoryUsageMB > limits.maxMemoryMB) {
      const violation: SecurityViolation = {
        type: "resource_limit",
        description: `Memory usage (${usage.memoryUsageMB}MB) exceeds limit (${limits.maxMemoryMB}MB)`,
        severity: "high",
        timestamp: new Date(),
        blocked: true,
      };
      this.recordViolation(contextId, violation);
      onViolation(violation);
    }

    // Check execution time limit
    if (usage.executionTimeMs > limits.maxExecutionTimeMs) {
      const violation: SecurityViolation = {
        type: "resource_limit",
        description: `Execution time (${usage.executionTimeMs}ms) exceeds limit (${limits.maxExecutionTimeMs}ms)`,
        severity: "high",
        timestamp: new Date(),
        blocked: true,
      };
      this.recordViolation(contextId, violation);
      onViolation(violation);
    }

    // Check call stack depth (if available)
    const stackDepth = this.estimateCallStackDepth();
    if (stackDepth > limits.maxCallStackDepth) {
      const violation: SecurityViolation = {
        type: "resource_limit",
        description: `Call stack depth (${stackDepth}) exceeds limit (${limits.maxCallStackDepth})`,
        severity: "medium",
        timestamp: new Date(),
        blocked: true,
      };
      this.recordViolation(contextId, violation);
      onViolation(violation);
    }

    usage.callStackDepth = stackDepth;
    usage.lastUpdated = new Date();
  }

  /**
   * Estimate memory usage (browser approximation)
   */
  private estimateMemoryUsage(): number {
    // Browser memory estimation is limited
    // We can use performance.memory if available (Chrome)
    if ("memory" in performance) {
      const memory = (performance as any).memory;
      return memory.usedJSHeapSize / (1024 * 1024); // Convert to MB
    }

    // Fallback: rough estimation based on object count
    // This is very approximate and not reliable
    return 0;
  }

  /**
   * Estimate call stack depth
   */
  private estimateCallStackDepth(): number {
    try {
      // Create an error to get stack trace
      const error = new Error();
      const stack = error.stack;
      if (stack) {
        // Count stack frames (rough approximation)
        return stack.split("\n").length - 1;
      }
    } catch (e) {
      // Ignore errors in stack depth estimation
    }
    return 0;
  }

  /**
   * Get context start time (would be stored elsewhere in real implementation)
   */
  private getContextStartTime(contextId: string): Date | null {
    // In a real implementation, this would be stored in the execution context
    // For now, we'll use a simple approximation
    return new Date(
      Date.now() - (this.resourceUsage.get(contextId)?.executionTimeMs || 0),
    );
  }

  /**
   * Clean up all monitoring data
   */
  cleanup(): void {
    // Stop all monitoring intervals
    this.monitoringIntervals.forEach((interval, contextId) => {
      this.stopMonitoring(contextId);
    });

    // Clear all data
    this.resourceUsage.clear();
    this.violations.clear();
  }

  /**
   * Get monitoring statistics
   */
  getStats(): {
    activeContexts: number;
    totalViolations: number;
    averageMemoryUsage: number;
    averageExecutionTime: number;
  } {
    const activeContexts = this.monitoringIntervals.size;
    let totalViolations = 0;
    let totalMemory = 0;
    let totalTime = 0;
    let contextCount = 0;

    this.resourceUsage.forEach((usage, contextId) => {
      const violations = this.violations.get(contextId) || [];
      totalViolations += violations.length;
      totalMemory += usage.memoryUsageMB;
      totalTime += usage.executionTimeMs;
      contextCount++;
    });

    return {
      activeContexts,
      totalViolations,
      averageMemoryUsage: contextCount > 0 ? totalMemory / contextCount : 0,
      averageExecutionTime: contextCount > 0 ? totalTime / contextCount : 0,
    };
  }
}

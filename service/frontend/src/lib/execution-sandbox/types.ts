/**
 * Type definitions for the execution sandbox system
 */

import { ErrorInfo } from "../error-handling/types";

// Sandbox security levels
export enum SandboxLevel {
  STRICT = "strict",
  MODERATE = "moderate",
  PERMISSIVE = "permissive",
}

// Resource usage information
export interface ResourceUsage {
  memoryUsageMB: number;
  executionTimeMs: number;
  callStackDepth: number;
  outputLength: number;
  lastUpdated: Date;
}

// Resource limits configuration
export interface ResourceLimits {
  maxMemoryMB: number;
  maxExecutionTimeMs: number;
  maxCallStackDepth: number;
  maxOutputLength: number;
}

// Sandbox configuration options
export interface SandboxOptions {
  memoryLimit: number;
  timeoutMs: number;
  allowedGlobals: string[];
  restrictedAPIs: string[];
  enableAsyncOps: boolean;
  sandboxLevel: SandboxLevel;
  enableResourceMonitoring: boolean;
}

// Execution context for sandboxed code
export interface ExecutionContext {
  id: string;
  globals: Record<string, any>;
  sandbox: SandboxInstance;
  resourceLimits: ResourceLimits;
  startTime: Date;
  isActive: boolean;
  memoryBaseline: number;
}

// Sandbox instance information
export interface SandboxInstance {
  id: string;
  iframe?: HTMLIFrameElement;
  worker?: Worker;
  vm?: any; // For Node.js vm module if available
  type: "iframe" | "worker" | "vm" | "direct";
  created: Date;
  lastUsed: Date;
}

// Execution result from sandbox
export interface ExecutionResult {
  output: string;
  errors: ErrorInfo[];
  warnings: string[];
  resourceUsage: ResourceUsage;
  executionTime: number;
  success: boolean;
  terminated: boolean;
}

// Sandbox monitoring data
export interface SandboxMonitoringData {
  contextId: string;
  resourceUsage: ResourceUsage;
  violations: SecurityViolation[];
  performance: PerformanceMetrics;
}

// Security violation information
export interface SecurityViolation {
  type: "api_access" | "resource_limit" | "code_injection" | "escape_attempt";
  description: string;
  severity: "low" | "medium" | "high" | "critical";
  timestamp: Date;
  blocked: boolean;
}

// Performance metrics
export interface PerformanceMetrics {
  compilationTime: number;
  executionTime: number;
  memoryPeak: number;
  memoryAverage: number;
  gcCollections: number;
}

// API access control configuration
export interface APIAccessControl {
  allowedGlobals: Set<string>;
  blockedGlobals: Set<string>;
  allowedPatterns: RegExp[];
  blockedPatterns: RegExp[];
  customValidators: ((api: string) => boolean)[];
}

// Sandbox termination reason
export enum TerminationReason {
  TIMEOUT = "timeout",
  MEMORY_LIMIT = "memory_limit",
  SECURITY_VIOLATION = "security_violation",
  USER_REQUEST = "user_request",
  ERROR = "error",
  COMPLETED = "completed",
}

// Sandbox event types
export interface SandboxEvents {
  "resource-warning": (usage: ResourceUsage) => void;
  "resource-limit": (limit: keyof ResourceLimits) => void;
  "security-violation": (violation: SecurityViolation) => void;
  "execution-complete": (result: ExecutionResult) => void;
  "execution-terminated": (reason: TerminationReason) => void;
}

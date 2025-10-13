/**
 * Error handling type definitions and interfaces for the qiqe code evaluation system
 */

// Error severity levels
export enum ErrorSeverity {
  INFO = "info",
  WARNING = "warning",
  ERROR = "error",
  CRITICAL = "critical",
}

// Error type classification
export enum ErrorType {
  SYNTAX_ERROR = "syntax_error",
  COMPILATION_ERROR = "compilation_error",
  RUNTIME_ERROR = "runtime_error",
  IMPORT_ERROR = "import_error",
  RESOURCE_ERROR = "resource_error",
  SECURITY_ERROR = "security_error",
  TIMEOUT_ERROR = "timeout_error",
  MEMORY_ERROR = "memory_error",
}

// Location information for qiqe source code
export interface QiqeLocation {
  line: number;
  column: number;
  file?: string;
  length?: number;
}

// Location information for JavaScript code
export interface JSLocation {
  line: number;
  column: number;
  source?: string;
}

// Stack frame information
export interface StackFrame {
  functionName: string;
  qiqeLocation?: QiqeLocation;
  jsLocation: JSLocation;
  source: string;
  isUserCode: boolean;
}

// Location mapping between qiqe and JavaScript
export interface LocationMapping {
  qiqeLocation: QiqeLocation;
  jsLocation: JSLocation;
  context: string;
}

// Error context information
export interface ErrorContext {
  executionId: string;
  timestamp: Date;
  codeSnippet: string;
  imports: string[];
  executionPhase: "parsing" | "compilation" | "execution";
  memoryUsage?: number;
  executionTime?: number;
}

// Core error information structure
export interface ErrorInfo {
  id: string;
  type: ErrorType;
  severity: ErrorSeverity;
  message: string;
  originalError?: Error;
  location?: QiqeLocation;
  stackTrace: StackFrame[];
  context: ErrorContext;
  suggestions: string[];
  recoverable: boolean;
  timestamp: Date;
}

// Formatted error for display
export interface FormattedError {
  title: string;
  message: string;
  location?: string;
  codeSnippet?: string;
  stackTrace?: string;
  suggestions: string[];
  severity: ErrorSeverity;
  canRecover: boolean;
}

// Source map for qiqe to JavaScript mapping
export interface SourceMap {
  qiqeToJs: LocationMapping[];
  jsToQiqe: LocationMapping[];
  functionMappings: FunctionMapping[];
  version: string;
}

// Function mapping information
export interface FunctionMapping {
  qiqeFunctionName: string;
  jsFunctionName: string;
  qiqeLocation: QiqeLocation;
  jsLocation: JSLocation;
}

// Error recovery result
export interface RecoveryResult {
  success: boolean;
  action: RecoveryAction;
  message: string;
  modifiedCode?: string;
}

// Recovery action types
export enum RecoveryAction {
  RETRY = "retry",
  SKIP = "skip",
  FALLBACK = "fallback",
  ABORT = "abort",
  SUGGEST_FIX = "suggest_fix",
}

// Error statistics for monitoring
export interface ErrorStats {
  totalErrors: number;
  errorsByType: Record<ErrorType, number>;
  errorsBySeverity: Record<ErrorSeverity, number>;
  recoverySuccessRate: number;
  averageResolutionTime: number;
}

// Error handler configuration
export interface ErrorHandlerConfig {
  enableSourceMapping: boolean;
  enableRecovery: boolean;
  maxStackTraceDepth: number;
  includeCodeSnippets: boolean;
  enableSuggestions: boolean;
  logLevel: ErrorSeverity;
}

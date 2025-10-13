/**
 * Error handling module exports
 */

// Export all types and interfaces
export * from "./types";

// Export constants and utilities
export * from "./constants";

// Export classes
export { ErrorHandler } from "./ErrorHandler";
export { SourceMapBuilder } from "./SourceMapBuilder";

// Re-export commonly used types for convenience
export type {
  ErrorInfo,
  ErrorContext,
  FormattedError,
  StackFrame,
  QiqeLocation,
  JSLocation,
  LocationMapping,
  SourceMap,
  RecoveryResult,
} from "./types";

export { ErrorType, ErrorSeverity, RecoveryAction } from "./types";

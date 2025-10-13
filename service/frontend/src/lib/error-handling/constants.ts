/**
 * Constants and utility definitions for error handling
 */

import { ErrorType, ErrorSeverity } from "./types";

// Error type to severity mapping
export const ERROR_TYPE_SEVERITY_MAP: Record<ErrorType, ErrorSeverity> = {
  [ErrorType.SYNTAX_ERROR]: ErrorSeverity.ERROR,
  [ErrorType.COMPILATION_ERROR]: ErrorSeverity.ERROR,
  [ErrorType.RUNTIME_ERROR]: ErrorSeverity.ERROR,
  [ErrorType.IMPORT_ERROR]: ErrorSeverity.ERROR,
  [ErrorType.RESOURCE_ERROR]: ErrorSeverity.WARNING,
  [ErrorType.SECURITY_ERROR]: ErrorSeverity.CRITICAL,
  [ErrorType.TIMEOUT_ERROR]: ErrorSeverity.WARNING,
  [ErrorType.MEMORY_ERROR]: ErrorSeverity.WARNING,
};

// Common error messages
export const ERROR_MESSAGES = {
  SYNTAX_ERROR: "Syntax error in qiqe code",
  COMPILATION_ERROR: "Failed to compile qiqe code to JavaScript",
  RUNTIME_ERROR: "Runtime error during code execution",
  IMPORT_ERROR: "Failed to import library or module",
  RESOURCE_ERROR: "Resource limit exceeded",
  SECURITY_ERROR: "Security violation detected",
  TIMEOUT_ERROR: "Code execution timed out",
  MEMORY_ERROR: "Memory limit exceeded",
  UNKNOWN_ERROR: "An unknown error occurred",
} as const;

// Error recovery suggestions
export const RECOVERY_SUGGESTIONS = {
  [ErrorType.SYNTAX_ERROR]: [
    "Check for missing semicolons or brackets",
    "Verify function syntax and parameter lists",
    "Ensure proper indentation and code structure",
  ],
  [ErrorType.COMPILATION_ERROR]: [
    "Check for unsupported language features",
    "Verify variable declarations and scoping",
    "Ensure all functions are properly defined",
  ],
  [ErrorType.RUNTIME_ERROR]: [
    "Check for undefined variables or functions",
    "Verify function arguments and return values",
    "Add error handling for edge cases",
  ],
  [ErrorType.IMPORT_ERROR]: [
    "Verify the import path is correct",
    "Check if the library exists and is accessible",
    "Ensure proper import syntax",
  ],
  [ErrorType.RESOURCE_ERROR]: [
    "Optimize code to use less memory",
    "Reduce the complexity of operations",
    "Consider breaking large operations into smaller chunks",
  ],
  [ErrorType.SECURITY_ERROR]: [
    "Remove potentially dangerous operations",
    "Use approved APIs and functions only",
    "Avoid direct JavaScript eval statements",
  ],
  [ErrorType.TIMEOUT_ERROR]: [
    "Optimize algorithm efficiency",
    "Avoid infinite loops",
    "Consider using iterative instead of recursive approaches",
  ],
  [ErrorType.MEMORY_ERROR]: [
    "Reduce data structure sizes",
    "Clear unused variables",
    "Optimize memory usage patterns",
  ],
} as const;

// Maximum values for error handling
export const ERROR_LIMITS = {
  MAX_STACK_TRACE_DEPTH: 50,
  MAX_ERROR_MESSAGE_LENGTH: 1000,
  MAX_CODE_SNIPPET_LENGTH: 200,
  MAX_SUGGESTIONS: 5,
  MAX_ERROR_HISTORY: 100,
} as const;

// Regular expressions for error pattern matching
export const ERROR_PATTERNS = {
  SYNTAX_ERROR: /SyntaxError|Unexpected token|Unexpected end of input/i,
  REFERENCE_ERROR: /ReferenceError|is not defined/i,
  TYPE_ERROR: /TypeError|Cannot read property|Cannot call method/i,
  RANGE_ERROR: /RangeError|Maximum call stack size exceeded/i,
  TIMEOUT_ERROR: /timeout|execution time limit/i,
  MEMORY_ERROR: /out of memory|heap|allocation failed/i,
} as const;

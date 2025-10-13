/**
 * ErrorHandler class for capturing, formatting, and managing errors in qiqe code execution
 */

import {
  ErrorInfo,
  ErrorContext,
  FormattedError,
  StackFrame,
  QiqeLocation,
  JSLocation,
  LocationMapping,
  SourceMap,
  ErrorType,
  ErrorSeverity,
  RecoveryResult,
  RecoveryAction,
  ErrorHandlerConfig,
} from "./types";

import {
  ERROR_TYPE_SEVERITY_MAP,
  ERROR_MESSAGES,
  RECOVERY_SUGGESTIONS,
  ERROR_LIMITS,
  ERROR_PATTERNS,
} from "./constants";

export class ErrorHandler {
  private config: ErrorHandlerConfig;
  private sourceMap: SourceMap | null = null;
  private errorHistory: ErrorInfo[] = [];

  constructor(config: Partial<ErrorHandlerConfig> = {}) {
    this.config = {
      enableSourceMapping: true,
      enableRecovery: true,
      maxStackTraceDepth: ERROR_LIMITS.MAX_STACK_TRACE_DEPTH,
      includeCodeSnippets: true,
      enableSuggestions: true,
      logLevel: ErrorSeverity.WARNING,
      ...config,
    };
  }

  /**
   * Set the source map for JavaScript to qiqe location mapping
   */
  setSourceMap(sourceMap: SourceMap): void {
    this.sourceMap = sourceMap;
  }

  /**
   * Capture and process an error with context information
   */
  captureError(error: Error, context: ErrorContext): ErrorInfo {
    const errorId = this.generateErrorId();
    const errorType = this.classifyError(error);
    const severity = ERROR_TYPE_SEVERITY_MAP[errorType];
    const stackTrace = this.getStackTrace(error);
    const qiqeLocation = this.mapToQiqeLocation(stackTrace[0]?.jsLocation);

    const errorInfo: ErrorInfo = {
      id: errorId,
      type: errorType,
      severity,
      message: this.sanitizeMessage(error.message),
      originalError: error,
      location: qiqeLocation,
      stackTrace,
      context,
      suggestions: this.generateSuggestions(errorType, error),
      recoverable: this.isRecoverable(errorType),
      timestamp: new Date(),
    };

    // Add to error history
    this.addToHistory(errorInfo);

    return errorInfo;
  }

  /**
   * Format an error for display to the user
   */
  formatError(errorInfo: ErrorInfo): FormattedError {
    const locationStr = errorInfo.location
      ? `Line ${errorInfo.location.line}, Column ${errorInfo.location.column}`
      : "Unknown location";

    const codeSnippet = this.config.includeCodeSnippets
      ? this.extractCodeSnippet(errorInfo)
      : undefined;

    const stackTraceStr = this.formatStackTrace(errorInfo.stackTrace);

    return {
      title: this.getErrorTitle(errorInfo.type),
      message: errorInfo.message,
      location: locationStr,
      codeSnippet,
      stackTrace: stackTraceStr,
      suggestions: errorInfo.suggestions,
      severity: errorInfo.severity,
      canRecover: errorInfo.recoverable,
    };
  }

  /**
   * Generate stack trace with qiqe context
   */
  getStackTrace(error: Error): StackFrame[] {
    const stack = error.stack || "";
    const lines = stack.split("\n").slice(1); // Remove error message line
    const frames: StackFrame[] = [];

    for (
      let i = 0;
      i < Math.min(lines.length, this.config.maxStackTraceDepth);
      i++
    ) {
      const line = lines[i].trim();
      if (!line) continue;

      const frame = this.parseStackFrame(line);
      if (frame) {
        frames.push(frame);
      }
    }

    return frames;
  }

  /**
   * Map JavaScript location to qiqe source location
   */
  mapToQiqeLocation(jsLocation?: JSLocation): QiqeLocation | undefined {
    if (!jsLocation || !this.config.enableSourceMapping || !this.sourceMap) {
      return undefined;
    }

    // Find the closest mapping in the source map
    const mapping = this.sourceMap.jsToQiqe.find(
      (map) =>
        map.jsLocation.line === jsLocation.line &&
        map.jsLocation.column <= jsLocation.column,
    );

    return mapping?.qiqeLocation;
  }

  /**
   * Attempt to recover from an error
   */
  attemptRecovery(errorInfo: ErrorInfo): RecoveryResult {
    if (!this.config.enableRecovery || !errorInfo.recoverable) {
      return {
        success: false,
        action: RecoveryAction.ABORT,
        message: "Error is not recoverable",
      };
    }

    // Implement recovery strategies based on error type
    switch (errorInfo.type) {
      case ErrorType.SYNTAX_ERROR:
        return this.recoverFromSyntaxError(errorInfo);
      case ErrorType.RUNTIME_ERROR:
        return this.recoverFromRuntimeError(errorInfo);
      case ErrorType.IMPORT_ERROR:
        return this.recoverFromImportError(errorInfo);
      default:
        return {
          success: false,
          action: RecoveryAction.SUGGEST_FIX,
          message: "Manual intervention required",
        };
    }
  }

  /**
   * Get error statistics
   */
  getErrorStats() {
    const totalErrors = this.errorHistory.length;
    const errorsByType: Record<ErrorType, number> = {} as any;
    const errorsBySeverity: Record<ErrorSeverity, number> = {} as any;

    // Initialize counters
    Object.values(ErrorType).forEach((type) => (errorsByType[type] = 0));
    Object.values(ErrorSeverity).forEach(
      (severity) => (errorsBySeverity[severity] = 0),
    );

    // Count errors
    this.errorHistory.forEach((error) => {
      errorsByType[error.type]++;
      errorsBySeverity[error.severity]++;
    });

    return {
      totalErrors,
      errorsByType,
      errorsBySeverity,
      recoverySuccessRate: this.calculateRecoverySuccessRate(),
      averageResolutionTime: this.calculateAverageResolutionTime(),
    };
  }

  // Private helper methods

  private generateErrorId(): string {
    return `err_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  private classifyError(error: Error): ErrorType {
    const message = error.message.toLowerCase();
    const name = error.name.toLowerCase();

    if (ERROR_PATTERNS.SYNTAX_ERROR.test(message) || name.includes("syntax")) {
      return ErrorType.SYNTAX_ERROR;
    }
    if (
      ERROR_PATTERNS.REFERENCE_ERROR.test(message) ||
      name.includes("reference")
    ) {
      return ErrorType.RUNTIME_ERROR;
    }
    if (ERROR_PATTERNS.TYPE_ERROR.test(message) || name.includes("type")) {
      return ErrorType.RUNTIME_ERROR;
    }
    if (ERROR_PATTERNS.RANGE_ERROR.test(message) || name.includes("range")) {
      return ErrorType.MEMORY_ERROR;
    }
    if (ERROR_PATTERNS.TIMEOUT_ERROR.test(message)) {
      return ErrorType.TIMEOUT_ERROR;
    }
    if (ERROR_PATTERNS.MEMORY_ERROR.test(message)) {
      return ErrorType.MEMORY_ERROR;
    }

    return ErrorType.RUNTIME_ERROR; // Default classification
  }

  private sanitizeMessage(message: string): string {
    return message.length > ERROR_LIMITS.MAX_ERROR_MESSAGE_LENGTH
      ? message.substring(0, ERROR_LIMITS.MAX_ERROR_MESSAGE_LENGTH) + "..."
      : message;
  }

  private generateSuggestions(errorType: ErrorType, error: Error): string[] {
    if (!this.config.enableSuggestions) {
      return [];
    }

    const baseSuggestions = RECOVERY_SUGGESTIONS[errorType] || [];
    const contextualSuggestions = this.getContextualSuggestions(error);

    return [...baseSuggestions, ...contextualSuggestions].slice(
      0,
      ERROR_LIMITS.MAX_SUGGESTIONS,
    );
  }

  private getContextualSuggestions(error: Error): string[] {
    const suggestions: string[] = [];
    const message = error.message.toLowerCase();

    if (message.includes("undefined")) {
      suggestions.push(
        "Check if all variables are properly declared and initialized",
      );
    }
    if (message.includes("function")) {
      suggestions.push(
        "Verify function names and ensure they are defined before use",
      );
    }
    if (message.includes("import")) {
      suggestions.push("Check import statements and file paths");
    }

    return suggestions;
  }

  private isRecoverable(errorType: ErrorType): boolean {
    return ![ErrorType.SECURITY_ERROR, ErrorType.COMPILATION_ERROR].includes(
      errorType,
    );
  }

  private parseStackFrame(line: string): StackFrame | null {
    // Parse JavaScript stack frame format
    const match = line.match(/at\s+(.+?)\s+\((.+?):(\d+):(\d+)\)/);
    if (!match) {
      return null;
    }

    const [, functionName, source, lineStr, columnStr] = match;
    const jsLocation: JSLocation = {
      line: parseInt(lineStr, 10),
      column: parseInt(columnStr, 10),
      source,
    };

    const qiqeLocation = this.mapToQiqeLocation(jsLocation);

    return {
      functionName: functionName || "anonymous",
      qiqeLocation,
      jsLocation,
      source,
      isUserCode: !source.includes("node_modules") && !source.includes("eval"),
    };
  }

  private extractCodeSnippet(errorInfo: ErrorInfo): string | undefined {
    if (!errorInfo.location || !errorInfo.context.codeSnippet) {
      return undefined;
    }

    const lines = errorInfo.context.codeSnippet.split("\n");
    const errorLine = errorInfo.location.line - 1;
    const start = Math.max(0, errorLine - 2);
    const end = Math.min(lines.length, errorLine + 3);

    return lines
      .slice(start, end)
      .map((line, index) => {
        const lineNum = start + index + 1;
        const marker = lineNum === errorLine + 1 ? ">>> " : "    ";
        return `${marker}${lineNum}: ${line}`;
      })
      .join("\n");
  }

  private formatStackTrace(stackTrace: StackFrame[]): string {
    return stackTrace
      .filter((frame) => frame.isUserCode)
      .map((frame) => {
        const location = frame.qiqeLocation
          ? `${frame.qiqeLocation.line}:${frame.qiqeLocation.column}`
          : `${frame.jsLocation.line}:${frame.jsLocation.column}`;
        return `  at ${frame.functionName} (${location})`;
      })
      .join("\n");
  }

  private getErrorTitle(errorType: ErrorType): string {
    switch (errorType) {
      case ErrorType.SYNTAX_ERROR:
        return "Syntax Error";
      case ErrorType.COMPILATION_ERROR:
        return "Compilation Error";
      case ErrorType.RUNTIME_ERROR:
        return "Runtime Error";
      case ErrorType.IMPORT_ERROR:
        return "Import Error";
      case ErrorType.RESOURCE_ERROR:
        return "Resource Error";
      case ErrorType.SECURITY_ERROR:
        return "Security Error";
      case ErrorType.TIMEOUT_ERROR:
        return "Timeout Error";
      case ErrorType.MEMORY_ERROR:
        return "Memory Error";
      default:
        return "Unknown Error";
    }
  }

  private recoverFromSyntaxError(errorInfo: ErrorInfo): RecoveryResult {
    // Basic syntax error recovery strategies
    return {
      success: false,
      action: RecoveryAction.SUGGEST_FIX,
      message: "Please fix the syntax error and try again",
    };
  }

  private recoverFromRuntimeError(errorInfo: ErrorInfo): RecoveryResult {
    // Runtime error recovery strategies
    return {
      success: false,
      action: RecoveryAction.SUGGEST_FIX,
      message: "Please check the runtime error and fix the issue",
    };
  }

  private recoverFromImportError(errorInfo: ErrorInfo): RecoveryResult {
    // Import error recovery strategies
    return {
      success: false,
      action: RecoveryAction.SUGGEST_FIX,
      message: "Please check the import path and try again",
    };
  }

  private addToHistory(errorInfo: ErrorInfo): void {
    this.errorHistory.push(errorInfo);

    // Keep only the most recent errors
    if (this.errorHistory.length > ERROR_LIMITS.MAX_ERROR_HISTORY) {
      this.errorHistory = this.errorHistory.slice(
        -ERROR_LIMITS.MAX_ERROR_HISTORY,
      );
    }
  }

  private calculateRecoverySuccessRate(): number {
    // Placeholder implementation
    return 0;
  }

  private calculateAverageResolutionTime(): number {
    // Placeholder implementation
    return 0;
  }
}

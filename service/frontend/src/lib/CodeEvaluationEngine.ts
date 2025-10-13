/**
 * Enhanced Code Evaluation Engine with improved JavaScript interoperability,
 * error handling, and sandbox integration
 */

import { ExecutionSandbox } from "./execution-sandbox/ExecutionSandbox";
import { ErrorHandler } from "./error-handling/ErrorHandler";
import { ImportManager } from "./import-system/ImportManager";
import {
  ensureStandardLibraryInitialized,
  getStandardLibraryLoader,
} from "./standard-library/StandardLibraryLoader";
import {
  SandboxOptions,
  SandboxLevel,
  ExecutionResult as SandboxExecutionResult,
  ResourceUsage,
} from "./execution-sandbox/types";
import { ErrorInfo } from "./error-handling/types";
import { ImportSpec, ResolvedImport } from "./import-system/types";

// Enhanced evaluation options
export interface EvaluationOptions {
  imports: ImportSpec[];
  timeout: number;
  memoryLimit: number;
  debugMode: boolean;
  sandboxLevel: SandboxLevel;
  enableAsyncOps: boolean;
  validateJavaScript: boolean;
  asyncTimeout?: number; // Separate timeout for async operations
  maxConcurrentAsync?: number; // Maximum concurrent async operations
}

// Enhanced evaluation result
export interface EvaluationResult {
  output: string;
  errors: ErrorInfo[];
  warnings: string[];
  executionTime: number;
  memoryUsage: ResourceUsage;
  success: boolean;
  debugInfo?: DebugInfo;
  imports?: ResolvedImport[];
  asyncOperations?: AsyncOperationInfo[];
}

// Async operation tracking
export interface AsyncOperationInfo {
  id: string;
  type: "promise" | "timeout" | "interval" | "custom";
  status: "pending" | "resolved" | "rejected" | "cancelled";
  startTime: number;
  endTime?: number;
  result?: any;
  error?: Error;
}

// Debug information structure
export interface DebugInfo {
  executionSteps: ExecutionStep[];
  variableStates: VariableState[];
  functionCalls: FunctionCall[];
  performanceMetrics: PerformanceMetrics;
}

export interface ExecutionStep {
  stepId: number;
  sourceLocation: { line: number; column: number };
  timestamp: number;
  memoryUsage: number;
}

export interface VariableState {
  name: string;
  value: any;
  type: string;
  scope: string;
}

export interface FunctionCall {
  functionName: string;
  arguments: any[];
  returnValue: any;
  executionTime: number;
}

export interface PerformanceMetrics {
  compilationTime: number;
  executionTime: number;
  memoryPeak: number;
  memoryAverage: number;
}

// Type conversion utilities
export interface TypeConverter {
  qiqeToJavaScript(value: any, targetType?: string): any;
  javaScriptToQiqe(value: any, targetType?: string): any;
  inferType(value: any): string;
  validateConversion(value: any, fromType: string, toType: string): boolean;
}

export class CodeEvaluationEngine {
  private sandbox: ExecutionSandbox;
  private errorHandler: ErrorHandler;
  private importManager: ImportManager;
  private debugMode: boolean = false;
  private asyncOperations: Map<string, AsyncOperationInfo> = new Map();
  private asyncOperationCounter: number = 0;
  private standardLibraryInitialized: boolean = false;

  constructor(sandboxLevel: SandboxLevel = SandboxLevel.MODERATE) {
    this.sandbox = new ExecutionSandbox(sandboxLevel);
    this.errorHandler = new ErrorHandler({
      enableSourceMapping: true,
      enableRecovery: true,
      includeCodeSnippets: true,
      enableSuggestions: true,
    });
    this.importManager = new ImportManager();

    // Initialize standard library in the background
    this.initializeStandardLibrary();
  }

  /**
   * Initialize the standard library automatically
   */
  private async initializeStandardLibrary(): Promise<void> {
    try {
      await ensureStandardLibraryInitialized({
        autoLoadStandardLibrary: true,
        preloadLibraries: ["std"],
        cacheEnabled: true,
        lazyLoadThreshold: 1024 * 50, // 50KB
        standardLibraryPath: "/qiqe/library/std.qq",
      });
      this.standardLibraryInitialized = true;
    } catch (error) {
      console.warn(
        "Failed to initialize standard library in CodeEvaluationEngine:",
        error,
      );
      // Don't throw - allow the engine to work without standard library if needed
    }
  }

  /**
   * Ensure standard library is available before evaluation
   */
  private async ensureStandardLibraryAvailable(): Promise<void> {
    if (!this.standardLibraryInitialized) {
      await this.initializeStandardLibrary();
    }
  }

  /**
   * Evaluate qiqe code with enhanced capabilities
   */
  async evaluate(
    code: string,
    options: Partial<EvaluationOptions> = {},
  ): Promise<EvaluationResult> {
    const startTime = Date.now();

    // Set default options
    const evaluationOptions: EvaluationOptions = {
      imports: [],
      timeout: 30000, // 30 seconds default
      memoryLimit: 128, // 128MB default
      debugMode: false,
      sandboxLevel: SandboxLevel.MODERATE,
      enableAsyncOps: false,
      validateJavaScript: true,
      asyncTimeout: 10000, // 10 seconds for async operations
      maxConcurrentAsync: 5, // Maximum 5 concurrent async operations
      ...options,
    };

    this.debugMode = evaluationOptions.debugMode;

    try {
      // Step 0: Ensure standard library is available
      await this.ensureStandardLibraryAvailable();

      // Step 1: Prepare imports with automatic standard library inclusion
      const importsWithStdLib = await this.prepareImportsWithStandardLibrary(
        evaluationOptions.imports,
      );

      // Step 2: Resolve imports
      const resolvedImports = await this.resolveImports(importsWithStdLib);

      // Step 3: Validate syntax
      const syntaxValidation = this.validateSyntax(code);
      if (!syntaxValidation.isValid) {
        return this.createErrorResult(syntaxValidation.errors, startTime);
      }

      // Step 4: Compile qiqe code to JavaScript
      const compilationResult = await this.compileCode(code, resolvedImports);
      if (!compilationResult.success) {
        return this.createErrorResult(compilationResult.errors, startTime);
      }

      // Step 5: Validate JavaScript code if enabled
      if (evaluationOptions.validateJavaScript) {
        const jsValidation = this.validateJavaScriptCode(
          compilationResult.jsCode,
          evaluationOptions,
        );
        if (!jsValidation.isValid) {
          return this.createErrorResult(jsValidation.errors, startTime);
        }
      }

      // Step 6: Execute in sandbox
      const executionResult = await this.executeInSandbox(
        compilationResult.jsCode,
        evaluationOptions,
        resolvedImports,
      );

      const totalTime = Date.now() - startTime;

      return {
        output: executionResult.output,
        errors: executionResult.errors,
        warnings: executionResult.warnings,
        executionTime: totalTime,
        memoryUsage: executionResult.resourceUsage,
        success: executionResult.success,
        debugInfo: this.debugMode
          ? this.generateDebugInfo(executionResult)
          : undefined,
        imports: resolvedImports,
        asyncOperations: evaluationOptions.enableAsyncOps
          ? this.getAsyncOperations()
          : undefined,
      };
    } catch (error) {
      const errorObj =
        error instanceof Error ? error : new Error(String(error));
      const errorInfo = this.errorHandler.captureError(errorObj, {
        executionId: `eval_${Date.now()}`,
        timestamp: new Date(),
        codeSnippet: code.substring(0, 200),
        imports: evaluationOptions.imports.map((imp) => imp.path),
        executionPhase: "execution",
      });

      return this.createErrorResult([errorInfo], startTime);
    }
  }

  /**
   * Prepare imports with automatic standard library inclusion
   */
  private async prepareImportsWithStandardLibrary(
    userImports: ImportSpec[],
  ): Promise<ImportSpec[]> {
    const imports: ImportSpec[] = [...userImports];

    // Check if standard library is already explicitly imported
    const hasStdLibImport = userImports.some(
      (imp) =>
        imp.path === "std" ||
        imp.path === "std.qq" ||
        imp.path.includes("std.qq"),
    );

    // Automatically include standard library if not already imported
    if (!hasStdLibImport) {
      const stdLibLoader = getStandardLibraryLoader();
      const stdLib = stdLibLoader.getStandardLibrary();

      if (stdLib) {
        imports.unshift({
          path: "std.qq",
          type: "standard",
          selective: undefined, // Import all functions from std
          alias: undefined,
        });
      }
    }

    return imports;
  }

  /**
   * Validate qiqe syntax
   */
  validateSyntax(code: string): { isValid: boolean; errors: ErrorInfo[] } {
    const errors: ErrorInfo[] = [];

    try {
      // Basic syntax validation - in a real implementation, this would use the qiqe parser
      // For now, we'll do basic checks

      // Check for balanced parentheses, brackets, and braces
      const balanceCheck = this.checkBalancedDelimiters(code);
      if (!balanceCheck.isValid) {
        const errorInfo = this.errorHandler.captureError(
          new Error(balanceCheck.message),
          {
            executionId: `syntax_${Date.now()}`,
            timestamp: new Date(),
            codeSnippet: code.substring(0, 200),
            imports: [],
            executionPhase: "parsing",
          },
        );
        errors.push(errorInfo);
      }

      // Check for basic qiqe syntax patterns
      const patternCheck = this.checkQiqePatterns(code);
      if (!patternCheck.isValid) {
        const errorInfo = this.errorHandler.captureError(
          new Error(patternCheck.message),
          {
            executionId: `syntax_${Date.now()}`,
            timestamp: new Date(),
            codeSnippet: code.substring(0, 200),
            imports: [],
            executionPhase: "parsing",
          },
        );
        errors.push(errorInfo);
      }

      return { isValid: errors.length === 0, errors };
    } catch (error) {
      const errorObj =
        error instanceof Error ? error : new Error(String(error));
      const errorInfo = this.errorHandler.captureError(errorObj, {
        executionId: `syntax_${Date.now()}`,
        timestamp: new Date(),
        codeSnippet: code.substring(0, 200),
        imports: [],
        executionPhase: "parsing",
      });

      return { isValid: false, errors: [errorInfo] };
    }
  }

  /**
   * Set debug mode
   */
  setDebugMode(enabled: boolean): void {
    this.debugMode = enabled;
  }

  /**
   * Get execution context information
   */
  getExecutionContext() {
    return {
      activeContexts: this.sandbox.getActiveContexts(),
      errorStats: this.errorHandler.getErrorStats(),
      // Note: ImportManager doesn't expose cache stats directly
      // This would need to be implemented if needed
      importCache: null,
    };
  }

  /**
   * Get the error handler instance for external access
   */
  getErrorHandler() {
    return this.errorHandler;
  }

  /**
   * Clean up resources
   */
  cleanup(): void {
    this.sandbox.cleanup();
    this.cancelAllAsyncOperations();
  }

  /**
   * Create a Promise wrapper for qiqe async operations
   */
  createAsyncOperation<T>(
    operation: () => Promise<T>,
    type: AsyncOperationInfo["type"] = "promise",
    timeout?: number,
  ): Promise<T> {
    const operationId = `async_${++this.asyncOperationCounter}`;
    const startTime = Date.now();

    const asyncInfo: AsyncOperationInfo = {
      id: operationId,
      type,
      status: "pending",
      startTime,
    };

    this.asyncOperations.set(operationId, asyncInfo);

    const promise = new Promise<T>((resolve, reject) => {
      const timeoutId = timeout
        ? setTimeout(() => {
            asyncInfo.status = "cancelled";
            asyncInfo.endTime = Date.now();
            asyncInfo.error = new Error(
              `Async operation timeout after ${timeout}ms`,
            );
            this.asyncOperations.set(operationId, asyncInfo);
            reject(asyncInfo.error);
          }, timeout)
        : null;

      operation()
        .then((result) => {
          if (timeoutId) clearTimeout(timeoutId);
          asyncInfo.status = "resolved";
          asyncInfo.endTime = Date.now();
          asyncInfo.result = result;
          this.asyncOperations.set(operationId, asyncInfo);
          resolve(result);
        })
        .catch((error) => {
          if (timeoutId) clearTimeout(timeoutId);
          asyncInfo.status = "rejected";
          asyncInfo.endTime = Date.now();
          asyncInfo.error = error;
          this.asyncOperations.set(operationId, asyncInfo);
          reject(error);
        });
    });

    return promise;
  }

  /**
   * Cancel all pending async operations
   */
  cancelAllAsyncOperations(): void {
    this.asyncOperations.forEach((operation, id) => {
      if (operation.status === "pending") {
        operation.status = "cancelled";
        operation.endTime = Date.now();
        operation.error = new Error("Operation cancelled during cleanup");
        this.asyncOperations.set(id, operation);
      }
    });
  }

  /**
   * Get information about async operations
   */
  getAsyncOperations(): AsyncOperationInfo[] {
    return Array.from(this.asyncOperations.values());
  }

  // Private helper methods

  private async resolveImports(
    imports: ImportSpec[],
  ): Promise<ResolvedImport[]> {
    if (imports.length === 0) {
      return [];
    }

    try {
      return await this.importManager.resolveImports(imports);
    } catch (error) {
      // Import resolution errors will be handled by the import manager
      return [];
    }
  }

  private async compileCode(
    code: string,
    imports: ResolvedImport[],
  ): Promise<{
    success: boolean;
    jsCode: string;
    errors: ErrorInfo[];
  }> {
    try {
      // Generate import statements
      const importStatements = imports
        .map((imp) => imp.compiledCode)
        .join("\n");

      // Basic qiqe to JavaScript compilation
      // In a real implementation, this would use the actual qiqe compiler
      const jsCode = this.basicQiqeToJsCompilation(
        code,
        importStatements,
        imports,
      );
      return {
        success: true,
        jsCode,
        errors: [],
      };
    } catch (error) {
      const errorObj =
        error instanceof Error ? error : new Error(String(error));
      const errorInfo = this.errorHandler.captureError(errorObj, {
        executionId: `compile_${Date.now()}`,
        timestamp: new Date(),
        codeSnippet: code.substring(0, 200),
        imports: imports.map((imp) => imp.spec.path),
        executionPhase: "compilation",
      });

      return {
        success: false,
        jsCode: "",
        errors: [errorInfo],
      };
    }
  }

  private validateJavaScriptCode(
    jsCode: string,
    options: EvaluationOptions,
  ): { isValid: boolean; errors: ErrorInfo[] } {
    const errors: ErrorInfo[] = [];

    try {
      // Basic JavaScript syntax validation
      new Function(jsCode);

      // Check for potentially dangerous patterns
      const dangerousPatterns = [
        {
          pattern: /eval\s*\(/,
          message: "Direct eval() usage is not allowed for security reasons",
        },
        {
          pattern: /XMLHttpRequest/,
          message: "XMLHttpRequest is not allowed in sandbox",
        },
        {
          pattern: /fetch\s*\(/,
          message: "fetch API is restricted in sandbox",
        },
        {
          pattern: /import\s*\(/,
          message: "Dynamic imports are not supported - use static imports",
        },
        {
          pattern: /require\s*\(/,
          message: "CommonJS require is not supported - use qiqe imports",
        },
      ];

      // Add Function constructor restriction only in strict mode or when specifically disabled
      // Note: The qiqe compiler may legitimately generate Function constructor calls
      if (options.sandboxLevel === SandboxLevel.STRICT) {
        dangerousPatterns.push({
          pattern: /Function\s*\(/,
          message: "Dynamic Function constructor is not allowed in strict mode",
        });
      }

      // Add setTimeout/setInterval restrictions only if async operations are disabled
      if (!options.enableAsyncOps) {
        dangerousPatterns.push(
          {
            pattern: /setTimeout\s*\(/,
            message: "setTimeout is restricted - use async operations instead",
          },
          {
            pattern: /setInterval\s*\(/,
            message: "setInterval is restricted - use async operations instead",
          },
        );
      }

      for (const { pattern, message } of dangerousPatterns) {
        if (pattern.test(jsCode)) {
          const errorInfo = this.errorHandler.captureError(new Error(message), {
            executionId: `js_validation_${Date.now()}`,
            timestamp: new Date(),
            codeSnippet: jsCode.substring(0, 200),
            imports: [],
            executionPhase: "compilation",
          });
          errors.push(errorInfo);
        }
      }

      // Check for excessive code complexity
      if (jsCode.length > 50000) {
        const errorInfo = this.errorHandler.captureError(
          new Error("Generated JavaScript code is too large (>50KB)"),
          {
            executionId: `js_validation_${Date.now()}`,
            timestamp: new Date(),
            codeSnippet: jsCode.substring(0, 200),
            imports: [],
            executionPhase: "compilation",
          },
        );
        errors.push(errorInfo);
      }

      return { isValid: errors.length === 0, errors };
    } catch (error) {
      const errorObj =
        error instanceof Error ? error : new Error(String(error));
      const errorInfo = this.errorHandler.captureError(errorObj, {
        executionId: `js_validation_${Date.now()}`,
        timestamp: new Date(),
        codeSnippet: jsCode.substring(0, 200),
        imports: [],
        executionPhase: "compilation",
      });

      return { isValid: false, errors: [errorInfo] };
    }
  }

  private async executeInSandbox(
    jsCode: string,
    options: EvaluationOptions,
    imports: ResolvedImport[],
  ): Promise<SandboxExecutionResult> {
    const allowedGlobals = [
      "console",
      "Math",
      "Date",
      "JSON",
      "parseInt",
      "parseFloat",
    ];
    const restrictedAPIs = ["eval"];

    // Only restrict Function constructor in strict mode
    if (options.sandboxLevel === SandboxLevel.STRICT) {
      restrictedAPIs.push("Function");
    }

    // Add async-related globals if async operations are enabled
    if (options.enableAsyncOps) {
      allowedGlobals.push("Promise", "setTimeout", "clearTimeout");
    } else {
      restrictedAPIs.push("setTimeout", "setInterval", "Promise");
    }

    const sandboxOptions: SandboxOptions = {
      memoryLimit: options.memoryLimit,
      timeoutMs: options.timeout,
      allowedGlobals,
      restrictedAPIs,
      enableAsyncOps: options.enableAsyncOps,
      sandboxLevel: options.sandboxLevel,
      enableResourceMonitoring: true,
    };

    const context = this.sandbox.createContext(sandboxOptions);

    try {
      if (options.enableAsyncOps) {
        return await this.executeAsyncInSandbox(jsCode, context, options);
      } else {
        return await this.sandbox.execute(jsCode, context);
      }
    } finally {
      this.sandbox.terminate(context.id);
    }
  }

  /**
   * Execute async code in sandbox with proper timeout handling
   */
  private async executeAsyncInSandbox(
    jsCode: string,
    context: any,
    options: EvaluationOptions,
  ): Promise<SandboxExecutionResult> {
    const asyncTimeout = options.asyncTimeout || options.timeout;
    const maxConcurrentAsync = options.maxConcurrentAsync || 5;

    // Create enhanced async execution environment
    const asyncExecutionCode = this.wrapCodeForAsyncExecution(jsCode, {
      timeout: asyncTimeout,
      maxConcurrent: maxConcurrentAsync,
      trackOperations: true,
    });

    // Create a timeout promise for the entire async execution
    const timeoutPromise = new Promise<never>((_, reject) => {
      setTimeout(() => {
        this.cancelAllAsyncOperations();
        reject(new Error(`Async execution timeout after ${asyncTimeout}ms`));
      }, asyncTimeout);
    });

    try {
      // Execute with async timeout and operation tracking
      const executionPromise = this.executeAsyncCodeWithTracking(
        asyncExecutionCode,
        context,
        options,
      );
      const result = await Promise.race([executionPromise, timeoutPromise]);

      // Process async operation results
      if (result.success) {
        const asyncOps = this.extractAsyncOperationsFromResult(result);
        this.trackAsyncOperations(asyncOps);

        // Wait for any pending async operations to complete or timeout
        await this.waitForAsyncOperations(asyncTimeout);
      }

      return result;
    } catch (error) {
      // Handle different types of async errors
      if (error instanceof Error) {
        if (error.message.includes("timeout")) {
          return this.createAsyncTimeoutResult(
            error,
            context.id,
            jsCode,
            asyncTimeout,
          );
        } else if (error.message.includes("concurrent")) {
          return this.createConcurrencyLimitResult(error, context.id, jsCode);
        }
      }

      throw error;
    }
  }

  /**
   * Execute async code with operation tracking
   */
  private async executeAsyncCodeWithTracking(
    code: string,
    context: any,
    options: EvaluationOptions,
  ): Promise<SandboxExecutionResult> {
    // Inject async operation tracking into the execution context
    const enhancedContext = {
      ...context,
      globals: {
        ...context.globals,
        __asyncTracker: {
          operations: this.asyncOperations,
          counter: this.asyncOperationCounter,
          maxConcurrent: options.maxConcurrentAsync || 5,
          timeout: options.asyncTimeout || options.timeout,
        },
      },
    };

    return await this.sandbox.execute(code, enhancedContext);
  }

  /**
   * Wrap code for async execution with proper error handling and tracking
   */
  private wrapCodeForAsyncExecution(
    code: string,
    options: {
      timeout: number;
      maxConcurrent: number;
      trackOperations: boolean;
    },
  ): string {
    return `
      // Async operation tracking and management
      const asyncTracker = __asyncTracker || {
        operations: new Map(),
        counter: 0,
        maxConcurrent: ${options.maxConcurrent},
        timeout: ${options.timeout}
      };

      // Enhanced Promise handling with timeout and tracking
      const createTrackedPromise = (executor, type = 'promise', operationTimeout = ${options.timeout}) => {
        const operationId = \`async_\${++asyncTracker.counter}\`;
        const startTime = Date.now();

        // Check concurrent operation limit
        const activeOps = Array.from(asyncTracker.operations.values())
          .filter(op => op.status === 'pending').length;

        if (activeOps >= asyncTracker.maxConcurrent) {
          throw new Error(\`Maximum concurrent async operations (\${asyncTracker.maxConcurrent}) exceeded\`);
        }

        const operationInfo = {
          id: operationId,
          type,
          status: 'pending',
          startTime,
          timeout: operationTimeout
        };

        asyncTracker.operations.set(operationId, operationInfo);

        const promise = new Promise((resolve, reject) => {
          // Set up operation timeout
          const timeoutId = setTimeout(() => {
            operationInfo.status = 'cancelled';
            operationInfo.endTime = Date.now();
            operationInfo.error = new Error(\`Async operation timeout after \${operationTimeout}ms\`);
            asyncTracker.operations.set(operationId, operationInfo);
            reject(operationInfo.error);
          }, operationTimeout);

          // Execute the operation
          try {
            const result = executor((value) => {
              clearTimeout(timeoutId);
              operationInfo.status = 'resolved';
              operationInfo.endTime = Date.now();
              operationInfo.result = value;
              asyncTracker.operations.set(operationId, operationInfo);
              resolve(value);
            }, (error) => {
              clearTimeout(timeoutId);
              operationInfo.status = 'rejected';
              operationInfo.endTime = Date.now();
              operationInfo.error = error;
              asyncTracker.operations.set(operationId, operationInfo);
              reject(error);
            });

            // Handle immediate resolution for non-promise executors
            if (result !== undefined && typeof result !== 'object') {
              clearTimeout(timeoutId);
              operationInfo.status = 'resolved';
              operationInfo.endTime = Date.now();
              operationInfo.result = result;
              asyncTracker.operations.set(operationId, operationInfo);
              resolve(result);
            }
          } catch (error) {
            clearTimeout(timeoutId);
            operationInfo.status = 'rejected';
            operationInfo.endTime = Date.now();
            operationInfo.error = error;
            asyncTracker.operations.set(operationId, operationInfo);
            reject(error);
          }
        });

        return promise;
      };

      // Enhanced async/await support for qiqe
      const awaitAsync = async (promise, timeoutMs = ${options.timeout}) => {
        if (!promise || typeof promise.then !== 'function') {
          return promise; // Return non-promise values immediately
        }

        // Create timeout wrapper for the await operation
        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => {
            reject(new Error(\`Await timeout after \${timeoutMs}ms\`));
          }, timeoutMs);
        });

        try {
          return await Promise.race([promise, timeoutPromise]);
        } catch (error) {
          // Track failed await operations
          const operationId = \`await_\${++asyncTracker.counter}\`;
          asyncTracker.operations.set(operationId, {
            id: operationId,
            type: 'await',
            status: 'rejected',
            startTime: Date.now(),
            endTime: Date.now(),
            error
          });
          throw error;
        }
      };

      // Promise utilities for qiqe
      const promiseAll = async (promises, timeoutMs = ${options.timeout}) => {
        if (!Array.isArray(promises)) {
          throw new Error('promiseAll expects an array of promises');
        }

        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => {
            reject(new Error(\`Promise.all timeout after \${timeoutMs}ms\`));
          }, timeoutMs);
        });

        try {
          return await Promise.race([Promise.all(promises), timeoutPromise]);
        } catch (error) {
          // Cancel any pending operations
          promises.forEach((promise, index) => {
            if (promise && typeof promise.catch === 'function') {
              promise.catch(() => {}); // Prevent unhandled rejection warnings
            }
          });
          throw error;
        }
      };

      const promiseRace = async (promises, timeoutMs = ${options.timeout}) => {
        if (!Array.isArray(promises)) {
          throw new Error('promiseRace expects an array of promises');
        }

        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => {
            reject(new Error(\`Promise.race timeout after \${timeoutMs}ms\`));
          }, timeoutMs);
        });

        return await Promise.race([...promises, timeoutPromise]);
      };

      // Delay utility with timeout protection
      const delay = (ms, value = undefined) => {
        if (ms > ${options.timeout}) {
          throw new Error(\`Delay duration (\${ms}ms) exceeds maximum timeout (\${${options.timeout}}ms)\`);
        }

        return createTrackedPromise((resolve) => {
          setTimeout(() => resolve(value), ms);
        }, 'delay', Math.min(ms + 1000, ${options.timeout})); // Add 1s buffer or use max timeout
      };

      // Async function wrapper for qiqe functions
      const asyncFunction = (fn) => {
        return async (...args) => {
          try {
            const result = await fn(...args);
            return result;
          } catch (error) {
            // Track async function errors
            const operationId = \`async_fn_\${++asyncTracker.counter}\`;
            asyncTracker.operations.set(operationId, {
              id: operationId,
              type: 'async_function',
              status: 'rejected',
              startTime: Date.now(),
              endTime: Date.now(),
              error
            });
            throw error;
          }
        };
      };

      // Override global Promise constructor with tracking
      const TrackedPromise = (executor) => {
        return createTrackedPromise(executor, 'promise');
      };

      // Copy static methods from Promise
      TrackedPromise.resolve = (value) => createTrackedPromise((resolve) => resolve(value), 'resolve');
      TrackedPromise.reject = (reason) => createTrackedPromise((_, reject) => reject(reason), 'reject');
      TrackedPromise.all = promiseAll;
      TrackedPromise.race = promiseRace;

      // Replace global Promise if in sandbox
      if (typeof Promise !== 'undefined') {
        Promise = TrackedPromise;
      }

      // Async execution wrapper
      const executeAsync = async () => {
        try {
          // User code execution
          ${code}

          // Return async operation tracking data
          return {
            asyncOperations: Array.from(asyncTracker.operations.values()),
            completedOperations: Array.from(asyncTracker.operations.values())
              .filter(op => op.status === 'resolved' || op.status === 'rejected').length,
            pendingOperations: Array.from(asyncTracker.operations.values())
              .filter(op => op.status === 'pending').length
          };
        } catch (error) {
          // Track execution errors
          const operationId = \`execution_\${++asyncTracker.counter}\`;
          asyncTracker.operations.set(operationId, {
            id: operationId,
            type: 'execution',
            status: 'rejected',
            startTime: Date.now(),
            endTime: Date.now(),
            error
          });
          throw error;
        }
      };

      // Execute and return result
      executeAsync();
    `;
  }

  /**
   * Wait for async operations to complete or timeout
   */
  private async waitForAsyncOperations(timeoutMs: number): Promise<void> {
    const startTime = Date.now();
    const checkInterval = 100; // Check every 100ms

    return new Promise((resolve, reject) => {
      const checkOperations = () => {
        const pendingOps = Array.from(this.asyncOperations.values()).filter(
          (op) => op.status === "pending",
        );

        if (pendingOps.length === 0) {
          resolve();
          return;
        }

        if (Date.now() - startTime > timeoutMs) {
          // Cancel pending operations
          pendingOps.forEach((op) => {
            op.status = "cancelled";
            op.endTime = Date.now();
            op.error = new Error("Operation cancelled due to timeout");
            this.asyncOperations.set(op.id, op);
          });

          reject(
            new Error(
              `Timeout waiting for ${pendingOps.length} async operations to complete`,
            ),
          );
          return;
        }

        setTimeout(checkOperations, checkInterval);
      };

      checkOperations();
    });
  }

  /**
   * Create timeout result for async operations
   */
  private createAsyncTimeoutResult(
    error: Error,
    contextId: string,
    code: string,
    timeout: number,
  ): SandboxExecutionResult {
    const errorInfo = this.errorHandler.captureError(error, {
      executionId: contextId,
      timestamp: new Date(),
      codeSnippet: code.substring(0, 200),
      imports: [],
      executionPhase: "execution",
    });

    return {
      output: "",
      errors: [errorInfo],
      warnings: ["Async operation timed out"],
      resourceUsage: {
        memoryUsageMB: 0,
        executionTimeMs: timeout,
        callStackDepth: 0,
        outputLength: 0,
        lastUpdated: new Date(),
      },
      executionTime: timeout,
      success: false,
      terminated: true,
    };
  }

  /**
   * Create concurrency limit result
   */
  private createConcurrencyLimitResult(
    error: Error,
    contextId: string,
    code: string,
  ): SandboxExecutionResult {
    const errorInfo = this.errorHandler.captureError(error, {
      executionId: contextId,
      timestamp: new Date(),
      codeSnippet: code.substring(0, 200),
      imports: [],
      executionPhase: "execution",
    });

    return {
      output: "",
      errors: [errorInfo],
      warnings: ["Too many concurrent async operations"],
      resourceUsage: {
        memoryUsageMB: 0,
        executionTimeMs: 0,
        callStackDepth: 0,
        outputLength: 0,
        lastUpdated: new Date(),
      },
      executionTime: 0,
      success: false,
      terminated: false,
    };
  }

  /**
   * Extract async operation information from execution result
   */
  private extractAsyncOperationsFromResult(result: any): AsyncOperationInfo[] {
    const operations: AsyncOperationInfo[] = [];

    try {
      // Check if result contains async operation data
      if (result && typeof result === "object") {
        // Look for async operations in the result output
        if (result.output && typeof result.output === "string") {
          try {
            // Try to parse async status from output if it was logged
            const asyncStatusMatch = result.output.match(
              /asyncStatus:\s*({.*?})/,
            );
            if (asyncStatusMatch) {
              const asyncStatus = JSON.parse(asyncStatusMatch[1]);
              if (
                asyncStatus.operations &&
                Array.isArray(asyncStatus.operations)
              ) {
                operations.push(...asyncStatus.operations);
              }
            }
          } catch (parseError) {
            // Ignore parsing errors
          }
        }

        // Look for async operations in result data
        if (result.asyncOperations && Array.isArray(result.asyncOperations)) {
          operations.push(...result.asyncOperations);
        }

        // Look for async status in result
        if (result.asyncStatus && result.asyncStatus.operations) {
          operations.push(...result.asyncStatus.operations);
        }

        // Check for promise-like objects in the result
        if (result.result && typeof result.result === "object") {
          if (result.result.then && typeof result.result.then === "function") {
            // This is a promise that wasn't awaited
            const operationId = `unresolved_promise_${++this.asyncOperationCounter}`;
            operations.push({
              id: operationId,
              type: "promise",
              status: "pending",
              startTime: Date.now(),
            });
          }
        }
      }

      // Validate and normalize operations
      return operations
        .map((op) => this.normalizeAsyncOperation(op))
        .filter((op) => op !== null) as AsyncOperationInfo[];
    } catch (error) {
      console.warn("Error extracting async operations from result:", error);
      return [];
    }
  }

  /**
   * Track async operations for monitoring
   */
  private trackAsyncOperations(operations: AsyncOperationInfo[]): void {
    operations.forEach((op) => {
      this.asyncOperations.set(op.id, op);
    });
  }

  /**
   * Normalize async operation data to ensure consistency
   */
  private normalizeAsyncOperation(operation: any): AsyncOperationInfo | null {
    try {
      if (!operation || typeof operation !== "object") {
        return null;
      }

      // Ensure required fields exist
      const id = operation.id || `normalized_${++this.asyncOperationCounter}`;
      const type = operation.type || "custom";
      const status = operation.status || "pending";
      const startTime = operation.startTime || Date.now();

      // Validate status
      const validStatuses = ["pending", "resolved", "rejected", "cancelled"];
      if (!validStatuses.includes(status)) {
        return null;
      }

      // Validate type
      const validTypes = [
        "promise",
        "timeout",
        "interval",
        "custom",
        "await",
        "async_function",
        "execution",
        "delay",
        "resolve",
        "reject",
      ];
      const normalizedType = validTypes.includes(type) ? type : "custom";

      const normalizedOperation: AsyncOperationInfo = {
        id,
        type: normalizedType as AsyncOperationInfo["type"],
        status: status as AsyncOperationInfo["status"],
        startTime,
      };

      // Add optional fields if they exist
      if (operation.endTime) {
        normalizedOperation.endTime = operation.endTime;
      }

      if (operation.result !== undefined) {
        normalizedOperation.result = operation.result;
      }

      if (operation.error) {
        normalizedOperation.error =
          operation.error instanceof Error
            ? operation.error
            : new Error(String(operation.error));
      }

      return normalizedOperation;
    } catch (error) {
      console.warn("Error normalizing async operation:", error);
      return null;
    }
  }

  private basicQiqeToJsCompilation(
    code: string,
    imports: string,
    resolvedImports: ResolvedImport[],
  ): string {
    return `
      ${imports}

      let output = "";

      // Enhanced async operation support
      const asyncOperations = new Map();
      let asyncCounter = 0;
      const maxConcurrentOps = 10; // Default limit
      const defaultTimeout = 30000; // 30 seconds

      // Promise creation with enhanced tracking and timeout
      const createPromise__qq = (executor, timeout = defaultTimeout) => {
        const operationId = \`promise_\${++asyncCounter}\`;
        const startTime = Date.now();

        // Check concurrent operation limit
        const activeOps = Array.from(asyncOperations.values())
          .filter(op => op.status === 'pending').length;

        if (activeOps >= maxConcurrentOps) {
          throw new Error(\`Maximum concurrent async operations (\${maxConcurrentOps}) exceeded\`);
        }

        const operationInfo = {
          id: operationId,
          type: 'promise',
          status: 'pending',
          startTime,
          timeout
        };

        asyncOperations.set(operationId, operationInfo);

        const promise = new Promise((resolve, reject) => {
          // Set up operation timeout
          const timeoutId = setTimeout(() => {
            operationInfo.status = 'cancelled';
            operationInfo.endTime = Date.now();
            operationInfo.error = new Error(\`Promise timeout after \${timeout}ms\`);
            asyncOperations.set(operationId, operationInfo);
            reject(operationInfo.error);
          }, timeout);

          try {
            executor((value) => {
              clearTimeout(timeoutId);
              operationInfo.status = 'resolved';
              operationInfo.endTime = Date.now();
              operationInfo.result = value;
              asyncOperations.set(operationId, operationInfo);
              resolve(value);
            }, (error) => {
              clearTimeout(timeoutId);
              operationInfo.status = 'rejected';
              operationInfo.endTime = Date.now();
              operationInfo.error = error;
              asyncOperations.set(operationId, operationInfo);
              reject(error);
            });
          } catch (error) {
            clearTimeout(timeoutId);
            operationInfo.status = 'rejected';
            operationInfo.endTime = Date.now();
            operationInfo.error = error;
            asyncOperations.set(operationId, operationInfo);
            reject(error);
          }
        });

        return promise;
      };

      // Enhanced await with timeout support
      const await__qq = async (promise, timeoutMs = defaultTimeout) => {
        if (!promise || typeof promise.then !== 'function') {
          return promise; // Return non-promise values immediately
        }

        // Create timeout wrapper for the await operation
        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => {
            reject(new Error(\`Await timeout after \${timeoutMs}ms\`));
          }, timeoutMs);
        });

        try {
          return await Promise.race([promise, timeoutPromise]);
        } catch (error) {
          // Track failed await operations
          const operationId = \`await_\${++asyncCounter}\`;
          asyncOperations.set(operationId, {
            id: operationId,
            type: 'await',
            status: 'rejected',
            startTime: Date.now(),
            endTime: Date.now(),
            error
          });
          throw error;
        }
      };

      // Delay function with timeout protection
      const delay__qq = (ms, value = undefined) => {
        if (ms > defaultTimeout) {
          throw new Error(\`Delay duration (\${ms}ms) exceeds maximum timeout (\${defaultTimeout}ms)\`);
        }

        return createPromise__qq((resolve) => {
          setTimeout(() => resolve(value), ms);
        }, Math.min(ms + 1000, defaultTimeout)); // Add 1s buffer or use max timeout
      };

      // Promise utilities
      const promiseAll__qq = async (promises, timeoutMs = defaultTimeout) => {
        if (!Array.isArray(promises)) {
          throw new Error('promiseAll expects an array of promises');
        }

        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => {
            reject(new Error(\`Promise.all timeout after \${timeoutMs}ms\`));
          }, timeoutMs);
        });

        try {
          return await Promise.race([Promise.all(promises), timeoutPromise]);
        } catch (error) {
          // Cancel any pending operations
          promises.forEach((promise) => {
            if (promise && typeof promise.catch === 'function') {
              promise.catch(() => {}); // Prevent unhandled rejection warnings
            }
          });
          throw error;
        }
      };

      const promiseRace__qq = async (promises, timeoutMs = defaultTimeout) => {
        if (!Array.isArray(promises)) {
          throw new Error('promiseRace expects an array of promises');
        }

        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => {
            reject(new Error(\`Promise.race timeout after \${timeoutMs}ms\`));
          }, timeoutMs);
        });

        return await Promise.race([...promises, timeoutPromise]);
      };

      // Async function wrapper for qiqe functions
      const asyncFunction__qq = (fn) => {
        return async (...args) => {
          try {
            const result = await fn(...args);
            return result;
          } catch (error) {
            // Track async function errors
            const operationId = \`async_fn_\${++asyncCounter}\`;
            asyncOperations.set(operationId, {
              id: operationId,
              type: 'async_function',
              status: 'rejected',
              startTime: Date.now(),
              endTime: Date.now(),
              error
            });
            throw error;
          }
        };
      };

      // Timeout wrapper for any promise
      const timeout__qq = (promise, timeoutMs) => {
        return Promise.race([
          promise,
          new Promise((_, reject) => {
            setTimeout(() => reject(new Error(\`Operation timeout after \${timeoutMs}ms\`)), timeoutMs);
          })
        ]);
      };

      // Retry mechanism for async operations
      const retry__qq = async (operation, maxRetries = 3, delay = 1000) => {
        let lastError;

        for (let attempt = 1; attempt <= maxRetries; attempt++) {
          try {
            return await operation();
          } catch (error) {
            lastError = error;

            if (attempt === maxRetries) {
              throw new Error(\`Operation failed after \${maxRetries} attempts: \${error.message}\`);
            }

            // Wait before retrying
            await delay__qq(delay * attempt); // Exponential backoff
          }
        }

        throw lastError;
      };

      // Async operation status checker
      const getAsyncStatus__qq = () => {
        const operations = Array.from(asyncOperations.values());
        return {
          total: operations.length,
          pending: operations.filter(op => op.status === 'pending').length,
          resolved: operations.filter(op => op.status === 'resolved').length,
          rejected: operations.filter(op => op.status === 'rejected').length,
          cancelled: operations.filter(op => op.status === 'cancelled').length,
          operations: operations
        };
      };

      // Type converter utilities
      const qiqeToJavaScript = (value) => {
        // Basic type conversion logic
        if (value === null || value === undefined) return value;
        if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
          return value;
        }
        if (Array.isArray(value)) {
          return value.map(v => qiqeToJavaScript(v));
        }
        if (typeof value === 'object') {
          const result = {};
          for (const [key, val] of Object.entries(value)) {
            result[key] = qiqeToJavaScript(val);
          }
          return result;
        }
        return value;
      };

      // Import context for resolved imports
      const importContext = ${JSON.stringify(resolvedImports.map((imp) => ({ path: imp.spec.path, alias: imp.spec.alias })))};

      // Async execution wrapper with enhanced error handling
      const executeAsync = async () => {
        try {
          // User code (potentially async)
          const result = await (async () => {
            ${code}
          })();

          // Wait for any remaining async operations to complete
          const pendingOps = Array.from(asyncOperations.values())
            .filter(op => op.status === 'pending');

          if (pendingOps.length > 0) {
            // Give pending operations a chance to complete
            await new Promise((resolve) => {
              const checkInterval = setInterval(() => {
                const stillPending = Array.from(asyncOperations.values())
                  .filter(op => op.status === 'pending');

                if (stillPending.length === 0) {
                  clearInterval(checkInterval);
                  resolve();
                }
              }, 100);

              // Timeout after 5 seconds
              setTimeout(() => {
                clearInterval(checkInterval);
                resolve();
              }, 5000);
            });
          }

          return {
            output,
            result,
            asyncOperations: Array.from(asyncOperations.values()),
            asyncStatus: getAsyncStatus__qq()
          };
        } catch (error) {
          // Track execution errors
          const operationId = \`execution_\${++asyncCounter}\`;
          asyncOperations.set(operationId, {
            id: operationId,
            type: 'execution',
            status: 'rejected',
            startTime: Date.now(),
            endTime: Date.now(),
            error
          });
          throw error;
        }
      };

      // Execute and return result
      executeAsync();
    `;
  }

  private checkBalancedDelimiters(code: string): {
    isValid: boolean;
    message: string;
  } {
    const stack: string[] = [];
    const pairs: Record<string, string> = { "(": ")", "[": "]", "{": "}" };
    const opening = Object.keys(pairs);
    const closing = Object.values(pairs);

    for (let i = 0; i < code.length; i++) {
      const char = code[i];

      if (opening.includes(char)) {
        stack.push(char);
      } else if (closing.includes(char)) {
        if (stack.length === 0) {
          return {
            isValid: false,
            message: `Unmatched closing delimiter '${char}' at position ${i}`,
          };
        }
        const last = stack.pop()!;
        if (pairs[last] !== char) {
          return {
            isValid: false,
            message: `Mismatched delimiter: expected '${pairs[last]}' but found '${char}' at position ${i}`,
          };
        }
      }
    }

    if (stack.length > 0) {
      return {
        isValid: false,
        message: `Unmatched opening delimiter '${stack[stack.length - 1]}'`,
      };
    }

    return { isValid: true, message: "" };
  }

  private checkQiqePatterns(code: string): {
    isValid: boolean;
    message: string;
  } {
    // Basic qiqe pattern validation
    // This would be much more sophisticated in a real implementation

    // Check for invalid characters or patterns
    const invalidPatterns = [
      {
        pattern: /\$\{/,
        message: "Template literals are not supported in qiqe",
      },
      { pattern: /`/, message: "Template strings are not supported in qiqe" },
      {
        pattern: /class\s+/,
        message: "Class declarations are not supported in qiqe",
      },
    ];

    for (const { pattern, message } of invalidPatterns) {
      if (pattern.test(code)) {
        return { isValid: false, message };
      }
    }

    return { isValid: true, message: "" };
  }

  private createErrorResult(
    errors: ErrorInfo[],
    startTime: number,
  ): EvaluationResult {
    return {
      output: "",
      errors,
      warnings: [],
      executionTime: Date.now() - startTime,
      memoryUsage: {
        memoryUsageMB: 0,
        executionTimeMs: Date.now() - startTime,
        callStackDepth: 0,
        outputLength: 0,
        lastUpdated: new Date(),
      },
      success: false,
    };
  }

  private generateDebugInfo(
    executionResult: SandboxExecutionResult,
  ): DebugInfo {
    // Generate debug information from execution result
    return {
      executionSteps: [], // Would be populated with actual execution steps
      variableStates: [], // Would be populated with variable states
      functionCalls: [], // Would be populated with function call information
      performanceMetrics: {
        compilationTime: 0, // Would be measured during compilation
        executionTime: executionResult.executionTime,
        memoryPeak: executionResult.resourceUsage.memoryUsageMB,
        memoryAverage: executionResult.resourceUsage.memoryUsageMB,
      },
    };
  }
}

/**
 * Type converter implementation for qiqe to JavaScript conversions
 */
class QiqeTypeConverter implements TypeConverter {
  qiqeToJavaScript(value: any, targetType?: string): any {
    if (value === null || value === undefined) {
      return value;
    }

    // Handle primitive types
    if (
      typeof value === "string" ||
      typeof value === "number" ||
      typeof value === "boolean"
    ) {
      if (targetType) {
        return this.convertToTargetType(value, targetType);
      }
      return value;
    }

    // Handle arrays
    if (Array.isArray(value)) {
      return value.map((v) => this.qiqeToJavaScript(v));
    }

    // Handle objects
    if (typeof value === "object") {
      const result: Record<string, any> = {};
      for (const [key, val] of Object.entries(value)) {
        result[key] = this.qiqeToJavaScript(val);
      }
      return result;
    }

    return value;
  }

  javaScriptToQiqe(value: any, targetType?: string): any {
    // For now, this is symmetric with qiqeToJavaScript
    // In a real implementation, this might handle qiqe-specific types
    return this.qiqeToJavaScript(value, targetType);
  }

  inferType(value: any): string {
    if (value === null) return "null";
    if (value === undefined) return "undefined";
    if (Array.isArray(value)) return "array";
    return typeof value;
  }

  validateConversion(value: any, fromType: string, toType: string): boolean {
    // Basic validation logic - fromType is used for validation context
    try {
      // Check if the conversion makes sense based on source type
      if (fromType === "string" && toType === "number") {
        return !isNaN(Number(value));
      }
      if (fromType === "array" && toType !== "array" && toType !== "object") {
        return false;
      }

      this.convertToTargetType(value, toType);
      return true;
    } catch {
      return false;
    }
  }

  private convertToTargetType(value: any, targetType: string): any {
    switch (targetType.toLowerCase()) {
      case "string":
        return String(value);
      case "number":
        const num = Number(value);
        if (isNaN(num)) throw new Error(`Cannot convert ${value} to number`);
        return num;
      case "boolean":
        return Boolean(value);
      case "array":
        if (!Array.isArray(value))
          throw new Error(`Cannot convert ${value} to array`);
        return value;
      case "object":
        if (typeof value !== "object")
          throw new Error(`Cannot convert ${value} to object`);
        return value;
      default:
        return value;
    }
  }
}

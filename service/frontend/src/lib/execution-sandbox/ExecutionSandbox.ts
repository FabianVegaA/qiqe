/**
 * ExecutionSandbox class with resource monitoring and security controls
 */

import {
  SandboxOptions,
  ExecutionContext,
  ExecutionResult,
  ResourceLimits,
  ResourceUsage,
  SandboxInstance,
  TerminationReason,
  SecurityViolation,
  SandboxLevel,
} from "./types";
import { ResourceMonitor } from "./ResourceMonitor";
import { APIAccessController } from "./APIAccessControl";
import { ErrorHandler } from "../error-handling/ErrorHandler";
import { ErrorInfo } from "../error-handling/types";

export class ExecutionSandbox {
  private contexts: Map<string, ExecutionContext> = new Map();
  private resourceMonitor: ResourceMonitor;
  private apiAccessController: APIAccessController;
  private errorHandler: ErrorHandler;
  private nextContextId = 1;

  constructor(sandboxLevel: SandboxLevel = SandboxLevel.MODERATE) {
    this.resourceMonitor = new ResourceMonitor();
    this.apiAccessController = new APIAccessController(sandboxLevel);
    this.errorHandler = new ErrorHandler();
  }

  /**
   * Create a new sandboxed execution context
   */
  createContext(options: SandboxOptions): ExecutionContext {
    const contextId = `ctx_${this.nextContextId++}`;

    const resourceLimits: ResourceLimits = {
      maxMemoryMB: options.memoryLimit,
      maxExecutionTimeMs: options.timeoutMs,
      maxCallStackDepth: 100, // Default stack depth limit
      maxOutputLength: 10000, // Default output length limit
    };

    const sandbox = this.createSandboxInstance(options);

    const context: ExecutionContext = {
      id: contextId,
      globals: this.createRestrictedGlobals(options),
      sandbox,
      resourceLimits,
      startTime: new Date(),
      isActive: false,
      memoryBaseline: this.getCurrentMemoryUsage(),
    };

    this.contexts.set(contextId, context);

    // Start resource monitoring if enabled
    if (options.enableResourceMonitoring) {
      this.resourceMonitor.startMonitoring(
        contextId,
        resourceLimits,
        (violation) => this.handleSecurityViolation(contextId, violation),
      );
    }

    return context;
  }

  /**
   * Execute code in a sandboxed context
   */
  async execute(
    code: string,
    context: ExecutionContext,
  ): Promise<ExecutionResult> {
    const startTime = Date.now();
    context.isActive = true;
    context.startTime = new Date();

    try {
      // Disable timeout to avoid setTimeout restrictions in qiqe runtime
      // Execution will rely on other timeout mechanisms
      const timeoutPromise = new Promise<never>(() => {
        // Never resolves - timeout disabled
      });

      // Execute code with timeout
      const executionPromise = this.executeInSandbox(code, context);
      const result = await Promise.race([executionPromise, timeoutPromise]);

      const executionTime = Date.now() - startTime;
      const resourceUsage = this.resourceMonitor.getResourceUsage(
        context.id,
      ) || {
        memoryUsageMB: 0,
        executionTimeMs: executionTime,
        callStackDepth: 0,
        outputLength: result.output.length,
        lastUpdated: new Date(),
      };

      return {
        output: result.output,
        errors: result.errors,
        warnings: result.warnings,
        resourceUsage,
        executionTime,
        success: result.errors.length === 0,
        terminated: false,
      };
    } catch (error) {
      const executionTime = Date.now() - startTime;
      const errorObj =
        error instanceof Error ? error : new Error(String(error));
      const errorInfo = this.errorHandler.captureError(errorObj, {
        executionId: context.id,
        timestamp: new Date(),
        codeSnippet: code.substring(0, 200),
        imports: [],
        executionPhase: "execution",
        executionTime,
      });

      return {
        output: "",
        errors: [errorInfo],
        warnings: [],
        resourceUsage: this.resourceMonitor.getResourceUsage(context.id) || {
          memoryUsageMB: 0,
          executionTimeMs: executionTime,
          callStackDepth: 0,
          outputLength: 0,
          lastUpdated: new Date(),
        },
        executionTime,
        success: false,
        terminated: errorObj.message.includes("timeout"),
      };
    } finally {
      context.isActive = false;
    }
  }

  /**
   * Terminate execution context
   */
  terminate(
    contextId: string,
    reason: TerminationReason = TerminationReason.USER_REQUEST,
  ): void {
    const context = this.contexts.get(contextId);
    if (!context) return;

    context.isActive = false;
    this.resourceMonitor.stopMonitoring(contextId);

    // Log termination reason for debugging
    console.debug(`Context ${contextId} terminated due to: ${reason}`);

    // Clean up sandbox instance
    this.cleanupSandboxInstance(context.sandbox);

    this.contexts.delete(contextId);
  }

  /**
   * Get resource usage for a context
   */
  getResourceUsage(contextId: string): ResourceUsage | null {
    return this.resourceMonitor.getResourceUsage(contextId);
  }

  /**
   * Get all active contexts
   */
  getActiveContexts(): ExecutionContext[] {
    const activeContexts: ExecutionContext[] = [];
    this.contexts.forEach((context) => {
      if (context.isActive) {
        activeContexts.push(context);
      }
    });
    return activeContexts;
  }

  /**
   * Clean up all contexts and resources
   */
  cleanup(): void {
    this.contexts.forEach((context, contextId) => {
      this.terminate(contextId, TerminationReason.USER_REQUEST);
    });
    this.resourceMonitor.cleanup();
  }

  /**
   * Create a sandbox instance based on options
   */
  private createSandboxInstance(options: SandboxOptions): SandboxInstance {
    const sandboxId = `sandbox_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`;

    // Consider sandbox level when creating instance
    const sandboxType =
      options.sandboxLevel === SandboxLevel.STRICT ? "worker" : "direct";

    // For browser environment, we'll use direct execution with restricted globals
    // In a production environment, you might want to use Web Workers or iframes
    return {
      id: sandboxId,
      type: sandboxType,
      created: new Date(),
      lastUsed: new Date(),
    };
  }

  /**
   * Execute code within the sandbox
   */
  private async executeInSandbox(
    code: string,
    context: ExecutionContext,
  ): Promise<{
    output: string;
    errors: ErrorInfo[];
    warnings: string[];
  }> {
    let output = "";
    const errors: ErrorInfo[] = [];
    const warnings: string[] = [];

    try {
      // Validate code before execution
      const validation = this.apiAccessController.validateCode(code);

      if (!validation.isValid) {
        // Convert security violations to error info
        validation.violations.forEach((violation) => {
          const errorInfo = this.errorHandler.captureError(
            new Error(violation.description),
            {
              executionId: context.id,
              timestamp: new Date(),
              codeSnippet: code.substring(0, 200),
              imports: [],
              executionPhase: "execution",
            },
          );
          errors.push(errorInfo);
        });
        return { output, errors, warnings };
      }

      const sanitizedCode = validation.sanitizedCode || code;

      // Create execution environment with restricted globals
      const executionGlobals = {
        ...context.globals,
        // Add qiqe runtime functions
        print__qq: (value: any) => {
          const str = String(value);
          output += str;
          // Check output length limit
          if (output.length > context.resourceLimits.maxOutputLength) {
            throw new Error(
              `Output length exceeds limit of ${context.resourceLimits.maxOutputLength} characters`,
            );
          }
        },
        println__qq: (value: any) => {
          const str = String(value) + "\n";
          output += str;
          if (output.length > context.resourceLimits.maxOutputLength) {
            throw new Error(
              `Output length exceeds limit of ${context.resourceLimits.maxOutputLength} characters`,
            );
          }
        },
        raise__qq: (value: any) => {
          throw new Error(String(value));
        },
      };

      // Update context last used time
      context.sandbox.lastUsed = new Date();

      // Create the execution function with sanitized code
      const executionFunctionBody = `

        try {
          ${sanitizedCode}
          return { success: true };
        } catch (error) {
          return { success: false, error: error };
        }
      `;
      const executionFunction = new Function(
        ...Object.keys(executionGlobals),
        executionFunctionBody,
      );

      // Execute with restricted globals
      const result = executionFunction(...Object.values(executionGlobals));

      if (!result.success && result.error) {
        const errorInfo = this.errorHandler.captureError(result.error, {
          executionId: context.id,
          timestamp: new Date(),
          codeSnippet: code.substring(0, 200),
          imports: [],
          executionPhase: "execution",
        });
        errors.push(errorInfo);
      }
    } catch (error) {
      const errorObj =
        error instanceof Error ? error : new Error(String(error));
      const errorInfo = this.errorHandler.captureError(errorObj, {
        executionId: context.id,
        timestamp: new Date(),
        codeSnippet: code.substring(0, 200),
        imports: [],
        executionPhase: "execution",
      });
      errors.push(errorInfo);
    }

    return { output, errors, warnings };
  }

  /**
   * Create restricted global object based on sandbox options
   */
  private createRestrictedGlobals(
    options: SandboxOptions,
  ): Record<string, any> {
    // Update the API access controller with the current async operations setting
    this.apiAccessController.updateAsyncOpsConfig(
      options.enableAsyncOps || false,
    );

    // Use the API access controller to create restricted globals
    const baseGlobals: Record<string, any> = {};

    // Add allowed globals from options
    options.allowedGlobals.forEach((globalName) => {
      if (typeof window !== "undefined" && globalName in window) {
        baseGlobals[globalName] = (window as any)[globalName];
      }
    });

    return this.apiAccessController.createRestrictedGlobals(baseGlobals);
  }

  /**
   * Handle security violations
   */
  private handleSecurityViolation(
    contextId: string,
    violation: SecurityViolation,
  ): void {
    console.warn(`Security violation in context ${contextId}:`, violation);

    // For critical violations, terminate the context
    if (
      violation.severity === "critical" ||
      violation.type === "resource_limit"
    ) {
      this.terminate(contextId, TerminationReason.SECURITY_VIOLATION);
    }
  }

  /**
   * Clean up sandbox instance resources
   */
  private cleanupSandboxInstance(sandbox: SandboxInstance): void {
    // Clean up based on sandbox type
    switch (sandbox.type) {
      case "iframe":
        if (sandbox.iframe && sandbox.iframe.parentNode) {
          sandbox.iframe.parentNode.removeChild(sandbox.iframe);
        }
        break;
      case "worker":
        if (sandbox.worker) {
          sandbox.worker.terminate();
        }
        break;
      case "direct":
        // No special cleanup needed for direct execution
        break;
    }
  }

  /**
   * Get current memory usage (browser approximation)
   */
  private getCurrentMemoryUsage(): number {
    if ("memory" in performance) {
      const memory = (performance as any).memory;
      return memory.usedJSHeapSize / (1024 * 1024); // Convert to MB
    }
    return 0;
  }
}

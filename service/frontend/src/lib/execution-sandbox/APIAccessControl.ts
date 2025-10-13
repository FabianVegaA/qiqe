/**
 * API Access Control system for execution sandbox
 */

import { APIAccessControl, SecurityViolation, SandboxLevel } from "./types";

export class APIAccessController {
  private accessControl: APIAccessControl;
  private sandboxLevel: SandboxLevel;
  private enableAsyncOps: boolean;

  constructor(
    sandboxLevel: SandboxLevel = SandboxLevel.MODERATE,
    enableAsyncOps: boolean = false,
  ) {
    this.sandboxLevel = sandboxLevel;
    this.enableAsyncOps = enableAsyncOps;
    this.accessControl = this.createAccessControl(sandboxLevel, enableAsyncOps);
  }

  /**
   * Update the async operations setting and reconfigure access control
   */
  public updateAsyncOpsConfig(enableAsyncOps: boolean): void {
    if (this.enableAsyncOps !== enableAsyncOps) {
      this.enableAsyncOps = enableAsyncOps;
      this.accessControl = this.createAccessControl(
        this.sandboxLevel,
        enableAsyncOps,
      );
    }
  }

  /**
   * Check if an API is allowed to be accessed
   */
  isAPIAllowed(apiName: string): boolean {
    // Check blocked globals first
    if (this.accessControl.blockedGlobals.has(apiName)) {
      return false;
    }

    // Check blocked patterns
    for (const pattern of this.accessControl.blockedPatterns) {
      if (pattern.test(apiName)) {
        return false;
      }
    }

    // Check allowed globals
    if (this.accessControl.allowedGlobals.has(apiName)) {
      return true;
    }

    // Check allowed patterns
    for (const pattern of this.accessControl.allowedPatterns) {
      if (pattern.test(apiName)) {
        return true;
      }
    }

    // Run custom validators
    for (const validator of this.accessControl.customValidators) {
      if (validator(apiName)) {
        return true;
      }
    }

    // Default deny for strict mode, allow for permissive mode
    return this.sandboxLevel === SandboxLevel.PERMISSIVE;
  }

  /**
   * Validate and sanitize JavaScript code before execution
   */
  validateCode(code: string): {
    isValid: boolean;
    violations: SecurityViolation[];
    sanitizedCode?: string;
  } {
    const violations: SecurityViolation[] = [];
    let sanitizedCode = code;

    // Check for dangerous patterns
    const dangerousPatterns = this.getDangerousPatterns();

    dangerousPatterns.forEach((description, pattern) => {
      if (pattern.test(code)) {
        violations.push({
          type: "code_injection",
          description: `Potentially dangerous code detected: ${description}`,
          severity: "high",
          timestamp: new Date(),
          blocked: true,
        });
      }
    });

    // Check for restricted API usage
    const apiUsagePatterns = this.getAPIUsagePatterns();

    apiUsagePatterns.forEach((apiName, pattern) => {
      const matches = code.match(pattern);
      if (matches) {
        if (!this.isAPIAllowed(apiName)) {
          violations.push({
            type: "api_access",
            description: `Attempted to access restricted API: ${apiName}`,
            severity: "medium",
            timestamp: new Date(),
            blocked: true,
          });
        }
      }
    });

    // Sanitize code if needed
    if (violations.length === 0) {
      sanitizedCode = this.sanitizeCode(code);
    }

    return {
      isValid: violations.length === 0,
      violations,
      sanitizedCode: violations.length === 0 ? sanitizedCode : undefined,
    };
  }

  /**
   * Create a secure eval wrapper with validation
   */
  createSecureEval(): (code: string) => any {
    return (code: string) => {
      const validation = this.validateCode(code);

      if (!validation.isValid) {
        const violationMessages = validation.violations
          .map((v) => v.description)
          .join("; ");
        throw new Error(`Code validation failed: ${violationMessages}`);
      }

      if (!validation.sanitizedCode) {
        throw new Error("Code sanitization failed");
      }

      // Use Function constructor instead of eval for better security
      try {
        const func = new Function("return (" + validation.sanitizedCode + ")");
        return func();
      } catch (error) {
        throw new Error(
          `Secure eval failed: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
    };
  }

  /**
   * Create restricted global object environment
   */
  createRestrictedGlobals(
    baseGlobals: Record<string, any> = {},
  ): Record<string, any> {
    const restrictedGlobals: Record<string, any> = {};

    // Add allowed globals from base
    Object.keys(baseGlobals).forEach((key) => {
      if (this.isAPIAllowed(key)) {
        restrictedGlobals[key] = baseGlobals[key];
      }
    });

    // Add safe built-in objects and functions
    const safeBuiltins = this.getSafeBuiltins();
    safeBuiltins.forEach((builtin) => {
      if (
        this.isAPIAllowed(builtin) &&
        typeof window !== "undefined" &&
        builtin in window
      ) {
        restrictedGlobals[builtin] = (window as any)[builtin];
      }
    });

    // Add secure eval wrapper
    restrictedGlobals.eval = this.createSecureEval();

    return restrictedGlobals;
  }

  /**
   * Update access control configuration
   */
  updateAccessControl(updates: Partial<APIAccessControl>): void {
    if (updates.allowedGlobals) {
      this.accessControl.allowedGlobals = updates.allowedGlobals;
    }
    if (updates.blockedGlobals) {
      this.accessControl.blockedGlobals = updates.blockedGlobals;
    }
    if (updates.allowedPatterns) {
      this.accessControl.allowedPatterns = updates.allowedPatterns;
    }
    if (updates.blockedPatterns) {
      this.accessControl.blockedPatterns = updates.blockedPatterns;
    }
    if (updates.customValidators) {
      this.accessControl.customValidators = updates.customValidators;
    }
  }

  /**
   * Create access control configuration based on sandbox level
   */
  private createAccessControl(
    level: SandboxLevel,
    enableAsyncOps: boolean = false,
  ): APIAccessControl {
    const baseConfig: APIAccessControl = {
      allowedGlobals: new Set(),
      blockedGlobals: new Set(),
      allowedPatterns: [],
      blockedPatterns: [],
      customValidators: [],
    };

    switch (level) {
      case SandboxLevel.STRICT:
        return this.createStrictAccessControl(baseConfig, enableAsyncOps);
      case SandboxLevel.MODERATE:
        return this.createModerateAccessControl(baseConfig, enableAsyncOps);
      case SandboxLevel.PERMISSIVE:
        return this.createPermissiveAccessControl(baseConfig, enableAsyncOps);
      default:
        return this.createModerateAccessControl(baseConfig, enableAsyncOps);
    }
  }

  /**
   * Create strict access control (minimal permissions)
   */
  private createStrictAccessControl(
    config: APIAccessControl,
    enableAsyncOps: boolean = false,
  ): APIAccessControl {
    // Only allow essential JavaScript built-ins
    const allowedGlobals = [
      "Object",
      "Array",
      "String",
      "Number",
      "Boolean",
      "Date",
      "Math",
      "JSON",
      "parseInt",
      "parseFloat",
      "isNaN",
      "isFinite",
    ];

    // Add setTimeout/setInterval to allowed globals if async operations are enabled
    if (enableAsyncOps) {
      allowedGlobals.push(
        "Promise",
        "setTimeout",
        "setInterval",
        "clearTimeout",
        "clearInterval",
      );
    }

    config.allowedGlobals = new Set(allowedGlobals);

    // Block dangerous APIs
    const blockedGlobals = [
      "eval",
      "Function",
      "XMLHttpRequest",
      "fetch",
      "WebSocket",
      "Worker",
      "SharedWorker",
      "localStorage",
      "sessionStorage",
      "indexedDB",
      "document",
      "window",
      "location",
      "history",
      "navigator",
      "screen",
      "alert",
      "confirm",
      "prompt",
    ];

    // Add setTimeout/setInterval to blocked globals only if async operations are disabled
    if (!enableAsyncOps) {
      blockedGlobals.push(
        "setTimeout",
        "setInterval",
        "clearTimeout",
        "clearInterval",
      );
    }

    config.blockedGlobals = new Set(blockedGlobals);

    // Block dangerous patterns
    const blockedPatterns = [
      /\beval\s*\(/gi,
      /\bFunction\s*\(/gi,
      /\b__proto__\b/gi,
      /\bconstructor\b/gi,
      /\bprototype\b/gi,
    ];

    // Add setTimeout/setInterval patterns only if async operations are disabled
    if (!enableAsyncOps) {
      blockedPatterns.push(/\bsetTimeout\s*\(/gi, /\bsetInterval\s*\(/gi);
    }

    config.blockedPatterns = blockedPatterns;

    return config;
  }

  /**
   * Create moderate access control (balanced permissions)
   */
  private createModerateAccessControl(
    config: APIAccessControl,
    enableAsyncOps: boolean = false,
  ): APIAccessControl {
    // Allow more JavaScript built-ins
    const allowedGlobals = [
      "Object",
      "Array",
      "String",
      "Number",
      "Boolean",
      "Date",
      "Math",
      "JSON",
      "parseInt",
      "parseFloat",
      "isNaN",
      "isFinite",
      "RegExp",
      "Error",
      "TypeError",
      "ReferenceError",
      "SyntaxError",
      "RangeError",
    ];

    // Add setTimeout/setInterval to allowed globals if async operations are enabled
    if (enableAsyncOps) {
      allowedGlobals.push(
        "Promise",
        "setTimeout",
        "setInterval",
        "clearTimeout",
        "clearInterval",
      );
    }

    config.allowedGlobals = new Set(allowedGlobals);

    // Block dangerous APIs
    const blockedGlobals = [
      "XMLHttpRequest",
      "fetch",
      "WebSocket",
      "Worker",
      "localStorage",
      "sessionStorage",
      "indexedDB",
      "document",
      "window",
      "location",
      "history",
      "alert",
      "confirm",
      "prompt",
    ];

    // Note: eval and Function constructor are not blocked in moderate mode to allow standard library usage

    // Add setTimeout/setInterval to blocked globals only if async operations are disabled
    if (!enableAsyncOps) {
      blockedGlobals.push(
        "setTimeout",
        "setInterval",
        "clearTimeout",
        "clearInterval",
      );
    }

    config.blockedGlobals = new Set(blockedGlobals);

    // Block dangerous patterns
    const blockedPatterns = [
      /\b__proto__\b/gi,
      /\bdocument\./gi,
      /\bwindow\./gi,
    ];

    // Note: eval and Function constructor patterns are not blocked in moderate mode to allow standard library usage

    // Add setTimeout/setInterval patterns only if async operations are disabled
    if (!enableAsyncOps) {
      blockedPatterns.push(/\bsetTimeout\s*\(/gi, /\bsetInterval\s*\(/gi);
    }

    config.blockedPatterns = blockedPatterns;

    return config;
  }

  /**
   * Create permissive access control (maximum permissions)
   */
  private createPermissiveAccessControl(
    config: APIAccessControl,
    enableAsyncOps: boolean = false,
  ): APIAccessControl {
    // Allow most JavaScript built-ins
    const allowedPatterns = [
      /^[A-Z][a-zA-Z0-9]*$/, // Built-in constructors
      /^[a-z][a-zA-Z0-9]*$/, // Built-in functions
    ];
    config.allowedPatterns = allowedPatterns;

    // Only block the most dangerous APIs (allow eval and Function for standard library)
    const blockedGlobals = ["XMLHttpRequest", "fetch"];
    config.blockedGlobals = new Set(blockedGlobals);

    // Minimal pattern blocking (allow eval and Function for standard library)
    config.blockedPatterns = [];

    return config;
  }

  /**
   * Get dangerous code patterns to detect
   */
  private getDangerousPatterns(): Map<RegExp, string> {
    const patterns = new Map([
      [/\b__proto__\s*=/gi, "Prototype pollution attempt"],
      [/\bconstructor\s*\.\s*constructor/gi, "Constructor chain access"],
      [/\bprocess\s*\./gi, "Node.js process access"],
      [/\brequire\s*\(/gi, "Node.js require usage"],
      [/\bimport\s*\(/gi, "Dynamic import usage"],
      [/javascript\s*:/gi, "JavaScript protocol usage"],
      [/data\s*:\s*text\/html/gi, "Data URL HTML injection"],
      [/\<script\b/gi, "Script tag injection"],
    ]);

    // Only add eval and Function constructor patterns in strict sandbox mode (standard library needs them)
    if (this.sandboxLevel === SandboxLevel.STRICT) {
      patterns.set(/\beval\s*\(/gi, "Direct eval usage");
      patterns.set(/\bFunction\s*\(/gi, "Function constructor usage");
    }

    return patterns;
  }

  /**
   * Get API usage patterns to detect
   */
  private getAPIUsagePatterns(): Map<RegExp, string> {
    const patterns = new Map([
      [/\bdocument\./gi, "document"],
      [/\bwindow\./gi, "window"],
      [/\blocation\./gi, "location"],
      [/\bnavigator\./gi, "navigator"],
      [/\bhistory\./gi, "history"],
      [/\blocalStorage\./gi, "localStorage"],
      [/\bsessionStorage\./gi, "sessionStorage"],
      [/\bXMLHttpRequest\b/gi, "XMLHttpRequest"],
      [/\bfetch\s*\(/gi, "fetch"],
      [/\bWebSocket\b/gi, "WebSocket"],
      [/\bWorker\b/gi, "Worker"],
    ]);

    // Only add setTimeout/setInterval patterns if async operations are disabled
    if (!this.enableAsyncOps) {
      patterns.set(/\bsetTimeout\s*\(/gi, "setTimeout");
      patterns.set(/\bsetInterval\s*\(/gi, "setInterval");
    }

    return patterns;
  }

  /**
   * Get list of safe built-in objects and functions
   */
  private getSafeBuiltins(): string[] {
    return [
      "Object",
      "Array",
      "String",
      "Number",
      "Boolean",
      "Date",
      "Math",
      "JSON",
      "RegExp",
      "Error",
      "TypeError",
      "ReferenceError",
      "SyntaxError",
      "RangeError",
      "parseInt",
      "parseFloat",
      "isNaN",
      "isFinite",
      "encodeURI",
      "decodeURI",
      "encodeURIComponent",
      "decodeURIComponent",
    ];
  }

  /**
   * Sanitize code by removing or replacing dangerous patterns
   */
  private sanitizeCode(code: string): string {
    let sanitized = code;

    // Remove comments that might contain dangerous code
    sanitized = sanitized.replace(/\/\*[\s\S]*?\*\//g, "");
    sanitized = sanitized.replace(/\/\/.*$/gm, "");

    // Remove or replace dangerous patterns
    const replacements = new Map([
      [/\beval\s*\(/gi, "secureEval("],
      [/\bFunction\s*\(/gi, "secureFunction("],
      [/\b__proto__\b/gi, "__proto_blocked__"],
      [/\bconstructor\s*\.\s*constructor/gi, "constructor_blocked"],
    ]);

    replacements.forEach((replacement, pattern) => {
      sanitized = sanitized.replace(pattern, replacement);
    });

    return sanitized;
  }
}

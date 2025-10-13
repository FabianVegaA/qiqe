/**
 * ImportManager - Handles module imports and dependency resolution
 */

import {
  ImportSpec,
  ResolvedImport,
  Library,
  DependencyGraph,
  DependencyNode,
  DependencyEdge,
  ImportError,
  ImportValidationResult,
} from "./types";

import { apiClient } from "../../api/run-code";

export class ImportManager {
  private libraryCache = new Map<string, Library>();
  private dependencyGraph: DependencyGraph = {
    nodes: [],
    edges: [],
    circularDependencies: [],
  };

  /**
   * Resolve imports and build dependency graph
   */
  async resolveImports(imports: ImportSpec[]): Promise<ResolvedImport[]> {
    // Validate imports first
    const validation = this.validateImports(imports);
    if (!validation.valid) {
      throw validation.errors[0]; // Throw first error
    }

    const resolved: ResolvedImport[] = [];
    const visited = new Set<string>();

    // Reset dependency graph for new resolution
    this.dependencyGraph = {
      nodes: [],
      edges: [],
      circularDependencies: [],
    };

    try {
      // Resolve each import
      for (const importSpec of imports) {
        console.debug(
          `Resolving import: ${importSpec.path} (${importSpec.type})`,
        );

        const resolvedImport = await this.resolveImportRecursive(
          importSpec,
          visited,
          [],
        );
        resolved.push(resolvedImport);
      }

      // Check for circular dependencies
      this.detectCircularDependencies();

      if (this.dependencyGraph.circularDependencies.length > 0) {
        throw this.createCircularDependencyError(
          this.dependencyGraph.circularDependencies[0],
        );
      }

      console.debug(
        `Successfully resolved ${resolved.length} imports with ${this.dependencyGraph.nodes.length} total dependencies`,
      );

      return resolved;
    } catch (error) {
      console.error("Import resolution failed:", error);
      throw error;
    }
  }

  /**
   * Recursively resolve a single import and its dependencies
   */
  private async resolveImportRecursive(
    importSpec: ImportSpec,
    visited: Set<string>,
    dependencyChain: string[],
  ): Promise<ResolvedImport> {
    const resolvedPath = this.resolvePath(importSpec.path, importSpec.type);

    // Check for circular dependency in current chain
    if (dependencyChain.includes(resolvedPath)) {
      throw this.createCircularDependencyError([
        ...dependencyChain,
        resolvedPath,
      ]);
    }

    // Load library
    const library = await this.loadLibrary(resolvedPath);

    // Add to dependency graph
    this.addNodeToGraph(resolvedPath, library);

    // Resolve dependencies
    const dependencies: ResolvedImport[] = [];
    const newDependencyChain = [...dependencyChain, resolvedPath];

    for (const dep of library.dependencies) {
      if (!visited.has(dep.path)) {
        visited.add(dep.path);

        const depImportSpec: ImportSpec = {
          path: dep.path,
          type: this.getImportType(dep.path),
        };

        const resolvedDep = await this.resolveImportRecursive(
          depImportSpec,
          visited,
          newDependencyChain,
        );

        dependencies.push(resolvedDep);

        // Add edge to dependency graph
        this.addEdgeToGraph(resolvedPath, dep.path, "direct");
      }
    }

    // Generate compiled code for this import
    const compiledCode = await this.compileLibrary(library, importSpec);

    return {
      spec: importSpec,
      library,
      dependencies,
      compiledCode,
    };
  }

  /**
   * Resolve import path based on type
   */
  private resolvePath(
    path: string,
    type: ImportSpec["type"],
    currentDirectory?: string,
  ): string {
    // Validate path format first
    if (!this.isValidPath(path)) {
      throw this.createImportError(
        "INVALID_PATH",
        path,
        new Error("Invalid path format"),
      );
    }

    switch (type) {
      case "absolute":
        return this.normalizeAbsolutePath(path);

      case "relative":
        return this.resolveRelativePath(path, currentDirectory);

      case "standard":
        return this.resolveStandardLibraryPath(path);

      default:
        throw this.createImportError(
          "INVALID_PATH",
          path,
          new Error(`Unknown import type: ${type}`),
        );
    }
  }

  /**
   * Validate path format
   */
  private isValidPath(path: string): boolean {
    if (!path || path.trim() === "") {
      return false;
    }

    // Check for invalid characters
    const invalidChars = /[<>:"|?*\x00-\x1f]/;
    if (invalidChars.test(path)) {
      return false;
    }

    // Check for path traversal attempts beyond reasonable limits
    const parts = path.split("/");
    let depth = 0;
    for (const part of parts) {
      if (part === "..") {
        depth--;
        if (depth < -10) {
          // Prevent excessive traversal
          return false;
        }
      } else if (part !== "." && part !== "") {
        depth++;
      }
    }

    return true;
  }

  /**
   * Normalize absolute path
   */
  private normalizeAbsolutePath(path: string): string {
    if (!path.startsWith("/")) {
      throw new Error("Absolute path must start with /");
    }
    return this.normalizePath(path);
  }

  /**
   * Resolve relative import paths
   */
  private resolveRelativePath(path: string, currentDirectory?: string): string {
    // Handle different relative path patterns
    if (path.startsWith("./")) {
      // Same directory
      const relativePath = path.substring(2);
      const baseDir = currentDirectory || "/qiqe/library";
      return this.normalizePath(`${baseDir}/${relativePath}`);
    } else if (path.startsWith("../")) {
      // Parent directory
      const baseDir = currentDirectory || "/qiqe/library";
      const pathParts = baseDir.split("/").filter((part) => part !== "");
      const relativeParts = path.split("/").filter((part) => part !== "");

      // Process each '../' by removing a directory level
      let currentParts = [...pathParts];
      for (const part of relativeParts) {
        if (part === "..") {
          if (currentParts.length > 0) {
            currentParts.pop();
          }
        } else if (part !== ".") {
          currentParts.push(part);
        }
      }

      return "/" + currentParts.join("/");
    } else {
      // Relative path without explicit './' prefix
      const baseDir = currentDirectory || "/qiqe/library";
      return this.normalizePath(`${baseDir}/${path}`);
    }
  }

  /**
   * Normalize path by removing redundant segments
   */
  private normalizePath(path: string): string {
    const parts = path.split("/").filter((part) => part !== "" && part !== ".");
    const normalized: string[] = [];

    for (const part of parts) {
      if (part === "..") {
        if (
          normalized.length > 0 &&
          normalized[normalized.length - 1] !== ".."
        ) {
          normalized.pop();
        } else {
          normalized.push(part);
        }
      } else {
        normalized.push(part);
      }
    }

    return "/" + normalized.join("/");
  }

  /**
   * Resolve standard library paths
   */
  private resolveStandardLibraryPath(path: string): string {
    // Standard library paths are resolved to the qiqe library directory
    return `/qiqe/library/${path}`;
  }

  /**
   * Determine import type from path
   */
  private getImportType(path: string): ImportSpec["type"] {
    if (path.startsWith("./") || path.startsWith("../")) {
      return "relative";
    } else if (path.startsWith("/")) {
      return "absolute";
    } else {
      return "standard";
    }
  }

  /**
   * Load library from path
   */
  async loadLibrary(path: string): Promise<Library> {
    // Check cache first
    if (this.libraryCache.has(path)) {
      return this.libraryCache.get(path)!;
    }

    try {
      // In a real implementation, this would load from file system
      const libraryContent = await this.fetchLibraryContent(path);
      const library = await this.parseLibrary(libraryContent, path);

      // Cache the library
      this.libraryCache.set(path, library);

      return library;
    } catch (error) {
      throw this.createImportError("MODULE_NOT_FOUND", path, error as Error);
    }
  }

  /**
   * Fetch library content using HTTP requests to the /lib API endpoint
   */
  private async fetchLibraryContent(path: string): Promise<string> {
    console.debug(`Fetching library content from path: ${path}`);

    try {
      // Extract filename from path for the API request
      const filename = this.extractFilename(path);
      console.debug(`Making API request for filename: ${filename}`);

      const response = await fetch("/lib", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Accept: "application/json",
          "Cache-Control": "no-cache",
        },
        body: JSON.stringify({ filename }),
        // Add timeout to prevent hanging requests
        signal: AbortSignal.timeout(10000), // 10 second timeout
      });

      if (!response.ok) {
        const errorMessage = `HTTP ${response.status}: ${response.statusText}`;
        console.error(`Failed to fetch library from API: ${errorMessage}`);
        throw new Error(`Failed to fetch library: ${errorMessage}`);
      }

      const responseData = await response.json();
      console.debug(`API response:`, responseData);

      // Check if the API returned an error
      if (!responseData.status) {
        const errorMessage = responseData.error || "Unknown API error";
        console.error(`API returned error: ${errorMessage}`);
        throw new Error(`Library API error: ${errorMessage}`);
      }

      const content = responseData.target_code;
      if (!content) {
        throw new Error("API returned empty content");
      }

      console.debug(
        `Successfully fetched library content (${content.length} characters) from ${path}`,
      );

      return content;
    } catch (error) {
      console.error(`Error fetching library content from ${path}:`, error);

      // Re-throw with more context
      if (error instanceof Error) {
        throw new Error(
          `Failed to load library from ${path}: ${error.message}`,
        );
      } else {
        throw new Error(`Failed to load library from ${path}: Unknown error`);
      }
    }
  }

  /**
   * Extract filename from library path for API requests
   */
  private extractFilename(path: string): string {
    // Remove leading slash if present
    const cleanPath = path.startsWith("/") ? path.substring(1) : path;

    // For standard library paths like "/qiqe/library/std.qq", extract just "std.qq"
    // For other paths, extract the filename part
    const parts = cleanPath.split("/");
    return parts[parts.length - 1];
  }

  /**
   * Parse library content and extract metadata
   */
  private async parseLibrary(content: string, path: string): Promise<Library> {
    // This would typically parse the qiqe code and extract exports/imports
    // For now, return a basic library structure
    const compiledLibrary = await apiClient.executeCode(content);
    if (compiledLibrary.error) {
      throw new Error(`Failed to compile library: ${compiledLibrary.error}`);
    }
    const compiledCode = compiledLibrary.result;
    const exports = this.extractExports(compiledCode);
    const dependencies = this.extractDependencies(compiledCode);

    return {
      name: this.getLibraryName(path),
      version: "1.0.0",
      path,
      exports,
      dependencies,
      metadata: {
        author: "qiqe",
        description: "Library",
        tags: [],
        lastModified: new Date(),
        size: compiledCode.length,
        checksum: this.calculateChecksum(compiledCode),
      },
      compiledCode: compiledCode,
    };
  }

  /**
   * Extract exported functions from library content
   */
  private extractExports(content: string): any[] {
    // Simple regex-based extraction (would be more sophisticated in real implementation)
    const exportMatches = content.match(/export\s+function\s+(\w+)/g) || [];

    return exportMatches.map((match) => {
      const name = match.replace(/export\s+function\s+/, "");
      return {
        name,
        signature: `${name}()`,
        documentation: `Function ${name}`,
        examples: [],
        deprecated: false,
        visibility: "public",
      };
    });
  }

  /**
   * Extract dependencies from library content
   */
  private extractDependencies(content: string): any[] {
    // Simple regex-based extraction (would be more sophisticated in real implementation)
    const importMatches =
      content.match(/import\s+.*\s+from\s+['"]([^'"]+)['"]/g) || [];

    return importMatches.map((match) => {
      const pathMatch = match.match(/from\s+['"]([^'"]+)['"]/);
      const path = pathMatch ? pathMatch[1] : "";

      return {
        name: this.getLibraryName(path),
        version: "1.0.0",
        path,
        required: true,
      };
    });
  }

  /**
   * Get library name from path
   */
  private getLibraryName(path: string): string {
    return path.split("/").pop()?.replace(".qq", "") || "unknown";
  }

  /**
   * Calculate checksum for content
   */
  private calculateChecksum(content: string): string {
    // Simple hash function (would use proper crypto hash in real implementation)
    let hash = 0;
    for (let i = 0; i < content.length; i++) {
      const char = content.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return hash.toString(16);
  }

  /**
   * Compile library with import specifications
   */
  private async compileLibrary(
    library: Library,
    importSpec: ImportSpec,
  ): Promise<string> {
    let compiledCode = library.compiledCode;

    // Handle selective imports
    if (importSpec.selective && importSpec.selective.length > 0) {
      compiledCode = this.filterSelectiveImports(
        compiledCode,
        importSpec.selective,
      );
    }

    // Handle aliases
    if (importSpec.alias) {
      compiledCode = this.applyAlias(compiledCode, importSpec.alias);
    }

    return compiledCode;
  }

  /**
   * Filter code for selective imports
   */
  private filterSelectiveImports(code: string, selective: string[]): string {
    // Extract only the selected functions
    const lines = code.split("\n");
    const filteredLines: string[] = [];

    for (const line of lines) {
      const isExport = line.trim().startsWith("export");
      if (isExport) {
        const functionName = this.extractFunctionName(line);
        if (functionName && selective.includes(functionName)) {
          filteredLines.push(line);
        }
      } else if (!line.trim().startsWith("//")) {
        // Include non-export, non-comment lines
        filteredLines.push(line);
      }
    }

    return filteredLines.join("\n");
  }

  /**
   * Extract function name from export line
   */
  private extractFunctionName(line: string): string | null {
    const match = line.match(/export\s+function\s+(\w+)/);
    return match ? match[1] : null;
  }

  /**
   * Apply alias to compiled code
   */
  private applyAlias(code: string, alias: string): string {
    // Wrap the code in a namespace with the alias
    return `
      const ${alias} = (function() {
        ${code}
        return { ${this.extractExportNames(code).join(", ")} };
      })();
    `;
  }

  /**
   * Extract export names from code
   */
  private extractExportNames(code: string): string[] {
    const exportMatches = code.match(/export\s+function\s+(\w+)/g) || [];
    return exportMatches.map((match) =>
      match.replace(/export\s+function\s+/, ""),
    );
  }

  /**
   * Add node to dependency graph
   */
  private addNodeToGraph(path: string, library: Library): void {
    if (!this.dependencyGraph.nodes.find((node) => node.id === path)) {
      this.dependencyGraph.nodes.push({
        id: path,
        path,
        library,
        visited: false,
        inStack: false,
      });
    }
  }

  /**
   * Add edge to dependency graph
   */
  private addEdgeToGraph(
    from: string,
    to: string,
    type: "direct" | "transitive",
  ): void {
    if (
      !this.dependencyGraph.edges.find(
        (edge) => edge.from === from && edge.to === to,
      )
    ) {
      this.dependencyGraph.edges.push({ from, to, type });
    }
  }

  /**
   * Detect circular dependencies using DFS with enhanced cycle detection
   */
  private detectCircularDependencies(): void {
    const visited = new Set<string>();
    const recursionStack = new Set<string>();
    const cycles: string[][] = [];

    // Reset node states
    for (const node of this.dependencyGraph.nodes) {
      node.visited = false;
      node.inStack = false;
    }

    // Run DFS from each unvisited node
    for (const node of this.dependencyGraph.nodes) {
      if (!visited.has(node.id)) {
        this.dfsDetectCycles(node.id, visited, recursionStack, [], cycles);
      }
    }

    this.dependencyGraph.circularDependencies = cycles;

    // Log detected cycles for debugging
    if (cycles.length > 0) {
      console.warn(`Detected ${cycles.length} circular dependencies:`, cycles);
    }
  }

  /**
   * DFS helper for cycle detection with enhanced tracking
   */
  private dfsDetectCycles(
    nodeId: string,
    visited: Set<string>,
    recursionStack: Set<string>,
    currentPath: string[],
    cycles: string[][],
  ): void {
    const node = this.dependencyGraph.nodes.find((n) => n.id === nodeId);
    if (!node) return;

    visited.add(nodeId);
    recursionStack.add(nodeId);
    currentPath.push(nodeId);

    node.visited = true;
    node.inStack = true;

    // Find all edges from this node
    const outgoingEdges = this.dependencyGraph.edges.filter(
      (edge) => edge.from === nodeId,
    );

    for (const edge of outgoingEdges) {
      const targetNode = this.dependencyGraph.nodes.find(
        (n) => n.id === edge.to,
      );

      if (!visited.has(edge.to)) {
        // Recursive case: visit unvisited node
        this.dfsDetectCycles(
          edge.to,
          visited,
          recursionStack,
          [...currentPath],
          cycles,
        );
      } else if (recursionStack.has(edge.to)) {
        // Found a back edge - this indicates a cycle
        const cycleStart = currentPath.indexOf(edge.to);
        if (cycleStart !== -1) {
          const cycle = currentPath.slice(cycleStart).concat([edge.to]);
          // Avoid duplicate cycles
          const cycleKey = cycle.sort().join("->");
          const existingCycle = cycles.find(
            (c) => c.sort().join("->") === cycleKey,
          );
          if (!existingCycle) {
            cycles.push(cycle);
          }
        }
      }
    }

    recursionStack.delete(nodeId);
    currentPath.pop();
    node.inStack = false;
  }

  /**
   * Get cached library
   */
  getCachedLibrary(path: string): Library | null {
    return this.libraryCache.get(path) || null;
  }

  /**
   * Invalidate cache for a specific path
   */
  invalidateCache(path: string): void {
    this.libraryCache.delete(path);
  }

  /**
   * Get dependency graph
   */
  getDependencyGraph(rootImports: string[]): DependencyGraph {
    return this.dependencyGraph;
  }

  /**
   * Get dependency resolution order using topological sort
   */
  getDependencyResolutionOrder(): string[] {
    if (this.dependencyGraph.circularDependencies.length > 0) {
      throw new Error(
        "Cannot determine resolution order due to circular dependencies",
      );
    }

    return this.topologicalSort();
  }

  /**
   * Perform topological sort on dependency graph
   */
  private topologicalSort(): string[] {
    const result: string[] = [];
    const visited = new Set<string>();
    const tempMark = new Set<string>();

    const visit = (nodeId: string): void => {
      if (tempMark.has(nodeId)) {
        throw new Error(`Circular dependency detected involving ${nodeId}`);
      }

      if (!visited.has(nodeId)) {
        tempMark.add(nodeId);

        // Visit all dependencies first
        const outgoingEdges = this.dependencyGraph.edges.filter(
          (edge) => edge.from === nodeId,
        );
        for (const edge of outgoingEdges) {
          visit(edge.to);
        }

        tempMark.delete(nodeId);
        visited.add(nodeId);
        result.unshift(nodeId); // Add to beginning for correct order
      }
    };

    // Visit all nodes
    for (const node of this.dependencyGraph.nodes) {
      if (!visited.has(node.id)) {
        visit(node.id);
      }
    }

    return result;
  }

  /**
   * Validate import specifications with comprehensive checks
   */
  validateImports(imports: ImportSpec[]): ImportValidationResult {
    const errors: ImportError[] = [];
    const warnings: string[] = [];
    const seenPaths = new Set<string>();
    const seenAliases = new Set<string>();

    for (const importSpec of imports) {
      try {
        // Validate path format
        if (!importSpec.path || importSpec.path.trim() === "") {
          errors.push(
            this.createImportError(
              "INVALID_PATH",
              importSpec.path,
              new Error("Empty path"),
            ),
          );
          continue;
        }

        // Validate path characters and format
        if (!this.isValidPath(importSpec.path)) {
          errors.push(
            this.createImportError(
              "INVALID_PATH",
              importSpec.path,
              new Error("Invalid path format or characters"),
            ),
          );
          continue;
        }

        // Validate import type
        if (!["relative", "absolute", "standard"].includes(importSpec.type)) {
          errors.push(
            this.createImportError(
              "INVALID_PATH",
              importSpec.path,
              new Error(`Invalid import type: ${importSpec.type}`),
            ),
          );
          continue;
        }

        // Validate path consistency with type
        const pathValidation = this.validatePathTypeConsistency(
          importSpec.path,
          importSpec.type,
        );
        if (!pathValidation.valid) {
          errors.push(
            this.createImportError(
              "INVALID_PATH",
              importSpec.path,
              new Error(pathValidation.error),
            ),
          );
          continue;
        }

        // Check for duplicate imports
        const resolvedPath = this.resolvePath(importSpec.path, importSpec.type);
        if (seenPaths.has(resolvedPath)) {
          warnings.push(
            `Duplicate import detected: ${importSpec.path} resolves to ${resolvedPath}`,
          );
        } else {
          seenPaths.add(resolvedPath);
        }

        // Validate selective imports
        if (importSpec.selective) {
          if (importSpec.selective.length === 0) {
            warnings.push(`Empty selective import list for ${importSpec.path}`);
          } else {
            // Validate function names in selective imports
            for (const funcName of importSpec.selective) {
              if (!this.isValidIdentifier(funcName)) {
                errors.push(
                  this.createImportError(
                    "INVALID_PATH",
                    importSpec.path,
                    new Error(
                      `Invalid function name in selective import: ${funcName}`,
                    ),
                  ),
                );
              }
            }

            // Check for duplicate function names in selective imports
            const uniqueFunctions = new Set(importSpec.selective);
            if (uniqueFunctions.size !== importSpec.selective.length) {
              warnings.push(
                `Duplicate function names in selective import for ${importSpec.path}`,
              );
            }
          }
        }

        // Validate alias
        if (importSpec.alias) {
          if (!this.isValidIdentifier(importSpec.alias)) {
            errors.push(
              this.createImportError(
                "INVALID_PATH",
                importSpec.path,
                new Error(`Invalid alias name: ${importSpec.alias}`),
              ),
            );
          } else if (seenAliases.has(importSpec.alias)) {
            errors.push(
              this.createImportError(
                "INVALID_PATH",
                importSpec.path,
                new Error(`Duplicate alias: ${importSpec.alias}`),
              ),
            );
          } else {
            seenAliases.add(importSpec.alias);
          }

          // Check if alias conflicts with selective imports
          if (
            importSpec.selective &&
            importSpec.selective.includes(importSpec.alias)
          ) {
            warnings.push(
              `Alias ${importSpec.alias} conflicts with selective import function name in ${importSpec.path}`,
            );
          }
        }

        // Validate file extension for qiqe files
        if (
          !importSpec.path.endsWith(".qq") &&
          importSpec.type !== "standard"
        ) {
          warnings.push(
            `Import ${importSpec.path} does not have .qq extension`,
          );
        }
      } catch (error) {
        errors.push(
          this.createImportError(
            "PARSE_ERROR",
            importSpec.path,
            error as Error,
          ),
        );
      }
    }

    return {
      valid: errors.length === 0,
      errors,
      warnings,
    };
  }

  /**
   * Validate that path format is consistent with import type
   */
  private validatePathTypeConsistency(
    path: string,
    type: ImportSpec["type"],
  ): { valid: boolean; error?: string } {
    switch (type) {
      case "absolute":
        if (!path.startsWith("/")) {
          return { valid: false, error: "Absolute path must start with /" };
        }
        break;

      case "relative":
        if (
          !path.startsWith("./") &&
          !path.startsWith("../") &&
          !path.includes("/")
        ) {
          // Allow simple relative paths without explicit './'
          return { valid: true };
        }
        if (path.startsWith("/")) {
          return { valid: false, error: "Relative path cannot start with /" };
        }
        break;

      case "standard":
        if (
          path.startsWith("/") ||
          path.startsWith("./") ||
          path.startsWith("../")
        ) {
          return {
            valid: false,
            error: "Standard library path should not contain path separators",
          };
        }
        break;
    }

    return { valid: true };
  }

  /**
   * Validate JavaScript identifier
   */
  private isValidIdentifier(name: string): boolean {
    // JavaScript identifier rules: start with letter, $, or _, followed by letters, digits, $, or _
    return (
      /^[a-zA-Z_$][a-zA-Z0-9_$]*$/.test(name) && !this.isReservedWord(name)
    );
  }

  /**
   * Check if name is a reserved JavaScript word
   */
  private isReservedWord(name: string): boolean {
    const reserved = [
      "break",
      "case",
      "catch",
      "class",
      "const",
      "continue",
      "debugger",
      "default",
      "delete",
      "do",
      "else",
      "export",
      "extends",
      "finally",
      "for",
      "function",
      "if",
      "import",
      "in",
      "instanceof",
      "new",
      "return",
      "super",
      "switch",
      "this",
      "throw",
      "try",
      "typeof",
      "var",
      "void",
      "while",
      "with",
      "yield",
      "let",
      "static",
      "enum",
      "implements",
      "package",
      "protected",
      "interface",
      "private",
      "public",
      "await",
      "async",
    ];
    return reserved.includes(name.toLowerCase());
  }

  /**
   * Create import error
   */
  private createImportError(
    type: ImportError["type"],
    path: string,
    cause: Error,
    dependencyChain?: string[],
  ): ImportError {
    const error = new Error(`Import error: ${cause.message}`) as ImportError;
    error.type = type;
    error.path = path;
    error.dependencyChain = dependencyChain;
    return error;
  }

  /**
   * Create circular dependency error
   */
  private createCircularDependencyError(cycle: string[]): ImportError {
    const error = new Error(
      `Circular dependency detected: ${cycle.join(" -> ")}`,
    ) as ImportError;
    error.type = "CIRCULAR_DEPENDENCY";
    error.path = cycle[0];
    error.dependencyChain = cycle;
    return error;
  }
}

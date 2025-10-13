/**
 * NamespaceManager - Handles selective imports and namespace management
 */

import { ImportSpec, ExportedFunction, Library } from "./types";

interface NamespaceEntry {
  name: string;
  originalName: string;
  library: string;
  type: "function" | "variable" | "class";
  visibility: "public" | "private";
}

interface Namespace {
  name: string;
  entries: Map<string, NamespaceEntry>;
  aliases: Map<string, string>;
  conflicts: ConflictInfo[];
}

interface ConflictInfo {
  name: string;
  sources: string[];
  resolution: "error" | "alias" | "override";
  resolvedName?: string;
}

interface SelectiveImportResult {
  selectedFunctions: ExportedFunction[];
  filteredCode: string;
  namespace: Namespace;
  conflicts: ConflictInfo[];
}

export class NamespaceManager {
  private namespaces = new Map<string, Namespace>();
  private globalNamespace: Namespace;

  constructor() {
    this.globalNamespace = {
      name: "global",
      entries: new Map(),
      aliases: new Map(),
      conflicts: [],
    };
    this.namespaces.set("global", this.globalNamespace);
  }

  /**
   * Process selective imports and create namespace
   */
  processSelectiveImports(
    library: Library,
    importSpec: ImportSpec,
  ): SelectiveImportResult {
    const namespace = this.createOrGetNamespace(
      importSpec.alias || library.name,
    );
    const selectedFunctions: ExportedFunction[] = [];
    const conflicts: ConflictInfo[] = [];

    // If no selective imports specified, import all public functions
    const functionsToImport =
      importSpec.selective && importSpec.selective.length > 0
        ? this.filterFunctionsByNames(library.exports, importSpec.selective)
        : library.exports.filter((fn) => fn.visibility === "public");

    // Process each function
    for (const func of functionsToImport) {
      const result = this.addFunctionToNamespace(namespace, func, library.path);

      if (result.success) {
        selectedFunctions.push(func);
      } else if (result.conflict) {
        conflicts.push(result.conflict);
      }
    }

    // Generate filtered code
    const filteredCode = this.generateFilteredCode(
      library,
      selectedFunctions,
      importSpec,
    );

    return {
      selectedFunctions,
      filteredCode,
      namespace,
      conflicts,
    };
  }

  /**
   * Filter functions by names
   */
  private filterFunctionsByNames(
    exports: ExportedFunction[],
    names: string[],
  ): ExportedFunction[] {
    return exports.filter((func) => names.includes(func.name));
  }

  /**
   * Add function to namespace with conflict detection
   */
  private addFunctionToNamespace(
    namespace: Namespace,
    func: ExportedFunction,
    libraryPath: string,
  ): { success: boolean; conflict?: ConflictInfo } {
    const existingEntry = namespace.entries.get(func.name);

    if (existingEntry) {
      // Conflict detected
      const conflict: ConflictInfo = {
        name: func.name,
        sources: [existingEntry.library, libraryPath],
        resolution: "error", // Default to error, can be resolved later
      };

      namespace.conflicts.push(conflict);
      return { success: false, conflict };
    }

    // Add to namespace
    const entry: NamespaceEntry = {
      name: func.name,
      originalName: func.name,
      library: libraryPath,
      type: "function",
      visibility: func.visibility,
    };

    namespace.entries.set(func.name, entry);
    return { success: true };
  }

  /**
   * Resolve naming conflicts
   */
  resolveConflicts(
    namespace: Namespace,
    resolutionStrategy: "auto" | "manual" = "auto",
  ): ConflictInfo[] {
    const resolvedConflicts: ConflictInfo[] = [];

    for (const conflict of namespace.conflicts) {
      if (resolutionStrategy === "auto") {
        const resolution = this.autoResolveConflict(namespace, conflict);
        resolvedConflicts.push(resolution);
      }
    }

    // Clear resolved conflicts
    namespace.conflicts = namespace.conflicts.filter(
      (conflict) =>
        !resolvedConflicts.some((resolved) => resolved.name === conflict.name),
    );

    return resolvedConflicts;
  }

  /**
   * Automatically resolve naming conflict
   */
  private autoResolveConflict(
    namespace: Namespace,
    conflict: ConflictInfo,
  ): ConflictInfo {
    // Strategy: Create aliases with library names
    const resolvedConflict: ConflictInfo = { ...conflict };

    for (let i = 0; i < conflict.sources.length; i++) {
      const source = conflict.sources[i];
      const libraryName = this.getLibraryNameFromPath(source);
      const aliasName = `${conflict.name}_${libraryName}`;

      // Check if alias is available
      if (!namespace.entries.has(aliasName)) {
        namespace.aliases.set(aliasName, conflict.name);
        resolvedConflict.resolution = "alias";
        resolvedConflict.resolvedName = aliasName;
        break;
      }
    }

    return resolvedConflict;
  }

  /**
   * Get library name from path
   */
  private getLibraryNameFromPath(path: string): string {
    return path.split("/").pop()?.replace(".qq", "") || "unknown";
  }

  /**
   * Create or get namespace
   */
  private createOrGetNamespace(name: string): Namespace {
    if (this.namespaces.has(name)) {
      return this.namespaces.get(name)!;
    }

    const namespace: Namespace = {
      name,
      entries: new Map(),
      aliases: new Map(),
      conflicts: [],
    };

    this.namespaces.set(name, namespace);
    return namespace;
  }

  /**
   * Generate filtered code for selective imports
   */
  private generateFilteredCode(
    library: Library,
    selectedFunctions: ExportedFunction[],
    importSpec: ImportSpec,
  ): string {
    const lines = library.compiledCode.split("\n");
    const filteredLines: string[] = [];
    const selectedNames = new Set(selectedFunctions.map((f) => f.name));

    // Add header comment
    filteredLines.push(`// Selective import from ${library.path}`);
    filteredLines.push(
      `// Selected functions: ${Array.from(selectedNames).join(", ")}`,
    );
    filteredLines.push("");

    // Process each line
    for (const line of lines) {
      const trimmedLine = line.trim();

      // Skip comments and empty lines in original
      if (trimmedLine.startsWith("//") || trimmedLine === "") {
        continue;
      }

      // Check if this is an export line
      if (trimmedLine.startsWith("export")) {
        const functionName = this.extractFunctionNameFromLine(line);

        if (functionName && selectedNames.has(functionName)) {
          // Include this export
          filteredLines.push(line);
        }
        // Skip exports not in selection
      } else {
        // Include non-export lines (helper functions, variables, etc.)
        // But only if they're referenced by selected functions
        if (this.isLineReferencedBySelectedFunctions(line, selectedFunctions)) {
          filteredLines.push(line);
        }
      }
    }

    // Apply namespace wrapping if alias is specified
    if (importSpec.alias) {
      return this.wrapInNamespace(
        filteredLines.join("\n"),
        importSpec.alias,
        selectedFunctions,
      );
    }

    return filteredLines.join("\n");
  }

  /**
   * Extract function name from export line
   */
  private extractFunctionNameFromLine(line: string): string | null {
    const patterns = [
      /export\s+function\s+(\w+)/,
      /export\s+const\s+(\w+)/,
      /export\s+let\s+(\w+)/,
      /export\s+var\s+(\w+)/,
      /export\s+class\s+(\w+)/,
    ];

    for (const pattern of patterns) {
      const match = line.match(pattern);
      if (match) {
        return match[1];
      }
    }

    return null;
  }

  /**
   * Check if a line is referenced by selected functions
   */
  private isLineReferencedBySelectedFunctions(
    line: string,
    selectedFunctions: ExportedFunction[],
  ): boolean {
    // Simple heuristic: include helper functions and variables
    // that might be used by exported functions
    const trimmedLine = line.trim();

    // Include function declarations (might be helper functions)
    if (
      trimmedLine.startsWith("function ") ||
      trimmedLine.startsWith("const ") ||
      trimmedLine.startsWith("let ") ||
      trimmedLine.startsWith("var ")
    ) {
      return true;
    }

    // Include class definitions
    if (trimmedLine.startsWith("class ")) {
      return true;
    }

    // Include other statements that might be dependencies
    return false;
  }

  /**
   * Wrap code in namespace
   */
  private wrapInNamespace(
    code: string,
    namespaceName: string,
    selectedFunctions: ExportedFunction[],
  ): string {
    const exportNames = selectedFunctions.map((f) => f.name);

    return `
// Namespace: ${namespaceName}
const ${namespaceName} = (function() {
  ${code}
  
  // Export selected functions
  return {
    ${exportNames.join(",\n    ")}
  };
})();
`;
  }

  /**
   * Merge multiple namespaces
   */
  mergeNamespaces(namespaceNames: string[], targetName: string): Namespace {
    const targetNamespace = this.createOrGetNamespace(targetName);
    const allConflicts: ConflictInfo[] = [];

    for (const namespaceName of namespaceNames) {
      const sourceNamespace = this.namespaces.get(namespaceName);
      if (!sourceNamespace) {
        continue;
      }

      // Merge entries
      for (const [name, entry] of Array.from(sourceNamespace.entries)) {
        const existingEntry = targetNamespace.entries.get(name);

        if (existingEntry && existingEntry.library !== entry.library) {
          // Conflict detected
          const conflict: ConflictInfo = {
            name,
            sources: [existingEntry.library, entry.library],
            resolution: "error",
          };
          allConflicts.push(conflict);
        } else {
          targetNamespace.entries.set(name, entry);
        }
      }

      // Merge aliases
      for (const [alias, original] of Array.from(sourceNamespace.aliases)) {
        targetNamespace.aliases.set(alias, original);
      }
    }

    targetNamespace.conflicts.push(...allConflicts);
    return targetNamespace;
  }

  /**
   * Get namespace by name
   */
  getNamespace(name: string): Namespace | null {
    return this.namespaces.get(name) || null;
  }

  /**
   * List all namespaces
   */
  listNamespaces(): string[] {
    return Array.from(this.namespaces.keys());
  }

  /**
   * Check if function exists in namespace
   */
  hasFunction(namespaceName: string, functionName: string): boolean {
    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      return false;
    }

    return (
      namespace.entries.has(functionName) || namespace.aliases.has(functionName)
    );
  }

  /**
   * Get function from namespace
   */
  getFunction(
    namespaceName: string,
    functionName: string,
  ): NamespaceEntry | null {
    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      return null;
    }

    // Check direct entry
    const directEntry = namespace.entries.get(functionName);
    if (directEntry) {
      return directEntry;
    }

    // Check alias
    const aliasTarget = namespace.aliases.get(functionName);
    if (aliasTarget) {
      return namespace.entries.get(aliasTarget) || null;
    }

    return null;
  }

  /**
   * Remove namespace
   */
  removeNamespace(name: string): boolean {
    if (name === "global") {
      return false; // Cannot remove global namespace
    }

    return this.namespaces.delete(name);
  }

  /**
   * Clear all namespaces except global
   */
  clearNamespaces(): void {
    const globalNs = this.namespaces.get("global");
    this.namespaces.clear();

    if (globalNs) {
      this.namespaces.set("global", globalNs);
    }
  }

  /**
   * Get namespace statistics
   */
  getNamespaceStats(namespaceName: string): {
    entryCount: number;
    aliasCount: number;
    conflictCount: number;
    libraries: string[];
  } | null {
    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      return null;
    }

    const libraries = new Set<string>();
    for (const entry of Array.from(namespace.entries.values())) {
      libraries.add(entry.library);
    }

    return {
      entryCount: namespace.entries.size,
      aliasCount: namespace.aliases.size,
      conflictCount: namespace.conflicts.length,
      libraries: Array.from(libraries),
    };
  }

  /**
   * Validate namespace name
   */
  validateNamespaceName(name: string): { valid: boolean; error?: string } {
    if (!name || name.trim() === "") {
      return { valid: false, error: "Namespace name cannot be empty" };
    }

    if (!/^[a-zA-Z_$][a-zA-Z0-9_$]*$/.test(name)) {
      return { valid: false, error: "Invalid namespace name format" };
    }

    if (name === "global") {
      return { valid: false, error: 'Cannot use reserved name "global"' };
    }

    return { valid: true };
  }

  /**
   * Generate namespace documentation
   */
  generateNamespaceDocumentation(namespaceName: string): string | null {
    const namespace = this.namespaces.get(namespaceName);
    if (!namespace) {
      return null;
    }

    const lines: string[] = [];
    lines.push(`# Namespace: ${namespaceName}`);
    lines.push("");

    // Functions
    if (namespace.entries.size > 0) {
      lines.push("## Functions");
      lines.push("");

      for (const [name, entry] of Array.from(namespace.entries)) {
        lines.push(`### ${name}`);
        lines.push(`- **Library**: ${entry.library}`);
        lines.push(`- **Type**: ${entry.type}`);
        lines.push(`- **Visibility**: ${entry.visibility}`);
        lines.push("");
      }
    }

    // Aliases
    if (namespace.aliases.size > 0) {
      lines.push("## Aliases");
      lines.push("");

      for (const [alias, original] of Array.from(namespace.aliases)) {
        lines.push(`- **${alias}** → ${original}`);
      }
      lines.push("");
    }

    // Conflicts
    if (namespace.conflicts.length > 0) {
      lines.push("## Conflicts");
      lines.push("");

      for (const conflict of namespace.conflicts) {
        lines.push(`### ${conflict.name}`);
        lines.push(`- **Sources**: ${conflict.sources.join(", ")}`);
        lines.push(`- **Resolution**: ${conflict.resolution}`);
        if (conflict.resolvedName) {
          lines.push(`- **Resolved Name**: ${conflict.resolvedName}`);
        }
        lines.push("");
      }
    }

    return lines.join("\n");
  }
}

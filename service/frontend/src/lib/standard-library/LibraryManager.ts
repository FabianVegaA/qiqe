/**
 * Standard Library Management System
 *
 * This module provides comprehensive library management capabilities including:
 * - Library metadata parsing and storage
 * - Version management and compatibility checking
 * - Function signature and documentation extraction
 * - Automatic standard library loading
 * - Function documentation and discovery
 */

import {
  Library,
  LibraryMetadata,
  ExportedFunction,
  Dependency,
} from "../import-system/types";

/**
 * Represents a qiqe library with enhanced metadata and management capabilities
 */
export class QiqeLibrary implements Library {
  public name: string;
  public version: string;
  public path: string;
  public exports: ExportedFunction[];
  public dependencies: Dependency[];
  public metadata: LibraryMetadata;
  public compiledCode: string;

  private _sourceCode: string;
  private _parsed: boolean = false;

  constructor(
    name: string,
    path: string,
    sourceCode: string,
    version: string = "1.0.0",
  ) {
    this.name = name;
    this.path = path;
    this.version = version;
    this._sourceCode = sourceCode;
    this.exports = [];
    this.dependencies = [];
    this.compiledCode = "";

    // Initialize metadata with defaults
    this.metadata = {
      author: "Unknown",
      description: "",
      tags: [],
      lastModified: new Date(),
      size: sourceCode.length,
      checksum: this.calculateChecksum(sourceCode),
    };

    // Parse the library on construction
    this.parseLibrary();
  }

  /**
   * Parse the library source code to extract metadata, functions, and documentation
   */
  private parseLibrary(): void {
    if (this._parsed) return;

    try {
      this.extractMetadata();
      this.extractFunctions();
      this.extractDependencies();
      this._parsed = true;
    } catch (error) {
      console.error(`Failed to parse library ${this.name}:`, error);
      throw new Error(
        `Library parsing failed: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Extract metadata from library comments and headers
   */
  private extractMetadata(): void {
    const lines = this._sourceCode.split("\n");
    let description = "";
    let author = "Unknown";
    const tags: string[] = [];

    // Look for metadata in comments at the top of the file
    for (let i = 0; i < Math.min(20, lines.length); i++) {
      const line = lines[i].trim();

      if (line.startsWith("# ") && !line.includes("Library")) {
        if (!description) {
          description = line.substring(2).trim();
        }
      }

      // Look for author information
      if (line.toLowerCase().includes("@author")) {
        const match = line.match(/@author\s+(.+)/i);
        if (match) {
          author = match[1].trim();
        }
      }

      // Look for tags
      if (line.toLowerCase().includes("@tags")) {
        const match = line.match(/@tags\s+(.+)/i);
        if (match) {
          tags.push(...match[1].split(",").map((tag) => tag.trim()));
        }
      }
    }

    this.metadata = {
      ...this.metadata,
      author,
      description: description || `${this.name} library`,
      tags: tags.length > 0 ? tags : ["standard", "library"],
    };
  }

  /**
   * Extract function definitions and their documentation
   */
  private extractFunctions(): void {
    const lines = this._sourceCode.split("\n");
    const functions: ExportedFunction[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();

      // Match function definitions: let functionName = ...
      const functionMatch = line.match(/^let\s+([a-zA-Z_][a-zA-Z0-9_']*)\s*=/);
      if (functionMatch) {
        const functionName = functionMatch[1];

        // Extract documentation from preceding comments
        const documentation = this.extractFunctionDocumentation(lines, i);

        // Extract function signature
        const signature = this.extractFunctionSignature(line);

        // Extract examples from documentation
        const examples = this.extractExamples(documentation);

        // Check if function is deprecated
        const deprecated = documentation.toLowerCase().includes("@deprecated");

        // Determine visibility (assume public unless marked private)
        const visibility = documentation.toLowerCase().includes("@private")
          ? "private"
          : "public";

        functions.push({
          name: functionName,
          signature,
          documentation: documentation || `Function: ${functionName}`,
          examples,
          deprecated,
          visibility,
        });
      }
    }

    this.exports = functions;
  }

  /**
   * Extract documentation for a function from preceding comment lines
   */
  private extractFunctionDocumentation(
    lines: string[],
    functionLineIndex: number,
  ): string {
    const docLines: string[] = [];
    let currentIndex = functionLineIndex - 1;

    // Look backwards for comment lines
    while (currentIndex >= 0) {
      const line = lines[currentIndex].trim();

      if (line.startsWith("#")) {
        docLines.unshift(line.substring(1).trim());
        currentIndex--;
      } else if (line === "") {
        // Skip empty lines
        currentIndex--;
      } else {
        // Stop at non-comment, non-empty line
        break;
      }
    }

    return docLines.join("\n").trim();
  }

  /**
   * Extract function signature from the function definition line
   */
  private extractFunctionSignature(line: string): string {
    // Extract the right side of the assignment
    const assignmentMatch = line.match(
      /^let\s+[a-zA-Z_][a-zA-Z0-9_']*\s*=\s*(.+)$/,
    );
    if (!assignmentMatch) return "unknown";

    const definition = assignmentMatch[1].trim();

    // Handle lambda functions
    if (definition.startsWith("\\")) {
      return this.parseLambdaSignature(definition);
    }

    // Handle eval expressions
    if (definition.startsWith("eval")) {
      return this.parseEvalSignature(definition);
    }

    // Handle function compositions and applications
    if (definition.includes(">>") || definition.includes("<<")) {
      return "composed function";
    }

    return "function";
  }

  /**
   * Parse lambda function signature
   */
  private parseLambdaSignature(definition: string): string {
    // Extract parameters from lambda: \x y z. body
    const lambdaMatch = definition.match(/^\\([^.]+)\./);
    if (lambdaMatch) {
      const params = lambdaMatch[1].trim().split(/\s+/);
      return `(${params.join(", ")}) -> result`;
    }
    return "lambda function";
  }

  /**
   * Parse eval expression signature
   */
  private parseEvalSignature(definition: string): string {
    // Try to extract JavaScript function signature from eval
    const evalMatch = definition.match(/eval\s+"([^"]+)"/);
    if (evalMatch) {
      const jsCode = evalMatch[1];
      // Look for arrow function pattern
      const arrowMatch = jsCode.match(/\(([^)]*)\)\s*=>/);
      if (arrowMatch) {
        const params = arrowMatch[1]
          .split(",")
          .map((p) => p.trim())
          .filter((p) => p);
        return `(${params.join(", ")}) -> result`;
      }
    }
    return "eval function";
  }

  /**
   * Extract examples from documentation
   */
  private extractExamples(documentation: string): string[] {
    const examples: string[] = [];
    const lines = documentation.split("\n");
    let inExample = false;
    let currentExample: string[] = [];

    for (const line of lines) {
      const trimmed = line.trim();

      if (trimmed.toLowerCase().includes("@example")) {
        inExample = true;
        currentExample = [];
        continue;
      }

      if (inExample) {
        if (trimmed === "" && currentExample.length > 0) {
          // End of example
          examples.push(currentExample.join("\n"));
          currentExample = [];
          inExample = false;
        } else if (trimmed !== "") {
          currentExample.push(trimmed);
        }
      }
    }

    // Add the last example if we were still in one
    if (inExample && currentExample.length > 0) {
      examples.push(currentExample.join("\n"));
    }

    return examples;
  }

  /**
   * Extract dependencies from import statements or eval expressions
   */
  private extractDependencies(): void {
    // For now, qiqe libraries don't have explicit dependencies
    // This could be extended to parse import statements if they're added to the language
    this.dependencies = [];
  }

  /**
   * Calculate checksum for the source code
   */
  private calculateChecksum(content: string): string {
    // Simple hash function for checksum
    let hash = 0;
    for (let i = 0; i < content.length; i++) {
      const char = content.charCodeAt(i);
      hash = (hash << 5) - hash + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return Math.abs(hash).toString(16);
  }

  /**
   * Get function by name
   */
  public getFunction(name: string): ExportedFunction | undefined {
    return this.exports.find((func) => func.name === name);
  }

  /**
   * Get all public functions
   */
  public getPublicFunctions(): ExportedFunction[] {
    return this.exports.filter((func) => func.visibility === "public");
  }

  /**
   * Search functions by name or documentation
   */
  public searchFunctions(query: string): ExportedFunction[] {
    const lowerQuery = query.toLowerCase();
    return this.exports.filter(
      (func) =>
        func.name.toLowerCase().includes(lowerQuery) ||
        func.documentation.toLowerCase().includes(lowerQuery) ||
        func.signature.toLowerCase().includes(lowerQuery),
    );
  }

  /**
   * Get functions by tag/category
   */
  public getFunctionsByCategory(category: string): ExportedFunction[] {
    const lowerCategory = category.toLowerCase();
    return this.exports.filter(
      (func) =>
        func.documentation.toLowerCase().includes(lowerCategory) ||
        this.metadata.tags.some((tag) =>
          tag.toLowerCase().includes(lowerCategory),
        ),
    );
  }

  /**
   * Check version compatibility
   */
  public isCompatibleWith(requiredVersion: string): boolean {
    return this.compareVersions(this.version, requiredVersion) >= 0;
  }

  /**
   * Compare two version strings
   */
  private compareVersions(version1: string, version2: string): number {
    const v1Parts = version1.split(".").map(Number);
    const v2Parts = version2.split(".").map(Number);

    for (let i = 0; i < Math.max(v1Parts.length, v2Parts.length); i++) {
      const v1Part = v1Parts[i] || 0;
      const v2Part = v2Parts[i] || 0;

      if (v1Part > v2Part) return 1;
      if (v1Part < v2Part) return -1;
    }

    return 0;
  }

  /**
   * Update library metadata
   */
  public updateMetadata(metadata: Partial<LibraryMetadata>): void {
    this.metadata = { ...this.metadata, ...metadata };
  }

  /**
   * Refresh library from source code
   */
  public refresh(newSourceCode?: string): void {
    if (newSourceCode) {
      this._sourceCode = newSourceCode;
      this.metadata.size = newSourceCode.length;
      this.metadata.checksum = this.calculateChecksum(newSourceCode);
      this.metadata.lastModified = new Date();
    }

    this._parsed = false;
    this.exports = [];
    this.dependencies = [];
    this.parseLibrary();
  }

  /**
   * Get library statistics
   */
  public getStats(): LibraryStats {
    return {
      name: this.name,
      version: this.version,
      functionCount: this.exports.length,
      publicFunctionCount: this.getPublicFunctions().length,
      size: this.metadata.size,
      lastModified: this.metadata.lastModified,
      hasDocumentation: this.exports.some(
        (func) => func.documentation.length > func.name.length + 10,
      ),
      hasExamples: this.exports.some((func) => func.examples.length > 0),
    };
  }
}

/**
 * Library statistics interface
 */
export interface LibraryStats {
  name: string;
  version: string;
  functionCount: number;
  publicFunctionCount: number;
  size: number;
  lastModified: Date;
  hasDocumentation: boolean;
  hasExamples: boolean;
}

/**
 * Enhanced LibraryMetadata class with additional functionality
 */
export class QiqeLibraryMetadata implements LibraryMetadata {
  public author: string;
  public description: string;
  public tags: string[];
  public lastModified: Date;
  public size: number;
  public checksum: string;

  // Additional metadata fields
  public license?: string;
  public homepage?: string;
  public repository?: string;
  public keywords?: string[];
  public contributors?: string[];

  constructor(
    metadata: LibraryMetadata & {
      license?: string;
      homepage?: string;
      repository?: string;
      keywords?: string[];
      contributors?: string[];
    },
  ) {
    this.author = metadata.author;
    this.description = metadata.description;
    this.tags = metadata.tags;
    this.lastModified = metadata.lastModified;
    this.size = metadata.size;
    this.checksum = metadata.checksum;
    this.license = metadata.license;
    this.homepage = metadata.homepage;
    this.repository = metadata.repository;
    this.keywords = metadata.keywords;
    this.contributors = metadata.contributors;
  }

  /**
   * Convert to JSON representation
   */
  public toJSON(): Record<string, any> {
    return {
      author: this.author,
      description: this.description,
      tags: this.tags,
      lastModified: this.lastModified.toISOString(),
      size: this.size,
      checksum: this.checksum,
      license: this.license,
      homepage: this.homepage,
      repository: this.repository,
      keywords: this.keywords,
      contributors: this.contributors,
    };
  }

  /**
   * Create from JSON representation
   */
  public static fromJSON(json: Record<string, any>): QiqeLibraryMetadata {
    return new QiqeLibraryMetadata({
      author: json.author || "Unknown",
      description: json.description || "",
      tags: json.tags || [],
      lastModified: json.lastModified
        ? new Date(json.lastModified)
        : new Date(),
      size: json.size || 0,
      checksum: json.checksum || "",
      license: json.license,
      homepage: json.homepage,
      repository: json.repository,
      keywords: json.keywords,
      contributors: json.contributors,
    });
  }

  /**
   * Validate metadata completeness
   */
  public validate(): { valid: boolean; issues: string[] } {
    const issues: string[] = [];

    if (!this.author || this.author === "Unknown") {
      issues.push("Author information is missing");
    }

    if (!this.description) {
      issues.push("Description is missing");
    }

    if (this.tags.length === 0) {
      issues.push("No tags specified");
    }

    if (!this.checksum) {
      issues.push("Checksum is missing");
    }

    return {
      valid: issues.length === 0,
      issues,
    };
  }
}

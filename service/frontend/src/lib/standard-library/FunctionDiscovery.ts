/**
 * Function Documentation and Discovery System
 *
 * This module provides comprehensive function discovery, documentation parsing,
 * and search capabilities for qiqe libraries.
 */

import { QiqeLibrary } from "./LibraryManager";
import { StandardLibraryLoader } from "./StandardLibraryLoader";
import { ExportedFunction } from "../import-system/types";

/**
 * Function signature information
 */
export interface FunctionSignature {
  name: string;
  parameters: Parameter[];
  returnType: string;
  description: string;
  examples: string[];
  category: string;
  complexity: "simple" | "moderate" | "complex";
  deprecated: boolean;
  since?: string;
  seeAlso?: string[];
}

/**
 * Function parameter information
 */
export interface Parameter {
  name: string;
  type: string;
  description: string;
  optional: boolean;
  defaultValue?: string;
}

/**
 * Search result for function discovery
 */
export interface FunctionSearchResult {
  function: ExportedFunction;
  library: QiqeLibrary;
  signature: FunctionSignature;
  relevanceScore: number;
  matchType: "name" | "description" | "parameter" | "example" | "category";
}

/**
 * Function category information
 */
export interface FunctionCategory {
  name: string;
  description: string;
  functions: ExportedFunction[];
  subcategories: string[];
}

/**
 * Documentation parsing options
 */
export interface DocumentationParseOptions {
  extractExamples: boolean;
  extractParameters: boolean;
  extractReturnType: boolean;
  extractComplexity: boolean;
  extractCategories: boolean;
}

/**
 * Default documentation parsing options
 */
export const DEFAULT_PARSE_OPTIONS: DocumentationParseOptions = {
  extractExamples: true,
  extractParameters: true,
  extractReturnType: true,
  extractComplexity: true,
  extractCategories: true,
};

/**
 * Function Discovery and Documentation System
 */
export class FunctionDiscoverySystem {
  private libraryLoader: StandardLibraryLoader;
  private functionSignatures: Map<string, FunctionSignature> = new Map();
  private categories: Map<string, FunctionCategory> = new Map();
  private searchIndex: Map<string, FunctionSearchResult[]> = new Map();
  private parseOptions: DocumentationParseOptions;

  constructor(
    libraryLoader: StandardLibraryLoader,
    parseOptions: Partial<DocumentationParseOptions> = {},
  ) {
    this.libraryLoader = libraryLoader;
    this.parseOptions = { ...DEFAULT_PARSE_OPTIONS, ...parseOptions };
    this.initializeCategories();
  }

  /**
   * Initialize predefined function categories
   */
  private initializeCategories(): void {
    const categories = [
      { name: "Basic", description: "Fundamental functions and utilities" },
      {
        name: "Arithmetic",
        description: "Mathematical operations and calculations",
      },
      { name: "Comparison", description: "Comparison and equality functions" },
      { name: "String", description: "String manipulation and processing" },
      { name: "List", description: "List operations and transformations" },
      { name: "Boolean", description: "Boolean logic operations" },
      { name: "Logging", description: "Debugging and logging utilities" },
      {
        name: "Higher-Order",
        description: "Functions that operate on other functions",
      },
      { name: "Utility", description: "General utility functions" },
    ];

    for (const category of categories) {
      this.categories.set(category.name, {
        ...category,
        functions: [],
        subcategories: [],
      });
    }
  }

  /**
   * Extract function signature from exported function
   */
  public extractFunctionSignature(
    func: ExportedFunction,
    library: QiqeLibrary,
  ): FunctionSignature {
    const cacheKey = `${library.name}:${func.name}`;

    if (this.functionSignatures.has(cacheKey)) {
      return this.functionSignatures.get(cacheKey)!;
    }

    const signature: FunctionSignature = {
      name: func.name,
      parameters: this.parseOptions.extractParameters
        ? this.extractParameters(func)
        : [],
      returnType: this.parseOptions.extractReturnType
        ? this.extractReturnType(func)
        : "unknown",
      description: this.extractDescription(func),
      examples: this.parseOptions.extractExamples ? func.examples : [],
      category: this.parseOptions.extractCategories
        ? this.extractCategory(func)
        : "Utility",
      complexity: this.parseOptions.extractComplexity
        ? this.extractComplexity(func)
        : "simple",
      deprecated: func.deprecated || false,
      since: this.extractSince(func),
      seeAlso: this.extractSeeAlso(func),
    };

    this.functionSignatures.set(cacheKey, signature);
    return signature;
  }

  /**
   * Extract function parameters from documentation
   */
  private extractParameters(func: ExportedFunction): Parameter[] {
    const parameters: Parameter[] = [];
    const lines = func.documentation.split("\n");

    for (const line of lines) {
      const paramMatch = line.match(/@param\s+(\w+):\s*(.+)/);
      if (paramMatch) {
        const [, name, description] = paramMatch;

        // Try to extract type information
        const typeMatch = description.match(/^(\w+)\s+(.+)/);
        const type = typeMatch ? typeMatch[1] : "any";
        const desc = typeMatch ? typeMatch[2] : description;

        parameters.push({
          name,
          type,
          description: desc,
          optional: desc.toLowerCase().includes("optional"),
          defaultValue: this.extractDefaultValue(desc),
        });
      }
    }

    // If no @param tags found, try to extract from signature
    if (parameters.length === 0) {
      return this.extractParametersFromSignature(func.signature);
    }

    return parameters;
  }

  /**
   * Extract parameters from function signature
   */
  private extractParametersFromSignature(signature: string): Parameter[] {
    const parameters: Parameter[] = [];

    // Handle lambda signatures: (param1, param2) -> result
    const lambdaMatch = signature.match(/\(([^)]+)\)\s*->/);
    if (lambdaMatch) {
      const paramString = lambdaMatch[1];
      const paramNames = paramString
        .split(",")
        .map((p) => p.trim())
        .filter((p) => p);

      for (const paramName of paramNames) {
        parameters.push({
          name: paramName,
          type: "any",
          description: `Parameter ${paramName}`,
          optional: false,
        });
      }
    }

    return parameters;
  }

  /**
   * Extract return type from documentation
   */
  private extractReturnType(func: ExportedFunction): string {
    const lines = func.documentation.split("\n");

    for (const line of lines) {
      const returnMatch = line.match(/@return:\s*(.+)/);
      if (returnMatch) {
        return returnMatch[1].trim();
      }
    }

    // Try to extract from signature
    const signatureMatch = func.signature.match(/->\s*(\w+)/);
    if (signatureMatch) {
      return signatureMatch[1];
    }

    return "unknown";
  }

  /**
   * Extract function description
   */
  private extractDescription(func: ExportedFunction): string {
    const lines = func.documentation.split("\n");
    const descriptionLines: string[] = [];

    for (const line of lines) {
      const trimmed = line.trim();

      // Skip empty lines and annotation lines
      if (!trimmed || trimmed.startsWith("@")) {
        continue;
      }

      // Stop at examples
      if (trimmed.toLowerCase().includes("example")) {
        break;
      }

      descriptionLines.push(trimmed);
    }

    return descriptionLines.join(" ").trim() || `Function: ${func.name}`;
  }

  /**
   * Extract function category
   */
  private extractCategory(func: ExportedFunction): string {
    const doc = func.documentation.toLowerCase();
    const name = func.name.toLowerCase();

    // Check for explicit category annotation
    const categoryMatch = func.documentation.match(/@category\s+(\w+)/i);
    if (categoryMatch) {
      return categoryMatch[1];
    }

    // Infer category from function name and documentation
    if (
      name.includes("add") ||
      name.includes("sub") ||
      name.includes("mul") ||
      name.includes("div") ||
      name.includes("mod") ||
      name.includes("pow") ||
      doc.includes("arithmetic") ||
      doc.includes("mathematical")
    ) {
      return "Arithmetic";
    }

    if (
      name.includes("eq") ||
      name.includes("lt") ||
      name.includes("gt") ||
      name.includes("compare") ||
      doc.includes("comparison")
    ) {
      return "Comparison";
    }

    if (
      name.includes("str") ||
      name.includes("concat") ||
      name.includes("length") ||
      name.includes("upper") ||
      name.includes("lower") ||
      doc.includes("string")
    ) {
      return "String";
    }

    if (
      name.includes("list") ||
      name.includes("map") ||
      name.includes("filter") ||
      name.includes("fold") ||
      name.includes("cons") ||
      doc.includes("list")
    ) {
      return "List";
    }

    if (
      name.includes("and") ||
      name.includes("or") ||
      name.includes("not") ||
      doc.includes("boolean") ||
      doc.includes("logic")
    ) {
      return "Boolean";
    }

    if (
      name.includes("debug") ||
      name.includes("info") ||
      name.includes("warn") ||
      name.includes("error") ||
      name.includes("log")
    ) {
      return "Logging";
    }

    if (
      name.includes("map") ||
      name.includes("filter") ||
      name.includes("fold") ||
      name.includes("compose") ||
      doc.includes("higher-order")
    ) {
      return "Higher-Order";
    }

    if (name === "id" || name === "const" || name === "flip") {
      return "Basic";
    }

    return "Utility";
  }

  /**
   * Extract function complexity
   */
  private extractComplexity(
    func: ExportedFunction,
  ): "simple" | "moderate" | "complex" {
    const doc = func.documentation.toLowerCase();

    // Check for explicit complexity annotation
    if (doc.includes("@complexity simple")) return "simple";
    if (doc.includes("@complexity moderate")) return "moderate";
    if (doc.includes("@complexity complex")) return "complex";

    // Infer complexity from signature and documentation
    const paramCount = (func.signature.match(/\w+/g) || []).length;
    const docLength = func.documentation.length;
    const hasExamples = func.examples.length > 0;

    if (paramCount <= 2 && docLength < 200 && !hasExamples) {
      return "simple";
    }

    if (paramCount <= 4 && docLength < 500) {
      return "moderate";
    }

    return "complex";
  }

  /**
   * Extract since version
   */
  private extractSince(func: ExportedFunction): string | undefined {
    const sinceMatch = func.documentation.match(/@since\s+([\d.]+)/i);
    return sinceMatch ? sinceMatch[1] : undefined;
  }

  /**
   * Extract see also references
   */
  private extractSeeAlso(func: ExportedFunction): string[] {
    const seeAlsoMatch = func.documentation.match(/@see\s+(.+)/i);
    if (seeAlsoMatch) {
      return seeAlsoMatch[1].split(",").map((ref) => ref.trim());
    }
    return [];
  }

  /**
   * Extract default value from parameter description
   */
  private extractDefaultValue(description: string): string | undefined {
    const defaultMatch = description.match(/default[:\s]+([^,\s]+)/i);
    return defaultMatch ? defaultMatch[1] : undefined;
  }

  /**
   * Search functions by query
   */
  public searchFunctions(
    query: string,
    options: {
      libraries?: string[];
      categories?: string[];
      maxResults?: number;
      includeDeprecated?: boolean;
    } = {},
  ): FunctionSearchResult[] {
    const {
      libraries = [],
      categories = [],
      maxResults = 50,
      includeDeprecated = false,
    } = options;

    const results: FunctionSearchResult[] = [];
    const lowerQuery = query.toLowerCase();

    // Get libraries to search
    const librariesToSearch =
      libraries.length > 0
        ? (libraries
            .map((name) => this.libraryLoader.getLibrary(name))
            .filter(Boolean) as QiqeLibrary[])
        : this.libraryLoader.getLoadedLibraries();

    for (const library of librariesToSearch) {
      for (const func of library.exports) {
        // Skip deprecated functions if not included
        if (func.deprecated && !includeDeprecated) {
          continue;
        }

        const signature = this.extractFunctionSignature(func, library);

        // Skip functions not in specified categories
        if (categories.length > 0 && !categories.includes(signature.category)) {
          continue;
        }

        const relevanceScore = this.calculateRelevanceScore(
          query,
          func,
          signature,
        );

        if (relevanceScore > 0) {
          const matchType = this.determineMatchType(query, func, signature);

          results.push({
            function: func,
            library,
            signature,
            relevanceScore,
            matchType,
          });
        }
      }
    }

    // Sort by relevance score (descending)
    results.sort((a, b) => b.relevanceScore - a.relevanceScore);

    return results.slice(0, maxResults);
  }

  /**
   * Calculate relevance score for search query
   */
  private calculateRelevanceScore(
    query: string,
    func: ExportedFunction,
    signature: FunctionSignature,
  ): number {
    const lowerQuery = query.toLowerCase();
    let score = 0;

    // Exact name match gets highest score
    if (func.name.toLowerCase() === lowerQuery) {
      score += 100;
    }
    // Name starts with query
    else if (func.name.toLowerCase().startsWith(lowerQuery)) {
      score += 80;
    }
    // Name contains query
    else if (func.name.toLowerCase().includes(lowerQuery)) {
      score += 60;
    }

    // Description matches
    if (signature.description.toLowerCase().includes(lowerQuery)) {
      score += 40;
    }

    // Parameter matches
    for (const param of signature.parameters) {
      if (
        param.name.toLowerCase().includes(lowerQuery) ||
        param.description.toLowerCase().includes(lowerQuery)
      ) {
        score += 20;
      }
    }

    // Example matches
    for (const example of signature.examples) {
      if (example.toLowerCase().includes(lowerQuery)) {
        score += 15;
      }
    }

    // Category matches
    if (signature.category.toLowerCase().includes(lowerQuery)) {
      score += 10;
    }

    return score;
  }

  /**
   * Determine the type of match for search result
   */
  private determineMatchType(
    query: string,
    func: ExportedFunction,
    signature: FunctionSignature,
  ): "name" | "description" | "parameter" | "example" | "category" {
    const lowerQuery = query.toLowerCase();

    if (func.name.toLowerCase().includes(lowerQuery)) {
      return "name";
    }

    if (signature.description.toLowerCase().includes(lowerQuery)) {
      return "description";
    }

    for (const param of signature.parameters) {
      if (
        param.name.toLowerCase().includes(lowerQuery) ||
        param.description.toLowerCase().includes(lowerQuery)
      ) {
        return "parameter";
      }
    }

    for (const example of signature.examples) {
      if (example.toLowerCase().includes(lowerQuery)) {
        return "example";
      }
    }

    if (signature.category.toLowerCase().includes(lowerQuery)) {
      return "category";
    }

    return "description";
  }

  /**
   * Get functions by category
   */
  public getFunctionsByCategory(categoryName: string): FunctionSearchResult[] {
    const results: FunctionSearchResult[] = [];
    const libraries = this.libraryLoader.getLoadedLibraries();

    for (const library of libraries) {
      for (const func of library.exports) {
        const signature = this.extractFunctionSignature(func, library);

        if (signature.category === categoryName) {
          results.push({
            function: func,
            library,
            signature,
            relevanceScore: 100,
            matchType: "category",
          });
        }
      }
    }

    return results.sort((a, b) =>
      a.function.name.localeCompare(b.function.name),
    );
  }

  /**
   * Get all available categories
   */
  public getCategories(): FunctionCategory[] {
    const categories = new Map<string, FunctionCategory>();
    const libraries = this.libraryLoader.getLoadedLibraries();

    // Initialize categories
    this.categories.forEach((category, name) => {
      categories.set(name, { ...category, functions: [] });
    });

    // Populate categories with functions
    for (const library of libraries) {
      for (const func of library.exports) {
        const signature = this.extractFunctionSignature(func, library);
        const category = categories.get(signature.category);

        if (category) {
          category.functions.push(func);
        } else {
          // Create new category if not exists
          categories.set(signature.category, {
            name: signature.category,
            description: `${signature.category} functions`,
            functions: [func],
            subcategories: [],
          });
        }
      }
    }

    return Array.from(categories.values()).filter(
      (cat) => cat.functions.length > 0,
    );
  }

  /**
   * Get function documentation
   */
  public getFunctionDocumentation(
    functionName: string,
    libraryName?: string,
  ): FunctionSignature | null {
    const libraries = libraryName
      ? ([this.libraryLoader.getLibrary(libraryName)].filter(
          Boolean,
        ) as QiqeLibrary[])
      : this.libraryLoader.getLoadedLibraries();

    for (const library of libraries) {
      const func = library.getFunction(functionName);
      if (func) {
        return this.extractFunctionSignature(func, library);
      }
    }

    return null;
  }

  /**
   * Get similar functions
   */
  public getSimilarFunctions(
    functionName: string,
    maxResults: number = 5,
  ): FunctionSearchResult[] {
    const targetFunc = this.getFunctionDocumentation(functionName);
    if (!targetFunc) return [];

    const results: FunctionSearchResult[] = [];
    const libraries = this.libraryLoader.getLoadedLibraries();

    for (const library of libraries) {
      for (const func of library.exports) {
        if (func.name === functionName) continue;

        const signature = this.extractFunctionSignature(func, library);
        const similarity = this.calculateSimilarity(targetFunc, signature);

        if (similarity > 0.3) {
          results.push({
            function: func,
            library,
            signature,
            relevanceScore: similarity * 100,
            matchType: "category",
          });
        }
      }
    }

    return results
      .sort((a, b) => b.relevanceScore - a.relevanceScore)
      .slice(0, maxResults);
  }

  /**
   * Calculate similarity between two function signatures
   */
  private calculateSimilarity(
    sig1: FunctionSignature,
    sig2: FunctionSignature,
  ): number {
    let similarity = 0;

    // Category similarity
    if (sig1.category === sig2.category) {
      similarity += 0.4;
    }

    // Parameter count similarity
    const paramDiff = Math.abs(sig1.parameters.length - sig2.parameters.length);
    similarity += Math.max(0, 0.3 - paramDiff * 0.1);

    // Description similarity (simple word overlap)
    const words1 = sig1.description.toLowerCase().split(/\s+/);
    const words2 = sig2.description.toLowerCase().split(/\s+/);
    const commonWords = words1.filter((word) => words2.includes(word));
    similarity +=
      (commonWords.length / Math.max(words1.length, words2.length)) * 0.3;

    return Math.min(1, similarity);
  }

  /**
   * Get discovery statistics
   */
  public getStats(): DiscoveryStats {
    const libraries = this.libraryLoader.getLoadedLibraries();
    const totalFunctions = libraries.reduce(
      (sum, lib) => sum + lib.exports.length,
      0,
    );
    const categories = this.getCategories();

    return {
      totalLibraries: libraries.length,
      totalFunctions,
      totalCategories: categories.length,
      functionsWithDocumentation: this.functionSignatures.size,
      functionsWithExamples: Array.from(
        this.functionSignatures.values(),
      ).filter((sig) => sig.examples.length > 0).length,
      deprecatedFunctions: Array.from(this.functionSignatures.values()).filter(
        (sig) => sig.deprecated,
      ).length,
    };
  }
}

/**
 * Discovery system statistics
 */
export interface DiscoveryStats {
  totalLibraries: number;
  totalFunctions: number;
  totalCategories: number;
  functionsWithDocumentation: number;
  functionsWithExamples: number;
  deprecatedFunctions: number;
}

/**
 * Default value extraction from parameter description
 */
export function extractDefaultValue(description: string): string | undefined {
  const patterns = [
    /default[:\s]+([^,\s]+)/i,
    /defaults?\s+to\s+([^,\s]+)/i,
    /\(default:\s*([^)]+)\)/i,
  ];

  for (const pattern of patterns) {
    const match = description.match(pattern);
    if (match) {
      return match[1].trim();
    }
  }

  return undefined;
}

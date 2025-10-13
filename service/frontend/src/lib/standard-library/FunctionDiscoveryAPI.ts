/**
 * Function Discovery API
 *
 * This module provides a high-level API for function discovery and documentation
 * that can be easily integrated into the frontend UI components.
 */

import {
  FunctionDiscoverySystem,
  FunctionSearchResult,
  FunctionSignature,
  FunctionCategory,
} from "./FunctionDiscovery";
import {
  StandardLibraryLoader,
  getStandardLibraryLoader,
} from "./StandardLibraryLoader";
import { QiqeLibrary } from "./LibraryManager";
import { ExportedFunction } from "../import-system/types";

/**
 * Search options for function discovery
 */
export interface FunctionSearchOptions {
  query: string;
  libraries?: string[];
  categories?: string[];
  maxResults?: number;
  includeDeprecated?: boolean;
  sortBy?: "relevance" | "name" | "category";
}

/**
 * Function documentation with enhanced metadata
 */
export interface EnhancedFunctionDoc extends FunctionSignature {
  library: string;
  libraryVersion: string;
  usage: string;
  relatedFunctions: string[];
  tags: string[];
}

/**
 * Category information with statistics
 */
export interface CategoryInfo extends FunctionCategory {
  functionCount: number;
  exampleCount: number;
  documentedCount: number;
}

/**
 * Discovery system statistics
 */
export interface DiscoverySystemStats {
  totalLibraries: number;
  totalFunctions: number;
  totalCategories: number;
  functionsWithDocumentation: number;
  functionsWithExamples: number;
  deprecatedFunctions: number;
  averageFunctionsPerCategory: number;
  documentationCoverage: number;
}

/**
 * Function Discovery API class
 */
export class FunctionDiscoveryAPI {
  private discoverySystem: FunctionDiscoverySystem;
  private libraryLoader: StandardLibraryLoader;
  private initialized: boolean = false;

  constructor(libraryLoader?: StandardLibraryLoader) {
    this.libraryLoader = libraryLoader || getStandardLibraryLoader();
    this.discoverySystem = new FunctionDiscoverySystem(this.libraryLoader);
  }

  /**
   * Initialize the discovery API
   */
  public async initialize(): Promise<void> {
    if (this.initialized) return;

    try {
      await this.libraryLoader.initialize();
      this.initialized = true;
    } catch (error) {
      throw new Error(
        `Failed to initialize Function Discovery API: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }

  /**
   * Search for functions with enhanced options
   */
  public async searchFunctions(
    options: FunctionSearchOptions,
  ): Promise<FunctionSearchResult[]> {
    await this.ensureInitialized();

    const {
      query,
      libraries = [],
      categories = [],
      maxResults = 20,
      includeDeprecated = false,
      sortBy = "relevance",
    } = options;

    let results = this.discoverySystem.searchFunctions(query, {
      libraries,
      categories,
      maxResults: maxResults * 2, // Get more results for sorting
      includeDeprecated,
    });

    // Apply custom sorting
    if (sortBy === "name") {
      results.sort((a, b) => a.function.name.localeCompare(b.function.name));
    } else if (sortBy === "category") {
      results.sort((a, b) => {
        const categoryCompare = a.signature.category.localeCompare(
          b.signature.category,
        );
        return categoryCompare !== 0
          ? categoryCompare
          : a.function.name.localeCompare(b.function.name);
      });
    }
    // 'relevance' is already sorted by the discovery system

    return results.slice(0, maxResults);
  }

  /**
   * Get enhanced function documentation
   */
  public async getFunctionDocumentation(
    functionName: string,
    libraryName?: string,
  ): Promise<EnhancedFunctionDoc | null> {
    await this.ensureInitialized();

    const signature = this.discoverySystem.getFunctionDocumentation(
      functionName,
      libraryName,
    );
    if (!signature) return null;

    // Find the library containing this function
    const libraries = libraryName
      ? ([this.libraryLoader.getLibrary(libraryName)].filter(
          Boolean,
        ) as QiqeLibrary[])
      : this.libraryLoader.getLoadedLibraries();

    let targetLibrary: QiqeLibrary | null = null;
    for (const library of libraries) {
      if (library.getFunction(functionName)) {
        targetLibrary = library;
        break;
      }
    }

    if (!targetLibrary) return null;

    // Get similar functions for related functions
    const similarFunctions = this.discoverySystem.getSimilarFunctions(
      functionName,
      5,
    );
    const relatedFunctions = similarFunctions.map(
      (result) => result.function.name,
    );

    // Generate usage examples
    const usage = this.generateUsageExample(signature);

    // Extract tags from documentation and category
    const tags = this.extractTags(signature);

    return {
      ...signature,
      library: targetLibrary.name,
      libraryVersion: targetLibrary.version,
      usage,
      relatedFunctions,
      tags,
    };
  }

  /**
   * Get all available categories with statistics
   */
  public async getCategories(): Promise<CategoryInfo[]> {
    await this.ensureInitialized();

    const categories = this.discoverySystem.getCategories();

    return categories.map((category) => ({
      ...category,
      functionCount: category.functions.length,
      exampleCount: category.functions.reduce(
        (sum, func) => sum + func.examples.length,
        0,
      ),
      documentedCount: category.functions.filter(
        (func) => func.documentation.length > func.name.length + 10,
      ).length,
    }));
  }

  /**
   * Get functions by category with enhanced information
   */
  public async getFunctionsByCategory(
    categoryName: string,
  ): Promise<FunctionSearchResult[]> {
    await this.ensureInitialized();

    return this.discoverySystem.getFunctionsByCategory(categoryName);
  }

  /**
   * Get function suggestions based on partial input
   */
  public async getFunctionSuggestions(
    partialName: string,
    maxSuggestions: number = 10,
  ): Promise<string[]> {
    await this.ensureInitialized();

    const allLibraries = this.libraryLoader.getLoadedLibraries();
    const suggestions: string[] = [];
    const lowerPartial = partialName.toLowerCase();

    for (const library of allLibraries) {
      for (const func of library.exports) {
        if (func.name.toLowerCase().startsWith(lowerPartial)) {
          suggestions.push(func.name);
        }
      }
    }

    // Remove duplicates and sort
    const uniqueSuggestions = Array.from(new Set(suggestions));
    uniqueSuggestions.sort();

    return uniqueSuggestions.slice(0, maxSuggestions);
  }

  /**
   * Get popular functions (most commonly used or well-documented)
   */
  public async getPopularFunctions(
    maxResults: number = 10,
  ): Promise<FunctionSearchResult[]> {
    await this.ensureInitialized();

    const allLibraries = this.libraryLoader.getLoadedLibraries();
    const functionResults: FunctionSearchResult[] = [];

    for (const library of allLibraries) {
      for (const func of library.exports) {
        const signature = this.discoverySystem.extractFunctionSignature(
          func,
          library,
        );

        // Calculate popularity score based on documentation quality and examples
        let popularityScore = 0;

        // Functions with examples are more popular
        popularityScore += signature.examples.length * 20;

        // Functions with good documentation are more popular
        if (signature.description.length > func.name.length + 20) {
          popularityScore += 30;
        }

        // Functions with parameters documented are more popular
        popularityScore += signature.parameters.length * 10;

        // Basic functions are generally popular
        if (
          ["Basic", "Arithmetic", "Comparison"].includes(signature.category)
        ) {
          popularityScore += 25;
        }

        // Common function names are popular
        const commonNames = [
          "map",
          "filter",
          "add",
          "show",
          "head",
          "tail",
          "cons",
        ];
        if (commonNames.includes(func.name)) {
          popularityScore += 40;
        }

        if (popularityScore > 0) {
          functionResults.push({
            function: func,
            library,
            signature,
            relevanceScore: popularityScore,
            matchType: "name",
          });
        }
      }
    }

    return functionResults
      .sort((a, b) => b.relevanceScore - a.relevanceScore)
      .slice(0, maxResults);
  }

  /**
   * Get recently added functions (based on library version or metadata)
   */
  public async getRecentFunctions(
    maxResults: number = 10,
  ): Promise<FunctionSearchResult[]> {
    await this.ensureInitialized();

    const allLibraries = this.libraryLoader.getLoadedLibraries();
    const functionResults: FunctionSearchResult[] = [];

    for (const library of allLibraries) {
      for (const func of library.exports) {
        const signature = this.discoverySystem.extractFunctionSignature(
          func,
          library,
        );

        // For now, we'll use the library's last modified date as a proxy for recency
        const recencyScore = library.metadata.lastModified.getTime();

        functionResults.push({
          function: func,
          library,
          signature,
          relevanceScore: recencyScore,
          matchType: "name",
        });
      }
    }

    return functionResults
      .sort((a, b) => b.relevanceScore - a.relevanceScore)
      .slice(0, maxResults);
  }

  /**
   * Get comprehensive discovery system statistics
   */
  public async getStats(): Promise<DiscoverySystemStats> {
    await this.ensureInitialized();

    const basicStats = this.discoverySystem.getStats();
    const categories = await this.getCategories();

    const averageFunctionsPerCategory =
      categories.length > 0
        ? Math.round(basicStats.totalFunctions / categories.length)
        : 0;

    const documentationCoverage =
      basicStats.totalFunctions > 0
        ? Math.round(
            (basicStats.functionsWithDocumentation /
              basicStats.totalFunctions) *
              100,
          )
        : 0;

    return {
      ...basicStats,
      averageFunctionsPerCategory,
      documentationCoverage,
    };
  }

  /**
   * Validate function name and provide suggestions
   */
  public async validateFunctionName(functionName: string): Promise<{
    exists: boolean;
    suggestions: string[];
    exactMatch?: EnhancedFunctionDoc;
  }> {
    await this.ensureInitialized();

    const exactMatch = await this.getFunctionDocumentation(functionName);
    const suggestions = await this.getFunctionSuggestions(functionName, 5);

    return {
      exists: exactMatch !== null,
      suggestions,
      exactMatch: exactMatch || undefined,
    };
  }

  /**
   * Get function usage patterns and examples
   */
  public async getFunctionUsagePatterns(functionName: string): Promise<{
    basicUsage: string;
    advancedUsage: string[];
    commonPatterns: string[];
    relatedFunctions: string[];
  } | null> {
    await this.ensureInitialized();

    const doc = await this.getFunctionDocumentation(functionName);
    if (!doc) return null;

    const basicUsage = this.generateBasicUsage(doc);
    const advancedUsage = this.generateAdvancedUsage(doc);
    const commonPatterns = this.generateCommonPatterns(doc);

    return {
      basicUsage,
      advancedUsage,
      commonPatterns,
      relatedFunctions: doc.relatedFunctions,
    };
  }

  /**
   * Ensure the API is initialized
   */
  private async ensureInitialized(): Promise<void> {
    if (!this.initialized) {
      await this.initialize();
    }
  }

  /**
   * Generate usage example for a function
   */
  private generateUsageExample(signature: FunctionSignature): string {
    if (signature.examples.length > 0) {
      return signature.examples[0];
    }

    // Generate basic usage based on signature
    const params = signature.parameters.map((p) => p.name).join(" ");
    return `${signature.name} ${params}`;
  }

  /**
   * Extract tags from function signature and documentation
   */
  private extractTags(signature: FunctionSignature): string[] {
    const tags: string[] = [];

    // Add category as a tag
    tags.push(signature.category.toLowerCase());

    // Add complexity as a tag
    tags.push(signature.complexity);

    // Add tags based on function characteristics
    if (signature.parameters.length === 0) {
      tags.push("no-params");
    } else if (signature.parameters.length > 3) {
      tags.push("many-params");
    }

    if (signature.examples.length > 0) {
      tags.push("has-examples");
    }

    if (signature.deprecated) {
      tags.push("deprecated");
    }

    // Extract tags from documentation
    const docTags = signature.description
      .toLowerCase()
      .match(/\b(recursive|pure|side-effect|async|sync)\b/g);
    if (docTags) {
      tags.push(...docTags);
    }

    return Array.from(new Set(tags));
  }

  /**
   * Generate basic usage pattern
   */
  private generateBasicUsage(doc: EnhancedFunctionDoc): string {
    const params = doc.parameters.map((p) => `<${p.name}>`).join(" ");
    return `${doc.name} ${params}`;
  }

  /**
   * Generate advanced usage patterns
   */
  private generateAdvancedUsage(doc: EnhancedFunctionDoc): string[] {
    const patterns: string[] = [];

    // Add examples as advanced usage
    patterns.push(...doc.examples);

    // Generate composition patterns for higher-order functions
    if (
      doc.category === "Higher-Order" ||
      doc.parameters.some((p) => p.type === "function")
    ) {
      patterns.push(`${doc.name} (\\x. x + 1) [1, 2, 3]`);
    }

    return patterns;
  }

  /**
   * Generate common usage patterns
   */
  private generateCommonPatterns(doc: EnhancedFunctionDoc): string[] {
    const patterns: string[] = [];

    // Add category-specific patterns
    switch (doc.category) {
      case "List":
        patterns.push(`${doc.name} someList`);
        patterns.push(`${doc.name} [] # empty list`);
        break;
      case "Arithmetic":
        patterns.push(`${doc.name} 1 2`);
        patterns.push(`${doc.name} x y`);
        break;
      case "String":
        patterns.push(`${doc.name} "hello"`);
        patterns.push(`${doc.name} someString`);
        break;
    }

    return patterns;
  }
}

/**
 * Global instance of the Function Discovery API
 */
let globalDiscoveryAPI: FunctionDiscoveryAPI | null = null;

/**
 * Get the global Function Discovery API instance
 */
export function getFunctionDiscoveryAPI(): FunctionDiscoveryAPI {
  if (!globalDiscoveryAPI) {
    globalDiscoveryAPI = new FunctionDiscoveryAPI();
  }
  return globalDiscoveryAPI;
}

/**
 * Initialize the global Function Discovery API
 */
export async function initializeFunctionDiscoveryAPI(): Promise<FunctionDiscoveryAPI> {
  const api = getFunctionDiscoveryAPI();
  await api.initialize();
  return api;
}

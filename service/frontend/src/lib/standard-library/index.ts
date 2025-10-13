/**
 * Standard Library Management System
 *
 * This module provides comprehensive standard library management for qiqe,
 * including library loading, function discovery, and documentation systems.
 */

// Library Management
export {
  QiqeLibrary,
  QiqeLibraryMetadata,
  type LibraryStats,
} from "./LibraryManager";

// Standard Library Loading
export {
  StandardLibraryLoader,
  getStandardLibraryLoader,
  initializeStandardLibrary,
  getStandardLibrary,
  DEFAULT_LOADER_CONFIG,
} from "./StandardLibraryLoader";
export type {
  LibraryLoaderConfig,
  LibraryLoadResult,
  LoaderStats,
} from "./StandardLibraryLoader";

// Function Discovery
export {
  FunctionDiscoverySystem,
  extractDefaultValue,
  DEFAULT_PARSE_OPTIONS,
} from "./FunctionDiscovery";
export type {
  FunctionSignature,
  Parameter,
  FunctionSearchResult,
  FunctionCategory,
  DocumentationParseOptions,
  DiscoveryStats,
} from "./FunctionDiscovery";

// Function Discovery API
export {
  FunctionDiscoveryAPI,
  type FunctionSearchOptions,
  type EnhancedFunctionDoc,
  type CategoryInfo,
  type DiscoverySystemStats,
  getFunctionDiscoveryAPI,
  initializeFunctionDiscoveryAPI,
} from "./FunctionDiscoveryAPI";

// Re-export types from import system for convenience
export type {
  Library,
  LibraryMetadata,
  ExportedFunction,
  Dependency,
} from "../import-system/types";

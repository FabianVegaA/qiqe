/**
 * SourceMapBuilder for creating and managing source maps between qiqe and JavaScript code
 */

import {
  SourceMap,
  LocationMapping,
  FunctionMapping,
  QiqeLocation,
  JSLocation,
} from "./types";

export class SourceMapBuilder {
  private qiqeToJs: LocationMapping[] = [];
  private jsToQiqe: LocationMapping[] = [];
  private functionMappings: FunctionMapping[] = [];
  private version: string = "1.0.0";

  /**
   * Add a location mapping between qiqe and JavaScript
   */
  addMapping(
    qiqeLocation: QiqeLocation,
    jsLocation: JSLocation,
    context: string,
  ): void {
    const mapping: LocationMapping = {
      qiqeLocation,
      jsLocation,
      context,
    };

    this.qiqeToJs.push(mapping);
    this.jsToQiqe.push(mapping);
  }

  /**
   * Add a function mapping
   */
  addFunctionMapping(
    qiqeFunctionName: string,
    jsFunctionName: string,
    qiqeLocation: QiqeLocation,
    jsLocation: JSLocation,
  ): void {
    this.functionMappings.push({
      qiqeFunctionName,
      jsFunctionName,
      qiqeLocation,
      jsLocation,
    });
  }

  /**
   * Build the complete source map
   */
  build(): SourceMap {
    // Sort mappings by location for efficient lookup
    this.qiqeToJs.sort((a, b) => {
      if (a.qiqeLocation.line !== b.qiqeLocation.line) {
        return a.qiqeLocation.line - b.qiqeLocation.line;
      }
      return a.qiqeLocation.column - b.qiqeLocation.column;
    });

    this.jsToQiqe.sort((a, b) => {
      if (a.jsLocation.line !== b.jsLocation.line) {
        return a.jsLocation.line - b.jsLocation.line;
      }
      return a.jsLocation.column - b.jsLocation.column;
    });

    return {
      qiqeToJs: [...this.qiqeToJs],
      jsToQiqe: [...this.jsToQiqe],
      functionMappings: [...this.functionMappings],
      version: this.version,
    };
  }

  /**
   * Clear all mappings
   */
  clear(): void {
    this.qiqeToJs = [];
    this.jsToQiqe = [];
    this.functionMappings = [];
  }

  /**
   * Create source map from qiqe code compilation
   */
  static fromCompilation(qiqeCode: string, jsCode: string): SourceMap {
    const builder = new SourceMapBuilder();

    // Simple line-by-line mapping for now
    // In a real implementation, this would be more sophisticated
    const qiqeLines = qiqeCode.split("\n");
    const jsLines = jsCode.split("\n");

    let jsLineOffset = 0;

    // Skip JavaScript runtime setup lines
    for (let i = 0; i < jsLines.length; i++) {
      if (jsLines[i].includes("// User code")) {
        jsLineOffset = i + 1;
        break;
      }
    }

    // Map each qiqe line to corresponding JavaScript line
    qiqeLines.forEach((qiqeLine, qiqeIndex) => {
      if (qiqeLine.trim()) {
        // Only map non-empty lines
        const qiqeLocation: QiqeLocation = {
          line: qiqeIndex + 1,
          column: 1,
        };

        const jsLocation: JSLocation = {
          line: jsLineOffset + qiqeIndex + 1,
          column: 1,
        };

        builder.addMapping(qiqeLocation, jsLocation, qiqeLine.trim());
      }
    });

    return builder.build();
  }

  /**
   * Find the closest qiqe location for a JavaScript location
   */
  static findQiqeLocation(
    sourceMap: SourceMap,
    jsLocation: JSLocation,
  ): QiqeLocation | undefined {
    // Binary search for the closest mapping
    let left = 0;
    let right = sourceMap.jsToQiqe.length - 1;
    let bestMatch: LocationMapping | undefined;

    while (left <= right) {
      const mid = Math.floor((left + right) / 2);
      const mapping = sourceMap.jsToQiqe[mid];

      if (mapping.jsLocation.line === jsLocation.line) {
        if (mapping.jsLocation.column <= jsLocation.column) {
          bestMatch = mapping;
          left = mid + 1; // Look for a better match
        } else {
          right = mid - 1;
        }
      } else if (mapping.jsLocation.line < jsLocation.line) {
        bestMatch = mapping;
        left = mid + 1;
      } else {
        right = mid - 1;
      }
    }

    return bestMatch?.qiqeLocation;
  }

  /**
   * Find the closest JavaScript location for a qiqe location
   */
  static findJSLocation(
    sourceMap: SourceMap,
    qiqeLocation: QiqeLocation,
  ): JSLocation | undefined {
    // Binary search for the closest mapping
    let left = 0;
    let right = sourceMap.qiqeToJs.length - 1;
    let bestMatch: LocationMapping | undefined;

    while (left <= right) {
      const mid = Math.floor((left + right) / 2);
      const mapping = sourceMap.qiqeToJs[mid];

      if (mapping.qiqeLocation.line === qiqeLocation.line) {
        if (mapping.qiqeLocation.column <= qiqeLocation.column) {
          bestMatch = mapping;
          left = mid + 1; // Look for a better match
        } else {
          right = mid - 1;
        }
      } else if (mapping.qiqeLocation.line < qiqeLocation.line) {
        bestMatch = mapping;
        left = mid + 1;
      } else {
        right = mid - 1;
      }
    }

    return bestMatch?.jsLocation;
  }
}

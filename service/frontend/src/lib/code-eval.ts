import { StatusCode } from "../hooks/useShell";
import { CodeEvaluationEngine } from "./CodeEvaluationEngine";
import { SandboxLevel } from "./execution-sandbox/types";
import { ensureStandardLibraryInitialized } from "./standard-library/StandardLibraryLoader";

type Props = {
  code: string;
  imports: string[];
  print: (output: string) => Promise<void>;
  raise: (output: string, statusCode: Exclude<StatusCode, 0>) => Promise<void>;
};

// Create a singleton instance of the enhanced evaluation engine
const evaluationEngine = new CodeEvaluationEngine(SandboxLevel.MODERATE);

// Ensure standard library is initialized when this module loads
let standardLibraryInitPromise: Promise<void> | null = null;

const ensureStandardLibraryReady = async (): Promise<void> => {
  if (!standardLibraryInitPromise) {
    standardLibraryInitPromise = ensureStandardLibraryInitialized({
      autoLoadStandardLibrary: true,
      preloadLibraries: ["std"],
      cacheEnabled: true,
      lazyLoadThreshold: 1024 * 50, // 50KB
      standardLibraryPath: "/qiqe/library/std.qq",
    }).then(() => {}); // Convert Promise<StandardLibraryLoader> to Promise<void>
  }
  return standardLibraryInitPromise;
};

const runtime = async ({ code, imports, print, raise }: Props) => {
  try {
    // Ensure standard library is ready before evaluation
    await ensureStandardLibraryReady();

    // Convert string imports to ImportSpec format
    // Note: The CodeEvaluationEngine will automatically include the standard library
    // if it's not already in the imports, so we don't need to add it manually here
    const importSpecs = imports
      .filter(
        (importStr) =>
          typeof importStr === "string" && importStr.trim().length > 0,
      ) // Filter out empty imports and non-strings
      .map((importStr) => ({
        path: importStr.trim(),
        type: "standard" as const,
        selective: undefined,
        alias: undefined,
      }));

    // Use the enhanced evaluation engine with automatic standard library loading
    const result = await evaluationEngine.evaluate(code, {
      imports: importSpecs,
      timeout: 30000,
      memoryLimit: 128,
      debugMode: false,
      sandboxLevel: SandboxLevel.MODERATE,
      enableAsyncOps: true, // Enable async operations to allow setTimeout for engine internals
      validateJavaScript: true,
    });

    if (result.success) {
      await print(result.output);
    } else {
      // Format errors for display
      const errorMessages = result.errors
        .map((error) => {
          // Access the error handler through the public method
          const errorHandler = evaluationEngine.getErrorHandler();
          const formattedError = errorHandler.formatError(error);
          return `${formattedError.title}: ${formattedError.message}`;
        })
        .join("\n");

      await raise(`${errorMessages}\n`, 1);
    }
  } catch (e) {
    await raise(`Error: ${(e as Error).message}\n`, 1);
  }
};

const codeEvaluate = ({
  code,
  imports,
  print,
  raise,
}: Props): Promise<void> => {
  return runtime({ code, imports, print, raise });
};

// Enhanced version that exposes more capabilities
export const enhancedCodeEvaluate = async ({
  code,
  imports,
  options = {},
}: {
  code: string;
  imports: string[];
  options?: {
    enableAsyncOps?: boolean;
    debugMode?: boolean;
    timeout?: number;
    memoryLimit?: number;
  };
}) => {
  // Ensure standard library is ready
  await ensureStandardLibraryReady();

  const importSpecs = imports
    .filter((importStr) => importStr.trim().length > 0)
    .map((importStr) => ({
      path: importStr.trim(),
      type: "standard" as const,
      selective: undefined,
      alias: undefined,
    }));

  return await evaluationEngine.evaluate(code, {
    imports: importSpecs,
    timeout: options.timeout || 30000,
    memoryLimit: options.memoryLimit || 128,
    debugMode: options.debugMode || false,
    sandboxLevel: SandboxLevel.MODERATE,
    enableAsyncOps:
      options.enableAsyncOps !== undefined ? options.enableAsyncOps : true, // Default to true
    validateJavaScript: true,
  });
};

// Export the evaluation engine instance for advanced usage
export { evaluationEngine };

export default codeEvaluate;

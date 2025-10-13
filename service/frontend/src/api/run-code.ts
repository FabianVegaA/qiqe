// Response type definitions for better type safety
export interface CodegenResponse {
  id: number;
  result: string;
  status: boolean;
  error: string;
  createdAt: string;
}

export interface LibraryResponse {
  target_code: string;
  status: boolean;
  error: string;
}

export interface ErrorResponse {
  error: string;
  status: boolean;
  timestamp: string;
  requestId?: string;
}

// Configuration - can be overridden via environment variables
const API_BASE_URL = process.env.REACT_APP_API_BASE_URL || "http://localhost";
const REQUEST_TIMEOUT = 30000; // 30 seconds to match API service timeout

/**
 * Enhanced API client with better error handling and type safety
 */
class ApiClient {
  private baseUrl: string;
  private timeout: number;

  constructor(
    baseUrl: string = API_BASE_URL,
    timeout: number = REQUEST_TIMEOUT,
  ) {
    this.baseUrl = baseUrl;
    this.timeout = timeout;
  }

  /**
   * Makes a fetch request with enhanced error handling (no timeout to avoid runtime restrictions)
   */
  private async fetchWithTimeout(
    url: string,
    options: RequestInit,
  ): Promise<Response> {
    try {
      const response = await fetch(url, {
        ...options,
        headers: {
          "Content-Type": "application/json;charset=utf-8",
          ...options.headers,
        },
      });

      return response;
    } catch (error) {
      if (error instanceof Error && error.name === "AbortError") {
        throw new Error("Request was cancelled");
      }
      throw error;
    }
  }

  /**
   * Handles API response and extracts JSON with proper error handling
   */
  private async handleResponse<T>(response: Response): Promise<T> {
    let responseData: any;

    try {
      responseData = await response.json();
    } catch (error) {
      throw new Error(
        `Invalid JSON response from server (status: ${response.status})`,
      );
    }

    // Handle HTTP error status codes
    if (!response.ok) {
      // Check if response follows the new error format
      if (
        responseData &&
        typeof responseData === "object" &&
        "error" in responseData
      ) {
        const errorResponse = responseData as ErrorResponse;
        throw new Error(
          errorResponse.error ||
            `HTTP ${response.status}: ${response.statusText}`,
        );
      } else {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
    }

    // Handle application-level errors (when HTTP status is 200 but status field is false)
    if (
      responseData &&
      typeof responseData === "object" &&
      "status" in responseData &&
      !responseData.status
    ) {
      const errorMessage = responseData.error || "Unknown application error";
      throw new Error(errorMessage);
    }

    return responseData as T;
  }

  /**
   * Execute code through the interpreter service
   */
  async executeCode(code: string): Promise<CodegenResponse> {
    if (!code || typeof code !== "string") {
      throw new Error("Code is required and must be a string");
    }

    if (code.trim().length === 0) {
      throw new Error("Code cannot be empty");
    }

    try {
      const response = await this.fetchWithTimeout(`${this.baseUrl}/codegen`, {
        method: "POST",
        body: JSON.stringify({ code }),
      });

      return await this.handleResponse<CodegenResponse>(response);
    } catch (error) {
      console.error("Code execution failed:", error);
      throw error instanceof Error
        ? error
        : new Error("Unknown error occurred during code execution");
    }
  }

  /**
   * Import library file
   */
  async importLibrary(filename: string): Promise<LibraryResponse> {
    if (!filename || typeof filename !== "string") {
      throw new Error("Filename is required and must be a string");
    }

    if (filename.trim().length === 0) {
      throw new Error("Filename cannot be empty");
    }

    try {
      const response = await this.fetchWithTimeout(`${this.baseUrl}/lib`, {
        method: "POST",
        body: JSON.stringify({ filename }),
      });

      return await this.handleResponse<LibraryResponse>(response);
    } catch (error) {
      console.error("Library import failed:", error);
      throw error instanceof Error
        ? error
        : new Error("Unknown error occurred during library import");
    }
  }
}

// Create singleton instance
const apiClient = new ApiClient();

/**
 * Execute code through interpreter - maintains backward compatibility
 */
export async function postCode(code: string): Promise<Response> {
  try {
    const result = await apiClient.executeCode(code);

    // Create a mock Response object to maintain backward compatibility
    // The playground component expects to call .json() on the response
    return new Response(JSON.stringify(result), {
      status: 200,
      statusText: "OK",
      headers: {
        "Content-Type": "application/json",
      },
    });
  } catch (error) {
    // Create a mock error Response to maintain backward compatibility
    const errorMessage =
      error instanceof Error ? error.message : "Unknown error";
    return new Response(
      JSON.stringify({ error: errorMessage, status: false }),
      {
        status: 500,
        statusText: "Internal Server Error",
        headers: {
          "Content-Type": "application/json",
        },
      },
    );
  }
}

/**
 * Import multiple library files - maintains backward compatibility
 */
export async function importLibs(paths: string[]): Promise<
  {
    label: string;
    target: string;
  }[]
> {
  if (!Array.isArray(paths)) {
    throw new Error("Directories must be provided as an array");
  }

  try {
    const results = await Promise.all(
      paths.map(async (label) => {
        try {
          const result = await apiClient.importLibrary(label);
          return {
            label: label,
            target: result.target_code,
          };
        } catch (error) {
          console.error(`Failed to import library ${label}:`, error);
          throw error;
        }
      }),
    );

    return results;
  } catch (error) {
    console.error("Failed to import libraries:", error);
    throw error;
  }
}

// Export the API client instance for advanced usage
export { apiClient };

# API Client Documentation

## Overview

The API client has been updated to work with the new nginx proxy architecture while maintaining backward compatibility with existing frontend code.

## Key Changes

### 1. Enhanced Error Handling

- **Structured Error Responses**: Now handles both HTTP errors and application-level errors
- **Timeout Support**: Configurable request timeouts (default: 30 seconds)
- **Better Error Messages**: More descriptive error messages for debugging

### 2. Type Safety

- **TypeScript Interfaces**: Added proper TypeScript interfaces for all request/response types
- **Input Validation**: Client-side validation for required parameters
- **Response Validation**: Validates response structure before processing

### 3. Configuration

- **Environment Variables**: Base URL can be configured via `REACT_APP_API_BASE_URL`
- **Flexible Configuration**: API client can be instantiated with custom settings

### 4. Backward Compatibility

- **Same Function Signatures**: `postCode()` and `importLibs()` maintain the same interfaces
- **Response Format**: Maintains compatibility with existing playground component
- **Error Handling**: Graceful degradation for network and API errors

## Usage

### Basic Usage (Backward Compatible)

```typescript
import { postCode, importLibs } from "./api/run-code";

// Execute code
const response = await postCode('print("Hello World")');
const data = await response.json();

// Import libraries
const libraries = await importLibs(["std.qq", "list.qq"]);
```

### Advanced Usage

```typescript
import { apiClient } from "./api/run-code";

// Direct API client usage with better error handling
try {
  const result = await apiClient.executeCode('print("Hello World")');
  console.log(result.result);
} catch (error) {
  console.error("Code execution failed:", error.message);
}

// Import single library
try {
  const library = await apiClient.importLibrary("std.qq");
  console.log(library.target_code);
} catch (error) {
  console.error("Library import failed:", error.message);
}
```

## Configuration

### Environment Variables

- `REACT_APP_API_BASE_URL`: Base URL for API requests (default: `http://localhost`)

### Custom Configuration

```typescript
import { ApiClient } from "./api/run-code";

const customClient = new ApiClient("https://api.example.com", 60000); // 60 second timeout
```

## Error Handling

The API client handles several types of errors:

1. **Network Errors**: Connection failures, DNS issues
2. **Timeout Errors**: Requests that exceed the configured timeout
3. **HTTP Errors**: 4xx and 5xx status codes
4. **Application Errors**: API responses with `status: false`
5. **Validation Errors**: Invalid input parameters

All errors are wrapped in descriptive Error objects with meaningful messages.

## Testing

The API client has been thoroughly tested to ensure reliability and backward compatibility with the existing frontend components.

## Migration Notes

### From Old Proxy (Port 3030) to New Architecture

The API client automatically uses the new nginx proxy architecture. The main changes:

1. **Base URL**: Changed from `http://localhost:3030` to `http://localhost` (configurable)
2. **Error Handling**: Enhanced to work with new structured error responses
3. **Timeout**: Increased from 1 second to 30 seconds to match backend timeout
4. **Type Safety**: Added TypeScript interfaces for better development experience

### Backward Compatibility

All existing code using `postCode()` and `importLibs()` will continue to work without changes. The functions maintain the same signatures and return types.

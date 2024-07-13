import { useState } from "react";

export type StatusCode = 0 | 1;

export function useConsole(): {
  console: [StatusCode, string][];
  print: (output: string) => Promise<void>;
  raise: (output: string, statusCode: Exclude<StatusCode, 0>) => Promise<void>;
} {
  const [console, setConsole] = useState<[StatusCode, string][]>([]);
  return {
    console,
    print: async (output: string) => {
      setConsole([[0, output], ...console]);
    },
    raise: async (output: string, statusCode: Exclude<StatusCode, 0>) => {
      setConsole([[statusCode, output], ...console]);
    },
  };
}

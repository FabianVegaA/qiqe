import { useState } from "react";

export type StatusCode = 0 | 1;

export function useShell(): {
  shell: [StatusCode, string][];
  print: (output: string) => Promise<void>;
  raise: (output: string, statusCode: Exclude<StatusCode, 0>) => Promise<void>;
  clear: () => void;
} {
  const [shell, setShell] = useState<[StatusCode, string][]>([]);
  return {
    shell,
    print: async (output: string) => {
      setShell((prevShell) => [[0, output], ...prevShell]);
    },
    raise: async (output: string, statusCode: Exclude<StatusCode, 0>) => {
      setShell((prevShell) => [[statusCode, output], ...prevShell]);
    },
    clear: () => {
      setShell([]);
    },
  };
}

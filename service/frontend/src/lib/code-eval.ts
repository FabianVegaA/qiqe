import { StatusCode } from "../hooks/useShell";

type Props = {
  code: string;
  imports: string[];
  print: (output: string) => Promise<void>;
  raise: (output: string, statusCode: Exclude<StatusCode, 0>) => Promise<void>;
};

const runtime = ({ code, imports, print, raise }: Props) => {
  try {
    const script = `
    /*
    * Execute the user code and return the output
    * @returns {string} The output of the user code
    * @throws {Error} If the user code throws an error
    */
    function execute() {
      // Output buffer
      let output = "";

      // Imports from the user
      ${imports.join("\\n").trim()};

      // Runtime functions
      const print__qq = (value) => { output += value };
      const println__qq = (value) => { output += value + "\\n" };
      const raise__qq = (value) => { throw new Error(value) };

      // User code
      ${code};

      // Return the output
      return output;
    }
    execute();
    `;
    console.debug(script);
    print(eval(script));
  } catch (e) {
    raise(`Error: ${(e as Error).message}\n`, 1);
  }
};

const codeEvaluate = ({
  code,
  imports,
  print,
  raise,
}: Props): Promise<void> => {
  return new Promise((resolve) => {
    runtime({ code, imports, print, raise });
    resolve();
  });
};

export default codeEvaluate;

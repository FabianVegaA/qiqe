type Props = {
  code: string;
  imports: string[];
};

type OutputEval = {
  stdout: string;
  stderr: string;
};

const runtime = ({code, imports}: Props): OutputEval => {
  const script = `
  ${imports.join("\\n")}

  (() => {
    let stdout = "";
    let stderr = "";
  
    try {
      function print(value) {
        stdout += value.toString() + "\\n";
      }
  
      ${code.replaceAll(/([_a-zA-Z][_a-zA-Z0-9]*)'/gm, "$1__PRIME__")};
    } catch (e) {
      stderr += e.toString();
    } finally {
      return { stdout, stderr };
    }
  })();`;
  try {
    return eval(script);
  } catch (e) {
    return { stdout: "", stderr: (e as Error).toString() };
  }
};

const codeEvaluate = ({ code, imports }: Props): Promise<string> => {
  return new Promise((resolve, reject) => {
    const { stdout, stderr } = runtime({ code: code, imports });
    if (stderr) {
      reject(stderr);
    }
    resolve(stdout);
  });
};

export default codeEvaluate;

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
  ${imports.join("\\n").trim()};
  (() => {
    let stdout = "";
    let stderr = "";
  
    try {
      const eval__qq = eval;

      function print__qq(value) {
        stdout += value.toString() + "\\n";
      }
      function throw__qq(value) {
        stderr += value.toString() + "\\n";
      }

      ${code};
    } catch (e) {
      stderr += e.toString();
    } finally {
      return { stdout, stderr };
    }
  })();`;
  console.debug(script);
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

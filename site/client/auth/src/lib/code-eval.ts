const prelude = `
"use strict";
const not = (a) => !a;
const id = (a) => a;
const eq = (a) => ((b) => (a === b));
const neq = (a) => (b) => not(eq(a)(b));
// Logical operators
const and = (a) => (b) => a && b;
const or = (a) => (b) => a || b;
const lt = (a) => (b) => a < b;
const gt = (a) => (b) => a > b;
const leq = (a) => (b) => lt(a)(b) || eq(a)(b);
const geq = (a) => (b) => gt(a)(b) || eq(a)(b);
// Arithmetic operators
const add = (a) => (b) => a + b;
const sub = (a) => (b) => a - b;
const mul = (a) => (b) => a * b;
const div = (a) => (b) => a / b;
const mod = (a) => (b) => a % b;
// String operators
const concat = (a) => (b) => a + b;
const show = (a) => a.toString();
`;

const runtime = (code: string): { stdout: string; stderr: string } => {
  const script = `
  ${prelude}

  (() => {
    let stdout = "";
    let stderr = "";
  
    try {
      function print(value) {
        stdout += value.toString() + "\\n";
      }
  
      ${code.replaceAll(/([_a-zA-Z][_a-zA-Z0-9]*)'$/g, "$1__PRIME__")};
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

const codeEvaluate = (jsCode: string): Promise<string> => {
  return new Promise((resolve, reject) => {
    const { stdout, stderr } = runtime(jsCode);
    if (stderr) {
      reject(stderr);
    }
    resolve(stdout);
  });
};

export default codeEvaluate;

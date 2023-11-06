const prelude = `
"use strict";
// Basic
const id = (a) => a;
const constant = (a) => (_) => a;
const flip = (f) => (a) => (b) => f(b)(a);
// Comparison operators
const eq = (a) => ((b) => (a === b));
const neq = (a) => (b) => not(eq(a)(b));
const lt = (a) => (b) => a < b;
const gt = (a) => (b) => a > b;
const leq = (a) => (b) => lt(a)(b) || eq(a)(b);
const geq = (a) => (b) => gt(a)(b) || eq(a)(b);
// Logical operators
const not = (a) => !a;
const and = (a) => (b) => a && b;
const or = (a) => (b) => a || b;
// Arithmetic operators
const add = (a) => (b) => a + b;
const sub = (a) => (b) => a - b;
const mul = (a) => (b) => a * b;
const div = (a) => (b) => a / b;
const mod = (a) => (b) => a % b;
const pow = (a) => (b) => a ** b;
const sqrt = (a) => pow(a)(0.5);
const abs = (a) => Math.abs(a);
const succ = (a) => add(a)(1);
const pred = (a) => sub(a)(1);
const neg = (a) => -a;
const min = (a) => (b) => Math.min(a, b);
const max = (a) => (b) => Math.max(a, b);
const floor = (a) => Math.floor(a);
const ceil = (a) => Math.ceil(a);
const round = (a) => Math.round(a);
const truncate = (a) => a < 0 ? ceil(a) : floor(a);
// String operators
const concat = (a) => (b) => a + b;
const show = (a) => a.toString();
const length = (a) => show(a).length;
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

const codeEvaluate = (jsCode: string): Promise<string> => {
  console.log(jsCode);
  return new Promise((resolve, reject) => {
    const { stdout, stderr } = runtime(jsCode);
    if (stderr) {
      reject(stderr);
    }
    resolve(stdout);
  });
};

export default codeEvaluate;

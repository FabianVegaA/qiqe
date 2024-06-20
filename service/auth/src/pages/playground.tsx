import { useState } from "react";
import AceEditor from "react-ace";
import "brace/mode/lisp";
import "brace/theme/github";

import postCode from "../api/run-code";
import codeEvaluate from "../lib/code-eval";

type Result = {
  id: number;
  status: boolean;
  error: string;
  result: string;
  createdAt: string;
};

export default function Playground() {
  const [code, setCode] = useState("");
  const [result, setResult] = useState([] as Result[]);

  const addResult = (res: Result) => setResult([res, ...result]);
  const clearResult = () => setResult([]);

  const setErr = (err: string) => {
    addResult({
      id: result.length,
      status: false,
      error: err || "Something went wrong. Please try again later.",
      result: "",
      createdAt: new Date().toDateString(),
    });
  };

  const runCode = async (code: string) => {
    const res = await postCode(code).catch(setErr);
    if (!res) return;

    await res
      .json()
      .then((data: Result) => {
        if (!data.status) {
          addResult(data);
          return;
        }

        codeEvaluate(data.result)
          .then((stdout: string) => {
            addResult({
              id: data.id,
              status: data.status,
              error: data.error,
              result: stdout,
              createdAt: data.createdAt,
            });
          })
          .catch((stderr: string) => {
            addResult({
              id: data.id,
              status: data.status && !stderr,
              error: data.error || stderr,
              result: "",
              createdAt: data.createdAt,
            });
          });
      })
      .catch(setErr);
  };

  const handleRun = (e: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    e.preventDefault();
    if (!code) return;
    runCode(code);
  };

  return (
    <div className="App">
      <section className="playground">
        <div className="toolbar">
          <button id="btn-run" className="pure-button" onClick={handleRun}>
            Run
          </button>
          <button id="btn-clear" className="pure-button" onClick={clearResult}>
            Clear
          </button>
          <button id="btn-save" className="pure-button">
            Save
          </button>
        </div>
        <div className="code-editor">
          <AceEditor
            mode="lisp"
            theme="github"
            onChange={setCode}
            name="code-editor"
          />
        </div>
        <div id="result" className="code-result">
          <ul>
            {Array.from(result.entries()).map(
              ([id, { createdAt, status, error, result }]: [
                number,
                Result
              ]) => (
                <li key={id}>
                  {status ? (
                    <pre className="result">{result.toString()}</pre>
                  ) : (
                    <pre className="error">{error.toString()}</pre>
                  )}
                  <div className="created-at">{createdAt}</div>
                </li>
              )
            )}
          </ul>
        </div>
      </section>
    </div>
  );
}

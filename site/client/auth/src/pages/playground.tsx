import { useState } from "react";

import AceEditor from "react-ace";
import "brace/mode/lisp";
import "brace/theme/github";

import postCode from "../api/run-code";

type Result = {
  id: number;
  status: boolean;
  error: string;
  result: string;
  createdAt: string;
};

export default function Playground() {
  const [code, setCode] = useState('print "Hello, World!"');
  const [result, setResult] = useState([] as Result[]);

  const runCode = async (code: string) => {
    const setErr = (err: string) => {
      setResult([
        {
          id: result.length,
          status: false,
          error: err || "Something went wrong. Please try again later.",
          result: "",
          createdAt: new Date().toDateString(),
        },
        ...result,
      ]);
    };

    const res = await postCode(code).catch(setErr);

    if (!res) return;

    await res
      .json()
      .then((data) => {
        setResult([
          {
            id: result.length,
            status: data.status,
            error: data.error,
            result: data.result,
            createdAt: data.createdAt,
          },
          ...result,
        ]);
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
          <button
            id="btn-clear"
            className="pure-button"
            onClick={() => {
              setResult([]);
            }}
          >
            Clear
          </button>
          <button id="btn-save" className="pure-button">
            Save
          </button>
        </div>
        <AceEditor
          mode="lisp"
          theme="github"
          onChange={setCode}
          name="playground"
          editorProps={{ $blockScrolling: true }}
        />
        <div id="result" className="code-result">
          <ul>
            {result.map((r) => (
              <li key={r.id}>
                {!r.status ? (
                  <div className="error">{r.error}</div>
                ) : (
                  <div className="result">{r.result}</div>
                )}
                <div className="created-at">{r.createdAt}</div>
              </li>
            ))}
          </ul>
        </div>
      </section>
    </div>
  );
}

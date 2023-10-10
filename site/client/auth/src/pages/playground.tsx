import { useState } from "react";

import AceEditor from "react-ace";
import "brace/mode/lisp";
import "brace/theme/github";

type Result = {
  id: number;
  status: string | Error;
  result: string;
  createdAt: string;
};

async function runCode(code: string) {
  // TODO: implement
  return {
    id: 1,
    status: "ok",
    result: "Hello, World!",
    createdAt: new Date().toISOString(),
  };
}

export default function Playground() {
  const [code, setCode] = useState("");
  const [result, setResult] = useState([] as Result[]);

  return (
    <div className="App">
      <section className="playground">
        <div className="toolbar">
          <button
            id="btn-run"
            className="pure-button"
            onClick={() => {
              runCode(code).then((r) => {
                setResult([r, ...result]);
              });
            }}
          >
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
                {r.status instanceof Error ? (
                  <div className="status error">
                    {(r.status as Error).message}
                  </div>
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

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

export default function Playground() {
  const [code, setCode] = useState("");
  const [result, setResult] = useState([] as Result[]);

  const runCode = async (code: string) => {
    const setErr = (err: string) => {
      setResult([
        {
          id: result.length,
          status: Error(err || "Something went wrong. Please try again later."),
          result: "",
          createdAt: new Date().toLocaleString(),
        },
        ...result,
      ]);
    };

    const res = await fetch("http://127.0.0.1:8000/api/playground/", {
      method: "POST",
      headers: {
        "Content-Type": "application/json; charset=UTF-8",
      },
      body: JSON.stringify({ code: code }),
    }).catch(setErr);

    if (!res) return;

    await res
      .json()
      .then((data) => {
        setResult([
          {
            id: result.length,
            status: data.status,
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

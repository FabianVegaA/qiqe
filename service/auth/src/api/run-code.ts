import axios from "axios";

const CODEGEN_URI = "http://localhost:3030";

const instance = axios.create({
  baseURL: CODEGEN_URI,
  timeout: 1000,
});

export async function postCode(code: string): Promise<Response> {
  return fetch(CODEGEN_URI + "/codegen", {
    method: "POST",
    body: JSON.stringify({ code: code }),
    headers: { "Content-Type": "application/json;charset=utf-8" },
  });
}

export async function importLibs(dirs: string[]): Promise<string[]> {
  console.log("importLibs", dirs);
  function* importLibs(
    dirs: string[]
  ): Generator<Promise<Response>, void, void> {
    for (const dir of dirs) {
      yield fetch(CODEGEN_URI + "/lib", {
        method: "POST",
        body: JSON.stringify({ filename: dir }),
        headers: { "Content-Type": "application/json;charset=utf-8" },
      });
    }
  }
  return await Promise.all(
    Array.from(importLibs(dirs)).map(async (res) => {
      return await res.then((res) =>
        res.status === 200 ? res.text() : Promise.reject(res)
      );
    })
  );
}

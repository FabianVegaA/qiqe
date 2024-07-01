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
  // return fetch(CODEGEN_URI + "/import", {
  //   method: "POST",
  //   body: JSON.stringify({ dirs }),
  //   headers: { "Content-Type": "application/json;charset=utf-8" },
  // }).then((res) => res.json());
  
  // Dummy implementation
  return [];
}

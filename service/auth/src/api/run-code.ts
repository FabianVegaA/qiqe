const postCode = async (code: string): Promise<Response> => {
  return fetch("http://127.0.0.1:3030/codegen", {
    method: "POST",
    body: JSON.stringify({ code: code }),
    headers: { "Content-Type": "application/json;charset=utf-8" },
  });
};

export default postCode;

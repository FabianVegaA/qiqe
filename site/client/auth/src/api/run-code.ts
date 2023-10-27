const postCode = async (code: string): Promise<Response> => {
  return fetch("http://127.0.0.1:8000/api/playground/", {
    method: "POST",
    headers: {
      "Content-Type": "application/json; charset=UTF-8",
    },
    body: JSON.stringify({ code: code }),
  });
};

export default postCode;

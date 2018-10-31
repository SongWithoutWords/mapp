import checkRequestErrors from "./errors";

function postData(url = ``, data = {}) {
  // Default options are marked with *
  return fetch(url, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json"
    },
    body: JSON.stringify(data)
  }).then(checkRequestErrors);
}

export default postData;

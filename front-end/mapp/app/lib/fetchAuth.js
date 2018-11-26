import checkRequestErrors from "../lib/errors";
import base64 from "react-native-base64";

function fetchAuth({
  url = "",
  method = "",
  data = {},
  email = "",
  password = ""
}) {
  const requestOptions = {
    method: method,
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json",
      Authorization: "Basic " + base64.encode(email + ":" + password)
    },
    body: JSON.stringify(data)
  };

  if(method.toUpperCase() === "GET" || method.toUpperCase() == "DELETE" ){
    delete requestOptions.body;
  }

  return fetch(url, requestOptions).then(checkRequestErrors);
}

export default fetchAuth;

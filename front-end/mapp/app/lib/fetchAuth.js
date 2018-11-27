import checkRequestErrors from "../lib/errors";
import base64 from "react-native-base64";

function fetchAuth({
  url = "",
  method = "",
  data = {},
  email = "",
  password = ""
}) {
  console.log(email);
  console.log(password);
  const requestOptions = {
    method: method,
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json",
      Authorization: "Basic " + base64.encode(email + ":" + password)
    },
    body: JSON.stringify(data)
  };
  console.log("fetchAuth" + JSON.stringify(requestOptions));
  if(method.toUpperCase() === "GET" || method.toUpperCase() == "DELETE" ){
    delete requestOptions.body;
  }

  return fetch(url, requestOptions).then(checkRequestErrors);
}

export default fetchAuth;

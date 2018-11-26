import checkRequestErrors from "./errors";
import fetchAuth from "./fetchAuth";

function postData(url = ``, data = {}, email="", password ="") {
  // Default options are marked with *
  const method = "POST";
  return fetchAuth({ url, method, data, email, password })
}

export default postData;

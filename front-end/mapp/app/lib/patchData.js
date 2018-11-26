import checkRequestErrors from "./errors";
import fetchAuth from "./fetchAuth";

function patchData(url = ``, data = {}, email="", password ="") {
  // Default options are marked with *
  const method = "PATCH";
  return fetchAuth({ url, method, data, email, password })
}

export default patchData;

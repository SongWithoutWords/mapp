import settings from "../config/settings";
import _ from "lodash";
import fetchAuth from "../lib/fetchAuth";

function getDoctors(email="", password="") {
  const url = settings.REMOTE_SERVER_URL + settings.DOCTOR_RES;
  const method = "GET";
  return fetchAuth({url, method, email, password})
    .then(response => {
      return response.json();
    })
    .then(function(responseJson) {
      console.log(responseJson);
      result = arrayToObj(responseJson);
      console.log(result);
      return result;
    });
}

function arrayToObj(array) {
  const result = {};
  const byId = {};
  const allIds = [];

  array.forEach(element => {
    byId[element.id] = element;
    allIds.push(element.id);
  });

  result.byId = byId;
  result.allIds = allIds;
  return result;
}

export default getDoctors;

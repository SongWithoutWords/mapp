import settings from "../config/settings";
import _ from "lodash";

function getDoctors(email="", password="") {
  return fetch(settings.REMOTE_SERVER_URL + settings.DOCTOR_RES, {
    method: "GET",
    headers: {
      email: email,
      password: password
    },
  })
    .then(response => {
      if (!response.ok) {
        throw new Error(response.status.toString());
      }
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

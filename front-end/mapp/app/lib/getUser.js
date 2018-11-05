import _ from "lodash";
import { USER_TYPE } from "../config/constants";

// signin/signup api calls
function getUser(url, form) {
  const { email, password, firstName = "", lastName = "" } = form;
  const jsonBody = { email, password, firstName, lastName };
  return fetch(url, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json"
    },
    body: JSON.stringify(jsonBody)
  })
    .then(response => {
      if (!response.ok) {
        throw new Error('Password or Email is incorrect');
      }
      return response.json();
    })
    .then(function(responseJson) {
      result = {};

      // user
      const userTypeString =
        "doctors" in responseJson ? USER_TYPE.PATIENT : USER_TYPE.DOCTOR;
      const user = _.merge({}, form, responseJson); // response json may overwrite attributes in form
      let { doctors = [], patients = [], pendingRequests = [] } = user;
      const myDoctors = doctors.map(x => x.id);
      const myPatients = patients.map(x => x.id);
      delete user.doctors;
      delete user.patients;
      user.myDoctors = myDoctors;
      user.myPatients = myPatients;
      user.userType = userTypeString;

      console.log(user);

      // patients and doctors
      patients = arrayToObj(patients);
      doctors = arrayToObj(doctors);

      // pendingRequests
      pendingRequests = arrayToObj(pendingRequests);

      // prescriptions
      result = {
        user,
        patients,
        doctors,
        pendingRequests
      };
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
export default getUser;
